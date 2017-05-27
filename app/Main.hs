{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonadComprehensions#-}
{-# LANGUAGE TypeOperators#-}

-----------------------------------------------------------------------------
--
-- Universal Framework Builder
--
-- HaskellSwift 2016
--
-----------------------------------------------------------------------------

module Main (
    main,
    XcodeScheme(..),
    XcodebuildOptionSet(..),
    BuildError(..),
    EBS,
    createBuildOptionSets,
    cleanFrameworks,
    buildFrameworks,
    combineFrameworks,
    (>>>=)
) where

-- The official GHC API
import           Data.Either
import qualified Data.List                       as List
import qualified Data.Map                        as Map
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.ByteString.Lazy.Char8      as DBLC
import qualified Data.ByteString                 as B

import           System.Environment              (getArgs)
import           System.Exit                     (ExitCode, exitFailure)
import           System.FilePath
import           System.IO                       (hPutStrLn, stderr)
import           System.Process                  (rawSystem, readProcessWithExitCode)
import           System.Directory

import           GHC.IO.Exception

import           Control.Monad.Except
import           Control.Concurrent
import           Control.Exception
import           Control.Monad

import           Text.Printf


-- Third Party API
import Options.Applicative
import Data.Semigroup ((<>))

-- Main
main :: IO ()
main = createXcodeScheme =<< execParser opts where opts = info (options <**> helper) ( fullDesc <> progDesc "build iOS/tvOS universal/fat framework")

-- Command-line interface
data TSOption
  = SchemeOption String | TargetOption String

data BUOptions
  = BUOptions {
  _project       :: String,
  _scheme        :: String,
  _configuraiton :: String}

projectOption = strOption (long "project" <> short 'p' <> metavar "name.xcodeproj" <> help "Build the project name.xcodeproj." )
schemeOption = strOption (long "scheme" <> short 's' <> metavar "schemename"  <> help "Build the scheme specified by schemename." )
configuraitonOption = strOption (long "configuration" <> short 'c' <> metavar "configurationname" <> help "Use the build configuration specified by configurationname when building each target.")

options :: Parser BUOptions
options = BUOptions <$> projectOption <*> schemeOption <*> configuraitonOption

createXcodeScheme :: BUOptions -> IO ()
createXcodeScheme (BUOptions projectname schemename configurationname) = do
  let xcodeScheme = XcodeScheme projectname schemename configurationname
  result <- build xcodeScheme
  case result of
      Left error -> print error
      Right _ -> return ()

-- Data types
data XcodeScheme = XcodeScheme {
    project :: String,
    scheme :: String,
    configuration :: String
} deriving (Show, Eq)

data XcodebuildOptionSet = XcodebuildOptionSet {
    xProject :: String,
    xScheme :: String,
    xSdk :: String,
    xConfiguration :: String
} deriving (Show, Eq)

data BuildError a = CreateBuildOptionSetsNoXcodeCommandLineToolError
    | CreateBuildOptionSetsBuildOptionFailedError a
    | CreateBuildOptionSetsNoneSupportedPlatformsError a
    | CleanFrameworksFailedError a
    | BuildFrameworksFailedError a
    | CombineFrameworksFailedError a
    deriving (Eq, Read, Show)

type EBS = Either (BuildError String)

-----------------------------------------------------------------------------
-- build universal framework
-----------------------------------------------------------------------------
build :: XcodeScheme -> IO (EBS String)
build xcodeScheme = createBuildOptionSets xcodeScheme >>>= cleanFrameworks >>>= buildFrameworks >>>= combineFrameworks xcodeScheme

-----------------------------------------------------------------------------
-- | create xcodebuildOptionSets
createBuildOptionSets :: XcodeScheme -> IO (Either (BuildError String) [XcodebuildOptionSet])
createBuildOptionSets xcodeScheme = getSupportedPlatforms xcodeScheme >>>= createBuildOptionSets' xcodeScheme

createBuildOptionSets' :: XcodeScheme -> [String] -> IO (Either (BuildError String) [XcodebuildOptionSet])
createBuildOptionSets' xcodeScheme platforms = return $ Right (map (createBuildOptionSet (project xcodeScheme) (scheme xcodeScheme) (configuration xcodeScheme)) platforms)

createBuildOptionSet :: String -> String -> String -> String -> XcodebuildOptionSet
createBuildOptionSet project scheme configuration platform = XcodebuildOptionSet project scheme platform configuration

--- get supported platforms --
getSupportedPlatforms :: XcodeScheme -> IO (Either (BuildError String) [String])
getSupportedPlatforms xcodeScheme = do
    r <- readBuildSettings xcodeScheme
    return $ getSupportedPlatforms' xcodeScheme r

getSupportedPlatforms' :: (MonadError (BuildError String) m) => XcodeScheme -> (ExitCode, String, String) -> m [String]
getSupportedPlatforms' xcodeScheme (ExitSuccess, output, error) = case value of
    Just xs -> return $ words xs
    Nothing -> throwError $ CreateBuildOptionSetsNoneSupportedPlatformsError ("scheme = " ++ error)
    where value = lookupBuildSettings "SUPPORTED_PLATFORMS" output
getSupportedPlatforms' xcodeScheme (_, output, error) = throwError $ CreateBuildOptionSetsNoneSupportedPlatformsError (debugInfo xcodeScheme ++ " , " ++ error)

-----------------------------------------------------------------------------
-- | clean frameworks
cleanFrameworks :: [XcodebuildOptionSet] -> IO (EBS [XcodebuildOptionSet])
cleanFrameworks optionSets = do
    mapM_ cleanFramework optionSets
    return $ Right optionSets

cleanFramework :: XcodebuildOptionSet -> IO (ExitCode, String, String)
cleanFramework optionSet = xcodebuild $ cleanOptions optionSet

-----------------------------------------------------------------------------
-- | build frameworks
buildFrameworks :: [XcodebuildOptionSet] -> IO (EBS ([XcodebuildOptionSet], [FilePath]))
buildFrameworks optionSets = buildFrameworks' optionSets >>= checkBuildResults optionSets

buildFrameworks' :: [XcodebuildOptionSet] -> IO [Either (BuildError String) String]
--buildFrameworks' optionSets = mapM (async . buildFramework) optionSets >>= mapM wait
buildFrameworks' = mapM buildFramework

buildFramework :: XcodebuildOptionSet -> IO (Either (BuildError String) String)
buildFramework optionSet = do
    putStrLn ("buildFramework\n" ++ show optionSet)
    (exitCode, output, error) <- buildFramework' optionSet
    case exitCode of
        ExitSuccess -> return $ Right output
        _ -> return $ Left $ BuildFrameworksFailedError $ show optionSet ++ ", error = " ++ error

buildFramework' :: XcodebuildOptionSet -> IO (ExitCode, String, String)
buildFramework' optionSet = xcodebuild $ buildOptions optionSet

--- check build results
checkBuildResults :: [XcodebuildOptionSet] -> [Either (BuildError String) String] -> IO (EBS ([XcodebuildOptionSet], [FilePath]))
checkBuildResults optionSets results = if all isRight results
                                           then return $ Right (optionSets, toTargetPaths $ rights results)
                                           else return $ Left (head $ lefts results)

toTargetPaths :: [FilePath] -> [FilePath]
toTargetPaths results = toTargetPaths' $ map toTouchCommandLine results

toTargetPaths' :: [FilePath] -> [FilePath]
toTargetPaths' xss = map (last . words) $ filter (not . null) xss

toTouchCommandLine :: String -> String
toTouchCommandLine xs = concat $ filter (List.isInfixOf ".framework") $ filter (List.isInfixOf "/usr/bin/touch -c") $ lines xs

getProductsPath  :: String -> [FilePath] -> [XcodebuildOptionSet] -> String
getProductsPath configuration paths buildOptionSets = getBuildPath paths </> getProductDirectoryName configuration buildOptionSets

getProductDirectoryName :: String -> [XcodebuildOptionSet] -> String
getProductDirectoryName configuration buildOptionSets = configuration ++ "-" ++ commonProductName buildOptionSets ++ "-universal/"

getBuildPath :: [String] -> String
getBuildPath = takeDirectory . takeDirectory . head

commonProductName :: [XcodebuildOptionSet] -> String
commonProductName buildOptionSets = commonProductName' platforms where
    platforms = map xSdk buildOptionSets

commonProductName' :: (Eq a, Ord a) => [[a]] -> [a]
commonProductName' = foldl1 commonSubstring

commonSubstring :: (Eq a, Ord a) => [a] -> [a] -> [a]
commonSubstring [] ys = []
commonSubstring xs [] = []
commonSubstring xs ys = commonSubstring' (map fst zs) (map snd zs) where
    zs = zip xs ys
commonSubstring' [] [] = []
commonSubstring' (x:xs) (y:ys)
    | x == y = x:commonSubstring' xs ys
   | otherwise = []

-----------------------------------------------------------------------------
-- | combine frameworks
combineFrameworks :: XcodeScheme -> ([XcodebuildOptionSet], [String]) -> IO (EBS String)
combineFrameworks xcodeScheme (optionSets, frameworkPaths) = do
    putStrLn "combineFrameworks"
    let productsPath   = getProductsPath (configuration xcodeScheme) frameworkPaths optionSets
    executablePath <- getExecutablePath xcodeScheme
    cleanUniversalFramework frameworkPaths productsPath
    createUniversalFramework frameworkPaths productsPath executablePath
    return $ Right ""

--- clean universal frameworks ---
cleanUniversalFramework :: [String] -> String -> IO ExitCode
cleanUniversalFramework [] _ = return (ExitFailure 1)
cleanUniversalFramework paths outputDirectory = do
    putStrLn "cleanUniversalFramework"
    createDirectoryIfMissing False outputDirectory
    return ExitSuccess

removeDirectoryWhenExist :: FilePath -> IO ()
removeDirectoryWhenExist dir = do
    exist <- doesDirectoryExist dir
    when exist (removeDirectoryRecursive dir)

--- create universal frameworks ---
createUniversalFramework :: [String] -> String -> EBS String -> IO ExitCode
createUniversalFramework frameworkPaths outputDirectory (Left error) = return $ ExitFailure 1
createUniversalFramework frameworkPaths outputDirectory (Right executablePath) = do
    createBaseFramework frameworkPaths outputDirectory
    lipo $ ["-create", "-output", outputDirectory </> executablePath] ++ executablePaths where
        executablePaths = getFrameworkExecutablePaths (takeFileName executablePath) frameworkPaths

createBaseFramework :: [String] -> String -> IO ExitCode
createBaseFramework frameworkPaths outputDirectory = do
    let baseFrameworkDirectory = head frameworkPaths
    let outputFrameworkDirectory = outputDirectory </> takeFileName baseFrameworkDirectory
    createDirectoryIfMissing False outputFrameworkDirectory
    putStrLn outputFrameworkDirectory
    copyDirectory baseFrameworkDirectory outputFrameworkDirectory

copyDirectory :: String -> String -> IO ExitCode
--copyDirectory src dst = cp ["-pR", src, dst]
copyDirectory src dst = ditto [src, dst]

copyDirectory1 :: String -> String -> IO ExitCode
--copyDirectory src dst = cp ["-pR", src, dst]
copyDirectory1 src dst = ditto [src, dst]

getFrameworkExecutablePaths :: String -> [String] -> [String]
getFrameworkExecutablePaths executableName = map (</> executableName)

---- get executable folder path and executable name --
getExecutablePath :: XcodeScheme -> IO (Either (BuildError String) String)
getExecutablePath xcodeScheme = do
    r <- readBuildSettings xcodeScheme
    return $ getExecutablePath' xcodeScheme r

getExecutablePath' :: (MonadError (BuildError String) m) => XcodeScheme -> (ExitCode, String, String) -> m String
getExecutablePath' xcodeScheme (ExitSuccess, output, error) = case value of
    Just xs -> return xs
    Nothing -> throwError $ CreateBuildOptionSetsNoneSupportedPlatformsError ("scheme = " ++ error)
    where value = lookupBuildSettings "EXECUTABLE_PATH" output
getExecutablePath' xcodeScheme (_, output, error) = throwError $ CreateBuildOptionSetsNoneSupportedPlatformsError (debugInfo xcodeScheme ++ " , " ++ error)

debugInfo :: XcodeScheme -> String
debugInfo xcodeScheme = "project = " ++ project xcodeScheme ++ " , " ++ "scheme = " ++ scheme xcodeScheme

-----------------------------------------------------------------------------
-- Helpers
-----------------------------------------------------------------------------
-- | Options
baseOptions :: XcodebuildOptionSet -> [String]
baseOptions optionSet = ["-project", xProject optionSet,
                        "-configuration", xConfiguration optionSet,
                        "-scheme", xScheme optionSet,
                        "-sdk", xSdk optionSet]

cleanOptions :: XcodebuildOptionSet -> [String]
cleanOptions optionSet = baseOptions optionSet ++ ["clean"]

buildOptions :: XcodebuildOptionSet -> [String]
buildOptions optionSet = baseOptions optionSet ++ ["build"]

-----------------------------------------------------------------------------
-- | BuildSettings
readBuildSettings xcodeScheme = xcodebuild ["-project", project xcodeScheme,
                                           "-showBuildSettings",
                                     "-scheme", scheme xcodeScheme,
                                     "-configuration", configuration xcodeScheme]

lookupBuildSettings :: String -> String -> Maybe String
lookupBuildSettings key output
    | null settings    = Nothing
    | otherwise        = Just $ parseSettingValue settings where
        settings     = lookupBuildSettings' key output

lookupBuildSettings' :: String -> String -> [String]
lookupBuildSettings' key buildSettings = filter (hasKey key) $ lines buildSettings

hasKey key = List.isInfixOf (key ++ " = ")
parseSettingValue settings = (strip' . tail . snd) $ breakKeyValuePair $ head settings
strip' = T.unpack . T.strip . T.pack
breakKeyValuePair = break (== '=')

-----------------------------------------------------------------------------
-- | Monad
(>>>=) :: IO (EBS a) -> (a -> IO (EBS b)) -> IO (EBS b)
(>>>=) x f = x >>= forMM f

forMM ::  (a -> IO (EBS b)) -> EBS a -> IO (EBS b)
forMM f x = case x of
    Left error -> return $ Left error
    Right y -> f y

-----------------------------------------------------------------------------
-- | Async
newtype Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
   var <- newEmptyMVar
   forkIO (action >>= putMVar var)
   return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

-----------------------------------------------------------------------------
-- | System commands
ls :: [String] -> IO (ExitCode, String, String)
ls args = readProcessWithExitCode "ls" args ""

git :: [String] -> IO ExitCode
git = rawSystem "git"

xcodebuild :: [String] -> IO (ExitCode, String, String)
xcodebuild args = readProcessWithExitCode "xcodebuild" args ""

lipo :: [String] -> IO ExitCode
lipo = rawSystem "lipo"

cp :: [String] -> IO ExitCode
cp = rawSystem "cp"

mv :: [String] -> IO ExitCode
mv = rawSystem "mv"

ditto :: [String] -> IO ExitCode
ditto = rawSystem "ditto"
