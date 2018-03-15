# universalbuild
build iOS/tvOS universal/fat framework

**Usage:**

universalbuild (-p|--project name.xcodeproj) (-s|--scheme schemename)
                          (-c|--configuration configurationname)

**Example:**

    git clone https://github.com/cruisediary/Pastel.git
    cd Pastel
    universalbuild -p ./Pastel.xcodeproj -s Pastel -c Debug
    
**Installation:**

Download the latest binary release.

https://github.com/unchartedworks/universalbuild/releases/download/v0.1.1/universalbuild-0.1.1.tar.bz2

    tar -xvjf ./universalbuild-0.1.1.tar.bz2
    cd universalbuild-0.1.1
    ./install.sh

**More**

https://github.com/unchartedworks/universalbuild/blob/master/app/Main.hs
