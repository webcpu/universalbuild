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

https://github.com/webcpu/universalbuild/releases/download/v1.2/universalbuild-1.2.zip

    unzip ./universalbuild-1.2.zip
    cd universalbuild-1.2
    ./install.sh

**More**

https://github.com/webcpu/universalbuild/blob/master/app/Main.hs
