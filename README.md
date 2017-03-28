# PCAplotting
PCA plotting app

Created March 2017 by Ruth Topless with help from Murray Cadzow

either clone the repo or download and extract the zip archive for the repo




Use the finalPCAclusterapp.R with the PCAtestdata.txt

Install the required libraries
```
if(!require("shiny")){
  install.packages("shiny")
}

if(!require("shinythemes")){
  install.packages("shinythemes")
}

if(!require("utils")){
  install.packages("utils")
}

if(!require("dplyr")){
  install.packages("dplyr")
}

if(!require("ggplot2")){
  install.packages("ggplot2")
}

if(!require("car")){
  install.packages("car")
}

if(!require("ggrepel")){
  install.packages("ggrepel")
}
```


The lab data can be found on the server here (path to server data)

To run the app with the lab data change this line (line ~16) in finalPCAclusterapp.R

```
PCAtestdata <- read.delim("PCAtestdata.txt", stringsAsFactors = F)
```

to 
```
PCAtestdata <- read.delim("/path/to/server/data/", stringsAsFactors = F)
```
