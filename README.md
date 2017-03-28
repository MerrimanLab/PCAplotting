# PCAplotting
PCA plotting app

Created March 2017 by Ruth Topless with help from Murray Cadzow

either clone the repo or download and extract the zip archive for the repo



Use the finalPCAclusterapp.R with the PCAtestdata.txt


The lab data can be found on the server here (path to server data)

To run the app with the lab data change this line (line ~16) in finalPCAclusterapp.R

```
PCAtestdata <- read.delim("PCAtestdata.txt", stringsAsFactors = F)
```

to 
```
PCAtestdata <- read.delim("/path/to/server/data/", stringsAsFactors = F)
```
