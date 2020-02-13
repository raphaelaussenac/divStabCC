# Delete all objects in the work space
rm(list=ls(all=TRUE))

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
}

####################################################
## foramt data
####################################################
source('./src/01_site_selection.R')
setwd(mainBasePath)
source('./src/02_merge.R')
setwd(mainBasePath)
source('./src/03_prepare_chron.R')
setwd(mainBasePath)
source('./src/04_BAI.R')
setwd(mainBasePath)
source('./src/05_compet.R')
setwd(mainBasePath)
source('./src/06_post-filtre.R')
setwd(mainBasePath)
source('./src/07_map.R')
setwd(mainBasePath)
source('./src/08_clim.R')
setwd(mainBasePath)
source('./src/09_DC.R')
setwd(mainBasePath)
source('./src/10_soil_sampling.R')
