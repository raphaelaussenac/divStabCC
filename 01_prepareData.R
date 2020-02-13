# Delete all objects in the work space
rm(list=ls(all=TRUE))

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  basePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC"
  setwd(basePath)
} else if (Sys.info()["sysname"] == "Windows"){
  basePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC"
  setwd(basePath)
}

####################################################
## foramt data
####################################################
source('./src/01_site_selection.R')
source('./src/02_merge.R')
source('./src/03_prepare_chron.R')
source('./src/04_BAI.R')
source('./src/05_compet.R')
source('./src/06_post-filtre.R')
source('./src/07_map.R')
source('./src/08_clim.R')
source('./src/09_DC.R')
source('./src/10_soil_sampling.R')
