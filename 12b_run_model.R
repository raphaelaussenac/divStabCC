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
## Model PET
####################################################
load("./data/dataBAI.rdata")
source("./src/11_fun_model.r")
sp <- "PET"
data <- prepare_mod_data(sp = sp)
speed <- "rapid" # rapid/lent

mod <- run_mod_fin(speed = speed, sp = sp)
if (speed=="rapid"){
  save(mod, file="./modelOutput/mod_PET_rapid_fin.rdata")
} else if (speed=="lent"){
  save(mod, file="./modelOutput/mod_PET_lent_fin.rdata")
}

####################################################
## Model SAB
####################################################
load("./data/dataBAI.rdata")
source("./src/11_fun_model.r")
sp <- "SAB"
data <- prepare_mod_data(sp = sp)
speed <- "rapid" # rapid/lent

mod <- run_mod_fin(speed = speed, sp = sp)
if (speed=="rapid"){
  save(mod, file="./modelOutput/mod_SAB_rapid_fin.rdata")
} else if (speed=="lent"){
  save(mod, file="./modelOutput/mod_SAB_lent_fin.rdata")
}
