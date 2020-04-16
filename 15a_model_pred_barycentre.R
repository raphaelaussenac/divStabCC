# Delete all objects in the work space
rm(list=setdiff(ls(), c("tex", "dra")))
start_time <- Sys.time()

####################################################
## Packages
####################################################
# https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
# Packages
library(ggplot2)
library(plyr)
library(merTools)
library(doParallel)

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
}

# function preparation données futures
source("./src/11_fun_model.r")

####################################################
## define DBH and competTot of the stand
####################################################
load("./data/dataBAI.rdata")
data$competTot <- data$competsoft + data$compethard
meanCompetTot <- mean(data$competTot)
meanDBH <- mean(data$DHP_CM)
rm(data)

####################################################
## prediction function
####################################################

fileNames <- Sys.glob(file.path("./data/futurClimate/allClimateData/barycentre", "*.rdata"))
fileNames <- fileNames

pred_mod <- function(s = c("SAB", "PET")){

  # load model
  if (s == "SAB"){
    load("./modelOutput/mod_SAB_lent_fin.rdata")
  } else if (s == "PET"){
    load("./modelOutput/mod_PET_lent_fin.rdata")
  }

  # predictions
  nsim <- 20
  predictions <- data.frame()
  for (i in fileNames){
    for (d in c(0, 0.5, 1)){ # levels of diversity
      load(i)
      # set variables to defined value
      data$plotyr <- NULL
      data$ID_PET_MES <- as.factor(1)
      data$ID_ARB <- as.factor(1)
      data$texture <- as.factor(2)
      data$drainage <- as.factor(1)
      data$ESSENCE <- s
      data$DHP_CM <- meanDBH
      # useless parameters to define (prevent bugue)
      data$prop_PET_BA <- 12
      data$prop_SAB_BA <- 12
      data$BAtot_CM2HA <- 12
      data$competsoft <- 12
      data$compethard <- 12
      # prepare data (scale, etc...) for predictions
      sp <- s
      div <- d
      newdata <- prepare_mod_data_futur_barycentre(data = data, sp = sp, div = div, meanCompetTot = meanCompetTot)
      rm(data) # free up memory
      # remove useless variables
      newdata[, c('plotyr', 'DHP_CM', 'prop_PET_BA', 'prop_SAB_BA', 'BAtot_CM2HA',
                'compethard', 'competsoft',
                'DCMAXjun_jul_aug', 'mixE')] <- NULL

      # pred <- predict(mod, re.form = NA, newdata = newdata)
      pred <- predictInterval(mod, newdata = newdata, which = "fixed", level = 0.95, n.sims = nsim, stat = "median", include.resid.var = FALSE, returnSims = TRUE)
      pred <- as.data.frame(attr(pred, "sim.results"))
      colnames(pred) <- paste('V', colnames(pred), sep = '')
      pred$yr <- newdata$yr
      rm(newdata) # free up memory
      # back transforming predictions: exp(log(BAI+1))
      pred[, colnames(pred)[substr(colnames(pred), 1, 1) == 'V']] <- exp(pred[, colnames(pred)[substr(colnames(pred), 1, 1) == 'V']]) - exp(log(1))
      pred$sp <- s
      pred$rcp <- substr(i, nchar(i)-16, nchar(i)-12)
      pred$mod <- substr(i, nchar(i)-10, nchar(i)-6)
      pred$div <- div
      pred <- pred[, c('sp', 'rcp', 'mod', 'div', 'yr', colnames(pred)[substr(colnames(pred), 1, 1) == 'V'])]
      predictions <- rbind(predictions, pred)
    }
  }
  save(predictions, file = paste("./modelOutput/barycentre/", s, ".rdata", sep = "")) # change name according to T and D
}

####################################################
## Run predictions on 2 cores
####################################################

cl <- makeCluster(2)
registerDoParallel(cl)
spe <- c("SAB", "PET")
foreach(i = 1:length(spe), .packages = "merTools") %dopar% {
  pred_mod(s = spe[i])
}

# this part might not be usefull ----------------
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister()
# -------------------------------------------------

stopCluster(cl)


end_time <- Sys.time()
end_time - start_time
