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
## new data
####################################################
new <- function(data = data){
  data$DHP_CM <- data$DHP_CM
  data$prop_PET_BA <- data$prop_PET_BA
  data$prop_SAB_BA <- data$prop_SAB_BA
  data$BAtot_CM2HA <- data$BAtot_CM2HA
  data$texture <- as.factor(data$texture) # 1, 2, 3 ou data$texture = 0
  data$drainage <- as.factor(data$drainage) # 1, 2, ou data$drainage = 0
  data$compethard <- data$compethard
  data$competsoft <- data$competsoft
  return(data)
}

####################################################
## prediction function
####################################################

fileNames <- Sys.glob(file.path("./data/futurClimate/allClimateData", "*.rdata"))
fileNames <- fileNames

pred_mod <- function(s = c("SAB", "PET")){

  # load model
  if (s == "SAB"){
    load("./modelOutput/mod_SAB_lent_fin.rdata")
  } else if (s == "PET"){
    load("./modelOutput/mod_PET_lent_fin.rdata")
  }

  # predictions
  nsim <- 10
  predictions <- as.data.frame(matrix(ncol = 9 + nsim))
  colnames(predictions) <- c("ID_PET_MES", "ID_ARB", "ESSENCE", "prop_SAB_BA","prop_PET_BA", "yr", paste("V", seq(1,nsim,1), sep=""), "rcp", "mod", "rcpmod" )
  for (i in fileNames){
    load(i)
    data <- new(data)
    sp <- s
    newdata <- prepare_mod_data_futur(data = data, sp = sp)
    rm(data) # free up memory

    # ############### model_select
    # fix <- model.matrix(~ BAtot_CM2HA + sizeE + mixE + compethard + competsoft + DC + DCp + Tannual + Pannual + texture + drainage + texture:drainage + mixE:BAtot_CM2HA + sizeE:DC + sizeE:DCp  + compethard:DC + competsoft:DC + compethard:DCp + competsoft:DCp + mixE:DC + mixE:DCp + texture:sizeE + texture:mixE + texture:compethard + texture:competsoft + texture:DC + texture:DCp + texture:Tannual + texture:Pannual + drainage:sizeE + drainage:mixE + drainage:compethard + drainage:competsoft + drainage:DC + drainage:DCp + drainage:Tannual + drainage:Pannual, data = newdata)
    # fix <- as.data.frame(fix)
    # newdata <- cbind(fix, newdata[, c("ID_PET_MES", "ID_ARB", "ESSENCE", 'prop_SAB_BA', 'prop_PET_BA', 'yr')])
    # ############### model_select

    # pred <- predict(mod, re.form = NA, newdata = newdata)
    pred <- predictInterval(mod, newdata = newdata, which = "fixed", level = 0.95, n.sims = nsim, stat = "median", include.resid.var = TRUE, returnSims = TRUE)
    pred <- as.data.frame(attr(pred, "sim.results"))
    newdata <- cbind(newdata, pred)
    rm(pred) # free up memory
    # back transforming predictions: exp(log(BAI+1))
    newdata[, paste("V", seq(1,nsim,1), sep="")] <- exp(newdata[, paste("V", seq(1,nsim,1), sep="")]) - exp(log(1))
    newdata$rcp <- substr(i, nchar(i)-16, nchar(i)-12)
    newdata$mod <- substr(i, nchar(i)-10, nchar(i)-6)
    newdata$rcpmod <- substr(i, nchar(i)-16, nchar(i)-6)
    model <- newdata[, colnames(predictions)]
    rm(newdata) # free up memory
    predictions <- rbind(predictions, model)
    rm(model) # free up memory
  }
  predictions <- predictions[-1, ]

  save(predictions, file = paste(getwd(), "/modelOutput/", "QC_BAI_", s, "_T0D0", ".rdata", sep = "")) # change name according to T and D

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
