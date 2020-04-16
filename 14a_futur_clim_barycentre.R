# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Data & Packages
####################################################
# Packages
detach("package:dplyr", unload=TRUE) # unload dplyr package if loaded / error message if not previously loaded
library(plyr) # pour la fonction "ddply"
library(reshape2) # pour la fonction melt
library(ggplot2)

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
}

# liste sites on which future climate is available
sites <- read.table("./data/futurClimate/liste_sites_clim_futur.csv", sep=",", header=T)
range(nchar(sites$ID_PET_MES))
sites$ID_PET_MES <- formatC(sites$ID_PET_MES, width = 12, format = "fg", flag = "0")
range(nchar(sites$ID_PET_MES))

# load barycentre id
load('./data/barycentre.rdata')

# load growth data
load("./data/dataBAI.rdata")
data <- data[data$ID_PET_MES == id,]
# keep only one tree
data <- data.frame(data[1,'ID_PET_MES'])
colnames(data) <- 'ID_PET_MES'


####################################################
## function formatage
####################################################

# DC max jun, jul, aug
funDC <- function(){
  colnames(DC) <- c("yr", "month", "day", sites$ID_PET_MES)
  DC <- melt(DC, id.vars=c("yr","month","day"))
  colnames(DC) <- c("yr", "month", "day", "ID_PET_MES", "DC")
  month <- c(6,7,8) # jun, jul, aug
  DCMAXjun_jul_aug <- ddply(DC[DC$month %in% month,], .(ID_PET_MES, yr), summarise, DCMAXjun_jul_aug=max(DC))
  DCMAXjun_jul_aug$plotyr <- paste(DCMAXjun_jul_aug$ID_PET_MES, DCMAXjun_jul_aug$yr, sep="")
  DCMAXjun_jul_aug <- DCMAXjun_jul_aug[,c("ID_PET_MES", "yr", "DCMAXjun_jul_aug", "plotyr")]
  return(DCMAXjun_jul_aug)
}

# Tmean annual
funT <- function(){
  colnames(Tannual) <- c("yr", sites$ID_PET_MES)
  Tannual <- melt(Tannual, id.vars="yr")
  colnames(Tannual) <- c("yr","ID_PET_MES", "Tannual")
  Tannual$plotyr <- paste(Tannual$ID_PET_MES, Tannual$yr, sep="")
  Tannual <- Tannual[,c("plotyr", "Tannual")]
  return(Tannual)
}

# Pmean annual
funP <- function(){
  colnames(Pannual) <- c("yr", sites$ID_PET_MES)
  Pannual <- melt(Pannual, id.vars="yr")
  colnames(Pannual) <- c("yr","ID_PET_MES", "Pannual")
  Pannual$plotyr <- paste(Pannual$ID_PET_MES, Pannual$yr, sep="")
  Pannual <- Pannual[,c("plotyr", "Pannual")]
  return(Pannual)
}

# DC t-1
funDCp <- function(data = data){
  DCp <- ddply(data, .(ID_PET_MES, yr), summarise, DCp = unique(DCMAXjun_jul_aug))
  range(DCp$yr)
  DCp$yr <- DCp$yr + 1
  DCp <- DCp[DCp$yr != 2101, ]
  DCp$plotyr <- paste(DCp$ID_PET_MES, DCp$yr, sep = "")
  data$plotyr <- paste(data$ID_PET_MES, data$yr, sep = "")
  data <- merge(data, DCp[, 3:4], by = "plotyr")
  return(data)
}

dim(data)
length(unique(data$ID_PET_MES))
length(unique(data$ID_ARB))

####################################################
## Format climatic data for all models
####################################################
setwd("./data/futurClimate/allClimateData")
fileNames <- Sys.glob("*.csv")
fileNames

# select the scenario and model
for (i in unique(substr(fileNames, 1, 11))){
  # DC
  DC <- read.table(paste(i, "_DC.csv", sep = ""), sep=",", header=T)
  DC <- funDC()

  # Tannual
  Tannual <- read.table(paste(i, "_Ta.csv", sep = ""), sep = ",", header = T)
  Tannual <- funT()

  # Pannual
  Pannual <- read.table(paste(i, "_Pa.csv", sep = ""), sep = ",", header = T)
  Pannual <- funP()

  # merge DC, T and P
  mod <- merge(DC, Tannual, by = "plotyr")
  mod <- merge(mod, Pannual, by = "plotyr")
  mod <- mod[, -1]

  # keep only barycentre plot
  data <- mod[mod$ID_PET_MES == id,]
  data <- funDCp(data)
  save(data, file = paste('./barycentre/',i, ".rdata", sep = ""))
}
