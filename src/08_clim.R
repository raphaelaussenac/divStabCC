# Delete all objects in the work space
rm(list=setdiff(ls(), "mainBasePath"))

####################################################
## Data & Packages
####################################################
# Packages
library(plyr) # pour la fonction "ddply"

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  basePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC/data"
  setwd(basePath)
} else if (Sys.info()["sysname"] == "Windows"){
  basePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC/data"
  setwd(basePath)
}

# clim data
clim <- read.table("./Q1985-2005.txt", sep=";", header=T)
# format ID_PET_MES
range(nchar(clim$ID))
clim$ID <- formatC(clim$ID, width = 12, format = "fg", flag = "0")
range(nchar(clim$ID))
clim$yr <- substr(clim$Date, 1, 4)

# growth data
load(file="./dataBAIcompetfilt.rdata")


####################################################
## Tmean (annual)
####################################################

Tannual <- ddply(clim, .(ID, yr), summarise, Tannual=mean(TMean))
# calculate annual Temperature for previous year
Tannual$Tp <- NA
for (i in Tannual$ID){
  Tannual[Tannual$ID == i, 'Tp'] <- c(NA, Tannual[Tannual$ID == i, 'Tannual'][1: length(Tannual[Tannual$ID == i, 'Tannual'])-1])
}
Tannual <- Tannual[!is.na(Tannual$Tp),]
Tannual$plotyr <- paste(Tannual$ID, Tannual$yr, sep="")
Tannual_save <- Tannual
Tannual <- Tannual[, 3:5]

####################################################
## Pmean (annual)
####################################################

Pannual <- ddply(clim, .(ID, yr), summarise, Pannual=sum(TotalPrecip))
# calculate annual Temperature for previous year
Pannual$Pp <- NA
for (i in Pannual$ID){
  Pannual[Pannual$ID == i, 'Pp'] <- c(NA, Pannual[Pannual$ID == i, 'Pannual'][1: length(Pannual[Pannual$ID == i, 'Pannual'])-1])
}
Pannual <- Pannual[!is.na(Pannual$Pp),]
Pannual$plotyr <- paste(Pannual$ID, Pannual$yr, sep="")
Pannual_save <- Pannual
Pannual <- Pannual[, 3:5]

####################################################
## merge clim data with growth data
####################################################

data$plotyr <- paste(data$ID_PET_MES, data$AN_CERNE, sep="")
data <- merge(data, Tannual, by = "plotyr")
data <- merge(data, Pannual, by = "plotyr")

####################################################
## Tmean & Pmean (period)
####################################################

Tperiod <- ddply(Tannual_save, .(ID), summarise, Tperiod=mean(Tannual))
Pperiod <- ddply(Pannual_save, .(ID), summarise, Pperiod=mean(Pannual))

# merge clim data with growth data
data <- merge(data, Tperiod, by.x = "ID_PET_MES", by.y = "ID")
data <- merge(data, Pperiod, by.x = "ID_PET_MES", by.y = "ID")

####################################################
## save
####################################################

save(data, file="./dataBAIcompetfilt.rdata")
