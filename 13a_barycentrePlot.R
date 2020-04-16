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

# load package
library(ggplot2)
library(plyr)

# load data
load("./data/dataBAI.rdata")

# retrieve site coordinates
coord <- ddply(data, .(ID_PET_MES), summarise, LONG = unique(LONG), LAT = unique(LAT))

# calculate barycentre =  centre of gravity
xa <- mean(coord$LONG)
ya <- mean(coord$LAT)

# mesaure distance between plots and barycentre
coord$dist <- sqrt(  ((xa - coord$LONG)^2) +  ((ya - coord$LAT)^2)    )

# identify nearest plot
id <- coord[coord$dist == min(coord$dist), 'ID_PET_MES']

# plot
ggplot()+
geom_point(data = coord, aes(LONG, LAT, col = dist)) +
scale_colour_gradient(low = "green", high = "blue")+
geom_point(aes(xa, ya), col = 'red') +
geom_point(aes(coord[coord$ID_PET_MES == id, 'LONG'], coord[coord$ID_PET_MES == id, 'LAT']), col = 'orange') +
coord_equal()

# save plot id
save(id, file="./data/barycentre.rdata")
