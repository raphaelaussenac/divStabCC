# Delete all objects in the work space
rm(list=setdiff(ls(), "mainBasePath"))
options(digits=16)

####################################################
##                  Data & Packages               ##
####################################################

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  basePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC/data"
  setwd(basePath)
} else if (Sys.info()["sysname"] == "Windows"){
  basePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC/data"
  setwd(basePath)
}

# sites list after filtering the cores
load("./dataBAIcompetfilt.rdata")
coord <- data

# list of the sites selected on the basis of the BA of species
load("./coord.rdata")


####################################################
##                  Map parameters                ##
####################################################
# devtools::install_github("dkahle/ggmap")
# devtools::install_github("hadley/ggplot2")
library(ggmap)
library(ggplot2)

# Set your API Key (get API key on google cloud platform)
if (Sys.info()["sysname"] == "Darwin"){
  path <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC/src/googleAPI.R"
} else if (Sys.info()["sysname"] == "Windows"){
  path <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC/src/googleAPI.R"
}
source(path)





# library(rgdal)
# reg  <- readOGR(dsn=".", layer="re05073g")   ### echelle sous-domaine (noms des shp)
# reg@data$id <- rownames(reg@data)
# reg.df <- fortify(reg)
# reg.df$id <- as.numeric(reg.df$id)

### centre de la carte
# lat <- c(44, 52)
# lon <- c(-80, -55)

borders <- c(bottom  = 45,
                 top     = 51.2,
                 left    = -79.7,
                 right   = -64)

                 #
                 #
                 # c("terrain",
                 #        "terrain-background", "terrain-labels", "terrain-lines", "toner",
                 #        "toner-2010", "toner-2011", "toner-background", "toner-hybrid",
                 #        "toner-labels", "toner-lines", "toner-lite", "watercolor"),



### télécharger fond de carte chez google
map <- get_stamenmap(borders, zoom = 7, maptype = "toner-lite")
# map <- get_map(location = c(lon = mean(lon), lat = mean(lat)), zoom = 6,
#                 maptype = "toner-background", source = "stamen")

### integrer fond de carte google dans ggplot
bckgrd <- ggmap(map)
# bckgrd <- ggmap(map)+
#   scale_x_continuous(limits = lon, expand = c(0, 0)) +
#   scale_y_continuous(limits = lat, expand = c(0, 0))

### parametres carte
theme = theme(panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  strip.background = element_blank(),
  panel.background = element_rect(fill = 'white'),
  legend.position = c(1,0),
  legend.justification=c(1,0),
  text = element_text(size=12),
  axis.text.x = element_text(size=10),
  legend.key = element_blank())

####################################################
##                        Map                     ##
####################################################
# all sites
bckgrd +
geom_point(data=coord,aes(LONG,LAT, color=Compo),size=1, alpha=0.5) +
theme_bw() +
theme +
xlab("longitude") + ylab("latitude") +
ggtitle("All sites with at least one core of one of the species")




#
#
# bckgrd +
# geom_point(data = coord, aes(LONG, LAT, color = Compo), size = 1, alpha = 1) +
# theme_bw()
# ggsave ("~/Desktop/map.pdf", width = 9, height= 6)
#
#
#
#
#
#
#
# # all sites color: proportion SAB
# bckgrd+
# geom_point(data=coord,aes(LONG,LAT, color=prop_SAB_BA),size=1, alpha=0.5)+
# scale_colour_gradient(low="black", high="green")+
# theme_bw()+
# theme+
# xlab("longitude") + ylab("latitude")+
# ggtitle("Proportion of A. balsema (%BA) within sites")
#
#
# # all sites color: proportion PET
# bckgrd+
# geom_point(data=coord,aes(LONG,LAT, color=prop_PET_BA),size=1, alpha=0.5)+
# scale_colour_gradient(low="black", high="green")+
# theme_bw()+
# theme+
# xlab("longitude") + ylab("latitude")+
# ggtitle("Proportion of P. tremuloïdes (%BA) within sites")
#
#
# # mixed
# bckgrd+
# geom_point(data=coord[coord$Compo=="MIXED",],aes(LONG,LAT, color=Compo),size=1, alpha=0.5)+
# theme_bw()+
# theme+
# xlab("longitude") + ylab("latitude")+
# ggtitle("Sites for which we have cores for both species")
#
# # Pure SAB
# bckgrd+
# geom_point(data=coord[coord$Compo=="SAB",],aes(LONG,LAT, color=Compo),size=1, alpha=0.5)+
# theme_bw()+
# theme+
# xlab("longitude") + ylab("latitude")+
# ggtitle("Sites for which we have only SAB cores")
#
# # Pure PET
# bckgrd+
# geom_point(data=coord[coord$Compo=="PET",],aes(LONG,LAT, color=Compo),size=1, alpha=0.5)+
# theme_bw()+
# theme+
# xlab("longitude") + ylab("latitude")+
# ggtitle("Sites for which we have only PET cores")
