# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Data & Packages
####################################################
# Packages
library(ggplot2)

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC/data/futurClimate/allClimateData"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC/data/futurClimate/allClimateData"
  setwd(mainBasePath)
}

####################################################
## extract climate values
####################################################

listTemp <- Sys.glob("*.rdata")

df <- data.frame()
for (i in listTemp){
  load(i)
  clim <- data[, c('yr', 'Tannual', 'Pannual', 'DCMAXjun_jul_aug')]
  clim$rcp <- substr(i, 1, 5)
  clim$model <- substr(i, 7, 11)
  df <- rbind(df, clim)
}

####################################################
## define 3 20 years periods
####################################################

df$period <- NA
df[df$yr <= 1971, "period"] <- "1952-1971"
df[df$yr >= 1986 & df$yr <= 2005, "period"] <- "1986-2005"
df[df$yr >= 2081 & df$yr <= 2100, "period"] <- "2081-2100"
df <- df[!is.na(df$period), ]


####################################################
## plot
####################################################

df <- df[df$yr > 1975,]

ggplot() +
  geom_density(data = df, aes(Tannual, linetype = period, col = rcp), size = 1, alpha = 0.2) +
  theme_bw() +
  xlab("T") +
  theme(legend.position="bottom", legend.title=element_blank())

ggplot() +
  geom_density(data = df, aes(Pannual, linetype = period, col = rcp), size = 1, alpha = 0.2) +
  theme_bw() +
  xlab("P") +
  theme(legend.position="bottom", legend.title=element_blank())

ggplot() +
  geom_density(data = df, aes(DCMAXjun_jul_aug, linetype = period, col = rcp), size = 1, alpha = 0.2) +
  theme_bw() +
  xlab("DC") +
  theme(legend.position="bottom", legend.title=element_blank())

#
# ggplot(data = df, aes(x = Tannual, y = Pannual)) +
#    # geom_point()
#    geom_density_2d()

####################################################
## mean
####################################################

# temperature
mean(df[df$rcp == 'rcp45' & df$period == '1986-2005','Tannual'])
mean(df[df$rcp == 'rcp85' & df$period == '1986-2005','Tannual'])
mean(df[df$rcp == 'rcp45' & df$period == '2081-2100','Tannual'])
mean(df[df$rcp == 'rcp85' & df$period == '2081-2100','Tannual'])

# precipitation
mean(df[df$rcp == 'rcp45' & df$period == '1986-2005','Pannual'])
mean(df[df$rcp == 'rcp85' & df$period == '1986-2005','Pannual'])
mean(df[df$rcp == 'rcp45' & df$period == '2081-2100','Pannual'])
mean(df[df$rcp == 'rcp85' & df$period == '2081-2100','Pannual'])

# drought
mean(df[df$rcp == 'rcp45' & df$period == '1986-2005','DCMAXjun_jul_aug'])
mean(df[df$rcp == 'rcp85' & df$period == '1986-2005','DCMAXjun_jul_aug'])
mean(df[df$rcp == 'rcp45' & df$period == '2081-2100','DCMAXjun_jul_aug'])
mean(df[df$rcp == 'rcp85' & df$period == '2081-2100','DCMAXjun_jul_aug'])

####################################################
## variance
####################################################

# temperature
var(df[df$rcp == 'rcp45' & df$period == '1986-2005','Tannual'])
var(df[df$rcp == 'rcp85' & df$period == '1986-2005','Tannual'])
var(df[df$rcp == 'rcp45' & df$period == '2081-2100','Tannual'])
var(df[df$rcp == 'rcp85' & df$period == '2081-2100','Tannual'])

# precipitation
var(df[df$rcp == 'rcp45' & df$period == '1986-2005','Pannual'])
var(df[df$rcp == 'rcp85' & df$period == '1986-2005','Pannual'])
var(df[df$rcp == 'rcp45' & df$period == '2081-2100','Pannual'])
var(df[df$rcp == 'rcp85' & df$period == '2081-2100','Pannual'])

# drought
var(df[df$rcp == 'rcp45' & df$period == '1986-2005','DCMAXjun_jul_aug'])
var(df[df$rcp == 'rcp85' & df$period == '1986-2005','DCMAXjun_jul_aug'])
var(df[df$rcp == 'rcp45' & df$period == '2081-2100','DCMAXjun_jul_aug'])
var(df[df$rcp == 'rcp85' & df$period == '2081-2100','DCMAXjun_jul_aug'])
