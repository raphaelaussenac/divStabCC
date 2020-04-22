# Delete all objects in the work space
rm(list=ls(all=TRUE))

####################################################
## Data & Packages
####################################################
# Packages
library(ggplot2)
library(plyr) # pour la fonction "ddply"


# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC/data/futurClimate/allClimateData"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC/data/futurClimate/allClimateData"
  setwd(mainBasePath)
}

# load barycentre id
load('~/Documents/GitHub/divStabCC/data/barycentre.rdata')

####################################################
## extract climate values
####################################################

listTemp <- Sys.glob("*.rdata")

df <- data.frame()
for (i in listTemp){
  load(i)
  clim <- data[, c('ID_PET_MES', 'yr', 'Tannual', 'Pannual', 'DCMAXjun_jul_aug')]
  clim$rcp <- substr(i, 1, 5)
  clim$model <- substr(i, 7, 11)
  df <- rbind(df, clim)
}

####################################################
## define 3 20 years periods
####################################################

df$period <- NA
# df[df$yr <= 1971, "period"] <- "1952-1971"
df[df$yr >= 1986 & df$yr <= 2005, "period"] <- "1986-2005"
df[df$yr >= 2081 & df$yr <= 2100, "period"] <- "2081-2100"
df <- df[!is.na(df$period), ]

####################################################
## climate at barycentre
####################################################

bary <- ddply(df[df$ID_PET_MES == id, ], .(rcp, period), summarise, Tmean = mean(Tannual), Pmean = mean(Pannual), DCmean = mean(DCMAXjun_jul_aug))


####################################################
## plot
####################################################

# choose colors for rcp
cbPalette <- c("cadetblue3", "gold2")

df <- df[df$yr > 1975,]

ggplot() +
  geom_histogram(data = df, aes(Tannual, col = rcp, fill = rcp), alpha = 0.2, position = 'dodge') +
  geom_vline(data = bary, aes(xintercept = Tmean, col = rcp, linetype = rcp), size = 0.7) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  facet_wrap( ~ period, scales = 'fixed') +
  theme_bw() +
  xlab("annual temperature") +
  # theme(legend.position="bottom", legend.title=element_blank())
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
#

ggplot() +
  geom_histogram(data = df, aes(Pannual, col = rcp, fill = rcp), alpha = 0.2, position = 'dodge') +
  geom_vline(data = bary, aes(xintercept = Pmean, col = rcp, linetype = rcp), size = 0.7) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  facet_wrap( ~ period, scales = 'fixed') +
  theme_bw() +
  xlab("annual precipitation") +
  # theme(legend.position="bottom", legend.title=element_blank())
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
#

ggplot() +
  geom_histogram(data = df, aes(DCMAXjun_jul_aug, col = rcp, fill = rcp), alpha = 0.2, position = 'dodge') +
  geom_vline(data = bary, aes(xintercept = DCmean, col = rcp, linetype = rcp), size = 0.7) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  facet_wrap( ~ period, scales = 'fixed') +
  theme_bw() +
  xlab("summer DC") +
  # theme(legend.position="bottom", legend.title=element_blank())
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
#


# ggplot(data = df, aes(x = Tannual, y = Pannual)) +
#    # geom_point()
#    geom_density_2d()

####################################################
## climate evolution QC
####################################################

climEvolQC <- ddply(df, .(rcp, period), summarise, Tmean = mean(Tannual),
                                                   Pmean = mean(Pannual),
                                                   DCmean = mean(DCMAXjun_jul_aug),
                                                   Tvar = var(Tannual),
                                                   Pvar = var(Pannual),
                                                   DCvar = var(DCMAXjun_jul_aug))
# diff
diff45 <- climEvolQC[climEvolQC$rcp == 'rcp45', 3:ncol(climEvolQC)]
diff45[3,] <- diff45[2,] - diff45[1,]
diff45

diff85 <- climEvolQC[climEvolQC$rcp == 'rcp85', 3:ncol(climEvolQC)]
diff85[3,] <- diff85[2,] - diff85[1,]
diff85


####################################################
## climate evolution at barycentre
####################################################

climEvolQC <- ddply(df[df$ID_PET_MES == id, ], .(rcp, period), summarise, Tmean = mean(Tannual),
                                                   Pmean = mean(Pannual),
                                                   DCmean = mean(DCMAXjun_jul_aug),
                                                   Tvar = var(Tannual),
                                                   Pvar = var(Pannual),
                                                   DCvar = var(DCMAXjun_jul_aug))
# diff
diff45 <- climEvolQC[climEvolQC$rcp == 'rcp45', 3:ncol(climEvolQC)]
diff45[3,] <- diff45[2,] - diff45[1,]
diff45

diff85 <- climEvolQC[climEvolQC$rcp == 'rcp85', 3:ncol(climEvolQC)]
diff85[3,] <- diff85[2,] - diff85[1,]
diff85

# same plot but fot bary only (df[df$ID_PET_MES == id, ] instead of df)
ggplot() +
  geom_histogram(data = df[df$ID_PET_MES == id, ], aes(Tannual, col = rcp, fill = rcp), alpha = 0.2, position = 'dodge') +
  geom_vline(data = bary, aes(xintercept = Tmean, col = rcp, linetype = rcp), size = 0.7) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  facet_wrap( ~ period, scales = 'fixed') +
  theme_bw() +
  xlab("annual temperature") +
  # theme(legend.position="bottom", legend.title=element_blank())
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
#

ggplot() +
  geom_histogram(data = df[df$ID_PET_MES == id, ], aes(Pannual, col = rcp, fill = rcp), alpha = 0.2, position = 'dodge') +
  geom_vline(data = bary, aes(xintercept = Pmean, col = rcp, linetype = rcp), size = 0.7) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  facet_wrap( ~ period, scales = 'fixed') +
  theme_bw() +
  xlab("annual precipitation") +
  # theme(legend.position="bottom", legend.title=element_blank())
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
#

ggplot() +
  geom_histogram(data = df[df$ID_PET_MES == id, ], aes(DCMAXjun_jul_aug, col = rcp, fill = rcp), alpha = 0.2, position = 'dodge') +
  geom_vline(data = bary, aes(xintercept = DCmean, col = rcp, linetype = rcp), size = 0.7) +
  scale_color_manual(values=cbPalette) +
  scale_fill_manual(values=cbPalette) +
  facet_wrap( ~ period, scales = 'fixed') +
  theme_bw() +
  xlab("summer DC") +
  # theme(legend.position="bottom", legend.title=element_blank())
  theme(panel.grid.minor = element_blank(),
        # panel.grid.major = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        legend.title=element_blank(),
        panel.spacing = unit(20, 'pt'))
