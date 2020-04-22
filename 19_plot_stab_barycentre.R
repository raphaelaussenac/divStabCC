# Delete all objects in the work space
rm(list=setdiff(ls(), c("tex", "dra")))

####################################################
## Data & Packages
####################################################

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
}

# library
library(reshape2)
library(plyr)
library(ggplot2)

# load BAI predictions
load('./modelOutput/barycentre/PET.rdata')
PET <- predictions
load('./modelOutput/barycentre/SAB.rdata')
SAB <- predictions
df <- rbind(PET, SAB)
df$div <- round(df$div, 3)

####################################################
## tree mean growth for each sp, rcp, mod, div, yr
## (aggregate all V1, V2, V3 simulations)
####################################################

# wide to long
df <- melt(df, id.vars = c('sp', 'rcp', 'mod', 'div', 'yr'), measure.vars = colnames(df)[substr(colnames(df), 1, 1) == 'V'])

# define periods
df$period <- 'p'
df[df$yr <= 2005 & df$yr > 1985, 'period'] <- '1986-2005'
df[df$yr <= 2100 & df$yr > 2080, 'period'] <- '2081-2100'
df <- df[df$period != 'p',]

# calculate tree mean growth
df <- ddply(df, .(sp, rcp, mod, div, yr), summarise, treeGrowth = mean(value)) #useless with prediction from 'predict' function

####################################################
## create stand constituted of 100 trees
####################################################

# nb of trees in the stand
nTrees <- 100

# multiply tree growth by 0:nTrees depending on sp proportion
df$multipliFactor <- (1 - df$div) * nTrees
df$Growth <- df$treeGrowth * df$multipliFactor

# convert div in propotion of fir
df[df$sp == "SAB", 'div'] <- round(1 - df[df$sp == "SAB", 'div'], 3)
colnames(df)[colnames(df) == 'div'] <- 'propSAB'

####################################################
## calculate the expected mixed stand growth from
## sp (individuals) growth in pure stand
####################################################

purePET <- df[df$sp == 'PET' & df$propSAB == 0, ]
pureSAB <- df[df$sp == 'SAB' & df$propSAB == 1, ]


for (propSAB in seq(0, 1, 0.01)){

  if(propSAB != 0){
    tempPET <- purePET[purePET$propSAB == 0, ]
    tempPET$propSAB <- propSAB
    tempPET$Growth <- tempPET$Growth * (1 - propSAB)
    purePET <- rbind(purePET, tempPET)
  }

  if(propSAB != 1){
    tempSAB <- pureSAB[pureSAB$propSAB == 1, ]
    tempSAB$propSAB <- propSAB
    tempSAB$Growth <- tempSAB$Growth * propSAB
    pureSAB <- rbind(pureSAB, tempSAB)
  }

}

pure <- rbind(purePET, pureSAB)

# define periods
pure$period <- 'p'
pure[pure$yr <= 2005 & pure$yr > 1985, 'period'] <- '1986-2005'
pure[pure$yr <= 2100 & pure$yr > 2080, 'period'] <- '2081-2100'

# calculate sp growth cov
pureVarCov <- pure[, c('sp', 'rcp', 'mod', 'propSAB', 'yr', 'period', 'Growth')]
pureVarCov <- dcast(pureVarCov, rcp + mod + propSAB + yr  + period ~ sp)
pureVarCov <- ddply(pureVarCov, .(rcp, mod, propSAB, period), summarise, COV = cov(PET, SAB),
                                                               PETvar = sd(PET),
                                                               SABvar = sd(SAB))
#

# calculate stand total growth
pure <- ddply(pure, .(rcp, mod, propSAB, yr), summarise, standGrowth = sum(Growth))

# calculate mean and var of stand growth
pure$period <- 'p'
pure[pure$yr <= 2005 & pure$yr > 1985, 'period'] <- '1986-2005'
pure[pure$yr <= 2100 & pure$yr > 2080, 'period'] <- '2081-2100'

pureMean <- ddply(pure, .(rcp, mod, propSAB, period), summarise, standMeanGrowth = mean(standGrowth))
pureVar <- ddply(pure, .(rcp, mod, propSAB, period), summarise, standVar = sd(standGrowth))
pureDf <- cbind(pureMean, 'standVar' = pureVar[, 'standVar'])
pureDf$stab <- pureDf$standMeanGrowth / pureDf$standVar


####################################################
## calculate stand total growth from tree growth in mixture
####################################################

# define periods
df$period <- 'p'
df[df$yr <= 2005 & df$yr > 1985, 'period'] <- '1986-2005'
df[df$yr <= 2100 & df$yr > 2080, 'period'] <- '2081-2100'

# calculate sp growth cov
growthVarCov <- df[, c('sp', 'rcp', 'mod', 'propSAB', 'yr', 'period', 'Growth')]
growthVarCov <- dcast(growthVarCov, rcp + mod + propSAB + yr  + period ~ sp)
growthVarCov <- ddply(growthVarCov, .(rcp, mod, propSAB, period), summarise, COV = cov(PET, SAB),
                                                               PETvar = sd(PET),
                                                               SABvar = sd(SAB))

# calculate stand total growth
df <- ddply(df, .(rcp, mod, propSAB, yr), summarise, standGrowth = sum(Growth))

# define periods
df$period <- 'p'
df[df$yr <= 2005 & df$yr > 1985, 'period'] <- '1986-2005'
df[df$yr <= 2100 & df$yr > 2080, 'period'] <- '2081-2100'

# calculate mean and var of stand growth
standMean <- ddply(df, .(rcp, mod, propSAB, period), summarise, standMeanGrowth = mean(standGrowth))
standVar <- ddply(df, .(rcp, mod, propSAB, period), summarise, standVar = sd(standGrowth))
standDf <- cbind(standMean, 'standVar' = standVar[, 'standVar'])
standDf$stab <- standDf$standMeanGrowth / standDf$standVar

####################################################
## calculate differences between the 2 periods
####################################################

# calculate difference of mean, var and TS between the 2 periods
# mean
diffMean <-  dcast(standMean, rcp + mod + propSAB ~ period)
diffMean$diffMean <- ((diffMean[, '2081-2100'] * 100) / diffMean[, '1986-2005']) - 100
# var
diffVar <-  dcast(standVar, rcp + mod + propSAB ~ period)
diffVar$diffVar <- ((diffVar[, '2081-2100'] * 100) / diffVar[, '1986-2005']) - 100
# TS
diffTS <-  dcast(standDf, rcp + mod + propSAB ~ period)
diffTS$diffTS <- ((diffTS[, '2081-2100'] * 100) / diffTS[, '1986-2005']) - 100

# cov
diffCOV <-  dcast(growthVarCov[, c('rcp', 'mod', 'propSAB', 'period', 'COV')], rcp + mod + propSAB ~ period)
diffCOV$diffCOV <- ((diffCOV[, '2081-2100'] * 100) / diffCOV[, '1986-2005']) - 100

# PETvar
diffPET <-  dcast(growthVarCov[, c('rcp', 'mod', 'propSAB', 'period', 'PETvar')], rcp + mod + propSAB ~ period)
diffPET$diffPET <- ((diffPET[, '2081-2100'] * 100) / diffPET[, '1986-2005']) - 100

# SABvar
diffSAB <-  dcast(growthVarCov[, c('rcp', 'mod', 'propSAB', 'period', 'SABvar')], rcp + mod + propSAB ~ period)
diffSAB$diffSAB <- ((diffSAB[, '2081-2100'] * 100) / diffSAB[, '1986-2005']) - 100

####################################################
## calculate differences between pure and mixed
####################################################

pureTemp<- pureDf
colnames(pureTemp) <- paste(colnames(pureTemp), 'pure', sep ='')
diffMix <- cbind(pureTemp, standDf)
diffMix$diffGrowth <- ((diffMix$standMeanGrowth * 100) / diffMix$standMeanGrowthpure ) - 100
diffMix$diffVar <- ((diffMix$standVar * 100) / diffMix$standVarpure ) - 100

colnames(pureVarCov) <- paste(colnames(pureVarCov), 'pure', sep = '')
covVarMix <- cbind(pureVarCov, growthVarCov)
covVarMix$covdiff <- ((covVarMix$COV * 100) / covVarMix$COVpure ) - 100
covVarMix$PETvardiff <- ((covVarMix$PETvar * 100) / covVarMix$PETvarpure ) - 100
covVarMix$SABvardiff <- ((covVarMix$SABvar * 100) / covVarMix$SABvarpure ) - 100

########################################################################################################
## Plot Mean
####################################################

# choose colors for rcp
cbPalette <- c("cadetblue3", "gold2")

# plot Mean ------------------------------------------------------------------------------------
# min, max and CI for each RCP
df_min <- ddply(standDf, .(rcp, propSAB, period), summarise, min = min(standMeanGrowth))
df_max <- ddply(standDf, .(rcp, propSAB, period), summarise, max = max(standMeanGrowth))
df_CImin <- ddply(standDf, .(rcp, propSAB, period), summarise, CImin = quantile(standMeanGrowth,0.375))
df_CImax <- ddply(standDf, .(rcp, propSAB, period), summarise, CImax = quantile(standMeanGrowth,0.625))
dfMed <- ddply(standDf, .(rcp, propSAB, period), summarise, med = median(standMeanGrowth))
dfMean <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfMean) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.2) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_color_manual(values=cbPalette) +
scale_fill_manual(values=cbPalette) +
geom_smooth(data = subset(dfMean, period == "1986-2005"), aes(x = propSAB, y = CImax), method = lm, col = "black", linetype = "dashed", size = 0.5) +
facet_grid( ~ period) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))

ggsave ("~/Desktop/overyielding.pdf", width = 8, height= 5)


# plot expected mean from pure stands ------------------------------------------------------------------------------------
# min, max and CI for each RCP
df_min <- ddply(pureDf, .(rcp, propSAB, period), summarise, min = min(standMeanGrowth))
df_max <- ddply(pureDf, .(rcp, propSAB, period), summarise, max = max(standMeanGrowth))
df_CImin <- ddply(pureDf, .(rcp, propSAB, period), summarise, CImin = quantile(standMeanGrowth,0.375))
df_CImax <- ddply(pureDf, .(rcp, propSAB, period), summarise, CImax = quantile(standMeanGrowth,0.625))
dfMed <- ddply(pureDf, .(rcp, propSAB, period), summarise, med = median(standMeanGrowth))
dfpureMean <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot() +
geom_ribbon(data = dfMean, aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.2) +
geom_ribbon(data = dfpureMean, aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(data = dfMean, aes(x = propSAB, y = med, col = rcp), size = 1) +
geom_line(data = dfpureMean, aes(x = propSAB, y = med, col = rcp, linetype = rcp), size = 0.5, alpha = 0.8) +
scale_color_manual(values=cbPalette) +
scale_fill_manual(values=cbPalette) +
# geom_smooth(data = subset(dfMean, period == "1986-2005"), aes(x = propSAB, y = CImax), method = lm, col = "black", linetype = "dashed", size = 0.5) +
facet_grid( ~ period) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

# plot diff mean growth in pure vs mixed stands ------------------------------------------------------------------------------------
# min, max and CI for each RCP
df_min <- ddply(diffMix, .(rcp, propSAB, period), summarise, min = min(diffGrowth))
df_max <- ddply(diffMix, .(rcp, propSAB, period), summarise, max = max(diffGrowth))
df_CImin <- ddply(diffMix, .(rcp, propSAB, period), summarise, CImin = quantile(diffGrowth,0.375))
df_CImax <- ddply(diffMix, .(rcp, propSAB, period), summarise, CImax = quantile(diffGrowth,0.625))
dfMed <- ddply(diffMix, .(rcp, propSAB, period), summarise, med = median(diffGrowth))
dfDiffGrowth <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfDiffGrowth) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.2) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_color_manual(values=cbPalette) +
scale_fill_manual(values=cbPalette) +
facet_grid( ~ period) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
scale_y_continuous(breaks=seq(0, 30, by = 2.5)) +
xlab('proportion of fir')+
ylab('difference of BAI (cm2) between pure and mixed stands') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#
# plot mean growth diff 2000 / 2100------------------------------------------------------------------------------------
# min, max and CI for each RCP
df_min <- ddply(diffMean, .(rcp, propSAB), summarise, min = min(diffMean))
df_max <- ddply(diffMean, .(rcp, propSAB), summarise, max = max(diffMean))
df_CImin <- ddply(diffMean, .(rcp, propSAB), summarise, CImin = quantile(diffMean,0.375))
df_CImax <- ddply(diffMean, .(rcp, propSAB), summarise, CImax = quantile(diffMean,0.625))
dfMed <- ddply(diffMean, .(rcp, propSAB), summarise, med = median(diffMean))
dfDiffMean <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfDiffMean) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
scale_y_continuous(breaks=seq(-30, 10, by = 5)) +
xlab('proportion of fir')+
ylab('expected difference in BAI (%)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

########################################################################################################
## Plot stand Variance
####################################################

# plot Var ------------------------------------------------------------------------------------
# min, max and CI for each RCP
df_min <- ddply(standDf, .(rcp, propSAB, period), summarise, min = min(standVar))
df_max <- ddply(standDf, .(rcp, propSAB, period), summarise, max = max(standVar))
df_CImin <- ddply(standDf, .(rcp, propSAB, period), summarise, CImin = quantile(standVar,0.375))
df_CImax <- ddply(standDf, .(rcp, propSAB, period), summarise, CImax = quantile(standVar,0.625))
dfMed <- ddply(standDf, .(rcp, propSAB, period), summarise, med = median(standVar))
dfVar <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfVar) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
facet_grid( ~ period) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected standard deviation of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

# plot diff var growth in pure vs mixed stands ------------------------------------------------------------------------------------
# min, max and CI for each RCP
df_min <- ddply(diffMix, .(rcp, propSAB, period), summarise, min = min(diffVar))
df_max <- ddply(diffMix, .(rcp, propSAB, period), summarise, max = max(diffVar))
df_CImin <- ddply(diffMix, .(rcp, propSAB, period), summarise, CImin = quantile(diffVar,0.375))
df_CImax <- ddply(diffMix, .(rcp, propSAB, period), summarise, CImax = quantile(diffVar,0.625))
dfMed <- ddply(diffMix, .(rcp, propSAB, period), summarise, med = median(diffVar))
dfDiffVar <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfDiffVar) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.2) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_color_manual(values=cbPalette) +
scale_fill_manual(values=cbPalette) +
facet_grid( ~ period) +
# scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
# scale_y_continuous(breaks=seq(0, 30, by = 2.5)) +
xlab('proportion of fir')+
ylab('difference of BAI (cm2) between pure and mixed stands') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#
# diff variance between 200-2100
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(diffVar, .(rcp, propSAB), summarise, min = min(diffVar))
df_max <- ddply(diffVar, .(rcp, propSAB), summarise, max = max(diffVar))
df_CImin <- ddply(diffVar, .(rcp, propSAB), summarise, CImin = quantile(diffVar,0.375))
df_CImax <- ddply(diffVar, .(rcp, propSAB), summarise, CImax = quantile(diffVar,0.625))
dfMed <- ddply(diffVar, .(rcp, propSAB), summarise, med = median(diffVar))
dfDiffVar <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfDiffVar) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (%)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#
########################################################################################################
## Plot sp cov
####################################################

# plot cov
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, min = min(COV))
df_max <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, max = max(COV))
df_CImin <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, CImin = quantile(COV,0.375))
df_CImax <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, CImax = quantile(COV,0.625))
dfMed <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, med = median(COV))
dfCov <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfCov) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
facet_grid( ~ period) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

# plot diff cov pure and mixed
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(covVarMix, .(rcp, propSAB, period), summarise, min = min(covdiff))
df_max <- ddply(covVarMix, .(rcp, propSAB, period), summarise, max = max(covdiff))
df_CImin <- ddply(covVarMix, .(rcp, propSAB, period), summarise, CImin = quantile(covdiff,0.375))
df_CImax <- ddply(covVarMix, .(rcp, propSAB, period), summarise, CImax = quantile(covdiff,0.625))
dfMed <- ddply(covVarMix, .(rcp, propSAB, period), summarise, med = median(covdiff))
dfCovdiff <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfCovdiff) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
facet_grid( ~ period) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#
# plot diff cov 2000 / 2100
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(diffCOV, .(rcp, propSAB), summarise, min = min(diffCOV))
df_max <- ddply(diffCOV, .(rcp, propSAB), summarise, max = max(diffCOV))
df_CImin <- ddply(diffCOV, .(rcp, propSAB), summarise, CImin = quantile(diffCOV,0.375))
df_CImax <- ddply(diffCOV, .(rcp, propSAB), summarise, CImax = quantile(diffCOV,0.625))
dfMed <- ddply(diffCOV, .(rcp, propSAB), summarise, med = median(diffCOV))
dfCovdiff <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfCovdiff) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))

########################################################################################################
## Plot PET var
####################################################

# plot varPET
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, min = min(PETvar))
df_max <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, max = max(PETvar))
df_CImin <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, CImin = quantile(PETvar,0.375))
df_CImax <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, CImax = quantile(PETvar,0.625))
dfMed <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, med = median(PETvar))
dfPETVar <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfPETVar) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
facet_grid( ~ period) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

# plot diff PETvar pure and mixed
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(covVarMix, .(rcp, propSAB, period), summarise, min = min(PETvardiff))
df_max <- ddply(covVarMix, .(rcp, propSAB, period), summarise, max = max(PETvardiff))
df_CImin <- ddply(covVarMix, .(rcp, propSAB, period), summarise, CImin = quantile(PETvardiff,0.375))
df_CImax <- ddply(covVarMix, .(rcp, propSAB, period), summarise, CImax = quantile(PETvardiff,0.625))
dfMed <- ddply(covVarMix, .(rcp, propSAB, period), summarise, med = median(PETvardiff))
dfPETdiff <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfPETdiff) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
facet_grid( ~ period) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#
# plot diff PETvar 2000 / 2100
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(diffPET, .(rcp, propSAB), summarise, min = min(diffPET))
df_max <- ddply(diffPET, .(rcp, propSAB), summarise, max = max(diffPET))
df_CImin <- ddply(diffPET, .(rcp, propSAB), summarise, CImin = quantile(diffPET,0.375))
df_CImax <- ddply(diffPET, .(rcp, propSAB), summarise, CImax = quantile(diffPET,0.625))
dfMed <- ddply(diffPET, .(rcp, propSAB), summarise, med = median(diffPET))
dfPETdiff <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfPETdiff) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

########################################################################################################
## Plot SAB var
####################################################

# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, min = min(SABvar))
df_max <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, max = max(SABvar))
df_CImin <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, CImin = quantile(SABvar,0.375))
df_CImax <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, CImax = quantile(SABvar,0.625))
dfMed <- ddply(growthVarCov, .(rcp, propSAB, period), summarise, med = median(SABvar))
dfSABVar <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfSABVar) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
facet_grid( ~ period) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

# plot diff SABvar pure and mixed
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(covVarMix, .(rcp, propSAB, period), summarise, min = min(SABvardiff))
df_max <- ddply(covVarMix, .(rcp, propSAB, period), summarise, max = max(SABvardiff))
df_CImin <- ddply(covVarMix, .(rcp, propSAB, period), summarise, CImin = quantile(SABvardiff,0.375))
df_CImax <- ddply(covVarMix, .(rcp, propSAB, period), summarise, CImax = quantile(SABvardiff,0.625))
dfMed <- ddply(covVarMix, .(rcp, propSAB, period), summarise, med = median(SABvardiff))
dfSABdiff <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfSABdiff) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
facet_grid( ~ period) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

# plot diff SABvar 2000 / 2100
# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(diffSAB, .(rcp, propSAB), summarise, min = min(diffSAB))
df_max <- ddply(diffSAB, .(rcp, propSAB), summarise, max = max(diffSAB))
df_CImin <- ddply(diffSAB, .(rcp, propSAB), summarise, CImin = quantile(diffSAB,0.375))
df_CImax <- ddply(diffSAB, .(rcp, propSAB), summarise, CImax = quantile(diffSAB,0.625))
dfMed <- ddply(diffSAB, .(rcp, propSAB), summarise, med = median(diffSAB))
dfSABdiff <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfSABdiff) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected difference in the variance of BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#

####################################################
## Plot TS
####################################################

# plot TS ------------------------------------------------------------------------------------
# min, max and CI for each RCP
df_min <- ddply(standDf, .(rcp, propSAB, period), summarise, min = min(stab))
df_max <- ddply(standDf, .(rcp, propSAB, period), summarise, max = max(stab))
df_CImin <- ddply(standDf, .(rcp, propSAB, period), summarise, CImin = quantile(stab,0.375))
df_CImax <- ddply(standDf, .(rcp, propSAB, period), summarise, CImax = quantile(stab,0.625))
dfMed <- ddply(standDf, .(rcp, propSAB, period), summarise, med = median(stab))
dfStab <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfStab) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
scale_fill_manual(values=cbPalette) +
facet_grid( ~ period) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected temporal stability of BAI') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))



# min, max and CI for each RCP ------------------------------------------------------------------------------------
df_min <- ddply(diffTS, .(rcp, propSAB), summarise, min = min(diffTS))
df_max <- ddply(diffTS, .(rcp, propSAB), summarise, max = max(diffTS))
df_CImin <- ddply(diffTS, .(rcp, propSAB), summarise, CImin = quantile(diffTS,0.375))
df_CImax <- ddply(diffTS, .(rcp, propSAB), summarise, CImax = quantile(diffTS,0.625))
dfMed <- ddply(diffTS, .(rcp, propSAB), summarise, med = median(diffTS))
dfDiffTS <- cbind(df_min, 'max' = df_max$max, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

ggplot(data = dfDiffTS) +
geom_ribbon(aes(x = propSAB, ymax = max, ymin = min, fill = rcp), alpha = 0.1) +
# geom_ribbon(aes(x = propSAB, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.6) +
geom_line(aes(x = propSAB, y = med, col = rcp), size = 1) +
geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 2) +
scale_fill_manual(values=cbPalette) +
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
xlab('proportion of fir')+
ylab('expected difference in the temporal stability of BAI') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
#


###############################################################################################
# mean = f(sd)

library(viridis)



dfQtMean <- ddply(standDf, .(rcp, mod, propSAB, period), summarise, median = quantile(standMeanGrowth, 0.5))
colnames(dfQtMean) <- paste(colnames(dfQtMean), 'Mean', sep = '')
dfQtVar <- ddply(standDf, .(rcp, mod, propSAB, period), summarise, median = quantile(standVar, 0.5))
colnames(dfQtVar) <- paste(colnames(dfQtVar), 'Var', sep = '')
test <- cbind(dfQtMean, dfQtVar)
test$rcpmod <- paste(test$rcpMean, test$modMean, sep = '')


ggplot() +
geom_point(data = test, aes(x = medianVar, y = medianMean, col = propSABMean, shape = rcpMean), alpha = 0.7) +
# geom_point(data = test[test$rcpMean == 'rcp45',], aes(x = medianVar, y = medianMean, col = propSABMean), shape = 16) +
# geom_point(data = test[test$rcpMean == 'rcp85',], aes(x = medianVar, y = medianMean, col = propSABMean), shape = 17) +
scale_color_viridis(option = "D") +
facet_grid(rcpMean ~ periodMean) +
xlab('expected standard deviation of BAI (cm2)')+
ylab('expected BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
