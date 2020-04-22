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

# calculate stand total growth
df <- ddply(df, .(rcp, mod, propSAB, yr), summarise, standGrowth = sum(Growth))

# define periods
df$period <- 'p'
df[df$yr <= 2005 & df$yr > 1985, 'period'] <- '1986-2005'
df[df$yr <= 2100 & df$yr > 2080, 'period'] <- '2081-2100'
df <- df[df$period != 'p',]

# calculate mean and var of stand growth
standMean <- ddply(df, .(rcp, mod, propSAB, period), summarise, standMeanGrowth = mean(standGrowth))
standVar <- ddply(df, .(rcp, mod, propSAB, period), summarise, standVar = sd(standGrowth))
standDf <- cbind(standMean, 'standVar' = standVar[, 'standVar'])
standDf$stab <- standDf$standMeanGrowth / standDf$standVar

# calculate difference of mean, var and TS between the 2 periods
# mean
diffMean <-  dcast(standMean, rcp + mod + propSAB ~ period)
diffMean$diffMean <- ((diffMean[, '2081-2100'] * 100) / diffMean[, '1986-2005']) - 100
# var
diffVar <-  dcast(standVar, rcp + mod + propSAB ~ period)
diffVar$diffVar <- diffVar[, '2081-2100'] - diffVar[, '1986-2005']
# TS
diffTS <-  dcast(standDf, rcp + mod + propSAB ~ period)
diffTS$diffTS <- diffTS[, '2081-2100'] - diffTS[, '1986-2005']

# diffDf <- cbind(diffMean, 'diffVar' = diffVar[, 'diffVar'], 'diffTS' = diffTS[, 'diffTS'])


####################################################
## calculate envelopes
####################################################

# TODO:
# - remplacer enveloppes internes par médiane
# - diagramme soustraction entre courbe overyielding et diagonale (prod attendues des espèces en mélange)

# choose colors for rcp
cbPalette <- c("cadetblue3", "gold2")

# plot Mean
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

ggsave ("~/Desktop/overyielding.pdf", width = 8, height= 5)


# plot Var
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


# plot TS
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

# plot diff --------------------------------------------------------------------
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
scale_x_continuous(breaks=seq(0, 1, by = 0.1), expand = c(0, 0)) +
scale_y_continuous(breaks=seq(-30, 10, by = 5)) +
xlab('proportion of fir')+
ylab('expected difference in BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))

# min, max and CI for each RCP
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

# min, max and CI for each RCP
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
