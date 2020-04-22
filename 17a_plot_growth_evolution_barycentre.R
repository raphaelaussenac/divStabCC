# Delete all objects in the work space
rm(list=ls(all=TRUE))

# library
library(ggplot2)
library(reshape2)
library(plyr)

# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC"
  setwd(mainBasePath)
}

# load predictions
load('./modelOutput/barycentre/PET.rdata')
df <- predictions
load('./modelOutput/barycentre/SAB.rdata')
df <- rbind(df, predictions)
df <- df[df$div %in% c(0,1),]

################################################################################
# calculate envelopes
################################################################################

# wide to long
df <- melt(df, id.vars = c("sp", "rcp", "mod", "div", "yr"), measure.vars = colnames(df)[substr(colnames(df), 1, 1) == 'V'])

# change names
df[df$rcp == "rcp45", "rcp"] <- "RCP4.5"
df[df$rcp == "rcp85", "rcp"] <- "RCP8.5"
colnames(df)[colnames(df) == "variable"] <- "sim"
colnames(df)[colnames(df) == "value"] <- "BAI"

# choose colors for rcp
cbPalette <- c("cadetblue3", "gold2")

# min, max and CI for each RCP
df_min <- ddply(df, .(sp, yr, rcp, div), summarise, BAImin = min(BAI))
df_max <- ddply(df, .(sp, yr, rcp, div), summarise, BAImax = max(BAI))
df_CImin <- ddply(df, .(sp, yr, rcp, div), summarise, CImin = wilcox.test(BAI,conf.int=TRUE)$conf.int[1])
df_CImax <- ddply(df, .(sp, yr, rcp, div), summarise, CImax = wilcox.test(BAI,conf.int=TRUE)$conf.int[2])
dfMed <- ddply(df, .(sp, yr, rcp, div), summarise, med = median(BAI))
df <- cbind(df_min, 'BAImax' = df_max$BAImax, 'CImin' = df_CImin$CImin, 'CImax' = df_CImax$CImax, 'med' = dfMed$med)

# plot pure stands
ggplot(data = df[df$div %in% c(0, 1),])+
geom_ribbon(aes(x = yr, ymax = BAImax, ymin = BAImin, fill = rcp), alpha = 0.2) +
# geom_ribbon(aes(x = yr, ymax = CImax, ymin = CImin, fill = rcp), alpha = 0.5) +
geom_line(aes(x = yr, y = med, col = rcp), size = 0.5, alpha = 0.8) +
scale_y_continuous(breaks= seq(2.5, 20, by = 2.5), expand = c(0, 0)) +
scale_fill_manual(values=cbPalette) +
scale_color_manual(values=cbPalette) +
facet_wrap(sp ~ div, nrow = 1, scales = 'fixed') +
xlab('year')+
ylab('expected BAI (cm2)') +
theme_light() +
theme(panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(colour = 'black'),
      legend.position = "bottom",
      legend.title=element_blank(),
      panel.spacing = unit(20, 'pt'))
