# Delete all objects in the work space
rm(list=ls(all=TRUE))
# library
library(ggplot2)


####################################################
# Data
####################################################
# File list
# Choose the work directory = folder
if (Sys.info()["sysname"] == "Darwin"){
  mainBasePath <- "/Users/raphaelaussenac/Documents/GitHub/divStabCC/modelOutput"
  setwd(mainBasePath)
} else if (Sys.info()["sysname"] == "Windows"){
  mainBasePath <- "C:/Users/raphael.aussenac/Documents/GitHub/divStabCC/modelOutput"
  setwd(mainBasePath)
}

####################################################
# TS, mean, var plot QC
####################################################

for (i in c("mean", "variance", "TS")){
  load(paste(i, "theoT0D0.rdata", sep = ""))

  ggplot()+
  geom_ribbon(data = p, aes(x = psab, ymax = Ymax, ymin = Ymin, fill = RCP), alpha = 0.5)+
  facet_wrap(~ plotp, nrow = 1)+
  theme_bw()+
  xlim(0,1)+
  theme(strip.background = element_rect(colour = "white", fill = "white"), legend.title=element_blank(), legend.position = "bottom",)+
  xlab("proportion of fir")+
  ylab("") #component)

  # ggsave(paste("~/Desktop/", i, "theoT0D0.pdf", sep = ""), width = 8, height = 5)

}

####################################################
# mean
####################################################
# mean list
multimean <- Sys.glob("meantheo*")
load(multimean[1])
pmean1 <- p
for (i in 2:length(multimean)){
  load(multimean[i])
  pmean1 <- rbind(pmean1, p)
}
pmean1$variable <- "mean"

# plot
ggplot(data = pmean1[pmean1$soil != 'T0D0', ])+
geom_ribbon(aes(x=psab, ymax=Ymax, ymin=Ymin, fill = RCP), alpha = 0.2)+
xlab("proportion of fir")+
ylab("")+
facet_grid(soil ~ plotp, scales = "fixed")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
# ggsave ("~/Desktop/chap3/plot/allplot.pdf", width = 8, height= 15)

####################################################
# variance
####################################################
# variance list
multivariance <- Sys.glob("variancetheo*")
load(multivariance[1])
pvariance1 <- p
for (i in 2:length(multivariance)){
  load(multivariance[i])
  pvariance1 <- rbind(pvariance1, p)
}
pvariance1$variable <- "variance"

# plot
ggplot(data = pvariance1[pvariance1$soil != 'T0D0', ])+
geom_ribbon(aes(x=psab, ymax=Ymax, ymin=Ymin, fill = RCP), alpha = 0.2)+
xlab("proportion of fir")+
ylab("")+
facet_grid(soil ~ plotp, scales = "free")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
# ggsave ("~/Desktop/chap3/plot/allplot.pdf", width = 8, height= 15)


####################################################
# TS
####################################################

# TS list
multiTS <- Sys.glob("TStheo*")
load(multiTS[1])
pTS1 <- p
for (i in 2:length(multiTS)){
  load(multiTS[i])
  pTS1 <- rbind(pTS1, p)
}
pTS1$variable <- "TS"


# plot
ggplot(data = pTS1[pTS1$soil != 'T0D0',])+
geom_ribbon(aes(x=psab, ymax=Ymax, ymin=Ymin, fill = RCP), alpha = 0.2)+
xlab("proportion of fir")+
ylab("")+
facet_grid(soil ~ plotp, scales = "fixed")+
theme_bw()+
theme(strip.background = element_rect(colour = "white", fill = "white"), legend.position = "bottom", legend.title = element_blank())
# ggsave ("~/Desktop/chap3/plot/allplot.pdf", width = 8, height= 15)
