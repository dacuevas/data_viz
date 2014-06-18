library("ggplot2")
library("reshape2")

setwd("~/Dropbox/Work/vdm_project/20140124_filtered/")
data <- read.table("curveinfo_filtered.txt", header=T, sep="\t", check.names=F)

pl <- ggplot(data, aes(x=growthlevel, y=maximumgrowthrate))
pl + geom_point(size=0.7) + #scale_x_continuous(breaks=c(0.3,0.4,0.5,0.75,1,1.25,1.5,1.75)) +
    scale_y_continuous(breaks=c(0.15,0.3,0.6)) + 
    theme(axis.text=element_text(colour="Black", size=8), axis.title=element_text(face="bold", size=12),
          panel.background=element_blank(), panel.grid.major=element_line(colour="grey"),
          panel.grid.minor=element_blank(), panel.border=element_rect(colour="Black", fill=NA),
          plot.title=element_text(face="bold")) +
    xlab("HM") + ylab("Maximum Growth Rate (OD/hr)")
    #geom_abline(intercept=0.0274,slope=0.1988) +
    #stat_smooth(method="lm")

cldata <- data
cldata$growthrate[cldata$maximumgrowthrate < 0.15] <- "< 0.15"
cldata$growthrate[cldata$maximumgrowthrate >= 0.15] <- ">= 0.15"
cpl <- ggplot(cldata, aes(x=growthrate, y=growthlevel))
cpl + geom_boxplot() + scale_y_continuous(breaks=c(0.4,0.5,1.0,1.5)) +
    theme(axis.text=element_text(colour="Black", size=8), axis.title=element_text(face="bold", size=12),
          panel.background=element_blank(), panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(), panel.border=element_rect(colour="Black", fill=NA),
          plot.title=element_text(face="bold")) +
    xlab("Maximum Growth Rate (OD/hr)") + ylab("HM") + coord_flip()
