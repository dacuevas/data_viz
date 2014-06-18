library("ggplot2")
library("reshape2")
library("plyr")
library("grid")

# Read in data
setwd("~/Dropbox/Work/vdm_project/20140124_filtered/")
data <- read.table("curveinfo_filtered.txt", sep="\t", header=T, check.names=F)

dglu <- subset(data, growthcondition == "D-Galactose")
dglu <- droplevels(dglu)
dglu_mean <- mean(dglu$growthlevel)
dglu_top <- dglu_mean + 2*sd(dglu$growthlevel)
dglu_bot <- dglu_mean - 2*sd(dglu$growthlevel)

dglu$category <- cut(dglu$growthlevel,
                     breaks=c(-Inf, dglu_bot, 0.4, dglu_top, Inf),
                     labels=c("Loss of Function","No Growth","Expected Growth","Gain of Function"))
dglu$category<- factor(dglu$category, levels=rev(levels(dglu$category)))

ggplot(dglu, aes(x=sample, y=growthlevel, group=growthcondition)) + 
  geom_point(aes(colour=category, shape=category), size=3) + 
  geom_line(alpha=0) + 
  scale_y_continuous(limits=c(0.2, 1.2)) +
	theme(panel.background=element_blank(),
        axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=8),
				axis.text.y=element_text(colour="black", size=8),
        axis.title=element_text(face="bold", size=10),
				panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        legend.title=element_text(size=10),
        legend.key=element_rect(fill=NA),
        legend.key.size=unit(.5,"cm"),
        legend.text=element_text(size=8),
        plot.title=element_text(face="bold", size=12)
        ) +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_colour_manual(values=c("Green","Royalblue","Black","Red")) +
	geom_hline(aes(yintercept=0.4),size=1) + 
  geom_hline(aes(yintercept=dglu_top), colour="Black", linetype="dashed") +
  geom_hline(aes(yintercept=dglu_mean), colour="Black", linetype="dashed", size=1) +
  geom_hline(aes(yintercept=dglu_bot), colour="Black", linetype="dashed") +
	ggtitle("D-galactose") + xlab("") + ylab("Growth Level")


