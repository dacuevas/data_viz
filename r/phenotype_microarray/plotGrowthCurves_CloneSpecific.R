library("ggplot2")
library("reshape2")
library("plyr")


setwd("~/Dropbox/Work/vdm_project/")
data <- read.table("edtcurves_separated2.txt", header=T, sep="\t", check.names=F)
data <- data[,c(-4)]
data_mod <- data[grep("EDT2239",data$clone),]
data_mod <- subset(data_mod, main_source == "Carbon")
meltData <- melt(data_mod, id.vars=c("clone", "substrate", "main_source"), variable.name="time", value.name="optical_density")

meltData$time <- as.numeric(as.character(meltData$time))
names(meltData)[names(meltData) == "clone"] <- c("Clone")
pl <- ggplot(meltData, aes(x=time, y=optical_density, colour=Clone, group=Clone))
pl + geom_line() + facet_wrap(~substrate, ncol=12) + coord_trans(y="log2") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"), panel.background=element_blank(),
          axis.text.y=element_text(colour="black"), panel.grid.minor=element_blank(), 
          panel.grid.major=element_blank(), axis.title.x=element_text(face="bold"),
          panel.border=element_rect(colour="grey90", fill=NA),
          axis.title.y=element_text(face="bold", angle=0), legend.key=element_rect(fill="white"),
          plot.title=element_text(face="bold")) + 
    ggtitle("EDT2239") + xlab("Time (hours)") + ylab(expression(paste("OD",600[nm], sep=""))) + scale_x_continuous(breaks=c(0.5, 10.0, 20.0, 30.0)) +
    scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "turquoise2", "black", "violetred"))
    #scale_colour_discrete()
