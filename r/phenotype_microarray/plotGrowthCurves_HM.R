library("ggplot2")
library("reshape2")
library("plyr")


setwd("~/Dropbox/Work/vdm_project/hm_investigation/")
dataHMs <- read.table("hmbins.txt", header=T, sep="\t", check.names=F)

dataODs <- read.table("mediancurves.txt", header=T, sep="\t", check.names=F)
dataODs <- dataODs[dataODs$clone == "EDT2440" & dataODs$main_source == "Carbon", c(-4)]

mergeData <- merge(dataHMs, dataODs, by=c("clone","substrate"))
meltData <- melt(mergeData, id.vars=c("clone", "substrate", "main_source", "bin"), variable.name="time", value.name="optical_density")

meltData$time <- as.numeric(as.character(meltData$time))
names(meltData)[names(meltData) == "clone"] <- c("Clone")
cb_palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")

myColors <- c("#666666", "#000000", "#6600CC", "#0000FF", "#66CCCC",
              "#00FF33", "#006600", "#FF9900", "#CC3300", "#660000" )
names(myColors) <- c("0.1", "0.2", "0.3", "0.4", "0.5",
                     "0.6", "0.7", "0.8", "0.9", "gt1.0")


pl <- ggplot(meltData, aes(x=time, y=optical_density, colour=bin, group=substrate))
pl + geom_line() + coord_trans(y="log2") +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"), panel.background=element_blank(),
				axis.text.y=element_text(colour="black"), panel.grid.minor=element_blank(), 
				panel.grid.major=element_blank(), axis.title.x=element_text(face="bold"),
				panel.border=element_rect(colour="grey90", fill=NA),
				axis.title.y=element_text(face="bold", angle=0), legend.key=element_rect(fill="white"),
				plot.title=element_text(face="bold")) + 
	ggtitle("EDT2440") + xlab("Time (hours)") + ylab(expression(paste("OD",600[nm], sep=""))) + scale_x_continuous(breaks=c(0.5, 10.0, 20.0, 30.0)) +
    scale_y_continuous(breaks=c(0.1,0.2,0.4,0.8)) +
	#scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "turquoise2", "black", "violetred"))
	#scale_colour_discrete()
    #scale_colour_brewer(palette="YlOrRd")
    #scale_colour_manual(values=cb_palette)
    #scale_colour_brewer(palette="Dark2")
    scale_colour_manual(name="bin", values=myColors)


dataG <- read.table("carbon_comparisons.txt", header=T, sep="\t", check.names=F)
dataC <- read.table("carbon_classifications.txt", header=T, sep="\t", check.names=F)
mergeData2 <- merge(dataC, dataG, by=c("clone","growthcondition"))
names(mergeData2)[names(mergeData2) == "harmonicmean"] <- c("HM")
names(mergeData2)[names(mergeData2) == "maximumgrowthrate"] <- c("growth_rate")
#mergeData2 <- subset(mergeData2, clone != "EDT2420" & growthcondition != "Potassium Sorbate")
HMpl <- ggplot(mergeData2, aes(x=HM, y=growth_rate))
HMpl + geom_point()

clones2plot <- subset(mergeData2, HM < 0.5 & growth_rate > 0.2 & growth_rate <= 0.3, select=c(clone, growthcondition, growth_rate, HM))
