library("ggplot2")
library("reshape2")
library("plyr")

# Clone and substrates of interest
cl <- "EDT2401"
sub <- "L-Rhamnose"


setwd("~/Dropbox/Work/vdm_project/20140124_filtered//")
dataMed <- read.table("median_curves_filtered.txt", header=T, sep="\t", check.names=F)
dataMed <- dataMed[,c(-5)]
dataLog <- read.table("../edtcurves_separated3.txt", header=T, sep="\t", check.names=F)
dataLog <- dataLog[,c(-4)]
dataRep <- read.table("rawcurves.txt", header=T, sep="\t", check.names=F)
dataRep <- dataRep[,c(-4)]

# Process median data
dataMed_mod <- dataMed
dataMed_mod <- dataMed[grep(cl,dataMed$sample),]
#dataMed_mod <- subset(dataMed_mod, main_source == "Carbon")
dataMed_mod <- subset(dataMed_mod, growthcondition == sub)
meltDataMed <- melt(dataMed_mod, id.vars=c("sample", "growthcondition", "mainsource","well"), variable.name="time", value.name="optical_density")
meltDataMed$time <- as.numeric(as.character(meltDataMed$time))
names(meltDataMed)[names(meltDataMed) == "sample"] <- c("Clone")
meltDataMed$main_source <- revalue(meltDataMed$main_source, c("Carbon"="C", "Nitrogen"="N"))


# Process logistic data
dataLog_mod <- dataLog
dataLog_mod <- dataLog[grep(cl,dataLog$clone),]
#dataLog_mod <- subset(dataLog_mod, main_source == "Carbon")
dataLog_mod <- subset(dataLog_mod, substrate == sub)
meltDataLog <- melt(dataLog_mod, id.vars=c("clone", "substrate", "main_source"), variable.name="time", value.name="optical_density")
meltDataLog$time <- as.numeric(as.character(meltDataLog$time))
names(meltDataLog)[names(meltDataLog) == "clone"] <- c("Clone")
meltDataLog$main_source <- revalue(meltDataLog$main_source, c("Carbon"="C", "Nitrogen"="N"))


#Process raw replicate data
dataRep_mod <- dataRep
#dataRep_mod <- dataRep[grep(cl,dataRep$clone),]
#dataRep_mod <- subset(dataRep_mod, main_source == "Carbon")
dataRep_mod <- subset(dataRep_mod, substrate == sub)
meltDataRep <- melt(dataRep_mod, id.vars=c("clone", "substrate", "main_source"), variable.name="time", value.name="optical_density")
meltDataRep$time <- as.numeric(as.character(meltDataRep$time))
names(meltDataRep)[names(meltDataRep) == "clone"] <- c("Clone")
meltDataRep$main_source <- revalue(meltDataRep$main_source, c("Carbon"="C", "Nitrogen"="N"))

names(meltDataLog)[names(meltDataLog) == "main_source"] <- c("mainsource")
names(meltDataLog)[names(meltDataLog) == "substrate"] <- c("growthcondition")
meltDataMed <- meltDataMed[,c(-4)]
meltDataLog$curve <- "replicates"
meltDataMed$curve <- "median"
alldata <- rbind(meltDataLog, meltDataMed)

#pl <- ggplot(meltDataLog, aes(x=time, y=optical_density, colour=Clone, group=Clone))
pl <- ggplot(alldata, aes(x=time, y=optical_density, colour=Clone, group=Clone))
#pl + geom_line(alpha=0.8) + geom_point() + 
#geom_line(data=meltDataMed, alpha=0.8, colour="black") + geom_point(data=meltDataMed, colour="black") + coord_trans(y="log2") +
#pl + geom_line(size=1) + geom_point(data=meltDataMed, size=1, colour="black") + facet_wrap(~main_source + substrate, ncol=12) + coord_trans(y="log2") +
#pl <- ggplot(meltDataMed, aes(x=time, y=optical_density, colour=Clone, group=Clone))
#pl + geom_line() + facet_wrap(~main_source + substrate, ncol=12) + coord_trans(y="log2") +
pl + geom_line(alpha=0.8) + geom_point() + coord_trans(y="log2") +
theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"), panel.background=element_blank(),
          axis.text.y=element_text(colour="black"), panel.grid.minor=element_blank(), 
          panel.grid.major=element_blank(), axis.title.x=element_text(face="bold"),
          panel.border=element_rect(colour="grey90", fill=NA),
          axis.title.y=element_text(face="bold", angle=0), legend.key=element_rect(fill="white"),
          plot.title=element_text(face="bold")) + 
    ggtitle("EDT2401 - L-Rhamnose") + xlab("Time (hours)") + ylab(expression(paste("OD",600[nm], sep=""))) + scale_x_continuous(breaks=c(0.5, 10.0, 20.0, 30.0)) +
    #scale_y_continuous(breaks=c(0.1,0.2,0.4,0.8)) +
    #scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "turquoise2", "black", "violetred"))
    scale_colour_discrete()
