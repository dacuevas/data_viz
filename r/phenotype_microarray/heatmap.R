library("ggplot2")
library("reshape2")

# Read in raw growth curves
setwd("~/Dropbox/Work/vdm_project/20140124_filtered/")
data <- read.table("median_curves_filtered.txt", sep="\t", header=T, check.names=F)
#data <- data[data$clone != "median", c(1,3:ncol(data))]
data <- subset(data, growthcondition == "D-Galactose")


# Read in harmonic mean data for sorting
hmdata <- read.table("curveinfo_filtered.txt", sep="\t", header=T, check.names=F)
hmdata <- subset(hmdata, growthcondition == "D-Galactose")
#hmdata <- subset(hmdata, growthcondition == "D-Fructose")

# Combine the datasets
alldata <- merge(subset(hmdata, select=c(sample,growthlevel)), data, by.x="sample", by.y="sample")

rm(hmdata)

# Reorder clones by harmonic mean
alldata$sample <- reorder(alldata$sample, alldata$growthlevel, FUN=mean)

#meltedData <- melt(alldata, id.vars=c("clone", "harmonicmean"), variable.name="time", value.name="optical_density")
meltedData <- melt(alldata, id.vars=c("sample", "mainsource", "growthcondition", "well", "growthlevel"), variable.name="time", value.name="optical_density")
meltedData <- subset(meltedData, time != "0.0")

# Pick out which clones to put at the top of the grahpic
#lvls <- levels(alldata$clone)
# Remove top clones from previous set and set factor
#meltedData$clone <- factor(meltedData$clone, levels=lvls)


# Plot out heatmap
pl <- ggplot(meltedData, aes(x=time, y=sample, fill=optical_density))

#pl + geom_raster() + scale_fill_gradientn(colours=c("black","#FFFF00")) +
pl + geom_raster() + scale_fill_gradientn(colours=c("black","white", "dodgerblue4")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("darkred","yellow", "darkgreen")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("darkred", "orange", "yellow", "darkgreen")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("firebrick", "orange", "yellow", "darkgreen")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("goldenrod", "darkred")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("moccasin", "darkred")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("grey97", "black")) + 
	theme(axis.text.x=element_text(angle=90, colour="Black", hjust=1, vjust=0.5), 
				axis.text.y=element_text(colour="Black"), axis.title.y=element_text(face="bold"), 
				panel.background=element_blank(), panel.grid.minor=element_blank()) + 
	ggtitle("D-Galactose") + xlab("Time (hr)") + ylab("")