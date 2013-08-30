library("ggplot2")
library("reshape2")

# Read in raw growth curves
setwd("~/Projects/modeling/manuscript_07-23/")
data <- read.table("fructose.txt", sep="\t", header=T, check.names=F)
data <- data[data$clone != "median", c(1,3:ncol(data))]


# Read in harmonic mean data for sorting
hmdata <- read.table("maindata_clonestouse.txt", sep="\t", header=T, check.names=F)
hmdata <- subset(hmdata, growthcondition == "D-Fructose")

# Combine the datasets
alldata <- merge(subset(hmdata, select=c(clone,harmonicmean)), data, by.x="clone", by.y="clone")

rm(hmdata)

# Reorder clones by harmonic mean
alldata$clone <- reorder(alldata$clone, alldata$harmonicmean, FUN=mean)

meltedData <- melt(alldata, id.vars=c("clone", "harmonicmean"), variable.name="time", value.name="optical_density")

# Pick out which clones to put at the top of the grahpic
lvls <- levels(alldata$clone)
# Remove top clones from previous set and set factor
meltedData$clone <- factor(meltedData$clone, levels=lvls)


# Plot out heatmap
pl <- ggplot(meltedData, aes(x=time, y=clone, fill=optical_density))

pl + geom_raster() + scale_fill_gradientn(colours=c("darkred","yellow", "darkgreen")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("darkred", "orange", "yellow", "darkgreen")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("firebrick", "orange", "yellow", "darkgreen")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("goldenrod", "darkred")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("moccasin", "darkred")) + 
#pl + geom_raster() + scale_fill_gradientn(colours=c("grey97", "black")) + 
	theme(axis.text.x=element_text(angle=90, colour="Black", family="Arial", size=14, hjust=1, vjust=0.5), 
				axis.text.y=element_text(colour="Black", family="Arial", size=12), axis.title.y=element_text(face="bold", size=14), strip.text=element_text(size=14), 
				panel.background=element_blank(), panel.grid.minor=element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=16, family="Arial")) + 
	ggtitle("Growth Dynamics\nD-Fructose") + xlab("Time (hours)") + ylab("")