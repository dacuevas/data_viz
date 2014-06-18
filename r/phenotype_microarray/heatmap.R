library("ggplot2")
library("reshape2")
library("grid")
library("plyr")

# Read in raw growth curves
setwd("~/Dropbox/Work/vdm_project/20140124_filtered/")
data <- read.table("median_curves_filtered.txt", sep="\t", header=T, check.names=F)
#data <- data[data$clone != "median", c(1,3:ncol(data))]
data <- subset(data, growthcondition == "D-Mannose" | growthcondition == "D-Galactose" | growthcondition == "Sucrose")


# Read in harmonic mean data for sorting
hmdata <- read.table("curveinfo_filtered.txt", sep="\t", header=T, check.names=F)
hmdata <- subset(hmdata, growthcondition == "D-Mannose" | growthcondition == "D-Galactose" | growthcondition == "Sucrose")
#hmdata <- subset(hmdata, growthcondition == "D-Fructose")

# Combine the datasets
alldata <- merge(subset(hmdata, select=c(sample,growthlevel)), data, by.x="sample", by.y="sample")

rm(hmdata)

# Reorder clones by harmonic mean
alldata$sample <- reorder(alldata$sample, alldata$growthlevel, FUN=mean)

#meltedData <- melt(alldata, id.vars=c("clone", "harmonicmean"), variable.name="time", value.name="optical_density")
meltedData <- melt(alldata, id.vars=c("sample", "mainsource", "growthcondition", "well", "growthlevel"), variable.name="time", value.name="optical_density")
meltedData <- subset(meltedData, time != "0.0")
meltedData$growthcondition <- revalue(meltedData$growthcondition, c("D-Galactose"="D-galactose","D-Mannose"="D-mannose"))
meltedData$growthcondition<- factor(meltedData$growthcondition, levels=c("Sucrose","D-galactose","D-mannose"))

# Pick out which clones to put at the top of the grahpic
#lvls <- levels(alldata$clone)
# Remove top clones from previous set and set factor
#meltedData$clone <- factor(meltedData$clone, levels=lvls)


# Plot out heatmap
pl <- ggplot(meltedData, aes(x=time, y=sample, fill=optical_density))
pl + geom_raster() + facet_wrap(~growthcondition) +
  scale_fill_gradientn(name="OD 600 nm", colours=c("black","white", "dodgerblue4"), limits=c(0, 1)) + 
  scale_x_discrete(breaks=c("1.0", "10.0", "20.0", "30.0")) +
	theme(axis.text.x=element_text(colour="Black"), 
    axis.text.x=element_text(colour="Black", size=8),
  	axis.text.y=element_blank(),
    axis.title=element_text(face="bold", size=10),
    axis.ticks.y=element_blank(),
    axis.ticks.x=element_line(colour="Black"),
		panel.background=element_blank(),
    panel.grid.minor=element_blank(),
		panel.border=element_rect(colour="grey30", fill=NA),
    legend.title=element_text(size=10),
		legend.key.size=unit(.5,"cm"),
    legend.text=element_text(size=8),
    strip.background=element_rect(colour="grey30", fill="White"),
    strip.text=element_text(face="bold"),
    plot.title=element_text(face="bold", size=12)
  ) +
	ggtitle("") + xlab("Time (hr)") + ylab("Clones")