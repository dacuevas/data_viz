library("ggplot2")
library("reshape2")
library("grid")
library("plyr")

# Read in raw growth curves
#setwd("~/Dropbox/Work/vdm_project/20140124_filtered/")
#data <- read.table("median_curves_filtered.txt", sep="\t", header=T, check.names=F)
setwd("~/Dropbox/Work/vdm_project/2014Jul22/")
data <- read.table("median_curves_2014Jul22.txt", sep="\t", header=T, check.names=F)
#data <- data[data$clone != "median", c(1,3:ncol(data))]
data <- subset(data, substrate == "D-Mannose" | substrate == "D-Galactose" | substrate == "Sucrose")


# Read in harmonic mean data for sorting
hmdata <- read.table("curveinfo_2014Jul22.txt", sep="\t", header=T, check.names=F)
hmdata <- subset(hmdata, substrate == "D-Mannose" | substrate == "D-Galactose" | substrate == "Sucrose")
#hmdata <- subset(hmdata, substrate == "D-Fructose")

# Combine the datasets
alldata <- merge(subset(hmdata, select=c(sample,growthlevel)), data, by.x="sample", by.y="sample")

rm(hmdata)

# Reorder clones by harmonic mean
alldata$sample <- reorder(alldata$sample, alldata$growthlevel, FUN=mean)


#meltedData <- melt(alldata, id.vars=c("clone", "harmonicmean"), variable.name="time", value.name="optical_density")
meltedData <- melt(alldata, id.vars=c("sample", "mainsource", "substrate", "well", "growthlevel"), variable.name="time", value.name="optical_density")
meltedData <- subset(meltedData, time != "0.0")
meltedData$substrate <- revalue(meltedData$substrate, c("D-Galactose"="D-galactose","D-Mannose"="D-mannose"))
meltedData$substrate<- factor(meltedData$substrate, levels=c("Sucrose","D-galactose","D-mannose"))

# Get average growth curve
for( su in c("Sucrose","D-Galactose","D-Mannose") ) {
    avgtmp <- alldata[grep(su, alldata$substrate),]
    well <- as.character(alldata$well[alldata$substrate == su][1])
    
    avgtmp2 <- colMeans(avgtmp[,7:ncol(avgtmp)])
    newrow <- data.frame("Average","Carbon",su,well,0,"0",0)
    colnames(newrow) <- colnames(meltedData)
    for( i in 1:length(avgtmp2) ) {
        newrow$time <- names(avgtmp2[i])
        newrow$optical_density <- avgtmp2[i]
        if( i ==1 & su == "D-Galactose") {
            newrow$substrate <- revalue(newrow$substrate, c("D-Galactose"="D-galactose"))
        } else if( i == 1 & su == "D-Mannose" ) {
            newrow$substrate <- revalue(newrow$substrate, c("D-Mannose"="D-mannose"))
        }
        meltedData <- rbind(meltedData, newrow)
    }
}
# Pick out which clones to put at the top of the grahpic
lvls <- levels(meltedData$sample)
top <- c("Average","EDT2416","EDT2239","EDT2440","EDT2441")
lvls <- lvls[! lvls %in% top]
# Remove top clones from previous set and set factor
meltedData$sample<- factor(meltedData$sample, levels=c(lvls,rev(top)))


# Plot out heatmap
pl <- ggplot(meltedData, aes(x=time, y=sample, fill=optical_density))
pl + geom_raster() + facet_wrap(~substrate) +
  scale_fill_gradientn(name="OD 600nm", colours=c("black","white", "dodgerblue4"), limits=c(0, 1)) + 
  scale_x_discrete(breaks=c("0.5", "10.0", "20.0", "30.0")) +
	theme(axis.text.x=element_text(colour="Black"), 
    axis.text.x=element_text(colour="Black", size=8),
    axis.text.y=element_text(colour="Black", size=10),
  	#axis.text.y=element_blank(),
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