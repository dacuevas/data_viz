library("ggplot2")
library("scales")
library("reshape2")
library("plyr")


setwd("~/Dropbox/Work/diversestrains/pmfiles/vibrio/")
data <- read.table("separatedcurves.txt", header=T, sep="\t", check.names=F)
data <- data[,c(-4)]
cl <- "EDT2405"; sub <- "L-Alanine"
cl <- "EDT2409"; sub <- "L-Alanine"
cl <- "EDT2420"; sub <- L-Alanine"
cl <- "EDT2420"; sub <- "Potassium Sorbate"
cl <- "EDT2431"; sub <- "L-Glutamic Acid"
cl <- "EDT2496"; sub <- L-Glutamic Acid"

cl <- "EDT2240"; sub <- "D-Alanine"
cl <- "EDT2240"; sub <- "L-Alanine"


cl <- "EDT2401"; sub <- "Negative Control"

sub <- "D-Mannose"

data_mod <- data[grep(cl,data$clone),]
data_mod <- subset(data_mod, main_source == "Carbon")
#meltData <- melt(data_mod, id.vars=c("clone", "substrate", "main_source"), variable.name="time", value.name="optical_density")
meltData <- melt(data, id.vars=c("clone", "substrate", "main_source"), variable.name="time", value.name="optical_density")

meltData$time <- as.numeric(as.character(meltData$time))
names(meltData)[names(meltData) == "clone"] <- c("Clone")
meltData$main_source <- revalue(meltData$main_source,c("Carbon"="C","Nitrogen"="N"))

# Get specifc substrate
df <- meltData[meltData$substrate == sub,]

pl <- ggplot(meltData, aes(x=time, y=optical_density, colour=Clone, group=Clone))
#pl <- ggplot(df, aes(x=time, y=optical_density, colour=Clone, group=Clone))
pl + geom_line() + facet_wrap(~main_source+substrate, ncol=12) + coord_trans(y="log2") + #annotation_logticks(base=2, sides="l", scaled=F, alpha=0.5) +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"), panel.background=element_blank(),
          axis.text.y=element_text(colour="black"), panel.grid.minor=element_blank(), 
          panel.grid.major=element_blank(), axis.title.x=element_text(face="bold"),
          panel.border=element_rect(colour="grey90", fill=NA),
          axis.title.y=element_text(face="bold", angle=0), legend.key=element_rect(fill="white"),
          plot.title=element_text(face="bold")) + 
    ggtitle("Vibrio") + xlab("Time (hours)") + ylab(expression(paste("OD",600[nm], sep=""))) + scale_x_continuous(breaks=c(0.5, 10.0, 20.0, 30.0)) +
    scale_y_continuous(breaks=c(0.1,0.2,0.4,0.8)) +
    #scale_y_continuous(breaks=trans_breaks("log2", function(x) 2^x),labels = trans_format('log2', math_format(2^.x))) +
    scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "turquoise2", "black", "violetred"))
    #scale_colour_discrete()
