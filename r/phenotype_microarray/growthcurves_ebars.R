# Import necessary packages
# These may need to be installed first
library("ggplot2")
library("scales")
library("reshape2")

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
											conf.interval=.95, .drop=TRUE) {
	require(plyr)
	
	# New version of length which can handle NA's: if na.rm==T, don't count them
	length2 <- function (x, na.rm=FALSE) {
		if (na.rm) sum(!is.na(x))
		else       length(x)
	}
	
	# This does the summary. For each group's data frame, return a vector with
	# N, mean, and sd
	datac <- ddply(data, groupvars, .drop=.drop,
								 .fun = function(xx, col) {
								 	c(N    = length2(xx[[col]], na.rm=na.rm),
								 		mean = mean   (xx[[col]], na.rm=na.rm),
								 		sd   = sd     (xx[[col]], na.rm=na.rm)
								 	)
								 },
								 measurevar
	)
	
	# Rename the "mean" column    
	datac <- rename(datac, c("mean" = measurevar))
	
	datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
	
	# Confidence interval multiplier for standard error
	# Calculate t-statistic for confidence interval: 
	# e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
	ciMult <- qt(conf.interval/2 + .5, datac$N-1)
	datac$ci <- datac$se * ciMult
	
	return(datac)
}

# setwd() changes the current working directory
# Example below changes mine to where the data is stored
setwd("~/sides/09may14/results_current/")

# Read in data as a table
# header=T : there is a header line
# sep="\t" : values are tab separated
# check.names=F : header names will be taken as is. There usually is a problem
#                 when numbers are part of the header
data <- read.table("raw_curves_current.txt", header=T, sep="\t", check.names=F)

# Stretch out the table so each row is a specific sample, well, time, and optical density value
meltData <- melt(data, id.vars=c("sample", "mainsource", "substrate", "well"), variable.name="time", value.name="optical_density")


meltData$well <- factor(meltData$well, levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
																								"B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
																								"C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
																								"D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
																								"E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
																								"F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
																								"G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
																								"H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))


# Change the time value from character string to numeric values. This causes a problem when trying to plot
meltData$time <- as.numeric(as.character(meltData$time))

dfc <- summarySE(meltData, measurevar="optical_density", groupvars=c("mainsource","substrate","time","well"))
pd <- position_dodge(.1)


# Create the plot using time in the x axis, optical density in the y axis.
# Here are several aesthetic changes to the graphs
# They include creating a line graph, a point graph, facetting based on substrate type, log2 transformation,
# and other aesthetic changes to the graph with colors, removing grid lines, labelling, etc.
ggplot(meltData, aes(x=time, y=optical_density, colour=mainsource)) +
  geom_line(alpha=0.80) +
  geom_point(size=1) +
  facet_wrap(~well + substrate, ncol=12) +
  coord_trans(y="log2") +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"),
				axis.text.y=element_text(colour="black"),
        axis.title.x=element_text(face="bold"),
				axis.title.y=element_text(face="bold"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(), 
				panel.grid.major=element_blank(),
				panel.border=element_rect(colour="grey90", fill=NA),
        legend.key=element_rect(fill="white"),
				plot.title=element_text(face="bold")
        ) + 
	ggtitle("Citrobacter sedlakii 119") + xlab("Time (hr)") + ylab("OD600 nm") +
	scale_colour_manual(values=c("#0072B2", "#D55E00"))

ggplot(dfc, aes(x=time, y=optical_density, colour=mainsource)) + 
	facet_wrap(~well + substrate, ncol=12) + coord_trans(y="log2") +
	geom_errorbar(aes(ymin=optical_density-se, ymax=optical_density+se), colour="grey", width=.1, position=pd) +
	geom_line(position=pd, alpha=0.8) +
	geom_point(position=pd, size=1) +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"),
				axis.text.y=element_text(colour="black"),
        axis.title.x=element_text(face="bold"),
				axis.title.y=element_text(face="bold"),
        panel.background=element_blank(),
        panel.grid.minor=element_blank(), 
				panel.grid.major=element_blank(),
				panel.border=element_rect(colour="grey90", fill=NA),
        legend.key=element_rect(fill="white"),
				plot.title=element_text(face="bold")
        ) +
	ggtitle("Citrobacter sedlakii 119") + xlab("Time (hr)") + ylab("OD600 nm") +
	scale_colour_manual(values=c("#0072B2", "#D55E00"))