# Import necessary packages
# These may need to be installed first
library("ggplot2")
library("scales")
library("reshape2")


dataNames <- c("blank")
dir <- "~/Dropbox/Genotype-phenotype/Experiments/GLY_SUC_Dilutions/2014Oct27/"
medianFile <- "results_glycerol/median_curves_ED582_glycerol.txt"
rawFile <- "finaldata2/blank.txt"
logisticFile <- "results_glycerol/logistic_curves_ED582_glycerol.txt"
saveImg <- T
timeOffset <- 24

# setwd() changes the current working directory
# Example below changes mine to where the data is stored
setwd(dir)

# Read in data as a table
# header=T : there is a header line
# sep="\t" : values are tab separated
# check.names=F : header names will be taken as is. There usually is a problem
#                 when numbers are part of the header
data <- read.table(medianFile, header=T, sep="\t", check.names=F)
data$type <- "Median"
data2 <- read.table(logisticFile, header=T, sep="\t", check.names=F)
data2$type <- "Logistic"
title <- "Median and Logistic-Glycerol"

# Stretch out the table so each row is a specific sample, well, time, and optical density value
meltData <- melt(data, id.vars=c("sample", "mainsource", "substrate", "well", "type"), variable.name="time", value.name="optical_density")
meltData2 <- melt(data2, id.vars=c("sample", "mainsource", "substrate", "well", "type"), variable.name="time", value.name="optical_density")
meltData <- rbind(meltData, meltData2)

meltData$well <- factor(meltData$well, levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                                                "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                                                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                                                "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                                                "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                                                "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                                                "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                                                "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))

meltData$sample <- factor(meltData$sample, levels=dataNames)

# Change the time value from character string to numeric values. This causes a problem when trying to plot
meltData$time <- as.numeric(as.character(meltData$time))
meltData$time <- meltData$time + timeOffset
meltData$optical_density[meltData$optical_density == 0] <- NA
meltData$optical_density[meltData$optical_density > 2] <- NA

# Create the plot using time in the x axis, optical density in the y axis,
# and grouping/coloring the values by Virus type
pl <- ggplot(meltData, aes(x=time, y=optical_density, colour=type))

# Here are several aesthetic changes to the graphs
# They include creating a line graph, a point graph, facetting based on substrate type, log2 transformation,
# and other aesthetic changes to the graph with colors, removing grid lines, labelling, etc.
pl + geom_line(alpha=0.80) + geom_point(size=1) + facet_wrap(~well + substrate, ncol=12) +
  coord_trans(y="log2") +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"),
        panel.background=element_blank(),
        axis.text=element_text(colour="black"),
        axis.title=element_text(face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major=element_blank(),
        panel.border=element_rect(colour="grey90", fill=NA),
        legend.key=element_rect(fill="white"),
        plot.title=element_text(face="bold")
    ) + 
    ggtitle(title) + xlab("Time (hr)") + ylab("OD600 nm") +
		scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "black"))

if( saveImg ) {
  ggsave(paste(title,".png", sep=""), width=33.87, height=25.40, units="cm", dpi=450)
}

# Individual replicate plots from raw curves
data <- read.table(rawFile, header=T, sep="\t", check.names=F)

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
meltData$time <- meltData$time + timeOffset
meltData$optical_density[meltData$optical_density == 0] <- NA
meltData$optical_density[meltData$optical_density > 2] <- NA

# Create all figures
for (sampleName in dataNames) {
  title <- sampleName
  meltDataTemp <- meltData[grep(title, meltData$sample),]
  
  pl <- ggplot(meltDataTemp, aes(x=time, y=optical_density, colour=sample))
  pl + geom_line(alpha=0.80) + geom_point(size=1) + facet_wrap(~well, ncol=12) +
    #coord_trans(y="log2") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"),
          panel.background=element_blank(),
          axis.text.y=element_text(colour="black"),
          panel.grid.minor=element_blank(),
          panel.grid.major=element_blank(),
          axis.title.x=element_text(face="bold"),
          panel.border=element_rect(colour="grey90", fill=NA),
          axis.title.y=element_text(face="bold"),
          legend.key=element_rect(fill="white"),
          plot.title=element_text(face="bold")
      ) + 
      ggtitle(title) + xlab("Time (hr)") + ylab("OD600 nm") +
    	scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "black"))
  
  if( saveImg ) {
    ggsave(paste(title,".png", sep=""), width=33.87, height=25.40, units="cm", dpi=450)
  }
}
