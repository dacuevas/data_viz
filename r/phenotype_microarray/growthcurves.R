# Import necessary packages
# These may need to be installed first
library("ggplot2")
library("scales")
library("reshape2")

# setwd() changes the current working directory
# Example below changes mine to where the data is stored
setwd("~/Google Drive/labwork/collabs/Lauren/arctic_20140312/dataforcurves/")

# Read in data as a table
# header=T : there is a header line
# sep="\t" : values are tab separated
# check.names=F : header names will be taken as is. There usually is a problem
#                 when numbers are part of the header
data <- read.table("140310_KAR-M_KAR-V_0C.txt", header=T, sep="\t", check.names=F)

# Stretch out the table so each row is a specific sample, well, time, and optical density value
meltData <- melt(data, id.vars=c("Virus", "Sample", "Media Source", "Substrates", "Well"), variable.name="time", value.name="optical_density")

meltData$well <- factor(meltData$well, levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                                                "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                                                "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                                                "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                                                "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                                                "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                                                "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                                                "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))

#meltData$clone <- factor(meltData$clone, levels=c("VRT5","VRT11"))

# Change the time value from character string to numeric values. This causes a problem when trying to plot
meltData$time <- as.numeric(as.character(meltData$time))

lims = c(60,80,100,120,140,160,180,200)
upper = lims[lims > max(meltData$time)][1]
#meltData$optical_density[meltData$optical_density <= 0] <- 0.07

# Create the plot using time in the x axis, optical density in the y axis,
# and grouping/coloring the values by Virus type
pl <- ggplot(meltData, aes(x=time, y=optical_density, colour=Virus))

# Here are several aesthetic changes to the graphs
# They include creating a line graph, a point graph, facetting based on substrate type, log2 transformation,
# and other aesthetic changes to the graph with colors, removing grid lines, labelling, etc.
pl + geom_line(alpha=0.80) + geom_point() + facet_wrap(~Substrates, ncol=12) + coord_trans(y="log2") +
    theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"), panel.background=element_blank(),
          axis.text.y=element_text(colour="black"), panel.grid.minor=element_blank(), 
          panel.grid.major=element_blank(), axis.title.x=element_text(face="bold"),
          panel.border=element_rect(colour="grey90", fill=NA),
          axis.title.y=element_text(face="bold"), legend.key=element_rect(fill="white"),
          plot.title=element_text(face="bold")) + 
    #scale_x_continuous(breaks=c(0,20,40,60,80,100,120,140,160,180,200), limits=c(0,upper)) +
    ggtitle("KAR-M & KAR-V (0°C)") + xlab("Time (hours)") + ylab("OD 600nm") +
    scale_colour_manual(values=c("#0072B2", "#D55E00"))