#!/usr/bin/Rscript

# Import necessary packages
# These may need to be installed first
suppressMessages(library("ggplot2"))
library("scales")
library("reshape2")
library("getopt")


#################################################################
# ARGUMENT PARSING
#################################################################
spec <- matrix(c(
        "indir",    "i", 1, "character",    "Input directory (required)",
        "outdir",   "n", 1, "character",    "Output directory (required)",
        "out",      "o", 1, "character",    "Suffix appended to all output files (optional)",
        "type",     "f", 1, "character",    "Type of image file (png or svg) (optional)",
        "dpi",      "p", 1, "integer",      "DPI for image (optional)",
        "width",    "w", 1, "integer",      "Width for image (optional, required if height is specified)",
        "height",   "t", 1, "integer",      "Height for image (optional, requred if width is specified)",
        "samples",  "s", 1, "character",    "Comma separated list of sample names in the order to plot",
        "toffset",  "x", 1, "double",       "The time offset to begin the plot with. Usually when starting time is not 0 hours.",
        "medlog",   "m", 0, "logical",      "Include plot for median growth curves and logistic growth curves",
        "help",     "h", 0, "logical",      "This help message"
        ), ncol=5, byrow=T)
opt <- getopt(spec)

if (!is.null(opt$help)) {
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}

if (is.null(opt$indir)) {
    cat("\nInput directory not specified. Use the '-i' option.\n\n")
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}

if (is.null(opt$outdir)) {
    cat("\nOutput directory not specified. Use the '-n' option.\n\n")
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}

if ((is.null(opt$width) && !is.null(opt$height))
    || (is.null(opt$height) && !is.null(opt$width))) {
    cat("\nYou must specify both height and width.\n\n")
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}


if (!is.null(opt$type) && !(opt$type %in% c("png","svg")) ) {
    cat("\nInvalid image type. You must specify either 'png' or 'svg'.\n\n")
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
} else if (is.null(opt$type)) {
    opt$type <- "svg"
}
if (is.null(opt$dpi)) {
    opt$dpi <- 200
} else if (opt$dpi > 600) {
    opt$dpi <- 600
}
if (is.null(opt$width)) {
    opt$width <- 33.87  # Made to be used in centimeters
} else if (opt$width > 50) {
    opt$width <- 50
}
if (is.null(opt$height)) {
    opt$height <- 25.40 # Made to be used in centimeters
} else if (opt$height > 50) {
    opt$height <- 50
}
if (is.null(opt$out)) {
    opt$out <- "out"
}

if (!is.null(opt$samples)) {
    dataNames <- strsplit(opt$samples, ",")[[1]]
} else {
}

if (is.null(opt$toffset)) {
    opt$toffset <- 0
}
#################################################################
# DATA PROCESSING
#################################################################
# Individual replicate plots from raw curves
rawFile <- paste(opt$indir, "/raw_curves_", opt$out, ".txt", sep="")
log <- paste(opt$outdir, "/logfile_", opt$out, ".txt", sep="")

# Read in data as a table
# header=T : there is a header line
# sep="\t" : values are tab separated
# check.names=F : header names will be taken as is. There usually is a problem
#                 when numbers are part of the header
data <- read.table(rawFile, header=T, sep="\t", check.names=F)

# Stretch out the table so each row is a specific sample, well, time, and optical density value
meltData <- melt(data, id.vars=c("sample", "mainsource", "substrate", "well"), variable.name="time", value.name="OD")

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
meltData$time <- meltData$time + opt$toffset
meltData$OD[meltData$OD == 0] <- NA
meltData$OD[meltData$OD > 2] <- NA

# Create all figures
for (sampleName in dataNames) {
  title <- sampleName
  meltDataTemp <- meltData[grep(title, meltData$sample),]
  
  pl <- ggplot(meltDataTemp, aes(x=time, y=OD, colour=sample))
  pl <- pl + geom_line(alpha=0.80) + geom_point(size=1) + facet_wrap(~well + substrate, ncol=12) +
    coord_trans(y="log2") +
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
      ggtitle(title) + xlab("Time (hr)") + ylab("OD 600nm") +
    	scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "black"))
  if( opt$type == "png" ) {
      ggsave(paste(title,".png", sep=""), plot=pl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else {
      ggsave(paste(title,".svg", sep=""), plot=pl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    }
}

# Make median and logistic plots
if (!is.null(opt$medlog)) {
    medianFile <- paste(opt$indir, "/median_curves_", opt$out,".txt", sep="")
    logisticFile <- paste(opt$indir, "/logistic_curves_", opt$out, ".txt", sep="")
    
    data.med <- read.table(medianFile, header=T, sep="\t", check.names=F)
    data.med$type <- "Median"
    data.log <- read.table(logisticFile, header=T, sep="\t", check.names=F)
    data.log$type <- "Logistic"
    title <- "Median and Logistic"
    if(is.null(opt$samples)) {
        dataNames <- as.vector(unique(data.med$sample))
    }
    
    # Stretch out the table so each row is a specific sample, well, time, and optical density value
    meltData.med <- melt(data.med, id.vars=c("sample", "mainsource", "substrate", "well", "type"), variable.name="time", value.name="OD")
    meltData.log <- melt(data.log, id.vars=c("sample", "mainsource", "substrate", "well", "type"), variable.name="time", value.name="OD")
    meltData <- rbind(meltData.med, meltData.log)
    
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
    meltData$time <- meltData$time + opt$toffset
    meltData$OD[meltData$OD == 0] <- NA
    meltData$OD[meltData$OD > 2] <- NA
    
    # Create the plot using time in the x axis, optical density in the y axis,
    # and grouping/coloring the values by Virus type
    pl <- ggplot(meltData, aes(x=time, y=OD, colour=type))
    
    # Here are several aesthetic changes to the graphs
    # They include creating a line graph, a point graph, facetting based on substrate type, log2 transformation,
    # and other aesthetic changes to the graph with colors, removing grid lines, labelling, etc.
    pl <- pl + geom_line(alpha=0.80) + geom_point(size=1) + facet_wrap(~well + substrate, ncol=12) +
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
        ggtitle(title) + xlab("Time (hr)") + ylab("OD 600nm") +
        scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "black"))
    
    if( opt$type == "png" ) {
      ggsave(paste(title,".png", sep=""), plot=pl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else {
      ggsave(paste(title,".svg", sep=""), plot=pl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    }
}