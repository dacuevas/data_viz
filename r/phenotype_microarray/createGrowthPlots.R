#!/usr/bin/Rscript

# Import necessary packages
# These may need to be installed first
suppressMessages(library("ggplot2"))
suppressMessages(require("scales"))
suppressMessages(require("reshape2"))
suppressMessages(require("getopt"))
suppressMessages(require("ggthemes"))
suppressMessages(require("plyr"))


#################################################################
# UTILITY FUNCTIONS
#################################################################

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
    										conf.interval=.95, .drop=TRUE) {

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

#################################################################
# ARGUMENT PARSING
#################################################################
spec <- matrix(c(
        "indir",    "i", 1, "character",    "Input directory (required)",
        "outdir",   "n", 1, "character",    "Output directory (required)",
        "suffix",   "o", 1, "character",    "Suffix appended to all output files (optional)",
        "plate",    "p", 0, "logical",      "Set flag if plate information is given",
        "type",     "f", 1, "character",    "Type of image file (png, eps, or svg) (optional)",
        "dpi",      "d", 1, "integer",      "DPI for image (optional)",
        "width",    "w", 1, "integer",      "Width for image (optional, required if height is specified)",
        "height",   "t", 1, "integer",      "Height for image (optional, requred if width is specified)",
        "help",     "h", 0, "logical",      "This help message"
        ), ncol=5, byrow=T)

opt <- getopt(spec)

# Check if help flag was given
if (!is.null(opt$help)) {
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}

# Check for input directory
if (is.null(opt$indir)) {
    cat("\nInput directory not specified. Use the '-i' option.\n\n")
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}

# Check for output directory
if (is.null(opt$outdir)) {
    cat("\nOutput directory not specified. Use the '-n' option.\n\n")
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}

# Check for width and height
if ((is.null(opt$width) && !is.null(opt$height))
    || (is.null(opt$height) && !is.null(opt$width))) {
    cat("\nYou must specify both height and width.\n\n")
    cat(paste(getopt(spec, usage=T), "\n"))
    q(status=1)
}

# Check image format and other metrics
if (!is.null(opt$type) && !(opt$type %in% c("png","svg","eps")) ) {
    cat("\nInvalid image type. You must specify either 'png', 'svg', or 'eps'.\n\n")
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
    opt$width <- 38  # In centimeters
} else if (opt$width > 50) {
    opt$width <- 50
}
if (is.null(opt$height)) {
    opt$height <- 25.40 # In centimeters
} else if (opt$height > 50) {
    opt$height <- 50
}

# Check output appendix
if (is.null(opt$suffix)) {
    opt$suffix<- "out"
}

# Check plate flag
if (is.null(opt$plate)) {
    plateFlag <- F
} else {
    plateFlag <- opt$plate
}

#################################################################
# DATA PROCESSING
#################################################################
# Individual replicate plots from raw curves
rawFile <- paste(opt$indir, "/raw_curves_", opt$suffix, ".txt", sep="")
logFile <- paste(opt$outdir, "/logistic_curves_sample_", opt$suffix, ".txt", sep="")

# Read in data as a table
# header=T : there is a header line
# sep="\t" : values are tab separated
# check.names=F : header names will be taken as is. There usually is a problem
#                 when numbers are part of the header
data <- read.table(rawFile, header=T, sep="\t", check.names=F)
logData <- read.table(logFile, header=T, sep="\t", check.names=F)

# Stretch out the table so each row is a specific sample, well, time, and optical density value
#meltData <- melt(data, id.vars=c("sample", "mainsource", "substrate", "well"), variable.name="time", value.name="OD")

data$well <- factor(data$well, levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                                        "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                                        "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                                        "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                                        "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                                        "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                                        "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                                        "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))
logData$well <- factor(logData$well, levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                                        "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                                        "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                                        "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                                        "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                                        "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                                        "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                                        "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))


# Change the time value from character string to numeric values. This causes a problem when trying to plot
data$time <- as.numeric(as.character(data$time))
logData$time <- as.numeric(as.character(logData$time))
# Force replicate value to be a character value
data$rep <- as.character(data$rep)
logData$rep <- as.character(logData$rep)

# Create all figures
dataNames <- unique(data$sample)
for (s in dataNames) {
    sd <- data[grep(s, data$sample, fixed=T),]

    # Create the plot using time in the x axis, optical density in the y axis.
    # Here are several aesthetic changes to the graphs
    # They include creating a line graph, a point graph, facetting based on substrate type, log2 transformation,
    # and other aesthetic changes to the graph with colors, removing grid lines, labelling, etc.
    print(paste("Generating raw plot for sample: '", s, "'...", sep=""))
    pl <- ggplot(sd, aes(x=time, y=od, colour=rep)) +
        facet_wrap(~well, ncol=12) +
        geom_line(size=0.8, alpha=0.8) +
        geom_point(size=1.5) +
    	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1, colour="black", size=12),
              axis.text.y=element_text(colour="black", size=12),
              axis.title.x=element_text(face="bold", size=15),
              axis.title.y=element_text(face="bold", size=15),
              panel.grid.major=element_blank(),
              strip.text=element_text(size=12),
              plot.title=element_text(face="bold")
            ) +
    	ggtitle(paste(s, "- raw data")) + xlab("Time (hr)") + ylab("OD (600nm)") +
        scale_color_tableau(name="Replicate")

    if( opt$type == "png" ) {
        ggsave(paste(opt$outdir, "/", s,".png", sep=""), plot=pl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else if( opt$type == "eps" ) {
        ggsave(paste(opt$outdir, "/", s,".eps", sep=""), plot=pl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else {
        ggsave(paste(opt$outdir, "/", s,".svg", sep=""), plot=pl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    }

    # Plot logistic fitted cure
    print(paste("Generating logistic plot for sample: '", s, "'...", sep=""))
    logsd <- logData[grep(s, logData$sample, fixed=T),]
    logpl <- ggplot(logsd, aes(x=time, y=od, colour=rep)) +
        facet_wrap(~well, ncol=12) +
        geom_line(size=0.8, alpha=0.8) +
        geom_point(size=1.5) +
    	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1, colour="black", size=12),
              axis.text.y=element_text(colour="black", size=12),
              axis.title.x=element_text(face="bold", size=15),
              axis.title.y=element_text(face="bold", size=15),
              panel.grid.major=element_blank(),
              strip.text=element_text(size=12),
              plot.title=element_text(face="bold")
            ) +
    	ggtitle(paste(s, "- logistic fitted")) + xlab("Time (hr)") + ylab("OD (600nm)") +
        scale_color_tableau(name="Replicate")

    if( opt$type == "png" ) {
        ggsave(paste(opt$outdir, "/log_", s,".png", sep=""), plot=logpl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else if( opt$type == "eps" ) {
        ggsave(paste(opt$outdir, "/log_", s,".eps", sep=""), plot=logpl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else {
        ggsave(paste(opt$outdir, "/log_", s,".svg", sep=""), plot=logpl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    }

    # Plot average with standard error bars
    print(paste("Generating average curve plot for sample: '", s, "'...", sep=""))
    grpvrs <- c("time", "well")
    if(plateFlag) {
        grpvrs <- c("mainsource", "compound", grpvrs)
    }
    if("plate" %in% colnames(sd)) {
        grpvrs <- c(grpvrs, "plate")
    }
    se.data <- summarySE(sd, measurevar="od", groupvars=grpvrs)
    sepl <- ggplot(se.data, aes(x=time, y=od)) +
        facet_wrap(~well, ncol=12) +
        geom_errorbar(aes(ymin=od-se, ymax=od+se), colour="grey30", width=.1) +
        geom_line(size=0.8, alpha=0.8, colour="#1F77B4") +
        geom_point(size=1.5, colour="#1F77B4") +
        theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1, colour="black", size=12),
              axis.text.y=element_text(colour="black", size=12),
              axis.title.x=element_text(face="bold", size=15),
              axis.title.y=element_text(face="bold", size=15),
              panel.grid.major=element_blank(),
              strip.text=element_text(size=12),
              plot.title=element_text(face="bold")
        ) +
        ggtitle(paste(s, "- average")) + xlab("Time (hr)") + ylab("OD (600nm)")

    if( opt$type == "png" ) {
        ggsave(paste(opt$outdir, "/avg_", s,".png", sep=""), plot=sepl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else if( opt$type == "eps" ) {
        ggsave(paste(opt$outdir, "/avg_", s,".eps", sep=""), plot=sepl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    } else {
        ggsave(paste(opt$outdir, "/avg_", s,".svg", sep=""), plot=sepl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
    }
}

# Make median
medianFile <- paste(opt$indir, "/all_curves_median_", opt$suffix,".txt", sep="")
medianData <- read.table(medianFile, header=T, sep="\t", check.names=F)
medianData$well <- factor(medianData$well, levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                                                  "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                                                  "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                                                  "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                                                  "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                                                  "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                                                  "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                                                  "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))


# Change the time value from character string to numeric values. This causes a problem when trying to plot
medianData$time <- as.numeric(as.character(medianData$time))

# Set linetypes
medianData$linetype = "solid"
medianData$linetype[grep("FULL", medianData$sample, fixed=T)] <- "dashed"

levels(medianData$sample) <- rev(levels(medianData$sample))

# Create the plot using time in the x axis, optical density in the y axis,
# and grouping/coloring the values by Virus type
print("Generating median plot of all samples")
medianpl <- ggplot(medianData, aes(x=time, y=od, colour=sample)) +
    facet_wrap(~well, ncol=12) +
    geom_line(size=0.8, alpha=0.8) +
    geom_point(size=1.5) +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=1, colour="black", size=12),
          axis.text.y=element_text(colour="black", size=12),
          axis.title.x=element_text(face="bold", size=15),
          axis.title.y=element_text(face="bold", size=15),
          panel.grid.major=element_blank(),
          strip.text=element_text(size=12),
          plot.title=element_text(face="bold")
        ) +
	ggtitle("Median") + xlab("Time (hr)") + ylab("OD (600nm)") +
    scale_colour_manual(name="Sample",
                        values=c("#1F77B4","#FF7F0E",
                                 "#2CA02C","#D62728",
                                 "#9467BD","#8C564B",
                                 "#E377C2","#7F7F7F",
                                 "#BCBD22","#17BECF",
                                 "#595959","#FFDD71"))

if( opt$type == "png" ) {
    ggsave(paste(opt$outdir, "/median_curves.png", sep=""), plot=medianpl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
} else if( opt$type == "eps" ) {
    ggsave(paste(opt$outdir, "/median_curves.eps", sep=""), plot=medianpl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
} else {
    ggsave(paste(opt$outdir, "/median_curves.svg", sep=""), plot=medianpl, width=opt$width, height=opt$height, units="cm", dpi=opt$dpi)
}
