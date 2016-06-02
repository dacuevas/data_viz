# Import necessary packages
# These may need to be installed first
suppressMessages(require("ggplot2"))
suppressMessages(require("scales"))
suppressMessages(require("reshape2"))
suppressMessages(require("getopt"))
suppressMessages(require("ggthemes"))
suppressMessages(require("plyr"))
suppressMessages(require("grid"))


#################################################################
# UTILITY FUNCTIONS
#################################################################

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean,
## and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the
##               variable to be summariezed
##   groupvars: a vector containing names of columns
##              that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the
##                  confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {

    # New version of length which can handle NA's:
    # if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) {
            sum(!is.na(x))
        }
        else {
            length(x)
        }
    }

    # This does the summary. For each group's data frame, return a vector with
    # N, mean, and sd
    datac <- ddply(data,
                   groupvars,
                   .drop=.drop,
                   .fun = function(xx, col) {
                       c(N = length2(xx[[col]], na.rm=na.rm),
                         mean = mean(xx[[col]], na.rm=na.rm),
                         sd = sd(xx[[col]], na.rm=na.rm))
                   },
                   measurevar)

    # Rename the "mean" column
    datac <- rename(datac, c("mean" = measurevar))

    # Calculate standard error of the mean
    datac$se <- datac$sd / sqrt(datac$N)

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval:
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


## makeFigure()
## Create and return a ggplot object containing multiple growth curve plots
## ARGUMENTS:
##   plot.data:  dataframe of growth curve data
##   plot.err:   boolean value specifying whether to include standard
##               error bars in the plots
##   plateFlag:  boolean value specifying whether plate information
##               is included in the dataframe
##   title:      string to use as the title of the figure
##   color.by:   string specifying which column to color plots by
##   x.lo:       minimum value for x-axis
##   x.hi:       maximum value for x-axis
##   y.lo:       minimum value for y-axis
##   y.hi:       maximum value for y-axis
## RETURN:
##   pl:         ggplot object containing growth curve plots
makeFigure <- function(plot.data, plot.err, plateFlag, title, color.by, x.lo, x.hi, y.lo, y.hi) {
    if (plot.err) {
        grpvrs <- c("time", "well")
        if(plateFlag) {
            grpvrs <- c("mainsource", "compound", grpvrs)
        }
        # Check if a "plate" column is present
        if("plate" %in% colnames(plot.data)) {
            grpvrs <- c(grpvrs, "plate")
        }
        plot.data <- summarySE(plot.data, measurevar="od", groupvars=grpvrs)
    }

    # Create the plot using time in the x axis, optical density in the y axis.
    # Here are several aesthetic changes to the graphs
    # They include creating a line graph, a point graph, facetting based on well,
    # and other aesthetic changes to the graph with colors, labelling, etc.
    # Color is based on specified column
    if (color.by == "sample") {
        pl <- ggplot(plot.data, aes(x=time, y=od, colour=sample))
    } else if (color.by == "replicate") {
        pl <- ggplot(plot.data, aes(x=time, y=od, colour=rep))
    } else {
        pl <- ggplot(plot.data, aes(x=time, y=od))
    }

    # Create facet panels
    pl <- pl + facet_wrap(~well + compound, ncol=12)

    # Plot error bars
    if (plot.err) {
        pl <- pl + geom_ribbon(aes(ymin=od-se, ymax=od+se, linetype=NA),
                               fill="#1F77B4", alpha=0.5) +
            geom_line(size=0.8, alpha=0.5, colour="#1F77B4") +
            geom_point(size=1.5, colour="#1F77B4")
    }
    else {
        pl <- pl + geom_line(size=0.8, alpha=0.5) +
            geom_point(size=1.5, alpha=0.5)
    }

    # Apply theme aesthetics
    pl <- pl + theme(axis.text=element_text(colour="black", size=12),
                     axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
                     axis.title=element_text(face="bold", size=15),
                     panel.grid.major=element_blank(),
                     panel.border=element_rect(colour="black", fill=NA),
                     panel.margin=unit(3, "mm"),
                     strip.text=element_text(face="bold", size=10, vjust=0),
                     strip.background=element_rect(colour="white",
                                                   fill=NA, size=3),
                     legend.key=element_rect(fill=NA),
                     plot.title=element_text(face="bold")
    ) +
        guides(colour=guide_legend(override.aes=list(size=1)))+
        ggtitle(title) + xlab("Time (hr)") + ylab("OD (600nm)") +
        scale_color_tableau(name="")

    # Check for x and y limits
    if (!is.null(x.lo) || !is.null(x.hi)) {
        if (is.null(x.lo)) {
            x.lo <- 0
        }
        if (is.null(x.hi)) {
            x.hi <- max(plot.data$time)
        }
        pl <- pl + scale_x_continuous(limits=c(x.lo, x.hi))
    }
    if (!is.null(y.lo) || !is.null(y.hi)) {
        if (is.null(y.lo)) {
            y.lo <- 0
        }
        if (is.null(y.hi) && plot.err) {
            y.hi <- max(plot.data$od + plot.data$se)
        } else if (is.null(y.hi)) {
            y.hi <- max(plot.data$od)
        }
        pl <- pl + scale_y_continuous(limits=c(y.lo, y.hi))
    }

    return(pl)
}

#################################################################
# ARGUMENT STATEMENTS
#################################################################
x_lo <- NULL
x_hi <- NULL
y_lo <- NULL
y_hi <- NULL
dpi <- 200
width <- 42
height <- 29
plateFlag <- T
plot.sep <- F
colorby <- "replicate"
plot.err <- F
plot.title <- expression(10^5*" — "*10^4)
infile <- "~/Transcend/GoogleDrive/work/collabs/Savannah/LaurenData_20150404/newparser_20160309/rawdata_5_4"
outfile <- "~/Desktop/myplot"

#################################################################
# DATA PROCESSING
#################################################################
# Read in data as a table
# header=T : there is a header line
# sep="\t" : values are tab separated
# check.names=F : header names will be taken as is. There usually is a problem
#                 when numbers are part of the header
data <- read.table(infile, header=T, sep="\t", check.names=F)

# Organize data by well numbers
data$well <- factor(data$well,
                    levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
                             "B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
                             "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
                             "D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
                             "E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
                             "F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
                             "G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
                             "H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))

# Convert long compound names
cmpd.newline <- data.frame(orig=c("L-Glutamic Acid",
                                  "Potassium Sorbate",
                                  "Negative Control",
                                  "Alpha-D-Glucose",
                                  "L-Aspartic Acid",
                                  "D-Glucose-6-Phosphate",
                                  "4 Hydroxy-Phenylacetate",
                                  "D-Aspartic Acid",
                                  "Alpha-D-Lactose",
                                  "L-Cysteic Acid",
                                  "2 Deoxy-D-Ribose",
                                  "L-Phenylalanine",
                                  "D-Glutamic Acid",
                                  "L-Pyro-Glutamic Acid",
                                  "Beta-Phenylethylamine",
                                  "N-Acetyl-D-Glucosamine",
                                  "D-Glucosamine",
                                  "2-Deoxy-D-Ribose",
                                  "L-Djenkolic Acid",
                                  "Acetyl Cysteine",
                                  "1-Butane-Sulfonic Acid",
                                  "Taurocholic Acid",
                                  "Potassium-Tetra-Thionate",
                                  "Magnesium Sulfate",
                                  "Diethyl-Dithiophosphate",
                                  "Sulfanic Acid",
                                  "DL-Alpha-Amino-N-Butyric Acid",
                                  "Sodium Thiosulfate",
                                  "DL-Ethionine",
                                  "N-Acetyl-DL-Methionine",
                                  "Methane Sulfonic Acid",
                                  "Gamma-Amino-N-Butyric Acid",
                                  "N-Acetyl-L-Cysteine",
                                  "Adenosine-5-Monophoshate",
                                  "Sodium Pyrophosphate",
                                  "Sodium Thiophosphate",
                                  "Potassium phosphate",
                                  "DL-Alpha-Glycerophosphate",
                                  "Creatinephosphate",
                                  "Beta-Glycerophosphate"),
                           newl=c("L-Glutamic\nAcid",
                                  "Potassium\nSorbate",
                                  "Negative\nControl",
                                  "Alpha-D-\nGlucose",
                                  "L-Aspartic\nAcid",
                                  "D-Glucose-6-\nPhosphate",
                                  "4 Hydroxy-\nPhenylacetate",
                                  "D-Aspartic\nAcid",
                                  "Alpha-D-\nLactose",
                                  "L-Cysteic\nAcid",
                                  "2 Deoxy-D-\nRibose",
                                  "L-\nPhenylalanine",
                                  "D-Glutamic\nAcid",
                                  "L-Pyro-\nGlutamic Acid",
                                  "Beta-Phenyl\nethylamine",
                                  "N-Acetyl-D-\nGlucosamine",
                                  "D-\nGlucosamine",
                                  "2-Deoxy-\nD-Ribose",
                                  "L-Djenkolic\nAcid",
                                  "Acetyl\nCysteine",
                                  "1-Butane-\nSulfonic Acid",
                                  "Taurocholic\nAcid",
                                  "Potassium-\nTetra-\nThionate",
                                  "Magnesium\nSulfate",
                                  "Diethyl-\nDithiophosphate",
                                  "Sulfanic\nAcid",
                                  "DL-Alpha-\nAmino-N-\nButyric Acid",
                                  "Sodium\nThiosulfate",
                                  "DL-\nEthionine",
                                  "N-Acetyl-\nDL-\nMethionine",
                                  "Methane\nSulfonic\nAcid",
                                  "Gamma-\nAmino-N-\nButyric Acid",
                                  "N-Acetyl-\nL-Cysteine",
                                  "Adenosine-5-\nMonophoshate",
                                  "Sodium\nPyrophosphate",
                                  "Sodium\nThiophosphate",
                                  "Potassium\nPhosphate",
                                  "DL-Alpha-\nGlycerophosphate",
                                  "Creatine\nPhosphate",
                                  "Beta-\nGlycerophosphate"))


for (i in seq(1, nrow(cmpd.newline))) {
    oldc <- as.character(cmpd.newline$orig[i])
    newc <- as.character(cmpd.newline$newl[i])
    levels(data$compound) <- c(levels(data$compound), newc)
    data$compound[data$compound == oldc] <- newc
}
tmp <- droplevels(data$compound)

# Other specific compounds
cmpd.newline <- data.frame(orig=c("Propionic Acid",
                                  "L-glutamic acid",
                                  "Alpha D Lactose",
                                  "L-phenyl-alanine",
                                  "Sodium Succinate",
                                  "D-glutamic acid 0.1%",
                                  "Alpha-D-glucose",
                                  "L-pyro glutamic acid",
                                  "L-aspartic acid, 0.1%",
                                  "Sodium pyruvate",
                                  "Beta phenyl-ethylamine",
                                  "D-glucose 6 PO4",
                                  "L-proline, 0.1%",
                                  "N- acetyl D glucosamine, 0.1%",
                                  "4 hydroxy phenyl acetic acid",
                                  "Alpha-D melebiose",
                                  "L-asparagine, 0.1%",
                                  "D-amino-n-Butyric Acid, 0.1%",
                                  "DL-_-glycero PO4"),
                           newl=c("Propionic\nAcid",
                                  "L-glutatmic\nacid",
                                  "Alpha\nD Lactose",
                                  "L-phenyl-\nalanine",
                                  "Sodium\nSuccinate",
                                  "D-glutamic\nacid 0.1%",
                                  "Alpha-D-\nglucose",
                                  "L-pyro\nglutamic acid",
                                  "L-aspartic\nacid 0.1%",
                                  "Sodium\npyruvate",
                                  "Beta phenyl\nethylamine",
                                  "D-glucose-\n6-PO4",
                                  "L-proline\n0.1%",
                                  "N-acetyl D-\nglucosamine\n0.1%",
                                  "4\nhydroxy phenyl\nacetic acid",
                                  "Alpha-D-\nmelebiose",
                                  "L-asparagine\n0.1%",
                                  "D-amino-n-\nButyric Acid\n0.1%",
                                  "DL-_-\nglycero PO4"))

for (i in seq(1, nrow(cmpd.newline))) {
    oldc <- as.character(cmpd.newline$orig[i])
    newc <- as.character(cmpd.newline$newl[i])
    levels(data$compound) <- c(levels(data$compound), newc)
    data$compound[data$compound == oldc] <- newc
}
tmp <- droplevels(data$compound)

# Change the time value from character string to numeric values
# This causes a problem when trying to plot
data$time <- as.numeric(as.character(data$time))
# Force replicate value to be a factor value
if("rep" %in% colnames(data)) {
    data$rep <- as.factor(data$rep)
}

# Create figure
# plot.sep is true when creating a plot per sample
if (plot.sep) {
    dataNames <- unique(data$sample)
    for (s in dataNames) {
        plot.data <- data[grep(s, data$sample, fixed=T),]
        if (plot.title == "") {
            title <- s
        } else {
            title <- paste(s, " - ", plot.title, sep="")
        }
        pl <- makeFigure(plot.data, plot.err, plateFlag, title, colorby,
                         x_lo, x_hi, y_lo, y_hi)
        ggsave(paste(outfile, "_", s, ".png", sep=""),
               plot=pl,
               width=width,
               height=height,
               units="cm",
               dpi=dpi)
    }
} else {
    pl <- makeFigure(data, plot.err, plateFlag, plot.title, colorby,
                     x_lo, x_hi, y_lo, y_hi)
    ggsave(paste(outfile, ".png", sep=""),
           plot=pl,
           width=width,
           height=height,
           units="cm",
           dpi=dpi)
}
