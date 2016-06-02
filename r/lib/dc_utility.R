# dc_utility.R
# A compilation of utility functions, themes, etc.
# Author: Daniel A. Cuevas
# Last updated: 02 June 2016

facets.theme <- theme(axis.text=element_text(colour="black", size=12),
                      axis.text.x=element_text(angle=90, hjust=1, vjust=0.5),
                      axis.title=element_text(face="bold", size=15),
                      panel.grid.major=element_blank(),
                      panel.border=element_rect(colour="black", fill=NA),
                      panel.margin=unit(3, "mm"),
                      strip.text=element_text(face="bold", size=10, vjust=0),
                      strip.background=element_rect(colour="white", fill=NA, size=3),
                      legend.key=element_rect(fill=NA),
                      plot.title=element_text(face="bold"))


# summarySE
# Gives count, mean, standard deviation, standard error of the mean,
# and confidence interval (default 95%).
#   data:          a data frame.
#   measurevar:    the name of a column that contains the
#                  variable to be summariezed
#   groupvars:     a vector containing names of columns
#                  that contain grouping variables
#   na.rm:         a boolean that indicates whether to ignore NA's
#   conf.interval: the percent range of the
#                  confidence interval (default is 95%)
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


