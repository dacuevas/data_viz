library("ggplot2")
library("reshape2")

# Read in data
setwd("~/Projects/modeling/manuscript_07-23/")
data <- read.table("function_distribution_0703.txt", sep="\t", header=T, check.names=F)

gofdata <- subset(data, category == "Gain of Function")
lofdata <- subset(data, category == "Loss of Function")
gofdata <- droplevels(gofdata)
lofdata <- droplevels(lofdata)

# Reorder data
gofdata$clone <- reorder(gofdata$clone, gofdata$clone, length)
lofdata$clone <- reorder(lofdata$clone, lofdata$clone, length)
gofdata$clone <- factor(gofdata$clone, levels=rev(levels(gofdata$clone)))
lofdata$clone <- factor(lofdata$clone, levels=rev(levels(lofdata$clone)))

gofdata$substrate <- reorder(gofdata$substrate, gofdata$substrate, length)
lofdata$substrate <- reorder(lofdata$substrate, lofdata$substrate, length)
gofdata$substrate <- factor(gofdata$substrate, levels=rev(levels(gofdata$substrate)))
lofdata$substrate <- factor(lofdata$substrate, levels=rev(levels(lofdata$substrate)))


# Plot
p1 <- ggplot(gofdata, aes(factor(clone)))
p1 + geom_bar() +
	theme(panel.background=element_blank(), panel.grid.minor=element_blank(),
				axis.text.x=element_text(angle=45, colour="black", family="Arial", hjust=1, vjust=1),
				axis.text.y=element_text(colour="black", family="Arial")) +
	ggtitle("Gain of Function") + xlab("") + ylab("")

p2 <- ggplot(lofdata, aes(factor(clone)))
p2 + geom_bar() + scale_y_discrete(breaks=c(3, 6, 9)) +
	theme(panel.background=element_blank(), panel.grid.minor=element_blank(),
				axis.text.x=element_text(angle=45, colour="black", family="Arial", hjust=1, vjust=1),
				axis.text.y=element_text(colour="black", family="Arial")) +
	ggtitle("Loss of Function") + xlab("") + ylab("")

p1 <- ggplot(gofdata, aes(factor(substrate)))
p1 + geom_bar() +
	theme(panel.background=element_blank(), panel.grid.minor=element_blank(),
				axis.text.x=element_text(angle=45, colour="black", family="Arial", hjust=1, vjust=1, size=5),
				axis.text.y=element_text(colour="black", family="Arial")) +
	ggtitle("Gain of Function") + xlab("") + ylab("")

p2 <- ggplot(lofdata, aes(factor(substrate)))
p2 + geom_bar() +
	theme(panel.background=element_blank(), panel.grid.minor=element_blank(),
				axis.text.x=element_text(angle=45, colour="black", family="Arial", hjust=1, vjust=1),
				axis.text.y=element_text(colour="black", family="Arial")) +
	ggtitle("Loss of Function") + xlab("") + ylab("")

aa <- subset(data, substrate == "Acetic Acid")
aa <- droplevels(aa)
ggplot(aa, aes(x=clone, y=harmonic_mean)) + geom_point() + scale_y_continuous(limits=c(0, 1)) +
	theme(panel.background=element_blank(), axis.text.x=element_text(angle=90, colour="black"), 
				panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
	geom_hline(aes(yintercept=0.5)) + geom_hline(aes(yintercept=(mean(aa$harmonic_mean))), colour="grey", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(aa$harmonic_mean) + 2*sd(aa$harmonic_mean))), colour="green", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(aa$harmonic_mean) - 2*sd(aa$harmonic_mean))), colour="red", linetype="dashed")

dglu <- subset(data, substrate == "D-Glucose")
dglu <- droplevels(dglu)
ggplot(dglu, aes(x=clone, y=harmonic_mean)) + geom_point() + scale_y_continuous(limits=c(0, 2)) +
	theme(panel.background=element_blank(), axis.text.x=element_text(angle=90, colour="black"), 
				panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
	geom_hline(aes(yintercept=0.5)) + geom_hline(aes(yintercept=(mean(dglu$harmonic_mean))), colour="grey", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(dglu$harmonic_mean) + 2*sd(dglu$harmonic_mean))), colour="green", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(dglu$harmonic_mean) - 2*sd(dglu$harmonic_mean))), colour="red", linetype="dashed")

xylitol <- subset(data, substrate == "Xylitol")
xylitol <- droplevels(xylitol)
ggplot(xylitol, aes(x=clone, y=harmonic_mean)) + geom_point() + scale_y_continuous(limits=c(0.2, 0.6)) +
	theme(panel.background=element_blank(), axis.text.x=element_text(angle=90, colour="black"), 
				panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
	geom_hline(aes(yintercept=0.5)) + geom_hline(aes(yintercept=(mean(xylitol$harmonic_mean))), colour="grey", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(xylitol$harmonic_mean) + 2*sd(xylitol$harmonic_mean))), colour="green", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(xylitol$harmonic_mean) - 2*sd(xylitol$harmonic_mean))), colour="red", linetype="dashed")
