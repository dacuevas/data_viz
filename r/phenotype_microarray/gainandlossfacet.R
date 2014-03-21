library("ggplot2")
library("reshape2")
library("plyr")

# Read in data
setwd("~/Dropbox/Work/vdm_project/20140124_filtered/")
data <- read.table("curveinfo_filtered.txt", sep="\t", header=T, check.names=F)

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

aa <- subset(data, substrate == "D-Serine")
aa <- droplevels(aa)
ggplot(aa, aes(x=clone, y=harmonic_mean)) + geom_point() + #scale_y_continuous(limits=c(0, 1)) +
	theme(panel.background=element_blank(), axis.text.x=element_text(angle=90, colour="black"), 
				panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
	geom_hline(aes(yintercept=0.5)) + geom_hline(aes(yintercept=(mean(aa$harmonic_mean))), colour="grey", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(aa$harmonic_mean) + 2*sd(aa$harmonic_mean))), colour="green", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(aa$harmonic_mean) - 2*sd(aa$harmonic_mean))), colour="green", linetype="dashed") +
    geom_hline(aes(yintercept=(mean(aa$harmonic_mean) + 3*sd(aa$harmonic_mean))), colour="red", linetype="dashed") +
    geom_hline(aes(yintercept=(mean(aa$harmonic_mean) - 3*sd(aa$harmonic_mean))), colour="red", linetype="dashed") +
    ggtitle("D-Serine")

dglu <- subset(data, growthcondition == "D-Galactose")
dglu <- droplevels(dglu)
ggplot(dglu, aes(x=sample, y=growthlevel, group=growthcondition)) + geom_point() + geom_line(alpha=0) + scale_y_continuous(limits=c(0.2, 1.2)) +
	geom_ribbon(colour="Blue",alpha=0.1,linetype="dashed",aes(ymin=max(c((mean(dglu$growthlevel) - 2*sd(dglu$growthlevel)),0)), ymax=(mean(dglu$growthlevel) + 2*sd(dglu$growthlevel)))) +
	theme(panel.background=element_blank(), axis.text.x=element_text(angle=90, colour="black", size=8),
				axis.text.y=element_text(colour="black", size=8), axis.title=element_text(face="bold", size=12),
				panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
	geom_hline(aes(yintercept=0.4),size=1) + geom_hline(aes(yintercept=(mean(dglu$growthlevel))), colour="Blue", linetype="dashed") +
	ggtitle("D-Galactose")
	#geom_ribbon(alpha=0.3,aes(ymin=mean(dglu$growthlevel) - 2*sd(dglu$growthlevel), ymax=mean(dglu$growthlevel) + 2*sd(dglu$growthlevel)))
	#geom_hline(aes(yintercept=(mean(dglu$harmonic_mean) + 2*sd(dglu$harmonic_mean))), colour="green", linetype="dashed") +
	#geom_hline(aes(yintercept=(mean(dglu$harmonic_mean) - 2*sd(dglu$harmonic_mean))), colour="red", linetype="dashed")

xylitol <- subset(data, substrate == "Xylitol")
xylitol <- droplevels(xylitol)
ggplot(xylitol, aes(x=clone, y=harmonic_mean)) + geom_point() + scale_y_continuous(limits=c(0.2, 0.6)) +
	theme(panel.background=element_blank(), axis.text.x=element_text(angle=90, colour="black"), 
				panel.grid.minor=element_blank(), panel.grid.major=element_blank()) +
	geom_hline(aes(yintercept=0.5)) + geom_hline(aes(yintercept=(mean(xylitol$harmonic_mean))), colour="grey", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(xylitol$harmonic_mean) + 2*sd(xylitol$harmonic_mean))), colour="green", linetype="dashed") +
	geom_hline(aes(yintercept=(mean(xylitol$harmonic_mean) - 2*sd(xylitol$harmonic_mean))), colour="red", linetype="dashed")
