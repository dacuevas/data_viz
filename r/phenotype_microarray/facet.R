library("ggplot2")
library("reshape2")

# Read in data
setwd("~/Dropbox/Work/vdm_project/20131105_oldHM/")
data95 <- read.table("function_distro.txt", sep="\t", header=T, check.names=F)

# Reorder data
substrateTab95EG <- table(subset(data95, category == "Expected Growth", select = substrate))
substrateTab95NG <- table(subset(data95, category == "No Growth", select = substrate))
data95$substrate <- factor(data95$substrate, levels = names(substrateTab95EG[order(-substrateTab95EG,substrateTab95NG)]))

# Plot
ggplot(data95,aes(substrate)) + geom_bar() + facet_wrap(~category,ncol=1) + 
	theme(axis.text.x=element_text(angle=90, colour="Black", family="Arial", size=14, hjust=1, vjust=0.5), 
				axis.text.y=element_text(colour="Black", family="Arial", size=12), axis.title.y=element_text(face="bold", size=14), strip.text=element_text(size=14), 
				panel.background=element_blank(), panel.grid.minor=element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=16, family="Arial")) + 
	ggtitle("Phenotypic Distribution By Substrate\n") + xlab("") + ylab("# of clones\n")

substrateTab95 <- table(subset(data95, category == "Gain of Function", select = substrate))
data95$substrate <- factor(data95$substrate, levels = names(substrateTab95[order(substrateTab95, decreasing=T)]))
cloneTab95 <- table(subset(data95, category == "Gain of Function", select = clone))
data95$clone <- factor(data95$clone, levels = names(cloneTab95[order(cloneTab95, decreasing=T)]))

# Plot
ggplot(data95[data95$category=="Gain of Function",],aes(x=substrate,y=clone)) + geom_point(size=5) + 
    theme(axis.text.x=element_text(angle=90, colour="Black", hjust=1, vjust=0.5), 
          axis.text.y=element_text(colour="Black", family="Arial", size=12), axis.title.y=element_text(face="bold", size=14), 
          axis.text.x=element_text(colour="Black", family="Arial", size=12), plot.title = element_text(lineheight=.8, face="bold", size=16)) + 
    ggtitle("Gain of Function\n") + xlab("") + ylab("# of clones\n")

substrateTab95 <- table(subset(data95, category == "Loss of Function", select = substrate))
data95$substrate <- factor(data95$substrate, levels = names(substrateTab95[order(substrateTab95, decreasing=T)]))
cloneTab95 <- table(subset(data95, category == "Loss of Function", select = clone))
data95$clone <- factor(data95$clone, levels = names(cloneTab95[order(cloneTab95, decreasing=T)]))

ggplot(data95[data95$category=="Loss of Function",],aes(x=substrate,y=clone)) + geom_point(size=5) + 
    theme(axis.text.x=element_text(angle=90, colour="Black", hjust=1, vjust=0.5), 
          axis.text.y=element_text(colour="Black", family="Arial", size=12), axis.title.y=element_text(face="bold", size=14), 
          axis.text.x=element_text(colour="Black", family="Arial", size=12), plot.title = element_text(lineheight=.8, face="bold", size=16)) + 
    ggtitle("Loss of Function\n") + xlab("") + ylab("# of clones\n")


###
# 99% CI (3 * std deviations)
###
data99 <- read.table("function_distribution_99.txt", sep="\t", header=T, check.names=F)

# Reorder data
substrateTab99 <- table(subset(data99, category == "Expected Growth", select = substrate))
data99$substrate <- factor(data99$substrate, levels = names(substrateTab99[order(substrateTab99, decreasing=T)]))

# Plot
ggplot(data99,aes(substrate)) + geom_bar() + facet_wrap(~category,ncol=1) + 
    theme(axis.text.x=element_text(angle=90, colour="Black", family="Arial", size=14, hjust=1, vjust=0.5), 
          axis.text.y=element_text(colour="Black", family="Arial", size=12), axis.title.y=element_text(face="bold", size=14), strip.text=element_text(size=14), 
          panel.background=element_blank(), panel.grid.minor=element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=16, family="Arial")) + 
    ggtitle("Phenotypic Distribution By Substrate\n") + xlab("") + ylab("# of clones\n")

substrateTab99 <- table(subset(data99, category == "Gain of Function", select = substrate))
data99$substrate <- factor(data99$substrate, levels = names(substrateTab99[order(substrateTab99, decreasing=T)]))
cloneTab99 <- table(subset(data99, category == "Gain of Function", select = clone))
data99$clone <- factor(data99$clone, levels = names(cloneTab99[order(cloneTab99, decreasing=T)]))

# Plot
ggplot(data99[data99$category=="Gain of Function",],aes(x=substrate,y=clone)) + geom_point(size=5) + facet_wrap(~category,ncol=1) + 
    theme(axis.text.x=element_text(angle=90, colour="Black", hjust=1, vjust=0.5), 
          axis.text.y=element_text(colour="Black", family="Arial", size=12), axis.title.y=element_text(face="bold", size=14), strip.text=element_text(size=14), 
          plot.title = element_text(lineheight=.8, face="bold", size=16)) + 
    ggtitle("Phenotypic Distribution By Substrate\n") + xlab("") + ylab("# of clones\n")



