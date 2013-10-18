library("ggplot2")
library("reshape2")

# Read in data
setwd("~/Dropbox/Work/vdm_project/20131017/")
data <- read.table("function_distribution_out.txt", sep="\t", header=T, check.names=F)

# Reorder data
substrateTab <- table(subset(data, category == "Expected Growth", select = substrate))
data$substrate <- factor(data$substrate, levels = names(substrateTab[order(substrateTab, decreasing=T)]))

# Plot
ggplot(data,aes(substrate)) + geom_bar() + facet_wrap(~category,ncol=1) + 
	theme(axis.text.x=element_text(angle=90, colour="Black", family="Arial", size=14, hjust=1, vjust=0.5), 
				axis.text.y=element_text(colour="Black", family="Arial", size=12), axis.title.y=element_text(face="bold", size=14), strip.text=element_text(size=14), 
				panel.background=element_blank(), panel.grid.minor=element_blank(), plot.title = element_text(lineheight=.8, face="bold", size=16, family="Arial")) + 
	ggtitle("Phenotypic Distribution By Substrate\n") + xlab("") + ylab("# of clones\n")