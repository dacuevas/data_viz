library("ggplot2")
library("reshape2")

setwd("~/Projects/modeling/metabolomics_20130723/")
data <- read.table("medians_quoted.txt", sep="\t", header=T, check.names=F)

mData <- melt(data, id.vars="metabolite", variable.name="clone", value.name="abundance")

for( i in seq(1,nrow(data) )) {
	m <- data[i,1]
	pl <- ggplot(subset(mData, metabolite == m), aes(x=abundance))
	pl <- pl + geom_density() +
		theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"), panel.background=element_blank(),
					axis.text.y=element_text(colour="black")) + ggtitle(m) +
		xlab("abundance") + ylab("Count") +
		geom_vline(aes(xintercept=mean(abundance)),
							 color="red", linetype="dashed", size=1) +
		geom_vline(aes(xintercept=mean(abundance)+sd(abundance)),
							 color="grey", linetype="dashed", size=1) +
		geom_vline(aes(xintercept=mean(abundance)-sd(abundance)),
							 color="grey", linetype="dashed", size=1)
	print(pl)
}


pl <- ggplot(mData, aes(x=abundance))
pl + geom_density() + facet_wrap(~metabolite, ncol=17, scales="free") +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black", size=12), panel.background=element_blank(),
				axis.text.y=element_blank(), strip.text=element_text(colour="black", size=15)) + ggtitle("") +
	xlab("Abundance") + ylab("")