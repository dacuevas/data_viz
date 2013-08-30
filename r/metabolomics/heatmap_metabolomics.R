library("ggplot2")
library("reshape2")
library("ggdendro")
library("grid")

# Read in z-scores
setwd("~/Savannah/")
data <- read.table("zscore_dendoheat_clones.txt", sep="\t", header=T, check.names=F)

newdata <- data
rownames(newdata) <- newdata$clone
newdata <- subset(newdata, select=c(-clone))
x <- as.matrix(scale(newdata))
dd.col <- as.dendrogram(hclust(dist(x)))
col.ord <- order.dendrogram(dd.col)

dd.row <- as.dendrogram(hclust(dist(t(x))))
row.ord <- order.dendrogram(dd.row)

xx <- scale(newdata)[col.ord, row.ord]
xx_names <- attr(xx, "dimnames")
df <- as.data.frame(xx)
colnames(df) <- xx_names[[2]]
df$clone <- xx_names[[1]]
df$clone <- with(df, factor(clone, levels=clone, ordered=TRUE))

mdf <- melt(df, id.vars="clone", variable.name="metabolite", value.name="zscore")

ddata_x <- dendro_data(dd.row)
ddata_y <- dendro_data(dd.col)


# Data to use for line graph placed above heatmap
metOrder <- levels((label(ddata_x))$label)
medianData <- read.table("~/Projects/modeling/metabolomics_20130723/medians_quoted.txt", sep="\t", header=T, check.names=F)
medianData <- subset(medianData, select=c("metabolite", "median"))
medianData$metabolite <- factor(medianData$metabolite, levels=metOrder)


### Set up a blank theme
theme_none <- theme(
	panel.grid.major = element_blank(),
	panel.grid.minor = element_blank(),
	panel.background = element_blank(),
	axis.title.x = element_text(colour=NA),
	axis.title.y = element_blank(),
	axis.text.x = element_blank(),
	axis.text.y = element_blank(),
	axis.line = element_blank()
)

### Create plot components ###
# Heatmap
p1 <- ggplot(mdf, aes(x=metabolite, y=clone)) +
	geom_tile(aes(fill=zscore)) + scale_fill_gradientn(colours=c("royalblue4","white", "darkred")) +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, size=8, colour="Black"),
				axis.text.y=element_text(vjust=0.5, size=8, colour="Black"))

# Dendrogram 1
p2 <- ggplot(segment(ddata_x)) +
	geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
	theme_none + theme(axis.title.x=element_blank())

# Dendrogram 2
p3 <- ggplot(segment(ddata_y)) +
	geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) +
	coord_flip() + theme_none

grid.newpage()
print(p1, vp=viewport(0.8, 0.8, x=0.4, y=0.4))
print(p2, vp=viewport(0.52, 0.2, x=0.45, y=0.9))
print(p3, vp=viewport(0.2, 0.8, x=0.9, y=0.4))


# Line graph to place above heatmap
p4 <- ggplot(medianData, aes(x=metabolite, y=median, group=1))
p4 + geom_line(colour="royalblue4") + coord_trans(y="log10") + scale_x_discrete(breaks=NULL) +
	theme(axis.text.x=element_text(angle=90, colour="Black", family="Arial", size=12, hjust=1, vjust=0.5),
				axis.text.y=element_text(colour="Black", family="Arial", size=12), strip.text=element_text(size=14),
				panel.background=element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_line(linetype="dashed", colour="black", size=0.1),
				panel.border=element_rect(linetype="solid", colour="black", fill=NA)) +
	ggtitle("") + xlab("") + ylab("")
