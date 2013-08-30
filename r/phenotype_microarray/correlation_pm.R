library("ggplot2")
library("reshape2")
library("ggdendro")
library("grid")

library("corrgram")

# Read in z-scores
setwd("~/Projects/modeling/manuscript_07-23/")
data <- read.table("dulcitol_transposed.txt", sep="\t", header=T, check.names=F)
data <- data[, c(3:ncol(data)-1)]
fundata <- read.table("gainoffunction.txt", sep="\t", header=T, check.names=F)
fundata <- subset(fundata, substrate == "Dulcitol")

corrgram(data, order=TRUE, lower.panel=panel.ellipse,
				 upper.panel=panel.pts, text.panel=panel.txt,
				 main="Correlation")

fundata2 <- subset(data, select=levels(droplevels(fundata$clone)))
corrgram(fundata2, order=TRUE, lower.panel=panel.ellipse,
				 upper.panel=panel.pts, text.panel=panel.txt,
				 main="Correlation")

res <- rcorr(as.matrix(data), type="pearson")
write.table(res$r, sep="\t", file="test.data.txt")