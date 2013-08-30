library("ggplot2")
library("reshape2")

setwd("~/Jason/stress_test/0819_ph_stress/results/newresults/")
data <- read.table("allcurves.txt", header=T, sep="\t", check.names=F)
data <- data[, c(1,2,8:ncol(data))]

meltData <- melt(data, id.vars=c("clone", "well"), variable.name="time", value.name="optical_density")
meltData$well <- factor(meltData$well, levels=c("A1", "A2", "A3", "A4", "A5", "A6", "A7", "A8", "A9", "A10", "A11", "A12",
																								"B1", "B2", "B3", "B4", "B5", "B6", "B7", "B8", "B9", "B10", "B11", "B12",
																								"C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12",
																								"D1", "D2", "D3", "D4", "D5", "D6", "D7", "D8", "D9", "D10", "D11", "D12",
																								"E1", "E2", "E3", "E4", "E5", "E6", "E7", "E8", "E9", "E10", "E11", "E12",
																								"F1", "F2", "F3", "F4", "F5", "F6", "F7", "F8", "F9", "F10", "F11", "F12",
																								"G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", "G11", "G12",
																								"H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10", "H11", "H12"))

meltData$time <- as.numeric(as.character(meltData$time))
names(meltData)[names(meltData) == "clone"] <- c("Clone")


# breaks = c()
# idx <- 0
# for(i in seq(0, 1900, 30)) {
#   if (idx %% 7 != 0) {
#     label <- ""
#   }
#   else {
#     label <- as.character(i)
#   }
#   breaks <- append(breaks, label)exit
#   idx <- idx + 1
# }

pl <- ggplot(meltData, aes(x=time, y=optical_density, colour=Clone, group=Clone))
pl + geom_line() + facet_wrap(~well, ncol=12) + coord_trans(y="log2") +
	theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5, colour="black"), panel.background=element_blank(),
				axis.text.y=element_text(colour="black"), panel.grid.minor=element_blank(), 
				panel.grid.major=element_blank(), axis.title.x=element_text(face="bold"),
				panel.border=element_rect(colour="gray90", fill=NA),
				axis.title.y=element_text(face="bold", angle=0), legend.key=element_rect(fill="white"),
				plot.title=element_text(face="bold")) + 
	ggtitle("Median Curves") + xlab("Time (hours)") + ylab("OD600nm") + scale_x_continuous(breaks=c(2.0, 10.0, 20.0, 30.0)) +
	scale_colour_manual(values=c("#0072B2", "#D55E00", "limegreen", "black"))

 