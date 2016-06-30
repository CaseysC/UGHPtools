#04_Plots.R
#script for basic R plots
#Nicole E Soltis
#05/23/2016
#-------------------------------------------------
rm(list=ls())
plotData <- read.csv("UGHP_data.csv")

#bean plot by tray
names(plotData)
library(ggplot2)
p <- ggplot(plotData, aes(factor(AgFlat), Scale.LS))
p + geom_violin()
library("beanplot")
beanplot(Scale.LS ~ AgFlat, data=ModDat, las=3)
text(srt=45)

#barplot with average lesion size by genotype
#and SE bars
FigDat <- ModDat
names(FigDat)
FigDat2 <- ddply(FigDat, c("PlGenoNm", "Species"), summarise,
                 N    = length(Scale.LS),
                 mean = mean(Scale.LS),
                 sd   = sd(Scale.LS),
                 se   = sd / sqrt(N))
FigDat2$SpLabs <- factor(FigDat2$Species, labels = c("Domesticated", "Wild"))
limits <- aes(ymax = mean + se, ymin=mean - se)
ggplot(FigDat2, aes(x = factor(PlGenoNm), y = mean))+
  geom_bar(stat="identity", fill="dodgerblue3")+
  theme_bw()+
  theme(text = element_text(size=24), axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y=expression(Mean ~ Lesion ~ Area ~ (cm^{2})), x=element_blank())+
  geom_errorbar(limits, width=0.25)+
  facet_grid(.~SpLabs, scales="free")