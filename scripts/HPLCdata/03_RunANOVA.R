#03_RunANOVA.R
#ANOVA script for data analysis
#Nicole E Soltis
#03/07/2016
#-------------------------------------------------
#load your data
rm(list=ls())
setwd("~/PATH/TO/DIRECTORY/")
#setwd("C:/Users/nesoltis/Documents/Projects/UGHPtools/data")
getwd()
#read in data file
aovDat <- read.csv("UGHP_data.csv")
#---------------------------------------------------------
#now we get to actually do an ANOVA!
names(aovDat)
#PARAMETRIC TESTS

#only test "good" genos?
aovDat2 <- aovDat[ which(aovDat$Note=='good'), ]
#two-way ANOVA
MY.ANOVA <- anova(lm(Total_Area.cm.~Geno_Cross*Date, data=aovDat2))
summary(MY.ANOVA)
MY.ANOVA 
attach(aovDat2)
interaction.plot(Date, Geno_Cross, Total_Area.cm.)

MY.ANOVA.2 <- anova(lm(Total_Area.cm.~Geno_Cross*Date + Tray, data=aovDat2))

library(lme4)
MY.mixmod <- lmer(Total_Area.cm.~Geno_Cross*Date + (1|Tray), data=aovDat2)
anova(MY.mixmod)

#another way to two-way ANOVA
MYMOD.aov <- aov(DV~INDVAR1*INDVAR2)
plot(MYMOD.aov)
summary(MYMOD.aov)

#one more assumption to check: are your residuals normally distributed?
MYResid <-lm(traf~INDVAR1*INDVAR2)
#run shapiro-wilk goodness of fit test on the residuals
shapiro.test(residuals(MYResid))

#Post-hoc tests: which LEVELS under your independent variables differ?
TukeyHSD(MYMOD.aov)
interaction.plot(INDVAR1,INDVAR2,traf)