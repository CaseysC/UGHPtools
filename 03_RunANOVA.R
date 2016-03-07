#03_RunANOVA.R
#ANOVA script for data analysis
#Nicole E Soltis
#03/07/2016
#-------------------------------------------------
#load your data
rm(list=ls())
setwd("~/PATH/TO/DIRECTORY/")
getwd()
#read in data file
MyDat4 <- read.csv("NEWDATAFRAME.csv")
#---------------------------------------------------------
#now we get to actually do an ANOVA!

#PARAMETRIC TESTS
#two-way ANOVA
MY.ANOVA <- anova(lm(traf~INDVAR1*INDVAR2))
summary(MY.ANOVA)
MY.ANOVA 
interaction.plot(INDVAR1,INDVAR2,traf)

#another way to two-way ANOVA
MYMOD.aov <- aov(traf~INDVAR1*INDVAR2)
plot(MYMOD.aov)
summary(MYMOD.aov)

#one more assumption to check: are your residuals normally distributed?
MYResid <-lm(traf~INDVAR1*INDVAR2)
#run shapiro-wilk goodness of fit test on the residuals
shapiro.test(residuals(MYResid))

#Post-hoc tests: which LEVELS under your independent variables differ?
TukeyHSD(MYMOD.aov)
interaction.plot(INDVAR1,INDVAR2,traf)

#----------------------------------------------------------------
#nonparametric alternatives to ANOVA
#Kruskal-Wallis: nonparametric one-way ANOVA
kruskal.test(DEPVAR~INDVAR)

#nonparametric alternative to two-way ANOVA
#equivalent to sign test if 2 columns
friedman.test(DEPVAR~INDVAR1|INDVAR2,data=MyDat4)