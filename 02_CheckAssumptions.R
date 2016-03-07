#02_CheckAssumptions.R
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
#--------------------------------------------------------
#CHECK ASSUMPTIONS BEFORE STATISTICS!
#check data structure
MyDat4 <- MyDat
names(MyDat4)
xtabs(~ INDVAR1 + INDVAR2, MyDat4)

#check assumption of normality of your dependent variable
attach(MyDat4)
#graphically...
hist(DEPVAR)
#more graphs
require(car); require(MASS)
MyDat4$DEPVAR.t <- MyDat4$DEPVAR + 1
#is it more normal or log-normal?
qqp(MyDat4$DEPVAR.t, "norm")
qqp(MyDat4$DEPVAR.t, "lnorm")

#statistically...
shapiro.test(DEPVAR)

#try transformations
transf <- (log((DEPVAR)))
hist(transf)
shapiro.test(transf)

#negative skew: small to large
transf <- (DEPVAR)^2
transf <- (DEPVAR)^3
#positive skew: small to large
transf <- (DEPVAR)^.5
transf <- log(DEPVAR)
transf <- log10(DEPVAR)
transf <- (-1/((DEPVAR)^.5))

#other options for normalization
transf <- MyDat4$DEPVAR / sum(MyDat4$DEPVAR)
transf <- MyDat4$DEPVAR / sqrt(sum(MyDat4$DEPVAR * MyDat4$DEPVAR))

#transform your dependent variable if necessary
MyDat4 <-transform(MyDat4, TRANSFORMED = (DEPVAR^2))
attach(MyDat4)

#next check assumption of homoscedasticity
#graphically...
boxplot(traf~INDVAR1*INDVAR2,
        ylab="YTITLE", main="PLOTTITLE", las=3)
#statistically...
bartlett.test(traf~INDVAR1*INDVAR2) 
leveneTest(traf~INDVAR1)
var.test(traf~INDVAR1)
#---------------------------------------------------------
#Power analysis: have you collected enough data?
library(pwr)
#one-way anova
pwr.anova.test(k=5,f=0.25,sig.level=0.05,power=0.8)
#two-way anova
#cohen's f2, related to R-squared (model fit)
#u is df of numerator
#= ((levels of factor 1)-1) * ((levels of factor 2)-1)
#v is total number of subjects across all categories
#MINUS (levels of factor 1)*(levels of factor 2)
#these values are given in anova table
pwr.f2.test(u=2, v=294, sig.level=0.05, power=0.8)
#f2 is Cohen's f-squared value, ~R-squared, so effect size low.
#by convention, f2 0.02 is small, 0.15 is medium, 0.35 is large
