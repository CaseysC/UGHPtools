#01_DataEntry.R
#ANOVA script for data analysis
#Nicole E Soltis
#03/07/2016
#-------------------------------------------------
#load your data
rm(list=ls())
#setwd("~/PATH/TO/DIRECTORY/")
setwd("~/Projects/UGHPtools/data")
getwd()
#read in data file
MyDat <- read.csv("MYFILE.csv")
names(MyDat)
View(MyDat)

#check structure of new object
str(MyDat)
#is it a data.frame?
class(MyDat)
#list column names
attach(MyDat)
names(MyDat)

#--------------------------------------------
#basic dataframe manipulation
#optional: subset columns
MyDat2 <- MyDat[,c(1:3)]

#optional: stick 2 data frames together by column
MyDat3 <- rbind(MyDat2, OTHERFILE)

#optional: remove any duplicate rows
MyDat2 <- unique(MyDat2)

#optional: remove rows depending on the value in one column
unique(MyDat3$VARIABLE)
MyDat3 <- MyDat3[MyDat3$VARIABLE!="VALUE",]

#optional: add a column that combines 2 other columns
MyDat3$NEWVAR <- paste(MyDat3$VAR1, MyDat3$VAR2, sep='') 

#optional: add a column by performing an equation on 2 existing columns
MyDat2 <- transform(MyDat2, NEWVAR=(OLDVAR1/OLDVAR2))

#optional: rename columns
names(MyDat3)
MyDat3 <- dplyr::select(MyDat, NEWNAME = OLDNAME, NEWNAME2 = OLDNAME2, matches("."))

#save your edited data frame  
write.csv(MyDat3, "NEWDATAFRAME.csv")

#and you can read it back in here
MyDat4 <- read.csv("NEWDATAFRAME.csv")