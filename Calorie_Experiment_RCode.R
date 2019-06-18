library(dplyr) # data munging
library(haven) # to read STATA dta files
library(mice) # missing data operations
library(codebook) # to look at codebooks as you would do in R
library(fastDummies) # to easily create dummy variables
library(sjlabelled) # for easily adding labels to categorical variables
library(ggplot2) # plots and graphics
library(robustbase) #for running robust SE regression in R
library(gmodels)
library(effsize)
library(fastDummies)
library(QuantPsyc)
library(lfactors)

options(digits=2)

#import data
rm(list = ls())
daylvl <- read.table("/Users/lingdai/Desktop/cafe-date-level.csv",sep = ",",header=1)
daylvl_old <- read.table("/Users/lingdai/Desktop/day-level.csv",sep = ",",header=1)

daylvl$Day_of_Week <- as.factor(daylvl$Day_of_Week)
daylvl$BL <- ifelse(daylvl$Treatment=='BL', 1, 0)
daylvl$BL1 <- ifelse(daylvl$Treatment=='BL1', 1, 0)
daylvl$BL2 <- ifelse(daylvl$Treatment=='BL2', 1, 0)
daylvl$S1 <- ifelse(daylvl$Treatment=='S1', 1, 0)
daylvl$S2 <- ifelse(daylvl$Treatment=='S2', 1, 0)
daylvl$S3 <- ifelse(daylvl$Treatment=='S3', 1, 0)
daylvl$S4 <- ifelse(daylvl$Treatment=='S4', 1, 0)
daylvl$Signage <- ifelse(daylvl$S1+daylvl$S2+daylvl$S3+daylvl$S4==1,1,0)

daylvl$Qtr <- ifelse(daylvl$Treatment=='BL1','2014Q1',ifelse(daylvl$Treatment=='BL2','2014Q3','2015Q1'))
daylvl$Week_of_Qtr <- relevel(as.factor(daylvl$Week_of_Qtr), ref='1')
daylvl$Treatment <- relevel(daylvl$Treatment, ref = "BL")
daylvl$Y2015 <- ifelse(daylvl$Treatment!='BL1'&daylvl$Treatment!='BL2',1,0)


#MA Thesis Table 1 Model 1
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 1 Model 2
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 1 Model 3
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 1 Model 4
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe +cafe:Qtr + Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 1 Model 5
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe:Qtr + Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 2 Model 1
lm <- lm(Proj_Calories ~ SubTotal + BL1 + BL2 + Signage + Coupon:cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 2 Model 2
lm <- lm(Proj_Calories ~ SubTotal + BL1 + BL2 + Signage + Coupon:cafe + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 2 Model 3
lm <- lm(Proj_Calories ~ SubTotal + BL1 + BL2 + Signage + Coupon:cafe + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 2 Model 4
lm <- lm(Proj_Calories ~ SubTotal + BL1 + BL2 + Signage + Coupon:cafe +
           cafe:Qtr + Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 2 Model 5
lm <- lm(Proj_Calories ~ SubTotal + BL1 + BL2 + Signage + Coupon:cafe +
           cafe:Qtr + Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 3 Model 1
lm <- lm(Cal_p_Dollar ~ SubTotal + Treatment + cafe:Coupon,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = TotalSales)
summary(lm)

#MA Thesis Table 3 Model 2
lm <- lm(Cal_p_Dollar ~ SubTotal + Treatment + cafe:Coupon + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = TotalSales)
summary(lm)

#MA Thesis Table 3 Model 3
lm <- lm(Cal_p_Dollar ~ SubTotal + Treatment + cafe:Coupon + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = TotalSales)
summary(lm)

#MA Thesis Table 3 Model 4
lm <- lm(Cal_p_Dollar ~ SubTotal + Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = TotalSales)
summary(lm)

#MA Thesis Table 3 Model 5
lm <- lm(Cal_p_Dollar ~ SubTotal + Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = TotalSales)
summary(lm)

#MA Thesis Table 4 Model 1
lm <- lm(SubTotal ~ Treatment + cafe:Coupon,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 4 Model 2
lm <- lm(SubTotal ~ Treatment + cafe:Coupon + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 4 Model 3
lm <- lm(SubTotal ~ Treatment + cafe:Coupon + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 4 Model 4
lm <- lm(SubTotal ~ Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 4 Model 5
lm <- lm(SubTotal ~ Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 5 Model 1
lm <- lm(Items ~ SubTotal + Treatment + cafe:Coupon,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 5 Model 2
lm <- lm(Items ~ SubTotal + Treatment + cafe:Coupon + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 5 Model 3
lm <- lm(Items ~ SubTotal + Treatment + cafe:Coupon + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 5 Model 4
lm <- lm(Items ~ SubTotal + Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 5 Model 5
lm <- lm(Items ~ SubTotal + Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 6 Model 1
lm <- lm(TotalSales ~ Treatment + cafe:Coupon,
         data=daylvl[which(daylvl$cafe!="Law"),])
summary(lm)

#MA Thesis Table 6 Model 2
lm <- lm(TotalSales ~ Treatment + cafe:Coupon + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),])
summary(lm)

#MA Thesis Table 6 Model 3
lm <- lm(TotalSales ~ Treatment + cafe:Coupon + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),])
summary(lm)

#MA Thesis Table 6 Model 4
lm <- lm(TotalSales ~ Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),])
summary(lm)

#MA Thesis Table 6 Model 5
lm <- lm(TotalSales ~ Treatment + cafe:Coupon + cafe:Qtr +
           Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),])
summary(lm)

#Change the coding of interventions and reload the data before running the code below
#MA Thesis Table 7 Model 1
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 7 Model 2
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 7 Model 3
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 7 Model 4
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe +cafe:Qtr + Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 7 Model 5
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe:Qtr + Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#Re-aggregate the date-cafe-lvl dataset with different sampling criterion before running the following code
#MA Thesis Table 8 Model 1
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 8 Model 2
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 8 Model 3
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe:Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 8 Model 4
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe +cafe:Qtr + Week_of_Qtr,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)

#MA Thesis Table 8 Model 5
lm <- lm(Proj_Calories ~ SubTotal + Treatment + Coupon:cafe + cafe:Qtr + Week_of_Qtr + Day_of_Week,
         data=daylvl[which(daylvl$cafe!="Law"),],
         weights = Count)
summary(lm)
