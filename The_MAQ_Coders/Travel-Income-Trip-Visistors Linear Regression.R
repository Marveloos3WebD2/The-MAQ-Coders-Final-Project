#Load Libraries
install.packages("Hmisc")
library("car")
library("dplyr")
library("effects")
library("Hmisc")
library("rcompanion")
library(tidyr)
library("mvnormtest")
library("multcomp")
library ("IDPmisc")
library("psych")


#Explore data:
head.(TravelNA)


#Question Set Up
#How does monthly income influence Numbers of Visistors per trip and number of Trips?

#IV= monthly income
#DV1= NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 
## make sure DVs are numeric
str(Travel$NumberOfPersonVisiting)
str(Travel$NumberOfTrips)

## Remove missing data
TravelNA <- NaRV.omit (Travel)


###Subset data
keeps <- c("NumberOfTrips", "NumberOfPersonVisiting")
TravelDVs <- TravelNA[keeps]

### Turn data into matrix
TravelDVsM <- as.matrix(TravelDVs)
TravelDVsM

#Test Assumptions
## Sample size:  with  4,128 entries our data exceed requirement

## Multivariate Normality
mshapiro.test(t(TravelDVsM))
###p-value < 2.2e-16 --> significant -> violate assumption.
###So unfortunately, these data do not meet the assumption for MANOVAs.
### We will run simple linear regressions for each DV.

#Scatter Plots
d <- ggplot(TravelNA, aes(x = NumberOfPersonVisiting, y = MonthlyIncome))
d + geom_point() + ggtitle("Number of Visitors per Trip and Monthly Income") +
        xlab("Visitors per Trip") + ylab("Monthly Income")+ geom_smooth(method=lm)
## Observation: The majority of customers have income falling in 15,000 to 40,000.
###People of higher income takes average 3 or 4 trips. 
###People who take 5 trips does not have signification different in their income.
## Although weak, income has a positive correlation with Number of Visitors per Trips

d <- ggplot(TravelNA, aes(x = NumberOfTrips, y = MonthlyIncome))
d + geom_point() + ggtitle("Number of Trips and Monthly Income") +
        xlab("Number Of Trips") + ylab("Monthly Income")+ geom_smooth(method=lm)
##Observation: Most people make under 10 trips total. 
##People making 20 trips and above does not have signification different income.
##People with the most income does not make much trips.
## Income has very little impact on number of trips taken.

#Calculating Correlation
cor.test(TravelNA$MonthlyIncome, TravelNA$NumberOfPersonVisiting, method="pearson", use = "complete.obs")
### p-value < 2.2e-16 indicate this is a significant correlation.
### r correlation coefficient is 0.1382458 indicate the correlation between 
### Monthly Income and Visitors per Trip is a weak positive correlation.

cor.test(TravelNA$MonthlyIncome, TravelNA$NumberOfTrips, method="pearson", use = "complete.obs")
### p-value < 1.87e-14 indicate this is a significant correlation.
### r correlation coefficient is 0.1188247 indicate the correlation between 
### Monthly Income and Number of Trip is a very weak positive correlation.
