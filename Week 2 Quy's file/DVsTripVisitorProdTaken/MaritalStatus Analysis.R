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


#Question SetUp: Does Maritial Status influence Number of Visitors and Trips

#IV= MaritalStatus
#DV1= NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 


##Creating Subset with all customers who bought the Products.
#remove missing data
TravilNA <- na.omit (Travel)

#Check value for ProdTaken
table(TravelNA$ProdTaken)
### only 797 customers bought product, 3331 customers did not.

#Subset data select only value of ProdTaken = 1
TravelProd <- subset(TravilNA, ProdTaken == 1)

#Check value for NumberOfTrips
table(TravelProd$NumberOfTrips)
### Since there is only 1 value of 19 and 20 trips, we are going to remove these values.

## Remove outliers 
ProdTrip <- subset(TravelProd, NumberOfTrips <= 10)
#Check value for NumberOfTrips
table(ProdTrip$NumberOfTrips)

#Make sure variables are appropriate datatype

#subset data to select only variables examine
MSKeep <- c("NumberOfPersonVisiting", "NumberOfTrips", "MaritalStatus")
TravelMaritalStatus <- ProdTrip[MSKeep]

## make sure DVs are numeric
str(ProdTrip$NumberOfPersonVisiting)
str(ProdTrip$NumberOfTrips)

#Test Normality Assumptions
plotNormalHistogram(apps$NumberOfPersonVisiting)
plotNormalHistogram(apps$NumberOfPersonVisiting)

