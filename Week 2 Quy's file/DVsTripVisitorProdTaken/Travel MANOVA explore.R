#Load Libraries
install.packages("Hmisc")
install.packages("ggstatsplot")

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
library(ggstatsplot)

#Explore data:
head(Travel)


#Question Set Up
#How does  influence Numbers of Visistors per trip and number of Trips among customers who made a purchase?

#IV= TypeofContact
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
TravelProd <- subset(Travel, ProdTaken == 1)

#Check value for NumberOfTrips
table(TravelProd$NumberOfTrips)
### Since there is only 1 value of 19 and 20 trips, we are going to remove these values.

## Remove outliers 
ProdTrip <- subset(TravelProd, NumberOfTrips <= 10)
#Check value for NumberOfTrips
table(ProdTrip$NumberOfTrips)


## make sure DVs are numeric
str(ProdTrip$NumberOfPersonVisiting)
str(ProdTrip$NumberOfTrips)

## Check normality of DVs

#Number of Person Visiting
plotNormalHistogram(ProdTrip$NumberOfPersonVisiting, main = "Number of Visitors Original Histogram")

ProdTrip$NumberOfPersonVisitingSquare <- ProdTrip$NumberOfPersonVisiting ^ 2
plotNormalHistogram(ProdTrip$NumberOfPersonVisitingSquare, main = "Number of Visitors Square Histogram")

ProdTrip$NumberOfPersonVisitingCUBE <- ProdTrip$NumberOfPersonVisiting ^ 3
plotNormalHistogram(ProdTrip$NumberOfPersonVisitingCUBE, main = "Number of Visitors Cube Histogram")

ProdTrip$NumberOfPersonVisitingLOG <- log(ProdTrip$NumberOfPersonVisiting)
plotNormalHistogram(ProdTrip$NumberOfPersonVisitingLOG, main = "Number of Visitors LOG Histogram")
####Square model is most normally distributed

##Number of Trips
plotNormalHistogram(ProdTrip$NumberOfTrips, main = "Number of Trips Original Histogram")

ProdTrip$NumberOfTrips <- transformTukey(ProdTrip$NumberOfTrips, plotit=TRUE)

ProdTrip$NumberOfTripsSquare <- ProdTrip$NumberOfTrips ^ 2
plotNormalHistogram(ProdTrip$NumberOfTripsSquare, main = "Number of Trips Square Histogram")

ProdTrip$NumberOfTripsSquareCUBE <- ProdTrip$NumberOfTripsSquare ^ 3
plotNormalHistogram(ProdTrip$NumberOfTripsSquareCUBE, main = "Number of Trips Cube Histogram")

ProdTrip$NumberOfTripsLOG <- log(ProdTrip$NumberOfTrips)
plotNormalHistogram(ProdTrip$NumberOfTripsLOG, main = "Number of Trips LOG Histogram")
####Log model is most normally distributed


###Subset data
keeps <- c("NumberOfPersonVisitingSquare", "NumberOfTripsLOG")
TravelDVs <- ProdTrip[keeps]

### Turn data into matrix
TravelDVsM <- as.matrix(TravelDVs)
TravelDVsM

#Test Assumptions
## Sample size:  with  906 entries our data exceed requirement

## Multivariate Normality
mshapiro.test(t(TravelDVsM))
###p-value < 4.081e-13 --> significant -> violate assumption.
###So unfortunately, these data do not meet the assumption for MANOVAs.
###However, our Histogram resemble normal distribution. We are proceed with analysis.


##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ TypeofContact, data=ProdTrip)


leveneTest(NumberOfTripsLOG ~ TypeofContact, data=ProdTrip)

