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
#How does the following IVs influence DVs among customers who did not made a purchase?

#IV1 = TypeofContact
#IV2 = Occupation
#IV3 = Gender
#IV4 = MaritalStatus
#IV5 = Passport

#DV1 = NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 


#Check value for ProdTaken
table(Travel$ProdTaken)
### only 920 customers bought product, 3968 customers did not.

#Subset data select only value of ProdTaken = 0
NotTakenProd <- subset(Travel, ProdTaken == 0)

#remove missing data
NoProdNA <- na.omit (NotTakenProd)

#Check value for NumberOfTrips
table(NoProdNA$NumberOfTrips)
### Since there is only 1 value of 19 and 20 trips, we are going to remove these values.

## Remove outliers 
NOProdTrip <- subset(NoProdNA, NumberOfTrips <= 10)
#Check value for NumberOfTrips
table(NOProdTrip$NumberOfTrips)
head(NOProdTrip)

## make sure DVs are numeric
str(NOProdTrip$NumberOfPersonVisiting)
str(NOProdTrip$NumberOfTrips)

#### make sure Passport is categorical
###Check datatype
class(NOProdTrip$Passport)
table(NOProdTrip$Passport)
###Convert data
PassportCat <- as.factor(NOProdTrip$Passport) 
###Double check datatype
class(PassportCat)


## Check normality of DVs

#Number of Person Visiting
plotNormalHistogram(NOProdTrip$NumberOfPersonVisiting, main = "Number of Visitors Original Histogram")

NOProdTrip$NumberOfPersonVisitingSquare <- NOProdTrip$NumberOfPersonVisiting ^ 2
plotNormalHistogram(NOProdTrip$NumberOfPersonVisitingSquare, main = "Number of Visitors Square Histogram")

NOProdTrip$NumberOfPersonVisitingCUBE <- NOProdTrip$NumberOfPersonVisiting ^ 3
plotNormalHistogram(NOProdTrip$NumberOfPersonVisitingCUBE, main = "Number of Visitors Cube Histogram")

NOProdTrip$NumberOfPersonVisitingLOG <- log(NOProdTrip$NumberOfPersonVisiting)
plotNormalHistogram(NOProdTrip$NumberOfPersonVisitingLOG, main = "Number of Visitors LOG Histogram")
####Log model is most normally distributed

##Number of Trips
plotNormalHistogram(NOProdTrip$NumberOfTrips, main = "Number of Trips Original Histogram")

NOProdTrip$NumberOfTripsSquare <- NOProdTrip$NumberOfTrips ^ 2
plotNormalHistogram(NOProdTrip$NumberOfTripsSquare, main = "Number of Trips Square Histogram")

NOProdTrip$NumberOfTripsSquareCUBE <- NOProdTrip$NumberOfTripsSquare ^ 3
plotNormalHistogram(NOProdTrip$NumberOfTripsSquareCUBE, main = "Number of Trips Cube Histogram")

NOProdTrip$NumberOfTripsLOG <- log(NOProdTrip$NumberOfTrips)
plotNormalHistogram(NOProdTrip$NumberOfTripsLOG, main = "Number of Trips LOG Histogram")
####Log model is most normally distributed


###Subset data
keeps <- c("NumberOfPersonVisitingSquare", "NumberOfTripsLOG")
TravelDVs <- NOProdTrip[keeps]

### Turn data into matrix
TravelDVsM <- as.matrix(TravelDVs)
TravelDVsM

#Test Assumptions
## Sample size:  with  906 entries our data exceed requirement

## Multivariate Normality
mshapiro.test(t(TravelDVsM))
###p-value < 2.2e-16 --> significant -> violate assumption.
###So unfortunately, these data do not meet the assumption for MANOVAs.
###However, our Histogram resemble normal distribution. We are proceed with analysis.



### Type of Contact
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingLOG ~ TypeofContact, data=NOProdTrip)
#P-value of 0.4865 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ TypeofContact, data=NOProdTrip)
#P-value of 0.0456 < 0.05 means this is a significant. --> did not pass the assumption

##Absence of Multicollinearity
cor.test(NOProdTrip$NumberOfPersonVisitingLOG, NOProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .228 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVA <- manova(cbind(NumberOfPersonVisitingLOG, NumberOfTripsLOG) ~ TypeofContact, data = NOProdTrip)
summary(MANOVA)
###With Pr(>F) value of 0.8183, this is not a significant test.
###There is not a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by TypeofContact.

##ANOVAs as Post Hocs
summary.aov(MANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.6963 is not significant.
###NumberOfTrips Pr(>F) value of 0.6919 is not significant.
###There is not a significant difference in Number Of Visitors nor trips by TypeofContact


