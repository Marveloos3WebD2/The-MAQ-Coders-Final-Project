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
#How does TypeofContact influence Numbers of Visistors per trip and number of Trips among customers who made a purchase?

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
TravelProd <- subset(TravilNA, ProdTaken == 1)

#Check value for NumberOfTrips
table(TravelProd$NumberOfTrips)
### Since there is only 1 value of 19 and 20 trips, we are going to remove these values.

## Remove outliers 
ProdTrip <- subset(TravelProd, NumberOfTrips <= 10)
#Check value for NumberOfTrips
table(ProdTrip$NumberOfTrips)

#Make sure variables are appropriate datatype

## make sure DVs are numeric
str(ProdTrip$NumberOfPersonVisiting)
str(ProdTrip$NumberOfTrips)

#### make sure CityTier is categorical
###Check datatype
class(ProdTrip$CityTier)
table(ProdTrip$CityTier)
###Convert data
CityTierCat <- as.factor(ProdTrip$CityTier) 
###Double check datatype
class(CityTierCat)

#### make sure Passport is categorical
###Check datatype
class(ProdTrip$Passport)
table(ProdTrip$Passport)
###Convert data
PassportCat <- as.factor(ProdTrip$Passport) 
###Double check datatype
class(PassportCat)

#### make sure OwnCar is categorical
###Check datatype
class(ProdTrip$OwnCar)
table(ProdTrip$OwnCar)
###Convert data
OwnCarCat <- as.factor(ProdTrip$OwnCar) 
###Double check datatype
class(OwnCarCat)



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



### Type of Contact
##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ TypeofContact, data=ProdTrip)
#P-value of 0.7251 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ TypeofContact, data=ProdTrip)
#P-value of 0.7251 > 0.05 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVA <- manova(cbind(NumberOfPersonVisitingSquare, NumberOfTripsLOG) ~ TypeofContact, data = ProdTrip)
summary(MANOVA)
###With Pr(>F) value of 0.003, this is significant too.
###There is a significant difference in NumberOfPersonVisitingSquare and NumberOfTripsLOG by TypeofContact.

##ANOVAs as Post Hocs
summary.aov(MANOVA, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.1449 is not significant.
###NumberOfTrips Pr(>F) value of 0.01928 is significant.
###There is a significant difference in Number Of Trips by TypeofContact.
###There is not a significant difference in Number Of Visitors by TypeofContact




#CityTier

##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ CityTierCat, data=ProdTrip)
#P-value of 0.8514 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ CityTierCat, data=ProdTrip)
#P-value of 0.7424 > 0.05 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVACT <- manova(cbind(NumberOfPersonVisitingSquare, NumberOfTripsLOG) ~ CityTierCat, data = ProdTrip)
summary(MANOVACT)
###With Pr(>F) value of 0.04258 almost = 0.05, this very significant correlation.
###CityTier has some influence in NumberOfPersonVisitingSquare and NumberOfTripsLOG.

##ANOVAs as Post Hocs
summary.aov(MANOVACT, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.06442 is not significant.
###NumberOfTrips Pr(>F) value of 0.1144 is not significant.
###There is a not significant difference in Number Of Trips nor Visitors by CityTier.
###However, CityTier has more of an impact on Number of Visitors than number of Trips.



#Gender 

##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ Gender, data=ProdTrip)
#P-value of 0.5505 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ Gender, data=ProdTrip)
#P-value of 0.2776 > 0.05 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVAG <- manova(cbind(NumberOfPersonVisitingSquare, NumberOfTripsLOG) ~ Gender, data = ProdTrip)
summary(MANOVAG)
###With Pr(>F) value of 0.08759 > 0.05, this not a significant correlation.
###Gender does not influence in NumberOfPersonVisitingSquare and NumberOfTripsLOG.



# MaritalStatus 

##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ MaritalStatus, data=ProdTrip)
#P-value of 0.7335 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ MaritalStatus, data=ProdTrip)
#P-value of 0.0001317 < 0.05 means this is a significant. --> does not pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVAM <- manova(cbind(NumberOfPersonVisitingSquare, NumberOfTripsLOG) ~ MaritalStatus, data = ProdTrip)
summary(MANOVAM)
###With Pr(>F) value of 0.04258 almost = 0.05, this very significant correlation.
###MaritalStatus has some influence in NumberOfPersonVisitingSquare and NumberOfTripsLOG.

##ANOVAs as Post Hocs
summary.aov(MANOVAM, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 1.693e-11 is significant.
###NumberOfTrips Pr(>F) value of 0.002528  is significant.
###There is a not significant difference in Number Of Trips nor Visitors by MaritalStatus.
###MaritalStatus does not have an impact on neither Number of Visitors nor number of Trips.


# Passport

##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ PassportCat, data=ProdTrip)
#P-value of 0.6659 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ PassportCat, data=ProdTrip)
#P-value of 0.4159 > 0.05 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVAP <- manova(cbind(NumberOfPersonVisitingSquare, NumberOfTripsLOG) ~ PassportCat, data = ProdTrip)
summary(MANOVAP)
###With Pr(>F) value of 0.1284 > 0.05, this is not a very significant correlation.
###Passport does not have any influence in NumberOfPersonVisitingSquare and NumberOfTripsLOG.



# OwnCar

##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ OwnCarCat, data=ProdTrip)
#P-value of 0.6659 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ OwnCarCat, data=ProdTrip)
#P-value of 0.4159 > 0.05 means this is a significant. --> does not pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVACAR <- manova(cbind(NumberOfPersonVisitingSquare, NumberOfTripsLOG) ~ OwnCarCat, data = ProdTrip)
summary(MANOVACAR)
###With Pr(>F) value of 0.1284> 0.05, this is not a significant correlation.
###OwnCar does not has some influence in NumberOfPersonVisitingSquare and NumberOfTripsLOG.



#	Designation 

##Homogeneity of Variance
leveneTest(NumberOfPersonVisitingSquare ~ Designation, data=ProdTrip)
#P-value of 0.9955 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ Designation, data=ProdTrip)
#P-value of 0.002285 < 0.05 means this is significant. --> does not pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.

##The Analysis
MANOVADes <- manova(cbind(NumberOfPersonVisitingSquare, NumberOfTripsLOG) ~ Designation, data = ProdTrip)
summary(MANOVADes)
###With Pr(>F) value of 0.001557 < 0.05, this is a very significant correlation.
###Designation has some influence in NumberOfPersonVisitingSquare and NumberOfTripsLOG.

##ANOVAs as Post Hocs
summary.aov(MANOVADes, test = "wilks") 

###NumberOfPersonVisiting Pr(>F) value of 0.1186 is not significant.
###NumberOfTrips Pr(>F) value of 0.0002882 is a significant.
###There is a difference in NumberOfTrips but not Number of Visitors by Designation. 

