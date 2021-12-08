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
#How does Numbers of Visistors per trip different among Occupation?

#IV= Occupation
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

##Homogeneity of Variance


### Turn data into matrix
fligner.test(NumberOfPersonVisitingSquare ~ Occupation, data=ProdTrip)
###p-value = 0.7377 >0.05 which means our data pass the assumption of homogeneity of variance.

###Sample Size: 795 entries assure we pass the requirements at least 20 per independent variable and 


##Computing ANOVAs with Equal Variance (Met Homogeneity of Variance Assumption)

OccupationANOVA <- aov(ProdTrip$NumberOfPersonVisitingSquare ~ ProdTrip$Occupation)
summary(OccupationANOVA)

### Pr(>F) of 0.293 > 0.05. This is not Significant.

###Computing ANOVAs with Unequal Variance (Violated Homogeneity of Variance Assumption)
OCANOVA <- lm(NumberOfPersonVisitingSquare ~ Occupation, data=ProdTrip)
Anova(OCANOVA, Type="II", white.adjust=TRUE)
### Pr(>F) of 0.3761 > 0.05. This is not Significant.
### The Numbers of Visistors per trip is not different among different Occupation.
