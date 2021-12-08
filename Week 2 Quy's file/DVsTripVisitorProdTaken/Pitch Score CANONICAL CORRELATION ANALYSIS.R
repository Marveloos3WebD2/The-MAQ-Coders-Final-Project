#Load Libraries
install.packages("Hmisc")
install.packages("ggstatsplot")
install.packages("GGally")
install.packages("CCA")
install.packages("CRAN")
install.packages("CCP")


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
library(GGally)
library(CCA)
library (CCP)


#Explore data:
head(Travel)


#Question Set Up
#How does PitchSatisfactionScore and ProductPitched influence Numbers of Visistors per trip and number of Trips among customers who made a purchase?

#IV1 = PitchSatisfactionScore
#IV2 = DurationOfPitch 
#DV1 = NumberOfPersonVisiting
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
head(ProdTrip)

## make sure variables are numeric
str(ProdTrip$NumberOfPersonVisiting)
str(ProdTrip$NumberOfTrips)
str(ProdTrip$PitchSatisfactionScore)

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
#P-value of 0.7251 > 0.05 means this is not a significant. --> pass the assumption

leveneTest(NumberOfTripsLOG ~ TypeofContact, data=ProdTrip)
#P-value of 0.7251 > 0.394 means this is not a significant. --> pass the assumption

##Absence of Multicollinearity
cor.test(ProdTrip$NumberOfPersonVisitingSquare, ProdTrip$NumberOfTripsLOG, method="pearson", use="complete.obs")
#Finally an assumption you have met! With a correlation of r = .27 < 0.7, you have an absence of multicollinearity.


##The Canonical Correlation Analysis
#Subset data with IVs and DVs of this analysis
#IV1 = PitchSatisfactionScore
#IV2 = ProductPitched 
#DV1 = NumberOfPersonVisiting
#DV2 = NumberOfTrips
keeps <- c ("PitchSatisfactionScore", "DurationOfPitch", "NumberOfPersonVisiting","NumberOfTrips")
PitchScoreCC <- ProdTrip[keeps] 

#IVsSubset: X
IVs <- PitchScoreCC [, 1:2]
#DVsSubset: Y
DVs <- PitchScoreCC [, 3:4]

#Exam the correlation within each subset
ggpairs(IVs)
ggpairs(DVs)

#Exam the correlation between the two sets of variables using matcor from CCA
# correlations
matcor(IVs, DVs)


#Code to pull section of the above results:
cc1 <- cc(DVs, IVs)
# display the canonical correlations
cc1$cor

# raw canonical coefficients
cc1[3:4]


# compute canonical loadings
cc2 <- comput(DVs, IVs, cc1)

# display canonical loadings
cc2[3:6]

# tests of canonical dimensions
rho <- cc1$cor
## Define number of observations, number of variables in first set, and number of variables in the second set.
n <- dim(DVs)[1]
p <- length(DVs)
q <- length(IVs)

## Calculate p-values using the F-approximations of different test statistics:
p.asym(rho, n, p, q, tstat = )

p.asym(rho, n, p, q, tstat = "Wilks")

p.asym(rho, n, p, q, tstat = "Hotelling")

p.asym(rho, n, p, q, tstat = "Pillai")

p.asym(rho, n, p, q, tstat = "Roy")

# standardized IVs canonical coefficients diagonal matrix of IVs sd's
s1 <- diag(sqrt(diag(cov(IVs))))
s1 %*% cc1$xcoef

# standardized DVs canonical coefficients diagonal matrix of DVs sd's
s2 <- diag(sqrt(diag(cov(DVs))))
s2 %*% cc1$ycoef



