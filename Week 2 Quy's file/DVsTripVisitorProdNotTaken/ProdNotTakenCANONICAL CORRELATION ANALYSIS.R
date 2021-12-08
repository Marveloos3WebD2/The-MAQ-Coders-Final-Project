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
#How does the following IVs influence Numbers of Visistors per trip and number of Trips among customers who did not made a purchase?

#IV1 = Age
#IV2 = PreferredPropertyStar
#IV3 = PitchSatisfactionScore
#IV4 = DurationOfPitch

#DV1 = NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 


##Creating Subset with all customers who did not taken a the Products.

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

## make sure variables are numeric
str(NOProdTrip$NumberOfPersonVisiting)
str(NOProdTrip$NumberOfTrips)
str(NOProdTrip$PitchSatisfactionScore)
str(NOProdTrip$DurationOfPitch)
str(NOProdTrip$PreferredPropertyStar)
str(NOProdTrip$Age)


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
#IVsSubset: X
IVskeeps <- c("Age","PreferredPropertyStar", "PitchSatisfactionScore", "DurationOfPitch")
IVs <- NOProdTrip[IVskeeps]

#DVsSubset: Y
DVskeeps <- c("NumberOfPersonVisitingLOG", "NumberOfTripsLOG")
DVs <- NOProdTrip[keeps]

#Exam the correlation within each subset
ggpairs(IVs)
ggpairs(DVs)

#Exam the correlation between the two sets of variables using matcor from CCA
# correlations
cormat<-matcor(DVs, IVs)

#Extracting the within study correlations for IVs and DVs and between set cor
round(cormat$Ycor, 4)

#The associations between the two sets can be extracted as
#Between set associations
cormat<-matcor(DVs,IVs)
round(cormat$XYcor, 4)

#Obtain the canonical correlations
can_cor1=cc(DVs, IVs)
can_cor1$cor

#Obtain raw canonical coefficients
can_cor1[3:4]

#computes the canonical loadings
can_cor2=comput(DVs,IVs,can_cor1)
#displays the canonical loadings
can_cor2[3:6] 

#obtain the statistical significance of the dimensions
#test of canonical dimensions
rho=can_cor1$cor
##defining the number of observations, no of variables in first set,
#and number of variables in second set
n=dim(DVs)[1]
p=length(DVs)
q=length(IVs)
##Calculating the F approximations using different test statistics
p.asym(rho,n,p,q,tstat="Wilks")
p.asym(rho,n,p,q,tstat="Hotelling")
p.asym(rho,n,p,q,tstat="Pillai")
p.asym(rho,n,p,q,tstat="Roy")

#Calculating standardized canonical coefficients using R
#standardizing the first set of canonical coefficients(DVs)
std_coef1<-diag(sqrt(diag(cov(DVs))))
std_coef1%*%can_cor1$xcoef
##standardizing the coeficents of the second set (IVs)
std_coef2<-diag(sqrt(diag(cov(IVs))))
std_coef2%*%can_cor1$ycoef
