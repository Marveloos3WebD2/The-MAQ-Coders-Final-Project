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
library("gmodels")
library("PerformanceAnalytics")


#Removing NA values:
TravelNA <- na.omit (Travel)
head(TravelNA)


# Exploring Dataset:

# Converting categorical variables into numeric values
TravelConvert <- TravelNA
TravelConvert[sapply(TravelConvert, is.factor)] <- data.matrix(TravelConvert[sapply(TravelConvert, is.factor)])

# Correlation Matrix
Travelmatrix <- cor(TravelConvert)
View(round(Travelmatrix, 2))

chart.Correlation(Travelmatrix, histogram=FALSE, method="pearson")


##Matrix Correlation observatioN:
#DVs: ProdTaken
#Highly Correlated IVs:
        ###Passport 0.271
        ###MaritalStatus 0.147
        ###NumberOfFollowups 0.11
        ###Designation -0.107
        ###MonthlyIncome -0.133
        ###Age -0.15
        ###ProductPitched  -0.15

## Exam Categorical variables that are highly correlated with ProdTaken:
keeps <- c ("ProdTaken", "Passport","MaritalStatus","Designation", "ProductPitched")
ProdCat <- TravelNA[keeps] 

#Independent Chi-Squares analysis

##Passport
CrossTable(ProdCat$ProdTaken, ProdCat$Passport, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
###Since all of our expected values are above 5, our data pass the assumption.
###With a p value of 4.848102e-68, this analysis is significant, meaning that having a passport or not does in fact make a difference in whether potential customer makes a purchased.

##Maritial Status
CrossTable(ProdCat$ProdTaken, ProdCat$MaritalStatus, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
###Since all of our expected values are above 5, our data pass the assumption.
###With a p value of 1.729159e-40, this analysis is significant, meaning that martial status in fact make a difference in whether potential customer makes a purchased.

##Designation
CrossTable(ProdCat$ProdTaken, ProdCat$Designation, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")
###Since all of our expected values are above 5, our data pass the assumption.
###With a p value of 8.401618e-44, this analysis is significant, meaning that Designation in fact make a difference in whether potential customer makes a purchased.

##ProductPitched
CrossTable(ProdCat$ProdTaken, ProdCat$ProductPitched, fisher=TRUE, chisq = TRUE, expected = TRUE, sresid=TRUE, format="SPSS")

## Exam Continuous variables that are highly correlated with ProdTaken:
## We want to know information among those customers who made a purchase.

##Creating Subset with all customers who bought the Products.
TravelProd <- subset(Travel, ProdTaken == 1)
ProdNA <- na.omit (TravelProd)
head(ProdNA)
table(TravelNA$ProdTaken)
### only 797 customers bought product, 3331 customers did not.

## Drop Categorical variable
Conkeeps <- c ("ProdTaken", "MonthlyIncome","Age","NumberOfFollowups")
ProdCon <- TravelNA[Conkeeps] 


#Stepwise Regression Analysis for Income Iv and DVs of NumberOfTrips and ProdTaken
##Question Set Up
###Does the Monthly Income influence the number of trips and customer decision to make a purchase?

##Data Wrangling: Ensure dependent Variables are Numeric
str(ProdCon$NumberOfTrips)
str(ProdCon$ProdTaken)
### Since both variable are int, we can proceed.

##Subsetting keep only your two dependent variabes
ConDVskeeps <- c("NumberOfTrips", "ProdTaken")
ProdConDVs <- ProdCon[ConDVskeeps]

##Although the test for normality can only handle 5,000 records, with 4,128 entries we are good to go.

##Format as a Matrix
ProdConIncome <- as.matrix(ProdConDVs)

##Test Assumptions:
###Sample Size: with a dataset of 323,746, our dataset fullfil the at least 20 cases per independent variable and that there must be more cases then dependent variables in every cell. 



