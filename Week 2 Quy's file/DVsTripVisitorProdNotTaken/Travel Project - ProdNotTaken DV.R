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


# Exploring Dataset:

#Check value for ProdTaken
table(Travel$ProdTaken)
### only 797 customers bought product, 3331 customers did not.

#Subset data select only value of ProdTaken = 0, then remove ProdTaken
TravelNotProd <- subset(Travel, ProdTaken == 0)
NoProdDrop <- subset(TravelNotProd, select = -c(ProdTaken))

#Removing NA values:
NoProdNA <- na.omit (NoProdDrop)

# Converting categorical variables into numeric values
NoProdConvert <- NoProdNA
NoProdConvert[sapply(NoProdConvert, is.factor)] <- data.matrix(NoProdConvert[sapply(NoProdConvert, is.factor)])

# Correlation Matrix
NoProdmatrix <- cor(NoProdConvert)
View(round(NoProdmatrix, 2))

chart.Correlation(NoProdmatrix, histogram=FALSE, method="pearson")


##Matrix Correlation observation for Prod Not Taken:
#DVs: NumberOfPersonVisiting, NumberOfTrips
#Highly Correlated IVs:
        ###Age (0.21 with Trips)
        ###NumberOfChildrenVisiting (0.59 Visitors, 0.15 Trips)
        ##MonthlyIncome (0.11 visitors, 0.13 Trips)

## Exam Categorical variables that are highly correlated with ProdTaken:
keeps <- c ("NumberOfTrips", "NumberOfPersonVisiting","Age","Designation", "NumberOfChildrenVisiting", "MonthlyIncome")
ProdCat <- NoProdNA[keeps] 

