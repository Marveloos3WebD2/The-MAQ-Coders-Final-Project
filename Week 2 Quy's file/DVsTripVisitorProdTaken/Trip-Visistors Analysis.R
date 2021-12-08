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
head(Travel)


#Question Set Up
#How does monthly income influence Numbers of Visistors per trip and number of Trips among customers who made a purchase?

#IV= monthly income
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

#View ProdNA
head(ProdNA)

#Scatter Plots
d <- ggplot(ProdNA, aes(x = NumberOfTrips, y = NumberOfPersonVisiting))
d + geom_point() + ggtitle("Number of Trips and Visitors per Trip") +
  xlab("Number Of Trips") + ylab("Visitors ")+ geom_smooth(method=lm)
###Number of Trips and Number of Visitors per Trips have a strong positive correlation.
###The majority of customers take under 10 trip with any group size.
###Groups of 4 make the most trips. 


#Calculating Correlation
cor.test(ProdNA$NumberOfTrips, ProdNA$NumberOfPersonVisiting, method="pearson", use = "complete.obs")
###p-value < 2.2e-16 indicate this is a significant correlation.
###r correlation coefficient is 0.1869892 indicates the correlation between Number of Trips and Visitors per Trip is a weak positive correlation. 
###Our calculation result affirms our Scatter Plots.
