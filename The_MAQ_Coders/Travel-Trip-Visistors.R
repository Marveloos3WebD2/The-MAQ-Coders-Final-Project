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
head.(TravelNA)


#Question Set Up
#How does monthly income influence Numbers of Visistors per trip and number of Trips?

#IV= monthly income
#DV1= NumberOfPersonVisiting
#DV2 = NumberOfTrips

#Data Wrangling: 

## Remove missing data
TravelNA <- NaRV.omit (Travel)


#Scatter Plots
d <- ggplot(TravelNA, aes(x = NumberOfTrips, y = NumberOfPersonVisiting))
d + geom_point() + ggtitle("Number of Trips and Visitors per Trip") +
  xlab("Number Of Trips") + ylab("Visitors ")+ geom_smooth(method=lm)
###Number of Trips and Number of Visitors per Trips have a strong positive correlation.
###The majority of customers take under 10 trip with any group size.
###Groups of 2 to 4 make the most trips. 
###People travel alone and group of 5 make 5 more less trip.



#Calculating Correlation
cor.test(TravelNA$NumberOfTrips, TravelNA$NumberOfPersonVisiting, method="pearson", use = "complete.obs")
###p-value < 2.2e-16 indicate this is a significant correlation.
###r correlation coefficient is 0.1869892 indicates the correlation between Number of Trips and Visitors per Trip is a weak positive correlation. 
###Our calculation result affirms our Scatter Plots.
