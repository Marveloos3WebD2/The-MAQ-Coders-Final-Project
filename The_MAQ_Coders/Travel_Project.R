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


#Explore data:
TravelNA <- na.omit (Travel)
head.(TravelNA)


# Choosing Appropriate Statistical Analyses:


# Converting categorical variables into numeric values
TravelConvert <- TravelNA
TravelConvert[sapply(TravelConvert, is.factor)] <- data.matrix(TravelConvert[sapply(TravelConvert, is.factor)])


# Correlation Matrix
Travelmatrix <- cor(TravelConvert)
View(round(Travelmatrix, 2))

##Matrix Correlation observatioN:
#DVs: NumberOfTrips and NumberOfPersonVisiting
##   correlated with each other 0.19 and the following IVs:
        ##NumberofChildrenVisiting continuous
        ##NumberofFollowups
        ##Monthly Income: continuous
        ##ProductPitch (number of visitors only)
        ##DurationPitch (number of visitors only)
        ##Age (number of trips only)

# IVs:that have litle correlation with our DVs 
        ##Occupation: Categorical more than 2 levels
        ##CityTier: Categorical more than 2 levels
        ##Designation: Categorical more than 2 levels

TripFollowUps <- TravelNA %>% group_by(NumberOfFollowups) %>% summarise_at(vars(NumberOfTrips), list(AverageTrip = mean))
TripFollowUps

VisitorsFollowUps <- TravelNA %>% group_by(NumberOfFollowups) %>% summarise_at(vars(NumberOfPersonVisiting), list(AverageVistors = mean))
VisitorsFollowUps
# Customers receive  of 5 and 6 follow ups have higher average number of trips made and number of person visiting.
# Customers receive 3 follow ups makes less average number of trips as well as lower average number person visiting.

TripIncome <- TravelNA %>% group_by(NumberOfTrips) %>% summarise_at(vars(MonthlyIncome), list(AverageIncome = mean))
TripIncome
VisitorsIncome <- TravelNA %>% group_by(NumberOfPersonVisiting) %>% summarise_at(vars(MonthlyIncome), list(AverageIncome = mean))
VisitorsIncome
#Customer with the highest average monthly income makes the most trips on average.
#However, customer with the lowest average monthly income of 17285 only rank 4th with 19 trips on average. Good potential customer target groups to 
##Worth noticing here is the group with most visitors rank 3rd in monthly income.
##Second by the highest group in monthly income.

TripChildren <- TravelNA %>% group_by(NumberOfChildrenVisiting) %>% summarise_at(vars(NumberOfTrips), list(AverageTrips = mean))
TripChildren
VisitorsChildren <- TravelNA %>% group_by(NumberOfChildrenVisiting) %>% summarise_at(vars(NumberOfPersonVisiting), list(AverageVistors = mean))
VisitorsChildren
##The higher number of children visiting, the higher average Trips and Visitors.
##Having children along is a strong indicator for customers to take trips.


TripAge <- TravelNA %>% group_by(NumberOfTrips) %>% summarise_at(vars(Age), list(AverageAge = mean))
TripAge
##People who are 39 to 40 have the highest number of trips, second by 30 to 31 age group.
##Notice customer of 41 years of age make only 4 to 5 trips.


VisitorsProduct <- TravelNA %>% group_by(ProductPitched) %>% summarise_at(vars(NumberOfPersonVisiting), list(AverageVisitors = mean))
VisitorsProduct
TripsProduct <- TravelNA %>% group_by(ProductPitched) %>% summarise_at(vars(NumberOfTrips), list(AverageTrips = mean))
TripsProduct
##Deluxe has the highest average visitors.
##Super Deluxe has 2nd to the lowest number of visitors but the most average number of trips.
##King has the lowest average visitors and number of trips.


VisitorsPitch <- TravelNA %>% group_by(NumberOfPersonVisiting) %>% summarise_at(vars(DurationOfPitch), list(AverageDuration = mean))
VisitorsPitch
##The longer the pitch duration the more visitors per trips.
