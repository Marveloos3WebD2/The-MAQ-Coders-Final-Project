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

#Check value for NumberOfTrips
table(TravelProd$NumberOfTrips)
### Since there is only 1 value of 19 and 20 trips, we are going to remove these values.

## Remove outliers 
ProdTrip <- subset(TravelProd, NumberOfTrips <= 10)
#Check value for NumberOfTrips
table(ProdTrip$NumberOfTrips)


# Converting categorical variables into numeric values
TravelConvert <- ProdTrip
TravelConvert[sapply(TravelConvert, is.factor)] <- data.matrix(TravelConvert[sapply(TravelConvert, is.factor)])

# Correlation Matrix
TravelProdMatrix <- rcorr(as.matrix(TravelConvert))
TravelProdMatrix

#Categorical IVs correlated with both DVs: TypeofContact, CityTier, Gender, MaritalStatus, Passport, OwnCar, Designation 
#Continuous IVs correlated with both DVs:PitchSatisfactionScore
#IVs correlated with NumberofTrips only: DurationPitch and PrefferedPropertyStar
#IVs correlated with NumberOfPersonVisiting only is Occupation

#Exam means of the above variable:

#Group by Type of Contact
TypeofContactM <- ProdTrip %>% group_by(TypeofContact) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
TypeofContactM


#Group by CityTier
CityTierM <- ProdTrip %>% group_by(CityTier) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
CityTierM

#Group by Gender
GenderM <- ProdTrip %>% group_by(Gender) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
GenderM

#Group by MaritalStatus
MaritalStatusM <- ProdTrip %>% group_by(MaritalStatus) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
MaritalStatusM

#Group by Passport
PassportsM <- ProdTrip %>% group_by(Passport) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
PassportsM

#Group by OwnCar
OwnCarM <- ProdTrip %>% group_by(OwnCar) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
OwnCarM

#Group by Designation
DesignationM <- ProdTrip %>% group_by(Designation) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
DesignationM

#Group by PitchSatisfactionScore
ScoreM <- ProdTrip %>% group_by(PitchSatisfactionScore) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
ScoreM


#Group by PrefferedPropertyStar
PropStarM <- ProdTrip %>% group_by(PreferredPropertyStar) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
PropStarM

#Group by Occupation
OccupationM <- ProdTrip %>% group_by(Occupation) %>% summarise_at(vars("NumberOfPersonVisiting", "NumberOfTrips"), list("Average" = mean))
OccupationM


#DurationPitch
table(TravelProd$DurationOfPitch)
table(TravelProd$NumberOfTrips)

TravelProdNA <- na.omit (TravelProd)
PitchM <- TravelProdNA %>% group_by(NumberOfTrips) %>% summarise_at(vars("DurationOfPitch"), list("Duration Average" = mean))
PitchM
max(PitchM$Average)
### Customer receive the longest average duration pitch make mainly 6 trips.
### Customer receive duration pitch of 13.8 make 8 trips.

  
      