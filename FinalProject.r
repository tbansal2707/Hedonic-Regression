library(readr)
library(tidyverse)
library(ggplot2)
library(mosaic)
library(corrplot)
library(dplyr)

setwd("/Users/tusharbansal/Desktop/MSORIE/Sem 1")
hotels <- read_csv("MATH STAT/Cities42FF.csv")

ggplot(hotels) + geom_histogram(aes(x = RoomRent))

#Most of the datapoints lie very close to and below 5000
#On further analysis
summary(hotels$RoomRent)

#We can see the mean and the median are 5601 and 3859 respectively

ggplot(aes(x=factor(IsNewYearEve), y=StarRating), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(IsNewYearEve), y=HotelCapacity), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(IsNewYearEve), y=Airport), data=hotels) + 
  geom_boxplot()


ggplot(aes(x=factor(IsMetroCity), y=StarRating), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(IsMetroCity), y=HotelCapacity), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(IsMetroCity), y=Airport), data=hotels) + 
  geom_boxplot()

ggplot(aes(x=factor(IsTouristDestination), y=StarRating), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(IsTouristDestination), y=HotelCapacity), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(IsTouristDestination), y=Airport), data=hotels) + 
  geom_boxplot()

ggplot(aes(x=factor(FreeWifi), y=StarRating), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(FreeWifi), y=HotelCapacity), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(FreeWifi), y=Airport), data=hotels) + 
  geom_boxplot()

ggplot(aes(x=factor(FreeBreakfast), y=StarRating), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(FreeBreakfast), y=HotelCapacity), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(FreeBreakfast), y=Airport), data=hotels) + 
  geom_boxplot()

ggplot(aes(x=factor(HasSwimmingPool), y=StarRating), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(HasSwimmingPool), y=HotelCapacity), data=hotels) + 
  geom_boxplot()
ggplot(aes(x=factor(HasSwimmingPool), y=Airport), data=hotels) + 
  geom_boxplot()

#Looking at these, it can be seen that (SwimmingPool, StarRating) might have an effect on each other

hotels <- hotels %>% relocate(IsMetroCity:IsNewYearEve, .after = HasSwimmingPool)
hotels <- hotels %>% relocate(RoomRent:Airport, .after = HasSwimmingPool)
cor1 = corrplot(corr = cor(hotels[9:18]), method = "color", addCoef.col = "red")
#The correlation between variables StarRating and Hotel Capacity


#Next, an indiavidual fir for each of the datapoints can be seen

fit1 = lm(RoomRent~IsNewYearEve+IsTouristDestination+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool, data=hotels)
summary(fit1)

fitted_boot1 = do(1000) * {
  fit = lm(RoomRent~IsNewYearEve+IsTouristDestination+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool, data=resample(hotels))
}
confint(fitted_boot1)



fit2 = lm(RoomRent~IsNewYearEve+IsTouristDestination+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating+IsMetroCity, data=hotels)
summary(fit2)

fitted_boot2 = do(1000) * {
  fit = lm(RoomRent~IsNewYearEve+IsTouristDestination+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating+IsMetroCity, data=resample(hotels))
}
confint(fitted_boot2)

vif(fit2)

resids = resid(fit2)
ggplot(aes(y=resids, x=Airport), data=hotels) + 
  geom_point()


fit3 = lm(RoomRent~IsNewYearEve*IsTouristDestination+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating+IsMetroCity, data=hotels)
summary(fit3)
fitted_boot3 = do(1000) * {
  fit = lm(RoomRent~IsNewYearEve*IsTouristDestination+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating+IsMetroCity, data=resample(hotels))
}
confint(fitted_boot3)

#City-Wise Analysis
hotelsdelhi <- hotels %>% filter(CityName =="Delhi") #510 datapoints
fitdelhi = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=hotelsdelhi)
summary(fitdelhi)

fitted_boot4 = do(1000) * {
  fit = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=resample(hotelsdelhi))
}
confint(fitted_boot4)

hotelskolkata <- hotels %>% filter(CityName =="Kolkata") #119 datapoints
fitkolkata = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=hotelskolkata)
summary(fitkolkata)

fitted_boot5 = do(1000) * {
  fit = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=resample(hotelskolkata))
}
confint(fitted_boot5)

hotelsgoa <- hotels %>% filter(CityName =="Goa") #152 datapoints
fitgoa = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=hotelsgoa)
summary(fitgoa)

fitted_boot6 = do(1000) * {
  fit = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=resample(hotelsgoa))
}
confint(fitted_boot6)

hotelschandigarh <- hotels %>% filter(CityName =="Chandigarh") #84 datapoints
fitchandigarh = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=hotelschandigarh)
summary(fitchandigarh)

fitted_boot7 = do(1000) * {
  fit = lm(RoomRent~IsNewYearEve+Airport+FreeBreakfast+FreeWifi+HasSwimmingPool+HotelCapacity+StarRating, data=resample(hotelschandigarh))
}
confint(fitted_boot7)



