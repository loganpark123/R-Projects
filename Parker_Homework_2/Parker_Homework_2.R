# Logan Parker
# Fall 2020 CSCI 444
# Homework 2
# Washington DC Bike Sharing Rentals

library(tidyverse)
library(dplyr)

bikes <- read_csv("data/bikeshare-day.csv")

bikes <- bikes %>%
  mutate(season = case_when(season == 1 ~ "Winter",
                            season == 2 ~ "Spring",
                            season == 3 ~ "Summer",
                            season == 4 ~ "Fall")) %>%
  mutate(holiday = case_when(holiday == 0 ~ "No",
                             holiday == 1 ~ "Yes")) %>%
  mutate(workingday = case_when(workingday == 0 ~ "Weekend",
                                workingday == 1 ~ "Weekday")) %>%
  mutate(yr = case_when(yr == 0 ~ 2011,
                        yr == 1 ~ 2012)) %>%
  mutate(weathersit = case_when(weathersit == 1 ~ "Clear",
                                weathersit == 2 ~ "Mist",
                                weathersit == 3 ~ "Light Rain",
                                weathersit == 4 ~ "Heavy Rain")) %>%
  rename("Washington DC Date" = dteday)

bikes <- bikes %>%
  mutate(Temperature = temp*41,
         "Adjusted Temperature" = atemp * 50,
         Humidity = hum *100,
         Windspeed = windspeed * 67)

bikes <- bikes %>%
  mutate(Temperature = (Temperature *9/5)+32,
         "Adjusted Temperature" = (.$"Adjusted Temperature" *9/5)+32)
bikes <- bikes %>%
  mutate(diff=cnt-(.$registered + .$nonregistered))

distinct(bikes,diff)

bikes <- rename(bikes, "Bike Rentals" = cnt)

library(lubridate)
bikes <- bikes %>%
  mutate("Washington DC Date"=as_date(mdy(.$`Washington DC Date`)))

bikes %>%
  ggplot(mapping=aes(x=.$'Washington DC Date',y=.$'Bike Rentals'))+
  geom_point(mapping=aes(color=Temperature))+
  labs(x="Washington DC Date", y="Bike Rentals")

bikes %>%
  ggplot(mapping=aes(x=.$'Washington DC Date',y=.$'Bike Rentals'))+
  geom_point(mapping=aes(color=bikes$'Adjusted Temperature'))+
  labs(x="Washington DC Date", y="Bike Rentals",col="Adjusted Temperature")

bikes %>%
  ggplot(mapping=aes(x=.$'Washington DC Date',y=.$'Bike Rentals'))+
  geom_point(mapping=aes(color=bikes$Humidity))+
  labs(x="Washington DC Date", y="Bike Rentals",col="Humidity")
  
bikes %>%
  ggplot(mapping=aes(x=.$'Washington DC Date',y=.$'Bike Rentals'))+
  geom_point(mapping=aes(color=bikes$'Windspeed'))+
  labs(x="Washington DC Date", y="Bike Rentals",col="Wind Speed")

bikes_long <- bikes %>%
  select(`Bike Rentals`,`Washington DC Date`,Temperature,`Adjusted Temperature`,Humidity,Windspeed)
bikes_long <- bikes_long %>%
  pivot_longer(cols=Temperature:Windspeed,names_to="Weather Conditions",values_to="Measurements")

bikes_long %>%
  ggplot(mapping = aes(x=`Washington DC Date`,y=`Bike Rentals`))+
  geom_point(mapping=aes(color = Measurements))+
  facet_wrap(~ `Weather Conditions`)

bikes_2011 <- bikes %>%
  filter(yr == 2011)
bikes_2012 <- bikes %>%
  filter(yr == 2012)

bikes_2011 %>%
  ggplot(mapping=aes(x=.$'Washington DC Date',y=.$'Bike Rentals'))+
  geom_point(mapping=aes(color=Temperature))+
  labs(x="Washington DC Date", y="Bike Rentals")

bikes_2012 %>%
  ggplot(mapping=aes(x=.$'Washington DC Date',y=.$'Bike Rentals'))+
  geom_point(mapping=aes(color=Temperature))+
  labs(x="Washington DC Date", y="Bike Rentals")

bikes_2011_mean_temp <- bikes_2011%>%
  group_by(mnth)%>%
  summarise("Mean Temp By Month" = mean(Temperature,na.rm = TRUE))
bikes_2011_correlation <-bikes_2011 %>%
  summarise("Correlation Coeffient"=cor(`Bike Rentals`,Temperature))

bikes_2012_mean_temp <- bikes_2012%>%
  group_by(mnth)%>%
  summarise("Mean Temp By Month" = mean(Temperature,na.rm = TRUE))
bikes_2012_correlation <-bikes_2012 %>%
  summarise("Correlation Coeffient"=cor(`Bike Rentals`,Temperature))

weekdays <- bikes %>%
  select(`Bike Rentals`,workingday)
weekdays <- weekdays %>%
  pivot_longer(cols=workingday,values_to="Value")

weekdays %>% 
  ggplot() +
  geom_col(mapping = aes(x=Value,y=`Bike Rentals`,fill=Value))

season <- bikes %>%
  select(`Bike Rentals`,season)
season %>% 
  ggplot() +
  geom_col(mapping = aes(x=season,y=`Bike Rentals`,fill=season))

registered <- bikes %>%
  select(`Bike Rentals`,registered,nonregistered,mnth)
registered <- registered %>%
  group_by(mnth) %>%
  summarise(reg = sum(registered),nonreg = sum(nonregistered))

registered <- registered %>%
  pivot_longer(cols=reg:nonreg,names_to="membership",values_to="values")
registered %>% 
  ggplot() +
  geom_col(mapping = aes(x=membership,y=values,fill=membership)) +
  facet_wrap(~ mnth)
