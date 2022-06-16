library(tidyverse)
library(sjmisc)

dn <- read_csv("data/dennys.csv")
lq <- read_csv("data/laquinta.csv")
states <- read_csv("data/states.csv")

#What are the dimensions (Variables and Observations) of the Denny’s
#dataset? What does each row in the dataset represent? What are the variables?
glimpse(dn)
#there are 6 variables and 1643 observations

#What are the dimensions (Variables and Observations) of the La Quinta
#dataset? What does each row in the dataset represent? What are the variables?
glimpse(lq)
#There are 6 variables and 909 observations

dn_us <- dn %>%
  filter(state %in% states$abbreviation)
dn_not_us <- dn %>%
  filter(state %nin% states$abbreviation)

#How many Denny’s locations are inside and outside the US?
#In dn, there are 1643 dennys in the US and 0 outside

lq_us <- lq %>%
  filter(state %in% states$abbreviation)
lq_not_us <- lq %>%
  filter(state %nin% states$abbreviation)
#How many La Quinta locations are inside and outside the US?
#There are 895 La Quintas in the us and 14 outside the us

dn_us_count <- dn_us %>%
  group_by(state) %>%
  add_count(state) %>%
  arrange(desc(n))

lq_us_count <- lq_us %>%
  group_by(state) %>%
  add_count(state) %>%
  arrange(desc(n))
#Which 10 states have the most Denny’s locations (call your data frame
#dn_us_count – you will need to use dn_us to determine this)? What about La
#Quinta (call your data frame lq_us_count – similarly, you will need to use lq_us to
#determine this)? Is this surprising? Why or why not?
#the most dennys are in CA,TX,FL,AZ,IL,NY,WA,OH,MO,PA
#The most La Quintas are in TX,FL,CA,GA,TN,OK,LA,NM,NY,AZ
#This is not very surprising because these are all populated and tourist driven states

lq_us <- lq_us %>%
  mutate(establishment = "La Quinta")
dn_us <- dn_us %>%
  mutate(establishment = "Denny's")

dn_lq <- bind_rows(dn_us, lq_us)

dn_lq %>%
  ggplot()+
  geom_point(mapping=aes(x=longitude,y=latitude, color=establishment), alpha=.25)

#What is the apparent trend of Denny’s locations to La Quinta locations, on a
#national level?
  #The two establishments are generally grouped together. This may be due to seeking high population places

california <- dn_lq %>%
  filter(state %in% c("CA"))

texas <- dn_lq %>%
  filter(state %in% c("TX"))

oregon <- dn_lq %>%
  filter(state %in% c("OR"))

kentucky <- dn_lq %>%
  filter(state %in% c("KY"))

library(maps)
state_data <- map_data(map = "state")

state_data %>%
  filter(region %in% c("california")) %>%
  ggplot() +
  geom_polygon(fill="white", color="gray", mapping=aes(x=long,y=lat, group=group)) +
  coord_quickmap()+ 

  geom_point(data = california, mapping = aes(x=longitude,
                                           y=latitude,
                                           color=establishment))
  
state_data %>%
  filter(region %in% c("texas")) %>%
  ggplot() +
  geom_polygon(fill="white", color="gray", mapping=aes(x=long,y=lat, group=group)) +
  coord_quickmap()+ 
  
  geom_point(data = texas, mapping = aes(x=longitude,
                                              y=latitude,
                                              color=establishment))
state_data %>%
  filter(region %in% c("oregon")) %>%
  ggplot() +
  geom_polygon(fill="white", color="gray", mapping=aes(x=long,y=lat, group=group)) +
  coord_quickmap()+ 
  
  geom_point(data = oregon, mapping = aes(x=longitude,
                                              y=latitude,
                                              color=establishment))
state_data %>%
  filter(region %in% c("kentucky")) %>%
  ggplot() +
  geom_polygon(fill="white", color="gray", mapping=aes(x=long,y=lat, group=group)) +
  coord_quickmap()+ 
  
  geom_point(data = kentucky, mapping = aes(x=longitude,
                                              y=latitude,
                                              color=establishment))

state_data %>%
  filter(region %in% c("california","texas","oregon","kentucky")) %>%
  ggplot() +
  geom_polygon(fill="white", color="gray", mapping=aes(x=long,y=lat, group=group)) +
  coord_quickmap()+ 
  
  geom_point(data = california, mapping = aes(x=longitude,
                                              y=latitude,
                                              color=establishment))+
  geom_point(data = texas, mapping = aes(x=longitude,
                                         y=latitude,
                                         color=establishment))+
  geom_point(data = oregon, mapping = aes(x=longitude,
                                          y=latitude,
                                          color=establishment))+
  geom_point(data = kentucky, mapping = aes(x=longitude,
                                            y=latitude,
                                            color=establishment))

#Do you agree with Mitch Hedberg that La Quinta is Spanish for “Next to
#Denny’s”? Explain.
#Yes I think that in the sense of the joke, it does mean next to denny's. 
#In states with similar numbers of the two establishments, the two tend to be near each other.
#they do, however, exist in several locations alone without their companion.
