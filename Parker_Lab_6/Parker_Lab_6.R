library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(leaflet)

nyc <- read_csv("data/NYC_2020listings.csv")

nyc <- nyc %>%
  rename(borough = neighbourhood_group, min_nights = minimum_nights,
         num_reviews = number_of_reviews, count = calculated_host_listings_count,
         availability = availability_365) %>%
  mutate(last_review = date(last_review)) %>%
  filter(availability != 0)

#Exercise 1

#For each type of room, which borough has the most Airbnbs? Which
#buroughs do not have hotel rooms?

#Manhattan has the most Enitre home/apt, Hotel rooms, and shared rooms.
#Brooklyn has the most Private rooms.
#There are no hotel rooms in the bronx or staten island.
nyc %>%
  ggplot(mapping=aes(x=room_type,fill = borough)) +
  geom_bar(position=position_dodge()) +
  labs(x = "Types of Rooms", y= "Number",title = "Number of Airbnbs by types of room in NYC")+
  theme(panel.background = element_blank())

#Exercise 2

#Which borough has the most Airbnbs? Which borough has the least Airbnbs?
#Manhattan has the most Airbnbs and staten island has the least.
ex2 <- nyc %>%
  group_by(borough)%>%
  summarise(n = sum(count))
ex2 %>%
  ggplot() +
  geom_bar(mapping=aes(x=reorder(borough,-n),y=n, fill=borough),
           stat = "identity",
           position=position_dodge()) +
  labs(x = "NCY Borough", y= "Number",title = "Number of Airbnbs in NYC")+
  theme(panel.background = element_blank())+
  guides(fill=FALSE)

  
#Exercise 3
#Which borough has the highest mean price? Which has the lowest?
#Manhatten has the highest mean price and the brox has the lowest.
ex3 <-nyc %>%
  group_by(borough) %>%
  summarise(avg=mean(price)) 
ex3 %>%
  ggplot() +
  geom_col(mapping = aes(x=reorder(borough,avg),y=avg,fill=borough))+
  labs(x="NYC Borough", y="Average Price",title = "Average Price by NYC Borough")+
  theme(panel.background = element_blank())+
  guides(fill=FALSE)

#Exercise 4
#Within each of the five boroughs, which neighborhood has the
#second highest cost (list cost, borough and neighborhood)? 

#Second Highest costing neighbourhood by bourough
#Manhatten - tribeca $431.57364
#Bronx - riverdale $308.71429
#Brooklyn - brooklyn heights $260
#Queens - Neponsit $275.00000
#Staten Island - Willlowbrook and tottenville are both $309.00000

#Within each of the five boroughs,which neighborhood has the second lowest 
#cost (list cost, borough and neighborhood)?

#Second Lowest costing neighbourhood by bourough
#Manhatten - Two Bridges $97.41026
#Bronx - Mount Eden $53
#Brooklyn - Bensonhurt $77.35417
#Queens - woodside $62.64591
#Staten Island - Great Kills $45

#What is the second highest average cost? What is the second lowest average cost?
#The second highest average cost is Flatiron District at $444.67500 in Manhattan
#The second lowest cost is Great Kills at $45.00000 in Staten Island
ex4 <- nyc %>%
  group_by(neighbourhood) %>%
  summarise(avg=mean(price),borough=borough)

ex4_2 <- ex4 %>%
  group_by(neighbourhood,borough)%>%
  distinct(avg)
ex4_2 %>%
  group_by(borough)%>%
  top_n(n=2,wt=avg)
ex4 %>%
  ggplot()+
  geom_bar(mapping = aes(x=reorder(neighbourhood,avg),y=avg,fill=neighbourhood),
           stat = "identity",
           position=position_dodge())+
  coord_flip()+
  labs(x="Neighborhood", y="Average Price by Neighborhood",title = "Average Price by Neighborhood")+
  theme(panel.background = element_blank())+
  facet_wrap(~borough,scales = "free")+
  guides(fill=FALSE)

#Exercise 5

ex5 <- ex4_2 %>%
  group_by(borough)%>%
  top_n(n=5,wt=avg)

ex5 %>%
  ggplot()+
  geom_bar(mapping = aes(x=reorder(neighbourhood,-avg),y=avg,fill=neighbourhood),
           stat = "identity",
           position=position_dodge())+
  labs(x="Neighborhood", y="Average Price",title = "Average Price by Neighborhood")+
  theme(panel.background = element_blank())+
  facet_wrap(~borough,scales = "free_x")+
  guides(fill=FALSE)

#Exercise 6

#Which borough has the highest number of reviews? How many?

nyc %>%
  group_by(borough)%>%
  summarise(reviews = sum(num_reviews))

#Brooklyn has the most reviews at 352856
ex6 <- nyc %>%
  filter(year(last_review) == 2020)

ex6 %>%
  ggplot()+
  geom_point(mapping=aes(x=last_review,y=num_reviews, color = borough, size = price, alpha = 0.3))+
  scale_color_brewer(palette = "Set2")+
  labs(title = "Airbnb Reviews", subtitle = "2020", x="Date of Last Review", y="Number of Reviews")+
  theme(panel.background = element_blank())
  
#Exercise 7
#Approximately (i.e., look at your plots rather than write the R code to give the exact number),
#what are the maximum number of reviews for each borough?
#The following are my approximations of the max reviews for each borough
#Bronx 350ish
#Brooklyn 530ish
#Manhattan 610ish
#Queens 690ish
#Staten Island 280ish

ex7 <- ex6

ex7 %>%
  ggplot()+
  geom_point(mapping=aes(x=last_review,y=num_reviews, color = borough, size = price, alpha = 0.3))+
  scale_color_brewer(palette = "Set3")+
  labs(title = "Airbnb Reviews", subtitle = "2020", x=element_blank(), y="Number of Reviews")+
  theme(legend.position="bottom")+
  facet_grid(cols=vars(borough))

#Exercise 8
nyc_neighborhood_top4 <- nyc %>%
  group_by(borough,neighbourhood)%>%
  summarise(average=mean(price),latitude=mean(latitude),longitude=mean(longitude))

nyc_neighborhood_top4 <- nyc_neighborhood_top4 %>%
  top_n(n=4,wt=average)
#What are the top 4 Airbnbs for each borough’s neighborhood? Copy and paste your
#output as a comment in your R script
#   Borough       Neighborhood        avg cost  latitude  longitude
# 1 Bronx         Eastchester          173.     40.9     -73.8
# 2 Bronx         Fieldston            394.     40.9     -73.9
# 3 Bronx         Riverdale            309.     40.9     -73.9
# 4 Bronx         Spuyten Duyvil       210.     40.9     -73.9
# 5 Brooklyn      Boerum Hill          227.     40.7     -74.0
# 6 Brooklyn      Brooklyn Heights     260.     40.7     -74.0
# 7 Brooklyn      Cobble Hill          225.     40.7     -74.0
# 8 Brooklyn      Sea Gate             440.     40.6     -74.0
# 9 Manhattan     Flatiron District    445.     40.7     -74.0
# 10 Manhattan     NoHo                 282.     40.7     -74.0
# 11 Manhattan     SoHo                 292.     40.7     -74.0
# 12 Manhattan     Tribeca              432.     40.7     -74.0
# 13 Queens        Arverne              261.     40.6     -73.8
# 14 Queens        Breezy Point         229.     40.6     -73.9
# 15 Queens        Briarwood            429.     40.7     -73.8
# 16 Queens        Neponsit             275      40.6     -73.9
# 17 Staten Island Fort Wadsworth       800      40.6     -74.1
# 18 Staten Island Randall Manor        203.     40.6     -74.1
# 19 Staten Island Tottenville          309      40.5     -74.2
# 20 Staten Island Willowbrook          309      40.6     -74.1


nyc_neighborhood_top4 %>%
  leaflet(options = leafletOptions(zoomSnap=0.1)) %>%
  addTiles() %>%
  setView(-74.00, 40.71, zoom = 12) %>%
  addMarkers(~longitude, ~latitude)
