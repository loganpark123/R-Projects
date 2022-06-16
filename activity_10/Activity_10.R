library(tidyverse)
library(maps)
library(socviz)
library(cowplot)
library(mapproj)
diabetes <- read_csv("data/diabetes2013.csv", skip = 1)

diabetes <- diabetes %>%
  rename(region = State, id = `FIPS Codes`) %>%
  mutate(region = tolower(region), percent = as.numeric(percent), number = as.numeric(number))%>%
  na.omit()

joined <- left_join(county_map,diabetes, by = "id")

joined %>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = id,
                       fill = percent)) +
  geom_polygon(color = "gray")+
  coord_equal()+
  scale_fill_gradient2(low = "beige", high = "red")+
  theme_map()+
  theme(legend.position = "bottom")

us_by_state <- diabetes %>%
  mutate(id = str_sub(id,1,2))%>%
  group_by(id, region) %>%
  summarise(obesityMean = mean(percent))

us_states <- map_data("state")

us_states_v2 <- left_join(us_states,us_by_state,by="region") 
centroid <- aggregate(data = us_states_v2,cbind(long,lat) ~obesityMean,FUN=mean)

us_states_v2 %>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = group,
                       fill = obesityMean)) +
  geom_polygon(color = "gray")+
  geom_text(data= centroid,
            mapping = aes(x=long,
                          y= lat,
                          label = format(round(obesityMean, 1), nsmall = 1)),
            color="black",size=4,inherit.aes = FALSE)+
  coord_map(projection = "albers",
            lat0 = 39,
            lat1 = 45)+
  scale_fill_gradient2(low = "beige", high = "red")+
  theme_map()+
  theme(legend.position = "bottom")
  

