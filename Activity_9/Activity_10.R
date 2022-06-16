library(tidyverse)
library(maps)
library(socviz)
library(cowplot)
diabetes <- read_csv("data/diabetes2013.csv", skip = 1)

diabetes <- diabetes %>%
  rename(region = State, id = `FIPS Codes`) %>%
  mutate(region = tolower(region), percent = as.numeric(percent, ), number = as.numeric(number))%>%
  na.omit()

left_join(county_map,diabetes)
  county_map %>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = group)) +
  geom_polygon(color = gray)+
  coord_equal()+
  theme_map()

county_map
