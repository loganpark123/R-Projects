# Logan Parker
# Fall 2020 CSCI 444
# Homework 4
# COVID-19 cases in the US

library(tidyverse)
library(usmap)
library(cowplot)
library(lubridate)
library(RColorBrewer)
library(gridExtra)

us_states <- us_map("state")
us_counties <- us_map("county")
covid_states <- read_csv("data/us_states_covid19_daily.csv")
covid_counties <- read_csv("data/us_counties_covid19_daily.csv")

covid_states <- covid_states %>%
  rename(abbr = "state")%>%
  mutate(date = ymd(date))%>%
  filter(!(abbr %in% c("AS","GU","MP","PR","VI")))

abbr <- distinct(covid_states, `abbr`)

covid_states_September <-covid_states %>%
  filter(month(date) == 9)

covid_states_September <- covid_states_September%>%
  group_by(abbr)%>%
  mutate("sum" = sum(positiveIncrease))
  

sept_map <-left_join(us_states, covid_states_September, by = "abbr")

centroid <- aggregate(data = sept_map, cbind(x, y)~sum, FUN=mean)

sept_map %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = sum)) +
  geom_polygon(color = "gray") +
  coord_equal() +
  theme_map()+
  labs(title = "September COVID-19 Cases", fill = "Raw Total")+
  geom_text(data = centroid, 
            mapping = aes(x = x,
                          y = y,
                          label = sum), 
            color = "black", size = 4, inherit.aes = FALSE)+
  scale_fill_distiller(palette = "Set1")

covid_states_September %>%
  distinct(abbr,sum) %>%
  arrange(sum)%>%
  head(10)

covid_states_September %>%
  distinct(abbr,sum) %>%
  arrange(desc(sum)) %>%
  head(10)

us_pop <- read_csv("data/USPopulation2019.csv")
us_pop <- us_pop %>%
  rename(abbr = state)
covid_state_perCapita <- left_join(sept_map,us_pop,by="abbr")
covid_state_perCapita <- covid_state_perCapita %>%
  mutate(perCapita = sum/pop)
  
covid_state_perCapita %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = perCapita)) +
  geom_polygon(color = "gray") +
  labs(title = "September COVID-19 Cases per capita", fill = "Per Capita")+
  coord_equal() +
  theme_map()+
  scale_fill_distiller(palette = "Set1")

covid_state_perCapita %>%
  distinct(abbr,perCapita) %>%
  arrange(perCapita)%>%
  head(10)

covid_state_perCapita %>%
  distinct(abbr,perCapita) %>%
  arrange(desc(perCapita)) %>%
  head(10)

covid_state_perCapita %>%
  distinct(abbr,perCapita, sum) %>%
  arrange(sum,perCapita)%>%
  head(10)

covid_state_perCapita %>%
  distinct(abbr,perCapita, sum) %>%
  arrange(desc(sum),desc(perCapita)) %>%
  head(10)

covidMonths <- c("March", "April", "May", "June", "July", "August", "September")
index <- 3
for(month in covidMonths){
  temp <- covid_states %>%
    filter(month(date) == index)
  
  temp <- temp%>%
    group_by(abbr)%>%
    mutate("sum" = sum(positiveIncrease))
  
  
  temp_map <-left_join(us_states, temp, by = "abbr")
  
  centroid <- aggregate(data = temp_map, cbind(x, y)~sum, FUN=mean)
  
temp_plot <- temp_map %>%
    ggplot(mapping = aes(x = x,
                         y = y,
                         group = group,
                         fill = sum)) +
    geom_polygon(color = "gray") +
    coord_equal() +
    theme_map()+
    labs(title = month,fill="Raw Total")+
    theme(plot.title = element_text(hjust = 0.5))+
    geom_text(data = centroid, 
              mapping = aes(x = x,
                            y = y,
                            label = sum), 
              color = "black", size = 2, inherit.aes = FALSE)+
    scale_fill_distiller(palette = "Set1")
index <- index+1
assign(month, temp_plot)
}
grid.arrange(March, April, May, June, July, August, September)



index <- 3
for(month in covidMonths){
  temp <- covid_states %>%
    filter(month(date) == index)
  
  temp <- temp%>%
    group_by(abbr)%>%
    mutate("sum" = sum(positiveIncrease))
  
  
  temp_map <-left_join(us_states, temp, by = "abbr")
  
  temp_perCapita <- left_join(temp_map,us_pop,by="abbr")
  temp_perCapita <- temp_perCapita %>%
    mutate(perCapita = sum/pop)
  temp_plot <- temp_perCapita %>%
    ggplot(mapping = aes(x = x,
                         y = y,
                         group = group,
                         fill = perCapita)) +
    geom_polygon(color = "gray") +
    coord_equal() +
    theme_map()+
    labs(title = paste(month, "COVID-19 Cases"),fill="Per Capita")+
    theme(plot.title = element_text(hjust = 0.5))+
    scale_fill_distiller(palette = "Set1")
  index <- index+1
  assign(month, temp_plot)
}
grid.arrange(March, April, May, June, July, August, September)

us_counties <- us_counties %>%
  mutate(fips = as.numeric(fips))%>%
  filter(abbr=="MS")
covid_counties <- covid_counties %>%
  mutate(date = mdy(date))
covid_counties_ms <- covid_counties %>%
  filter(state == "Mississippi")

covid_counties_ms_march <- covid_counties_ms %>%
  filter(month(date)==3, day(date)==31)
covid_counties_ms_sept <- covid_counties_ms %>%
  filter(month(date)==9, day(date)==26)


ms_march <- left_join(us_counties,covid_counties_ms_march,by="fips") %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = cases)) +
  geom_polygon(color = "gray") +
  coord_equal() +
  theme_map()+
  labs(title = "Mississippi March COVID-19 Cases",fill="Raw Total")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_distiller(palette = "Set1")

ms_sept <- left_join(us_counties,covid_counties_ms_sept,by="fips") %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = cases)) +
  geom_polygon(color = "gray") +
  coord_equal() +
  theme_map()+
  labs(title = "Mississippi September COVID-19 Cases",fill="Raw Total")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_distiller(palette = "Set1")

grid.arrange(ms_march,ms_sept)

covid_counties_ms_march %>%
  arrange(desc(cases)) %>%
  head(5)

covid_counties_ms_sept %>%
  arrange(desc(cases)) %>%
  head(5)














