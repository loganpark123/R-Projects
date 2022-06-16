library(tidyverse)
library(leaflet)
library(lubridate)
library(usmap)
library(cowplot)
library(geojsonio)
library(htmltools)
library(rvest)
library(gridExtra)
calls <- read_csv("data/911.csv")
calls <- calls %>%
  filter(year(timeStamp)==2017,month(timeStamp)==1)

crime <- read_csv("data/crime_and_incarceration_by_state.csv")
crime <- crime %>%
  rename("state" = jurisdiction) %>%
  mutate(state = tolower(state))


state_map <- us_map("states") %>%
  rename(state = full) %>%
  mutate(state = tolower(state))

prisoner_map <- crime %>%
  filter(year == 2016, state != "federal") %>%
  group_by(state,prisoner_count,state_population)%>%
  summarise(crimePop = prisoner_count/state_population)%>%
  left_join(crime_map,.,on = "state")

prisoner_map %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = prisoner_count))+
  geom_polygon(color = "gray") +
  coord_equal() +
  theme_map()+
  scale_fill_distiller(palette = "YlOrRd",direction = 1)+
  labs(title = "Number of Prisoners by State 2016",fill = "Prisoner Count")

prisoner_map %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = crimePop))+
  geom_polygon(color = "gray") +
  coord_equal() +
  theme_map()+
  scale_fill_distiller(palette = "YlOrRd",direction = 1)+
  labs(title = "Prisoners vs Population 2016",fill = element_blank())
crime16 <- crime %>%
  filter(year == 2016, state != "federal")
  select(state,state_population,violent_crime_total)
  
cor(crime16$state_population,crime16$violent_crime_total)
crime16%>%
  ggplot(mapping = aes(state_population,violent_crime_total))+
  geom_point()+
  labs(title = paste("Coorelation of population and Violent Crime:",cor(crime16$state_population,crime16$violent_crime_total)))

violent_map <- crime %>%
  filter(year == 2016, state != "federal") %>%
  group_by(state,state_population,violent_crime_total)%>%
  summarise(violentPop = violent_crime_total/state_population)%>%
  left_join(state_map,.,on = "state")

violent_map %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = violent_crime_total))+
  geom_polygon(color = "gray") +
  coord_equal() +
  theme_map()+
  scale_fill_distiller(palette = "YlOrRd",direction = 1)+
  labs(title = "Violent Crime by State 2016",fill = "Violent Crimes")

violent_map %>%
  ggplot(mapping = aes(x = x,
                       y = y,
                       group = group,
                       fill = violentPop))+
  geom_polygon(color = "gray") +
  coord_equal() +
  theme_map()+
  scale_fill_distiller(palette = "YlOrRd",direction = 1)+
  labs(title = "Violent Crimes vs Population 2016",fill = element_blank())

prisoners16 <- crime %>%
  filter(year == 2016, state != "federal") %>%
  mutate(prisonPop = prisoner_count/state_population)

prisoners16 %>%
  ggplot(mapping = aes(x=reorder(state,prisoner_count),y=prisoner_count, fill=state))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Number of Prisoners by State in 2016",x=element_blank(),y="Prisoner Count")

prisoners16 %>%
  ggplot(mapping = aes(x=reorder(state,violent_crime_total),y=violent_crime_total, fill=state))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Number of Violent crimes by State in 2016",x=element_blank(),y="Violent Crimes")+
  guides(fill = FALSE)

states <- geojsonio::geojson_read("data/us-states.json",what = "sp")
bins <- c(0, 1000, 10000, 30000, 50000, 75000, 100000,150000, Inf)
pal <- colorBin("YlOrRd", domain = prisoners16$prisoner_count, bins = bins)
labels <- sprintf(
  "<strong>%s</strong><br/>%d total prisoners<br />Total population %d",
  prisoners16$state, prisoners16$prisoner_count, prisoners16$state_population
) %>% lapply(htmltools::HTML)

leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = "pk.eyJ1IjoibG9nYW5wYXJrMTIzIiwiYSI6ImNraGk3aDczeDA3YmwyeHM4bG44bG81cTAifQ.Y_t7mMxNVcc6TMuy5tfccw"))%>%
  addPolygons(
    fillColor = ~pal(prisoners16$prisoner_count),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
    ) %>%
  addLegend(pal = pal, values = ~prisoner_map$prisoner_count, opacity = 0.7, title = NULL,
            position = "bottomright")

crime %>%
  ggplot(mapping = aes(x=reorder(state,prisoner_count),y=prisoner_count, fill=state))+
  geom_bar(stat = "identity")+
  coord_flip()+
  labs(title = "Number of Prisoners by State in 2016",x=element_blank(),y="Prisoner Count")+
  guides(fill=FALSE)+
  facet_wrap(~year)

#covidMonths <- c("March", "April", "May", "June", "July", "August", "September")
index <- 3

state_map <- us_map("states",include = c("Florida"))%>%
  rename(state = full) %>%
  mutate(state = tolower(state))
temp <- crime %>%
  filter(state=="florida")
temp
pal <- colorBin("YlOrRd", domain = temp$prisoner_count)
for(index in 2001:2016){
  temp <- crime %>%
    filter(year == index, state=="florida")
  
  temp <- temp%>%
    group_by(state)
  
  temp_map <-left_join(state_map, temp, by = "state")
  
  #centroid <- aggregate(data = temp_map, cbind(x, y)~sum, FUN=mean)
  
  temp_plot <- temp_map %>%
    ggplot(mapping = aes(x = x,
                         y = y,
                         group = group,
                         fill = pal(prisoner_count))) +
    geom_polygon(color = "gray") +
    coord_equal() +
    theme_map()+
    labs(title = index,fill="Total Prisoners")+
    theme(plot.title = element_text(hjust = 0.5))
    # geom_text(data = centroid, 
    #           mapping = aes(x = x,
    #                         y = y,
    #                         label = sum), 
    #           color = "black", size = 2, inherit.aes = FALSE)+
    #scale_fill_distiller(palette = "YlOrRd", direction = 1)
  #index <- index+1
  assign(paste("plot",index, sep = ""), temp_plot)
}
grid.arrange(plot2001, plot2002, plot2003, plot2004, plot2005, plot2006, plot2007, plot2008,
             plot2009, plot2010, plot2011, plot2012, plot2013, plot2014, plot2015, plot2016)
library(tidytext)

prison <- read_csv("data/prison_custody_by_state.csv")
ucr <- read_csv("data/ucr_by_state.csv")
okmap <- us_map("counties", include = c("Oklahoma"))
okmap <- okmap %>%
  mutate(county = paste(county,"county",sep = " "))

agencies <- read_csv("data/agencies.csv")
agencies <- agencies %>%
  rename(county = PUB_AGENCY_NAME)

left_join(okmap,agencies,by = "county") %>%
  ggplot(mapping = aes(x,
                       y,
                       group = group,
                       fill=))+
  geom_polygon()+
  coord_equal()+
  theme_map()
