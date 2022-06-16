# Logan Parker
# Fall 2020 CSCI 444
# Homework 3
# Breweries in the US
library(tidyverse)
library(rvest)
library(robotstxt)
library(tidytext)
paths_allowed("https://www.ratebeer.com/")

page <- read_html("https://www.ratebeer.com/breweries/mississippi/24/213/")
page %>% xml_structure()

name <- page %>% html_nodes("#brewerTable a:nth-child(1)") %>%  html_text()
city_open <- page %>% html_nodes(".filter") %>%  html_text()
city_open <- paste(city_open, "opened", sep="_")
city_closed <- page %>% html_nodes("#brewerTable span") %>%  html_text()
city_closed <- paste(city_closed, "closed", sep="_")
city <- c(city_open,city_closed)
type <- page %>% html_nodes("td.hidden-sm") %>%  html_text()
beerCount <- page %>% html_nodes(".hidden-sm+ td") %>%  html_text()
established <- page %>% html_nodes("td:nth-child(5)") %>%  html_text()
url <- page %>% html_nodes("#brewerTable a:nth-child(1)") %>%  html_attr("href")
url <- paste("https://www.ratebeer.com", url, sep="")

msBreweries <- tibble(name,city,type,beerCount = as.numeric(str_trim(beerCount)),established = as.numeric(established),url)  
msBreweries %>% write_csv("data/msBreweries.csv")
msBreweries <- read_csv("data/msBreweries.csv")

msBreweries <- msBreweries %>%
  separate(city, c("city","status"),"_", extra = "merge")

msBreweries %>%
  count(type)%>%
  ggplot()+
  geom_bar(mapping= aes(x=reorder(type,n),y=n,fill=type),stat = "identity")+
  coord_flip()+
  labs(x = element_blank(), y = element_blank(), title="Mississippi Breweries",fill = "Brewery")
  
#Which type of brewery is the most popular in Mississippi? 
#microbreweries appear to be the most popular

msBreweries %>%
  ggplot()+
  geom_bar(mapping= aes(x=established,fill=type))+
  labs(x = "Year Established", y = "Number", title="Mississippi Breweries by Year",fill = "Brewery")
#Looking at the years 2005-2020, which type of brewery has the most openings (i.e., established)?
#Regardless of type of brewery, which year did the most breweries open?
#Microbreweries have had the most openings between 2005 and 2020
#2013 was the year with the most openings of breweries

msBreweries %>%
  ggplot()+
  geom_bar(mapping= aes(x=established,fill=type),width = .9)+
  labs(x = "Year Established", y = "Number", title="Mississippi Breweries by Year",fill = "Brewery")+
  facet_wrap(~status)
#Which type of breweries have not fared so well in Mississippi (i.e., most number that have closed by type)?
#brewpubs have had the most closures in mississippi.

#Why is Wyoming 51 and not 50? What is the additional entry’s name and what is its number?
#Wyoming is 51 because washington dc is included at /48/

states <- read_csv("data/states.csv")
states <- states %>%
  pull(name)
states[51] <- "Washington DC"
states <- sort(states)
states_lower <- tolower(states)

allnames <- c()
allcities <- c()
alltypes <- c()
allbeerCounts <- c()
allestablished <- c()
count <- 1
for(s in states_lower){
  url <- paste("https://www.ratebeer.com/breweries",s,count,"213/", sep = "/")
  print(url)
  page <- read_html(url)
  
  name <- page %>% html_nodes("#brewerTable a:nth-child(1)") %>%  html_text()
  city_open <- page %>% html_nodes(".filter") %>%  html_text()
  city_open <- paste(city_open, states[count], "opened", sep="_")
  city_closed <- page %>% html_nodes("#brewerTable span") %>%  html_text()
  city_closed <- paste(city_closed, states[count], "closed", sep="_")
  city <- c(city_open,city_closed)
  type <- page %>% html_nodes("td.hidden-sm") %>%  html_text()
  beerCount <- page %>% html_nodes(".hidden-sm+ td") %>%  html_text()
  established <- page %>% html_nodes("td:nth-child(5)") %>%  html_text()
  
  allnames <- c(allnames, name)
  allcities <- c(allcities, city)
  alltypes <- c(alltypes, type)
  allbeerCounts <- c(allbeerCounts, beerCount)
  allestablished <- c(allestablished, established)
  count <- count +1
}

usBreweries <- tibble(name = allnames,city = allcities, type = alltypes,beerCounts = as.numeric(str_trim(allbeerCounts)),established = as.numeric(allestablished))

usBreweries %>% write_csv("data/usBreweries.csv")

usBreweries <- read_csv("data/usBreweries.csv")

usBreweries <- usBreweries %>%
  separate(city, c("city","state", "status"),"_", extra = "merge")

usBreweries %>%
  ggplot()+
  geom_bar(mapping= aes(x=type,fill=type),width = .9)+
  labs(x = "Brewery Type", y = "Number", title="US Breweries by Type",fill = "Brewery")+
  facet_wrap(~status)
#Which type of brewery has had the most success? Which type of brewery
#has been hardest hit by closures?
#Microbreweries have had the most success, and they have also had the most closures

usBreweries %>%
  group_by(type) %>%
  count(established)%>%
  ggplot(mapping = aes(x=established,y=n,color=type))+
  geom_line()+
  geom_point()+
  labs(x = "Brewery Type", y = "Year", title="US Establishment of Breweries",fill = "Brewery")

#At what year would you say the number of breweries started
#to increase in the US (This year should be before 2000)? 
#The number began to increase around 1990 

usBreweries %>%
  group_by(type) %>%
  count(established)%>%
  filter(established >=2000)%>%
  ggplot(mapping = aes(x=established,y=n,color=type))+
  geom_line()+
  geom_point()+
  labs(x = "Brewery Type", y = "Year", title="US Establishment of Breweries",fill = "Brewery")

#Which type of brewery has had the most success? What is the peak of its growth?
#why do you think this peak growth has occurred (not just for
#this type of brewery, but all breweries)?
#Microbreweries again had the most success. The peak of its growth was around 2016-2017.
#The article leads me to believe the market was saturated for small breweries and the fad was wearing off.

usBreweries %>%
  filter(established == 2017)%>% 
  count(status)
#What are these numbers and how do they
#compare to the brewbound article? Do you think the numbers are close enough?
#The number of breweries established in 2017 is 907 in my data. 
#The number of breweries closed that were establihsed in 2017 is 47
#I think these numbers are close enough, but I cant know if these breweries acutally closed in 2017 or after

usBreweries %>%
  group_by(type) %>%
  count(state) %>%
  ungroup %>%
  mutate(order = reorder_within(state, n, type)) %>%
  ggplot(mapping = aes(fill=state))+
  geom_bar(mapping= aes(x=order,y=n),stat = "identity",width = 0.9)+
  scale_x_reordered() +
  coord_flip()+
  labs(x = element_blank(), y = element_blank(), title="Number of Breweries by State")+
  facet_wrap(~type,scales = "free")+
  guides(fill = FALSE)

#Which type of brewery
#DOES NOT have California at the top of the list (i.e., the most breweries)? What state is
#at the top of this list? Why do you think this is the case? What type of brewery is only
#found in four states – name the four states? What is the definition of this type of
#brewery? Of the remaining six types of breweries, which states have the least number of
#breweries (six answers)?
#contract brewers does not have california at the top. Indiana is at the top of the list, but im not sure why. (maybe they do some IPA haha)
#Comissioner is only in california, indiana, wisconsin, and connecticut
#the definition from ratebeer for comissioner is meant to capture things like supermarket chains that commission in some cases multiple beers from multiple brewers without a lot of input into recipe details
#the states with the lowest count for each type is
#brewpub - Mississippi
#Brewpub/Brewery - alabama 
#client Brewer - Hawaii
#commercial Brewery - Louisiana
#Contract Brewer - Arizona
#Microbrewery - washington DC