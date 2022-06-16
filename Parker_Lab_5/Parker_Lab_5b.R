library(tidyverse)
library(rvest)

videoGames <- read_csv("data/metaScores_NAs_2020.csv")

videoGames <- videoGames %>%
  mutate(genre = str_trim(str_squish(str_sub(.$genre,11,-1))))

#What is the maximum number of commas (the
#maximum count) for the 100 video games (your code should identify the maximum)?
#Copy and paste your output as a comment in your script. 
#The max commas in any one video game is 5.
videoGames <- videoGames %>%
  mutate(commas = str_count(.$genre,","))
maxCommas <- max(videoGames$commas, na.rm = TRUE)
sum(videoGames$commas)

genres <- c("genre1","genre2","genre3","genre4","genre5","genre6")

videoGames <- videoGames %>%
  separate(genre,genres,",",extra = "merge") %>% 
  mutate(genre1 = str_trim(genre1)) %>%
  mutate(genre2 = str_trim(genre2)) %>%
  mutate(genre3 = str_trim(genre3)) %>%
  mutate(genre4 = str_trim(genre4)) %>%
  mutate(genre5 = str_trim(genre5)) %>%
  mutate(genre6 = str_trim(genre6))

videoGames <- videoGames %>%
  pivot_longer(cols=genre1:genre6,names_to = "genreNum", values_to="genre",values_drop_na=TRUE)

#How many observations in your new videoGames data
#frame (HINT: It is more than 280)? How many unique genres? ? Copy and paste your output as a
#comment in your script.
glimpse(videoGames)
#there are 289 observations now.
length(unique(videoGames$genre))
#There are 53 unique genres

#What is the most common genre?
genre_count <- videoGames %>%
  group_by(genre) %>%
  count(genre)

genre_count %>%
  ggplot(mapping = aes(x=genre,y=n,fill = genre))+
  geom_col(mapping=aes(x = reorder(genre, n),y=n))+
  coord_flip()+
  guides(fill=FALSE)
#the most common genre is Action

#What is the most popular genre based on user scores, filling by genre? 
user <- videoGames %>%
  na.omit() %>%
  select(userScore,genre) %>%
  group_by(genre) %>%
  mutate(mean = mean(userScore)) 
user <- user %>%
  select(genre,mean)
user <- unique(user)
user %>%
  ggplot(mapping = aes(x=genre,y=mean,fill=genre))+
  geom_col(mapping=aes(x = reorder(genre,mean),y=mean))+
  coord_flip()+
  guides(fill=FALSE)
#defense is the most popular genre based on user scores. 

#What is the most popular genre based on metacritic scores, filling by genre?
meta <- videoGames %>%
  na.omit() %>%
  select(metaScore,genre) %>%
  group_by(genre) %>%
  mutate(mean = mean(metaScore)) 
meta <- meta %>%
  select(genre,mean)
meta <- unique(meta)
meta %>%
  ggplot(mapping = aes(x=genre,y=mean,fill=genre))+
  geom_col(mapping=aes(x = reorder(genre,mean),y=mean))+
  coord_flip()+
  guides(fill=FALSE)
#flight is the most popular by metascore


