library(tidyverse)
library(rvest)
library(robotstxt)

paths_allowed("https://www.metacritic.com/")

page <- read_html("https://www.metacritic.com/browse/games/score/metascore/year/all/")
page %>% write_csv("data/page.csv")
xml_structure(page)
title <- page %>% html_nodes(".title h3") %>%  html_text()
title <- title[-1]
  
url <- page %>% html_nodes(".title") %>% html_attr("href") %>%
  na.omit()

#How did you modify title such that it only has 100 observations (if you had 100 to begin with then state this).
#I made title equal to title without the first element becaue the first
#element was a newline char

#What tag do you want that has the href to the website?
#I just used the .title href because that was the first solution i tried 
#before reading the image alternative.

url <- paste("https://www.metacritic.com", url, sep="")
# Is your data clean (including listing an absolute url https://www.metatcritic.com/ … )? If not, what base-R function allows you to concatenate
#using paste, i concatenated the main url to the beginning of each string in the vector

#Did you get 100 observations?
metaScore <- page %>% html_nodes(".large") %>%  html_text()
metaScore <- metaScore[seq(1,298,3)]
#You can also use this which I realized later on 
metaScore <- page %>% html_nodes(".clamp-metascore .positive") %>%  html_text()
metaScore <- as.numeric(metaScore)
#I didnt get the correct number of observations, but the correct data 
#was there in the position 1,4,7... so i just deleted the unnecessary stuff

userScore <- page %>% html_nodes(".user") %>%  html_text()
userScore <- as.numeric(userScore)
#Did you get 100 observations?
#Yes. It was fairly simple once I realized where the user score was
oneUrl <- url[1]

count <-1
genre <- c()
for(oneURL in url){
print(oneURL)
onePage <- read_html(oneURL)
gameGenres <- onePage %>% html_nodes(".product_genre") %>%  html_text()
genre <- c(genre, gameGenres)
count <- count + 1
}

#Did you successfully grab just one value for genre? You should have. What is the value for gameGenres (include the output as a comment in your script)?
#Yes, i successfully grabbed one string it includes Genre(s) in the beggining so i may remove that
videoGames_NAs <- tibble(title,url,metaScore,userScore,genre)
videoGames_NAs %>% write_csv("data/metaScores_NAs_2020.csv")
#Which games and which corresponding variables have NAs.
#The user variable for spelunky 2 had a tbd which became na after as.numeric

videoGames <- videoGames_NAs %>%
  na.omit() %>%
  write_csv("data/metaScores_2020.csv")

videoGames %>% 
  ggplot() +
  geom_point(mapping = aes(metaScore,userScore),color="blue")

r <- cor(videoGames$metaScore,videoGames$userScore)
#The r = 0.1998685
#This correlation does supprise me. I would have guessed that it would be 
#around 80 percent. 

