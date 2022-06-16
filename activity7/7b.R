library(readr)
library(tidyverse)
library(rvest)
IMDB <- read_html("https://www.imdb.com/chart/boxoffice/")
title <- IMDB %>% html_nodes(".titleColumn a") %>%  html_text()
weekend <- IMDB %>% html_nodes(".titleColumn+ .ratingColumn") %>%  html_text()
gross <- IMDB %>% html_nodes(".titleColumn+ .ratingColumn") %>%  html_text()
weeks <- IMDB %>% html_nodes(".weeksColumn") %>%  html_text()

topGrossingFilms <- tibble(title,weekend,gross,weeks)
write_csv(topGrossingFilms, "data/topGrossingFilms.csv")
topGrossingFilms <- topGrossingFilms %>%
  mutate(gross=as.numeric(str_remove_all(gross,"[$M\\n]"))) %>%
  mutate(weeks=as.numeric(str_remove_all(weeks,"[\\n]"))) %>%
  mutate(GrossAverage = gross/weeks) %>%
  arrange(desc(GrossAverage))

         