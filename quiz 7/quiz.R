library(tidyverse)
install.packages("rvest")
library(rvest)
install.packages("stringr")
library(stringr)
library(readr)
cats <- read_csv("data/cats.csv")
cats_wide <- cats %>%
  pivot_wider(names_from = handedness, values_from = name)
myWord <- "antidisestablishment"
myWord %>% str_replace_all("[at]","*")

page <- read_html("https://en.wikipedia.org/wiki/List_of_largest_cats")
name <- page %>% html_nodes("th+ td a") %>%  html_text()

myWord %>% str_extract_all("[aeiou]")

myWord %>% str_remove_all("s")
catst <- read_delim("data/cat-lovers.txt",delim="*")
html_nodes("td+ td > a")
name <- page %>%  html_nodes("td:nth-child(9) a") %>%  html_attr("href") 
name <- page %>%  html_nodes("td+ td > a") %>%  html_attr("href") 
