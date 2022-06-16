#Logan Parker
library(tidyverse)
cats <- read_csv('cat-lovers-v2.csv')

cats <- cats %>%
  mutate(handedness = case_when(handedness == "l" ~ "left",handedness == "r" ~ "right",handedness == "a" ~ "ambidextrous"))
view(cats)

cats <- cats %>%
  mutate(number_of_cats = case_when(number_of_cats == "three" ~ "3",
                                    number_of_cats == "1.5 - honestly I think one of my cats is half human" ~ "2",
                                    TRUE ~ number_of_cats))%>%
  mutate(number_of_cats = strtoi(cats$number_of_cats))
mean = mean(cats$number_of_cats)
#Now, what is the mean number of cats owned?
#the mean number of cats owned is .81666

#How many respondents have below the mean number of cats?
below_mean <- cats %>%
  filter(number_of_cats<mean)
  
#33 people have below the mean number of cats.      