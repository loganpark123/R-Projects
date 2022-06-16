#Logan Parker, csci 444, lab 1

library(tidyverse)
library(datasauRus)

str(datasaurus_dozen)

#How many variables are in the data frame and what are their datatypes? How many observations?
#There are 3 variables and 1846 observations. The data types of the variables are chr, dbl, dbl
#tibble [1,846 x 3] (S3: tbl_df/tbl/data.frame)
#$ dataset: chr [1:1846] "dino" "dino" "dino" "dino" ...
#$ x      : num [1:1846] 55.4 51.5 46.2 42.8 40.8 ...
#$ y      : num [1:1846] 97.2 96 94.5 91.4 88.3 ...
#- attr(*, "spec")=
 # .. cols(
  #  ..   dataset = col_character(),
   # ..   x = col_double(),
    #..   y = col_double()
    #.. )

unique(datasaurus_dozen$dataset)
#How many datasets are there?
#There are 13 datasets.
# [1] "dino"       "away"       "h_lines"    "v_lines"   
# [5] "x_shape"    "star"       "high_lines" "dots"      
# [9] "circle"     "bullseye"   "slant_up"   "slant_down"
# [13] "wide_lines"

datasaurus_dozen %>% count(dataset)

dino_data <- datasaurus_dozen %>%
  filter(dataset == "dino")

dino_data %>%
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point(color = "tomato1")

dino_data %>%
  summarize(r = cor(x, y))
# # A tibble: 1 x 1
# r
# <dbl>
#   1 -0.0645

dino_data <- datasaurus_dozen %>%
  filter(dataset == "star")

dino_data %>%
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point(color = "lightyellow")

dino_data %>%
  summarize(r = cor(x, y))

# A tibble: 1 x 1
# r
# <dbl>
#   1 -0.0630

# How does this value compare to the r of dino
#These values are very similar.

dino_data <- datasaurus_dozen %>%
  filter(dataset == "circle")

dino_data %>%
  ggplot(mapping = aes(x = x, y = y)) +
  geom_point(color = "blue")

dino_data %>%
  summarize(r = cor(x, y))

# # A tibble: 1 x 1
# r
# <dbl>
#   1 -0.0683
# How does this value compare to the r of dino & star
# The r value is again very similar but it is the largest decimal.

datasaurus_dozen %>%
  ggplot(aes(x = x, y = y, color = dataset)) +
  geom_point() +
  facet_wrap(~ dataset, ncol = 3)
