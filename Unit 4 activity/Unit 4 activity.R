install.packages("nycflights13")
library(nycflights13)
library(tidyverse)
data(package="nycflights13")
#How many datasets? 5
glimpse(flights)
#what type of R object is flights? flights is a dataframe
#How many variables and observations? 19 varialbles and 336,776 observations
Jan1 <- flights[c("carrier", "flight", "origin", "dest","arr_time","dep_time")]
Jan1$total_time <- Jan1$arr_time - Jan1$dep_time
Jan1 <- Jan1[1:842,]
install.packages("devtools")
devtools::install_github("hadley/emo")
emo::ji("airplane")
