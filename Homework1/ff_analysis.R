library(purrr)
library(tidyverse)
data_dir <- dir("ff_data",pattern="*.csv")

ff<-data_dir %>% 
  map_dfr(~read_csv(file.path("ff_data", .),
                   col_names = FALSE))
names <- c("subject","potatoey","buttery","grassy","rancid","painty")
colnames(ff)<-names

ff %>%
  ggplot(aes(x=subject,y=potatoey))+
  geom_bar(stat = "identity",color="purple",fill="purple")
