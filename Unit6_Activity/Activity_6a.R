#Logan Parker
library(tidyverse)
library(reshape2)
ff <- as_tibble(french_fries) 

select_vars <- c("potato", "buttery","grassy","rancid","painty")
ff <- ff %>%
  arrange(time,subject) %>%
  mutate(mean= rowMeans(select(.,select_vars),na.rm = TRUE))
view(ff)


ff <-ff %>% 
  group_by(time, treatment, subject) %>%
  summarise(.,potatoAVG = mean(potato,na.rm = TRUE),
            butteryAVG = mean(buttery,na.rm = TRUE),
            grassyAVG = mean(grassy,na.rm = TRUE),
            rancidAVG = mean(rancid,na.rm = TRUE),
            paintyAVG = mean(painty,na.rm = TRUE))
view(ff)
