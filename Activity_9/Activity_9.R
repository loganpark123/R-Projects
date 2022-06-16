library(tidyverse)
library(readr)
library(readxl)
library(lubridate)
core <- read_excel("data/coreSample.xlsx")
glimpse(core)

core <- core %>%
  mutate(date = as.Date(as.character(date),format="%Y"))
glimpse(core)

core <- core %>%
  pivot_longer(cols =Li:U, names_to="element")

core %>%
  group_by(element) %>%
  ggplot() +
  geom_line(mapping=aes(x=date,y=value,group=element,color=element))

myColors<-palette("Set1")
core %>%
  filter(element %in% c("Al","Cs")) %>%
  ggplot()+
  geom_line(mapping=aes(x=date,y=value,group=element,color=element)) +
  scale_color_manual(values = myColors)
