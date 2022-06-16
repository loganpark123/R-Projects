library(tidyverse)
drugs <- read_delim("data/drugdeaths.txt",delim="^")

drugs <- drugs %>%
  select(Sex,Age,Heroin:Tramad)

drugs_longer <- drugs %>%
  pivot_longer(Heroin:Tramad,names_to = "Type",values_to = "Result") %>%
  na.omit()

drugs_longer <- drugs_longer %>%
  group_by(Type) %>%
  count(Type)

drugs_longer %>%
  ggplot() +
  geom_col(mapping = aes(x=Type,y=n,fill=Type))
