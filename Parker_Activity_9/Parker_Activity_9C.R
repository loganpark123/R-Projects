library(tidyverse)
library(RColorBrewer)
imdb <- read_csv("data/imdb_top_250.csv")
pal <- brewer.pal(9,"Set1")
imdb %>%
  ggplot() +
  geom_histogram(mapping = aes(x=year),bins=9,fill=pal)+
  scale_fill_brewer(palette = "Set1")+
  theme(panel.background = element_blank(), plot.title = element_text(hjust=0.5))+
  labs(title = "Top 250 Grossing Movies",x="Year",y="Dollars in Millions")
