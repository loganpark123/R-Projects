library(reshape2)
library(tidyverse)
ff <- french_fries
ff <- ff %>%
  pivot_longer(potato:painty, names_to = "category", values_to = "amount")
ff <- ff %>%
  group_by(category) %>%
  summarise(mean = mean(amount, na.rm=TRUE))
ff %>%
  ggplot() +
  coord_flip()+
  geom_col(mapping=aes(x = reorder(category, mean),y=mean,fill=category))+
  guides(fill=FALSE) +
  theme(plot.title = element_text(hjust=0.5),axis.title.y = element_blank())+
  labs(y="Average Rating", title = "French Fries") +
  scale_fill_manual(values=c("purple","red","green","yellow","blue"))
  
  
