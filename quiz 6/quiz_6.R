#Logan Parker

library(tidyverse)


#How do I define a vector of the just the counties? Call your variable counties and use the county variable.
counties <- pull(midwest, county)

#How do I create a data frame , WI,  only containing information from Wisconsin (WI).  ALSO, add a new variable called region that is assigned the value Wisconsin.
WI <- midwest %>%
  filter(state %in% c("WI")) %>%
  mutate(region="Wisconsin")

#How do I create a data frame, pop50, that contains the number of counties in each Midwestern state with a population greater than 50000 - i.e.,  poptotal greater than 50000?  Your answer should only have 5 observations.
pop50 <- midwest %>%
  filter(poptotal > 50000) %>%
  add_count(state) %>%
  select(state,n) %>%
  distinct()

#How do I create a data frame, avgMI, which contains the average population for Michigan(MI) only.  Call your new column avg.?
avgMI <- midwest %>%
  filter(state %in% c("MI")) 
avgMI <- avgMI %>%
  mutate(avg = mean(avgMI$poptotal))

#How do I define a data frame of the just the population? Call your data frame popDF and use the poptotal variable.
#I think this is what you mean, but i also put another answer below for a DF with the total of the populations
popDF <- midwest %>%
  select(poptotal)
  
popDF <- data.frame("population" =sum(midwest$poptotal))
