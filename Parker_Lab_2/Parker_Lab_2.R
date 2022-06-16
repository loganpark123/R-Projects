library(tidyverse)
install.packages("fivethirtyeight")
library(fivethirtyeight)

grads <- college_recent_grads
View(grads)

grads <- grads %>%
  arrange(unemployment_rate) %>%
  select(rank, major, unemployment_rate)

grads_v1 <- grads %>%
  arrange(unemployment_rate) %>%
  select(rank, major, unemployment_rate) %>%
  mutate(unemployment_rate = round(unemployment_rate, digits = 3))

grads_v2 <- grads %>%
  arrange(unemployment_rate) %>%
  select(rank, major, unemployment_rate) %>%
  mutate(unemployment_rate = sprintf("%0.3f%%", unemployment_rate * 100))

grads_v3 <- grads %>%
  arrange(unemployment_rate) %>%
  select(rank, major, unemployment_rate)
options(digits = 3)

grads_v3 %>%
  head(10)
grads_v3 %>%
  tail(10)
#As a comment in your code,
#list ONLY the names of the top 10 majors with the lowest unemployment rate as well as only the
#names of the bottom 10 (do not include rank or rate). Do any of the top 10 or bottom 10 majors
#surprise you – enter your response as a comment in your code.
#lowest unemployment rates: mathmatics and computer science, military technologies, botany, soil science, educational administration and supervision, Engineering Mechanics Physics And Science, Court Reporting, Mathematics Teacher Education,Petroleum Engineering, General Agriculture
#highest unemployment rates: Nuclear Engineering,Public Administration,Computer Networking And Telecommunication,Clinical Psychology, Public Policy, Communication Technologies, Mining And Mineral Engineering, Computer Programming And Data Processing,Geography,Architecture
#i am super shocked by botany having 0 unemployment and also by nuclear engineers being so unemployed
high_women_grads <- college_recent_grads %>%
  arrange(desc(sharewomen)) %>%
  select(rank, major, sharewomen) %>%
  rename(prop_women=sharewomen)
  
low_women_grads <- college_recent_grads %>%
  arrange(sharewomen) %>%
  select(rank, major, sharewomen) %>%
  rename(prop_women=sharewomen)

high_women_grads %>%
  head(10)
low_women_grads %>%
  head(10)
# Now, as a comment in your code, list ONLY the names of the top 10 and only the
#names of the bottom 10 majors (do not include any other variables – you could pipe your data to
#a select function that only chooses the name…just a thought). In addition, add a comment to
#answer the question why you think this is?

#highest number of women
#Early Childhood Education                 
#Communication Disorders Sciences And Services      
#Medical Assisting Services                 
#Elementary Education                       
#Family And Consumer Sciences               
#Special Needs Education                    
#Human Services And Community Organization      
#Social Work                                
#Nursing                                    
#Miscellaneous Health Medical Profess

#Lowest number of women
#1 Military Technologies                      
#2 Mechanical Engineering Related Technologies
#3 Construction Services                      
#4 Mining And Mineral Engineering             
#5 Naval Architecture And Marine Engineering  
#6 Mechanical Engineering                     
#7 Petroleum Engineering                      
#8 Transportation Sciences And Technologies   
#9 Forestry                                   
#10 Aerospace Engineering 
#The jobs that have no women are more dangerous and require alot of manual labor and the 
#jobs that have majority women are jobs that take care of other people and children. Women are better at 
#comforting and cargiving usually.


#Why is median typically used over mean when describing the typical income of a group of people? 
#median gives a better representation when there are extreme outliers
college_recent_grads %>%
  ggplot(mapping = aes(x = median, fill = major_category)) +
  geom_histogram(binwidth = 5000)
#what did you choose? I chose 5000^

college_recent_grads %>%
  summarize(min = min(median), max = max(median),
            mean = mean(median), med = median(median),
            sd = sd(median),
            q1 = quantile(median, probs = 0.25),
            q3 = quantile(median, probs = 0.75))
#Based on your histogram distribution, which statistic is most helpful, mean or median (which
#is closer to the center of your data based on quartile 1 and quartile 3)? 
#median is closer to the center of the data

college_recent_grads %>%
  ggplot(mapping = aes(x = median, fill = major_category)) +
  geom_histogram() +
  facet_wrap( ~ major_category, ncol = 4)
# which major category do you think has the highest median income? Which has the lowest median income?
#engineering has the highest median income
college_recent_grads %>%
  group_by(major_category) %>%
  summarise(max = max(median)) %>%
  arrange(desc(max))

college_recent_grads %>%
  group_by(major_category) %>%
  summarise(min = min(median)) %>%
  arrange(min)

women_grads_median <- college_recent_grads %>%
  select(major,major_category,sharewomen,median) %>%
  rename(prop_women=sharewomen) %>%
  na.omit
#what is the linear relationship (correlation) between the proportion of women (x) and the median income (y)? 

women_grads_median %>%
  summarize(r = cor(prop_women, median))

NA_values <- women_grads_median %>%
  filter(is.na(prop_women))
color_names <- c("darkolivegreen","springgreen","tomato", "yellow","violet","peachpuff","orchid","orange","hotpink1","deeppink4","coral","burlywood","red","aquamarine1","chocolate","darkorchid")
#colors should be defined in ggplot
women_grads_median %>%
  ggplot(mapping=aes(x=prop_women,y=median, color=major_category))+
  geom_point()+ 
  scale_color_manual(values=color_names)
#what trend do you see between median income and the proportion of women by major category? does this supprise you?
#this scatterplot seems to indicate that women tend do have jobs that are under 40000 dollars. This is not very surprising seeing the categories that women tend towards above.

