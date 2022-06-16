library(tidyverse) 
library(maps)
library(readxl) 
library(mapproj)
library(RColorBrewer) 
library(usmap)
library(socviz) 
library(cowplot) 

organ <- read_csv("data/All_States_Transplants.csv")

us_pop <- read_csv("data/state_pop.csv") %>%
  select(state,pop)

organPop <- left_join(organ,us_pop,by="state")

us_states <- us_map(region="states")%>%
  rename(state = abbr, long = x, lat = y)


totalTransplants <- organPop %>%
  filter(age == "All") %>%
  group_by(state) %>%
  summarize(n = sum(total))

left_join(us_states,totalTransplants,by="state")%>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = group,
                       fill = n)) +
  geom_polygon(color = "gray")+
  coord_equal()+
  scale_fill_distiller(palette="Set1")+
  theme_map()+
  labs(title="US Living Organ Transplants")

#Which state/district appears to have the highest number of
#transplants? Why are some states colored gray?
#california appers to have the highest number of transplants
#some are grey because they didnt have any data for the total transplants
totalTransplants %>% 
  arrange(n) %>%
  head(5)
#the bottom 5
#state     n
#<chr> <dbl>
#1 DE     1084
#2 VT     1285
#3 NH     1455
#4 SD     1835
#5 ND     2040

totalTransplants %>% 
  arrange(desc(n)) %>%
  head(5)
#The top five
#state      n
#<chr>  <dbl>
#1 CA    140718
#2 TX     94037
#3 PA     82863
#4 NY     77470
#5 FL     69527

popTransplants <- organPop %>%
  filter(age == "All") %>%
  group_by(state) %>%
  summarize(n = sum(total)/pop) %>%
  distinct()


left_join(us_states,popTransplants,by="state")%>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = group,
                       fill = n)) +
  geom_polygon(color = "gray")+
  coord_equal()+
  scale_fill_distiller(palette="Set1")+
  theme_map()+
  labs(title="US Living Organ Transplants by Population", fill = "per capita")

popTransplants %>% 
  arrange(desc(n)) %>%
  head(5)

#Now, which state/district appears to have the highest
#number of living donors per capita?
#DC has the hightest number of living donors per capita

#What state/district has the most donors per capita? What major hospital(s)
#is/are located in this state/district?
#state       n
#<chr>   <dbl>
#1 DC    0.0126 
#2 PA    0.00357
#3 NE    0.00356
#4 MN    0.00338
#5 WI    0.00280

#MedStar Washington Hospital Center is in DC 

kidney <- organPop %>%
  filter(age == "All", organ == "Kidney") %>%
  group_by(state) %>%
  summarize(n = sum(total)/pop) %>%
  distinct()

left_join(us_states,kidney,by="state")%>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = group,
                       fill = n)) +
  geom_polygon(color = "gray")+
  coord_equal()+
  scale_fill_distiller(palette="Set1")+
  theme_map()+
  labs(title="US Kidney Transplants by Living Donors", fill = "per capita")

kindey %>% 
  arrange(desc(n)) %>%
  head(5)

#Also, identify the top 5 states, using similar code as
#instructions 2. Copy and paste your output as a comment in your script:
#state       n
#<chr>   <dbl>
#1 DC    0.0126 
#2 PA    0.00357
#3 NE    0.00356
#4 MN    0.00338
#5 WI    0.00280

kidneyByAge <- organPop %>%
  filter(age %in% c("18-34 Years","35-49 Years","50-64 Years"), organ == "Kidney")

left_join(us_states,kidneyByAge,by="state")%>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = group,
                       fill = total)) +
  geom_polygon(color = "gray")+
  coord_equal()+
  scale_fill_distiller(palette="Paired")+
  theme_map()+
  labs(title="US Living Donor Kidney Transplants by Age", fill = "Transplants") +
  facet_wrap(~age)
#At a glance, besides California, which four states seem to offer living donor kidney transplants
#to all three age groups? Which states seems to serve the younger age group over the older?
#Besides california, texas, florida, Pennsylvania, and New York seem to offer to all threee
#texas, florida and california seem to serve the younger over older

organsAndAge <- organPop %>%
  filter(age %in% c("18-34 Years","35-49 Years","50-64 Years"))%>%
  arrange(organ)

left_join(us_states,organsAndAge,by="state")%>%
  ggplot(mapping = aes(x = long,
                       y = lat,
                       group = group,
                       fill = total)) +
  geom_polygon(color = "gray")+
  coord_equal()+
  scale_fill_distiller(palette="Paired")+
  theme_map()+
  labs(title="US Living Donor Organ Transplants by Age", fill = "Transplants") +
  facet_wrap(~organ+age)
# Of the four states you listed for
#question 4 that offer living donor kidney transplants to all three age groups, how do they fair
#offering living donor liver transplants?
#texas, florida, Pennsylvania, and New York do not offer nearly the same number of 
#liver transplants as they do kidney.