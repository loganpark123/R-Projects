# Logan Parker
# Fall 2020 CSCI 444
# Homework 1
# Myers-Briggs Personality Test
library(tidyverse)
names <- c("Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9", "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19", "Q20", "Q21", "Q22", "Q23", "Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31", "Q32", "Q33", "Q34", "Q35", "Q36", "Q37", "Q38", "Q39", "Q40", "Q41", "Q42", "Q43", "Q44", "Q45", "Q46", "Q47", "Q48", "Q49", "Q50", "Q51", "Q52", "Q53", "Q54", "Q55", "Q56", "Q57", "Q58", "Q59", "Q60", "Q61", "Q62", "Q63", "Q64", "Q65", "Q66", "Q67", "Q68", "Q69", "Q70", "EI", "SN", "TF", "JP", "Personality_Type" )
library(purrr)

data_dir <- dir("data",pattern="*.csv")

data<-data_dir %>% 
  map_dfr(~read_csv(file.path("data", .),
                    col_names = FALSE
                    )) %>%
  mutate(X73 = case_when(X73=="TRUE" ~ "T", TRUE ~"F"))
data <- na.omit(data)
view(data)
glimpse(data)
colnames(data)<-names
data %>%
  ggplot(aes(x=Personality_Type, fill=Personality_Type))+
  geom_bar()

responses <- data %>%
  pivot_longer(Q1:Q70, names_to="question", values_to="answer") %>%
  select(question, answer) %>%
  group_by(question, answer) %>%
  add_count(answer) %>%
  distinct() %>%
  arrange(question)

responses %>%
  ggplot(aes(x=answer,y=n,fill=answer)) +
  geom_bar(stat="identity")+
  facet_wrap(~question,ncol = 7) +
  ggtitle("Myer's Briggs Results", subtitle = "Logan Parker")

responses_alt <- data %>%
  pivot_longer(Q1:Q70, names_to="question", values_to="answer") %>%
  select(question, answer) %>%
  group_by(question, answer) %>%
  add_count(answer) %>%
  distinct() 
responses_alt <-responses_alt%>%
  mutate(rank = as.numeric(str_sub(question,2))) %>%
  arrange(rank,answer)

responses_alt %>%
  ggplot(aes(x=answer,y=n,fill=answer)) +
  geom_bar(stat="identity")+
  facet_wrap(~rank,ncol = 7)

minority <- responses_alt %>%
  filter(n <= min(.$n)+1)
