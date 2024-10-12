library(tidyverse)
library(readxl)
library(rio)
setwd("")


####cleaning
library(lda)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tm)
library(quanteda.dictionaries)
library(quanteda.sentiment)










####parties
#speeches_one %>% count(political_party) %>% export("political_parties_one.xlsx")
#speeches_two %>% count(political_party) %>% export("political_parties_two.xlsx")
#speeches_three %>% count(political_party) %>% export("political_parties_three.xlsx")

#political_parties_one = read_excel("political_parties_one.xlsx")
#political_parties_two = read_excel("political_parties_two.xlsx")
#political_parties_three = read_excel("political_parties_three.xlsx")


#political_parties = bind_rows(political_parties_one,political_parties_two,political_parties_three)
#export(political_parties,"political_parties_speeches.xlsx")














###party sentiments
parties_scores_one = read_csv("parties_sentiments_1989-2004.csv")
parties_scores_two = read_csv("parties_sentiments_2005-2014.csv")
parties_scores_three = read_csv("parties_sentiments_2015-2020.csv")
parties_scores_four = read_csv("parties_sentiments_2021-2024.csv")

parties_scores = bind_rows(parties_scores_one,parties_scores_two,parties_scores_three,parties_scores_four)

#parties_scores %>% left_join(political_parties) %>%export("regression.xlsx")



###Please insert the name of the party you want to see in the line below (all greek, all lowercase and full name)
###in the case of pasok/kinal please delete the filter in line 67
parties_scores %>% filter(political_party %in% c("συνασπισμος ριζοσπαστικης αριστερας")) %>%
  #mutate(political_party=if_else(political_party=="κινημα αλλαγης","πανελληνιο σοσιαλιστικο κινημα",political_party)) %>%  
  ggplot(aes(date,difference_prop))+
  geom_smooth()+
  geom_hline(yintercept = 0,color="black")+
  ylab("Americanism")+
  xlab("Date")+
  theme(text=element_text(size = 12))










####Ideology Sentiments
parties_scores_one = read_csv("ideology_sentiments_1989-2004.csv")
parties_scores_two = read_csv("ideology_sentiments_2005-2014.csv")
parties_scores_three = read_csv("ideology_sentiments_2015-2020.csv")
parties_scores_four = read_csv("ideology_sentiments_2021-2024.csv")

parties_scores= bind_rows(parties_scores_one,parties_scores_two,parties_scores_three,parties_scores_four)


parties_scores %>%  filter(!Ideology %in% c("Outsider","Independent"),!is.na(Ideology))%>% mutate(Ideology=if_else(Ideology=="Center-Right to Right","Center-Right",
                                                                                                                   if_else(Ideology=="Center to Center-Left","Center-Left",
                                                                                                                           if_else(Ideology=="Left","Far-Left",
                                                                                                                                   if_else(Ideology=="Right","Far-Right",Ideology))))) %>%
  
  ggplot(aes(as.numeric(date),difference_prop,color=Ideology))+
  geom_smooth()+
  geom_hline(yintercept = 0,color="black",size=2)+
  xlab("Year")+
  ylab("Americanism")+
  theme(text=element_text(size=13),axis.title = element_text(size=13))








###overall sentiment
sentiments_one = read_excel("1989-2004_sentiments.xlsx")
sentiments_two = read_excel("2005-2014_sentiments.xlsx")
sentiments_three = read_excel("2015-2020_sentiments.xlsx")
sentiments_four = read_excel("2021-2024_sentiments.xlsx")

sentiments_combined = bind_rows(sentiments_one,sentiments_two,sentiments_three,sentiments_four)


sentiments_combined %>% group_by(date) %>% summarise(average_score=mean(weighted_mean)) %>% filter(!is.na(date)) %>% 
  ggplot(aes(x=date,y=average_score,group=1))+
  geom_line()+
  geom_hline(yintercept = 0, color = "red")


sentiments_combined %>% group_by(date) %>% summarise(average_score=mean(weighted_mean)) %>% filter(!is.na(date)) %>% 
  ggplot(aes(x=date,y=average_score,group=1))+
  geom_smooth()+
  geom_hline(yintercept = 0, color = "red")+
  ylab("Americanism")+
  xlab("Year")+
  theme(text=element_text(size=10.5),axis.title = element_text(size=13))






