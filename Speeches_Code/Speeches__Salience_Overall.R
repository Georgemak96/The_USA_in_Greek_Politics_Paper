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






####SALIENCE
political_parties= read_excel("political_parties_speeches.xlsx")
political_parties=political_parties %>% mutate(new=if_else(Ideology%in%c("Center to Center-Left","Center-Right to Right"),"Center",if_else(Ideology%in%c("Left","Right"),"Other",NA)))





####salience by date
salience_overall_one = read_excel("salience_overall_1989-2004.xlsx")
salience_overall_two = read_excel("salience_overall_2005-2014.xlsx")
salience_overall_three = read_excel("salience_overall_2015-2020.xlsx")
salience_overall_four= read_excel("salience_overall_2021-2024.xlsx")


salience_overall = bind_rows(salience_overall_one,salience_overall_two,salience_overall_three,salience_overall_four)
salience_overall %>% ggplot(aes(as.numeric(date),salience))+
  geom_smooth()





###salience by party
party_salience_one = read_excel("Party_Salience_1989-2004.xlsx")
party_salience_two = read_excel("Party_Salience_2005-2014.xlsx")
party_salience_three = read_excel("Party_Salience_2015-2020.xlsx")
party_salience_four = read_excel("Party_Salience_2021-2024.xlsx")

party_salience = bind_rows(party_salience_one,party_salience_two,party_salience_three,party_salience_four)

party_salience %>% ggplot(aes(as.numeric(date),salience))+
  geom_smooth()+
  facet_wrap(~political_party)

#party_salience %>% export("Party_Salience.xlsx")




####salience by party family
salience_one = read_excel("Ideology_salience_1989-2004.xlsx")
salience_two = read_excel("Ideology_salience_2005-2014.xlsx")
salience_three = read_excel("Ideology_salience_2015-2020.xlsx")
salience_four = read_excel("Ideology_salience_2021-2024.xlsx")
salience = bind_rows(salience_one,salience_two,salience_three,salience_four)

salience %>% mutate(Ideology=if_else(new=="Center","Center-Left and Center-Right Parties",
                                     if_else(new=="Other", "Far-Left and Far-Right Parties",new))) %>%  ggplot(aes(as.numeric(date),salience,color=Ideology))+
  geom_smooth()+
  xlab("Year")+
  ylab("Proportion of US mentions (%)")+
  theme(text=element_text(size=13),axis.title = element_text(size=13))


