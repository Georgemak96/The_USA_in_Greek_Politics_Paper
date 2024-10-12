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



speeches_two = read_csv("Parliamentary_Speeches_2005-2014.csv")







library(tidytext)

# Split the speech variable into sentences
speeches_two <- speeches_two %>%
  group_by(member_name, sitting_date, parliamentary_period, parliamentary_session, parliamentary_sitting, political_party, government, member_region, roles, member_gender) %>% 
  # Group by other variables if needed
  #group_by(speaker, date, political_party) %>%
  # Split speech into sentences
  mutate(sentence_id = row_number()) %>%
  unnest_tokens(sentence, speech, token = "sentences")




####remove tones and us detection
speeches_two$sentence = chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", speeches_two$sentence)
usa_speeches = speeches_two %>% mutate(USA=str_detect(sentence,"ΗΠΑ |Η\\.Π\\.Α\\. |ηπα |η\\.π\\.α\\. |ατλαντικη|Ατλαντικη| αμερικ| Αμερικ| νατο |ΝΑΤΟ|Ν\\.Α\\.Τ\\.Ο\\.|Ηνωμενες|ηνωμενες|Ηνωμενων π|ηνωμενων π")) %>% filter(USA==T)


not_usa_speeches = speeches_two%>% anti_join(usa_speeches,by=c("sentence","member_name"))
usa_speeches_whole = usa_speeches %>%bind_rows(not_usa_speeches)

####SALIENCE
political_parties= read_excel("political_parties_speeches.xlsx")
political_parties=political_parties %>% mutate(new=if_else(Ideology%in%c("Center to Center-Left","Center-Right to Right"),"Center",if_else(Ideology%in%c("Left","Right"),"Other",NA)))


usa_speeches_whole$USA = if_else(is.na(usa_speeches_whole$USA),FALSE,usa_speeches_whole$USA)
usa_speeches_whole = usa_speeches_whole %>%ungroup() %>%mutate(date=str_extract(sitting_date,"....$")) 









####salience by date

usa_speeches_whole %>% group_by(date) %>% summarise(salience = mean(USA)) %>% ggplot(aes(as.numeric(date),salience))+
  geom_smooth()

#usa_speeches_whole %>% group_by(date) %>% summarise(salience = mean(USA)) %>% export("salience_overall_2005-2014.xlsx")






###salience by party
date_sentences = usa_speeches_whole %>% group_by(date) %>% summarise(n_sentences = n())

usa_speeches_whole %>%
  ungroup() %>% 
  group_by(political_party,date) %>% count(USA) %>% filter(!is.na(political_party)) %>% left_join(date_sentences) %>% ungroup() %>% 
  mutate(salience = (n/n_sentences)*100) %>% filter(USA==T) %>% select(-c(USA,n)) #%>% export("Party_Salience_2005-2014.xlsx")






####salience by party family
date_sentences = usa_speeches_whole  %>% group_by(date) %>% count(USA) %>% filter(USA==TRUE)



####export
usa_speeches_whole %>%left_join(political_parties,by="political_party") %>% ungroup() %>% 
  
  group_by(new,date) %>% count(USA) %>% filter(!is.na(new)) %>% left_join(date_sentences,by="date") %>% ungroup() %>% 
  mutate(salience = (n.x/n.y)*100) %>% filter(USA.x==T) #%>% export("Ideology_salience_2005-2014.xlsx")




