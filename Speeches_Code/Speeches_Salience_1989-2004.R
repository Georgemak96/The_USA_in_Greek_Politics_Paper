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



speeches_one = read_csv("Parliamentary_Speeches_1989-2004.csv")






###text cleaning of OCR speeches
speeches_ninety_six = read_excel("1996 Speeches.xlsx")
speeches_ninety_six$sitting_date = as.Date(speeches_ninety_six$sitting_date)
speeches_ninety_six$speech=  chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", speeches_ninety_six$speech)
stopwords_mine= stopwords::stopwords(language = "el", source = "misc")
stopwords_mine= chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", stopwords_mine)
corpus= corpus(speeches_ninety_six$speech, docvars = speeches_ninety_six)
manifesto_corpus<- corpus %>% tokens(remove_punct = T,remove_numbers = T,include_docvars = T)  %>% tokens_remove(c(stopwords_mine)) %>%
  tokens_wordstem(language = "greek") 

manifesto_corpus=as.list(manifesto_corpus)
head(manifesto_corpus)




# Convert the list to a character vector
cleaned_texts <- sapply(manifesto_corpus, function(tokens) paste(tokens, collapse = " "))


# Add the cleaned text to the original dataset
speeches_ninety_six$cleaned_text <- cleaned_texts



###joining OCR speeches with the rest of the speeches
speeches_one$sitting_date = as.Date(speeches_one$sitting_date,format = "%d/%m/%Y")
speeches_one =bind_rows(speeches_one,speeches_ninety_six)
speeches_one = speeches_one %>% arrange(sitting_date)








library(tidytext)

# Split the speech variable into sentences
speeches_one <- speeches_one %>%
  group_by(member_name, sitting_date, parliamentary_period, parliamentary_session, parliamentary_sitting, political_party, government, member_region, roles, member_gender) %>% 
  # Group by other variables if needed
  #group_by(speaker, date, political_party) %>%
  # Split speech into sentences
  mutate(sentence_id = row_number()) %>%
  unnest_tokens(sentence, speech, token = "sentences")




####remove tones and us detection
speeches_one$sentence = chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", speeches_one$sentence)
usa_speeches = speeches_one %>% mutate(USA=str_detect(sentence,"ΗΠΑ |Η\\.Π\\.Α\\. |ηπα |η\\.π\\.α\\. |ατλαντικη|Ατλαντικη| αμερικ| Αμερικ| νατο |ΝΑΤΟ|Ν\\.Α\\.Τ\\.Ο\\.|Ηνωμενες|ηνωμενες|Ηνωμενων π|ηνωμενων π")) %>% filter(USA==T)


not_usa_speeches = speeches_one%>% anti_join(usa_speeches,by=c("sentence","member_name"))
usa_speeches_whole = usa_speeches %>%bind_rows(not_usa_speeches)

####SALIENCE
political_parties= read_excel("political_parties_speeches.xlsx")
political_parties=political_parties %>% mutate(new=if_else(Ideology%in%c("Center to Center-Left","Center-Right to Right"),"Center",if_else(Ideology%in%c("Left","Right"),"Other",NA)))


usa_speeches_whole$USA = if_else(is.na(usa_speeches_whole$USA),FALSE,usa_speeches_whole$USA)
usa_speeches_whole = usa_speeches_whole %>%ungroup() %>%mutate(date=str_extract(sitting_date,"^....")) 









####salience by date

usa_speeches_whole %>% group_by(date) %>% summarise(salience = mean(USA)) %>% ggplot(aes(as.numeric(date),salience))+
  geom_smooth()

#usa_speeches_whole %>% group_by(date) %>% summarise(salience = mean(USA)) %>% export("salience_overall_1989-2004.xlsx")






###salience by party
date_sentences = usa_speeches_whole %>% group_by(date) %>% summarise(n_sentences = n())

usa_speeches_whole %>%
  ungroup() %>% 
  group_by(political_party,date) %>% count(USA) %>% filter(!is.na(political_party)) %>% left_join(date_sentences) %>% ungroup() %>% 
  mutate(salience = (n/n_sentences)*100) %>% filter(USA==T) %>% select(-c(USA,n)) #%>% export("Party_Salience_1989-2004.xlsx")






####salience by party family
date_sentences = usa_speeches_whole  %>% group_by(date) %>% count(USA) %>% filter(USA==TRUE)



####export
usa_speeches_whole %>%left_join(political_parties,by="political_party") %>% ungroup() %>% 
  
  group_by(new,date) %>% count(USA) %>% filter(!is.na(new)) %>% left_join(date_sentences,by="date") %>% ungroup() %>% 
  mutate(salience = (n.x/n.y)*100) %>% filter(USA.x==T) #%>% export("Ideology_salience_1989-2004.xlsx")




