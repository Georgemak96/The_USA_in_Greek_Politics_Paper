library(tidyverse)
library(readxl)
library(rio)
setwd("")

###reading manifestos dataset
manifestos_dataset<- read_excel("manifestos_dataset.xlsx")


###party antiamericanism per time
parties= read_excel("manifesto_parties_ideology.xlsx")
manifestos_dataset= manifestos_dataset %>% left_join(parties)

###Replacing NA's with 0
manifestos_dataset<- manifestos_dataset %>% mutate(`Framing: Anti-imperialist`= if_else(is.na(`Framing: Anti-imperialist`),0,1))


###Dividing the framing emotional rational 
manifestos_dataset= manifestos_dataset %>% mutate(`Framing: Rational`= if_else(`Framing: Rational (=2) / Emotional (=1)`==2,1,0)) %>%
  mutate(`Framing: Emotional`= if_else(`Framing: Rational (=2) / Emotional (=1)`==1,1,0)) %>% select(-`Framing: Rational (=2) / Emotional (=1)`)



###americanism by party family

weights= manifestos_dataset %>% group_by(partyname,date) %>% count(USneg) %>% mutate(sum=sum(n)) %>%ungroup() %>%distinct(partyname,date,.keep_all = T)%>%group_by(date) %>% 
  mutate(weight=sum/sum(sum))  %>% select(partyname,date,weight) %>% ungroup()
centre_left_parties= manifestos_dataset %>% filter(Ideology=="Center to Center-Left") %>% 
  group_by(date) %>% summarise(weighted_mean=mean(USpos)-mean(USneg))  %>%
  mutate(Americanism=weighted_mean*100) %>% mutate(Ideology="Center to Center-Left")

left_parties= manifestos_dataset %>%  filter(Ideology=="Left") %>% 
  group_by(date) %>% summarise(weighted_mean=mean(USpos)-mean(USneg))  %>%
  mutate(Americanism=weighted_mean*100) %>% mutate(Ideology="Left")

center_right= manifestos_dataset %>% filter(Ideology=="Center-Right to Right") %>% 
  group_by(date) %>% summarise(weighted_mean=mean(USpos)-mean(USneg))  %>%
  mutate(Americanism=weighted_mean*100) %>% mutate(Ideology="Center-Right to Right")


right_parties= manifestos_dataset %>% filter(Ideology=="Far-Right") %>% 
  group_by(date) %>% summarise(weighted_mean=mean(USpos)-mean(USneg)) %>%
  mutate(Americanism=weighted_mean*100)%>% mutate(Ideology="Far-Right")

left_parties%>% bind_rows(right_parties,center_right,centre_left_parties) %>% mutate(date=str_extract(date,"....")) %>% 
  mutate(Ideology=if_else(Ideology=="Center-Right to Right","Center-Right",if_else(Ideology=="Center to Center-Left","Center-Left",
                                                                                   if_else(Ideology=="Left","Far-Left",Ideology)))) %>% 
  ggplot(aes(x=as.numeric(date),y=Americanism,color=Ideology))+
  geom_smooth() +
  theme_minimal() +
  xlab("Year")+
  geom_hline(yintercept = 0, color = "black",size=2)+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14))




###antiamericanism by party family

centre_left_parties= manifestos_dataset %>% group_by(partyname,date) %>% summarise(USpos=mean(USpos),USneg=mean(USneg), Americanism=(USpos-USneg)) %>%
  left_join(weights) %>%  ungroup() %>% 
  left_join(parties,by="partyname") %>% filter(Ideology=="Center to Center-Left") %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USneg,w=weight))  %>%
  mutate(Anti_Americanism=weighted_mean*100) %>% mutate(Ideology="Center to Center-Left")

left_parties= manifestos_dataset %>% group_by(partyname,date) %>% summarise(USpos=mean(USpos),USneg=mean(USneg), Americanism=(USpos-USneg)) %>%
  left_join(weights) %>%  ungroup() %>% 
  left_join(parties,by="partyname") %>% filter(Ideology=="Left") %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USneg,w=weight))  %>%
  mutate(Anti_Americanism=weighted_mean*100) %>% mutate(Ideology="Left")

center_right= manifestos_dataset %>% group_by(partyname,date) %>% summarise(USpos=mean(USpos),USneg=mean(USneg), Americanism=(USpos-USneg)) %>%
  left_join(weights) %>%  ungroup() %>% 
  left_join(parties,by="partyname") %>% filter(Ideology=="Center-Right to Right") %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USneg,w=weight))  %>%
  mutate(Anti_Americanism=weighted_mean*100) %>% mutate(Ideology="Center-Right to Right")


right_parties= manifestos_dataset %>% group_by(partyname,date)  %>% summarise(USpos=mean(USpos),USneg=mean(USneg), Americanism=(USpos-USneg)) %>%
  left_join(weights)  %>%ungroup() %>% 
  left_join(parties,by="partyname") %>% filter(Ideology=="Far-Right") %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USneg,w=weight)) %>%
  mutate(Anti_Americanism=weighted_mean*100)%>% mutate(Ideology="Far-Right")

left_parties%>% bind_rows(right_parties,center_right,centre_left_parties) %>% 
  ggplot(aes(x=as.numeric(date),y=Anti_Americanism,color=as.factor(Ideology)))+
  geom_line(size=3) +
  theme_minimal()






###americanism per party
manifestos_dataset %>% group_by(partyname,date) %>% summarise(USneg=mean(USneg)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(partyname) %>% summarise(weighted_mean=weighted.mean(x=USneg,w=weight))  %>%
  mutate(Anti_Americanism=weighted_mean*100) %>% 
  ggplot(aes(x=reorder(partyname,Anti_Americanism),y=Anti_Americanism))+
  geom_col() +
  theme_minimal()+
  coord_flip()


###anti-americanism per time

manifestos_dataset %>% group_by(partyname,date) %>%summarise(USneg=mean(USneg)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USneg,w=weight)) %>%
  mutate(Anti_Americanism=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=Anti_Americanism))+
  geom_line() +
  theme_minimal()



###americanism per time
manifestos_dataset %>% group_by(partyname,date) %>% summarise(USpos=mean(USpos),USneg=mean(USneg), Americanism=(USpos-USneg)) %>% 
  left_join(weights)  %>% ungroup() %>%  group_by(date)  %>% 
  summarise(Americanism=weighted.mean(x=Americanism,w = weight)) %>% mutate(date=str_extract(date,"....")) %>% 
  ggplot(aes(x=as.numeric(date),y=Americanism))+
  geom_smooth() +
  theme_minimal()+
  xlab("Year")+
  geom_hline(yintercept = 0, color = "black",size=2)+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14))





###USpos per time 
manifestos_dataset %>% group_by(partyname,date) %>% summarise(USpos=mean(USpos)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USpos,w=weight))  %>%
  mutate(USpos=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=USpos))+
  geom_line() +
  theme_minimal()



###antiimperialism
manifestos_dataset %>% group_by(partyname,date) %>% summarise(`Framing: Anti-imperialist`=mean(`Framing: Anti-imperialist`)) %>% left_join(weights)%>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Framing: Anti-imperialist`,w=weight))  %>%
  mutate(Anti_Imperialism=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=Anti_Imperialism))+
  geom_line() +
  theme_minimal()


####anticapitalism
manifestos_dataset %>% group_by(partyname,date) %>%summarise(`Framing: Anti-capitalist`=mean(`Framing: Anti-capitalist`)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Framing: Anti-capitalist`,w=weight)) %>%
  mutate(Anti_Capitalism=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=Anti_Capitalism))+
  geom_line() +
  theme_minimal()


###USSRpos
manifestos_dataset %>% group_by(partyname,date) %>% summarise(USSRpos=mean(USSRpos)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USSRpos,w=weight))  %>%
  mutate(USSRpos=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=USSRpos))+
  geom_line() +
  theme_minimal()


###socialismpos
manifestos_dataset %>% group_by(partyname,date) %>% summarise(SocialismPos=mean(SocialismPos)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=SocialismPos,w=weight))  %>%
  mutate(SocialismPos=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=SocialismPos))+
  geom_line() +
  theme_minimal()






###socialismneg
manifestos_dataset %>% group_by(partyname,date)  %>% summarise(SocialismNeg=mean(SocialismNeg)) %>% left_join(weights) %>%ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=SocialismNeg,w=weight))  %>%
  mutate(SocialismNeg=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=SocialismNeg))+
  geom_line() +
  theme_minimal()


###USSRneg
manifestos_dataset %>% group_by(partyname,date) %>% summarise(USSRneg=mean(USSRneg)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=USSRneg,w=weight)) %>%
  mutate(USSRneg=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=USSRneg))+
  geom_line() +
  theme_minimal()




####Israel pos
manifestos_dataset %>% group_by(partyname,date) %>%summarise(`Israel/JewsPos`=mean(`Israel/JewsPos`)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Israel/JewsPos`,w=weight))  %>%
  mutate(`Israel/JewsPos`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Israel/JewsPos`))+
  geom_line() +
  theme_minimal()



####Israel neg
manifestos_dataset %>% group_by(partyname,date) %>% summarise(`Israel/JewsNeg`=mean(`Israel/JewsNeg`)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Israel/JewsNeg`,w=weight))  %>%
  mutate(`Israel/JewsNeg`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Israel/JewsNeg`))+
  geom_line() +
  theme_minimal()



####Framing Historical
manifestos_dataset %>% group_by(partyname,date) %>%summarise(`Framing: Historical/National`=mean(`Framing: Historical/National`)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Framing: Historical/National`,w=weight)) %>%
  mutate(`Framing: Historical/National`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Framing: Historical/National`))+
  geom_line() +
  theme_minimal()


####Framing Cultural
manifestos_dataset %>% group_by(partyname,date) %>% summarise(`Framing: Cultural`=mean(`Framing: Cultural`)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Framing: Cultural`,w=weight)) %>%
  mutate(`Framing: Cultural`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Framing: Cultural`))+
  geom_line() +
  theme_minimal()


####Palestine Pos 
manifestos_dataset %>% group_by(partyname,date) %>% summarise(PalestinePos=mean(PalestinePos)) %>% left_join(weights)%>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=PalestinePos,w=weight))  %>%
  mutate(PalestinePos=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=PalestinePos))+
  geom_line() +
  theme_minimal()


####Cyprus 
manifestos_dataset %>% group_by(partyname,date) %>%summarise(Cyprus=mean(Cyprus)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=Cyprus,w=weight)) %>%
  mutate(Cyprus=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=Cyprus))+
  geom_line() +
  theme_minimal()



####Greece Turkey
manifestos_dataset %>% group_by(partyname,date) %>% summarise(`Greek - Turkish relations`=mean(`Greek - Turkish relations`)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Greek - Turkish relations`,w=weight))  %>%
  mutate(`Greek - Turkish relations`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Greek - Turkish relations`))+
  geom_line() +
  theme_minimal()




###Civil War
manifestos_dataset %>% group_by(partyname,date) %>%summarise(`Civil War`=mean(`Civil War`)) %>% left_join(weights) %>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Civil War`,w=weight))  %>%
  mutate(`Civil War`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Civil War`))+
  geom_line() +
  theme_minimal()


###Junta
manifestos_dataset %>% group_by(partyname,date) %>%summarise(Junta=mean(Junta)) %>% left_join(weights)%>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=Junta,w=weight))  %>%
  mutate(Junta=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=Junta))+
  geom_line() +
  theme_minimal()


###Framing Rational
manifestos_dataset %>% group_by(partyname,date) %>% summarise(`Framing: Rational`=mean(`Framing: Rational`)) %>% left_join(weights)%>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Framing: Rational`,w=weight))  %>%
  mutate(`Framing: Rational`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Framing: Rational`))+
  geom_line() +
  theme_minimal()

###Framing Emotional
manifestos_dataset %>% group_by(partyname,date) %>% summarise(`Framing: Emotional`=mean(`Framing: Emotional`)) %>% left_join(weights)%>% ungroup() %>% 
  group_by(date) %>% summarise(weighted_mean=weighted.mean(x=`Framing: Emotional`,w=weight)) %>% mutate(date=str_extract(date,pattern = "....")) %>%
  mutate(`Framing: Emotional`=weighted_mean*100) %>% 
  ggplot(aes(x=as.numeric(date),y=`Framing: Emotional`))+
  geom_line() +
  theme_minimal()

















library(lda)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textmodels)
library(quanteda.textplots)
library(tm)





#manifestos_dataset_one<- manifestos_dataset %>% filter(USneg==1)
#manifestos_dataset_zero<- manifestos_dataset %>% filter(USneg==0)
#set.seed(1)
#sampled_zeros <- manifestos_dataset_zero[sample(nrow(manifestos_dataset_zero), 3000, replace = F), ]
#manifestos_dataset<- bind_rows(manifestos_dataset_one,sampled_zeros)


####cleaning
manifestos_dataset$text=  chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", manifestos_dataset$text)
stopwords_mine= stopwords::stopwords(language = "el", source = "misc")
stopwords_mine= chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", stopwords_mine)
corpus= corpus(manifestos_dataset$text, docvars = manifestos_dataset)
manifesto_corpus<- corpus %>% tokens(remove_punct = T,remove_numbers = T,include_docvars = T)  %>% tokens_remove(c(stopwords_mine)) %>%
  tokens_wordstem(language = "greek") 

manifesto_corpus=as.list(manifesto_corpus)
head(manifesto_corpus)




# Convert the list to a character vector
cleaned_texts <- sapply(manifesto_corpus, function(tokens) paste(tokens, collapse = " "))


# Add the cleaned text to the original dataset
manifestos_dataset$cleaned_text <- cleaned_texts




####Averaging the dataset

manifestos_dataset_new<- manifestos_dataset %>% group_by(partyname,date) %>% summarise(USneg= mean(USneg),cleaned_text=str_c(cleaned_text,collapse = " "),
                                                                                       USpos= mean(USpos)) %>% mutate(Americanism=USpos-USneg)


manifestos_dataset %>% count(USpos)
manifestos_dataset_new %>% ggplot(aes(Americanism))+
  geom_histogram()




###US SALIENCE
manifestos_dataset_us = manifestos_dataset %>% mutate(us_locate=if_else(USpos|USneg==1,1,0))


manifestos_dataset_us %>% 
  group_by(date) %>% summarise(us_salience=mean(us_locate)) %>% mutate(us_salience=us_salience*100) %>% mutate(date=str_extract(date,"....")) %>% 
  ggplot(aes(as.numeric(date),us_salience))+
  geom_smooth()+
  theme_minimal()+
  xlab("Year")+
  ylab("US Salience (%)")+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14))
  

manifestos_dataset_us %>%
  group_by(Ideology,date) %>% summarise(us_salience=mean(x=us_locate)) %>% mutate(us_salience=us_salience*100) %>% mutate(date=str_extract(date,"....")) %>%
  mutate(Ideology=if_else(Ideology=="Center-Right to Right","Center-Right",if_else(Ideology=="Center to Center-Left","Center-Left",
                                                                                   if_else(Ideology=="Left","Far-Left",Ideology)))) %>% 
  ggplot(aes(as.numeric(date),us_salience,color=Ideology))+
  geom_smooth()+
  theme_minimal()+
  xlab("Year")+
  ylab("US Salience (%)")+
  theme(text = element_text(size = 14),axis.text = element_text(size = 14))




library(ggpubr)
###correlation of salience and antiamericanism 
cor.test(manifestos_dataset_us$USneg,manifestos_dataset_us$us_locate)


