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

















###SENTIMENT ANALYSIS 










library(tidytext)

# Split the speech variable into sentences
speeches_one <- speeches_one %>%
  # Group by other variables if needed
  group_by(member_name, sitting_date, parliamentary_period, parliamentary_session, parliamentary_sitting, political_party, government, member_region, roles, member_gender) %>% 
  # Group by other variables if needed
  #group_by(speaker,date,political_party) %>%
  # Split speech into sentences
  mutate(sentence_id = row_number()) %>%
  unnest_tokens(sentence, speech, token = "sentences")



####remove tones and us detection
speeches_one$sentence = chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", speeches_one$sentence)
usa_speeches = speeches_one %>% mutate(USA=str_detect(sentence,"ΗΠΑ |Η\\.Π\\.Α\\. |ηπα |η\\.π\\.α\\. |ατλαντικη|Ατλαντικη| αμερικ| Αμερικ| νατο |ΝΑΤΟ|Ν\\.Α\\.Τ\\.Ο\\.|Ηνωμενες|ηνωμενες|Ηνωμενων π|ηνωμενων π")) %>% filter(USA==T)



###text cleaning of  speeches for sentiment analysis
stopwords_mine= stopwords::stopwords(language = "el", source = "misc")
stopwords_mine= chartr("ή ά ό ύ ώ έ ί ϊ ΐ ϋ", "η α ο υ ω ε ι ι ι υ", stopwords_mine)
corpus= corpus(usa_speeches$sentence, docvars = usa_speeches)
manifesto_corpus<- corpus %>% tokens(remove_punct = T,remove_numbers = T,include_docvars = T)  %>% tokens_remove(c(stopwords_mine)) %>%
  tokens_wordstem(language = "greek")  %>% tokens_ngrams(n=1:6) %>%
  dfm(tolower=T) %>% dfm_trim(min_docfreq = 2) %>% dfm_weight(scheme = "prop")

####sentiment analysis 
dict = dictionary(list(neg=c("αμερικανικ_οικονομικ_κυριαρχ","αμερικαν_συμφεροντ","χωρ_αμερικανονατοικ_ιμπεριαλιστικ","ιμπεριαλιστικ_συμμαχι","σχεδ_νατ","κερδοφορι_κεφαλαι","εξοπλιστ_σχεδιασμ","στρατιωτικοποιησ","αστ_ταξ","εκβιαστ","αμερικανοπνευστ","ιμπεριαλιστικ_οργαν","συμφεροντ_αστ_ταξ","αστικ_ταξ","ιμπεριαλιστικ_σχεδιασμ","σχεδιασμ_νατ_ευρωπαικ_ενωσ","απεξαρτησ_ρωσ_φυσικ_αερι","βαρ_κυριαρχ_δικαιωμ_χωρ","αποδεσμευσ_νατ_ευρωπαικ_ενω","φιλ_αμερικαν","αμερικαν_συμφεροντ","αντιγραψτ_αμερικαν_μοντελ","χρυσ_αυγ","ξεν_δυναμ","αμερικανικ_πολυεθνικ","γη_υδωρ","παγκοσμι_πολεμ","φονιαδ λα","αμερικαν_σχεδιασμ","ορεξη_αμερικαν","φιλοσ_αμερικαν","φιλοι_αμερικαν","παρακολουθημ","ιμπεριαλιστ_οργαν","προωθησ συμφεροντ ηπ","χατιρ","αγαπημενοσ_φιλ","φιλοσ_σ","φιλοσ_των_αμερικαν","θεραπαινιδ","μιζ","νεοφιλελευθερ","αμερικανοκρατουμεν","αμερικανοκινητ","ξενο_παραγοντ","εξοπλιστ_προγραμμ","στρατιωτικ_επεμβασ","επεμβασ_αμερικαν","χωρ_ιμπεριαλιστικ_οργαν","αμερικαν_ευρωπαικ_ιμπεριαλ","παραμον_αμερικαν_βασ","αμερικανικ_βασ","απαγορευσ_πυρην_δοκιμ","νε_ταξ_πραγμ","επεμβασ","στρατηγικ_σχεδιασμ_νατ","ενοπλ_δυναμ","στρατηγικ_σχεδιασμ","συνεργο","εξαρτω","παραβ","εξαρτ","εξαρτησ","φιλοιμπεριαλ","βομβαρδιστ","μονοπωλ","αμερικανονατοικ","ιμπεριαλ","ιμπεριαλιστικ","ολιγαρχ"),
                       pos = c("συνεργασι_κυβερνησ_ελληνικ_δημοκρατι_κυβερνησ","συμφωνι_αμοιβαι_αμυντικ","συνεργασι_ηνωμεν_πολιτει","αμυντικ_συνεργασι_ηνωμεν","αμερικ_παραρτημ_δυ_επιστολ","πολιτει_αμερικ_βελτιωσ_διεθν","εθνικ_αμυν_ελληνικ_δημοκρατι","παρατασ_μνημον_συνεργασι","συνεργασι_κυβερνησ_ελληνικ_δημοκρατι_κυβερνησ","κυρωσ_μνημον_συνεννοησ","συμμαχ_δυναμ_ευρωπ","φιλοξενουσ_χωρ_επιχειρησ","συμφωνι_συναντιληψ_κυβερνησ","υποστηριξ_φιλοξενουσ_χωρ","επιχειρησ_αν_συμμαχ","συμμαχ_δυναμ_ευρωπ","ηνωμεν_πολιτει_ευρωπ","ηγεσι_συμμαχικ_διοικησ","ομοσπονδιακ_υπουργει_αμυν_ομοσπονδιακ_δημοκρατι","συμβασ_αμοιβαι_δικαστικ","επιστημονικ_τεχνολογικ_συνεργασι","λειτουργ_σταθμ_αναμεταδοσ", "συνεργασι_κυβερνησ_ελληνικ_δημοκρατι_ηνωμεν_πολιτει","αμοιβαι_διοικητικ_συνδρομ","συμφεροντ_χωρ","αφοσιωσ_διατηρησ_ειρην_δεσμευσ","αμοιβαι_αμυντικ_συνεργασι","βελτιωσ","διμερ","θετικ","εμπιστοσυν","συνεργασι","σεβασμ","θεσμ","σταθεροτητ","προωθησ","αρμονικ","δεσμ","αμυν","φιλικ","αμοιβαι","οφελ","ευχαρ","παραδοσιακ")))
sentiments = manifesto_corpus %>% dfm_lookup(dict)

sentiments <- convert(sentiments,to="data.frame")
sentiments$positive<- if_else((sentiments$pos - sentiments$neg)>0,1,0)
sentiments$negative<- if_else((sentiments$pos - sentiments$neg)<0,1,0)
sentiments$neutral<- if_else((sentiments$pos - sentiments$neg)==0,1,0)



#####joining sentiments with original dataset
usa_speeches= usa_speeches %>% bind_cols(sentiments)



usa_speeches %>% ungroup() %>% mutate(date=str_extract(sitting_date,"^....")) %>% group_by(date) %>% summarise(prop_pos=mean(positive,na.rm=T),prop_neg=mean(negative,na.rm=T),
                                                                                                      difference_prop=prop_pos-prop_neg)
















not_usa_speeches = speeches_one%>% anti_join(usa_speeches,by=c("sentence","member_name"))
usa_speeches_whole = usa_speeches %>%bind_rows(not_usa_speeches)

usa_speeches_whole$positive = if_else(is.na(usa_speeches_whole$positive),0,usa_speeches_whole$positive)
usa_speeches_whole$negative = if_else(is.na(usa_speeches_whole$negative),0,usa_speeches_whole$negative)
usa_speeches_whole$neutral = if_else(is.na(usa_speeches_whole$neutral),0,usa_speeches_whole$neutral)










####calculating sitting date scores
weights_for_parties = usa_speeches_whole %>% ungroup() %>%mutate(date=str_extract(sitting_date,"^...."))%>%
  group_by(political_party,date) %>% summarise(n_sentences = n()) %>% ungroup() %>% group_by(date) %>%
  mutate(weights=n_sentences/sum(n_sentences))

parties_scores = usa_speeches_whole %>% ungroup() %>%mutate(date=str_extract(sitting_date,"^....")) %>%  
  group_by(political_party,date) %>% summarise(prop_pos=mean(positive,na.rm=T),prop_neg=mean(negative,na.rm=T),
                                               difference_prop=prop_pos-prop_neg) %>% left_join(weights_for_parties)

#write_csv(parties_scores,"parties_sentiments_1989-2004.csv")
date_scores_weighted = parties_scores %>% ungroup() %>% group_by(date) %>% summarise(weighted_mean = weighted.mean(difference_prop,weights))



#export(date_scores_weighted,"1989-2004_sentiments.xlsx")




####scores by Ideology

political_parties= read_excel("political_parties_speeches.xlsx")
parties_scores = usa_speeches_whole %>% ungroup() %>%mutate(date=str_extract(sitting_date,"^....")) %>%  left_join(political_parties) %>% 
  group_by(Ideology) %>% summarise(prop_pos=mean(positive,na.rm=T),prop_neg=mean(negative,na.rm=T),
                                   difference_prop=prop_pos-prop_neg) 



####scores by Ideology through time 
parties_scores = usa_speeches_whole %>% ungroup() %>%mutate(date=str_extract(sitting_date,"^....")) %>%  left_join(political_parties) %>% 
  group_by(Ideology,date) %>% summarise(prop_pos=mean(positive,na.rm=T),prop_neg=mean(negative,na.rm=T),
                                        difference_prop=prop_pos-prop_neg) 



#write_csv(parties_scores,"ideology_sentiments_1989-2004.csv")







###Here you can read a sample of the classified texts. Just insert the name of the party you want (greek, lowercase, full name) and the date (four digits)
neg_texts = usa_speeches_whole %>%ungroup() %>% left_join(political_parties) %>%   mutate(true=str_detect(sitting_date,"2021")) %>% filter(true==T,political_party=="συνασπισμος ριζοσπαστικης αριστερας") %>% arrange(desc(neg)) %>% slice(1:100)
pos_texts = usa_speeches_whole %>%ungroup() %>% left_join(political_parties) %>%   mutate(true=str_detect(sitting_date,"2021")) %>% filter(true==T,political_party=="συνασπισμος ριζοσπαστικης αριστερας") %>% arrange(desc(pos)) %>% slice(1:100)





####Collocations to discover word for the lexicon of pro and anti american terms
### Insert the name of the party you want in the line below (lowercase, greek, full name)
usa_speeches_party = usa_speeches %>% filter(political_party=="σπαρτιατες")
corpus_party = corpus(usa_speeches_party$sentence,docvars=usa_speeches_party)

manifesto_corpus_colloc<- corpus_pasok %>% tokens(remove_punct = T,remove_numbers = T,include_docvars = T,padding = T)  %>% tokens_remove(c(stopwords_mine)) %>%
  tokens_wordstem(language = "greek")

###here is the collocation, you can fix the size of it with the size argument below
colloc = textstat_collocations(manifesto_corpus_colloc,size=4,min_count = 2)
colloc %>% arrange(desc(count)) %>%filter(count>=3)




