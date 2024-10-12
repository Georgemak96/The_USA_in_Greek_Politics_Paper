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











###Topic modelling (Preparation)
###In order to perform topic modelling for each year please insert the election date in the line below as well as change the variable name (e.g. manifestos_dataset_1989 etc.)
manifestos_dataset_1981<- manifestos_dataset %>% filter(date%in%c(200403),USneg==1)

corpus= corpus(manifestos_dataset_1981$cleaned_text, docvars=manifestos_dataset_1981)
manifesto_corpus<- corpus %>% tokens(include_docvars = T) %>%  dfm(tolower = T) 


dfm_manifestos = as.data.frame(manifesto_corpus)
dfm_manifestos = dfm_manifestos %>% na.omit()

















####Supervised topic modelling 


###search for k
library(stm)
out_c <- quanteda::convert(manifesto_corpus, to = "stm")
docs_c <- out_c$documents
vocab_c = out_c$vocab
meta = out_c$meta
K <- c(2:6)
set.seed(2)
fit <- searchK(docs_c,vocab_c, K = K, verbose = TRUE,prevalence = as.formula("~partyname+`Framing: Rational`+ `Framing: Emotional`+`Framing: Historical/National`+`Framing: Cultural`+`Framing: Anti-capitalist`+`Framing: Anti-imperialist`"), data=docvars(manifesto_corpus),init.type="Spectral")
plot.searchK(fit)

fit$results
####fitting the stm model
stm_c <- stm(manifesto_corpus,
             data = docvars(manifesto_corpus),
             K =2,
             prevalence=as.formula("~partyname+`Framing: Rational`+ `Framing: Emotional`+`Framing: Historical/National`+`Framing: Cultural`+`Framing: Anti-capitalist`+`Framing: Anti-imperialist`"),
             init.type="Spectral",
             verbose = TRUE)





###evaluating model quality
topicQuality(model=stm_c, documents=docs_c)

labelTopics(stm_c, c(1:5),n=5)
plot.STM(stm_c,type="labels",n=5)


####plot topic proportions
plot(stm_c, type = "summary",n=5)








