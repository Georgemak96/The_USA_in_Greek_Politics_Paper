library(tidyverse)
library(readxl)
library(rio)
library(lme4)
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




##### ALL THE FOLLOWING FILTER CODE WAS WRITTEN IN ORDER TO PERFORM SENTIMENT ANALYSIS AGAIN 





usa_speeches_one = read_csv("usa_speeches_pos_neg_1989-2004.csv")
usa_speeches_two= read_csv("usa_speeches_pos_neg_2005-2014.csv")
usa_speeches_three = read_csv("usa_speeches_pos_neg_2015-2020.csv")
usa_speeches_four = read_csv("usa_speeches_pos_neg_2021-2024.csv")

usa_speeches_four  = usa_speeches_four %>% rename(sitting_date=date)

usa_speeches = bind_rows(usa_speeches_one,usa_speeches_two,usa_speeches_three,usa_speeches_four)



usa_speeches %>% count(sentiment)


###party metadata
political_parties = read_excel("political_parties_speeches.xlsx")
usa_speeches = usa_speeches %>% left_join(political_parties)



###SLDA
lexicon<- lexicalize(usa_speeches$cleaned_text,lower = T)


params <- sample(c(-1, 1),4, replace = TRUE)  ## starting values
outcome<- usa_speeches$sentiment


# Check the length of documents before calling slda.em
#doc_lengths <- sapply(lexicon$documents, length)
# Filter out documents with zero length
#non_zero_doc_indices <- doc_lengths > 0
#lexicon$documents <- lexicon$documents[non_zero_doc_indices]
#outcome <- outcome[non_zero_doc_indices]

set.seed(2)
slda_model<- slda.em(lexicon$documents,K=4,vocab = lexicon$vocab, num.e.iterations = 100,num.m.iterations = 4,alpha = 1,eta=0.1,params = params, variance=var(outcome),
                     annotations = outcome,method = "sLDA")







topic_assignments = sapply(slda_model$assignments, statip::mfv1) %>% unlist()







usa_speeches = usa_speeches %>% bind_cols(topic_assignments) %>% rename(topics=...20)

usa_speeches$topics = as.factor(usa_speeches$topics)



usa_speeches$sentiment = factor(usa_speeches$sentiment, levels = c(-1,1), labels = c("0","1"))
#usa_speeches = usa_speeches %>% filter(Ideology!="Outsider")
###creating variables
usa_speeches = usa_speeches %>% mutate(date=str_extract(sitting_date,"....-.."))

usa_speeches$first_memorandum = if_else(usa_speeches$date=="2010-05",1,0)
usa_speeches$economic_crisis = if_else(usa_speeches$date=="2008-09",1,0)
usa_speeches$referendum = if_else(usa_speeches$date=="2015-07",1,0)
#regression$Ideology = if_else(regression$political_party=="ενωση κεντρωων",regression$Ideology=="Center",regression$Ideology)
usa_speeches$USSR = if_else(usa_speeches$date=="1992-01",1,0)
usa_speeches$Imia = if_else(usa_speeches$date=="1996-01",1,0)
usa_speeches$Iraq=if_else(usa_speeches$date %in% c("2003-05"),1,0)
usa_speeches$Yugo=if_else(usa_speeches$date %in% c("1999-03","1999-04","1999-05","1999-06"),1,0)
usa_speeches = usa_speeches %>% mutate(date=str_extract(sitting_date,"...."))

regression = read_excel("regression.xlsx")
regression$date = as.character(regression$date)
usa_speeches = usa_speeches %>% left_join(regression)

usa_speeches$Ideology = as.factor(usa_speeches$Ideology)
usa_speeches = usa_speeches %>% pivot_wider(names_from = Ideology, values_from = Ideology, values_fn = length, values_fill = 0)



model = glm(sentiment~ topics+Imia+Yugo+referendum+Government_Opposition+first_memorandum+Iraq+economic_crisis+USSR+date+Outsider+Independent+ `Center-Right to Right`+`Center to Center-Left`+Left+Right-1, family = binomial(link="probit"),data = usa_speeches)
model_2 = glmer(sentiment~ topics+Imia+Yugo+referendum+first_memorandum+Iraq+economic_crisis+USSR+political_party+(1|date),family = binomial(link="probit"),data=usa_speeches)
summary(model)




library(sandwich)
library(lmtest)
coeftest(model,vcov=vcovCL,cluster=~date)

vars=rownames(data.frame(coef(model)) %>% filter(coef.model.!="Outsider"))
as.matrix(coeftest(model,vcov=vcovCL,cluster=~date)) %>% bind_cols(vars) %>%filter(...5 %in% c("`Center-Right to Right`","`Center to Center-Left`","Left","Right","Government_Opposition","Yugo","Iraq","Imia","USSR","first_memorandum","economic_crisis","referendum","topics0","topics1","topics2","topics3"))%>%
  mutate(...5 = if_else(...5=="topics0","Defense",if_else(...5=="topics1","EU-US-International Institutions",
                                                          if_else(...5=="topics2","Bases",if_else(...5=="topics3","Anti-Imperialism",
                                                                                                      if_else(...5=="Imia","Imia Crisis",
                                                                                                              if_else(...5=="referendum","Referendum",
                                                                                                                      if_else(...5=="Government_Opposition","Government-Opposition",
                                                                                                                              if_else(...5=="first_memorandum","First Memorandum",
                                                                                                                                      if_else(...5=="Iraq","War in Iraq",
                                                                                                                                              if_else(...5=="USSR","USSR Dissolution",
                                                                                                                                                      if_else(...5=="Yugo","Bombings of Yugoslavia",
                                                                                                                                                              if_else(...5=="`Center-Right to Right`","Center-Right",
                                                                                                                                                                      if_else(...5=="`Center to Center-Left`","Center-Left",
                                                                                                                                                                              if_else(...5=="Left","Far-Left",
                                                                                                                                                                                      if_else(...5=="Right","Far-Right",
                                                                                                                                                                                              if_else(...5=="economic_crisis","Economic Crisis",...5))))))))))))))))) %>%
  
  ggplot(aes(reorder(...5,Estimate),Estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=Estimate- (1.96*`Std. Error`),ymax=Estimate+ (1.96*`Std. Error`)))+
  coord_flip()+
  geom_hline(yintercept = 0,size=1,linetype="dashed")+
  xlab("Variables")+
  ylab("Coefficients")+
  theme(text=element_text(size=12),axis.title = element_text(size=12))


performance::r2(model)







###LASSO
corpus = corpus(usa_speeches$cleaned_text,docvars = usa_speeches)
manifesto_corpus_topic_modelling_new<- corpus %>% tokens(remove_punct = T,remove_numbers = T,include_docvars = T)  %>% tokens_ngrams(n=3:6) %>% 
  dfm(tolower = T) %>% dfm_trim(min_docfreq = 2)

sentiments <- docvars(manifesto_corpus_topic_modelling_new)$sentiment
dfm_manifestos <- as.data.frame(manifesto_corpus_topic_modelling_new)
dfm_manifestos <- cbind(sentiments, dfm_manifestos)

dfm_manifestos %>% count(sentiments)
dfm_manifestos<- dfm_manifestos %>% select(-doc_id)


library(caret)
set.seed(2)
index<- createDataPartition(dfm_manifestos$sentiments, p=.7,list=F )
train<- dfm_manifestos %>% slice(index)
test<- dfm_manifestos %>% slice(-index)

train %>% count(sentiments)





###LASSO training
###USNEG predicting words
train_x<- train %>% select(-sentiments) %>% data.matrix()
train_y<- train %>% select(sentiments) %>% data.matrix()
test_x<- test %>% select(-sentiments)%>% data.matrix()
test_y<- test %>% select(sentiments) %>% data.matrix()




library(glmnet)
set.seed(2)
ridge_model<- cv.glmnet(train_x,train_y, alpha=1,family="binomial")
plot(ridge_model)
predictions<- predict(ridge_model,test_x,s=ridge_model$lambda.min,type = "class")



confusionMatrix(as.factor(test_y),as.factor(predictions), mode = "prec_recall",positive = "2")

small.lambda.index <- which(ridge_model$lambda == ridge_model$lambda.min)
small.lambda.betas <- ridge_model$glmnet.fit$beta[,small.lambda.index]
small.lambda.betas[order(small.lambda.betas)][1:35]

###plot most predicting words

most_positive = small.lambda.betas[order(small.lambda.betas)][1:20] %>% as.data.frame()
most_negative= small.lambda.betas[order(-small.lambda.betas)][1:20]%>% as.data.frame()

most_predicting = bind_rows(most_negative,most_positive)

most_predicting$names = rownames(most_predicting)
most_predicting %>% ggplot(aes(x= reorder(names,.),y=.))+
  geom_col()+
  coord_flip()+
  ylab("Coefficients")+
  xlab("n-grams")+
  theme(text=element_text(size=13))























