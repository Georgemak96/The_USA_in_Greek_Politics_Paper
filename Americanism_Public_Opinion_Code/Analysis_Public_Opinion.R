library(haven)
library(tidyverse)
setwd(".../Combined_Data")
data=read_dta("final dataset.dta")





all = read_csv("Eurobarometer_MRB_Dianeosis_Data.csv")





####Read the party sentiments from the other paper 
setwd(".../Manifestos and Speeches Files")
parties_scores_one = read_csv("parties_sentiments_1989-2004.csv")
parties_scores_two = read_csv("parties_sentiments_2005-2014.csv")
parties_scores_three = read_csv("parties_sentiments_2015-2020.csv")
parties_scores_four = read_csv("parties_sentiments_2021-2024.csv")

parties_scores = bind_rows(parties_scores_one,parties_scores_two,parties_scores_three,parties_scores_four)





####Kolmogorov-Smirnov tests and visualisations

###In the following lines (until line 43) please change the name of the parties (full name, greek, lowercase) and the variable names (if you want to, not necessary)
####in order to run the tests and visualisations for the rest of the parties too
nd = all  %>% filter(kke==1) %>% group_by(year) %>% summarise(americanism=((1-mean(anti_usa,na.rm=T))-mean(anti_usa,na.rm=T)))


test = parties_scores %>% filter(political_party%in%c("κομμουνιστικο κομμα ελλαδας")) %>% left_join(nd,by=c("date"="year")) %>% filter(!is.na(americanism)) %>%
  mutate(americanism=scale(americanism)) %>% mutate(difference_prop=scale(difference_prop))





kolmogorov_kke = ks.test(test$difference_prop[,1],test$americanism[,1])





test %>% pivot_longer(c(americanism,difference_prop), names_to = "Americanism",values_to = "value") %>% ggplot(aes(value,colour=Americanism))+
  stat_ecdf()+
  scale_color_discrete(labels = c("Mass Level","Elite Level"))






setwd(".../Combined_Data")



all_1980 = all %>% filter(year==1980)
all_1980$anti_usa = as.factor(all_1980$anti_usa)
model_1980 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+gender+left+right+middle_low+middle_high+high+
              prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
            +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1980)

summary(model_1980)





all_1982 = all %>% filter(year==1982)
all_1982$anti_usa = as.factor(all_1982$anti_usa)
model_1982 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1982)

summary(model_1982)






all_1984 = all %>% filter(year==1984)
all_1984$anti_usa = as.factor(all_1984$anti_usa)
model_1984 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1984)

summary(model_1984)









all_1985 = all %>% filter(year==1985)
all_1985$anti_usa = as.factor(all_1985$anti_usa)
model_1985 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1985)

summary(model_1985)





all_1986 = all %>% filter(year==1986)
all_1986$anti_usa = as.factor(all_1986$anti_usa)
model_1986 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1986)

summary(model_1986)







all_1987 = all %>% filter(year==1987)
all_1987$anti_usa = as.factor(all_1987$anti_usa)
model_1987 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1987)

summary(model_1987)








all_1988 = all %>% filter(year==1988)
all_1988$anti_usa = as.factor(all_1988$anti_usa)
model_1988 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1988)

summary(model_1988)












all_1989 = all %>% filter(year==1989)
all_1989$anti_usa = as.factor(all_1989$anti_usa)
model_1989 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1989)

summary(model_1989)






all_1990 = all %>% filter(year==1990)
all_1990$anti_usa = as.factor(all_1990$anti_usa)
model_1990 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+syriza+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1990)

summary(model_1990)










all_1991 = all %>% filter(year==1991)
all_1991$anti_usa = as.factor(all_1991$anti_usa)
model_1991 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+syriza+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1991)

summary(model_1991)







all_1993 = all %>% filter(year==1993)
all_1993$anti_usa = as.factor(all_1993$anti_usa)
model_1993 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1993)

summary(model_1993)







all_1994 = all %>% filter(year==1994)
all_1994$anti_usa = as.factor(all_1994$anti_usa)
model_1994 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1994)

summary(model_1994)









all_1995 = all %>% filter(year==1995)
all_1995$anti_usa = as.factor(all_1995$anti_usa)
model_1995 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1995)

summary(model_1995)












all_1996 = all %>% filter(year==1996)
all_1996$anti_usa = as.factor(all_1996$anti_usa)
model_1996 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+syriza+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1996)

summary(model_1996)










all_1997 = all %>% filter(year==1997)
all_1997$anti_usa = as.factor(all_1997$anti_usa)
model_1997 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+syriza+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1997)

summary(model_1997)








all_2002 = all %>% filter(year==2002)
all_2002$anti_usa = as.factor(all_2002$anti_usa)
model_2002 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade4+decade5+decade6+decade7, family = binomial(link = "probit"),data=all_2002)

summary(model_2002)









all_2003 = all %>% filter(year==2003)
all_2003$anti_usa = as.factor(all_2003$anti_usa)
model_2003 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade4+decade5+decade6+decade7, family = binomial(link = "probit"),data=all_2003)

summary(model_2003)








all_2004 = all %>% filter(year==2004)
all_2004$anti_usa = as.factor(all_2004$anti_usa)
model_2004 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+syriza+life+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+com_rur+com_big+ec_good+region_center+region_north
                 +decade3+decade4+decade5+decade6+decade7, family = binomial(link = "probit"),data=all_2004)

summary(model_2004)










all_2005 = all %>% filter(year==2005)
all_2005$anti_usa = as.factor(all_2005$anti_usa)
model_2005 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+
                   prof_status_emp+prof_status_self+com_rur+com_big+ec_good+region_center+region_north
                 +decade3+decade4+decade5+decade6+decade7, family = binomial(link = "probit"),data=all_2005)

summary(model_2005)






all_2006 = all %>% filter(year==2006)
all_2006$anti_usa = as.factor(all_2006$anti_usa)
model_2006 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+life+gender+left+right+
                   prof_status_emp+prof_status_self+com_rur+com_big+ec_good+region_center+region_north
                 +decade3+decade4+decade5+decade6+decade7, family = binomial(link = "probit"),data=all_2006)

summary(model_2006)












all_2007 = all %>% filter(year==2007)
all_2007$anti_usa = as.factor(all_2007$anti_usa)
model_2007 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade4+decade5+decade6+decade7+decade8, family = binomial(link = "probit"),data=all_2007)

summary(model_2007)









all_2008 = all %>% filter(year==2008)
all_2008$anti_usa = as.factor(all_2008$anti_usa)
model_2008 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2008)

summary(model_2008)









all_2009 = all %>% filter(year==2009)
all_2009$anti_usa = as.factor(all_2009$anti_usa)
model_2009 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2009)

summary(model_2009)




all_2010 = all %>% filter(year==2010)
all_2010$anti_usa = as.factor(all_2010$anti_usa)
model_2010 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2010)

summary(model_2010)





all_2011 = all %>% filter(year==2011)
all_2011$anti_usa = as.factor(all_2011$anti_usa)
model_2011 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2011)

summary(model_2011)








all_2012 = all %>% filter(year==2012)
all_2012$anti_usa = as.factor(all_2012$anti_usa)
model_2012 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2012)

summary(model_2012)






all_2013 = all %>% filter(year==2013)
all_2013$anti_usa = as.factor(all_2013$anti_usa)
model_2013 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2013)

summary(model_2013)








all_2014 = all %>% filter(year==2014)
all_2014$anti_usa = as.factor(all_2014$anti_usa)
model_2014 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2014)

summary(model_2014)










all_2016 = all %>% filter(year==2016)
all_2016$anti_usa = as.factor(all_2016$anti_usa)
model_2016 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2016)

summary(model_2016)









all_2017 = all %>% filter(year==2017)
all_2017$anti_usa = as.factor(all_2017$anti_usa)
model_2017 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2017)

summary(model_2017)






all_2018 = all %>% filter(year==2018)
all_2018$anti_usa = as.factor(all_2018$anti_usa)
model_2018 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2018)

summary(model_2018)










all_2019 = all %>% filter(year==2019)
all_2019$anti_usa = as.factor(all_2019$anti_usa)
model_2019 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2019)

summary(model_2019)







all_2020 = all %>% filter(year==2020)
all_2020$anti_usa = as.factor(all_2020$anti_usa)
model_2020 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2020)

summary(model_2020)





all_2021 = all %>% filter(year==2021)
all_2021$anti_usa = as.factor(all_2021$anti_usa)
model_2021 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2021)

summary(model_2021)








all_2022 = all %>% filter(year==2022)
all_2022$anti_usa = as.factor(all_2022$anti_usa)
model_2022 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2022)

summary(model_2022)







all_2023 = all %>% filter(year==2023)
all_2023$anti_usa = as.factor(all_2023$anti_usa)
model_2023 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2023)

summary(model_2023)



all_2024 = all %>% filter(year==2024)
all_2024$anti_usa = as.factor(all_2024$anti_usa)
model_2024 = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+pasok+nd+kke+syriza+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2024)

summary(model_2024)








coef_one = data.frame(coefficients = coef(model_1980), variables=names(coef(model_1980)),standard_errors = summary(model_1980)$coefficients[, "Std. Error"]) %>% mutate(year=1980)
coef_two = data.frame(coefficients = coef(model_1982), variables=names(coef(model_1982)),standard_errors = summary(model_1982)$coefficients[, "Std. Error"])%>% mutate(year=1982)
coef_three = data.frame(coefficients = coef(model_1984), variables=names(coef(model_1984)),standard_errors = summary(model_1984)$coefficients[, "Std. Error"])%>% mutate(year=1984)
coef_four = data.frame(coefficients = coef(model_1985), variables=names(coef(model_1985)),standard_errors = summary(model_1985)$coefficients[, "Std. Error"])%>% mutate(year=1985)
coef_five = data.frame(coefficients = coef(model_1986), variables=names(coef(model_1986)),standard_errors = summary(model_1986)$coefficients[, "Std. Error"])%>% mutate(year=1986)
coef_six = data.frame(coefficients = coef(model_1987), variables=names(coef(model_1987)),standard_errors = summary(model_1987)$coefficients[, "Std. Error"])%>% mutate(year=1987)
coef_seven = data.frame(coefficients = coef(model_1988), variables=names(coef(model_1988)),standard_errors = summary(model_1988)$coefficients[, "Std. Error"])%>% mutate(year=1988)
coef_eight = data.frame(coefficients = coef(model_1989), variables=names(coef(model_1989)),standard_errors = summary(model_1989)$coefficients[, "Std. Error"])%>% mutate(year=1989)
coef_nine = data.frame(coefficients = coef(model_1990), variables=names(coef(model_1990)),standard_errors = summary(model_1990)$coefficients[, "Std. Error"])%>% mutate(year=1990)
coef_ten = data.frame(coefficients = coef(model_1991), variables=names(coef(model_1991)),standard_errors = summary(model_1991)$coefficients[, "Std. Error"])%>% mutate(year=1991)
coef_twelve = data.frame(coefficients = coef(model_1993), variables=names(coef(model_1993)),standard_errors = summary(model_1993)$coefficients[, "Std. Error"])%>% mutate(year=1993)
coef_thirteen = data.frame(coefficients = coef(model_1994), variables=names(coef(model_1994)),standard_errors = summary(model_1994)$coefficients[, "Std. Error"])%>% mutate(year=1994)
coef_fourteen = data.frame(coefficients = coef(model_1995), variables=names(coef(model_1995)),standard_errors = summary(model_1995)$coefficients[, "Std. Error"])%>% mutate(year=1995)
coef_fifteen = data.frame(coefficients = na.omit(coef(model_1996)), variables=names(na.omit(coef(model_1996))),standard_errors = summary(model_1996)$coefficients[, "Std. Error"])%>% mutate(year=1996)
coef_sixteen = data.frame(coefficients = coef(model_1997), variables=names(coef(model_1997)),standard_errors = summary(model_1997)$coefficients[, "Std. Error"])%>% mutate(year=1997)
coef_seventeen = data.frame(coefficients = coef(model_2002), variables=names(coef(model_2002)),standard_errors = summary(model_2002)$coefficients[, "Std. Error"])%>% mutate(year=2002)
coef_eighteen = data.frame(coefficients = coef(model_2003), variables=names(coef(model_2003)),standard_errors = summary(model_2003)$coefficients[, "Std. Error"])%>% mutate(year=2003)
coef_nineteen = data.frame(coefficients = coef(model_2004), variables=names(coef(model_2004)),standard_errors = summary(model_2004)$coefficients[, "Std. Error"])%>% mutate(year=2004)
coef_twenty = data.frame(coefficients = coef(model_2005), variables=names(coef(model_2005)),standard_errors = summary(model_2005)$coefficients[, "Std. Error"])%>% mutate(year=2005)
coef_twenty_one = data.frame(coefficients = coef(model_2006), variables=names(coef(model_2006)),standard_errors = summary(model_2006)$coefficients[, "Std. Error"])%>% mutate(year=2006)
coef_twenty_two = data.frame(coefficients = na.omit(coef(model_2007)), variables=names(na.omit(coef(model_2007))),standard_errors = summary(model_2007)$coefficients[, "Std. Error"])%>% mutate(year=2007)
coef_twenty_three = data.frame(coefficients = na.omit(coef(model_2008)), variables=names(na.omit(coef(model_2008))),standard_errors = summary(model_2008)$coefficients[, "Std. Error"])%>% mutate(year=2008)
coef_twenty_four = data.frame(coefficients = na.omit(coef(model_2009)), variables=names(na.omit(coef(model_2009))),standard_errors = summary(model_2009)$coefficients[, "Std. Error"])%>% mutate(year=2009)
coef_twenty_five = data.frame(coefficients = na.omit(coef(model_2010)), variables=names(na.omit(coef(model_2010))),standard_errors = summary(model_2010)$coefficients[, "Std. Error"])%>% mutate(year=2010)
coef_twenty_six = data.frame(coefficients = na.omit(coef(model_2011)), variables=names(na.omit(coef(model_2011))),standard_errors = summary(model_2011)$coefficients[, "Std. Error"])%>% mutate(year=2011)
coef_twenty_seven = data.frame(coefficients = na.omit(coef(model_2012)), variables=names(na.omit(coef(model_2012))),standard_errors = summary(model_2012)$coefficients[, "Std. Error"])%>% mutate(year=2012)
coef_twenty_eight = data.frame(coefficients = na.omit(coef(model_2013)), variables=names(na.omit(coef(model_2013))),standard_errors = summary(model_2013)$coefficients[, "Std. Error"])%>% mutate(year=2013)
coef_twenty_nine = data.frame(coefficients = na.omit(coef(model_2014)), variables=names(na.omit(coef(model_2014))),standard_errors = summary(model_2014)$coefficients[, "Std. Error"])%>% mutate(year=2014)
coef_thirty = data.frame(coefficients = na.omit(coef(model_2016)), variables=names(na.omit(coef(model_2016))),standard_errors = summary(model_2016)$coefficients[, "Std. Error"])%>% mutate(year=2016)
coef_thirty_one = data.frame(coefficients = na.omit(coef(model_2017)), variables=names(na.omit(coef(model_2017))),standard_errors = summary(model_2017)$coefficients[, "Std. Error"])%>% mutate(year=2017)
coef_thirty_two = data.frame(coefficients = na.omit(coef(model_2018)), variables=names(na.omit(coef(model_2018))),standard_errors = summary(model_2018)$coefficients[, "Std. Error"])%>% mutate(year=2018)
coef_thirty_three = data.frame(coefficients = na.omit(coef(model_2019)), variables=names(na.omit(coef(model_2019))),standard_errors = summary(model_2019)$coefficients[, "Std. Error"])%>% mutate(year=2019)
coef_thirty_four = data.frame(coefficients = na.omit(coef(model_2020)), variables=names(na.omit(coef(model_2020))),standard_errors = summary(model_2020)$coefficients[, "Std. Error"])%>% mutate(year=2020)
coef_thirty_five = data.frame(coefficients = na.omit(coef(model_2021)), variables=names(na.omit(coef(model_2021))),standard_errors = summary(model_2021)$coefficients[, "Std. Error"])%>% mutate(year=2021)
coef_thirty_six = data.frame(coefficients = na.omit(coef(model_2022)), variables=names(na.omit(coef(model_2022))),standard_errors = summary(model_2022)$coefficients[, "Std. Error"])%>% mutate(year=2022)
coef_thirty_seven = data.frame(coefficients = na.omit(coef(model_2023)), variables=names(na.omit(coef(model_2023))),standard_errors = summary(model_2023)$coefficients[, "Std. Error"])%>% mutate(year=2023)
coef_thirty_eight = data.frame(coefficients = na.omit(coef(model_2024)), variables=names(na.omit(coef(model_2024))),standard_errors = summary(model_2024)$coefficients[, "Std. Error"])%>% mutate(year=2024)



coefficients_all = bind_rows(coef_one,coef_two,coef_three,coef_four,coef_five,coef_six,coef_seven,coef_eight,coef_nine,coef_ten,coef_twelve,coef_thirteen,
                             coef_fourteen,coef_fifteen,coef_sixteen,coef_seventeen,coef_eighteen,coef_nineteen,coef_twenty,coef_twenty_one,coef_twenty_two,
                             coef_twenty_three,coef_twenty_four,coef_twenty_five,coef_twenty_six,coef_twenty_seven,coef_twenty_eight,coef_twenty_nine,
                             coef_thirty,coef_thirty_one,coef_thirty_two,coef_thirty_three,coef_thirty_four,coef_thirty_five,coef_thirty_six,coef_thirty_seven,coef_thirty_eight)







###Graph 1
coefficients_all %>% filter(variables %in% c("left","right")) %>% rename(Ideology=variables) %>% 
  ggplot(aes(year,coefficients,color=Ideology))+
  geom_line()+
  geom_ribbon(aes(ymin=coefficients-(1.96*standard_errors), ymax=coefficients+(1.96*standard_errors)),alpha=0.2)+
  geom_hline(yintercept = 0,color="black")+
  ylab("Coefficients")+
  xlab("Year")+
  theme(text=element_text(size=13))



###Graph 2
coefficients_all %>% filter(variables %in% c("pasok","nd","syriza","kke")) %>% rename(Parties=variables) %>% 
  ggplot(aes(year,coefficients,color=Parties))+
  geom_line()+
  xlab("Year")+
  ylab("Coefficients")+
  theme(text=element_text(size=13))





###Graph 5
coefficients_all %>% filter(variables %in% c("ec_good","ec_bad")) %>% rename(EU_STANCES=variables) %>% 
  ggplot(aes(year,coefficients,color=EU_STANCES))+
  geom_line()+
  geom_ribbon(aes(ymin=coefficients-(1.96*standard_errors), ymax=coefficients+(1.96*standard_errors)),alpha=0.2)+
  geom_hline(yintercept = 0,color="black")+
  xlab("Year")+
  ylab("Coefficients")+
  theme(text=element_text(size=13))









####INCLUDING RUSSIAN STANCES
model_1980_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+anti_rus+still_studying+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1980)

summary(model_1980_rus)








model_1986_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+anti_rus+still_studying+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade1+decade4+decade5, family = binomial(link = "probit"),data=all_1986)

summary(model_1986_rus)




model_1990_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+syriza+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1990)

summary(model_1990_rus)






model_1991_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+syriza+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1991)

summary(model_1991_rus)





model_1993_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1993)

summary(model_1993_rus)




model_1994_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1994)

summary(model_1994_rus)





model_1995_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1995)

summary(model_1995_rus)




model_1996_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+syriza+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1996)

summary(model_1996_rus)







model_1997_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+syriza+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade2+decade4+decade5+decade6, family = binomial(link = "probit"),data=all_1997)

summary(model_1997_rus)










model_2002_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+gender+left+right+middle_low+middle_high+high+
                   prof_status_emp+prof_status_self+prof_status_unemp+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade3+decade4+decade5+decade6+decade7, family = binomial(link = "probit"),data=all_2002)

summary(model_2002_rus)







model_2016_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2016)

summary(model_2016_rus)







model_2017_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade4+decade5+decade6+decade7+decade8+decade9, family = binomial(link = "probit"),data=all_2017)

summary(model_2017_rus)





model_2018_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2018)

summary(model_2018_rus)





model_2019_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2019)

summary(model_2019_rus)





model_2020_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2020)

summary(model_2020_rus)





model_2021_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2021)

summary(model_2021_rus)






model_2022_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2022)

summary(model_2022_rus)






model_2023_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+anti_rus+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2023)

summary(model_2023_rus)



model_2024_rus = glm(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+pasok+nd+kke+syriza+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10, family = binomial(link = "probit"),data=all_2024)

summary(model_2024_rus)




rus_1 = data.frame(coefficients = coef(model_1980_rus), variables=names(coef(model_1980_rus)),standard_errors = summary(model_1980_rus)$coefficients[, "Std. Error"]) %>% mutate(year=1980)
rus_2 = data.frame(coefficients = coef(model_1986_rus), variables=names(coef(model_1986_rus)),standard_errors = summary(model_1986_rus)$coefficients[, "Std. Error"])%>% mutate(year=1986)
rus_3 = data.frame(coefficients = coef(model_1990_rus), variables=names(coef(model_1990_rus)),standard_errors = summary(model_1990_rus)$coefficients[, "Std. Error"])%>% mutate(year=1990)
rus_4 = data.frame(coefficients = coef(model_1991_rus), variables=names(coef(model_1991_rus)),standard_errors = summary(model_1991_rus)$coefficients[, "Std. Error"])%>% mutate(year=1991)
rus_5 = data.frame(coefficients = coef(model_1993_rus), variables=names(coef(model_1993_rus)),standard_errors = summary(model_1993_rus)$coefficients[, "Std. Error"])%>% mutate(year=1993)
rus_6 = data.frame(coefficients = coef(model_1994_rus), variables=names(coef(model_1994_rus)),standard_errors = summary(model_1994_rus)$coefficients[, "Std. Error"])%>% mutate(year=1994)
rus_7 = data.frame(coefficients = coef(model_1995_rus), variables=names(coef(model_1995_rus)),standard_errors = summary(model_1995_rus)$coefficients[, "Std. Error"])%>% mutate(year=1995)
rus_8 = data.frame(coefficients = na.omit(coef(model_1996_rus)), variables=names(na.omit(coef(model_1996_rus))),standard_errors = summary(model_1996_rus)$coefficients[, "Std. Error"])%>% mutate(year=1996)
rus_9 = data.frame(coefficients = coef(model_1997_rus), variables=names(coef(model_1997_rus)),standard_errors = summary(model_1997_rus)$coefficients[, "Std. Error"])%>% mutate(year=1997)
rus_10 = data.frame(coefficients = coef(model_2002_rus), variables=names(coef(model_2002_rus)),standard_errors = summary(model_2002_rus)$coefficients[, "Std. Error"])%>% mutate(year=2002)
rus_11 = data.frame(coefficients = na.omit(coef(model_2016_rus)), variables=names(na.omit(coef(model_2016_rus))),standard_errors = summary(model_2016_rus)$coefficients[, "Std. Error"])%>% mutate(year=2016)
rus_12 = data.frame(coefficients = na.omit(coef(model_2017_rus)), variables=names(na.omit(coef(model_2017_rus))),standard_errors = summary(model_2017_rus)$coefficients[, "Std. Error"])%>% mutate(year=2017)
rus_13 = data.frame(coefficients = na.omit(coef(model_2018_rus)), variables=names(na.omit(coef(model_2018_rus))),standard_errors = summary(model_2018_rus)$coefficients[, "Std. Error"])%>% mutate(year=2018)
rus_14 = data.frame(coefficients = na.omit(coef(model_2019_rus)), variables=names(na.omit(coef(model_2019_rus))),standard_errors = summary(model_2019_rus)$coefficients[, "Std. Error"])%>% mutate(year=2019)
rus_15 = data.frame(coefficients = na.omit(coef(model_2020_rus)), variables=names(na.omit(coef(model_2020_rus))),standard_errors = summary(model_2020_rus)$coefficients[, "Std. Error"])%>% mutate(year=2020)
rus_16 = data.frame(coefficients = na.omit(coef(model_2021_rus)), variables=names(na.omit(coef(model_2021_rus))),standard_errors = summary(model_2021_rus)$coefficients[, "Std. Error"])%>% mutate(year=2021)
rus_17 = data.frame(coefficients = na.omit(coef(model_2022_rus)), variables=names(na.omit(coef(model_2022_rus))),standard_errors = summary(model_2022_rus)$coefficients[, "Std. Error"])%>% mutate(year=2022)
rus_18 = data.frame(coefficients = na.omit(coef(model_2023_rus)), variables=names(na.omit(coef(model_2023_rus))),standard_errors = summary(model_2023_rus)$coefficients[, "Std. Error"])%>% mutate(year=2023)
rus_19 = data.frame(coefficients = na.omit(coef(model_2024_rus)), variables=names(na.omit(coef(model_2024_rus))),standard_errors = summary(model_2024_rus)$coefficients[, "Std. Error"])%>% mutate(year=2024)



coefficients_all_rus = bind_rows(rus_1,rus_2,rus_3,rus_4,rus_5,rus_6,rus_7,rus_8,rus_9,rus_10,rus_11,rus_12,rus_13,rus_14,rus_15,rus_16,rus_17,rus_18,rus_19)




#library(stargazer)
#stargazer(model_2024,type = "html", out = "regression_results_2024.html",
          #column.labels = c("2024"))



#summary(model_2024)







###Graph 3 
coefficients_all_rus %>% filter(variables %in% c("anti_rus")) %>%
  ggplot(aes(year,-coefficients))+
  geom_line()+
  geom_ribbon(aes(ymin=-coefficients-(1.96*standard_errors), ymax=-coefficients+(1.96*standard_errors)),alpha=0.2)+
  geom_hline(yintercept = 0,color="black")+
  ylab("Coefficients")+
  xlab("Year")+
  theme(text=element_text(size=13))




###Graph 4
all %>% group_by(year) %>% summarise(anti=mean(anti_usa,na.rm=T),pro=1-mean(anti_usa,na.rm=T),score=pro-anti) %>% 
  ggplot(aes(year,score))+
  geom_smooth()+
  geom_hline(yintercept = 0, color = "red")+
  ylab("Americanism")+
  xlab("Year")+
  theme(text=element_text(size=13))










###Graph 5
all %>%filter(!is.na(anti_rus)) %>%  group_by(year) %>% summarise(anti=mean(anti_rus,na.rm=T),pro=1-mean(anti_rus,na.rm=T),score=pro-anti) %>% 
  ggplot(aes(year,pro))+
  geom_smooth()+
  ylab("Pro_Russia")+
  xlab("Year")+
  theme(text=element_text(size=13))















all$anti_usa = as.factor(all$anti_usa)
library(lme4)
all$year=as.factor(all$year)
model_all = glmer(anti_usa~ up_to_fifteen+sixteen_nineteen+twenty_plus+still_studying+pasok+nd+kke+syriza+life+gender+left+right+middle_low+
                   middle_high+high+prof_status_unemp+prof_status_emp+prof_status_self+com_rur+com_big+ec_good+ec_bad+region_center+region_north
                 +decade5+decade6+decade7+decade8+decade9+decade10+(1|year), family = binomial(link = "probit"),data=all)

summary(model_all)





#stargazer(model_all,type = "html", out = "regression_results_overall.html")
