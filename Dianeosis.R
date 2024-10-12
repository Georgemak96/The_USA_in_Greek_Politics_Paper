library(haven)
library(tidyverse)
setwd(".../MRB DATA")


eurobarometer_mrb = read_csv("Eurobarometer_MRB_Data.csv")


setwd(".../Dianeosis Data")


###DIANEOSIS 2016
dianeosis_2016 = read_sav("2016.sav")

dianeosis_2016 = dianeosis_2016 %>% mutate(anti_usa=if_else(GoodBad_amercans==1,0,if_else(GoodBad_amercans==2,1,NA)))
dianeosis_2016 = dianeosis_2016 %>% mutate(anti_rus=if_else(GoodBad_russians==1,0,if_else(GoodBad_russians==2,1,NA)))

dianeosis_2016 = dianeosis_2016 %>% mutate(eu=if_else(EUParticipation==3|EUParticipation==4,"ec_bad",if_else(EUParticipation==1|EUParticipation==2,"ec_good","Not")))
dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

dianeosis_2016=dianeosis_2016 %>%mutate(past_vote = if_else(pastvote==6,0,if_else(pastvote %in% c(4,5,13:50),4,if_else(pastvote>=66,5,pastvote))))
dianeosis_2016$past_vote = factor(dianeosis_2016$past_vote, levels = c(0:5), labels = c("kke","nd","syriza","pasok","others","invalid"))
dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)

dianeosis_2016 = dianeosis_2016 %>% mutate(left_right = if_else(LRplacement_p %in% c(1:2),"left",if_else(LRplacement_p%in%c(3:5),"center",if_else(LRplacement_p%in%c(6:7),"right","None"))))
dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = income,values_from = income,values_fn = length, values_fill = 0)
dianeosis_2016 = dianeosis_2016 %>% rename(low="1",middle_low=c("2","3"),middle_high=c("4","5"),high=c("6","7")) %>%
  mutate(middle_low=middle_low1+middle_low2,
         middle_high = middle_high1+middle_high2,
         high = high1+high2)




dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = profession,values_from = profession,values_fn = length, values_fill = 0)
dianeosis_2016 = dianeosis_2016 %>% rename(prof_status_self=c("5","6","12"),
                                                 prof_status_emp = c("1","2","10"),
                                                 prof_status_not = c("3","4","7","9"),
                                                 prof_status_unemp = c("8")) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2+prof_status_self3,
         prof_status_emp = prof_status_emp1+prof_status_emp2+prof_status_emp3,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4)







dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = UrbanRural,values_from = UrbanRural,values_fn = length, values_fill = 0,names_repair = "unique")
dianeosis_2016 = dianeosis_2016 %>% rename(com_big="1",
                                                 com_rur="2")




dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = region,values_from = region,values_fn = length, values_fill = 0)
dianeosis_2016 = dianeosis_2016 %>% rename(region_north=c("2","3"),
                                                 region_center=c("1","4","5","6","9","10"),
                                                 region_crete=c("7","8"))%>% 
  mutate(region_north=region_north1+region_north2,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)




dianeosis_2016 = dianeosis_2016 %>% mutate(gender=if_else(Gender==1,1,if_else(Gender==2,0,NA)))




dianeosis_2016 = dianeosis_2016 %>% mutate(age=if_else(age_continuous<=24,1,if_else(age_continuous %in% c(25:34),2,
                                                                                    if_else(age_continuous %in% c(35:44),3,
                                                                                            if_else(age_continuous %in% c(45,54),4,
                                                                                                    if_else(age_continuous %in% c(55:64),5,
                                                                                                            if_else(age_continuous>=65,6,age_continuous)))))))

dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = age,values_from = age,values_fn = length, values_fill = 0)
dianeosis_2016 = dianeosis_2016 %>% rename(decade9="1",decade8="2",decade7="3",decade6="4",decade5="5",decade4="6")



dianeosis_2016 = dianeosis_2016 %>% pivot_wider(names_from = edu,values_from = edu,values_fn = length, values_fill = 0)
dianeosis_2016 = dianeosis_2016 %>% rename(up_to_fifteen=c("1","2","3"),
                                                 sixteen_nineteen="4",
                                                 twenty_plus=c("5","6","7"))%>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2+up_to_fifteen3,
                                                                                       twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3)


dianeosis_2016 = dianeosis_2016 %>% mutate(year=2016)


dianeosis_2016 = dianeosis_2016 %>% select(up_to_fifteen,sixteen_nineteen,twenty_plus,gender,anti_usa,anti_rus,pasok,nd,kke,syriza,others,left,center,right,low,middle_low,middle_high,high,prof_status_not,
                          prof_status_emp,prof_status_self,prof_status_unemp,com_rur,com_big,ec_good,ec_bad,region_center,region_crete,region_north,
                          decade9,decade8,decade6,decade7,decade4,decade5,year)













###DIANEOSIS 2020
dianeosis_2020 = read_sav("2020_a.sav")

dianeosis_2020 = dianeosis_2020 %>% mutate(anti_usa=if_else(Q145==1,0,if_else(Q145==2,1,NA)))
dianeosis_2020 = dianeosis_2020 %>% mutate(anti_rus=if_else(Q148==1,0,if_else(Q148==2,1,NA)))

dianeosis_2020 = dianeosis_2020 %>% mutate(eu=if_else(Q02==3|Q02==4,"ec_bad",if_else(Q02==1|Q02==2,"ec_good","Not")))
dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

dianeosis_2020=dianeosis_2020 %>%mutate(past_vote = if_else(Q41 %in% c(5:10),5,if_else(Q41>=11,6,Q41)))
dianeosis_2020$past_vote = factor(dianeosis_2020$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))
dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)

dianeosis_2020 = dianeosis_2020 %>% mutate(left_right = if_else(Q39 %in% c(1:2),"left",if_else(Q39%in%c(3:5),"center",if_else(Q39%in%c(6:7),"right","None"))))
dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = D06,values_from = D06,values_fn = length, values_fill = 0)
dianeosis_2020 = dianeosis_2020 %>% rename(low="1",middle_low=c("2","3"),middle_high=c("4","5"),high=c("6","7")) %>%
  mutate(middle_low=middle_low1+middle_low2,
         middle_high = middle_high1+middle_high2,
         high = high1+high2)



dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = D05,values_from = D05,values_fn = length, values_fill = 0,names_repair = "unique")
dianeosis_2020 = dianeosis_2020 %>% rename(prof_status_self=c("5","6","7"),
                                           prof_status_emp = c("1","2","11"),
                                           prof_status_not = c("3","4","8...140","10"),
                                           prof_status_unemp = c("9")) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2+prof_status_self3,
         prof_status_emp = prof_status_emp1+prof_status_emp2+prof_status_emp3,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4)






dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = D11,values_from = D11,values_fn = length, values_fill = 0)
dianeosis_2020 = dianeosis_2020 %>% rename(com_big="1",
                                           com_rur="2")



dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = D09,values_from = D09,values_fn = length, values_fill = 0,names_repair = "unique")
dianeosis_2020 = dianeosis_2020 %>% rename(region_north=c("2","5","11","13"),
                                           region_center=c("1","3","4","6","8...164","9","12...165"),
                                           region_crete=c("7","10"))%>% 
  mutate(region_north=region_north1+region_north2+region_north3+region_north4,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6+region_center7,
         region_crete=region_crete1+region_crete2)



dianeosis_2020 = dianeosis_2020 %>% mutate(gender=if_else(D01==1,1,if_else(D01==2,0,NA)))






dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = D02,values_from = D02,values_fn = length, values_fill = 0)
dianeosis_2020 = dianeosis_2020 %>% rename(decade10="1",decade9="2",decade8="3",decade7="4",decade6="5",decade5="6")



dianeosis_2020 = dianeosis_2020 %>% pivot_wider(names_from = D03,values_from = D03,values_fn = length, values_fill = 0)
dianeosis_2020 = dianeosis_2020 %>% rename(up_to_fifteen=c("1","2","3"),
                                           sixteen_nineteen="4",
                                           twenty_plus=c("5","6","7"))%>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2+up_to_fifteen3,
                                                                                 twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3)


dianeosis_2020 = dianeosis_2020 %>% mutate(year=2019)



dianeosis_2020 = dianeosis_2020 %>% select(up_to_fifteen,sixteen_nineteen,twenty_plus,gender,anti_usa,anti_rus,pasok,nd,kke,syriza,others,left,center,right,low,middle_low,middle_high,high,prof_status_not,
                                           prof_status_emp,prof_status_self,prof_status_unemp,com_rur,com_big,ec_good,ec_bad,region_center,region_crete,region_north,
                                           decade9,decade8,decade6,decade7,decade10,decade5,year)














###DIANEOSIS 2022
dianeosis_2022 = read_sav("2022_a.sav")


dianeosis_2022 = dianeosis_2022 %>% mutate(anti_usa=if_else(Q17_5==1,0,if_else(Q17_5==2,1,NA)))
dianeosis_2022 = dianeosis_2022 %>% mutate(anti_rus=if_else(Q17_8==1,0,if_else(Q17_8==2,1,NA)))

dianeosis_2022 = dianeosis_2022 %>% mutate(eu=if_else(Q02==3|Q02==4,"ec_bad",if_else(Q02==1|Q02==2,"ec_good","Not")))
dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

dianeosis_2022=dianeosis_2022 %>%mutate(past_vote = if_else(Q40 %in% c(5:10),5,if_else(Q40>=11,6,Q40)))
dianeosis_2022$past_vote = factor(dianeosis_2022$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))
dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)

dianeosis_2022 = dianeosis_2022 %>% mutate(left_right = if_else(Q39 %in% c(1:2),"left",if_else(Q39%in%c(3:5),"center",if_else(Q39%in%c(6:7),"right","None"))))
dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = D05,values_from = D05,values_fn = length, values_fill = 0)
dianeosis_2022 = dianeosis_2022 %>% rename(low="1",middle_low=c("2","3"),middle_high=c("4","5"),high=c("6","7")) %>%
  mutate(middle_low=middle_low1+middle_low2,
         middle_high = middle_high1+middle_high2,
         high = high1+high2)



dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = D04,values_from = D04,values_fn = length, values_fill = 0,names_repair = "unique")
dianeosis_2022 = dianeosis_2022 %>% rename(prof_status_self=c("5","6","7"),
                                           prof_status_emp = c("1","2","11"),
                                           prof_status_not = c("3","4","8...123","10"),
                                           prof_status_unemp = c("9")) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2+prof_status_self3,
         prof_status_emp = prof_status_emp1+prof_status_emp2+prof_status_emp3,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4)






dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = D10,values_from = D10,values_fn = length, values_fill = 0)
dianeosis_2022 = dianeosis_2022 %>% rename(com_big="1",
                                           com_sm="2",
                                           com_rur="3")



dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = PER,values_from = PER,values_fn = length, values_fill = 0,names_repair = "unique")
dianeosis_2022 = dianeosis_2022 %>% rename(region_north=c("2","5","11","13"),
                                           region_center=c("1","3","4...138","6","8...143","9","12...148"),
                                           region_crete=c("7","10"))%>% 
  mutate(region_north=region_north1+region_north2+region_north3+region_north4,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6+region_center7,
         region_crete=region_crete1+region_crete2)



dianeosis_2022 = dianeosis_2022 %>% mutate(gender=if_else(D01==1,0,if_else(D01==2,1,NA)))






dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = D02,values_from = D02,values_fn = length, values_fill = 0)
dianeosis_2022 = dianeosis_2022 %>% rename(decade10="1",decade9="2",decade8="3",decade7="4",decade6="5",decade5="6")



dianeosis_2022 = dianeosis_2022 %>% pivot_wider(names_from = D03,values_from = D03,values_fn = length, values_fill = 0)
dianeosis_2022 = dianeosis_2022 %>% rename(up_to_fifteen=c("1","2","3"),
                                           sixteen_nineteen="4",
                                           twenty_plus=c("5","6","7"))%>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2+up_to_fifteen3,
                                                                                 twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3)


dianeosis_2022 = dianeosis_2022 %>% mutate(year=2022)




dianeosis_2022 = dianeosis_2022 %>% select(up_to_fifteen,sixteen_nineteen,twenty_plus,gender,anti_usa,anti_rus,pasok,nd,kke,syriza,others,left,center,right,low,middle_low,middle_high,high,prof_status_not,
                                           prof_status_emp,prof_status_self,prof_status_unemp,com_rur,com_big,com_sm,ec_good,ec_bad,region_center,region_crete,region_north,
                                           decade9,decade8,decade6,decade7,decade10,decade5,year)








###DIANEOSIS 2018
#dianeosis_2018 = read_sav("TPE2018_SPSS.sav",encoding = "ISO-8859-7")
#dianeosis_2018 %>% write_sav("2018.sav")



dianeosis_2018 = read_sav("2018.sav")


dianeosis_2018 = dianeosis_2018 %>% mutate(anti_usa=if_else(Q10_5==1,0,if_else(Q10_5==2,1,NA)))
dianeosis_2018 = dianeosis_2018 %>% mutate(anti_rus=if_else(Q10_8==1,0,if_else(Q10_8==2,1,NA)))

dianeosis_2018 = dianeosis_2018 %>% mutate(eu=if_else(Q1==3|Q1==4,"ec_bad",if_else(Q1==1|Q1==2,"ec_good","Not")))
dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

dianeosis_2018=dianeosis_2018 %>%mutate(past_vote = if_else(Q45==6,0,if_else(Q45 %in% c(4,5,7:11),4,if_else(Q45>=12,5,Q45))))
dianeosis_2018$past_vote = factor(dianeosis_2018$past_vote, levels = c(0:5), labels = c("kke","nd","syriza","pasok","others","invalid"))
dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)

dianeosis_2018 = dianeosis_2018 %>% mutate(left_right = if_else(Q43 %in% c(1:2),"left",if_else(Q43%in%c(3:5),"center",if_else(Q43%in%c(6:7),"right","None"))))
dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = D5,values_from = D5,values_fn = length, values_fill = 0)
dianeosis_2018 = dianeosis_2018 %>% rename(low="1",middle_low=c("2","3"),middle_high=c("4","5"),high=c("6")) %>%
  mutate(middle_low=middle_low1+middle_low2,
         middle_high = middle_high1+middle_high2)




dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = D4,values_from = D4,values_fn = length, values_fill = 0,names_repair = "unique")
dianeosis_2018 = dianeosis_2018 %>% rename(prof_status_self=c("5","6","7...161"),
                                           prof_status_emp = c("1","2","11"),
                                           prof_status_not = c("3","4","8","10"),
                                           prof_status_unemp = c("9")) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2+prof_status_self3,
         prof_status_emp = prof_status_emp1+prof_status_emp2+prof_status_emp3,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4)






dianeosis_2018 = dianeosis_2018 %>% mutate(PERIFA=if_else(PERIFA<=10,1,2))
dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = PERIFA,values_from = PERIFA,values_fn = length, values_fill = 0)
dianeosis_2018 = dianeosis_2018 %>% rename(com_big="1",
                                           com_rur="2")




dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = PERIF,values_from = PERIF,values_fn = length, values_fill = 0)
dianeosis_2018 = dianeosis_2018 %>% rename(region_north=c("2","3"),
                                           region_center=c("1","4","5","6","9","10"),
                                           region_crete=c("7","8"))%>% 
  mutate(region_north=region_north1+region_north2,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)




dianeosis_2018 = dianeosis_2018 %>% mutate(gender=if_else(D1==1,1,if_else(D1==2,0,NA)))




dianeosis_2018 = dianeosis_2018 %>% mutate(age=if_else(D2<=24,1,if_else(D2 %in% c(25:34),2,
                                                                                    if_else(D2 %in% c(35:44),3,
                                                                                            if_else(D2 %in% c(45,54),4,
                                                                                                    if_else(D2 %in% c(55:64),5,
                                                                                                            if_else(D2>=65,6,D2)))))))

dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = age,values_from = age,values_fn = length, values_fill = 0)
dianeosis_2018 = dianeosis_2018 %>% rename(decade10="1",decade9="2",decade8="3",decade7="4",decade6="5",decade5="6")



dianeosis_2018 = dianeosis_2018 %>% pivot_wider(names_from = D3,values_from = D3,values_fn = length, values_fill = 0)
dianeosis_2018 = dianeosis_2018 %>% rename(up_to_fifteen=c("1","2","3"),
                                           sixteen_nineteen="4",
                                           twenty_plus=c("5","6","7"))%>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2+up_to_fifteen3,
                                                                                 twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3)


dianeosis_2018 = dianeosis_2018 %>% mutate(year=2018)



dianeosis_2018 = dianeosis_2018 %>% select(up_to_fifteen,sixteen_nineteen,twenty_plus,gender,anti_usa,anti_rus,pasok,nd,kke,syriza,others,left,center,right,low,middle_low,middle_high,high,prof_status_not,
                                           prof_status_emp,prof_status_self,prof_status_unemp,com_rur,com_big,ec_good,ec_bad,region_center,region_crete,region_north,
                                           decade9,decade8,decade6,decade7,decade10,decade5,year)









###2024 
dianeosis_2024 = read_sav("2024_new.sav")


dianeosis_2024 = dianeosis_2024 %>% mutate(anti_usa=if_else(q16_6==1,0,if_else(q16_6==2,1,NA)))
dianeosis_2024 = dianeosis_2024 %>% mutate(anti_rus=if_else(q16_9==1,0,if_else(q16_9==2,1,NA)))

dianeosis_2024 = dianeosis_2024 %>% mutate(eu=if_else(q4==3|q4==4,"ec_bad",if_else(q4==1|q4==2,"ec_good","Not")))
dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

dianeosis_2024=dianeosis_2024 %>%mutate(past_vote = if_else(q26 %in% c(66:99),12,q26))
dianeosis_2024$past_vote = factor(dianeosis_2024$past_vote, levels = c(1:12), labels = c("nd","syriza","pasok","kke","spartiates","elliniki lisi","niki","plefsi eleftherias","mera25","patriotikos synaspismos","others","invalid"))
dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)

dianeosis_2024 = dianeosis_2024 %>% mutate(left_right = if_else(q25 %in% c(1:2),"left",if_else(q25%in%c(3:5),"center",if_else(q25%in%c(6:7),"right","None"))))
dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = D5,values_from = D5,values_fn = length, values_fill = 0)
dianeosis_2024 = dianeosis_2024 %>% rename(low="1",middle_low=c("2","3"),middle_high=c("4","5"),high=c("6")) %>%
  mutate(middle_low=middle_low1+middle_low2,
         middle_high = middle_high1+middle_high2)




dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = D4,values_from = D4,values_fn = length, values_fill = 0,names_repair = "unique")
dianeosis_2024 = dianeosis_2024 %>% rename(prof_status_self=c("5","6","7"),
                                           prof_status_emp = c("1","2","11"),
                                           prof_status_not = c("3","4","8","10"),
                                           prof_status_unemp = c("9...147")) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2+prof_status_self3,
         prof_status_emp = prof_status_emp1+prof_status_emp2+prof_status_emp3,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4)






dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = URBAN,values_from = URBAN,values_fn = length, values_fill = 0)
dianeosis_2024 = dianeosis_2024 %>% rename(com_big="1",
                                           com_rur="2")




dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = PERIFEREIA,values_from = PERIFEREIA,values_fn = length, values_fill = 0)
dianeosis_2024 = dianeosis_2024 %>% rename(region_north=c("2","3","4"),
                                           region_center=c("1","5","6","8","9","10"),
                                           region_crete=c("11","12","13"))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2+region_crete3)




dianeosis_2024 = dianeosis_2024 %>% mutate(gender=if_else(gender==1,1,if_else(gender==2,0,NA)))




dianeosis_2024 = dianeosis_2024 %>% mutate(age=if_else(AGE<=24,1,if_else(AGE %in% c(25:34),2,
                                                                        if_else(AGE %in% c(35:44),3,
                                                                                if_else(AGE %in% c(45,54),4,
                                                                                        if_else(AGE %in% c(55:64),5,
                                                                                                if_else(AGE>=65,6,AGE)))))))

dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = age,values_from = age,values_fn = length, values_fill = 0)
dianeosis_2024 = dianeosis_2024 %>% rename(decade10="1",decade9="2",decade8="3",decade7="4",decade6="5",decade5="6")



dianeosis_2024 = dianeosis_2024 %>% pivot_wider(names_from = D3,values_from = D3,values_fn = length, values_fill = 0)
dianeosis_2024 = dianeosis_2024 %>% rename(up_to_fifteen=c("1"),
                                           sixteen_nineteen="2",
                                           twenty_plus=c("3","4","7"))%>% mutate(twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3)


dianeosis_2024 = dianeosis_2024 %>% mutate(year=2024)



dianeosis_2024 = dianeosis_2024 %>% select(up_to_fifteen,sixteen_nineteen,twenty_plus,gender,anti_usa,anti_rus,pasok,nd,kke,syriza,left,center,right,low,middle_low,middle_high,high,prof_status_not,
                                           prof_status_emp,prof_status_self,prof_status_unemp,com_rur,com_big,ec_good,ec_bad,region_center,region_crete,region_north,
                                           decade9,decade8,decade6,decade7,decade10,decade5,year)











new = bind_rows(dianeosis_2016,dianeosis_2018,dianeosis_2020,dianeosis_2022,dianeosis_2024)


eurobarometer_mrb_dianeosis = bind_rows(eurobarometer_mrb,new)

#eurobarometer_mrb_dianeosis %>% write_csv("Combined_Data/Eurobarometer_MRB_Dianeosis_Data.csv")



