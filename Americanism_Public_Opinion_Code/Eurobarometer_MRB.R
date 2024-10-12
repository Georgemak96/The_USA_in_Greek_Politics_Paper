library(haven)
library(tidyverse)
setwd("")


data = read_dta("final dataset.dta")

data = data %>% pivot_wider(names_from = educ_age,values_from = educ_age,values_fn = length, values_fill = 0)
data = data %>% rename(up_to_fifteen="1",
                       sixteen_nineteen="2",
                       twenty_plus="3",
                       still_studying="4")

data$up_to_fifteen = if_else(data$up_to_fifteen>1,1,data$up_to_fifteen)
data$sixteen_nineteen = if_else(data$sixteen_nineteen>1,1,data$sixteen_nineteen)
data$twenty_plus = if_else(data$twenty_plus>1,1,data$twenty_plus)
data$still_studying = if_else(data$still_studying>1,1,data$still_studying)





###DECEMBER 2008
december_2008_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2008.sav")
december_2008_mrb = december_2008_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2008_mrb = december_2008_mrb %>% mutate(anti_usa=if_else(a75a1==1|a75a1==2,1,if_else(a75a1==4|a75a1==5,0,NA)))
december_2008_mrb=december_2008_mrb %>%mutate(past_vote = if_else(k3>=5&k3<=8,5,if_else(k3>=9,6,k3)))
december_2008_mrb$past_vote = factor(december_2008_mrb$past_vote, levels = c(1:6), labels = c("nd","pasok","kke","syriza","others","invalid"))

december_2008_mrb = december_2008_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2008_mrb = december_2008_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2008_mrb = december_2008_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2008_mrb = december_2008_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2008_mrb = december_2008_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                             prof_status_emp = c(`@dhm53`,`@dhm54`),
                             prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                             prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2008_mrb = december_2008_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                             com_sm = `@dhm4`,
                             com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2008_mrb = december_2008_mrb %>% mutate(eu=if_else(a75a2==1|a75a2==2,"ec_bad",if_else(a75a2==4|a75a2==5,"ec_good",if_else(a75a2==3,"ec_neither","Not"))))
december_2008_mrb = december_2008_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2008_mrb = december_2008_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                             region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                             region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2008_mrb = december_2008_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2008_mrb = december_2008_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                             sixteen_nineteen=`@dhm35`,
                             twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                             still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                             twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                             still_studying=still_studying1+still_studying2)





december_2008_mrb = december_2008_mrb %>% mutate(year=2008)
december_2008_mrb = december_2008_mrb %>% rename(gender=`@dhm17`)




###JUNE 2008
december_2008_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2008.sav")
december_2008_june_mrb = december_2008_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2008_june_mrb = december_2008_june_mrb %>% mutate(anti_usa=if_else(a58a1==1|a58a1==2,1,if_else(a58a1==4|a58a1==5,0,NA)))
december_2008_june_mrb=december_2008_june_mrb %>%mutate(past_vote = if_else(k3>=5&k3<=7,5,if_else(k3>=8,6,k3)))
december_2008_june_mrb$past_vote = factor(december_2008_june_mrb$past_vote, levels = c(1:6), labels = c("nd","pasok","kke","syriza","others","invalid"))

december_2008_june_mrb = december_2008_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2008_june_mrb = december_2008_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2008_june_mrb = december_2008_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2008_june_mrb = december_2008_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2008_june_mrb = december_2008_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                 prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                 prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                 prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2008_june_mrb = december_2008_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                 com_sm = `@dhm4`,
                                                 com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2008_june_mrb = december_2008_june_mrb %>% mutate(eu=if_else(a58a2==1|a58a2==2,"ec_bad",if_else(a58a2==4|a58a2==5,"ec_good",if_else(a58a2==3,"ec_neither","Not"))))
december_2008_june_mrb = december_2008_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2008_june_mrb = december_2008_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                 region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                 region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2008_june_mrb = december_2008_june_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2008_june_mrb = december_2008_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                 sixteen_nineteen=`@dhm35`,
                                                 twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                 still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                 twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                 still_studying=still_studying1+still_studying2)





december_2008_june_mrb = december_2008_june_mrb %>% mutate(year=2008)
december_2008_june_mrb = december_2008_june_mrb %>% rename(gender=`@dhm17`)









###DECEMBER 2009
december_2009_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2009.sav")
december_2009_mrb = december_2009_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2009_mrb = december_2009_mrb %>% mutate(anti_usa=if_else(a63a1==1|a63a1==2,1,if_else(a63a1==4|a63a1==5,0,NA)))
december_2009_mrb=december_2009_mrb %>%mutate(past_vote = if_else(k3>=5&k3<=8,5,if_else(k3>=9,6,k3)))
december_2009_mrb$past_vote = factor(december_2009_mrb$past_vote, levels = c(1:6), labels = c("pasok","nd","kke","syriza","others","invalid"))

december_2009_mrb = december_2009_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2009_mrb = december_2009_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8,9,0),"right",NA))))
december_2009_mrb = december_2009_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2009_mrb = december_2009_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2009_mrb = december_2009_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2009_mrb = december_2009_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2009_mrb = december_2009_mrb %>% mutate(eu=if_else(a63a2==1|a63a2==2,"ec_bad",if_else(a63a2==4|a63a2==5,"ec_good",if_else(a63a2==3,"ec_neither","Not"))))
december_2009_mrb = december_2009_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2009_mrb = december_2009_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2009_mrb = december_2009_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2009_mrb = december_2009_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2009_mrb = december_2009_mrb %>% mutate(year=2009)
december_2009_mrb = december_2009_mrb %>% rename(gender=`@dhm17`)










###JUNE 2010
december_2010_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2010.sav")
december_2010_june_mrb = december_2010_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2010_june_mrb = december_2010_june_mrb %>% mutate(anti_usa=if_else(a75a1==1|a75a1==2,1,if_else(a75a1==4|a75a1==5,0,NA)))
december_2010_june_mrb=december_2010_june_mrb %>%mutate(past_vote = if_else(k3==5,0,if_else(k3 %in% c(4,6,7),4,if_else(k3>=8,5,k3))))
december_2010_june_mrb$past_vote = factor(december_2010_june_mrb$past_vote, levels = c(0:5), labels = c("syriza","pasok","nd","kke","others","invalid"))

december_2010_june_mrb = december_2010_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2010_june_mrb = december_2010_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2010_june_mrb = december_2010_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2010_june_mrb = december_2010_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2010_june_mrb = december_2010_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                 prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                 prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                 prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2010_june_mrb = december_2010_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                 com_sm = `@dhm4`,
                                                 com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2010_june_mrb = december_2010_june_mrb %>% mutate(eu=if_else(a75a2==1|a75a2==2,"ec_bad",if_else(a75a2==4|a75a2==5,"ec_good",if_else(a75a2==3,"ec_neither","Not"))))
december_2010_june_mrb = december_2010_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2010_june_mrb = december_2010_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                 region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                 region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2010_june_mrb = december_2010_june_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2010_june_mrb = december_2010_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                 sixteen_nineteen=`@dhm35`,
                                                 twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                 still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                 twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                 still_studying=still_studying1+still_studying2)





december_2010_june_mrb = december_2010_june_mrb %>% mutate(year=2010)
december_2010_june_mrb = december_2010_june_mrb %>% rename(gender=`@dhm17`)







###DECEMBER 2010
december_2010_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2010.sav")
december_2010_dec_mrb = december_2010_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2010_dec_mrb = december_2010_dec_mrb %>% mutate(anti_usa=if_else(a78a1==1|a78a1==2,1,if_else(a78a1==4|a78a1==5,0,NA)))
december_2010_dec_mrb=december_2010_dec_mrb %>%mutate(past_vote = if_else(k3==5,0,if_else(k3 %in% c(4,6,7),4,if_else(k3>=8,5,k3))))
december_2010_dec_mrb$past_vote = factor(december_2010_dec_mrb$past_vote, levels = c(0:5), labels = c("syriza","pasok","nd","kke","others","invalid"))

december_2010_dec_mrb = december_2010_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2010_dec_mrb = december_2010_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2010_dec_mrb = december_2010_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2010_dec_mrb = december_2010_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2010_dec_mrb = december_2010_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2010_dec_mrb = december_2010_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2010_dec_mrb = december_2010_dec_mrb %>% mutate(eu=if_else(a78a2==1|a78a2==2,"ec_bad",if_else(a78a2==4|a78a2==5,"ec_good",if_else(a78a2==3,"ec_neither","Not"))))
december_2010_dec_mrb = december_2010_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2010_dec_mrb = december_2010_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2010_dec_mrb = december_2010_dec_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2010_dec_mrb = december_2010_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2010_dec_mrb = december_2010_dec_mrb %>% mutate(year=2010)
december_2010_dec_mrb = december_2010_dec_mrb %>% rename(gender=`@dhm17`)










###JUNE 2011
december_2011_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2011.sav")
december_2011_june_mrb = december_2011_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2011_june_mrb = december_2011_june_mrb %>% mutate(anti_usa=if_else(a88a1==1|a88a1==2,1,if_else(a88a1==4|a88a1==5,0,NA)))
december_2011_june_mrb=december_2011_june_mrb %>%mutate(past_vote = if_else(k3==5,0,if_else(k3 %in% c(4,6,7),4,if_else(k3>=8,5,k3))))
december_2011_june_mrb$past_vote = factor(december_2011_june_mrb$past_vote, levels = c(0:5), labels = c("syriza","pasok","nd","kke","others","invalid"))

december_2011_june_mrb = december_2011_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2011_june_mrb = december_2011_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2011_june_mrb = december_2011_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2011_june_mrb = december_2011_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2011_june_mrb = december_2011_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2011_june_mrb = december_2011_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2011_june_mrb = december_2011_june_mrb %>% mutate(eu=if_else(a88a2==1|a88a2==2,"ec_bad",if_else(a88a2==4|a88a2==5,"ec_good",if_else(a88a2==3,"ec_neither","Not"))))
december_2011_june_mrb = december_2011_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2011_june_mrb = december_2011_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2011_june_mrb = december_2011_june_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2011_june_mrb = december_2011_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2011_june_mrb = december_2011_june_mrb %>% mutate(year=2011)
december_2011_june_mrb = december_2011_june_mrb %>% rename(gender=`@dhm17`)









###DECEMBER 2011
december_2011_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2011.sav")
december_2011_dec_mrb = december_2011_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2011_dec_mrb = december_2011_dec_mrb %>% mutate(anti_usa=if_else(a57a1==1|a57a1==2,1,if_else(a57a1==4|a57a1==5,0,NA)))
december_2011_dec_mrb=december_2011_dec_mrb %>%mutate(past_vote = if_else(k3==5,0,if_else(k3 %in% c(4,6,7),4,if_else(k3>=8,5,k3))))
december_2011_dec_mrb$past_vote = factor(december_2011_dec_mrb$past_vote, levels = c(0:5), labels = c("syriza","pasok","nd","kke","others","invalid"))

december_2011_dec_mrb = december_2011_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2011_dec_mrb = december_2011_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2011_dec_mrb = december_2011_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2011_dec_mrb = december_2011_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2011_dec_mrb = december_2011_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2011_dec_mrb = december_2011_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2011_dec_mrb = december_2011_dec_mrb %>% mutate(eu=if_else(a57a2==1|a57a2==2,"ec_bad",if_else(a57a2==4|a57a2==5,"ec_good",if_else(a57a2==3,"ec_neither","Not"))))
december_2011_dec_mrb = december_2011_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2011_dec_mrb = december_2011_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2011_dec_mrb = december_2011_dec_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2011_dec_mrb = december_2011_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2011_dec_mrb = december_2011_dec_mrb %>% mutate(year=2011)
december_2011_dec_mrb = december_2011_dec_mrb %>% rename(gender=`@dhm17`)









###DECEMBER 2012
december_2012_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2012.sav")
december_2012_dec_mrb = december_2012_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2012_dec_mrb = december_2012_dec_mrb %>% mutate(anti_usa=if_else(a45a1==1|a45a1==2,1,if_else(a45a1==4|a45a1==5,0,NA)))
december_2012_dec_mrb=december_2012_dec_mrb %>%mutate(past_vote = if_else(k3==7,0,if_else(k3 %in% c(4:6,8:11),4,if_else(k3>=12,5,k3))))
december_2012_dec_mrb$past_vote = factor(december_2012_dec_mrb$past_vote, levels = c(0:5), labels = c("kke","nd","syriza","pasok","others","invalid"))

december_2012_dec_mrb = december_2012_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2012_dec_mrb = december_2012_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2012_dec_mrb = december_2012_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2012_dec_mrb = december_2012_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2012_dec_mrb = december_2012_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2012_dec_mrb = december_2012_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2012_dec_mrb = december_2012_dec_mrb %>% mutate(eu=if_else(a45a2==1|a45a2==2,"ec_bad",if_else(a45a2==4|a45a2==5,"ec_good",if_else(a45a2==3,"ec_neither","Not"))))
december_2012_dec_mrb = december_2012_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2012_dec_mrb = december_2012_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2012_dec_mrb = december_2012_dec_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2012_dec_mrb = december_2012_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2012_dec_mrb = december_2012_dec_mrb %>% mutate(year=2012)
december_2012_dec_mrb = december_2012_dec_mrb %>% rename(gender=`@dhm17`)






###JUNE 2013
december_2013_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2013.sav")
december_2013_june_mrb = december_2013_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2013_june_mrb = december_2013_june_mrb %>% mutate(anti_usa=if_else(a45a1==1|a45a1==2,1,if_else(a45a1==4|a45a1==5,0,NA)))
december_2013_june_mrb=december_2013_june_mrb %>%mutate(past_vote = if_else(k3==7,0,if_else(k3 %in% c(4:6,8:11),4,if_else(k3>=12,5,k3))))
december_2013_june_mrb$past_vote = factor(december_2013_june_mrb$past_vote, levels = c(0:5), labels = c("kke","nd","syriza","pasok","others","invalid"))

december_2013_june_mrb = december_2013_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2013_june_mrb = december_2013_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2013_june_mrb = december_2013_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2013_june_mrb = december_2013_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2013_june_mrb = december_2013_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2013_june_mrb = december_2013_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2013_june_mrb = december_2013_june_mrb %>% mutate(eu=if_else(a45a2==1|a45a2==2,"ec_bad",if_else(a45a2==4|a45a2==5,"ec_good",if_else(a45a2==3,"ec_neither","Not"))))
december_2013_june_mrb = december_2013_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2013_june_mrb = december_2013_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2013_june_mrb = december_2013_june_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2013_june_mrb = december_2013_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2013_june_mrb = december_2013_june_mrb %>% mutate(year=2013)
december_2013_june_mrb = december_2013_june_mrb %>% rename(gender=`@dhm17`)






###DECEMBER 2013
december_2013_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2013.sav")
december_2013_dec_mrb = december_2013_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2013_dec_mrb = december_2013_dec_mrb %>% mutate(anti_usa=if_else(a47a1==1|a47a1==2,1,if_else(a47a1==4|a47a1==5,0,NA)))
december_2013_dec_mrb=december_2013_dec_mrb %>%mutate(past_vote = if_else(k3==7,0,if_else(k3 %in% c(4:6,8:11),4,if_else(k3>=12,5,k3))))
december_2013_dec_mrb$past_vote = factor(december_2013_dec_mrb$past_vote, levels = c(0:5), labels = c("kke","nd","syriza","pasok","others","invalid"))

december_2013_dec_mrb = december_2013_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2013_dec_mrb = december_2013_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2013_dec_mrb = december_2013_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2013_dec_mrb = december_2013_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2013_dec_mrb = december_2013_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2013_dec_mrb = december_2013_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2013_dec_mrb = december_2013_dec_mrb %>% mutate(eu=if_else(a47a2==1|a47a2==2,"ec_bad",if_else(a47a2==4|a47a2==5,"ec_good",if_else(a47a2==3,"ec_neither","Not"))))
december_2013_dec_mrb = december_2013_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2013_dec_mrb = december_2013_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2013_dec_mrb = december_2013_dec_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2013_dec_mrb = december_2013_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2013_dec_mrb = december_2013_dec_mrb %>% mutate(year=2013)
december_2013_dec_mrb = december_2013_dec_mrb %>% rename(gender=`@dhm17`)








###JUNE 2014
december_2014_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2014.sav")
december_2014_june_mrb = december_2014_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2014_june_mrb = december_2014_june_mrb %>% mutate(anti_usa=if_else(a55a1==1|a55a1==2,1,if_else(a55a1==4|a55a1==5,0,NA)))
december_2014_june_mrb=december_2014_june_mrb %>%mutate(past_vote = if_else(k3==7,0,if_else(k3 %in% c(4:6,8:11),4,if_else(k3>=12,5,k3))))
december_2014_june_mrb$past_vote = factor(december_2014_june_mrb$past_vote, levels = c(0:5), labels = c("kke","nd","syriza","pasok","others","invalid"))

december_2014_june_mrb = december_2014_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2014_june_mrb = december_2014_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2014_june_mrb = december_2014_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2014_june_mrb = december_2014_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2014_june_mrb = december_2014_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2014_june_mrb = december_2014_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2014_june_mrb = december_2014_june_mrb %>% mutate(eu=if_else(a55a2==1|a55a2==2,"ec_bad",if_else(a55a2==4|a55a2==5,"ec_good",if_else(a55a2==3,"ec_neither","Not"))))
december_2014_june_mrb = december_2014_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2014_june_mrb = december_2014_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2014_june_mrb = december_2014_june_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2014_june_mrb = december_2014_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2014_june_mrb = december_2014_june_mrb %>% mutate(year=2014)
december_2014_june_mrb = december_2014_june_mrb %>% rename(gender=`@dhm17`)










###DECEMBER 2014
december_2014_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2014.sav")
december_2014_dec_mrb = december_2014_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2014_dec_mrb = december_2014_dec_mrb %>% mutate(anti_usa=if_else(a55a1==1|a55a1==2,1,if_else(a55a1==4|a55a1==5,0,NA)))
december_2014_dec_mrb=december_2014_dec_mrb %>%mutate(past_vote = if_else(k3==7,0,if_else(k3 %in% c(4:6,8:11),4,if_else(k3>=12,5,k3))))
december_2014_dec_mrb$past_vote = factor(december_2014_dec_mrb$past_vote, levels = c(0:5), labels = c("kke","nd","syriza","pasok","others","invalid"))

december_2014_dec_mrb = december_2014_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2014_dec_mrb = december_2014_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2014_dec_mrb = december_2014_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2014_dec_mrb = december_2014_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2014_dec_mrb = december_2014_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2014_dec_mrb = december_2014_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2014_dec_mrb = december_2014_dec_mrb %>% mutate(eu=if_else(a55a2==1|a55a2==2,"ec_bad",if_else(a55a2==4|a55a2==5,"ec_good",if_else(a55a2==3,"ec_neither","Not"))))
december_2014_dec_mrb = december_2014_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2014_dec_mrb = december_2014_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2014_dec_mrb = december_2014_dec_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2014_dec_mrb = december_2014_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2014_dec_mrb = december_2014_dec_mrb %>% mutate(year=2014)
december_2014_dec_mrb = december_2014_dec_mrb %>% rename(gender=`@dhm17`)






### JUNE 2016
december_2016_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2016.sav")
december_2016_june_mrb = december_2016_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2016_june_mrb = december_2016_june_mrb %>% mutate(anti_usa=if_else(a67a2==1|a67a2==2,1,if_else(a67a2==4|a67a2==5,0,NA)))
december_2016_june_mrb = december_2016_june_mrb %>% mutate(anti_rus=if_else(a67a13==1|a67a13==2,1,if_else(a67a13==4|a67a13==5,0,NA)))
december_2016_june_mrb=december_2016_june_mrb %>%mutate(past_vote = if_else(k3 %in% c(3,6:12),3,if_else(k3>=13,6,k3)))
december_2016_june_mrb$past_vote = factor(december_2016_june_mrb$past_vote, levels = c(1:6), labels = c("syriza","nd","others","pasok","kke","invalid"))

december_2016_june_mrb = december_2016_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2016_june_mrb = december_2016_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2016_june_mrb = december_2016_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2016_june_mrb = december_2016_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2016_june_mrb = december_2016_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2016_june_mrb = december_2016_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2016_june_mrb = december_2016_june_mrb %>% mutate(eu=if_else(a67a3==1|a67a3==2,"ec_bad",if_else(a67a3==4|a67a3==5,"ec_good",if_else(a67a3==3,"ec_neither","Not"))))
december_2016_june_mrb$eu=if_else(is.na(december_2016_june_mrb$eu),"None",december_2016_june_mrb$eu)
december_2016_june_mrb = december_2016_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2016_june_mrb = december_2016_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2016_june_mrb = december_2016_june_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2016_june_mrb = december_2016_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2016_june_mrb = december_2016_june_mrb %>% mutate(year=2016)
december_2016_june_mrb = december_2016_june_mrb %>% rename(gender=`@dhm17`)







###DECEMBER 2016
december_2016_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2016.sav")
december_2016_dec_mrb = december_2016_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2016_dec_mrb = december_2016_dec_mrb %>% mutate(anti_usa=if_else(a85a2==1|a85a2==2,1,if_else(a85a2==4|a85a2==5,0,NA)))
december_2016_dec_mrb = december_2016_dec_mrb %>% mutate(anti_rus=if_else(a85a13==1|a85a13==2,1,if_else(a85a13==4|a85a13==5,0,NA)))
december_2016_dec_mrb=december_2016_dec_mrb %>%mutate(past_vote = if_else(k3 %in% c(3,6:14),3,if_else(k3>=15,6,k3)))
december_2016_dec_mrb$past_vote = factor(december_2016_dec_mrb$past_vote, levels = c(1:6), labels = c("syriza","nd","others","pasok","kke","invalid"))

december_2016_dec_mrb = december_2016_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2016_dec_mrb = december_2016_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2016_dec_mrb = december_2016_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2016_dec_mrb = december_2016_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2016_dec_mrb = december_2016_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2016_dec_mrb = december_2016_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2016_dec_mrb = december_2016_dec_mrb %>% mutate(eu=if_else(a85a3==1|a85a3==2,"ec_bad",if_else(a85a3==4|a85a3==5,"ec_good",if_else(a85a3==3,"ec_neither","Not"))))
december_2016_dec_mrb$eu=if_else(is.na(december_2016_dec_mrb$eu),"None",december_2016_dec_mrb$eu)
december_2016_dec_mrb = december_2016_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2016_dec_mrb = december_2016_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2016_dec_mrb = december_2016_dec_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2016_dec_mrb = december_2016_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2016_dec_mrb = december_2016_dec_mrb %>% mutate(year=2016)
december_2016_dec_mrb = december_2016_dec_mrb %>% rename(gender=`@dhm17`)






###JUNE 2017
december_2017_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2017.sav")
december_2017_june_mrb = december_2017_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2017_june_mrb = december_2017_june_mrb %>% mutate(anti_usa=if_else(a73a2==1|a73a2==2,1,if_else(a73a2==4|a73a2==5,0,NA)))
december_2017_june_mrb = december_2017_june_mrb %>% mutate(anti_rus=if_else(a73a13==1|a73a13==2,1,if_else(a73a13==4|a73a13==5,0,NA)))
december_2017_june_mrb=december_2017_june_mrb %>%mutate(past_vote = if_else(k3 %in% c(3,6:14),3,if_else(k3>=15,6,k3)))
december_2017_june_mrb$past_vote = factor(december_2017_june_mrb$past_vote, levels = c(1:6), labels = c("syriza","nd","others","pasok","kke","invalid"))

december_2017_june_mrb = december_2017_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2017_june_mrb = december_2017_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2017_june_mrb = december_2017_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2017_june_mrb = december_2017_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2017_june_mrb = december_2017_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2017_june_mrb = december_2017_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2017_june_mrb = december_2017_june_mrb %>% mutate(eu=if_else(a73a3==1|a73a3==2,"ec_bad",if_else(a73a3==4|a73a3==5,"ec_good",if_else(a73a3==3,"ec_neither","Not"))))
december_2017_june_mrb$eu=if_else(is.na(december_2017_june_mrb$eu),"None",december_2017_june_mrb$eu)
december_2017_june_mrb = december_2017_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2017_june_mrb = december_2017_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2017_june_mrb = december_2017_june_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2017_june_mrb = december_2017_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2017_june_mrb = december_2017_june_mrb %>% mutate(year=2017)
december_2017_june_mrb = december_2017_june_mrb %>% rename(gender=`@dhm17`)






###DECEMBER 2017
december_2017_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2017.sav")
december_2017_dec_mrb = december_2017_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2017_dec_mrb = december_2017_dec_mrb %>% mutate(anti_usa=if_else(a74a2==1|a74a2==2,1,if_else(a74a2==4|a74a2==5,0,NA)))
december_2017_dec_mrb = december_2017_dec_mrb %>% mutate(anti_rus=if_else(a74a13==1|a74a13==2,1,if_else(a74a13==4|a74a13==5,0,NA)))
december_2017_dec_mrb=december_2017_dec_mrb %>%mutate(past_vote = if_else(k3 %in% c(3,6:14),3,if_else(k3>=15,6,k3)))
december_2017_dec_mrb$past_vote = factor(december_2017_dec_mrb$past_vote, levels = c(1:6), labels = c("syriza","nd","others","pasok","kke","invalid"))

december_2017_dec_mrb = december_2017_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2017_dec_mrb = december_2017_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2017_dec_mrb = december_2017_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2017_dec_mrb = december_2017_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2017_dec_mrb = december_2017_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2017_dec_mrb = december_2017_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2017_dec_mrb = december_2017_dec_mrb %>% mutate(eu=if_else(a74a3==1|a74a3==2,"ec_bad",if_else(a74a3==4|a74a3==5,"ec_good",if_else(a74a3==3,"ec_neither","Not"))))
december_2017_dec_mrb$eu=if_else(is.na(december_2017_dec_mrb$eu),"None",december_2017_dec_mrb$eu)
december_2017_dec_mrb = december_2017_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2017_dec_mrb = december_2017_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2017_dec_mrb = december_2017_dec_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2017_dec_mrb = december_2017_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2017_dec_mrb = december_2017_dec_mrb %>% mutate(year=2017)
december_2017_dec_mrb = december_2017_dec_mrb %>% rename(gender=`@dhm17`)




###JUNE 2018
december_2018_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2018.sav")
december_2018_june_mrb = december_2018_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2018_june_mrb = december_2018_june_mrb %>% mutate(anti_usa=if_else(a64a2==1|a64a2==2,1,if_else(a64a2==4|a64a2==5,0,NA)))
december_2018_june_mrb = december_2018_june_mrb %>% mutate(anti_rus=if_else(a64a13==1|a64a13==2,1,if_else(a64a13==4|a64a13==5,0,NA)))
december_2018_june_mrb=december_2018_june_mrb %>%mutate(past_vote = if_else(k3 %in% c(3,6:14),3,if_else(k3>=15,6,k3)))
december_2018_june_mrb$past_vote = factor(december_2018_june_mrb$past_vote, levels = c(1:6), labels = c("syriza","nd","others","pasok","kke","invalid"))

december_2018_june_mrb = december_2018_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2018_june_mrb = december_2018_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2018_june_mrb = december_2018_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2018_june_mrb = december_2018_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2018_june_mrb = december_2018_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2018_june_mrb = december_2018_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2018_june_mrb = december_2018_june_mrb %>% mutate(eu=if_else(a64a3==1|a64a3==2,"ec_bad",if_else(a64a3==4|a64a3==5,"ec_good",if_else(a64a3==3,"ec_neither","Not"))))
december_2018_june_mrb$eu=if_else(is.na(december_2018_june_mrb$eu),"None",december_2018_june_mrb$eu)
december_2018_june_mrb = december_2018_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2018_june_mrb = december_2018_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2018_june_mrb = december_2018_june_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2018_june_mrb = december_2018_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2018_june_mrb = december_2018_june_mrb %>% mutate(year=2018)
december_2018_june_mrb = december_2018_june_mrb %>% rename(gender=`@dhm17`)






###DECEMBER 2018
december_2018_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2018.sav")
december_2018_dec_mrb = december_2018_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2018_dec_mrb = december_2018_dec_mrb %>% mutate(anti_usa=if_else(a69a2==1|a69a2==2,1,if_else(a69a2==4|a69a2==5,0,NA)))
december_2018_dec_mrb = december_2018_dec_mrb %>% mutate(anti_rus=if_else(a69a13==1|a69a13==2,1,if_else(a69a13==4|a69a13==5,0,NA)))
december_2018_dec_mrb=december_2018_dec_mrb %>%mutate(past_vote = if_else(k3 %in% c(3,6:14),3,if_else(k3>=15,6,k3)))
december_2018_dec_mrb$past_vote = factor(december_2018_dec_mrb$past_vote, levels = c(1:6), labels = c("syriza","nd","others","pasok","kke","invalid"))

december_2018_dec_mrb = december_2018_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2018_dec_mrb = december_2018_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2018_dec_mrb = december_2018_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2018_dec_mrb = december_2018_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2018_dec_mrb = december_2018_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2018_dec_mrb = december_2018_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2018_dec_mrb = december_2018_dec_mrb %>% mutate(eu=if_else(a69a3==1|a69a3==2,"ec_bad",if_else(a69a3==4|a69a3==5,"ec_good",if_else(a69a3==3,"ec_neither","Not"))))
december_2018_dec_mrb$eu=if_else(is.na(december_2018_dec_mrb$eu),"None",december_2018_dec_mrb$eu)
december_2018_dec_mrb = december_2018_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2018_dec_mrb = december_2018_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2018_dec_mrb = december_2018_dec_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2018_dec_mrb = december_2018_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2018_dec_mrb = december_2018_dec_mrb %>% mutate(year=2018)
december_2018_dec_mrb = december_2018_dec_mrb %>% rename(gender=`@dhm17`)



###DECEMBER 2019
december_2019_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2019.sav")
december_2019_dec_mrb = december_2019_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2019_dec_mrb = december_2019_dec_mrb %>% mutate(anti_usa=if_else(a92a2==1|a92a2==2,1,if_else(a92a2==4|a92a2==5,0,NA)))
december_2019_dec_mrb = december_2019_dec_mrb %>% mutate(anti_rus=if_else(a92a13==1|a92a13==2,1,if_else(a92a13==4|a92a13==5,0,NA)))
december_2019_dec_mrb=december_2019_dec_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:14),5,if_else(k3>=15,6,k3)))
december_2019_dec_mrb$past_vote = factor(december_2019_dec_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2019_dec_mrb = december_2019_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2019_dec_mrb = december_2019_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2019_dec_mrb = december_2019_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2019_dec_mrb = december_2019_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2019_dec_mrb = december_2019_dec_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2019_dec_mrb = december_2019_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2019_dec_mrb = december_2019_dec_mrb %>% mutate(eu=if_else(a92a3==1|a92a3==2,"ec_bad",if_else(a92a3==4|a92a3==5,"ec_good",if_else(a92a3==3,"ec_neither","Not"))))
december_2019_dec_mrb$eu=if_else(is.na(december_2019_dec_mrb$eu),"None",december_2019_dec_mrb$eu)
december_2019_dec_mrb = december_2019_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2019_dec_mrb = december_2019_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2019_dec_mrb = december_2019_dec_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2019_dec_mrb = december_2019_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2019_dec_mrb = december_2019_dec_mrb %>% mutate(year=2019)
december_2019_dec_mrb = december_2019_dec_mrb %>% rename(gender=`@dhm17`)






###JUNE 2020
december_2020_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2020.sav")
december_2020_june_mrb = december_2020_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2020_june_mrb = december_2020_june_mrb %>% mutate(anti_usa=if_else(a84a2==1|a84a2==2,1,if_else(a84a2==4|a84a2==5,0,NA)))
december_2020_june_mrb = december_2020_june_mrb %>% mutate(anti_rus=if_else(a84a13==1|a84a13==2,1,if_else(a84a13==4|a84a13==5,0,NA)))
december_2020_june_mrb=december_2020_june_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:14),5,if_else(k3>=15,6,k3)))
december_2020_june_mrb$past_vote = factor(december_2020_june_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2020_june_mrb = december_2020_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2020_june_mrb = december_2020_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2020_june_mrb = december_2020_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2020_june_mrb = december_2020_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2020_june_mrb = december_2020_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2020_june_mrb = december_2020_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2020_june_mrb = december_2020_june_mrb %>% mutate(eu=if_else(a84a3==1|a84a3==2,"ec_bad",if_else(a84a3==4|a84a3==5,"ec_good",if_else(a84a3==3,"ec_neither","Not"))))
december_2020_june_mrb$eu=if_else(is.na(december_2020_june_mrb$eu),"None",december_2020_june_mrb$eu)
december_2020_june_mrb = december_2020_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2020_june_mrb = december_2020_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2020_june_mrb = december_2020_june_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2020_june_mrb = december_2020_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2020_june_mrb = december_2020_june_mrb %>% mutate(year=2020)
december_2020_june_mrb = december_2020_june_mrb %>% rename(gender=`@dhm17`)






###NOVEMBER 2020
december_2020_nov_mrb = read_sav("ΤΑΣΕΙΣ ΝΟΕΜΒΡΙΟΣ 2020.sav")
december_2020_nov_mrb = december_2020_nov_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2020_nov_mrb = december_2020_nov_mrb %>% mutate(anti_usa=if_else(a65a2==1|a65a2==2,1,if_else(a65a2==4|a65a2==5,0,NA)))
december_2020_nov_mrb = december_2020_nov_mrb %>% mutate(anti_rus=if_else(a65a13==1|a65a13==2,1,if_else(a65a13==4|a65a13==5,0,NA)))
december_2020_nov_mrb=december_2020_nov_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:14),5,if_else(k3>=15,6,k3)))
december_2020_nov_mrb$past_vote = factor(december_2020_nov_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2020_nov_mrb = december_2020_nov_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2020_nov_mrb = december_2020_nov_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2020_nov_mrb = december_2020_nov_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2020_nov_mrb = december_2020_nov_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2020_nov_mrb = december_2020_nov_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                           prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                           prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                           prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2020_nov_mrb = december_2020_nov_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2020_nov_mrb = december_2020_nov_mrb %>% mutate(eu=if_else(a65a3==1|a65a3==2,"ec_bad",if_else(a65a3==4|a65a3==5,"ec_good",if_else(a65a3==3,"ec_neither","Not"))))
december_2020_nov_mrb$eu=if_else(is.na(december_2020_nov_mrb$eu),"None",december_2020_nov_mrb$eu)
december_2020_nov_mrb = december_2020_nov_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2020_nov_mrb = december_2020_nov_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2020_nov_mrb = december_2020_nov_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2020_nov_mrb = december_2020_nov_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                           sixteen_nineteen=`@dhm35`,
                                                           twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                           still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2020_nov_mrb = december_2020_nov_mrb %>% mutate(year=2020)
december_2020_nov_mrb = december_2020_nov_mrb %>% rename(gender=`@dhm17`)







###JUNE 2021
december_2021_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2021.sav")
december_2021_june_mrb = december_2021_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2021_june_mrb = december_2021_june_mrb %>% mutate(anti_usa=if_else(a98a2==1|a98a2==2,1,if_else(a98a2==4|a98a2==5,0,NA)))
december_2021_june_mrb = december_2021_june_mrb %>% mutate(anti_rus=if_else(a98a13==1|a98a13==2,1,if_else(a98a13==4|a98a13==5,0,NA)))
december_2021_june_mrb=december_2021_june_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:14),5,if_else(k3>=15,6,k3)))
december_2021_june_mrb$past_vote = factor(december_2021_june_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2021_june_mrb = december_2021_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2021_june_mrb = december_2021_june_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2021_june_mrb = december_2021_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2021_june_mrb = december_2021_june_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2021_june_mrb = december_2021_june_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2021_june_mrb = december_2021_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2021_june_mrb = december_2021_june_mrb %>% mutate(eu=if_else(a98a3==1|a98a3==2,"ec_bad",if_else(a98a3==4|a98a3==5,"ec_good",if_else(a98a3==3,"ec_neither","Not"))))
december_2021_june_mrb$eu=if_else(is.na(december_2021_june_mrb$eu),"None",december_2021_june_mrb$eu)
december_2021_june_mrb = december_2021_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2021_june_mrb = december_2021_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2021_june_mrb = december_2021_june_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2021_june_mrb = december_2021_june_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2021_june_mrb = december_2021_june_mrb %>% mutate(year=2021)
december_2021_june_mrb = december_2021_june_mrb %>% rename(gender=`@dhm17`)






###DECEMBER 2021
december_2021_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2021.sav")
december_2021_dec_mrb = december_2021_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2021_dec_mrb = december_2021_dec_mrb %>% mutate(anti_usa=if_else(a98a2==1|a98a2==2,1,if_else(a98a2==4|a98a2==5,0,NA)))
december_2021_dec_mrb = december_2021_dec_mrb %>% mutate(anti_rus=if_else(a98a13==1|a98a13==2,1,if_else(a98a13==4|a98a13==5,0,NA)))
december_2021_dec_mrb=december_2021_dec_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:14),5,if_else(k3>=15,6,k3)))
december_2021_dec_mrb$past_vote = factor(december_2021_dec_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2021_dec_mrb = december_2021_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2021_dec_mrb = december_2021_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2021_dec_mrb = december_2021_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2021_dec_mrb = december_2021_dec_mrb %>% rename(low=`@dhm126`:`@dhm129`,middle_low=`@dhm130`:`@dhm134`,middle_high=`@dhm135`:`@dhm139`,high=`@dhm140`:`@dhm144`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2021_dec_mrb = december_2021_dec_mrb %>% rename(prof_status_self=c(`@dhm65`,`@dhm73`),
                                                           prof_status_emp = c(`@dhm66`,`@dhm67`),
                                                           prof_status_not = c(`@dhm68`,`@dhm71`,`@dhm72`,`@dhm74`),
                                                           prof_status_unemp = c(`@dhm69`,`@dhm70`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2021_dec_mrb = december_2021_dec_mrb %>% rename(com_big=c(`@dhm2`,`@dhm3`,`@dhm4`),
                                                           com_sm = `@dhm5`,
                                                           com_rur=`@dhm6`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2021_dec_mrb = december_2021_dec_mrb %>% mutate(eu=if_else(a98a3==1|a98a3==2,"ec_bad",if_else(a98a3==4|a98a3==5,"ec_good",if_else(a98a3==3,"ec_neither","Not"))))
december_2021_dec_mrb$eu=if_else(is.na(december_2021_dec_mrb$eu),"None",december_2021_dec_mrb$eu)
december_2021_dec_mrb = december_2021_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2021_dec_mrb = december_2021_dec_mrb %>% rename(region_north=c(`@dhm8`,`@dhm10`,`@dhm14`),
                                                           region_center=c(`@dhm7`,`@dhm9`,`@dhm11`,`@dhm12`,`@dhm15`,`@dhm17`),
                                                           region_crete=c(`@dhm13`,`@dhm16`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2021_dec_mrb = december_2021_dec_mrb %>% rename(decade10=`@dhm20`,decade9=`@dhm21`,decade8=`@dhm22`,decade7=`@dhm23`,decade6=`@dhm24`,decade5=`@dhm25`)



december_2021_dec_mrb = december_2021_dec_mrb %>% rename(up_to_fifteen=c(`@dhm46`,`@dhm47`),
                                                           sixteen_nineteen=`@dhm48`,
                                                           twenty_plus=c(`@dhm51`,`@dhm52`,`@dhm53`),
                                                           still_studying=c(`@dhm49`,`@dhm50`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                           twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                           still_studying=still_studying1+still_studying2)





december_2021_dec_mrb = december_2021_dec_mrb %>% mutate(year=2021)
december_2021_dec_mrb = december_2021_dec_mrb %>% rename(gender=`@dhm18`)







###JUNE 2022
december_2022_june_mrb = read_sav("ΤΑΣΕΙΣ ΙΟΥΝΙΟΣ 2022.sav")
december_2022_june_mrb = december_2022_june_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2022_june_mrb = december_2022_june_mrb %>% mutate(anti_usa=if_else(a80b2==1|a80b2==2,1,if_else(a80b2==4|a80b2==5,0,NA)))
december_2022_june_mrb = december_2022_june_mrb %>% mutate(anti_rus=if_else(a80b13==1|a80b13==2,1,if_else(a80b13==4|a80b13==5,0,NA)))
december_2022_june_mrb=december_2022_june_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:14),5,if_else(k3>=15,6,k3)))
december_2022_june_mrb$past_vote = factor(december_2022_june_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2022_june_mrb = december_2022_june_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2022_june_mrb = december_2022_june_mrb %>% mutate(left_right = if_else(k1 %in% c(2:4),"left",if_else(k1%in%c(5:8),"center",if_else(k1%in%c(9:11),"right",NA))))
december_2022_june_mrb = december_2022_june_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2022_june_mrb = december_2022_june_mrb %>% rename(low=`@dhm125`:`@dhm128`,middle_low=`@dhm129`:`@dhm133`,middle_high=`@dhm134`:`@dhm138`,high=`@dhm139`:`@dhm143`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2022_june_mrb = december_2022_june_mrb %>% rename(prof_status_self=c(`@dhm64`,`@dhm72`),
                                                         prof_status_emp = c(`@dhm65`,`@dhm66`),
                                                         prof_status_not = c(`@dhm67`,`@dhm70`,`@dhm71`,`@dhm73`),
                                                         prof_status_unemp = c(`@dhm68`,`@dhm69`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2022_june_mrb = december_2022_june_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2022_june_mrb = december_2022_june_mrb %>% mutate(eu=if_else(a80b3==1|a80b3==2,"ec_bad",if_else(a80b3==4|a80b3==5,"ec_good",if_else(a80b3==3,"ec_neither","Not"))))
december_2022_june_mrb$eu=if_else(is.na(december_2022_june_mrb$eu),"None",december_2022_june_mrb$eu)
december_2022_june_mrb = december_2022_june_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2022_june_mrb = december_2022_june_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2022_june_mrb = december_2022_june_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2022_june_mrb = december_2022_june_mrb %>% rename(up_to_fifteen=c(`@dhm45`,`@dhm46`),
                                                         sixteen_nineteen=`@dhm47`,
                                                         twenty_plus=c(`@dhm50`,`@dhm51`,`@dhm52`),
                                                         still_studying=c(`@dhm48`,`@dhm49`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2022_june_mrb = december_2022_june_mrb %>% mutate(year=2022)
december_2022_june_mrb = december_2022_june_mrb %>% rename(gender=`@dhm17`)








###DECEMBER 2022
december_2022_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2022.sav")
december_2022_dec_mrb = december_2022_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2022_dec_mrb = december_2022_dec_mrb %>% mutate(anti_usa=if_else(a92a2==1|a92a2==2,1,if_else(a92a2==4|a92a2==5,0,NA)))
december_2022_dec_mrb = december_2022_dec_mrb %>% mutate(anti_rus=if_else(a92a13==1|a92a13==2,1,if_else(a92a13==4|a92a13==5,0,NA)))
december_2022_dec_mrb=december_2022_dec_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:14),5,if_else(k3>=15,6,k3)))
december_2022_dec_mrb$past_vote = factor(december_2022_dec_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2022_dec_mrb = december_2022_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2022_dec_mrb = december_2022_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2022_dec_mrb = december_2022_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2022_dec_mrb = december_2022_dec_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2022_dec_mrb = december_2022_dec_mrb %>%  rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                          prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                          prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                          prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2022_dec_mrb = december_2022_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                           com_sm = `@dhm4`,
                                                           com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2022_dec_mrb = december_2022_dec_mrb %>% mutate(eu=if_else(a92a3==1|a92a3==2,"ec_bad",if_else(a92a3==4|a92a3==5,"ec_good",if_else(a92a3==3,"ec_neither","Not"))))
december_2022_dec_mrb$eu=if_else(is.na(december_2022_dec_mrb$eu),"None",december_2022_dec_mrb$eu)
december_2022_dec_mrb = december_2022_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2022_dec_mrb = december_2022_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                           region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                           region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2022_dec_mrb = december_2022_dec_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2022_dec_mrb = december_2022_dec_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2022_dec_mrb = december_2022_dec_mrb %>% mutate(year=2022)
december_2022_dec_mrb = december_2022_dec_mrb %>% rename(gender=`@dhm17`)








###DECEMBER 2023
december_2023_dec_mrb = read_sav("ΤΑΣΕΙΣ ΔΕΚΕΜΒΡΙΟΣ 2023.sav")
december_2023_dec_mrb = december_2023_dec_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2023_dec_mrb = december_2023_dec_mrb %>% mutate(anti_usa=if_else(a77b2==1|a77b2==2,1,if_else(a77b2==4|a77b2==5,0,NA)))
december_2023_dec_mrb = december_2023_dec_mrb %>% mutate(anti_rus=if_else(a77b13==1|a77b13==2,1,if_else(a77b13==4|a77b13==5,0,NA)))
december_2023_dec_mrb=december_2023_dec_mrb %>%mutate(past_vote = if_else(k3 %in% c(5:12),5,if_else(k3>=13,6,k3)))
december_2023_dec_mrb$past_vote = factor(december_2023_dec_mrb$past_vote, levels = c(1:6), labels = c("nd","syriza","pasok","kke","others","invalid"))

december_2023_dec_mrb = december_2023_dec_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2023_dec_mrb = december_2023_dec_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2023_dec_mrb = december_2023_dec_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2023_dec_mrb = december_2023_dec_mrb %>% rename(low=`@dhm125`:`@dhm128`,middle_low=`@dhm129`:`@dhm133`,middle_high=`@dhm134`:`@dhm138`,high=`@dhm139`:`@dhm143`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2023_dec_mrb = december_2023_dec_mrb %>%  rename(prof_status_self=c(`@dhm64`,`@dhm72`),
                                                          prof_status_emp = c(`@dhm65`,`@dhm66`),
                                                          prof_status_not = c(`@dhm67`,`@dhm70`,`@dhm71`,`@dhm73`),
                                                          prof_status_unemp = c(`@dhm68`,`@dhm69`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2023_dec_mrb = december_2023_dec_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2023_dec_mrb = december_2023_dec_mrb %>% mutate(eu=if_else(a77b3==1|a77b3==2,"ec_bad",if_else(a77b3==4|a77b3==5,"ec_good",if_else(a77b3==3,"ec_neither","Not"))))
december_2023_dec_mrb$eu=if_else(is.na(december_2023_dec_mrb$eu),"None",december_2023_dec_mrb$eu)
december_2023_dec_mrb = december_2023_dec_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2023_dec_mrb = december_2023_dec_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2023_dec_mrb = december_2023_dec_mrb %>% rename(decade10=`@dhm19`,decade9=`@dhm20`,decade8=`@dhm21`,decade7=`@dhm22`,decade6=`@dhm23`,decade5=`@dhm24`)



december_2023_dec_mrb = december_2023_dec_mrb %>% rename(up_to_fifteen=c(`@dhm45`,`@dhm46`),
                                                         sixteen_nineteen=`@dhm47`,
                                                         twenty_plus=c(`@dhm50`,`@dhm51`,`@dhm52`),
                                                         still_studying=c(`@dhm48`,`@dhm49`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2023_dec_mrb = december_2023_dec_mrb %>% mutate(year=2023)
december_2023_dec_mrb = december_2023_dec_mrb %>% rename(gender=`@dhm17`)









###FEBRUARY 2016
december_2016_feb_mrb = read_sav("ΤΑΣΕΙΣ ΦΕΒΡΟΥΑΡΙΟΣ 2016.sav")
december_2016_feb_mrb = december_2016_feb_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2016_feb_mrb = december_2016_feb_mrb %>% mutate(anti_usa=if_else(a61a1==1|a61a1==2,1,if_else(a61a1==4|a61a1==5,0,NA)))
december_2016_feb_mrb=december_2016_feb_mrb %>%mutate(past_vote = if_else(k3 %in% c(3,6:12),3,if_else(k3>=13,6,k3)))
december_2016_feb_mrb$past_vote = factor(december_2016_feb_mrb$past_vote, levels = c(1:6), labels = c("syriza","nd","others","pasok","kke","invalid"))

december_2016_feb_mrb = december_2016_feb_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2016_feb_mrb = december_2016_feb_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2016_feb_mrb = december_2016_feb_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2016_feb_mrb = december_2016_feb_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2016_feb_mrb = december_2016_feb_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                         prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                         prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                         prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2016_feb_mrb = december_2016_feb_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                         com_sm = `@dhm4`,
                                                         com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2016_feb_mrb = december_2016_feb_mrb %>% mutate(eu=if_else(a61a2==1|a61a2==2,"ec_bad",if_else(a61a2==4|a61a2==5,"ec_good",if_else(a61a2==3,"ec_neither","Not"))))
december_2016_feb_mrb$eu=if_else(is.na(december_2016_feb_mrb$eu),"None",december_2016_feb_mrb$eu)
december_2016_feb_mrb = december_2016_feb_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2016_feb_mrb = december_2016_feb_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                         region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                         region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2016_feb_mrb = december_2016_feb_mrb %>% rename(decade9=`@dhm19`,decade8=`@dhm20`,decade7=`@dhm21`,decade6=`@dhm22`,decade5=`@dhm23`,decade4=`@dhm24`)



december_2016_feb_mrb = december_2016_feb_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                         sixteen_nineteen=`@dhm35`,
                                                         twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                         still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                         twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                         still_studying=still_studying1+still_studying2)





december_2016_feb_mrb = december_2016_feb_mrb %>% mutate(year=2016)
december_2016_feb_mrb = december_2016_feb_mrb %>% rename(gender=`@dhm17`)













###JUNE 2007
december_2007_nov_mrb = read_sav("ΤΑΣΕΙΣ ΝΟΕΜΒΡΙΟΣ 2007.sav")
december_2007_nov_mrb = december_2007_nov_mrb  %>% mutate(life=if_else(a3==1|a3==2,1,if_else(a3==4|a3==5,0,NA)))
december_2007_nov_mrb = december_2007_nov_mrb %>% mutate(anti_usa=if_else(a82a1==1|a82a1==2,1,if_else(a82a1==4|a82a1==5,0,NA)))
december_2007_nov_mrb=december_2007_nov_mrb %>%mutate(past_vote = if_else(k3>=5&k3<=7,5,if_else(k3>=8,6,k3)))
december_2007_nov_mrb$past_vote = factor(december_2007_nov_mrb$past_vote, levels = c(1:6), labels = c("nd","pasok","kke","syriza","others","invalid"))

december_2007_nov_mrb = december_2007_nov_mrb %>% pivot_wider(names_from = past_vote,values_from = past_vote,values_fn = length, values_fill = 0)
december_2007_nov_mrb = december_2007_nov_mrb %>% mutate(left_right = if_else(k1 %in% c(1:3),"left",if_else(k1%in%c(4:7),"center",if_else(k1%in%c(8:10),"right",NA))))
december_2007_nov_mrb = december_2007_nov_mrb %>% pivot_wider(names_from = left_right,values_from = left_right,values_fn = length, values_fill = 0)

december_2007_nov_mrb = december_2007_nov_mrb %>% rename(low=`@dhm113`:`@dhm116`,middle_low=`@dhm117`:`@dhm121`,middle_high=`@dhm122`:`@dhm126`,high=`@dhm127`:`@dhm131`) %>%
  mutate(low=low1+low2+low3+low4,
         middle_low=middle_low1+middle_low2+middle_low3+middle_low4+middle_low5,
         middle_high = middle_high1+middle_high2+middle_high3+middle_high4+middle_high5,
         high = high1+high2+high3+high4+high5)


december_2007_nov_mrb = december_2007_nov_mrb %>% rename(prof_status_self=c(`@dhm52`,`@dhm60`),
                                                 prof_status_emp = c(`@dhm53`,`@dhm54`),
                                                 prof_status_not = c(`@dhm55`,`@dhm58`,`@dhm59`,`@dhm61`),
                                                 prof_status_unemp = c(`@dhm56`,`@dhm57`)) %>%
  mutate(prof_status_self = prof_status_self1+prof_status_self2,
         prof_status_emp = prof_status_emp1+prof_status_emp2,
         prof_status_not = prof_status_not1+prof_status_not2+prof_status_not3+prof_status_not4,
         prof_status_unemp = prof_status_unemp1+prof_status_unemp2)


december_2007_nov_mrb = december_2007_nov_mrb %>% rename(com_big=c(`@dhm1`,`@dhm2`,`@dhm3`),
                                                 com_sm = `@dhm4`,
                                                 com_rur=`@dhm5`) %>% 
  mutate(com_big=com_big1+com_big2+com_big3) 


december_2007_nov_mrb = december_2007_nov_mrb %>% mutate(eu=if_else(a82a2==1|a82a2==2,"ec_bad",if_else(a82a2==4|a82a2==5,"ec_good",if_else(a82a2==3,"ec_neither","Not"))))
december_2007_nov_mrb = december_2007_nov_mrb %>% pivot_wider(names_from = eu,values_from = eu,values_fn = length, values_fill = 0)

december_2007_nov_mrb = december_2007_nov_mrb %>% rename(region_north=c(`@dhm7`,`@dhm9`,`@dhm13`),
                                                 region_center=c(`@dhm6`,`@dhm8`,`@dhm10`,`@dhm11`,`@dhm14`,`@dhm16`),
                                                 region_crete=c(`@dhm12`,`@dhm15`))%>% 
  mutate(region_north=region_north1+region_north2+region_north3,
         region_center=region_center1+region_center2+region_center3+region_center4+region_center5+region_center6,
         region_crete=region_crete1+region_crete2)



december_2007_nov_mrb = december_2007_nov_mrb %>% rename(decade8=`@dhm19`,decade7=`@dhm20`,decade6=`@dhm21`,decade5=`@dhm22`,decade4=`@dhm23`,decade3=`@dhm24`)



december_2007_nov_mrb = december_2007_nov_mrb %>% rename(up_to_fifteen=c(`@dhm33`,`@dhm34`),
                                                 sixteen_nineteen=`@dhm35`,
                                                 twenty_plus=c(`@dhm38`,`@dhm39`,`@dhm40`),
                                                 still_studying=c(`@dhm36`,`@dhm37`)) %>% mutate(up_to_fifteen=up_to_fifteen1+up_to_fifteen2,
                                                                                                 twenty_plus= twenty_plus1+twenty_plus2+twenty_plus3,
                                                                                                 still_studying=still_studying1+still_studying2)





december_2007_nov_mrb = december_2007_nov_mrb %>% mutate(year=2007)
december_2007_nov_mrb = december_2007_nov_mrb %>% rename(gender=`@dhm17`)






new = bind_rows(data,december_2007_nov_mrb,december_2008_june_mrb,december_2008_mrb,december_2009_mrb,december_2010_dec_mrb,december_2010_june_mrb,
                december_2011_dec_mrb,december_2011_june_mrb,december_2012_dec_mrb,december_2013_dec_mrb,december_2013_june_mrb,december_2014_dec_mrb,
                december_2014_june_mrb,december_2016_dec_mrb,december_2016_feb_mrb,december_2016_june_mrb,december_2017_dec_mrb,december_2017_june_mrb,
                december_2018_dec_mrb,december_2018_june_mrb,december_2019_dec_mrb,december_2020_june_mrb,december_2020_nov_mrb,december_2021_dec_mrb,
                december_2021_june_mrb,december_2022_dec_mrb,december_2022_june_mrb,december_2023_dec_mrb)





new = new %>% select(up_to_fifteen,sixteen_nineteen,twenty_plus,still_studying,gender,life,anti_usa,anti_rus,pasok,nd,kke,syriza,others,left,center,right,low,middle_low,middle_high,high,prof_status_not,prof_status_self,
                     prof_status_emp,prof_status_unemp,com_rur,com_sm,com_big,ec_good,ec_bad,ec_neither,region_center,region_north,region_crete,
                     decade1,decade2,decade3,decade4,decade5,decade6,decade7,decade8,decade9,decade10,year)








#new %>% write_csv("Eurobarometer_MRB_Data.csv")


