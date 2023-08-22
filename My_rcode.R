library(haven)
library(tidyverse)
library(survey)
library(rpart)
library(rpart.plot)
library(partykit)
library(party)
library(caret)
HH_SEC_9A2_Q1Q2Q3Q4 <- read_dta("HH_SEC_9A2_Q1Q2Q3Q4.dta")
poverty_indicators2016 <- read_dta("poverty_indicators2016.dta")
HH_SEC_1A_Q1Q2Q3Q4 <- read_dta("~/Desktop/thesis_mahbub/Dataset/HH_SEC_1A_Q1Q2Q3Q4.dta")

Diversity <- HH_SEC_9A2_Q1Q2Q3Q4
Food_item <- Diversity %>% count(FOOD_ITEM)
Diversity <- Diversity %>% 
 mutate(G1 = case_when(FOOD_ITEM %in% c("Bread/ Bonroti","Cake",
                                        "Flour","Pop rice","Rice-Fine",
                                        "Rice - Medium","Rice - Coarse",
                                        "Ripe papaya","Beaten rice","Biscuits",
                                        "Puffed rice","Vermicelli/ Suji",
                                        " Wheat (Atta)")~1),
        G2 = case_when(FOOD_ITEM %in% c("Potato")~1),
        G3 = case_when(FOOD_ITEM %in% c("Bean/ Lobey","Cauliflower/ Cabbage",
                                        "Green banana/ Green papaya",
                                        "Ladies' finger","Perbol (Patal)",
                                        "Snake gourd/ Ribbed gourd","Water gourd",
                                        " White gourd/ Pumpkin",
                                        "(All types of leafy veg.(Spinach/ Amaranta/ Basil)",
                                        " Arum/ Ol-kachu/ Kachur-mukhi",
                                        " Balsam apple ","Brinjal","Radish")~1),
        G4 = case_when(FOOD_ITEM %in% c("Amra/Kamranga","Apple","Bedana",
                                        "Black berry","Jelly/ Jam","Orange",
                                        "Pineapple",
                                        "Sauce/Sirka","Tomato","Amshatta",
                                        " Grape","Guava","Jack fruit",
                                        "Leeches", "Mango",
                                        " Melon/ Bangi"," Palm","Pickles",
                                        " Ripe banana","Safeda")~1),
        G5 = case_when(FOOD_ITEM %in% c("Buffalo","Duck","Hen","Meat",
                                        "Sheep","Shrimp","Beef","Mutton")~1),
        G6 =  case_when(FOOD_ITEM %in% c("Duck egg" ,"Hen egg")~1),
        G7 =  case_when(FOOD_ITEM %in% c("Baila","Eelfish","Hilsa",
                                         "Mala-kachi/ Chala-chapila/Khalsha",
                                         "Other small fishes (with tangra)",
                                         "Puti/ Big Puti/ Telapia/ Nilotica",
                                         "Boal/ Air","Dried fish",
                                         "Fish", "koi","Magur/ Shing","Pangash",
                                         " Rhui/ Katla/ Mrigel/ Kali baush","sea fish",
                                         " Shoal/ Gajar/ Taki","Silver carp/ Grass carp/ Miror carp")~1),
        G8  = case_when(FOOD_ITEM %in% c("Green gram (boot)","Lentil (musur)",
                                         "Mashkalai"," Chickling-Vetch (mug)",
                                         "Pea gram (kheshari)")~1),
        G9  = case_when(FOOD_ITEM %in% c("Liquid milk","Milk drinks",
                                         "Powder milk","Casein (ponir)/ Butter","Curd")~1),
        G10 = case_when(FOOD_ITEM %in% c("Dalda/ Vanashpati","Ghee",
                                         "Mustard oil","Palm oil","Patties/Cake",
                                         "Samucha/Singara/Puri/Cake","Burger",
                                         " Fried chicken","Hotdog","Meals (Rice/Biriani)",
                                         " Pizza","Sandwich","Soybean oil")~1),
        G11 = case_when(FOOD_ITEM %in% c("Chocolate","Ice-cream","Khaja/ Logenze/ Toffee",
                                         "Rasogolla/ Chamcham/ Shandash","Sugar/ Misri"," Halua/ Batasha/ Kadma",
                                         "Jilapi/ Bundia/ Amriti",
                                         " Molasses (Sugarcane/ Date/ Palm) ")~1)) 






Food <- Diversity %>% group_by(day,hhold) %>%  
  summarise(Group_1= mean(G1,na.rm=T),
                                                      Group_2= mean(G2,na.rm=T),
                                                      Group_3= mean(G3,na.rm=T),
                                                      Group_4= mean(G4,na.rm=T),
                                                      Group_5= mean(G5,na.rm=T),
                                                      Group_6= mean(G6,na.rm=T),
                                                      Group_7= mean(G7,na.rm=T),
                                                      Group_8= mean(G8,na.rm=T),
                                                      Group_9= mean(G9,na.rm=T),
                                                      Group_10= mean(G10,na.rm=T),
                                                      Group_11= mean(G11,na.rm=T)) %>%
  arrange(day) %>% 
  arrange(hhold) %>%
  ungroup() %>% 
  mutate(Group_1=replace_na(Group_1,0),
         Group_2=replace_na(Group_2,0),
         Group_3=replace_na(Group_3,0),
         Group_4=replace_na(Group_4,0),
         Group_5=replace_na(Group_5,0),
         Group_6=replace_na(Group_6,0),
         Group_7=replace_na(Group_7,0),
         Group_8=replace_na(Group_8,0),
         Group_9=replace_na(Group_9,0),
         Group_10=replace_na(Group_10,0),
         Group_11=replace_na(Group_11,0)) %>% 
  mutate(Food_diversity=Group_1+Group_2+Group_3+Group_4+Group_5+
                            Group_6+Group_7+Group_8+Group_9+Group_10+Group_11 )




Food_diversity <- Food %>% 
  select(hhold,Food_diversity) %>% 
  group_by(hhold) %>% 
  summarise(Food_diversity=mean(Food_diversity))



Food_cosumption_per_capatia <- Diversity %>%
  filter(FOOD_ITEM!="") %>%
  select(id,day,FOOD_ITEM, s9a2q02,hhold, id_01_name,p1,wgt,hhwgt) %>%
  distinct() %>% 
  pivot_wider(names_from = FOOD_ITEM, 
              values_from = s9a2q02,values_fn = list) %>% 
  unnest(cols = everything() ) %>% 
  arrange(day) %>% 
  arrange(id) %>% 
  mutate_at(c(8:125), ~replace_na(.,0)) 






  
 Energy <-  function(dat){
   dat1 = dat %>%  
     mutate( Energy = `Soybean oil`*8.84+   Potato*0.7+ Beef*1.5+`Balsam apple`*0.4+  
               `Perbol (Patal)`*0.2+  `Molasses (Sugarcane/ Date/ Palm)` *3.5+ `Rice - Coarse` *3.5+
               `Green gram (boot)`*3.5+`Wheat (Atta)`*3.34+`Other small fishes (with tangra)`*2.5+
               Tea*0.002+`Mustard oil`*8.84+Biscuits*0.004+Shrimp*1.07+Brinjal*0.2+`Duck egg`*135+
               `Ripe banana`*0.89+Pickles*0.11+`Liquid milk`*0.5+Chocolate*1.44+
               `Rhui/ Katla/ Mrigel/ Kali baush`*2.5+
               `Khaja/ Logenze/ Toffee`*4.5+Pangash*2.5+
               `Silver carp/ Grass carp/ Miror carp`*2.5+`Water gourd`*0.1+ 
               Fish*2.5+`Jilapi/ Bundia/ Amriti`*4+`Melon/ Bangi`*0.34+
               `Chickling-Vetch (mug)`*0.89+Meat*2.5+
               `Ripe papaya`*0.46 + 3.5*`Mala-kachi/ Chala-chapila/Khalsha`+
               `Soft drinks/bottle water`*0.4+`Bread/ Bonroti`*2.4 +
               Safeda*0.94 +  Pineapple*0.5 +
               `Fried chicken`*3.5 + `Tobacco leaf`*0+
               `Powder milk`*5.5+`Pea gram (kheshari)`*3.5 +
                `Soft drinks(peepsi/RC/Mojo/Coke, Sherbat, etc.` *0.004+
               `Black berry`*0.43+Leeches*0.0066+`Patties/Cake`*4+
               Duck*2.91+Bedana*0.63+`Palm oil`*8+Pizza*3+`Dried fish`*3.5+
               Radish*0.2+Burger*3+
               `Liquid (Ros) of Sugarcane/ Date/Palm`*0.0078+`Dalda/ Vanashpati`*7+
               `Milk drinks`*0.7+
               `Halua/ Batasha/ Kadma`*3.5+`Casein (ponir)/ Butter`*2.7+Sheep*1.17+`Ladies' finger`*0.3+
               `Gul and Other (specify)`*0+`Lentil (musur)`*3.5+
               `White gourd/ Pumpkin`*0.2+`Ice-cream`*1.49+
               `Beaten rice`*3.5+`(All types of leafy veg.(Spinach/ Amaranta/ Basil)`*0.23+
               `Hen egg`*75+`Puffed rice`*3.5
             +`Samucha/Singara/Puri/Cake`*2+`Rasogolla/ Chamcham/ Shandash`*3+
               koi*2.5+`Green banana/ Green papaya`*0.89+
               Mango*0.4+Bidies*0+`Puti/ Big Puti/ Telapia/ Nilotica`*3.5+
               `Arum/ Ol-kachu/ Kachur-mukhi`*1.2+
               `Rice - Medium`*3.5+ `Sugar/ Misri`*4+`Rice - Fine`*3.5+
               `Tea/ Coffee leaf`*0.005+Hen*1.5+`Bean/ Lobey`*0.67+
               `sea fish`*1.5+Cake*4+Grape*0.69+ `Shoal/ Gajar/ Taki`*2.5+Hilsa*2.5+
               Guava*1.4+`Snake gourd/ Ribbed gourd`*0.2
             +`Magur/ Shing`*2.5+`Pop rice`*3.5+Baila*3.5+`Meals (Rice/Biriani)`*1.4+
               `Amra/Kamranga`*0.31+Tomato*0.2+ 
               `Jack fruit`*0.5+Flour*3.4+Mutton*2.63+Apple*0.52+Coffee*0.0005+
               `Ovaltine/ Horlicks`*0.123+Palm*7.2+`Boal/ Air`*2.5+
               `Vermicelli/ Suji`*3.5+Amshatta*0.67+`Green coconut water`*0.0002+
               Eelfish*6.5+Orange*0.52+Curd*0.7+`Jelly/ Jam`*2.68+
               Mashkalai*3.5+Ghee*7.5+`Cauliflower/ Cabbage`*0.25+Hotdog*3.04+
               Buffalo*0.77+Sandwich*3+`Sauce/Sirka`*2.7
             
              )
   
   return(dat1)
 }
   
  
 
 
 dat2 <- Food_cosumption_per_capatia %>% 
   Energy(.) 
 
 dat_count<- HH_SEC_1A_Q1Q2Q3Q4 %>% 
   count(hhold) 
 
 Final_data_Kin<- merge(dat_count, dat2, by="hhold")  %>% 
   group_by(hhold,id,id_01_name,p1,wgt,hhwgt) %>% 
   summarise(`Energy/day` = mean(Energy), Member= mean(n),
             K_in_c_d= `Energy/day`/Member) %>% 
   ungroup() %>% 
   select(hhold,K_in_c_d)

  
 
 Final_data_perct <- poverty_indicators2016 %>% 
   mutate(percent=(fexp/(fexp+nfexp))*100) %>% 
   select(hhold,percent,hhwgt,psu,stratum16,quarter,popwgt,urbrural,id_01_name)
 
 sexhh_1 <-  read_dta("~/Desktop/thesis_mahbub/Dataset/HH_SEC_1A_Q1Q2Q3Q4.dta")
 
 
 
sex_age <- sexhh_1 %>% 
   mutate(age_group = case_when(s1aq03<= 15~"1_15",
                                s1aq03 >15 & s1aq03 <50 ~"15_49",
                                s1aq03>49 ~"GE50"),
          Gender= case_when(s1aq01==1~"M",s1aq01==2~"F")) %>% 
  group_by(hhold, Gender) %>% 
  count(age_group) %>% 
  ungroup() %>% 
  mutate(indicator =paste(Gender,age_group, sep = "_")) %>% 
  select(c(hhold,indicator,n)) %>%
  pivot_wider(names_from = indicator,values_from = n) %>% 
  select(-c(M_NA,NA_NA)) %>%  
  mutate_at(c(2:7), ~replace_na(.,0)) 

  
sexhh <- sexhh_1 %>% 
  select(hhold, s1aq02,s1aq01,s1aq03) %>% 
  filter(s1aq02==1) %>% 
  mutate(Sex_hh=case_when(s1aq01==1~"Male",
                          s1aq01==2~"Female"),
         Age_hh=s1aq03) %>% 
  select(hhold,Sex_hh,Age_hh)





 
education1 <- read_dta("~/Desktop/thesis_mahbub/Dataset/HH_SEC_8C_Q1Q2Q3Q4.dta")
 
education <- education1 %>%
  select(hhold,s8cq11) 
  

wall_elc1 <- read_dta("~/Desktop/thesis_mahbub/Dataset/HH_SEC_6A_Q1Q2Q3Q4.dta")
wall_elc <- wall_elc1 %>% 
  select(hhold,s6aq07,s6aq17) %>% 
  mutate(brick=case_when(s6aq07==5~"1"),
         mud=case_when(s6aq07==2~"1"),
         wood=case_when(s6aq07==4~"1"),
         electricity= case_when(s6aq17==1~"1")) %>% 
  select(hhold,brick,mud,wood,electricity)%>%  
  mutate_at(c(2:5), ~replace_na(.,0)) 


land <- read_dta("~/Desktop/thesis_mahbub/Dataset/HH_SEC_7A_Q1Q2Q3Q4.dta")
land <- land %>% 
  select(hhold,s7aq06)

price_rice1 <- HH_SEC_9A2_Q1Q2Q3Q4

price_rice <- price_rice1 %>% 
  mutate(cost=s9a2q04) %>% 
  group_by(hhold,FOOD_ITEM,day) %>% 
  summarise(cost=mean(cost,na.rm=T)) %>% 
  filter(grepl('Rice', FOOD_ITEM)) %>% 
  ungroup() %>% 
  group_by(hhold) %>% 
  summarise(cost=sum(cost)/14)



emloyment1 <- read_dta("~/Desktop/thesis_mahbub/Dataset/HH_SEC_4A_Q1Q2Q3Q4.dta")
employment <- emloyment1 %>% 
  filter(s4aq00==1) %>% 
  select(s4aq07,hhold,s4aq08) %>% 
  mutate(agri=case_when(s4aq07==2~"1"),
         non_agri=case_when(s4aq08==2~"1"),
         daylobour=case_when(s4aq08==1~"1"),
         salarywage=case_when(s4aq08==4~"1")) %>% 
  select(hhold,agri,non_agri,daylobour,salarywage) %>% 
  mutate_at(c(2:5), ~replace_na(.,0)) 



safety1 <- read_dta("~/Desktop/thesis_mahbub/Dataset/HH_SEC_1c_Q1Q2Q3Q4.dta")
safety <- safety1 %>% 
  filter(s1cq00==1) %>% 
  select(hhold,s1cq01) %>% 
  mutate(safety_nate=case_when(s1cq01==1~"1")) %>% 
  select(hhold,safety_nate) %>% 
  mutate_at(2, ~replace_na(.,0)) 

Final_data1 <- list(Food_diversity,Final_data_Kin,
                    Final_data_perct,sex_age,sexhh,price_rice,
                    land,wall_elc, employment,safety)      

Final_data<- Final_data1 %>% 
  reduce(inner_join, by='hhold',all=FALSE) 




Final_data <- Final_data %>% 
  mutate(Food_insecrity = if_else(Food_diversity <5 &
                                   K_in_c_d<2225 & percent>45, 1,0),
        div_indi = if_else(Food_diversity< 5,1,0), K_in = if_else(K_in_c_d<2225,1,0),
        per_in = if_else( percent > 45, 1, 0)) %>% 
  select(-c(id_01_name,popwgt,quarter,stratum16))
  

Final_data$brick<- as.factor(Final_data$brick)
Final_data$mud <- as.factor(Final_data$mud)
Final_data$wood <- as.factor(Final_data$wood)
Final_data$electricity <- as.factor(Final_data$electricity)
Final_data$Sex_hh <- as.factor(Final_data$Sex_hh)
Final_data$agri <- as.factor(Final_data$agri)
Final_data$non_agri <- as.factor(Final_data$non_agri)
Final_data$daylobour <- as.factor(Final_data$daylobour)
Final_data$salarywage <- as.factor(Final_data$salarywage)
Final_data$salarywage <- as.factor(Final_data$salarywage)
Final_data$safety_nate <- as.factor(Final_data$safety_nate)



write.csv(Final_data,file = "final.csv",row.names = F,na="")

Final_data <- read_csv("final.csv")
Final_data <- na.omit(Final_data)



train.des1 <- svydesign(id=~hhold, weights = ~hhwgt, 
                        strata = ~psu,nest = T, data = Final_data)

food_ins <- survey::svyglm(formula = Food_insecrity ~ urbrural+ F_15_49+M_1_15+
                                    M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                                    electricity+agri+non_agri+daylobour+salarywage+safety_nate
                                    ,design = train.des1, 
                                  na.action = na.omit, family = quasibinomial)  

table<-broom::tidy(food_ins,exp=T,
                   confint.default(object =food_ins))  
write.csv(table,file = "food_ins.csv")


div_ind <- survey::svyglm(formula = div_indi ~ urbrural+ F_15_49+M_1_15+
                             M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                             electricity+agri+non_agri+daylobour+salarywage+safety_nate
                           ,design = train.des1, 
                           na.action = na.omit, family = quasibinomial)  

table2<-broom::tidy(div_ind,exp=T,
                   confint.default(object =div_ind))  
write.csv(table2,file = "div_ind.csv")

div_LR <- survey::svyglm(formula = Food_diversity ~ urbrural+ F_15_49+M_1_15+
                            M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                            electricity+agri+non_agri+daylobour+salarywage+safety_nate
                          ,design = train.des1, 
                          na.action = na.omit)  
table<-broom::tidy(div_LR,
                   confint.default(object =div_LR)) 
write.csv(table,file = "div_LR.csv")

K_in <- survey::svyglm(formula = K_in ~ urbrural+ F_15_49+M_1_15+
                            M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                            electricity+agri+non_agri+daylobour+salarywage+safety_nate
                          ,design = train.des1, 
                          na.action = na.omit, family = quasibinomial)


table3<-broom::tidy(K_in,exp=T,
                    confint.default(object =K_in))  
write.csv(table3,file = "K_in.csv")



K_LR <- survey::svyglm(formula = K_in_c_d ~ urbrural+ F_15_49+M_1_15+
                         M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                         electricity+agri+non_agri+daylobour+salarywage+safety_nate
                       ,design = train.des1, 
                       na.action = na.omit)  

table<-broom::tidy(K_LR,
                   confint.default(object =K_LR)) 
write.csv(table,file = "K_LR.csv")









per_in <- survey::svyglm(formula = per_in ~ urbrural+ F_15_49+M_1_15+
                         M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                         electricity+agri+non_agri+daylobour+salarywage+safety_nate
                       ,design = train.des1, 
                       na.action = na.omit, family = quasibinomial)  

table4<-broom::tidy(per_in,exp=T,
                    confint.default(object =per_in))  
write.csv(table4,file = "per_in.csv")


per_LR <- survey::svyglm(formula = percent ~ urbrural+ F_15_49+M_1_15+
                 M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                 electricity+agri+non_agri+daylobour+salarywage+safety_nate
               ,design = train.des1, 
               na.action = na.omit)  


table<-broom::tidy(per_LR,
                   confint.default(object =per_LR)) 
write.csv(table,file = "per_LR.csv")







control <- ctree_control(maxdepth = 4)
Final_data %>% 
  ctree(Food_insecrity ~ urbrural+ F_15_49+M_1_15+
          M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
          electricity+agri+non_agri+daylobour+salarywage+safety_nate, data = .,control = control) %>% plot()
Final_data %>% 
  ctree(div_indi ~ urbrural+ F_15_49+M_1_15+
          M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
          electricity+agri+non_agri+daylobour+salarywage+safety_nate, data = .,control = control) %>% plot()



Final_data %>% 
  ctree(Food_diversity ~ urbrural+ F_15_49+M_1_15+
          M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
          electricity+agri+non_agri+daylobour+salarywage+safety_nate, data = .,
       control = control) %>% plot()

Final_data %>% 
  ctree(K_in ~ urbrural+ F_15_49+M_1_15+
          M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
          electricity+agri+non_agri+daylobour+salarywage+safety_nate, data = .,
       control = control) %>% plot()
Final_data %>% 
  ctree(K_in_c_d ~ urbrural+ F_15_49+M_1_15+
          M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
          electricity+agri+non_agri+daylobour+salarywage+safety_nate, data = .,
        control = control) %>% plot()




Final_data %>% 
  ctree(per_in ~ urbrural+ F_15_49+M_1_15+
          M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
          electricity+agri+non_agri+daylobour+salarywage+safety_nate, data = .,
       control = control) %>% plot()



Final_data %>% 
  ctree(percent~ urbrural+ F_15_49+M_1_15+
          M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
          electricity+agri+non_agri+daylobour+salarywage+safety_nate, data = .,
       control = control) %>% plot()







library(randomForest)

modper<-randomForest(formula=per_in ~urbrural+ F_15_49+M_1_15+
                       M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                       electricity+agri+non_agri+daylobour+salarywage+safety_nate,data=Final_data,mtry=4,
                   ntree=500,na.action = na.omit)

feat_imp_df <- varImp(modper) %>%
  data.frame() %>%
  mutate(feature = row.names(.))
rownames(feat_imp_df)<-NULL

ggplot(feat_imp_df, aes(x = reorder(feature,Overall), y=Overall))+
  geom_point(aes(color=feature),show.legend=F)+geom_segment(aes(feature,xend=feature,y=0,yend=Overall,color=feature),
                            size = 5, lineend = "butt",show.legend = F)+
  coord_flip()+xlab("Variable")
  


modkin<-randomForest(formula=K_in ~urbrural+ F_15_49+M_1_15+
                       M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                       electricity+agri+non_agri+daylobour+salarywage+safety_nate,data=Final_data,mtry=4,
                     ntree=500,na.action = na.omit)

feat_imp_df <- varImp(modkin) %>%
  data.frame() %>%
  mutate(feature = row.names(.))

ggplot(feat_imp_df, aes(x = reorder(feature,Overall), y=Overall))+
  geom_point(aes(color=feature),show.legend=F)+geom_segment(aes(feature,xend=feature,y=0,yend=Overall,color=feature),
                                                            size = 5, lineend = "butt",show.legend = F)+
  coord_flip()+xlab("Variable")



moddivind<-randomForest(formula=div_indi ~urbrural+ F_15_49+M_1_15+
                       M_15_49+F_1_15+F_GE50+M_GE50+Sex_hh+Age_hh+cost+s7aq06+brick+mud+wood+
                       electricity+agri+non_agri+daylobour+salarywage+safety_nate,data=Final_data,mtry=4,
                     ntree=500,na.action = na.omit)

feat_imp_df <- varImp(moddivind) %>%
  data.frame() %>%
  mutate(feature = row.names(.))
ggplot(feat_imp_df, aes(x = reorder(feature,Overall), y=Overall))+
  geom_point(aes(color=feature),show.legend=F)+geom_segment(aes(feature,xend=feature,y=0,yend=Overall,color=feature),
                                                            size = 5, lineend = "butt",show.legend = F)+
  coord_flip()+xlab("Variable")









library(RCurl)
library(xrf)






final<-final %>% mutate(K_in = recode(as.factor(K_in), '1' = "Yes",'0'="No"),
                        div_indi = recode(as.factor(div_indi), '1' = "Yes",'0'="No"),
                        per_in= recode(as.factor(per_in), '1' = "Yes",'0'="No"))


FinaL_new<- final %>% select(K_in, urbrural, F_15_49, M_1_15,
                               M_15_49, F_1_15, F_GE50, M_GE50, Sex_hh, Age_hh, cost, s7aq06, brick, mud, wood,
                               electricity, agri, non_agri, daylobour, salarywage,safety_nate) 
  
Final <- FinaL_new[complete.cases(FinaL_new),]
m_xrf_Kin <- xrf(K_in ~ ., Final, family = 'binomial', 
             xgb_control = list(nrounds = 100, max_depth = 3))


coef_overlap <- coef(m_xrf_Kin, lambda = 'lambda.1se')
#coef_deoverlap <- coef(m_xrf_deoverlap, lambda = 'lambda.1se')
  
  coef_overlap %>%
  filter(coefficient_lambda.1se != 0) %>%
  arrange(rule)

FinaL_new<- final %>% select(div_indi, urbrural, F_15_49, M_1_15,
                             M_15_49, F_1_15, F_GE50, M_GE50, Sex_hh, Age_hh, cost, s7aq06, brick, mud, wood,
                             electricity, agri, non_agri, daylobour, salarywage,safety_nate) 

Final <- FinaL_new[complete.cases(FinaL_new),]
m_xrf_Kin <- xrf(div_indi ~ ., Final, family = 'binomial', 
                 xgb_control = list(nrounds = 100, max_depth = 3))


FinaL_new<- final %>% select(per_in, urbrural, F_15_49, M_1_15,
                             M_15_49, F_1_15, F_GE50, M_GE50, Sex_hh, Age_hh, cost, s7aq06, brick, mud, wood,
                             electricity, agri, non_agri, daylobour, salarywage,safety_nate) 

Final <- FinaL_new[complete.cases(FinaL_new),]
m_xrf_perin <- xrf(per_in ~ ., Final, family = 'binomial', 
                 xgb_control = list(nrounds = 100, max_depth = 3))





library(survey)

train.des <- svydesign(id=~hhold, weights = ~hhwgt, 
                        strata = ~psu,nest = T, data = Final_data)

library(gtsummary)

K_in_des <-train.des %>% 
  tbl_svysummary(by =K_in, percent = "row", include = c(urbrural, F_15_49, M_1_15,
                                                        M_15_49, F_1_15, F_GE50, M_GE50, Sex_hh, Age_hh, cost, s7aq06, brick, mud, wood,
                                                        electricity, agri, non_agri, daylobour, salarywage,safety_nate
  ))%>% 
  add_n() %>% 
  as_tibble()


per_in_des <-train.des %>% 
  tbl_svysummary(by =per_in, percent = "row", include = c(urbrural, F_15_49, M_1_15,
                                                        M_15_49, F_1_15, F_GE50, M_GE50, Sex_hh, Age_hh, cost, s7aq06, brick, mud, wood,
                                                        electricity, agri, non_agri, daylobour, salarywage,safety_nate
  ))%>% 
  add_n() %>% 
  as_tibble()

div_indi_des <-train.des %>% 
  tbl_svysummary(by =div_indi, percent = "row", include = c(urbrural, F_15_49, M_1_15,
                                                        M_15_49, F_1_15, F_GE50, M_GE50, Sex_hh, Age_hh, cost, s7aq06, brick, mud, wood,
                                                        electricity, agri, non_agri, daylobour, salarywage,safety_nate
  ))%>% 
  add_n() %>% 
  as_tibble()



