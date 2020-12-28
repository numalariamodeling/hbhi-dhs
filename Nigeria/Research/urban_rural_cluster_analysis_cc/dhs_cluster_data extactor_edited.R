rm(list=ls())

#important to download the github version of tidyverse.Uncomment the script below to run
#install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("r-pkgs/usethis")

#devtools::install_github("hadley/tidyverse")



# Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear")

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


dhs <- list.files(pattern = ".*NGPR7AFL.*\\.DTA", recursive = F, full.names = TRUE)
dhs <- sapply(dhs, read_dta, simplify = F)

dhs2 <- list.files(pattern = ".*NGKR7AFL.*\\.DTA", recursive = F, full.names = TRUE)
dhs2 <- sapply(dhs2, read_dta, simplify = F)
# clean and select pfpr data 

pfpr_data <- dhs[[1]] # uses the DHS person recode dataset 

look_for(dhs[[1]], "smear")

table(pfpr_data$hml32) # frequency table for smear test 

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_df <- pfpr_data %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6))





# prep dataset for cluster level analysis - we start with urban cluster analysis 

val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural

#pfpr_place <- pfpr_df %>% filter(hv025 == 1)
#pfpr_dhs <- pfpr_data%>% filter(hv025 == 1)
pfpr_place <- pfpr_df
pfpr_dhs <- pfpr_data
# estimate cluster-level malaria prevalence

pfpr_place<- funEnv$dataclean.para(pfpr_place, hv005, hc1, hml32, 'hml32', 'p_test') 

svy_mal <- funEnv$svydesign.fun(pfpr_place)

clu_est <- funEnv$result.fun('p_test', 'hv001', design=svy_mal, pfpr_place, "hv007")
head(clu_est)


# next we estimate proportion of people in high SES by cluster
# recode the weath quintile variable 
table(pfpr_df$hv270)

pfpr_wealth <- pfpr_df %>%  mutate(wealth = ifelse(hv270 <4, 0, 1))
table(pfpr_wealth$wealth)

pfpr_wealth<- funEnv$dataclean.para(pfpr_wealth, hv005, hv005, wealth, 'wealth', 'wealth_2') 
table(pfpr_wealth$wealth_2)

svyd_wealth<- funEnv$svydesign.fun(pfpr_wealth)

clu_wealth <- result.fun('wealth_2', 'hv001', design=svyd_wealth, pfpr_wealth, "hv007")
head(clu_wealth)


#disagreegate wealth 
look_for(dhs[[1]], "material")
table(pfpr_df$hv205)


#Housing quality
#Estimate the proportion of clusters with good floor quality 
pfpr_floor<- pfpr_df %>%  mutate(floor_type = ifelse(hv213 == 30| hv213 == 31|
                                                       hv213 == 33| hv213 == 34|
                                                       hv213 == 35,1, 0))
table(pfpr_floor$floor_type)

pfpr_floor<- funEnv$dataclean.para(pfpr_floor, hv005, hv005, floor_type, 'floor_type', 'house_floor') 
table(pfpr_floor$house_floor)

svyd_floor<- funEnv$svydesign.fun(pfpr_floor)

clu_floor <- result.fun('house_floor', 'hv001', design=svyd_floor, pfpr_floor, "hv007")
head(clu_floor)

#Estimate the proportion of clusters with good wall quality 
pfpr_wall<- pfpr_df %>%  mutate(wall_type = ifelse(hv214 == 30| hv214 == 31|
                                                     hv214 == 33| hv214 == 34|
                                                     hv214 == 35|hv214 == 36,1, 0))
table(pfpr_wall$wall_type)

pfpr_wall<- funEnv$dataclean.para(pfpr_wall, hv005, hv005, wall_type, 'wall_type', 'house_wall') 
table(pfpr_wall$house_wall)

svyd_wall<- funEnv$svydesign.fun(pfpr_wall)

clu_wall <- result.fun('house_wall', 'hv001', design=svyd_wall, pfpr_wall, "hv007")
head(clu_wall)


#Estimate the proportion of clusters with good roof quality 
pfpr_roof <- pfpr_df %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
                                                      hv215 == 33| hv215 == 34|
                                                      hv215 == 35|hv215 == 36,1, 0))
table(pfpr_roof$roof_type)

pfpr_roof<- funEnv$dataclean.para(pfpr_roof, hv005, hv005, roof_type, 'roof_type', 'house_roof') 
table(pfpr_roof$house_roof)

svyd_roof<- funEnv$svydesign.fun(pfpr_roof)

clu_roof <- result.fun('house_roof', 'hv001', design=svyd_roof, pfpr_roof, "hv007")
head(clu_roof)

#Estimate of the proportion of clusters with good housing quality
pfpr_housing<- pfpr_df %>%  mutate(floor_type = ifelse(hv214 == 30| hv214 == 31|
                                                         hv214 == 33| hv214 == 34|
                                                         hv214 == 35|hv214 == 36,1, 0))

pfpr_housing<- pfpr_housing %>%  mutate(wall_type = ifelse(hv214 == 30| hv214 == 31|
                                                             hv214 == 33| hv214 == 34|
                                                             hv214 == 35|hv214 == 36,1, 0))


pfpr_housing <- pfpr_housing %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
                                                              hv215 == 33| hv215 == 34|
                                                              hv215 == 35|hv215 == 36,1, 0))

pfpr_housing_q <- pfpr_housing %>%  mutate(housing_q = ifelse(floor_type == 1 &
                                                                wall_type == 1 &
                                                                roof_type == 1,1, 0))

table(pfpr_housing_q$housing_q)

pfpr_housing_q<- funEnv$dataclean.para(pfpr_housing_q, hv005, hv005, housing_q, 'housing_q', 'housing_qua') 
table(pfpr_housing_q$housing_qua)

svyd_housing_q<- funEnv$svydesign.fun(pfpr_housing_q)

clu_housing_q <- result.fun('housing_qua', 'hv001', design=svyd_housing_q, pfpr_housing_q, "hv007")
head(clu_housing_q)


# next estimate the proportion of U5 children in each cluster 

look_for(dhs[[1]], "age")
summary(is.na(pfpr_dhs$hc1))

pfpr_u5 <- pfpr_dhs %>%  mutate(u5_prop = ifelse(hml16 <=4, 1,0))
table(pfpr_u5$u5_prop)


pfpr_u5<- dataclean.para(pfpr_u5, hv005, hv005, u5_prop, 'u5_prop', 'u5_prop') 
table(pfpr_u5$u5_prop)


svyd_u5 <- svydesign.fun(pfpr_u5)

clu_u5_prop <- result.fun('u5_prop', 'hv001', design=svyd_u5, pfpr_u5, "hv007")
head(clu_u5_prop)

# next estimate the average age in each cluster 

look_for(dhs[[1]], "age")
summary(is.na(pfpr_dhs$hv105))

pfpr_hh_members_age<- dataclean.para(pfpr_data, hv005, hv005, hv105, 'hv105', 'hh_members_age') 
table(pfpr_hh_members_age$hh_members_age)

svyd_hh_members_age <- svydesign.fun(pfpr_hh_members_age)
table(pfpr_hh_members_age$hh_members_age)

clu_hh_members_age <- svyby(~hh_members_age, ~hv001, design = svyd_hh_members_age, FUN=svymean, ci=TRUE,vartype="ci")
head(clu_hh_members_age)

# next estimate the gender proportion in each cluster 
look_for(dhs[[1]], "sex")
table(dhs[[1]]$hc27)

pfpr_sex <- pfpr_dhs %>%  mutate(sex = ifelse(hc27 == 1,0, 1))
pfpr_sex <- dataclean.para(pfpr_sex, hv005, hv005, hc27, 'sex', 'sex_f') 
table(pfpr_sex$sex_f)


svyd_sex <- svydesign.fun(pfpr_sex)

clu_sex <- result.fun('sex_f', 'hv001', design=svyd_sex, pfpr_sex, "hv007")
head(clu_sex)

# estimate proportion of pregnant women in each cluster 
look_for(dhs[[1]], "pregnant")
table(dhs[[1]]$ha54)

pfpr_preg<- dataclean.para(pfpr_dhs, hv005, hv005, ha54, 'ha54', 'preg') 
table(pfpr_preg$preg)


svyd_preg <- svydesign.fun(pfpr_preg)

clu_preg <- result.fun('preg', 'hv001', design=svyd_preg, pfpr_preg, "hv007")
head(clu_preg)

#URBAN or rural
look_for(dhs[[1]], "rural")
table(dhs[[1]]$hv025)

pfpr_rural<- pfpr_df[,c("hv001", "hv025")]
pfpr_rural$hv025 <- pfpr_rural$Rural_urban
colnames(pfpr_rural)[2]<- "Rural_urban"
table(pfpr_rural$Rural_urban)
pfpr_rural <- unique(pfpr_rural, by = "hv001")
table(pfpr_rural$Rural_urban)

# proportion with secondary or greater education 
look_for(dhs[[1]], "education")
val_labels(dhs[[1]]$hv106)

pfpr_h_edu <- pfpr_dhs %>%  mutate(edu_a = ifelse(hv106 <2, 0,ifelse(hv106 == 8, NA, ifelse(hv106 == 2|3, 1, NA)))) %>% drop_na(edu_a)
table(pfpr_dhs$hv106)
table(pfpr_h_edu$edu_a)

pfpr_edu<- dataclean.para(pfpr_h_edu, hv005, hv005, hv106, 'edu_a', 'edu_a') 
table(pfpr_edu$edu_a)

svyd_edu <- svydesign.fun(pfpr_edu)

clu_edu <- result.fun('edu_a', 'hv001', design=svyd_edu, pfpr_edu, "hv007")
head(clu_edu)


#ITN use proportion among u5
var_label(pfpr_data$hml16)

# subsetting for ITN proportion 
pfpr_itn <- pfpr_data %>% filter(hv103 == 1  & hml16 %in% c(0:4)) %>%  
  mutate(net_use = ifelse(hml12 %in% c(1,2), 1,0))

table(pfpr_itn$net_use)

pfpr_itn<- dataclean.para(pfpr_itn, hv005, hv005, net_use, 'net_use', 'net_use_u5') 
table(pfpr_itn$net_use_u5)

svyd_itn <- svydesign.fun(pfpr_itn)

clu_u5_net_use <- result.fun('net_use_u5', 'hv001', design=svyd_itn, pfpr_itn, "hv007")
head(clu_u5_net_use)


#ITN use proportion among pregnant women 
val_labels(pfpr_data$hv104)

# subsetting for ITN proportion 
pfpr_itn_preg <- pfpr_data %>% filter(hv103 == 1  & hv104 == 2 & hml18 == 1 & hml16 %in% c(15:49)) %>%  
  mutate(net_use = ifelse(hml12 %in% c(1,2), 1,0))

table(pfpr_itn_preg$net_use)

pfpr_itn_preg<- dataclean.para(pfpr_itn_preg, hv005, hv005, net_use, 'net_use', 'net_use_preg') 
table(pfpr_itn_preg$net_use_preg)

svyd_itn_preg <- svydesign.fun(pfpr_itn_preg)

clu_u5_net_preg <- result.fun('net_use_preg', 'hv001', design=svyd_itn_preg, pfpr_itn, "hv007")
head(clu_u5_net_preg)


# median household size
look_for(dhs[[1]], "number")

table(pfpr_data$hv013)

pfpr_hh_size<- dataclean.para(pfpr_data, hv005, hv005, hv009, 'hv009', 'hh_size') 
table(pfpr_hh_size$hh_size)

svyd_hh_size <- svydesign.fun(pfpr_hh_size)
table(pfpr_hh_size$hh_size)

clu_hh_size<- svyby(~hh_size, ~hv001, design = svyd_hh_size, FUN=svyquantile, quantiles=0.5, ci=TRUE,vartype="ci")
head(clu_hh_size)




# care seeking proportion among u5 children

look_for(dhs2[[1]], "Artemisinin")

table(dhs2[[1]]$ml13e)

pfpr_care <- dhs2[[1]] %>% filter(b5 == 1  & b19 <  60 & h22 == 1) 

pfpr_care <- dataclean(pfpr_care, v005, v005,' ml13e', "ACT_use_u5")

svyd_care <- svydesign.fun(pfpr_care)
table(pfpr_care$ACT_use_u5)

clu_u5_care<- result.fun('ACT_use_u5', 'v001', design=svyd_care, pfpr_care, "v007")
head(clu_u5_care)
colnames(clu_u5_care)[1]<- "hv001"
#colnames(clu_u5_care)[4]<- "hv007"

# population density
clu_pop_den <- read.csv("NGGC7BFL.csv")%>% 
  dplyr::select(hv001 = DHSCLUST, pop_den = UN_Population_Density_2015)%>% 
  mutate(l_pop_den = log(pop_den))

summary(clu_pop_den$l_pop_den)

#KAP proportions 
#Malaria can be fully cured by medicine 
dhs2_df<- dhs2[[1]]

kap_cure_med <- dataclean(dhs2_df, v005, v005, '"s1108ai"', "kap_cure_med")
kap_cure_med$kap_cure_med[kap_cure_med$kap_cure_med == 8] <- 0

svyd_cure_kap <- svydesign.fun(kap_cure_med)
table(kap_cure_med$kap_cure_med)

clu_kap_cure<- result.fun('kap_cure_med', 'v001', design=svyd_cure_kap, kap_cure_med, "v007")
head(clu_kap_cure)
colnames(clu_kap_cure)[1]<- "hv001"

#Malaria can lead to death
kap_death <- dataclean(dhs2_df, v005, v005, '"s1108ba"', "kap_death")
kap_death$kap_death[kap_death$kap_death == 8] <- 0

svyd_death_kap <- svydesign.fun(kap_death)
table(kap_death$kap_death)

clu_kap_death<- result.fun('kap_death', 'v001', design=svyd_death_kap, kap_death, "v007")
head(clu_kap_death)
colnames(clu_kap_death)[1]<- "hv001"

#No worry about malaria due to easy treatment

kap_treat <- dataclean(dhs2_df, v005, v005, '"s1108bc"', "kap_treat")
kap_treat$kap_treat[kap_treat$kap_treat == 8] <- 0

svyd_treat_kap <- svydesign.fun(kap_treat)
table(kap_treat$kap_treat)

clu_kap_treat<- result.fun('kap_treat', 'v001', design=svyd_treat_kap, kap_treat, "v007")
head(clu_kap_treat)
colnames(clu_kap_treat)[1]<- "hv001"

#Know people sick with malaria

kap_know <- dataclean(dhs2_df, v005, v005, '"s1108bd"', "kap_know")
kap_know$kap_know[kap_know$kap_know == 8] <- 0

svyd_know_kap <- svydesign.fun(kap_know)
table(kap_know$kap_know)

clu_kap_know<- result.fun('kap_know', 'v001', design=svyd_know_kap, kap_know, "v007")
head(clu_kap_know)
colnames(clu_kap_know)[1]<- "hv001"

#Only weak children can die from malaria

kap_weak <- dataclean(dhs2_df, v005, v005, '"s1108bf"', "kap_weak")
kap_weak$kap_weak[kap_weak$kap_weak == 8] <- 0

svyd_weak_kap <- svydesign.fun(kap_weak)
table(kap_weak$kap_weak)

clu_kap_weak<- result.fun('kap_weak', 'v001', design=svyd_weak_kap, kap_weak, "v007")
head(clu_kap_weak)
colnames(clu_kap_weak)[1]<- "hv001"

# bind the datasets 

all_clu <- left_join(clu_est, clu_wealth, by = "hv001") %>% 
  left_join(., clu_u5_prop, by = "hv001") %>% 
  left_join(., clu_preg, by = "hv001") %>% 
  left_join(., clu_edu, by = "hv001") %>% 
  left_join(., clu_u5_net_use, by = "hv001") %>% 
  left_join(., clu_u5_net_preg, by = "hv001") %>% 
  left_join(., clu_hh_size, by = "hv001") %>% 
  left_join(., clu_u5_care, by = "hv001") %>% 
  left_join(., clu_pop_den, by = "hv001") %>% 
  left_join(., clu_hh_members_age, by = "hv001") %>%
  left_join(., pfpr_rural, by = "hv001") %>%
  left_join(., clu_sex, by = "hv001")


all_clu['data_source'] = 'dhs2018'
  
  

# correlation coefficient and regression with simple linear model

require(stats)
reg<-lm(p_test~edu_a, data = all_clu)
reg
summary(reg)
coreg <- coefficients(reg)

y <- cor(all_clu$p_test, all_clu$edu_a, use = "pairwise.complete.obs")
y
# equation of line 

eq <- paste0("r = ", round(y, 2), ",", " ", "y =", round(coreg[1], 2), " ", "+" , " " , abs(round(coreg[2], 3)), "* x", ",",
             " ", "p = 1")
eq

plot<- ggplot(all_clu, aes(x=u5_prop, y=p_test)) + 
  geom_point(shape = 18, color = "red") + 
  xlab("Proportion of U5 in individual clusters")+ 
  ylab("U5 Parasite Prevalence") +
  geom_abline(intercept =  0.08901   , slope =  0.30515)+ 
  ggtitle(eq)+
  theme(plot.title = element_text(hjust = 0.5))
plot

png("C:/Users/Admin/Documents/NU - Malaria Modeling/Non Linear/results/urban_mal_u5_prop_v2.png")
print(plot)
dev.off()

reg<-lm(p_test~ wealth_2 + edu_a + net_use_u5 + net_use_preg + ACT_use_u5 + u5_prop + preg + hh_size + pop_den, 
        data = all_clu)
reg
summary(reg)


# fever proportion (maybe)

