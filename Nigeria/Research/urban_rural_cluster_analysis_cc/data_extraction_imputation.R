rm(list=ls())

#important to download the github version of tidyverse.Uncomment the script below to run
#install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("r-pkgs/usethis")

#devtools::install_github("hadley/tidyverse")



#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear")

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


dhs <- list.files(pattern = ".*NGPR.*\\.DTA", recursive = F, full.names = TRUE)
dhs <- sapply(dhs, read_dta, simplify = F)

dhs2 <- list.files(pattern = ".*NGKR.*\\.DTA", recursive = F, full.names = TRUE)
dhs2 <- sapply(dhs2, read_dta, simplify = F)
# clean and select pfpr data 

pfpr_data <- dhs[[1]] # uses the DHS person recode dataset 

#merging with the KR dataset

pfpr_care <- dhs2[[1]] %>% filter(b5 == 1  & b19 <  60 & h22 == 1) 

pfpr_care <- pfpr_care[,c("v001", "ml13e")]
colnames(pfpr_care)[1]<- "hv001"

#pfpr_data <- left_join(pfpr_data, pfpr_care, by = "hv001") 

pfpr_data2 <- pfpr_data


# Treat variables as factors
pfpr_data2$hml32 <- as.factor(pfpr_data2$hml32) #p_test
pfpr_data2$hv270 <- as.factor(pfpr_data2$hv270) #wealth_2
pfpr_data2$hml16 <- as.factor(pfpr_data2$hml16) #age
pfpr_data2$ha54 <- as.factor(pfpr_data2$ha54) #preg
pfpr_data2$hv106 <- as.factor(pfpr_data2$hv106) #education
pfpr_data2$hv103 <- as.factor(pfpr_data2$hv103) #ITN use proportion among u5
pfpr_data2$hv104 <- as.factor(pfpr_data2$hv104) #ITN use proportion among pregnant women 
pfpr_data2$hv009 <- as.factor(pfpr_data2$hv009) # median household size

p_missing <- unlist(lapply(pfpr_data2, function(x) sum(is.na(x))))/nrow(pfpr_data)
sort(p_missing[p_missing > 0], decreasing = TRUE)

# Select variables that could cause problems in the imputation process
pfpr_data2 <- pfpr_data2 %>% 
  dplyr::select(hv001, hml32, hv270, hml16, ha54, hv106, hv103, hv104, hv009, 
                hv042, hc1, hv025, hv007, hv005, hml16, )

# We run the mice code with 0 iterations 

imp <- mice(pfpr_data2, maxit=0)

# Extract predictorMatrix and methods of imputation 

predM <- imp$predictorMatrix
meth <- imp$method

# Setting values of variables I'd like to leave out to 0 in the predictor matrix
predM[, c("hv001","hv042","hc1", "hv025", "hv007", "hv001", "hml16")] <- 0

# Specify a separate imputation model for variables of interest 

# Ordered categorical variables 
poly <- c("hv270", "hv106")

# Dichotomous variable
log <- c("hml32", "ha54", "hv103","hv104","hv009")

# Turn their methods matrix into the specified imputation models
meth[poly] <- "polr"
meth[log] <- "logreg"

# With this command, we tell mice to impute the pfpr_data2 data, create 5
# datasets, use predM as the predictor matrix and don't print the imputation
# process. If you would like to see the process, set print as TRUE

imp2 <- mice(pfpr_data2, maxit = 5, 
             predictorMatrix = predM, 
             method = meth, print =  FALSE)

head(imp2$imp$hml32)
tail(imp2$imp$hml32)

# extraction of the first imputed, complete dataset and look at the first
# rows using the complete function

pfpr_data2comp <- mice::complete(imp2, 1)
head(pfpr_data2comp)


# First, turn the datasets into long format
pfpr_dataimp_long <- mice::complete(imp2, action="long", include = TRUE)


# Convert two variables into numeric
pfpr_dataimp_long$hml16 <- with(pfpr_dataimp_long, as.integer(pfpr_dataimp_long$hml16))

pfpr_dataimp_long$hv009 <- with(pfpr_dataimp_long, as.integer(pfpr_dataimp_long$hv009))


#Convert back to mids type - mice can work with this type
pfpr_dataimp_long_mids<-as.mids(pfpr_dataimp_long)


#Replace inputed coloums

#replace_imputed <- function(pfpr_data, pfpr_dataimp_long){
  
  #namestoChange <- colnames(pfpr_data)[colnames(pfpr_dataimp_long) %in% colnames(pfpr_data)]
  
  #for(i in 1:length(namestoChange)){
    #pfpr_data[namestoChange[i]] <- pfpr_dataimp_long[namestoChange[i]]
  #}
  #return(pfpr_data)
  
#}

#pfpr_data_inputed <- replace_imputed(pfpr_data, pfpr_dataimp_long)

pfpr_data <- pfpr_dataimp_long

table(pfpr_data$hml32) # frequency table for smear test 

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_df <- pfpr_data %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6))





# prep dataset for cluster level analysis - we start with urban cluster analysis 

val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural

pfpr_place <- pfpr_df %>% filter(hv025 == 1)
pfpr_dhs <- pfpr_data%>% filter(hv025 == 1)
#pfpr_place <- pfpr_df
#pfpr_dhs <- pfpr_data
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


# estimate proportion of pregnant women in each cluster 
look_for(dhs[[1]], "pregnant")
table(dhs[[1]]$ha54)

pfpr_preg<- dataclean.para(pfpr_dhs, hv005, hv005, ha54, 'ha54', 'preg') 
table(pfpr_preg$preg)


svyd_preg <- svydesign.fun(pfpr_preg)

clu_preg <- result.fun('preg', 'hv001', design=svyd_preg, pfpr_preg, "hv007")
head(clu_preg)

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
  left_join(., clu_pop_den, by = "hv001") 
