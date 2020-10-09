#rm(list=ls())

#important to download the github version of tidyverse.Uncomment the script below to run
#install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("r-pkgs/usethis")

#devtools::install_github("hadley/tidyverse")



# Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt","data.table", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "lme4", "faraway")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/Admin/Documents/NU - Malaria Modeling/Non Linear")

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("C:/Users/Admin/Documents/NU - Malaria Modeling/Non Linear", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


dhs <- list.files(pattern = ".*NGPR.*\\.DTA", recursive = F, full.names = TRUE)
dhs <- sapply(dhs, read_dta, simplify = F)

dhs2 <- list.files(pattern = ".*NGKR.*\\.DTA", recursive = F, full.names = TRUE)
dhs2 <- sapply(dhs2, read_dta, simplify = F)
# clean and select pfpr data 

pfpr_data <- dhs[[1]] # uses the DHS person recode dataset 

look_for(dhs[[1]], "smear")

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


# next we estimate proportion of people in high SES by cluster
# recode the weath quintile variable 
table(pfpr_df$hv270)

pfpr_dhs <- pfpr_dhs %>%  mutate(wealth = ifelse(hv270 <4, 0, 1))
table(pfpr_dhs$wealth)

pfpr_dhs_w <- funEnv$dataclean.para(pfpr_dhs, hv005, hv005, wealth, 'wealth', 'wealth_2') 
table(pfpr_dhs$wealth)

#ITN use proportion among u5
var_label(pfpr_data$hml16)

# subsetting for ITN proportion 
pfpr_dhs_u5 <- pfpr_dhs_w %>% mutate(net_use_u5 = ifelse(hv103 == 1& hml16 %in% c(0:4) & hml12 %in% c(1,2),1, 0))


table(pfpr_dhs_u5$net_use_u5)

# estimate proportion of pregnant women in each cluster 
look_for(dhs[[1]], "pregnant")
table(dhs[[1]]$sh09)

pfpr_dhs_preg <- pfpr_dhs_u5 %>%  mutate(preg = ifelse(hv104 != 2, NA, ifelse(hml16 %in% c(15:49) & hml18 == 1|0, 1, 0)))

table(pfpr_dhs_preg$preg)


# proportion with secondary or greater education 
look_for(dhs[[1]], "education")
val_labels(dhs[[1]]$hv106)

pfpr_dhs_ed <- pfpr_dhs_preg %>%  mutate(edu_a = ifelse(hv106 <2,  0,ifelse(hv106 == 8, NA, ifelse(hv106 == 2|3, 1, NA)))) %>% drop_na(edu_a)
table(pfpr_dhs_ed$hv106)
table(pfpr_dhs_ed$edu_a)


#ITN use proportion among pregnant women 
val_labels(pfpr_data$hv104)

# subsetting for ITN proportion 
pfpr_dhs_itn_p <- pfpr_dhs_ed %>% 
  mutate(net_use_preg = ifelse(hml18 != 1, NA, ifelse(hv103 == 1 & hml18 == 1 & hml16 %in% c(15:49) & hml12 %in% c(1,2),1, 0)))


table(pfpr_dhs_itn_p$net_use_preg)


# median household size
look_for(dhs[[1]], "number")

table(pfpr_dhs$hv013)

pfpr_dhs_hh <- dataclean.para(pfpr_dhs_itn_p, hv005, hv005, hv009, 'hv009', 'hh_size') 
table(pfpr_dhs_hh$hh_size)


# care seeking proportion among u5 children

look_for(dhs2[[1]], "artemisinin")

table(dhs2[[1]]$ml13e)

pfpr_care <- dhs2[[1]] %>% filter(b5 == 1  & b8 <  5 & h22 == 1) 

pfpr_care <- dataclean(pfpr_care, v005, v005,' ml13e', "ACT_use_u5")
pfpr_care <- pfpr_care[,c("v001", "v002", "ACT_use_u5")]
colnames(pfpr_care)[1]<- "hv001"
colnames(pfpr_care)[2]<- "hv002"
table(pfpr_care$ACT_use_u5)

#joining  datasets using multiple keys, by Cluster then household


pfpr_place < - setDT(pfpr_dhs_hh)
pfpr_place < - setDT(pfpr_place)
setkey(pfpr_place,hv001,hv002)
setkey(pfpr_dhs_hh,hv001,hv002)


filtered_df <- pfpr_dhs_hh  %>% filter(hv042 == 1)

left_merge <- filtered_df[pfpr_place, allow.cartesian = T] #Left Join (if dt1 is the "left" table)

pfpr_place < - setDT(left_merge)
pfpr_place < - setDT(pfpr_care)
setkey(left_merge,hv001,hv002)
setkey(pfpr_care,hv001,hv002)



left_merge_1 <- left_merge[pfpr_care, allow.cartesian = T] #Left Join

final_filtered_df <- left_merge_1  %>% filter(hv042 == 1)

final_filtered_df <- final_filtered_df %>% mutate(rural = ifelse(hv025 == 2, 1,0))

#survey design

svyd_filtered <- svydesign.fun(final_filtered_df)


#Fitting logistic regression

fit <- glmer(p_test ~ (1 | hv001), family = binomial("logit"), data = final_filtered_df, 
             weights=wt) 

summary(fit) 

fita <- glm(p_test ~ 1, data = final_filtered_df, family = binomial("logit")) 

logLik(fita)-logLik(fit) 

(fit2 <- glmer(p_test ~ wealth_2 + edu_a + hh_size  + net_use_preg + preg + 
                 net_use_u5 + ACT_use_u5 + rural + (1 | hv001), 
               family = binomial("logit"), data = final_filtered_df, weights=wt)) 


summary(fit2)

plot_model(fit2, title = " ", line.size = 1, dot.size = 2) + ylim(0.0, 7)



## extract the coefficients from the model and exponentiate
exp(coef(fit2))

#odds ratio confidence intervals
exp(confint(fit2))

#calculatIN predicted probabilities for each of our outcome levels using the fitted function. 
head(pp <- fitted(fit2))
