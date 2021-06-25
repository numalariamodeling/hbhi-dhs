#rm(list=ls())

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

look_for(dhs[[1]], "smear")

table(pfpr_data$hml32) # frequency table for smear test 



# prep dataset for cluster level analysis - we start with urban cluster analysis 

val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural

#pfpr_place <- pfpr_df %>% filter(hv025 == 1)
#pfpr_dhs <- pfpr_data%>% filter(hv025 == 1)
pfpr_place <- pfpr_df
pfpr_dhs <- pfpr_data
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
table(dhs[[1]]$ha54)

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


###final individual level dataset



pfpr_df <- pfpr_dhs_hh%>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6))

pfpr_df <- funEnv$dataclean.para(pfpr_df, hv005, hc1, hml32, 'hml32', 'p_test')



pfpr_data2 <- pfpr_df[ ,colnames(pfpr_df) 
                   %in% c("hv001", "p_test",'wealth_2', "preg", "edu_a", "net_use_preg", 
                          "net_use_u5", 'hh_size', "hv005", "hv022", "hv021", "hc1", "hv042", "hv103")]
#Select variables that could cause problems in the imputation process

#pfpr_data2 <- pfpr_data2 %>% 
  #dplyr::select(p_test, wealth_2, preg, edu_a, net_use_preg, net_use_u5, hh_size,)

# Treat variables as factors
pfpr_data2$hml32 <- as.factor(pfpr_data2$p_test) #p_test
pfpr_data2$hv270 <- as.factor(pfpr_data2$wealth_2) #wealth_2
#pfpr_data2$hml16 <- as.factor(pfpr_data2$age) #age
pfpr_data2$preg <- as.factor(pfpr_data2$preg) #preg
pfpr_data2$edu_a <- as.factor(pfpr_data2$edu_a) #education
pfpr_data2$net_use_u5 <- as.factor(pfpr_data2$net_use_u5) #ITN use proportion among u5
pfpr_data2$net_use_preg <- as.factor(pfpr_data2$net_use_preg) #ITN use proportion among pregnant women 
pfpr_data2$hh_size <- as.factor(pfpr_data2$hh_size) # median household size


p_missing <- unlist(lapply(pfpr_data2, function(x) sum(is.na(x))))/nrow(pfpr_data)
sort(p_missing[p_missing > 0], decreasing = TRUE)


# We run the mice code with 0 iterations 

imp <- mice(pfpr_data2, maxit=0)

# Extract predictorMatrix and methods of imputation 

predM <- imp$predictorMatrix
meth <- imp$method

# Setting values of variables I'd like to leave out to 0 in the predictor matrix
predM[, c("hv001","hv005", "hv022", "hv021", "hc1", "hv042", "hv103")] <- 0

# Specify a separate imputation model for variables of interest 

# Ordered categorical variables 
poly <- c("wealth_2", "edu_a")

# Dichotomous variable
log <- c("p_test", "preg", "net_use_preg", "net_use_u5")

# Turn their methods matrix into the specified imputation models
meth[poly] <- "polr"
meth[log] <- "logreg"

meth

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
pfpr_dataimp_long$net_use_preg <- with(pfpr_dataimp_long, as.integer(pfpr_dataimp_long$net_use_preg))

pfpr_dataimp_long$preg <- with(pfpr_dataimp_long, as.integer(pfpr_dataimp_long$preg))

#Convert back to mids type - mice can work with this type
pfpr_dataimp_long_mids<-as.mids(pfpr_dataimp_long)

##############################converting entire dataset to numeric dataframe for survey designing#################################
svy_mal <- with(pfpr_dataimp_long_mids, (funEnv$svydesign.fun))


clu_est <- funEnv$result.fun('p_test', 'hv001', design=svy_mal, pfpr_dataimp_long_mids, "hv007")
