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
  dplyr::select(hv001, hml32, hv270, hml16, ha54, hv106, hv103, hv104, hv009)

# We run the mice code with 0 iterations 

imp <- mice(pfpr_data2, maxit=0)

# Extract predictorMatrix and methods of imputation 

predM <- imp$predictorMatrix
meth <- imp$method

# Setting values of variables I'd like to leave out to 0 in the predictor matrix
predM[, c("hv001")] <- 0

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

replace_imputed <- function(pfpr_data, pfpr_dataimp_long_mids){
  
  namestoChange <- colnames(pfpr_data)[colnames(pfpr_dataimp_long_mids) %in% colnames(pfpr_data)]
  
  for(i in 1:length(namestoChange)){
    pfpr_data[namestoChange[i]] <- pfpr_dataimp_long_mids[namestoChange[i]]
  }
  return(pfpr_data)
  
}

pfpr_data_inputed <- replace_imputed(pfpr_data, pfpr_dataimp_long_mids)
