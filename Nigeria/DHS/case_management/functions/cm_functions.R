x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "ggpubr")

lapply(x, library, character.only = TRUE) #applying the library function to packages



options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


#This function read in all files with three different file patterns in all folders and subfolders in the working directory

read.files <- function(filepat1,path,fun) {
  filenames <- list.files(path = path, pattern = filepat1, recursive = TRUE, full.names = TRUE)
  #in_files_K <- list.files(path = path, pattern =  filepat2, recursive = TRUE, full.names = TRUE)
  #in_files_P <- list.files(path = path, pattern = filepat3, recursive = TRUE, full.names = TRUE)
  #filenames <- rbind(in_files_I, in_files_K, in_files_P)
  sapply(filenames, fun, simplify = F)
}


recoder <- function(x){
  ifelse(x > 6, NA,ifelse(x == 0, 0, 1))
}


#function for cleaning other files 
dataclean<-function(data, filter_var, filter_var1, cols, new_col){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  data <- data %>% filter(!is.na(!!filter_var) | (!is.na(!!filter_var1)))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=v005/1000000,strat=v022,
           id=v021, num_p=1) 
}


#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


#generating results and table function with estimates 
result.fun<- function(var, var1, design, data) { #year
  year <- unique(na.omit(data$v007))
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) # svyciprop, method ='logit', levels=0.95, vartype= "se"
  
  # num_est<-svyby(formula=make.formula(var2), by=make.formula(var1), FUN=svytotal, design, na.rm=T)%>% 
  #   dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
  
  # p_est%>%left_join(num_est)%>% rename(`Number of Participants` = num_p)
  
  #year <- data.frame(year = unique(data[,year]))
  
  cbind(p_est, year)
  
}




ACT.fun_region <- function(df){
  df1<-dataclean(df, ml13e, v005, 'ml13e', 'comboACT')  
  svyd <- svydesign.fun(df1)
  #generate LGA estimates 
  df2 <- result.fun('comboACT', 'v024',design=svyd, data =df1)
  repDS_region %>%  left_join(df2)
}


generate.ACT.state_LGA_repDS <- function(df, var){
  df1<-dataclean(df, ml13e, v005, 'ml13e', 'comboACT')  
  svyd <- svydesign.fun(df1)
  #generate LGA estimates 
  df2 <- result.fun('comboACT', var,design=svyd, data =df1) 
  if(var == "LGA"){
    df2 <- df2 %>% mutate(LGA = case_when(LGA == "kiyawa"~ "Kiyawa", LGA == "kaita" ~"Kaita", TRUE ~ LGA)) 
  }else{
    print("no name corrections for state, repDs or regional estimates")
  }
  repDS_LGA %>% left_join(df2)%>% tidyr::fill(year, .direction = "updown")
}




generate_logit  <- function(data) {
  data%>%mutate(logit_ACT=ifelse(!is.na(comboACT),logit(comboACT),NA),
                var_logit_ACT=(se^2)/(comboACT^2*(1-comboACT)^2),
                # var_logit_obese=ifelse(se==0,NA,var_logit_obese),
                var_logit_ACT=ifelse(se<0.00001,NA,var_logit_ACT),
                logit_ACT=ifelse(is.na(var_logit_ACT),NA,logit_ACT))%>% fill(year, .direction = "updown")
}

generate_smooth_values <- function(dat){
  year <- unique(dat$year)
  mod2 <- inla(smoothing.model.2,
               family = "gaussian",
               data =dat,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
               control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
               scale=prec)
  cbind(mod2, year)
}

# handy functions for transforming the data
logit<-function(x){
  log(x/(1-x))
}


expit<-function(x){
  exp(x)/(1+exp(x))
}



#over function 
over.fun <- function(df) {
  sp::over(SpatialPoints(coordinates(df),proj4string = df@proj4string), LGAshp)
}



