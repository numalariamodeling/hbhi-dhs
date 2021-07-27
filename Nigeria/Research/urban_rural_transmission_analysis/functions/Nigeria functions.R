
# # Reading in the necessary packages 
list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "sjlabelled", "raster", "rlist", 'rgeos', 'INLA', 'ggpubr',
       'cowplot', 'gridExtra', 'lme4')


#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


#read files function 
read.files <- function(path, general_pattern, specific_pattern, fun) {
  files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(specific_pattern, files))]
  sapply(files, fun, simplify = F)
}



#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


#survey estimates generating functions  
result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}


result.mean<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, vartype= "se", na.rm=T, influence = TRUE)
}


result.median<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svyquantile, design, quantiles=0.5, ci=TRUE, vartype="ci", na.rm=T)
}



#estimation functions 
estim_prop <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.prop(col, by, design=svy_mal)
}



estim_mean <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.mean(col, by, design=svy_mal)
}

estim_median <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.median(col, by, design=svy_mal)
}


get_crs <- function(df, raster){
  dhs <- spTransform(x = df, CRSobj = crs(raster))
}



extract_fun <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR)%>%
    mutate(hv001 = dhs$DHSCLUST) 
}

# extract_fun2 <- function(raster, dhs, buffer){
#   clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>% mutate(dhs_year = dhs$DHSYEAR)%>% 
#     mutate(hv001 = dhs$DHSCLUST)%>% rename_at(3,~"varname")%>% subset(select = c(4, 2, 3, 5))
#   
# }
# 
# extrclean.fun <- function(csv, varname){
#   csv 
#   dff <- csv
#   dff %>% rename_at(2,~varname)
# }
# 
# extrclean.fun2 <- function(csv, varname){
#   csv 
#   dff <- csv
#   dff %>% rename_at(3,~varname)
# }

######## Survey monthS fxn shopefile, 1 for 2010, 2 for 2015, and 3 for 2018
# srvymnth.fun <- function(list_id, srvymnth){
#   hh_survey <- dhs_hh[[3]]%>% filter(hv025 ==1)
#   hh_survey <- hh_survey[!duplicated(hh_survey$hv001),]
#   hh_survey <- hh_survey[,c("hv001", "hv006")] %>% rename_at(1,~"DHSCLUST")%>% filter(hv006 == 8)
#   dhs_2018_aug = merge(dhs[[3]], hh_survey)  %>% st_as_sf()
# }