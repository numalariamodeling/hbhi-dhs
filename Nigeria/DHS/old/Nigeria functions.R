x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "ggpubr")

lapply(x, library, character.only = TRUE) #applying the library function to packages

# smoothing, printing, plotting, mapping, saving 
library(INLA); library(spdep)

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


#This function read in all files with three different file patterns in all folders and subfolders in the working directory

read.files <- function(filepat1,path,fun) {
  filenames <- list.files(path = path, pattern = filepat1, recursive = TRUE, full.names = TRUE)
  #in_files_K <- list.files(path = path, pattern =  filepat2, recursive = TRUE, full.names = TRUE)
  #in_files_P <- list.files(path = path, pattern = filepat3, recursive = TRUE, full.names = TRUE)
  #filenames <- rbind(in_files_I, in_files_K, in_files_P)
  sapply(filenames, fun, simplify = F)
}


read.files2 <- function(filepat1, filepat2, filepat3) {
  in_files_I<- list.files(path = "data", pattern = filepat1, recursive = TRUE, full.names = TRUE)
  in_files_K <- list.files(path = "data", pattern = filepat2, recursive = TRUE, full.names = TRUE)
  in_files_P<- list.files(path = "data", pattern = filepat3, recursive = TRUE, full.names = TRUE)
  filenames <- rbind(in_files_I, in_files_K, in_files_P)
  sapply(filenames, read_sav, simplify = F)
}


# map_sim results
map_fun <- function(shpfile, map_val) {
  number <- unique(na.omit(shpfile$meta_information))
  tm_shape(shpfile) + #this is the health district shapfile with LLIn info
    tm_polygons(col = map_val, textNA = "No data", 
                title = "", palette = "seq", breaks=c(0, 0.1, 0.2, 
                                                      0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
    tm_layout(title = paste0(number,  " " , "State U5 PfPR 2025"),
              aes.palette = list(seq="RdYlBu")) 
}


mutate_f <- function(data, col_1, col_2, col_3) { 
  mutate(data, wt=!!col_1/1000000,strat=!!col_2, id=!!col_3, num_p=1) 
}

# This function recodes x = 8 as NA, x = 3 or 0 as 0 and any remaining values of x to 1
recoder.ml0 <- function(data){
  data %>% mutate(ml0 = ifelse(ml0 > 6, NA,ifelse(ml0 == 0, 0, 1)))
}

recoder.wealth <- function(data, col){
  name_new <- paste0(quo_name(col), "_new")
  mutate(data, !!name_new := ifelse(!!col < 4, 0, 1))
}

# recoder <- function(data, col){
#   name_new <- paste0(quo_name(col), "_new")
#   mutate(data, !!name_new := ifelse(!!col == 0, 0, 1))
# }

recoder.gen <- function(data, col){
  name_new <- paste0(quo_name(col), "_new")
  mutate(data, !!name_new := ifelse(!!col > 7, 0, ifelse(!!col == 0, 0, 1)))
}

recoder.ml101 <- function(data){
  data %>% mutate(ml101 = ifelse(ml101 > 6, NA,ifelse(ml101 == 0 | ml101 ==3, 0, 1)))
}

recoder <- function(x){
ifelse(x > 6, NA,ifelse(x == 0, 0, 1))
}

recoder.pfpr <- function(x){
  ifelse(x > 6, NA,ifelse(x == 0|x == 6, 0, 1))
}


# This function applies 'recoder' to multiple variables 
apply.recoder <-  function(data,var){
  data%>%
    mutate_at(vars(contains(var)), recoder)
}

recoder2 <- function(x){
  ifelse(x > 6, NA,ifelse(x == 0 | x ==3, 0, 1))
}



recoder3 <- function(x){
  ifelse(x > 97, NA,ifelse(x > 7, 0, 1))
}


recoder4 <- function(x){
  ifelse(x > 97, NA,ifelse(x > 24, 0, 1))
}

recoder5 <- function(x){
  ifelse(x > 6, NA,ifelse(x == 2, 0, 1))
}


recoder.ml4_1 <- function(data){
  data %>% mutate(m14_1 = ifelse(m14_1 > 40, NA,ifelse(m14_1 == 0, 0, 1)))
}


recoder.ml4_1_v2 <- function(data){
  data %>% mutate(m14_1 = ifelse(m14_1 > 90, NA,ifelse(m14_1 < 3, 0, 1)))
}

recoder.m49a <- function(data){
  data %>% mutate(m49a_1 = ifelse(m49a_1 > 1, NA,ifelse(m49a_1 == 0, 0, 1)))
}


recoder.ml1 <- function(data){
  data %>% mutate(ml1_1 = ifelse(ml1_1 > 97, NA,ifelse(ml1_1 < 3, 0, 1)))
}


#recoder itn
recode_itn <- function(data) {
  data %>% mutate(hh_itn = ifelse(hml12 == 9, NA,ifelse(hml12 ==1 | hml12 ==2, 1, 0)))
}

#This function takes an admin boundary and a point shape file and plots them on top of each other 
raw.plot.fun <- function(data1,data2, adtitle){
  plot(data1, main=adtitle)
  plot(data2,add=T,col=4) 
}


#This function uses the person recode dataset to create the month of the survey in the format mm-yyyy and mm-dd-yyyy. it also recides v001 to hv001 
survey.month.fun <- function(data) {
  data%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% #dhs pr 2010
    dplyr::rename(v001 = hv001) %>%
    mutate(YYYY = (floor((hv008 - 1)/12)+1900))%>%
    mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
    mutate(time2 = str_c(MM, YYYY, sep = '-'))
} 

#function for cleaning parasitemia datasets 
dataclean.para <-function(data, filter_var, filter_var1, filter_var2, cols, new_col){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  filter_var2 <- enquo(filter_var2)
 data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1)) %>% filter(!is.na(!!filter_var2))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=hv005/1000000,strat=hv022,
           id=hv021, num_p=1)
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


#function for cleaning other files 
dataclean.HH<-function(data, filter_var, filter_var1, cols, new_col){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  data <- data %>% filter(!is.na(!!filter_var)) %>% filter (!is.na(!!filter_var1))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=hv005/1000000,strat=hv022,
           id=hv021, num_p=1) 
}

#function for cleaning mics files 
dataclean.mics<-function(data, filter_var, filter_var1, cols, new_col){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=chweight,strat=stratum,
           id=PSU, num_p=1) 
}


dataclean2.HH<-function(data, filter_var, filter_var1, cols, new_col, strata){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=hv005/1000000,strat=strata,
           id=hv021, num_p=1) 
}

#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}

#generating results and table function with estimates 
result.fun.para <- function(var, var1, var2, var3, design) {
  
  target_quo <- parse_quosure(var3)
  
  ptest<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, vartype= "ci", level =0.75, method ="beta", na.rm=T) %>% arrange(!!target_quo, time2)
  
  ptest_num_ym<-svyby(formula=make.formula(var2), by=make.formula(var1), FUN=svytotal, design, na.rm=T)%>% 
    dplyr:: select(-se)%>%arrange(!!target_quo, time2)%>% mutate(num_p = round(num_p, 0))
  
  ptest%>%left_join(ptest_num_ym)%>% rename(PfPr = var,`Number of Kids` = num_p)
  
}


#generating results and table function with estimates 
result.clu.fun.para <- function(var, var1, var2, var3, design, data) {
  
  target_quo <- parse_quosure(var3)
  
  ptest<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) %>% arrange(!!target_quo, time2)
  
  ptest_num_ym<<- data%>%dplyr::select(hv001, time2, num_p)%>%na.omit(data)%>%group_by(hv001, time2) %>% summarise_each(funs(mean, sd, std.error, n())) %>% 
  dplyr::select(-mean, -sd, -std.error)
  
  ptest%>%left_join(ptest_num_ym)%>% rename(PfPr = var, `Number of Kids` = n, DHSCLUST = hv001)
  
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


#generating results and table function with estimates 
result.clu.fun<- function(var, var1, design, data, year) {
  
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) 
  
  num_est <- data%>%drop_na(var)%>%dplyr::select(v001, num_p)%>%group_by(v001) %>% summarise_each(funs(mean, sd, std.error, n()))%>% 
    dplyr::select(-mean, -sd, -std.error)
  
  df <- p_est%>%left_join(num_est)%>% rename(`Number of Participants` = n, DHSCLUST = v001)
  
  year <- data.frame(year = unique(data[,year]))
  
  cbind(df, year)
  
}



#over function 
over.fun <- function(df) {
  sp::over(SpatialPoints(coordinates(df),proj4string = df@proj4string), LGAshp)
}


# This function maps data  
tmap.fun1 <- function(adminfile, DSmapvalue, adminlegtitle, main_title, text_title, ptsfile, bubble_size, bubble_col_size) {
  tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
    tm_polygons(col = DSmapvalue, textNA = "No data", 
                title = adminlegtitle, palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(main.title= main_title,
              main.title.position = c("center", "top"), aes.palette = list(seq="-RdYlBu"))+
    tm_text(text_title, size=0.5, root = 4)+ 
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col_size, 
               border.col= "black", palette="seq",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"))+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}


# This function maps data  
tmap.fun2 <- function(adminfile, DSmapvalue, adminlegtitle, title) {
  tm_shape(adminfile) + #this is the health district shapfile with LLIn info
    tm_polygons(col = DSmapvalue, textNA = "No data", 
                title = adminlegtitle, palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(title = title, aes.palette = list(seq="-RdYlBu"))
}


#Another function for mapping data 
tmap.fun3 <- function(DSshape, colname, legtitle, maintitle, ptsfile, bubble_size, bubble_col_size){
  tm_shape(DSshape) + #this is the health district shapefile with test result info
    tm_polygons(col = colname, textNA = "No data", 
                title = legtitle, palette = "RdYlBu", breaks=c(0, 0.1, 0.2, 
                                                               0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
    tm_layout(title=maintitle) +
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col_size, 
               border.col= "black", palette="RdYlBu",
               breaks=c(0, 0.1, 0.2, 
                        0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0), legend.col.show=F)+
    tm_legend(legend.title.size = 0.8, legend.just="top") 
}


#Another function for mapping data 
tmap.fun4 <- function(DSshape, maintitle, legtitle, colname){
  tm_shape(DSshape) + #this is the health district shapefile with test result info
    tm_polygons(col = colname, textNA = "No data", 
                title = legtitle, palette = "seq", breaks=c(0, 0.1, 0.2, 
                                                            0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
    tm_layout(title=maintitle, aes.palette = list(seq="-RdYlBu")) 
}


#Another function for mapping data 
tmap.fun5 <- function(DSshape, maintitle, text_title){
  tm_shape(DSshape) + #this is the health district shapefile with test result info
    tm_polygons()+
    tm_layout(title=maintitle) + 
    tm_text(text_title, size=0.5, root = 4)
}


#Another function for mapping data 
tmap.fun6 <- function(DSshape, colname, legtitle, maintitle, ptsfile, bubble_size, bubble_col_size, text_title){
  tm_shape(DSshape) + #this is the health district shapefile with test result info
    tm_polygons(col = colname, textNA = "No data", 
                title = legtitle, palette = "RdYlBu", breaks=c(0, 0.1, 0.2, 
                                                               0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
    tm_layout(title=maintitle) +
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col_size, 
               border.col= "black", palette="RdYlBu",
               breaks=c(0, 0.1, 0.2, 
                        0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0), legend.col.show=F)+
    tm_legend(legend.title.size = 0.8, legend.just="top") +
    tm_shape(DSshape) +
    tm_borders() +
    tm_text(text_title, size=0.8, root = 4)
}


#cluster mapping
tmap.clu <- function(adminfile,ptsfile, bubble_size, bubble_col, title, na_ptsfile) {
  tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col,
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_shape(na_ptsfile)+
    tm_bubbles(size = 0.3, textNA = "Missing", legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="RdYlBu"), title=title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}


tmap.clu2 <- function(adminfile,ptsfile, bubble_size, bubble_col, title) {
  tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col, 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}


tmap.clu3 <- function(adminfile,ptsfile, bubble_size, bubble_col, title) {
  tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col, 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_shape(ptsfile[is.na(ptsfile$p_test),])+
    tm_bubbles(textNA = "Missing", legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title=title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}


tmap.clu4 <- function(adminfile,ptsfile, bubble_size, bubble_col, title) {
  tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col, 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}


max.fun <- function(data){
  data %>% mutate(norm_cases = sum_cases/max(sum_cases))
}


clean.xls <- function(data){
  x <- str_split(data[[1,1]], "=", simplify = 'TRUE')[,2]
  data$p_test <- x[[1]]
  y <- data %>% dplyr::select(!!c(1, 2, 4, 14, 16, 17, 18))
  y[[2,1]] <- "cat"
  y[[2,3]] <- "frequency"
  y[[2,4]] <- "PfPr_microscopy"
  y$...16[[2]] <- "ci_l"
  y$...17[[2]] <- "ci_u"
  y$p_test[[2]] <- "repDS"
  names(y) <-y[2, ]
  y <- y[c(7,8, 9), ]
  y <- y %>% fill(cat)
}



# ACT functions 

generate.ACT.state_LGA_repDS <- function(df, var){
  df1<-dataclean(df, ml13e, v005, 'ml13e', 'comboACT')  
  svyd <- svydesign.fun(df1)
  #generate LGA estimates 
  df2 <- result.fun('comboACT', var,design=svyd, data =df1)  %>% fill(year, .direction = "updown")
  if(var == "LGA"){
  df2 <- df2 %>% mutate(LGA = case_when(LGA == "kiyawa"~ "Kiyawa", LGA == "kaita" ~"Kaita", TRUE ~ LGA)) %>% fill(year, .direction = "updown")
  }else{
    print("no name corrections for state, repDs or regiona estimates")
  }
  repDS_LGA %>% left_join(df2)
}




ACT.fun_region <- function(df){
  df1<-dataclean(df, ml13e, v005, 'ml13e', 'comboACT')  
  svyd <- svydesign.fun(df1)
  #generate LGA estimates 
  df2 <- result.fun('comboACT', 'v024',design=svyd, data =df1)
  repDS_region %>%  left_join(df2)
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
