
#This function read in all files with three different file patterns in all folders and subfolders in the working directory

read.files <- function(filepat1, filepat2, filepat3) {
  in_files_I <- list.files(path = "data", pattern = filepat1, recursive = TRUE, full.names = TRUE)
  in_files_K <- list.files(path = "data", pattern =  filepat2, recursive = TRUE, full.names = TRUE)
  in_files_P <- list.files(path = "data", pattern = filepat3, recursive = TRUE, full.names = TRUE)
  filenames <- rbind(in_files_I, in_files_K, in_files_P)
  sapply(filenames, read_dta, simplify = F)
}




# This function recodes x = 8 as NA, x = 3 or 0 as 0 and any remaining values of x to 1
recoder.ml0 <- function(data){
  data %>% mutate(ml0 = ifelse(ml0 > 6, NA,ifelse(ml0 == 0, 0, 1)))
}

recoder <- function(x){
  ifelse(x > 6, NA,ifelse(x == 0, 0, 1))
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
  ifelse(x > 97, NA,ifelse(x > 9, 0, 1))
}


recoder4 <- function(x){
  ifelse(x > 97, NA,ifelse(x > 32, 0, 1))
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
  data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=v005/1000000,strat=v022,
           id=v021, num_p=1) 
}


#function for cleaning other files 
dataclean.HH<-function(data, filter_var, filter_var1, cols, new_col){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=hv005/1000000,strat=hv022,
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
  
  ptest_num_ym<<- data%>%dplyr::select(v001, time2, num_p)%>%na.omit(data)%>%group_by(v001, time2) %>% summarise_each(funs(mean, sd, std.error, n())) %>% 
  dplyr::select(-mean, -sd, -std.error)
  
  ptest%>%left_join(ptest_num_ym)%>% rename(PfPr = var, `Number of Kids` = n, DHSCLUST = v001)
  
}


#generating results and table function with estimates 
result.fun<- function(var, var1, var2, design) {
  
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "ci", na.rm=T) 
  
  num_est<-svyby(formula=make.formula(var2), by=make.formula(var1), FUN=svytotal, design, na.rm=T)%>% 
    dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
  
  p_est%>%left_join(num_est)%>% rename(`Number of Participants` = num_p)
  
}


#generating results and table function with estimates 
result.clu.fun<- function(var, var1, design, data) {
  
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) 
  
  num_est <- data%>%dplyr::select(v001, num_p)%>%na.omit(data)%>%group_by(v001) %>% summarise_each(funs(mean, sd, std.error, n()))%>% 
    dplyr::select(-mean, -sd, -std.error)
  
  p_est%>%left_join(num_est)%>% rename(`Number of Participants` = n, DHSCLUST = v001)
  
}



#over function 
over.fun <- function(df) {
  over(SpatialPoints(coordinates(df),proj4string = df@proj4string), LGAshp)
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
                title = legtitle, palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                               0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(title=maintitle) +
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col_size, 
               border.col= "black", palette="RdYlBu",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}

max.fun <- function(data){
  data %>% mutate(norm_cases = sum_cases/max(sum_cases))
}
