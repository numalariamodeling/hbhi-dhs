#This function read in all files with three different file patterns in all folders and subfolders in the working directory

read.files <- function(filepat1, filepat2, filepat3) {
  in_files_I <- list.files(path = "data", pattern = filepat1, recursive = TRUE, full.names = TRUE)
  in_files_K <- list.files(path = "data", pattern =  filepat2, recursive = TRUE, full.names = TRUE)
  in_files_P <- list.files(path = "data", pattern = filepat3, recursive = TRUE, full.names = TRUE)
  filenames <- rbind(in_files_I, in_files_K, in_files_P)
  sapply(filenames, read_dta, simplify = F)
}


#This function takes an admin boundary and a point shape file and plots them on top of each other 
raw.plot.fun <- function(data1,data2, adtitle){
  plot(data1, main=adtitle)
  plot(data2,add=T,col=4) 
}


#over function 
over.fun <- function(df) {
  over(SpatialPoints(coordinates(df),proj4string = df@proj4string), DS_shape)
}

#This function uses the person recode dataset to create the month of the survey in the format mm-yyyy and mm-dd-yyyy. it also recides v001 to hv001 
survey.month.fun <- function(data) {
  data%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% #dhs pr 2010
    dplyr::rename(v001 = hv001) %>%
    mutate(YYYY = (floor((hv008 - 1)/12)+1900))%>%
    mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
    mutate(time2 = str_c(MM, YYYY, sep = '-'))
} 


# This function recodes x = 8 as NA, x = 3 or 0 as 0 and any remaining values of x to 1
recoder <- function(x){
  ifelse(x == 8, NA,ifelse(x == 3 | x == 0, 0, 1))
}

# This function recodes x = 6 as NA, X = 4 as or andy remaining values of x to 1
recoder2 <- function(x){
  ifelse(x == 6, NA,ifelse(x == 4, 0, 1))
}


recoder3 <- function(x){
  ifelse(x > 3, NA,ifelse(x == 0, 0, 1))
}

# This function applies 'recoder' to multiple variables 
apply.recoder <-  function(data,var){
  data%>%
    mutate_at(vars(contains(var)), recoder)
}


# This function applies 'recoder2' to multiple variables 
apply.recoder2 <-  function(data,var){
  data%>%
    mutate_at(vars(contains(var)), recoder2)
}


recoder.ml4_1 <- function(data){
  data %>% mutate(m14_1 = ifelse(m14_1 > 90, NA,ifelse(m14_1 == 0, 0, 1)))
}


recoder.ml4_1_v2 <- function(data){
  data %>% mutate(m14_1 = ifelse(m14_1 > 90, NA,ifelse(m14_1 < 3, 0, 1)))
}

recoder.m49a <- function(data){
  data %>% mutate(m49a_1 = ifelse(m49a_1 > 7, NA,ifelse(m49a_1 == 0, 0, 1)))
}

recoder.ml1 <- function(data){
  data %>% mutate(ml1_1 = ifelse(ml1_1 > 97, NA,ifelse(ml1_1 < 3, 0, 1)))
}


recoder.ml0 <- function(data){
  data %>% mutate(ml0 = ifelse(ml0 > 8, NA,ifelse(ml0 == 0| ml0 == 3, 0, 1)))
}


recoder.nets <- function(data){
  data %>% mutate(hh_net = hv009/hml1, net_ratio = ifelse(hh_net > 2, 0, 1))
}

recoder.hv227 <- function(data){
  data %>% mutate(hv227 = ifelse(hv227 >8 , NA,ifelse(hv227 == 0, 0, 1)))
}


recode_itn <- function(data){
  data %>% mutate(hh.itn= ifelse(hml12 == "1"| hml12 == "2"| hml12 == "3", 1, 0))
}

# This function takes a dhs IR dataset and selects the U5 related variables in short format and converts to long format 
short_to_long.fun <- function(data){
  data%>%dplyr::select(b2_01:b2_04, ml0_1:ml0_4,h22_1:h22_4,h32z_1:h32z_4, ml13e_1:ml13e_4,
                                   caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
    mutate_at(vars(contains('h22')), as.numeric)%>%
    mutate_at(vars(contains('h32z')), as.numeric)%>%
    mutate_at(vars(contains('ml13e')), as.numeric)%>%
    pivot_longer(cols = ml0_1:ml13e_3, names_to = "var", values_to = "count")
}
  
  
# This function takes a long dataset and selects LLIN values only for child 1 - 4 and creates LLIN categorical variable 
create.data <- function(data, var1, var2, var3, var4, cols, new_col){
    data1<-data[data$var == var1|data$var == var2|data$var == var3|data$var == var4,] %>%
      mutate(wt=v005/1000000,strat=v022,
             id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by clusters
      filter(!is.na(wt)) %>% 
      filter(!is.na(count))
    data2<-rename_(data1, .dots = setNames(cols, new_col))
}      
    


# This function creates the design variable list for any DHS dataset 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


# This function computes survey adjusted prevalence values at level using the HT formula. Remember to specify function name 
svyby.fun <- function(var, var2, design, namefun ) {
  svyby(make.formula(var),# which variable do we want estimate
        make.formula(var2), # by which variable
        FUN = namefun, # what is the function (svymean gives the horvitz thompson estimate and variance)
        design, # my svy design object
        na.rm=T)
}

# This function merges the number of kids in each cluster with any dataset and renames three variables 
merge.rename.fun <- function(data, data4, cols, new_col, cols1, new_col1, cols2, new_col2) {
  data1<-rename_(data, .dots = setNames(cols, new_col)) 
  data2<-rename_(data1, .dots = setNames(cols1, new_col1))
  data3<-rename_(data2, .dots = setNames(cols2, new_col2))
  data4%>% 
    left_join(data3)
}

# This function maps data  
tmap.fun <- function(adminfile, DSmapvalue, adminlegtitle, main_title, text_title, ptsfile, bubble_size, bubble_col_size) {
  tm_shape(adminfile) + #this is the health district shapfile with LLIn info
    tm_polygons(col = DSmapvalue, textNA = "No data", 
                title = adminlegtitle, palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(main.title= main_title,
              main.title.position = c("center", "top"))+
    tm_text(text_title, size=0.5, root = 4)+ 
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col_size, 
               border.col= "black", palette="RdYlBu",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}


# This function maps data  
tmap.fun1 <- function(adminfile, DSmapvalue, adminlegtitle, main_title, text_title, ptsfile, bubble_size, bubble_col_size) {
  tm_shape(adminfile) + #this is the health district shapfile with LLIn info
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




#Another function for mapping data 
tmap.fun2 <- function(DSshape, title){
  tm_shape(DSshape) + #this is the health district shapefile with test result info
    tm_polygons(col = "p_test", textNA = "No data", 
                title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(aes.palette = list(seq = "-RdYlBu"), title=title)
}

#Another function for mapping data 
tmap.fun3 <- function(DSshape, title, col){
  tm_shape(DSshape) + #this is the health district shapefile with test result info
    tm_polygons(col = col, textNA = "No data", 
                title = 'ITN Use Adult Women', palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                     0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(title=title) 
}

#Another function for mapping data 
tmap.fun4 <- function(DSshape, maintitle, legtitle, colname){
  tm_shape(DSshape) + #this is the health district shapefile with test result info
    tm_polygons(col = colname, textNA = "No data", 
                title = legtitle, palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                               0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(main.title=maintitle, main.title.position = c("center", "top")) + 
    tm_text("NOMDEP", size=0.5, root = 4)
}


#Another function for mapping data 
tmap.fun5 <- function(DSshape, colname, legtitle, maintitle, ptsfile, bubble_size, bubble_col_size){
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


dataclean.HH<-function(data, filter_var, filter_var1, cols, new_col){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=hv005/1000000,strat=hv022,
           id=hv021, num_p=1) 
}


dataclean2<-function(data, filter_var, filter_var1, cols, new_col, strata){
  filter_var <- enquo(filter_var)
  filter_var1 <- enquo(filter_var1)
  data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
  data5<-rename_(data, .dots = setNames(cols, new_col))  
  data5%>%   
    mutate(wt=v005/1000000,strat=strata,
           id=v021, num_p=1) 
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

dataclean.para <- function(data, cols, new_col, y){
  data5<-rename_(data, .dots = setNames(cols, new_col)) 
  data5%>%  
    mutate(wt=hv005/1000000,strat=hv022,
           id=hv021, num_p=y)%>% #column for the number of persons so that we can compute the totals by cluster 
    filter(!is.na(wt)) %>% 
    filter(!is.na(hc1))
}


#generating results and table function with estimates 
result.fun<- function(var, var1, var2, design) {
  
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "ci", na.rm=T) 
  
  num_est<-svyby(formula=make.formula(var2), by=make.formula(var1), FUN=svytotal, design, na.rm=T)%>% 
    dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
  
  p_est%>%left_join(num_est)%>% rename(`Number of Participants` = num_p)
  
}


result.fun.HH<- function(var, var1, var2, design) {
  
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "ci", na.rm=T) 
  
  num_est<-svyby(formula=make.formula(var2), by=make.formula(var1), FUN=svytotal, design, na.rm=T)%>% 
    dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
  
  p_est%>%left_join(num_est)%>% rename(`Number of Households` = num_p)
  
}


result.clu.fun<- function(var, var1, design, data) {
  
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) 
  
  num_est <- data%>%dplyr::select(v001, num_p)%>%na.omit(data)%>%group_by(v001) %>% summarise_each(funs(mean, sd, std.error, n()))%>% 
    dplyr::select(-mean, -sd, -std.error)
  
  p_est%>%left_join(num_est)%>% rename(`Number of Participants` = n, DHSCLUST = v001)
  
}


result.clu.fun.HH<- function(var, var1, design, data) {
  
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) 
  
  num_est <- data%>%dplyr::select(hv001, num_p)%>%na.omit(data)%>%group_by(hv001) %>% summarise_each(funs(mean, sd, std.error, n()))%>% 
    dplyr::select(-mean, -sd, -std.error)
  
  p_est%>%left_join(num_est)%>% rename(`Number of Households` = n, DHSCLUST = v001)
  
}

