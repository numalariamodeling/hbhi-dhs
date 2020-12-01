

recoder.pfpr <- function(x){
  ifelse(x > 6, NA,ifelse(x == 0|x == 6, 0, 1))
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


result.fun<- function(var, var1,design, data) { #year,var2,
  year <- unique(na.omit(data$hv007))
  p_est<-survey::svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) # svyciprop, method ='logit', levels=0.95, vartype= "se"
  
 # num_est<-survey::svyby(formula=make.formula(var2), by=make.formula(var1), FUN=svytotal, design, na.rm=T)%>% 
    #dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
  
  #p_num <-p_est%>%left_join(num_est)%>% rename(`Number of Participants` = num_p)
  
  #year <- data.frame(year = unique(data[,year]))
  
  cbind(p_est, year)
  
}


generate.PR.state_LGA_repDS <- function(df, var1, var2){
  df1<-dataclean.para(df, hv005, hc1, hml32, 'hml32', 'p_test') 
  svyd <- svydesign.fun(df1)
  #generate LGA estimates 
  df2 <- result.fun('p_test', var1,design=svyd, data =df1) 
  # if(var1 == "LGA"){
  #   df2 <- df2 %>% mutate(LGA = case_when(LGA == "kiyawa"~ "Kiyawa", LGA == "kaita" ~"Kaita", TRUE ~LGA)) 
  # }else{
  #   print("no name corrections for state, repDs or regional estimates")
  # }
  # #repDS_LGA %>% left_join(df2)%>% tidyr::fill(year, .direction = "updown")
}


#