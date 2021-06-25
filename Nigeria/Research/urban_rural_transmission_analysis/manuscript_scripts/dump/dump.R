# next we estimate proportion of people in high SES by cluster
# recode the weath quintile variable 
table(pfpr_df$hv106)

pfpr_wealth <- pfpr_df %>%  mutate(wealth = ifelse(hv270 <4, 0, 1))
table(pfpr_wealth$wealth)

pfpr_wealth<- funEnv$dataclean.para(pfpr_wealth, hv005, hv005, wealth, 'wealth', 'wealth_2') 
table(pfpr_wealth$wealth_2)

svyd_wealth<- funEnv$svydesign.fun(pfpr_wealth)

clu_wealth <- funEnv$result.fun('wealth_2', 'hv001', design=svyd_wealth, pfpr_wealth, "hv007")
head(clu_wealth)



#Housing quality
#Estimate the proportion of clusters with good floor quality 
pfpr_floor<- pfpr_df %>%  mutate(floor_type = ifelse(hv213 == 30| hv213 == 31|
                                                       hv213 == 33| hv213 == 34|
                                                       hv213 == 35,1, 0))
table(pfpr_floor$floor_type)

pfpr_floor<- funEnv$dataclean.para(pfpr_floor, hv005, hv005, floor_type, 'floor_type', 'house_floor') 
table(pfpr_floor$house_floor)

svyd_floor<- funEnv$svydesign.fun(pfpr_floor)

clu_floor <- result.fun('house_floor', 'hv001', design=svyd_floor, pfpr_floor, "hv007")
head(clu_floor)


#Estimate the proportion of clusters with good wall quality 

table(pfpr_wall$wall_type)

pfpr_wall<- funEnv$dataclean.para(pfpr_wall, hv005, hv005, wall_type, 'wall_type', 'house_wall') 
table(pfpr_wall$house_wall)

svyd_wall<- funEnv$svydesign.fun(pfpr_wall)

clu_wall <- result.fun('house_wall', 'hv001', design=svyd_wall, pfpr_wall, "hv007")
head(clu_wall)

#Estimate the proportion of clusters with good roof quality 
pfpr_roof <- pfpr_df %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
                                                      hv215 == 33| hv215 == 34|
                                                      hv215 == 35|hv215 == 36,1, 0))
table(pfpr_roof$roof_type)

pfpr_roof<- funEnv$dataclean.para(pfpr_roof, hv005, hv005, roof_type, 'roof_type', 'house_roof') 
table(pfpr_roof$house_roof)

svyd_roof<- funEnv$svydesign.fun(pfpr_roof)

clu_roof <- result.fun('house_roof', 'hv001', design=svyd_roof, pfpr_roof, "hv007")
head(clu_roof)


#Estimate of the proportion of clusters with good housing quality
pfpr_housing<- pfpr_df %>%  mutate(floor_type = ifelse(hv213== 30| hv213 == 31|
                                                         hv213 == 33| hv213 == 34|
                                                         hv213 == 35|hv213 == 36,1, 0))

pfpr_housing<- pfpr_housing %>%  mutate(wall_type = ifelse(hv214 == 30| hv214 == 31|
                                                             hv214 == 33| hv214 == 34|
                                                             hv214 == 35|hv214 == 36,1, 0))


pfpr_housing <- pfpr_housing %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
                                                              hv215 == 33| hv215 == 34|
                                                              hv215 == 35|hv215 == 36,1, 0))

pfpr_housing_q <- pfpr_housing %>%  mutate(housing_q = ifelse(floor_type == 1 &
                                                                wall_type == 1 &
                                                                roof_type == 1,1, 0))

table(pfpr_housing_q$housing_q)

pfpr_housing_q<- funEnv$dataclean.para(pfpr_housing_q, hv005, hv005, housing_q, 'housing_q', 'housing_qua') 
table(pfpr_housing_q$housing_qua)

svyd_housing_q<- funEnv$svydesign.fun(pfpr_housing_q)

clu_housing_q <- result.fun('housing_qua', 'hv001', design=svyd_housing_q, pfpr_housing_q, "hv007")
head(clu_housing_q)


#net use 
pfpr_itn_use <- pfpr_data%>%  
  mutate(net_use_1 = ifelse(hml12 %in% c(1,2), 1,0))

table(pfpr_itn_use$net_use_1)


pfpr_itn_use<- dataclean.para(pfpr_itn_use, hv005, hv005, net_use_1, 'net_use_1', 'net_use') 
table(pfpr_itn_use$net_use)

svyd_itn_use <- svydesign.fun(pfpr_itn_use)

clu_pfpr_itn_use <- result.fun('net_use', 'hv001', design=svyd_itn_use, pfpr_itn_use, "hv007")
head(clu_pfpr_itn_use)


# next estimate the gender proportion in each cluster 
look_for(dhs[[1]], "sex")
table(dhs[[1]]$hc27)

pfpr_sex <- pfpr_dhs %>%  mutate(sex = ifelse(hc27 == 1,0, 1))# this doesn't estimate gender proportions 
pfpr_sex <- dataclean.para(pfpr_sex, hv005, hv005, hc27, 'sex', 'sex_f') 
table(pfpr_sex$sex_f)


svyd_sex <- svydesign.fun(pfpr_sex)

clu_sex <- result.fun('sex_f', 'hv001', design=svyd_sex, pfpr_sex, "hv007")
head(clu_sex)



#dhs_ir <- read.files(DataDir, "*NGIR.*\\.DTA", 'NGIR7AFL|NGIR71FL|NGIR61FL', read_dta) #reads in the IR files for education 

#rm(dhs_ir)

#look_for(dhs_ir[[3]], "medicine")

# dhs_pr <- dhs_pr %>% map(~dplyr::select(., hv001, hv042, hv103,  hc1, hml32, hml16, hv025, hv270, hv213, hv214, hv215, hc27,
#                                  hv005, hml12,hv022, hv021, hml10))
# 
# 
# list.save(dhs_pr, file=file.path(DataDir, 'dhs_pr_list_pfpr.rds'))
#rm(dhs_pr)
#dhs_pr <-list.load(file=file.path(DataDir, 'dhs_pr_list_pfpr.rds'))


# dataclean.para <-function(data, filter_var, filter_var1, filter_var2, cols, new_col){
#   # filter_var <- enquo(filter_var)
#   # filter_var1 <- enquo(filter_var1)
#   # filter_var2 <- enquo(filter_var2)
#   # data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1)) %>% filter(!is.na(!!filter_var2))
#   data5<-rename_(data, .dots = setNames(cols, new_col))  
#   data5%>%   
#     mutate(wt=hv005/1000000,strat=hv022,
#            id=hv021, num_p=1)
# } 


# # map_sim results
# map_fun <- function(shpfile, map_val) {
#   number <- unique(na.omit(shpfile$meta_information))
#   tm_shape(shpfile) + #this is the health district shapfile with LLIn info
#     tm_polygons(col = map_val, textNA = "No data", 
#                 title = "", palette = "seq", breaks=c(0, 0.1, 0.2, 
#                                                       0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
#     tm_layout(title = paste0(number,  " " , "State U5 PfPR 2025"),
#               aes.palette = list(seq="RdYlBu")) 
# }
# 
# 
# mutate_f <- function(data, col_1, col_2, col_3) { 
#   mutate(data, wt=!!col_1/1000000,strat=!!col_2, id=!!col_3, num_p=1) 
# }
# 
# # This function recodes x = 8 as NA, x = 3 or 0 as 0 and any remaining values of x to 1
# recoder.ml0 <- function(data){
#   data %>% mutate(ml0 = ifelse(ml0 > 6, NA,ifelse(ml0 == 0, 0, 1)))
# }
# 
# recoder.wealth <- function(data, col){
#   name_new <- paste0(quo_name(col), "_new")
#   mutate(data, !!name_new := ifelse(!!col < 4, 0, 1))
# }
# 
# recoder.gen <- function(data, col){
#   name_new <- paste0(quo_name(col), "_new")
#   mutate(data, !!name_new := ifelse(!!col > 7, 0, ifelse(!!col == 0, 0, 1)))
# }
# 
# recoder.ml101 <- function(data){
#   data %>% mutate(ml101 = ifelse(ml101 > 6, NA,ifelse(ml101 == 0 | ml101 ==3, 0, 1)))
# }
# 
# recoder <- function(x){
#   ifelse(x > 6, NA,ifelse(x == 0, 0, 1))
# }
# 
# recoder.pfpr <- function(x){
#   ifelse(x > 6, NA,ifelse(x == 0|x == 6, 0, 1))
# }
# 
# 
# # This function applies 'recoder' to multiple variables 
# apply.recoder <-  function(data,var){
#   data%>%
#     mutate_at(vars(contains(var)), recoder)
# }
# 
# recoder2 <- function(x){
#   ifelse(x > 6, NA,ifelse(x == 0 | x ==3, 0, 1))
# }
# 
# 
# 
# recoder3 <- function(x){
#   ifelse(x > 97, NA,ifelse(x > 7, 0, 1))
# }
# 
# 
# recoder4 <- function(x){
#   ifelse(x > 97, NA,ifelse(x > 24, 0, 1))
# }
# 
# recoder5 <- function(x){
#   ifelse(x > 6, NA,ifelse(x == 2, 0, 1))
# }
# 
# 
# recoder.ml4_1 <- function(data){
#   data %>% mutate(m14_1 = ifelse(m14_1 > 40, NA,ifelse(m14_1 == 0, 0, 1)))
# }
# 
# 
# recoder.ml4_1_v2 <- function(data){
#   data %>% mutate(m14_1 = ifelse(m14_1 > 90, NA,ifelse(m14_1 < 3, 0, 1)))
# }
# 
# recoder.m49a <- function(data){
#   data %>% mutate(m49a_1 = ifelse(m49a_1 > 1, NA,ifelse(m49a_1 == 0, 0, 1)))
# }
# 
# 
# recoder.ml1 <- function(data){
#   data %>% mutate(ml1_1 = ifelse(ml1_1 > 97, NA,ifelse(ml1_1 < 3, 0, 1)))
# }
# 
# 
# #recoder itn
# recode_itn <- function(data) {
#   data %>% mutate(hh_itn = ifelse(hml12 == "9", NA,ifelse(hml12 =="1" | hml12 =="2", 1, 0)))
# }
# 
# #This function takes an admin boundary and a point shape file and plots them on top of each other 
# raw.plot.fun <- function(data1,data2, adtitle){
#   plot(data1, main=adtitle)
#   plot(data2,add=T,col=4) 
# }
# 
# 
# #This function uses the person recode dataset to create the month of the survey in the format mm-yyyy and mm-dd-yyyy. it also recides v001 to hv001 
# survey.month.fun <- function(data) {
#   data%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% #dhs pr 2010
#     dplyr::rename(v001 = hv001) %>%
#     mutate(YYYY = (floor((hv008 - 1)/12)+1900))%>%
#     mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
#     mutate(time2 = str_c(MM, YYYY, sep = '-'))
# } 
# 
# 
# 
# #function for cleaning other files 
# dataclean<-function(data, filter_var, filter_var1, cols, new_col){
#   filter_var <- enquo(filter_var)
#   filter_var1 <- enquo(filter_var1)
#   data <- data %>% filter(!is.na(!!filter_var) | (!is.na(!!filter_var1)))
#   data5<-rename_(data, .dots = setNames(cols, new_col))  
#   data5%>%   
#     mutate(wt=v005/1000000,strat=v022,
#            id=v021, num_p=1) 
# }
# 
# 
# #function for cleaning other files 
# dataclean.HH<-function(data, filter_var, filter_var1, cols, new_col){
#   filter_var <- enquo(filter_var)
#   filter_var1 <- enquo(filter_var1)
#   data <- data %>% filter(!is.na(!!filter_var)) %>% filter (!is.na(!!filter_var1))
#   data5<-rename()(data, .dots = setNames(cols, new_col))  
#   data5%>%   
#     mutate(wt=hv005/1000000,strat=hv022,
#            id=hv021, num_p=1) 
# }
# 
# #function for cleaning mics files 
# dataclean.mics<-function(data, filter_var, filter_var1, cols, new_col){
#   filter_var <- enquo(filter_var)
#   filter_var1 <- enquo(filter_var1)
#   data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
#   data5<-rename()(data, .dots = setNames(cols, new_col))  
#   data5%>%   
#     mutate(wt=chweight,strat=stratum,
#            id=PSU, num_p=1) 
# }
# 
# 
# dataclean2.HH<-function(data, filter_var, filter_var1, cols, new_col, strata){
#   filter_var <- enquo(filter_var)
#   filter_var1 <- enquo(filter_var1)
#   data <- data %>% filter(!is.na(!!filter_var)) %>% filter(!is.na(!!filter_var1))
#   data5<-rename()(data, .dots = setNames(cols, new_col))  
#   data5%>%   
#     mutate(wt=hv005/1000000,strat=strata,
#            id=hv021, num_p=1) 
# }
# 
# #survey design function 
# svydesign.fun <- function(filename){
#   svydesign(id= ~id,
#             strata=~strat,nest=T, 
#             weights= ~wt, data=filename)
# }
# 
# #generating results and table function with estimates 
# result.fun.para <- function(var, var1, var2, var3, design) {
#   
#   target_quo <- parse_quo(var3)
#   
#   ptest<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, vartype= "ci", level =0.75, method ="beta", na.rm=T) %>% arrange(!!target_quo, time2)
#   
#   ptest_num_ym<-svyby(formula=make.formula(var2), by=make.formula(var1), FUN=svytotal, design, na.rm=T)%>% 
#     dplyr:: select(-se)%>%arrange(!!target_quo, time2)%>% mutate(num_p = round(num_p, 0))
#   
#   ptest%>%left_join(ptest_num_ym)%>% rename(PfPr = var,`Number of Kids` = num_p)
#   
# }
# 
# 
# #generating results and table function with estimates 
# result.clu.fun.para <- function(var, var1, var2, var3, design, data) {
#   
#   target_quo <- parse_quosure(var3)
#   
#   ptest<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) %>% arrange(!!target_quo, time2)
#   
#   ptest_num_ym<<- data%>%dplyr::select(hv001, time2, num_p)%>%na.omit(data)%>%group_by(hv001, time2) %>% summarise_each(funs(mean, sd, std.error, n())) %>% 
#   dplyr::select(-mean, -sd, -std.error)
#   
#   ptest%>%left_join(ptest_num_ym)%>% rename(PfPr = var, `Number of Kids` = n, DHSCLUST = hv001)
#   
# }
# 
# 
# 
# 
# #generating results and table function with estimates 
# result.clu.fun<- function(var, var1, design, data, year) {
#   
#   p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, na.rm=T) 
#   
#   num_est <- data%>%drop_na(var)%>%dplyr::select(v001, num_p)%>%group_by(v001) %>% summarise_each(funs(mean, sd, std.error, n()))%>% 
#     dplyr::select(-mean, -sd, -std.error)
#   
#   df <- p_est%>%left_join(num_est)%>% rename(`Number of Participants` = n, DHSCLUST = v001)
#   
#   year <- data.frame(year = unique(data[,year]))
#   
#   cbind(df, year)
#   
# }
# 
# 
# 
# #over function 
# over.fun <- function(df) {
#   over(SpatialPoints(coordinates(df),proj4string = df@proj4string), LGAshp)
# }
# 
# 
# # This function maps data  
# tmap.fun1 <- function(adminfile, DSmapvalue, adminlegtitle, main_title, text_title, ptsfile, bubble_size, bubble_col_size) {
#   tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
#     tm_polygons(col = DSmapvalue, textNA = "No data", 
#                 title = adminlegtitle, palette = "seq", breaks=c(0, 0.2, 0.3, 
#                                                                  0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
#     tm_layout(main.title= main_title,
#               main.title.position = c("center", "top"), aes.palette = list(seq="-RdYlBu"))+
#     tm_text(text_title, size=0.5, root = 4)+ 
#     tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
#     tm_bubbles(size=bubble_size, col = bubble_col_size, 
#                border.col= "black", palette="seq",
#                breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
#     tm_layout(aes.palette = list(seq ="-RdYlBu"))+
#     tm_legend(legend.title.size = 0.8, legend.just="top")
# }
# 
# 
# # This function maps data  
# tmap.fun2 <- function(adminfile, DSmapvalue, adminlegtitle, title) {
#   tm_shape(adminfile) + #this is the health district shapfile with LLIn info
#     tm_polygons(col = DSmapvalue, textNA = "No data", 
#                 title = adminlegtitle, palette = "seq", breaks=c(0, 0.2, 0.3, 
#                                                                  0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
#     tm_layout(title = title, aes.palette = list(seq="-RdYlBu"))
# }
# 
# 
# #Another function for mapping data 
# tmap.fun3 <- function(DSshape, colname, legtitle, maintitle, ptsfile, bubble_size, bubble_col_size){
#   tm_shape(DSshape) + #this is the health district shapefile with test result info
#     tm_polygons(col = colname, textNA = "No data", 
#                 title = legtitle, palette = "RdYlBu", breaks=c(0, 0.1, 0.2, 
#                                                                0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
#     tm_layout(title=maintitle) +
#     tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
#     tm_bubbles(size=bubble_size, col = bubble_col_size, 
#                border.col= "black", palette="RdYlBu",
#                breaks=c(0, 0.1, 0.2, 
#                         0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0), legend.col.show=F)+
#     tm_legend(legend.title.size = 0.8, legend.just="top") 
# }
# 
# 
# #Another function for mapping data 
# tmap.fun4 <- function(DSshape, maintitle, legtitle, colname){
#   tm_shape(DSshape) + #this is the health district shapefile with test result info
#     tm_polygons(col = colname, textNA = "No data", 
#                 title = legtitle, palette = "seq", breaks=c(0, 0.1, 0.2, 
#                                                             0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
#     tm_layout(title=maintitle, aes.palette = list(seq="-RdYlBu")) 
# }
# 
# 
# #Another function for mapping data 
# tmap.fun5 <- function(DSshape, maintitle, text_title){
#   tm_shape(DSshape) + #this is the health district shapefile with test result info
#     tm_polygons()+
#     tm_layout(title=maintitle) + 
#     tm_text(text_title, size=0.5, root = 4)
# }
# 
# 
# #Another function for mapping data 
# tmap.fun6 <- function(DSshape, colname, legtitle, maintitle, ptsfile, bubble_size, bubble_col_size, text_title){
#   tm_shape(DSshape) + #this is the health district shapefile with test result info
#     tm_polygons(col = colname, textNA = "No data", 
#                 title = legtitle, palette = "RdYlBu", breaks=c(0, 0.1, 0.2, 
#                                                                0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
#     tm_layout(title=maintitle) +
#     tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
#     tm_bubbles(size=bubble_size, col = bubble_col_size, 
#                border.col= "black", palette="RdYlBu",
#                breaks=c(0, 0.1, 0.2, 
#                         0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0), legend.col.show=F)+
#     tm_legend(legend.title.size = 0.8, legend.just="top") +
#     tm_shape(DSshape) +
#     tm_borders() +
#     tm_text(text_title, size=0.8, root = 4)
# }
# 
# 
# #cluster mapping
# tmap.clu <- function(adminfile,ptsfile, bubble_size, bubble_col, title, na_ptsfile) {
#   tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
#     tm_polygons()+
#     tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
#     tm_bubbles(size=bubble_size, col = bubble_col,
#                border.col= "black", palette="seq",textNA = "Missing",
#                breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
#     tm_shape(na_ptsfile)+
#     tm_bubbles(size = 0.3, textNA = "Missing", legend.col.show=T)+
#     tm_layout(aes.palette = list(seq ="RdYlBu"), title=title)+
#     tm_legend(legend.title.size = 0.8, legend.just="top")
# }
# 
# 
# tmap.clu2 <- function(adminfile,ptsfile, bubble_size, bubble_col, title) {
#   tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
#     tm_polygons()+
#     tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
#     tm_bubbles(size=bubble_size, col = bubble_col, 
#                border.col= "black", palette="seq",textNA = "Missing",
#                breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
#     tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
#     tm_legend(legend.title.size = 0.8, legend.just="top")
# }
# 
# 
# tmap.clu3 <- function(adminfile,ptsfile, bubble_size, bubble_col, title) {
#   tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
#     tm_polygons()+
#     tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
#     tm_bubbles(size=bubble_size, col = bubble_col, 
#                border.col= "black", palette="seq",textNA = "Missing",
#                breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
#     tm_shape(ptsfile[is.na(ptsfile$p_test),])+
#     tm_bubbles(textNA = "Missing", legend.col.show=T)+
#     tm_layout(aes.palette = list(seq ="-RdYlBu"), title=title)+
#     tm_legend(legend.title.size = 0.8, legend.just="top")
# }
# 
# 
# tmap.clu4 <- function(adminfile,ptsfile, bubble_size, bubble_col, title) {
#   tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
#     tm_polygons()+
#     tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
#     tm_bubbles(size=bubble_size, col = bubble_col, 
#                border.col= "black", palette="seq",textNA = "Missing",
#                breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
#     tm_layout(aes.palette = list(seq ="RdYlBu"), title = title)+
#     tm_legend(legend.title.size = 0.8, legend.just="top")
# }
# 
# 
# max.fun <- function(data){
#   data %>% mutate(norm_cases = sum_cases/max(sum_cases))
# }
# 
# 
# clean.xls <- function(data){
#   x <- str_split(data[[1,1]], "=", simplify = 'TRUE')[,2]
#   data$p_test <- x[[1]]
#   y <- data %>% dplyr::select(!!c(1, 2, 4, 14, 16, 17, 18))
#   y[[2,1]] <- "cat"
#   y[[2,3]] <- "frequency"
#   y[[2,4]] <- "PfPr_microscopy"
#   y$...16[[2]] <- "ci_l"
#   y$...17[[2]] <- "ci_u"
#   y$p_test[[2]] <- "repDS"
#   names(y) <-y[2, ]
#   y <- y[c(7,8, 9), ]
#   y <- y %>% fill(cat)
# }



#function for cleaning parasitemia datasets 
dataclean.para <-function(data, cols, new_col){
  df%>%   
    mutate(wt=hv005/1000000,strat=hv022,
           id=hv021, num_p=1)
} 



# net_df <- dhs_pr %>%  map(~mutate(., net_use = ifelse(hml12 %in% c(1,2), 1,0),
#                                   wt=hv005/1000000,strat=hv022,
#                                   id=hv021, num_p=1)) %>%
#   map(~dplyr::select(., net_use, wt, strat, id, num_p, hv001))
# 
# list.save(net_df, file=file.path(DataDir, 'net_use_list_pfpr.rds'))


# 
# net_use_2010 <- estim_function(net_df[[1]], 'net_use')
# net_use_2015 <- estim_function(net_df[[2]], 'net_use')
# net_use_2018 <- estim_function(net_df[[3]], 'net_use')
# 
# net_use_all <- rbind(net_use_2010, net_use_2015, net_use_2018)
# write.csv(net_use_all, file =file.path(DataDir, 'urban_malaria_cluster_est', "net_use_DHS_10_15_18.csv"))




# clu_hh_members_age <- svyby(~hh_members_age, ~hv001, design = svyd_hh_members_age, FUN=svymean, ci=TRUE,vartype="ci")
# head(clu_hh_members_age)
# 
# 
# 
# 
# 
# # next estimate the average age in each cluster 
# 
# look_for(dhs[[1]], "age")
# summary(is.na(pfpr_dhs$hv105))
# 
# pfpr_hh_members_age<- dataclean.para(pfpr_data, hv005, hv005, hv105, 'hv105', 'hh_members_age') 
# table(pfpr_hh_members_age$hh_members_age)
# 
# svyd_hh_members_age <- svydesign.fun(pfpr_hh_members_age)
# table(pfpr_hh_members_age$hh_members_age)
# 
# clu_hh_members_age <- svyby(~hh_members_age, ~hv001, design = svyd_hh_members_age, FUN=svymean, ci=TRUE,vartype="ci")
# head(clu_hh_members_age)



# 
# 
# # median household size
# look_for(dhs[[1]], "number")
# 
# table(pfpr_data$hv013)
# 
# pfpr_hh_size<- dataclean.para(pfpr_data, hv005, hv005, hv009, 'hv009', 'hh_size') 
# table(pfpr_hh_size$hh_size)
# 
# svyd_hh_size <- svydesign.fun(pfpr_hh_size)
# table(pfpr_hh_size$hh_size)
# 
# clu_hh_size<- svyby(~hh_size, ~hv001, design = svyd_hh_size, FUN=svyquantile, quantiles=0.5, ci=TRUE,vartype="ci")
# head(clu_hh_size)




#household net use 
df <-list.load(file=file.path(DataDir, 'net_use_list_pfpr.rds'))
col <- list('net_use')
df <- map2(df,col, estim_prop)
df <- plyr::ldply(df) 
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "net_use_DHS_10_15_18.csv"))


#educational attainment 
col <- list('edu_a')
df <- dhs_pr %>% 
  map(~drop_na(.,edu_a))
df <- map2(df ,col, estim_prop)
df <- plyr::ldply(df) 
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "edu_a_DHS_10_15_18.csv"))



#wealth 
col <- list('wealth')
df <- dhs_pr %>% 
  map(~drop_na(.,wealth))
df <- map2(df,col, estim_prop)
df <- plyr::ldply(df) 
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "wealth_DHS_10_15_18.csv"))



#housing quality 
col <- list('housing_q')
df <- dhs_pr %>% 
  map(~drop_na(.,housing_q))
df <- map2(df,col, estim_prop)
df <- plyr::ldply(df) 
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "housing_q_DHS_10_15_18.csv"))



#good floor 
col <- list('floor_type')
df <- dhs_pr %>% 
  map(~drop_na(.,df))
df <- map2(df,col, estim_prop)
df <- plyr::ldply(df) 
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "floor_DHS_10_15_18.csv"))


#good wall 
col <- list('wall_type')
df <- dhs_pr %>% 
  map(~drop_na(.,wall_type))
df <- map2(df,col, estim_prop)
df <- plyr::ldply(df) 
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "wall_DHS_10_15_18.csv"))


#good roof 
col <- list('roof_type')
df <- dhs_pr %>% 
  map(~drop_na(.,roof_type))
df <- map2(df,col, estim_prop)
df <- plyr::ldply(df)
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "roof_DHS_10_15_18.csv"))


#median age within each cluster 
col <- list('age')
df <- dhs_pr %>% 
  map(~drop_na(.,age))
df <- map2(df,col, estim_median)
df <- plyr::ldply(df)
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "median_age_DHS_10_15_18.csv"))


#median household size by cluster 
col <- list('hv013')
df <- dhs_pr %>% 
  map(~drop_na(.,hv013))
df <- map2(df,col, estim_median)
df <- plyr::ldply(df)
write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', "median_hh_size_DHS_10_15_18.csv"))




#pfpr 
col <- list('hml32')
pr_df <- pfpr_df %>% 
  map(~drop_na(.,hml32))
pfpr_clu <- map2(pr_df,col, estim_prop)
pfpr_clu <- plyr::ldply(pfpr_clu)
write.csv(pfpr_clu, file =file.path(DataDir, 'urban_malaria_cluster_est', "pfpr_DHS_10_15_18.csv"))


#proportion of children that are female 
col <- list('sex')
sex_df <- pfpr_df %>% 
  map(~drop_na(.,sex))
sex_clu <- map2(sex_df,col, estim_prop)
sex_clu <- plyr::ldply(sex_clu)
write.csv(sex_clu, file =file.path(DataDir, 'urban_malaria_cluster_est', "child_gender_female_DHS_10_15_18.csv"))

#net use among children that have access 
net_df <- pfpr_df %>% map(~filter(., hml10 == 1))
col <- list('net_use')
net_df <- net_df %>% 
  map(~drop_na(.,net_use))
net_access_clu <- map2(net_df,col, estim_prop)
net_access_clu <- plyr::ldply(net_access_clu)
write.csv(net_access_clu, file =file.path(DataDir, 'urban_malaria_cluster_est', "net_use_access_child_DHS_10_15_18.csv"))


#net use 
col <- list('net_use')
net_df <- pfpr_df %>% 
  map(~drop_na(.,net_use))
net_use_clu <- map2(net_df,col, estim_prop)
net_use_clu <- plyr::ldply(net_access_clu)
write.csv(net_use_clu, file =file.path(DataDir, 'urban_malaria_cluster_est', "net_use_child_DHS_10_15_18.csv"))


# 
# 
# # care seeking proportion among u5 children
# 
# look_for(dhs18_2[[1]], "Artemisinin")
# 
# table(dhs18_2[[1]]$ml13e)
# 
# pfpr_care <- dhs18_2[[1]] %>% filter(b5 == 1  & b19 <  60 & h22 == 1) 
# 
# pfpr_care <- dataclean(pfpr_care, v005, v005,' ml13e', "ACT_use_u5")
# 
# svyd_care <- svydesign.fun(pfpr_care)
# table(pfpr_care$ACT_use_u5)
# 
# clu_u5_care<- result.fun('ACT_use_u5', 'v001', design=svyd_care, pfpr_care, "v007")
# head(clu_u5_care)
# colnames(clu_u5_care)[1]<- "hv001"
# 


# #####################################################################################
# ########################### MIS 2015 Data Extraction ##################################
# 
# # prep dataset for cluster level analysis - we start with urban cluster analysis 
# 
# val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural
# 
# #Urban dataset extraction 
# pfpr_place_15 <- pfpr_df_15 %>% filter(hv025 == 1)
# 
# #Rural dataset extraction 
# #pfpr_dhs <- pfpr_data%>% filter(hv025 == 2)
# 
# #Both Urban and Urban dataset extraction 
# # pfpr_place_15 <- pfpr_df_15
# # pfpr_dhs <- pfpr_data
# 
# 
# # estimate cluster-level malaria prevalence
# 
# pfpr_place_15<- funEnv$dataclean.para(pfpr_place_15, hv005, hc1, hml32, 'hml32', 'p_test') 
# 
# svy_mal <- funEnv$svydesign.fun(pfpr_place_15)
# 
# clu15_est <- funEnv$result.fun('p_test', 'hv001', design=svy_mal, pfpr_place_15, "hv007")
# head(clu15_est)
# 
# # next we estimate proportion of people in high SES by cluster
# # recode the weath quintile variable 
# table(pfpr_df_15$hv106)
# 
# pfpr_wealth <- pfpr_df_15 %>%  mutate(wealth = ifelse(hv270 <4, 0, 1))
# table(pfpr_wealth$wealth)
# 
# pfpr_wealth<- funEnv$dataclean.para(pfpr_wealth, hv005, hv005, wealth, 'wealth', 'wealth_2') 
# table(pfpr_wealth$wealth_2)
# 
# svyd_wealth<- funEnv$svydesign.fun(pfpr_wealth)
# 
# clu15_wealth <- funEnv$result.fun('wealth_2', 'hv001', design=svyd_wealth, pfpr_wealth, "hv007")
# head(clu15_wealth)
# 
# 
# 
# #disagreegate wealth 
# look_for(dhs[[1]], "material")
# table(pfpr_df_15$hv205)
# 
# 
# #Housing quality
# #Estimate the proportion of clusters with good floor quality 
# pfpr_floor<- pfpr_df_15 %>%  mutate(floor_type = ifelse(hv213 == 30| hv213 == 31|
#                                                           hv213 == 33| hv213 == 34|
#                                                           hv213 == 35,1, 0))
# table(pfpr_floor$floor_type)
# 
# pfpr_floor<- funEnv$dataclean.para(pfpr_floor, hv005, hv005, floor_type, 'floor_type', 'house_floor') 
# table(pfpr_floor$house_floor)
# 
# svyd_floor<- funEnv$svydesign.fun(pfpr_floor)
# 
# clu15_floor <- result.fun('house_floor', 'hv001', design=svyd_floor, pfpr_floor, "hv007")
# head(clu15_floor)
# 
# #Estimate the proportion of clu15sters with good wall quality 
# pfpr_wall<- pfpr_df_15 %>%  mutate(wall_type = ifelse(hv214 == 30| hv214 == 31|
#                                                         hv214 == 33| hv214 == 34|
#                                                         hv214 == 35|hv214 == 36,1, 0))
# table(pfpr_wall$wall_type)
# 
# pfpr_wall<- funEnv$dataclean.para(pfpr_wall, hv005, hv005, wall_type, 'wall_type', 'house_wall') 
# table(pfpr_wall$house_wall)
# 
# svyd_wall<- funEnv$svydesign.fun(pfpr_wall)
# 
# clu15_wall <- result.fun('house_wall', 'hv001', design=svyd_wall, pfpr_wall, "hv007")
# head(clu15_wall)
# 
# 
# #Estimate the proportion of clu15sters with good roof quality 
# pfpr_roof <- pfpr_df_15 %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
#                                                          hv215 == 33| hv215 == 34|
#                                                          hv215 == 35|hv215 == 36,1, 0))
# table(pfpr_roof$roof_type)
# 
# pfpr_roof<- funEnv$dataclean.para(pfpr_roof, hv005, hv005, roof_type, 'roof_type', 'house_roof') 
# table(pfpr_roof$house_roof)
# 
# svyd_roof<- funEnv$svydesign.fun(pfpr_roof)
# 
# clu15_roof <- result.fun('house_roof', 'hv001', design=svyd_roof, pfpr_roof, "hv007")
# head(clu15_roof)
# 
# #Estimate of the proportion of clu15sters with good housing quality
# pfpr_housing<- pfpr_df_15 %>%  mutate(floor_type = ifelse(hv213 == 30| hv214 == 31|
#                                                             hv213 == 33| hv214 == 34|
#                                                             hv213 == 35|hv214 == 36,1, 0))
# 
# pfpr_housing<- pfpr_housing %>%  mutate(wall_type = ifelse(hv214 == 30| hv214 == 31|
#                                                              hv214 == 33| hv214 == 34|
#                                                              hv214 == 35|hv214 == 36,1, 0))
# 
# 
# pfpr_housing <- pfpr_housing %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
#                                                               hv215 == 33| hv215 == 34|
#                                                               hv215 == 35|hv215 == 36,1, 0))
# 
# pfpr_housing_q <- pfpr_housing %>%  mutate(housing_q = ifelse(floor_type == 1 &
#                                                                 wall_type == 1 &
#                                                                 roof_type == 1,1, 0))
# 
# table(pfpr_housing_q$housing_q)
# 
# pfpr_housing_q<- funEnv$dataclean.para(pfpr_housing_q, hv005, hv005, housing_q, 'housing_q', 'housing_qua') 
# table(pfpr_housing_q$housing_qua)
# 
# svyd_housing_q<- funEnv$svydesign.fun(pfpr_housing_q)
# 
# clu15_housing_q <- result.fun('housing_qua', 'hv001', design=svyd_housing_q, pfpr_housing_q, "hv007")
# head(clu15_housing_q)
# 
# # next estimate the average age in each clu15ster 
# 
# look_for(dhs[[1]], "age")
# summary(is.na(pfpr_dhs$hv105))
# 
# pfpr_hh_members_age<- dataclean.para(pfpr_data, hv005, hv005, hv105, 'hv105', 'hh_members_age') 
# table(pfpr_hh_members_age$hh_members_age)
# 
# svyd_hh_members_age <- svydesign.fun(pfpr_hh_members_age)
# table(pfpr_hh_members_age$hh_members_age)
# 
# clu15_hh_members_age <- svyby(~hh_members_age, ~hv001, design = svyd_hh_members_age, FUN=svymean, ci=TRUE,vartype="ci")
# head(clu15_hh_members_age)
# 
# # next estimate the gender proportion in each clu15ster 
# look_for(dhs[[1]], "sex")
# table(dhs[[1]]$hc27)
# 
# pfpr_sex <- pfpr_dhs %>%  mutate(sex = ifelse(hc27 == 1,0, 1))
# pfpr_sex <- dataclean.para(pfpr_sex, hv005, hv005, hc27, 'sex', 'sex_f') 
# table(pfpr_sex$sex_f)
# 
# 
# svyd_sex <- svydesign.fun(pfpr_sex)
# 
# clu15_sex <- result.fun('sex_f', 'hv001', design=svyd_sex, pfpr_sex, "hv007")
# head(clu15_sex)
# 
# #URBAN or rural
# look_for(dhs[[1]], "rural")
# table(dhs[[1]]$hv025)
# 
# pfpr_rural<- pfpr_df_15[,c("hv001", "hv025")]
# colnames(pfpr_rural)[2]<- "Rural_urban"
# table(pfpr_rural$Rural_urban)
# pfpr_rural <- unique(pfpr_rural, by = "hv001")
# table(pfpr_rural$Rural_urban)
# 
# # proportion with secondary or greater education 
# look_for(dhs[[1]], "education")
# val_labels(dhs[[1]]$hv106)
# 
# pfpr_h_edu <- pfpr_dhs %>%  mutate(edu_a = ifelse(hv106 <2, 0,ifelse(hv106 == 8, NA, ifelse(hv106 == 2|3, 1, NA)))) %>% drop_na(edu_a)
# table(pfpr_dhs$hv106)
# table(pfpr_h_edu$edu_a)
# 
# pfpr_edu<- dataclean.para(pfpr_h_edu, hv005, hv005, hv106, 'edu_a', 'edu_a') 
# table(pfpr_edu$edu_a)
# 
# svyd_edu <- svydesign.fun(pfpr_edu)
# 
# clu15_edu <- result.fun('edu_a', 'hv001', design=svyd_edu, pfpr_edu, "hv007")
# head(clu15_edu)
# 
# # median household size
# look_for(dhs[[1]], "number")
# 
# table(pfpr_data$hv013)
# 
# pfpr_hh_size<- dataclean.para(pfpr_data, hv005, hv005, hv009, 'hv009', 'hh_size') 
# table(pfpr_hh_size$hh_size)
# 
# svyd_hh_size <- svydesign.fun(pfpr_hh_size)
# table(pfpr_hh_size$hh_size)
# 
# clu15_hh_size<- svyby(~hh_size, ~hv001, design = svyd_hh_size, FUN=svyquantile, quantiles=0.5, ci=TRUE,vartype="ci")
# head(clu15_hh_size)
# 
# # care seeking proportion among u5 children
# 
# look_for(dhs2[[1]], "fever")
# 
# table(dhs2[[1]]$ml13e)
# 
# pfpr_care <- dhs2[[1]] %>% filter(b5 == 1  & b8 <  5 & h22 == 1) 
# 
# pfpr_care <- dataclean(pfpr_care, v005, v005,' ml13e', "ACT_use_u5")
# pfpr_care <- pfpr_care %>% filter(ACT_use_u5 != 9)
# table(pfpr_care$ACT_use_u5)
# 
# svyd_care <- svydesign.fun(pfpr_care)
# table(pfpr_care$ACT_use_u5)
# 
# clu15_u5_care<- result.fun('ACT_use_u5', 'v001', design=svyd_care, pfpr_care, "v007")
# head(clu15_u5_care)
# colnames(clu15_u5_care)[1]<- "hv001"
# 
# # population density
# clu15_pop_den <- read.csv("NGGC7BFL.csv")%>% 
#   dplyr::select(hv001 = DHSCLUST, pop_den = UN_Population_Density_2015)%>% 
#   mutate(l_pop_den = log(pop_den))
# 
# summary(clu15_pop_den$l_pop_den)
# 
# #state names
# 
# states15 <- pfpr_df_15[,c("hv001", "shstate")]
# colnames(states15)[2]<- "state"
# states15 <- as_label(states15)
# 
# #regiosns
# regions15 <- pfpr_df_15[,c("hv001", "hv024")]
# colnames(regions15)[2]<- "region"
# regions15 <- as_label(regions18)
# 
# #KAP proportions 
# #Malaria can be fully cured by medicine 
# dhs2_df<- dhs2[[1]]
# 
# kap_cure_med <- dataclean(dhs2_df, v005, v005, 's509', "kap_cure_med")
# kap_cure_med <- subset(kap_cure_med, kap_cure_med != 8)
# #kap_cure_med$kap_cure_med[kap_cure_med$kap_cure_med == 8] <- 0
# 
# 
# svyd_cure_kap <- svydesign.fun(kap_cure_med)
# table(kap_cure_med$kap_cure_med)
# 
# clu_kap_cure<- result.fun('kap_cure_med', 'v001', design=svyd_cure_kap, kap_cure_med, "v007")
# head(clu_kap_cure)
# colnames(clu_kap_cure)[1]<- "hv001"
# 
# #Malaria can lead to death
# kap_death <- dataclean(dhs2_df, v005, v005, '"s1108ba"', "kap_death")
# kap_death <- subset(mtcars, cyl != 8)
# kap_death$kap_death[kap_death$kap_death == 8] <- 0
# 
# svyd_death_kap <- svydesign.fun(kap_death)
# table(kap_death$kap_death)
# 
# clu_kap_death<- result.fun('kap_death', 'v001', design=svyd_death_kap, kap_death, "v007")
# head(clu_kap_death)
# colnames(clu_kap_death)[1]<- "hv001"
# 
# #No worry about malaria due to easy treatment
# 
# kap_treat <- dataclean(dhs2_df, v005, v005, '"s1108bc"', "kap_treat")
# kap_treat$kap_treat[kap_treat$kap_treat == 8] <- 0
# 
# svyd_treat_kap <- svydesign.fun(kap_treat)
# table(kap_treat$kap_treat)
# 
# clu_kap_treat<- result.fun('kap_treat', 'v001', design=svyd_treat_kap, kap_treat, "v007")
# head(clu_kap_treat)
# colnames(clu_kap_treat)[1]<- "hv001"
# 
# #Know people sick with malaria
# 
# kap_know <- dataclean(dhs2_df, v005, v005, 's501', "kap_know")
# kap_know$kap_know[kap_know$kap_know == 8] <- 0
# 
# svyd_know_kap <- svydesign.fun(kap_know)
# table(kap_know$kap_know)
# 
# clu_kap_know<- result.fun('kap_know', 'v001', design=svyd_know_kap, kap_know, "v007")
# head(clu_kap_know)
# colnames(clu_kap_know)[1]<- "hv001"
# 
# #Only weak children can die from malaria
# 
# kap_weak <- dataclean(dhs2_df, v005, v005, '"s1108bf"', "kap_weak")
# kap_weak$kap_weak[kap_weak$kap_weak == 8] <- 0
# 
# svyd_weak_kap <- svydesign.fun(kap_weak)
# table(kap_weak$kap_weak)
# 
# clu_kap_weak<- result.fun('kap_weak', 'v001', design=svyd_weak_kap, kap_weak, "v007")
# head(clu_kap_weak)
# colnames(clu_kap_weak)[1]<- "hv001"
# 
# ##################### Extraction of data raster files using DHS points ##################
# #loading temperature raster file
# building_rastr <- raster(file.path(NGDir, "data/Raster_files/NGA_buildings_count.TIF"))
# plot(building_rastr)
# 
# #loading dhs 2018 points 
# dhs18_sp <- readOGR(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),)
# buffered_dhs18 <- buffer(dhs18_sf, width=2000) #buffered by 2km
# plot(buffered_dhs18)
# 
# ## S4 method for signature 'Raster,SpatialPolygons'
# bulding_count <- mask(building_rastr, buffered_dhs18)
# 
# 
# # bind the datasets 
# 
# mis15_clu15 <- left_join(clu15_est, clu15_wealth, by = "hv001") %>% 
#   left_join(., clu15_u5_prop, by = "hv001") %>% 
#   left_join(., clu15_edu, by = "hv001") %>% 
#   left_join(., clu15_floor, by = "hv001") %>% 
#   left_join(., clu15_wall, by = "hv001") %>% 
#   left_join(., clu15_roof, by = "hv001") %>% 
#   left_join(., clu15_housing_q, by = "hv001") %>% 
#   left_join(., clu15_pfpr_itn_use, by = "hv001") %>% 
#   left_join(., clu15_hh_members_age, by = "hv001") %>%
#   left_join(., pfpr_rural_15, by = "hv001") %>%
#   left_join(., clu15_sex, by = "hv001") %>%
#   left_join(., clu15_hh_size, by = "hv001") %>%
#   left_join(., clu15_u5_care, by = "hv001") %>%
#   left_join(., clu15_pop_den, by = "hv001") %>%
#   left_join(., states15, by = "hv001") %>%
#   left_join(., regions15, by = "hv001") 
# 
# mis15_clu15['data_source'] = 'mis2015'
# 
# 
# #####################################################################################
# ########################### MIS 2010 Data Extraction ##################################
# 
# # prep dataset for cluster level analysis - we start with urban cluster analysis 
# 
# val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural
# 
# #Urban dataset extraction 
# pfpr_place_10 <- pfpr_df_10 %>% filter(hv025 == 1)
# 
# #Rural dataset extraction 
# #pfpr_dhs <- pfpr_data%>% filter(hv025 == 2)
# 
# #Both Urban and Urban dataset extraction 
# # pfpr_place_10 <- pfpr_df_10
# # pfpr_dhs <- pfpr_data
# 
# 
# # estimate cluster-level malaria prevalence
# 
# pfpr_place_10<- funEnv$dataclean.para(pfpr_place_10, hv005, hc1, hml32, 'hml32', 'p_test') 
# 
# svy_mal <- funEnv$svydesign.fun(pfpr_place_10)
# 
# clu10_est <- funEnv$result.fun('p_test', 'hv001', design=svy_mal, pfpr_place_10, "hv007")
# head(clu10_est)
# 
# # next we estimate proportion of people in high SES by cluster
# # recode the weath quintile variable 
# table(pfpr_df_10$hv106)
# 
# pfpr_wealth <- pfpr_df_10 %>%  mutate(wealth = ifelse(hv270 <4, 0, 1))
# table(pfpr_wealth$wealth)
# 
# pfpr_wealth<- funEnv$dataclean.para(pfpr_wealth, hv005, hv005, wealth, 'wealth', 'wealth_2') 
# table(pfpr_wealth$wealth_2)
# 
# svyd_wealth<- funEnv$svydesign.fun(pfpr_wealth)
# 
# clu10_wealth <- funEnv$result.fun('wealth_2', 'hv001', design=svyd_wealth, pfpr_wealth, "hv007")
# head(clu10_wealth)
# 
# 
# 
# #disagreegate wealth 
# look_for(dhs[[1]], "material")
# table(pfpr_df_10$hv205)
# 
# 
# #Housing quality
# #Estimate the proportion of clusters with good floor quality 
# pfpr_floor<- pfpr_df_10 %>%  mutate(floor_type = ifelse(hv213 == 30| hv213 == 31|
#                                                           hv213 == 33| hv213 == 34|
#                                                           hv213 == 35,1, 0))
# table(pfpr_floor$floor_type)
# 
# pfpr_floor<- funEnv$dataclean.para(pfpr_floor, hv005, hv005, floor_type, 'floor_type', 'house_floor') 
# table(pfpr_floor$house_floor)
# 
# svyd_floor<- funEnv$svydesign.fun(pfpr_floor)
# 
# clu10_floor <- result.fun('house_floor', 'hv001', design=svyd_floor, pfpr_floor, "hv007")
# head(clu10_floor)
# 
# #Estimate the proportion of clu10sters with good wall quality 
# pfpr_wall<- pfpr_df_10 %>%  mutate(wall_type = ifelse(hv214 == 30| hv214 == 31|
#                                                         hv214 == 33| hv214 == 34|
#                                                         hv214 == 35|hv214 == 36,1, 0))
# table(pfpr_wall$wall_type)
# 
# pfpr_wall<- funEnv$dataclean.para(pfpr_wall, hv005, hv005, wall_type, 'wall_type', 'house_wall') 
# table(pfpr_wall$house_wall)
# 
# svyd_wall<- funEnv$svydesign.fun(pfpr_wall)
# 
# clu10_wall <- result.fun('house_wall', 'hv001', design=svyd_wall, pfpr_wall, "hv007")
# head(clu10_wall)
# 
# 
# #Estimate the proportion of clu10sters with good roof quality 
# pfpr_roof <- pfpr_df_10 %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
#                                                          hv215 == 33| hv215 == 34|
#                                                          hv215 == 35|hv215 == 36,1, 0))
# table(pfpr_roof$roof_type)
# 
# pfpr_roof<- funEnv$dataclean.para(pfpr_roof, hv005, hv005, roof_type, 'roof_type', 'house_roof') 
# table(pfpr_roof$house_roof)
# 
# svyd_roof<- funEnv$svydesign.fun(pfpr_roof)
# 
# clu10_roof <- result.fun('house_roof', 'hv001', design=svyd_roof, pfpr_roof, "hv007")
# head(clu10_roof)
# 
# #Estimate of the proportion of clu10sters with good housing quality
# pfpr_housing<- pfpr_df_10 %>%  mutate(floor_type = ifelse(hv213 == 30| hv214 == 31|
#                                                             hv213 == 33| hv214 == 34|
#                                                             hv213 == 35|hv214 == 36,1, 0))
# 
# pfpr_housing<- pfpr_housing %>%  mutate(wall_type = ifelse(hv214 == 30| hv214 == 31|
#                                                              hv214 == 33| hv214 == 34|
#                                                              hv214 == 35|hv214 == 36,1, 0))
# 
# 
# pfpr_housing <- pfpr_housing %>%  mutate(roof_type = ifelse(hv215 == 30| hv215 == 31|
#                                                               hv215 == 33| hv215 == 34|
#                                                               hv215 == 35|hv215 == 36,1, 0))
# 
# pfpr_housing_q <- pfpr_housing %>%  mutate(housing_q = ifelse(floor_type == 1 &
#                                                                 wall_type == 1 &
#                                                                 roof_type == 1,1, 0))
# 
# table(pfpr_housing_q$housing_q)
# 
# pfpr_housing_q<- funEnv$dataclean.para(pfpr_housing_q, hv005, hv005, housing_q, 'housing_q', 'housing_qua') 
# table(pfpr_housing_q$housing_qua)
# 
# svyd_housing_q<- funEnv$svydesign.fun(pfpr_housing_q)
# 
# clu10_housing_q <- result.fun('housing_qua', 'hv001', design=svyd_housing_q, pfpr_housing_q, "hv007")
# head(clu10_housing_q)
# 
# # next estimate the average age in each clu10ster 
# 
# look_for(dhs[[1]], "age")
# summary(is.na(pfpr_dhs$hv105))
# 
# pfpr_hh_members_age<- dataclean.para(pfpr_data, hv005, hv005, hv105, 'hv105', 'hh_members_age') 
# table(pfpr_hh_members_age$hh_members_age)
# 
# svyd_hh_members_age <- svydesign.fun(pfpr_hh_members_age)
# table(pfpr_hh_members_age$hh_members_age)
# 
# clu10_hh_members_age <- svyby(~hh_members_age, ~hv001, design = svyd_hh_members_age, FUN=svymean, ci=TRUE,vartype="ci")
# head(clu10_hh_members_age)
# 
# # next estimate the gender proportion in each clu10ster 
# look_for(dhs[[1]], "sex")
# table(dhs[[1]]$hc27)
# 
# pfpr_sex <- pfpr_dhs %>%  mutate(sex = ifelse(hc27 == 1,0, 1))
# pfpr_sex <- dataclean.para(pfpr_sex, hv005, hv005, hc27, 'sex', 'sex_f') 
# table(pfpr_sex$sex_f)
# 
# 
# svyd_sex <- svydesign.fun(pfpr_sex)
# 
# clu10_sex <- result.fun('sex_f', 'hv001', design=svyd_sex, pfpr_sex, "hv007")
# head(clu10_sex)
# 
# #URBAN or rural
# look_for(dhs[[1]], "rural")
# table(dhs[[1]]$hv025)
# 
# pfpr_rural<- pfpr_df_10[,c("hv001", "hv025")]
# colnames(pfpr_rural)[2]<- "Rural_urban"
# table(pfpr_rural$Rural_urban)
# pfpr_rural <- unique(pfpr_rural, by = "hv001")
# table(pfpr_rural$Rural_urban)
# 
# # proportion with secondary or greater education 
# look_for(dhs[[1]], "education")
# val_labels(dhs[[1]]$hv106)
# 
# pfpr_h_edu <- pfpr_dhs %>%  mutate(edu_a = ifelse(hv106 <2, 0,ifelse(hv106 == 8, NA, ifelse(hv106 == 2|3, 1, NA)))) %>% drop_na(edu_a)
# table(pfpr_dhs$hv106)
# table(pfpr_h_edu$edu_a)
# 
# pfpr_edu<- dataclean.para(pfpr_h_edu, hv005, hv005, hv106, 'edu_a', 'edu_a') 
# table(pfpr_edu$edu_a)
# 
# svyd_edu <- svydesign.fun(pfpr_edu)
# 
# clu10_edu <- result.fun('edu_a', 'hv001', design=svyd_edu, pfpr_edu, "hv007")
# head(clu10_edu)
# 
# # median household size
# look_for(dhs[[1]], "number")
# 
# table(pfpr_data$hv013)
# 
# pfpr_hh_size<- dataclean.para(pfpr_data, hv005, hv005, hv009, 'hv009', 'hh_size') 
# table(pfpr_hh_size$hh_size)
# 
# svyd_hh_size <- svydesign.fun(pfpr_hh_size)
# table(pfpr_hh_size$hh_size)
# 
# clu10_hh_size<- svyby(~hh_size, ~hv001, design = svyd_hh_size, FUN=svyquantile, quantiles=0.5, ci=TRUE,vartype="ci")
# head(clu10_hh_size)
# 
# # care seeking proportion among u5 children
# 
# look_for(dhs2[[1]], "fever")
# 
# table(dhs2[[1]]$ml13e)
# 
# pfpr_care <- dhs2[[1]] %>% filter(b5 == 1  & b8 <  5 & h22 == 1) 
# 
# pfpr_care <- dataclean(pfpr_care, v005, v005,' ml13e', "ACT_use_u5")
# pfpr_care <- pfpr_care %>% filter(ACT_use_u5 != 9)
# table(pfpr_care$ACT_use_u5)
# 
# svyd_care <- svydesign.fun(pfpr_care)
# table(pfpr_care$ACT_use_u5)
# 
# clu10_u5_care<- result.fun('ACT_use_u5', 'v001', design=svyd_care, pfpr_care, "v007")
# head(clu10_u5_care)
# colnames(clu10_u5_care)[1]<- "hv001"
# 
# # population density
# clu10_pop_den <- read.csv("NGGC7BFL.csv")%>% 
#   dplyr::select(hv001 = DHSCLUST, pop_den = UN_Population_Density_2010)%>% 
#   mutate(l_pop_den = log(pop_den))
# 
# summary(clu10_pop_den$l_pop_den)
# 
# #state names
# 
# states10 <- pfpr_df_10[,c("hv001", "shstate")]
# colnames(states10)[2]<- "state"
# states10 <- as_label(states10)
# 
# #regiosns
# regions10 <- pfpr_df_10[,c("hv001", "hv024")]
# colnames(regions10)[2]<- "region"
# regions10 <- as_label(regions18)
# 

# 
# ##################### Extraction of data raster files using DHS points ##################
# #loading temperature raster file
# building_rastr <- raster(file.path(NGDir, "data/Raster_files/NGA_buildings_count.TIF"))
# plot(building_rastr)
# 
# #loading dhs 2018 points 
# dhs18_sp <- readOGR(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),)
# buffered_dhs18 <- buffer(dhs18_sp, width=2000) #buffered by 2km
# plot(buffered_dhs18)
# 
# #reprojecting raster and polygons
# projection(buffered_dhs18) = projection(building_rastr)
# 
# ## S4 method for signature 'Raster,SpatialPolygons'
# 
# bulding_count <- extract(building_rastr, dhs18_sp, df=TRUE, buffer = 2000)
# 
# s <- raster(building_rastr)
# for (i in 1:length(bulding_count)) { s[bulding_count[[i]]] <- i }
# # bind the datasets 
# 
# mis10_clu10 <- left_join(clu10_est, clu10_wealth, by = "hv001") %>% 
#   left_join(., clu10_u5_prop, by = "hv001") %>% 
#   left_join(., clu10_edu, by = "hv001") %>% 
#   left_join(., clu10_floor, by = "hv001") %>% 
#   left_join(., clu10_wall, by = "hv001") %>% 
#   left_join(., clu10_roof, by = "hv001") %>% 
#   left_join(., clu10_housing_q, by = "hv001") %>% 
#   left_join(., clu10_pfpr_itn_use, by = "hv001") %>% 
#   left_join(., clu10_hh_members_age, by = "hv001") %>%
#   left_join(., pfpr_rural_10, by = "hv001") %>%
#   left_join(., clu10_sex, by = "hv001") %>%
#   left_join(., clu10_hh_size, by = "hv001") %>%
#   left_join(., clu10_u5_care, by = "hv001") %>%
#   left_join(., clu10_pop_den, by = "hv001") %>%
#   left_join(., states10, by = "hv001") %>%
#   left_join(., regions10, by = "hv001") 
# 
# mis10_clu10['data_source'] = 'mis2010'
# 
# urbandataset <- dplyr::bind_rows(dhs_clu, mis10_clu15, mis10_clu10)
# 
# 
# write_csv(dataset, paste0(BinDir, '/urban_dataset_DHS/urbandataset.csv'))


dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #reads in the IR files



d_ <- st_distance(dhs[[1]]) #create a distance matrix in meters 
mode(d_) = "numeric"
new_d_<-data.frame(d_) %>%  mutate_all(list(~str_split(.,'\\[', simplify = T)[,1])) # extracts the matrix (next on the agenda is to append urban or rural and then find the nearest rural area)
#also find how many rural areas are within a certain distance 


# #calculate the nearest rural cluster 
# dhs <- map(dhs, st_as_sf)
# dist.mat <- map(dhs, st_distance) # Great Circle distance since in lat/lon
# num.fun <- function(dist.mat){
#   apply(dist.mat, 1, function(x) {sum(x < 2500) - 1})# Number within 1.5km: Subtract 1 to exclude the point itself
# }
# 
# num.1500 <- map(dist.mat, num.fun)
# 
# 
# 
# # Calculate nearest distance
# nn.dist.fun <- function(dist.mat){apply(dist.mat, 1, function(x) {return(sort(x, partial = 2)[2])})}
# nn.dist <- map(dist.mat, nn.dist.fun)
# 
# 
# # Get index for nearest distance
# nn.index.fun <- function(dist.mat) {apply(dist.mat, 1, function(x) { order(x, decreasing=F)[2] })}
# nn.index<- map(dist.mat, nn.index.fun)
# 
# dhs10<- dhs[[1]] %>% st_drop_geometry()
# n.data <- dhs_10
# colnames(n.data)[1] <- "neighbor"
# colnames(n.data)[2:ncol(n.data)] <- paste0("n.", colnames(n.data)[2:ncol(n.data)])
# mydata2 <- data.frame(dhs10,
#                       n.data[nn.index[[1]], ],
#                       n.distance = nn.dist[[1]],
#                       radius1500 = num.1500[[1]])
# rownames(mydata2) <- seq(nrow(mydata2))
# 
# 
# 
# 
