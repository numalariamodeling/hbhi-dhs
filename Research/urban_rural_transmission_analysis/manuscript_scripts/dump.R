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
