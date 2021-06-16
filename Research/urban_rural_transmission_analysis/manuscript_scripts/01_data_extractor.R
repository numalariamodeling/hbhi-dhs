## These scripts are used to extract cluster level data and variables for urban settings in Nigeria 

rm(list=ls())


## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
SimDir <- file.path(ProjectDir, 'simulation_input')
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <- file.path(NGDir, "data")
Rdata <- file.path(DataDir, "urban_malaria_rdata")
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src")
ResearchDir <- file.path(SrcDir,'Research', 'urban_rural_transmission_analysis')
DHSDir <- file.path(SrcDir, "DHS")
VarDir <- file.path(DHSDir, "1_variables_scripts")



# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(ResearchDir , "functions", "Nigeria functions.R"))


## ----------------------------------------------------
### Read in PR  data (DHS 2010, 2015, 2018)  
## ----------------------------------------------------

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

dhs <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL|NGPR71FL|NGPR61FL', read_dta)  #reads in the PR files





## -----------------------------------------
### Data processing 
## -----------------------------------------

#create a variables for wealth and housing quality, sex, net use, survey design and educational attainment  for all years
dhs<- dhs %>% map(~mutate(., wealth = ifelse(hv270 <4, 0, 1),
                                   floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 33, 34, 35),1, 0)),
                                                      wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 31, 33, 34,35),1, 0)),
                                   roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(30, 31, 33, 34,35, 36),1, 0)),
                                   housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0),
                                   sex = ifelse(hc27 == 1,0, 1), 
                                   net_use = ifelse(hml12 %in% c(1,2), 1,0),
                                   wt=hv005/1000000,strat=hv022,
                                   id=hv021, num_p=1,
                                 edu_a = ifelse(hv106 < 2, 0,ifelse(hv106 >= 8, NA, ifelse(hv106 == 2|3, 1, NA))),
                                 age = ifelse(hv105 >= 98, NA, hv105),
                                 household_size = hv013,
                                 p_test = ifelse(hml32 > 1, NA, hml32))) %>% 
  map(~filter(., hv025 == 1)) %>% 
  map(~dplyr::select(., -c(hv013, hml32, hv105, hv106, hv021, hv005, hv022, hml12, hc27, hv215, hv214, hv213, hv270)))



# create PR dataset by filtering for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test and urban area(hv025))
pfpr_df <- dhs %>% map(~filter(., hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6) & hml16 <59))



## -----------------------------------------
### estimation using all PR files 
## -----------------------------------------

#proportions 

vars <- c('net_use', 'edu_a', 'wealth', 'housing_q', 'floor_type', 'wall_type', 'roof_type', 'sex', 'net_use')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- map2(df,col, estim_prop)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', paste0(vars[i], "_all_DHS_10_15_18.csv")))
  
}


#mean

vars <- c('age')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- map2(df,col, estim_mean)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', paste0(vars[i], "_all_DHS_10_15_18.csv")))
  
}



#median

vars <- c('age', 'household_size')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- map2(df,col, estim_mean)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', paste0(vars[i], "_all_DHS_10_15_18.csv")))
  
}



## -----------------------------------------------------------------------------
### estimation using PR files for children tested for malaria with microscopy 
## ------------------------------------------------------------------------------

#proportions 

vars <- c('p_test', 'sex', 'net_use')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- map2(df,col, estim_prop)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', paste0(vars[i], "_PfPR_DHS_10_15_18.csv")))
  
}



## ----------------------------------------------------
### Read in KR  data (DHS 2010, 2015, 2018) 
## ----------------------------------------------------

dhs <- read.files(DataDir, "*NGKR.*\\.DTA", 'NGKR7AFL|NGKR71FL|NGKR61FL', read_dta) #reads in the KR files 
dhs <- dhs %>%  map(~mutate(., fever =  ifelse(h22 >= 8, NA, h22), ACT_use_U5 = ifelse(ml13e >=8, NA, ml13e),wt=v005/1000000,strat=v022,
                            id=v021, num_p=1)) %>% map(~dplyr::select(., c(fever, ACT_use_U5, wt, strat, id, v001, b5)))



#proportions 

vars <- c('fever', 'ACT_use_U5')

for (i in 1:length(vars)) {
  
  if (vars[i] == 'ACT_use_U5'){
    col <- list(vars[i])
    by <- list('v001')
    dhs <- dhs %>%  map(~filter(., b5 == 1  & fever == 1)) #b5 - child is alive
    df <- dhs %>% 
      map(~drop_na(.,vars[i]))
    df <- pmap(list(df,col,by), estim_prop)
    df <- plyr::ldply(df)
    write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', paste0(vars[i], "_PfPR_DHS_10_15_18.csv")))
    
  }else{
  col <- list(vars[i])
  by <- list('v001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', paste0(vars[i], "_PfPR_DHS_10_15_18.csv")))
  }
}












# # population density
# clu_pop_den <- read.csv("NGGC7BFL.csv")%>% 
#   dplyr::select(hv001 = DHSCLUST, pop_den = UN_Population_Density_2015)%>% 
#   mutate(l_pop_den = log(pop_den))
# 
# summary(clu_pop_den$l_pop_den)
# 
# #state names
# 
# states18 <- pfpr_df[,c("hv001", "shstate")]
# colnames(states18)[2]<- "state"
# states18 <- as_label(states18)
# 
# #regiosns
# regions18 <- pfpr_df[,c("hv001", "hv024")]
# colnames(regions18)[2]<- "region"
# regions18 <- as_label(regions18)
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
# dhs_clu <- left_join(clu_est, clu_wealth, by = "hv001") %>% 
#   left_join(., clu_u5_prop, by = "hv001") %>% 
#   left_join(., clu_edu, by = "hv001") %>% 
#   left_join(., clu_floor, by = "hv001") %>% 
#   left_join(., clu_wall, by = "hv001") %>% 
#   left_join(., clu_roof, by = "hv001") %>% 
#   left_join(., clu_housing_q, by = "hv001") %>% 
#   left_join(., clu_pfpr_itn_use, by = "hv001") %>% 
#   left_join(., clu_hh_members_age, by = "hv001") %>%
#   left_join(., pfpr_rural, by = "hv001") %>%
#   left_join(., clu_sex, by = "hv001") %>%
#   left_join(., clu_hh_size, by = "hv001") %>%
#   left_join(., clu_u5_care, by = "hv001") %>%
#   left_join(., clu_pop_den, by = "hv001") %>%
#   left_join(., states18, by = "hv001") %>%
#   left_join(., regions18, by = "hv001") 
# 
# dhs_clu['data_source'] = 'dhs2018'
# 
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
