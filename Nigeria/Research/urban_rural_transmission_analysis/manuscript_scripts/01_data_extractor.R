## These scripts are used to extract cluster level data and variables for urban settings in Nigeria 
rm(list=ls())
memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
ResultDir <-file.path(ProjectDir, "results")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')




# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "Nigeria functions.R"))
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


## ----------------------------------------------------
### Read in PR  data (DHS 2010, 2015, 2018)  
## ----------------------------------------------------


dhs <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL|NGPR71FL|NGPR61FL', read_dta)  #reads in the PR files



table(dhs[[1]]$hv215)

## -----------------------------------------
### Data processing 
## -----------------------------------------

#create a variables for wealth and housing quality, sex, net use, survey design and educational attainment  for all years
dhs<- dhs %>% map(~mutate(., wealth = ifelse(hv270 <4, 0, 1),
                            floor_type = ifelse(hv213 >= 98, NA, ifelse(hv213 %in% c(30, 31, 33, 34, 35),1, 0)),
                                              wall_type = ifelse(hv214 >= 98, NA , ifelse (hv214 %in% c(30, 31, 33, 34,35),1, 0)),
                            roof_type = ifelse(hv215 >= 98, NA, ifelse(hv215 %in% c(31),1, 0)),
                            housing_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1,1, 0),
                            all_female_sex = ifelse(hc27 == 1,0, 1), 
                            female_child_sex = all_female_sex,
                            net_use = ifelse(hml12 %in% c(1,2), 1,0),
                            wt=hv005/1000000,strat=hv022,
                            id=hv021, num_p=1,
                            edu_a = ifelse(hv106 %in% c(0, 1, 2), 0,ifelse(hv106 >= 8, NA, ifelse(hv106 == 2|3, 1, NA))),
                            age = ifelse(hv105 >= 98, NA, hv105),
                            median_age = age,
                            mean_age =age,
                            household_size = hv013,
                            p_test = ifelse(hml32 > 1, NA, hml32),
                            U5_pop = ifelse(hc1 %in% c(0:59), 1, 0),
                            region = hv024, interview_month = hv006)) %>% 
  map(~filter(., hv025 == 1)) %>% 
  map(~dplyr::select(., -c(hv013, hv105, hv106, hv021, hv005, hv022, hml12, hc27, hv215, hv214, hv213, hv270, hv024, hv006)))




#creating variable for computing pregnant women proportions 
dhs[[1]]$preg_women <- ifelse(dhs[[1]]$sh09 >= 8 , NA, ifelse(dhs[[1]]$sh09 == 1, 1, 0))
dhs[[2]]$preg_women <- ifelse(dhs[[2]]$sh09 >= 8, NA, ifelse(dhs[[2]]$sh09 == 1, 1, 0))
dhs[[3]]$preg_women <- ifelse(dhs[[3]]$ha54 >= 8, NA, ifelse(dhs[[3]]$ha54 == 1, 1, 0))



 # create PR dataset by filtering for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test and urban area(hv025). PR is for children 6 - 59 months)
pfpr_df <- dhs %>% map(~filter(., hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6) & hml16 <59))



## -----------------------------------------
### estimation using all PR files 
## -----------------------------------------

#proportions 

vars <- c('net_use', 'edu_a', 'wealth', 'housing_q', 'floor_type', 'wall_type', 'roof_type', 'all_female_sex', 'U5_pop', 'preg_women')

vars<- c('roof_type')
for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]*100
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  
}


#mean

vars <- c('mean_age')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <-  pmap(list(df,col,by), estim_mean)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  
}



#median

vars <- c('median_age', 'household_size')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_median)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  
}



# month of state, region, month of survey by cluster 

vars <- c('shstate', 'region', 'interview_month')

for (i in 1:length(vars)){
  if (vars[i] == 'interview month'){
    df <- dhs %>%  map(~dplyr::select(., c(hv001, vars[i])))
    df <- plyr:: ldply(df)
    write.csv(df, file =file.path(DataDir, 'urban_malaria_cluster_est', paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  }else{
  df <- dhs %>%  map(~dplyr::select(., c(hv001, vars[i])))
  df <- plyr:: ldply(df) %>%  distinct() 
  df[vars[i]] <- as_label(df[vars[i]])
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_all_DHS_PR_10_15_18.csv")))
  }
}





## -----------------------------------------------------------------------------
### estimation using PR files for children tested for malaria with microscopy 
## ------------------------------------------------------------------------------

#proportions 
#overall

pfpr_df<- pfpr_df %>%  map(~mutate(., net_use_child = net_use))

vars <- c('female_child_sex', 'net_use_child', 'p_test')


for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <-  pfpr_df %>% 
    map(~drop_na(.,vars[i])) 
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]*100
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_PfPR_DHS_10_15_18.csv")))
  
}

#by state 
pfpr_df[[1]]$state <- as_label(pfpr_df[[1]]$shstate)
pfpr_df[[2]]$state <- as_label(pfpr_df[[2]]$shstate)
pfpr_df[[3]]$state <- as_label(pfpr_df[[3]]$shstate)



vars <- c('sex', 'net_use_child', 'p_test')
state <- c('lagos')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('hv001')
  df <-  pfpr_df %>% 
    map(~drop_na(.,vars[i])) %>%  map(~filter(., state == state[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0(vars[i],  state[i], "_PfPR_DHS_10_15_18.csv")))
  
}



## ----------------------------------------------------
### Read in KR  data (DHS 2010, 2015, 2018) 
## ----------------------------------------------------

dhs <- read.files(DataDir, "*NGKR.*\\.DTA", 'NGKR7AFL|NGKR71FL|NGKR61FL', read_dta) #reads in the KR files 
dhs <- dhs %>%  map(~mutate(., fever =  ifelse(h22 >= 8, NA, h22), ACT_use_U5 = ifelse(ml13e >=8, NA, ml13e),
                            wt=v005/1000000,strat=v022,id=v021, num_p=1, med_treat_fever = ifelse(h32z >=8, NA, h32z),)) %>%
  map(~dplyr::select(., c(fever, ACT_use_U5, wt, strat, id, v001, b5, v025, med_treat_fever))) %>% 
  map(~filter(., v025 == 1)) 


#proportions 

vars <- c('fever', 'ACT_use_U5', 'med_treat_fever')

for (i in 1:length(vars)) {
  
  if (vars[i] == 'ACT_use_U5'|vars[i] == 'med_treat_fever' ){
    col <- list(vars[i])
    by <- list('v001')
    dhs <- dhs %>%  map(~filter(., b5 == 1  & fever == 1)) #b5 - child is alive
    df <- dhs %>% 
      map(~drop_na(.,vars[i]))
    df <- pmap(list(df,col,by), estim_prop)
    df <- plyr::ldply(df)
    df[, vars[i]]<- df[, vars[i]]*100
    write.csv(df, file =file.path(DataIn, paste0(vars[i], "_KR_DHS_10_15_18.csv")))
    
  }else{
  col <- list(vars[i])
  by <- list('v001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]*100
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_KR_DHS_10_15_18.csv")))
  }
}



## ----------------------------------------------------
### estimation using the 2018 IR file 
## ----------------------------------------------------

dhs <- read.files(DataDir, "*NGIR.*\\.DTA", 'NGIR7AFL', read_dta) #reads in the IR files
dhs <- dhs %>%  map(~mutate(., wt=v005/1000000,strat=v022, id=v021)) %>%  map(~filter(., v025 == 1)) 

#create a variable for movement proxy and occupation  
dhs <- dhs %>% map(~mutate(., trips_woman = ifelse(v167 >=99, NA, v167), #Number of trips in last 12 months 
                                duration_travel_woman = ifelse(v168 >=9, NA, v168), #Away for more than one month in the last 12 months 
                                agri_worker_partner = ifelse(v705 %in% c(4, 5), 1, ifelse(v705 >=98, NA, 0)), # if husband/partner is agricultural worker or not
                                last_work_partner = ifelse(v704a >=8, NA, ifelse(v704a %in% c(1, 2), 1, 0)), # if husband/partner has worked in the last 7 days or in the last 12 months
                                agri_worker_woman = ifelse(v717 %in% c(4, 5), 1, ifelse(v717 >=98, NA, 0)), # if woman is agricultural worker or no
                                agri_worker_both = ifelse(agri_worker_partner ==1 & agri_worker_woman ==1, 1, 0), # if both husband and wife are agricultural workers
                      last_work_woman = ifelse(v731 %in% c(1, 2, 3), 1, ifelse(v731 >=9, NA, 0)),#if respondent has worked in the last past year, is currently working, has a job or is on leave in the last 7 days 
                      seasonal_work_woman = ifelse(v732 == 2, 1, ifelse(v732 ==9, NA, 0)))) #if respondent or woman is a seasonal worker or not 
                  


# knowledge questions recode

dhs[[1]]$s1108ai <- ifelse(dhs[[1]]$s1108ai == 1, 1, 0)
dhs[[1]]$s1108ba <- ifelse(dhs[[1]]$s1108ba == 1, 1, 0)
dhs[[1]]$s1108bc <- ifelse(dhs[[1]]$s1108bc  == 0, 1, 0)
dhs[[1]]$s1108bd <- ifelse(dhs[[1]]$s1108bd   == 1, 1, 0)
dhs[[1]]$s1108bf  <- ifelse(dhs[[1]]$s1108bf  == 0, 1, 0)

dhs[[1]]$know_vul <- ifelse(dhs[[1]]$s1108ai == 1 & dhs[[1]]$s1108ba == 1 & dhs[[1]]$s1108bc == 1 & dhs[[1]]$s1108bd == 1 & dhs[[1]]$s1108bf ==1, 1, 0)


#proportions 

vars <- c('know_vul', 'agri_worker_partner', 'last_work_partner', 'agri_worker_woman', 'agri_worker_both', 'last_work_woman', 'seasonal_work_woman' , 'duration_travel_woman')



for (i in 1:length(vars)) {
col <- list(vars[i])
by <- list('v001')
df <- dhs %>% 
  map(~drop_na(.,vars[i]))
df <- pmap(list(df,col,by), estim_prop)
df <- plyr::ldply(df)
df[, vars[i]]<- df[, vars[i]]*100
write.csv(df, file =file.path(DataIn, paste0(vars[i], "_IR_DHS_18.csv")))
}




#median

vars <- c('trips_woman')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('v001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_median)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_IR_DHS_18.csv")))
  
}


## ----------------------------------------------------
### estimation using the 2018 MR file 
## ----------------------------------------------------

dhs <- read.files(DataDir, "*NGMR.*\\.DTA", 'NGMR7AFL', read_dta) #reads in the IR files
dhs <- dhs %>%  map(~mutate(., wt=mv005/1000000,strat=mv022, id=mv021)) %>%  map(~filter(., mv025 == 1)) 

look_for(dhs[[1]], 'mv716')

table(dhs[[1]]$mv716)

#create a variable for movement proxy and occupation  
dhs <- dhs %>% map(~mutate(., trips_man = ifelse(mv167 >=97, NA, mv167), #Number of times away from home in the last 12 months 
                           duration_travel_man = ifelse(mv168 >=9, NA, mv168), #Away for more than one month in the last 12 months 
                           agri_worker_man = ifelse(mv717 %in% c(4, 5), 1, ifelse(mv717 >=98, NA, 0)), # if male is agricultural worker or not
                           last_work_man = ifelse(mv731 >=9, NA, ifelse(mv731 %in% c(1, 2), 1, 0)), # if male has worked in the last 12 months
                           seasonal_work_man = ifelse(mv732 == 2, 1, ifelse(mv732 ==9, NA, 0)))) #if man is a seasonal worker or not 



#proportions 

vars <- c('agri_worker_man', 'last_work_man', 'seasonal_work_man' , 'duration_travel_man')



for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('mv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_prop)
  df <- plyr::ldply(df)
  df[, vars[i]]<- df[, vars[i]]*100
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_MR_DHS_18.csv")))
}


#median

vars <- c('trips_man')

for (i in 1:length(vars)) {
  col <- list(vars[i])
  by <- list('mv001')
  df <- dhs %>% 
    map(~drop_na(.,vars[i]))
  df <- pmap(list(df,col,by), estim_median)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0(vars[i], "_MR_DHS_18.csv")))
  
}



## ----------------------------------------------------
### Geospatial covariates extraction 
## ----------------------------------------------------


dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 

#pop density raster files
files <- list.files(path = file.path(DataDir, "Raster_files") , pattern = "*deg.tif$", full.names = TRUE, recursive = TRUE)
files<- files[(grep('gpw_v4', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
df <- map2(dhs, raster, get_crs)
df <- pmap(list(raster, df, vars[i]), extract_fun)
df <- plyr::ldply(df)
write.csv(df, file =file.path(DataIn, paste0('pop_density_', as.character(vars[i]), 'm_buffer', "_DHS_10_15_18.csv")))
}



# pop density extraction with general FB data 

raster_3 <- raster(file.path(DataDir, "Raster_files/facebook_pop_density/nga_general_2020.tif"))
raster[[3]] <-raster_3

names(new_raster)[3]<- 'facebook_2020'
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0('pop_density_FB_', as.character(vars[i]), 'm_buffer', "_DHS_10_15_18.csv")))
}



# pop density extraction with U5 FB data 
raster <- raster(file.path(DataDir, "Raster_files/facebook_pop_density/nga_children_under_five_2020.tif"))
raster <- list(raster)



vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file =file.path(DataIn, paste0('pop_density_U5_FB_', as.character(vars[i]), 'm_buffer', "_DHS_10_15_18.csv")))
}



