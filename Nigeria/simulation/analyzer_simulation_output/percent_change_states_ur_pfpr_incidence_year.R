#This script processes simulation output to generate percent change in outcomes with a 2015, 2020 and 2019 baseline at the state and national level in Nigeria
# Created by Ifeoma Ozodiegwu 



# create baseline file 
rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")
simInDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v2")

source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))


percent_change_fun <- function(df, boolean){
  
  if(boolean == TRUE){
    df %>% group_by(State) %>% mutate(PfPR_percent_change = (PfPR - PfPR[year =="2020" & scenario == "NGA projection scenario 1"])/PfPR[year =="2020" & scenario == 'NGA projection scenario 1']* 100, 
                                      U5_PfPR_percent_change = 
                                        (U5_PfPR - U5_PfPR[year =="2020" & scenario == "NGA projection scenario 1"])/U5_PfPR[year =="2020" & scenario == "NGA projection scenario 1"]* 100, 
                                      incidence_percent_change = 
                                        (incidence - incidence[year =="2020" & scenario == "NGA projection scenario 1"])/incidence[year =="2020" & scenario == "NGA projection scenario 1"]* 100, 
                                      U5_incidence_percent_change = 
                                        (U5_incidence - U5_incidence[year =="2020" & scenario == "NGA projection scenario 1"])/U5_incidence[year =="2020" & scenario == "NGA projection scenario 1"]* 100,
                                      death_percent_change = 
                                        (death_rate_mean - death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"])/death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"]* 100,
                                      U5_death_percent_change = 
                                        (U5_death_rate_mean - U5_death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"])/U5_death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"]* 100)
  } else if(boolean == FALSE){
    df %>% mutate(PfPR_percent_change = (PfPR - PfPR[year =="2020" & scenario == "NGA projection scenario 1"])/PfPR[year =="2020" & scenario == 'NGA projection scenario 1']* 100, 
                  U5_PfPR_percent_change = 
                    (U5_PfPR - U5_PfPR[year =="2020" & scenario == "NGA projection scenario 1"])/U5_PfPR[year =="2020" & scenario == "NGA projection scenario 1"]* 100, 
                  incidence_percent_change = 
                    (incidence - incidence[year =="2020" & scenario == "NGA projection scenario 1"])/incidence[year =="2020" & scenario == "NGA projection scenario 1"]* 100, 
                  U5_incidence_percent_change = 
                    (U5_incidence - U5_incidence[year =="2020" & scenario == "NGA projection scenario 1"])/U5_incidence[year =="2020" & scenario == "NGA projection scenario 1"]* 100,
                  death_percent_change = 
                    (death_rate_mean - death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"])/death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"]* 100,
                  U5_death_percent_change = 
                    (U5_death_rate_mean - U5_death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"])/U5_death_rate_mean[year =="2020" & scenario == "NGA projection scenario 1"]* 100)
  }
  
}        


# state analysis 
scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
    files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_state", full.names = TRUE)
    df <- sapply(files, read_csv, simplify = F)
}

df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR, U5_PfPR,  
                                               incidence, U5_incidence, death_rate_1,death_rate_2,U5_death_rate_1, U5_death_rate_2)



df_all<- df %>% mutate(State = str_split(.id, "_", simplify = TRUE)[,9], 
  State = str_sub(State, 1, str_length(State)-4), scenario = str_split(.id, "/", simplify = T)[, 10],
  death_rate_mean = (death_rate_1 + death_rate_2)/2, U5_death_rate_mean = (U5_death_rate_1 + U5_death_rate_2)/2) 
 
  


fin_df <- percent_change_fun(df_all, TRUE)  


write_csv(fin_df, file.path(WorkDir, "/2020_to_2030_v2/percent_change_States_indicators.csv"))



#create new variables to detects the column with the greatest reduction in indicators or the best scenario

find_df2 <- fin_df%>%  filter(scenario != "NGA projection scenario 2") %>% group_by(State) %>% 
  mutate(pfpr_min = ifelse(PfPR_percent_change == min(PfPR_percent_change), 1, 0),
         U5_pfpr_min = ifelse(U5_PfPR_percent_change == min(U5_PfPR_percent_change), 1, 0),
         incidence_min = ifelse(U5_PfPR_percent_change == min(U5_PfPR_percent_change), 1, 0), 
         U5_incidence_min = ifelse(U5_incidence_percent_change == min(U5_incidence_percent_change), 1, 0), 
         death_min = ifelse(death_percent_change == min(death_percent_change), 1, 0),
         U5_death_min = ifelse(U5_death_percent_change == min(U5_death_percent_change), 1, 0))%>% 
  rowwise() %>% 
  mutate(sumVar = sum(c_across(pfpr_min:U5_death_min)),
         best_scenario = ifelse(sumVar == 6, scenario, "variable"))
  

write_csv(find_df2, file.path(WorkDir, "/2020_to_2030_v2/percent_change_States_indicators_second_best_scenario.csv"))



# national level % change 

scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_2020_2030.csv", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}

#read in 2019 annual indicators

df_2019 <- read_csv(file.path(WorkDir, "2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc 1", "annual_indicators_2011_2020.csv"))

df[['C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2030_v2/NGA projection scenario 0/annual_indicators_2020_2030.csv']] <- df_2019



df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR, U5_PfPR,  incidence, U5_incidence, death_rate_mean, U5_death_rate_mean)

# yearly % change 
# df_all<- df %>% mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) %>%  group_by(scenario) %>%  
#   mutate(PfPR_trend = PfPR - lag(PfPR, default = PfPR[1]),
#          U5_PfPR_trend = U5_PfPR - lag(U5_PfPR,default = U5_PfPR[1]),
#          incidence_trend = incidence - lag(incidence, default = incidence[1]),
#          U5_incidence_trend = U5_incidence - lag(U5_incidence, default = U5_incidence[1]),
#          death_rate_trend = death_rate_mean - lag(death_rate_mean, default = death_rate_mean[1]),
#          U5_death_rate_trend = U5_death_rate_mean - lag(U5_death_rate_mean, default = U5_death_rate_mean[1]))

write_csv(df_all, file.path(WorkDir, "/2020_to_2030_v2/yearly_percent_change_indicators.csv"))


#2015 % change 
fin_df <- df %>%  mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) %>% 
  mutate(PfPR_percent_change = (PfPR - PfPR[year =="2015"])/PfPR[year =="2015"]* 100, 
    U5_PfPR_percent_change = (U5_PfPR - U5_PfPR[year =="2015"])/U5_PfPR[year =="2015"]* 100, 
         incidence_percent_change = (incidence - incidence[year =="2015"])/incidence[year =="2015"]* 100, 
                 U5_incidence_percent_change = (U5_incidence - U5_incidence[year =="2015"])/U5_incidence[year =="2015"]* 100,
                      death_percent_change = (death_rate_mean - death_rate_mean[year =="2015"])/death_rate_mean[year =="2015"]* 100,
                           U5_death_percent_change = (U5_death_rate_mean - U5_death_rate_mean[year =="2015"])/U5_death_rate_mean[year =="2015"]* 100)
                          
#2020 % change 

fin_df_2020  <- percent_change_fun(df, FALSE)

write_csv(fin_df, file.path(WorkDir, "/2020_to_2030_v2/percent_change_indicators_2015_base.csv"))

write_csv(fin_df_2020, file.path(ProcessDir, "/2020_to_2030_v2/percent_change_indicators_2020_base.csv"))




# SMC states 


scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv")) %>%  filter(Scenario_no %in% c(1, 2, 6, 7))

for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_each_LGA.csv", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}


for (i in 1:3){
  files <- list.files(path = simInDir, pattern = "*PAAR_2020_2030.csv|smc_bau_2020_2030.csv|smc_increase80_2020_2030.csv", full.names = T)
  SMC_df <- sapply(files, read_csv, simplify = F)
}


SMC_LGA <- map(SMC_df, ~ .x %>% 
      distinct(LGA))




#function to compute indicators 
sum_fun <- function(df1, df2){
  df<- df1 %>%  filter(LGA %in% df2$LGA)%>% 
  group_by(year) %>% 
    summarise(incidence =sum(cases)/sum(geopode.pop) * 1000,
              U5_incidence =sum(cases_U5)/sum(geopode.pop_U5) * 1000,
              PfPR =sum(positives)/sum(geopode.pop),
              U5_PfPR =sum(positives_U5)/sum(geopode.pop_U5),
              death_rate_1_all_ages = sum(deaths_1)/sum(geopode.pop) *1000,
              death_rate_2_all_ages = sum(deaths_2)/sum(geopode.pop) *1000,
              death_rate_1_U5 = sum(deaths_U5_1)/sum(geopode.pop_U5) *1000,
              death_rate_2_U5 = sum(deaths_U5_2)/sum(geopode.pop_U5) *1000,
              death_rate_mean = (death_rate_1_all_ages+death_rate_2_all_ages)/2,
              U5_death_rate_mean = (death_rate_1_U5+death_rate_2_U5)/2) %>% 
    dplyr::select(-c(death_rate_1_all_ages,death_rate_2_all_ages,death_rate_1_U5, death_rate_2_U5))
}


df_ls <- map2(df, SMC_LGA, sum_fun)

df <- plyr::ldply(df_ls, rbind)

df_all<- df %>% mutate( scenario = str_split(.id, "/", simplify = T)[, 10]) 


fin_df <- percent_change_fun(df_all, FALSE) 

write_csv(fin_df, file.path(ProcessDir, "/percent_change_indicators_2020_base_SMC_states.csv"))
