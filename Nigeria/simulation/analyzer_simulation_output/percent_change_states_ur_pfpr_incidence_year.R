#This script processes simulation output to generate percent change in outcomeswith a 2020 and 2019 baseline at the state and national level in Burkina
# Created by Ifeoma Ozodiegwu 



# create baseline file 
rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
WorkDir <- file.path(TeamDir, "projects/hbhi_nigeria/simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")

source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))

ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")

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
  
# imap_dfc(function(x, name) {
#   if (name %in% cols_to_mutate) {
#     new_vals <- ifelse(x == min(x), 1, 0)
#     tibble(!!quo_name(name) := x, !!quo_name(paste0(name, "_n")) := new_vals)
#   } else {
#     tibble(!!quo_name(name) := x)
#   }
# }) 
# 
# 
# %>%  
#   

write_csv(find_df2, file.path(WorkDir, "/2020_to_2030_v2/percent_change_States_indicators_second_best_scenario.csv"))



# national level % change 

scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_2020_2030.csv", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}

#read in 2019 annual indicators

df_2019 <- read_csv(file.path(WorkDir, "2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc", "annual_indicators_2011_2020.csv"))

df[['C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2030_v2/NGA projection scenario 0/annual_indicators_2020_2030.csv']] <- df_2019



df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR, U5_PfPR,  incidence, U5_incidence, death_rate_mean, U5_death_rate_mean)


df_all<- df %>% mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) %>%  group_by(scenario) %>%  
  mutate(PfPR_trend = PfPR - lag(PfPR, default = PfPR[1]),
         U5_PfPR_trend = U5_PfPR - lag(U5_PfPR,default = U5_PfPR[1]),
         incidence_trend = incidence - lag(incidence, default = incidence[1]),
         U5_incidence_trend = U5_incidence - lag(U5_incidence, default = U5_incidence[1]),
         death_rate_trend = death_rate_mean - lag(death_rate_mean, default = death_rate_mean[1]),
         U5_death_rate_trend = U5_death_rate_mean - lag(U5_death_rate_mean, default = U5_death_rate_mean[1]))

write_csv(df_all, file.path(WorkDir, "/2020_to_2030_v2/yearly_percent_change_indicators.csv"))


#2019 % change 
fin_df <- df_all %>% 
  mutate(PfPR_percent_change = (PfPR - PfPR[year =="2019"])/PfPR[year =="2019"]* 100, 
    U5_PfPR_percent_change = (U5_PfPR - U5_PfPR[year =="2019"])/U5_PfPR[year =="2019"]* 100, 
         incidence_percent_change = (incidence - incidence[year =="2019"])/incidence[year =="2019"]* 100, 
                 U5_incidence_percent_change = (U5_incidence - U5_incidence[year =="2019"])/U5_incidence[year =="2019"]* 100,
                      death_percent_change = (death_rate_mean - death_rate_mean[year =="2019"])/death_rate_mean[year =="2019"]* 100,
                           U5_death_percent_change = (U5_death_rate_mean - U5_death_rate_mean[year =="2019"])/U5_death_rate_mean[year =="2019"]* 100)
                          
#2020 % change 

fin_df_2020 <-fin_df <- percent_change_fun(df_all, FALSE)

write_csv(fin_df, file.path(WorkDir, "/2020_to_2030_v2/percent_change_indicators_2019_base.csv"))

write_csv(fin_df_2020, file.path(WorkDir, "/2020_to_2030_v2/percent_change_indicators_2020_base.csv"))


