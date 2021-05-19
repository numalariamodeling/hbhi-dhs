###############################################################################
# state analysis
###############################################################################

# scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))
# 
# for (row in 1:nrow(scen_dat)){
#     files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_mean", full.names = TRUE)
#     files<- files[-(grep('_funder_|_2020_2030|each_LGA', files))]
#     df <- sapply(files, read_csv, simplify = F)
# }
# 
# 
# 
# df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR_all_ages, PfPR_U5,  
#                                                incidence_all_ages, incidence_U5, death_rate_1_all_ages,death_rate_2_all_ages,death_rate_1_U5, death_rate_2_U5)
# 
# 
# 
# df_all<- df %>% mutate(State =  str_split(.id, "indicators_", simplify = TRUE)[,2], 
#   State = str_sub(State, 1, str_length(State)-4), scenario = str_split(.id, "/", simplify = T)[, 10],
#   death_rate_mean_all_ages= (death_rate_1_all_ages + death_rate_2_all_ages)/2, death_rate_mean_U5 = (death_rate_1_U5 + death_rate_2_U5)/2) 
#  
# 
# 
# 
# 
# fin_df <- percent_change_fun(df_all, TRUE)  
# 
# 
# write_csv(fin_df, paste0(PrintDir,  "/",Sys.Date(), "_percent_change_States_indicators_2020_base.csv"))
# 
# 
# 
# #create new variables to detects the column with the greatest reduction in indicators or the best scenario
# 
# find_df2 <- fin_df%>%  filter(year ==2020 | year == 2030 ) %>% 
#   group_by(State) %>% 
#   mutate(pfpr_min = ifelse(PfPR_percent_change == min(PfPR_percent_change), 1, 0),
#          U5_pfpr_min = ifelse(U5_PfPR_percent_change == min(U5_PfPR_percent_change), 1, 0),
#          incidence_min = ifelse(U5_PfPR_percent_change == min(U5_PfPR_percent_change), 1, 0), 
#          U5_incidence_min = ifelse(U5_incidence_percent_change == min(U5_incidence_percent_change), 1, 0), 
#          death_min = ifelse(death_percent_change == min(death_percent_change), 1, 0),
#          U5_death_min = ifelse(U5_death_percent_change == min(U5_death_percent_change), 1, 0))%>% 
#   rowwise() %>% 
#   mutate(sumVar = sum(c_across(pfpr_min:U5_death_min)),
#          best_scenario = ifelse(sumVar == 6, scenario, "variable"))
#   
# 
# write_csv(find_df2, paste0(PrintDir, "/",  Sys.Date(), "percent_change_States_indicators_best_scenario_2020_and_2030.csv"))




# yearly % change 
# df_all<- df %>% mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) %>%  group_by(scenario) %>%  
#   mutate(PfPR_trend = PfPR - lag(PfPR, default = PfPR[1]),
#          U5_PfPR_trend = U5_PfPR - lag(U5_PfPR,default = U5_PfPR[1]),
#          incidence_trend = incidence - lag(incidence, default = incidence[1]),
#          U5_incidence_trend = U5_incidence - lag(U5_incidence, default = U5_incidence[1]),
#          death_rate_trend = death_rate_mean - lag(death_rate_mean, default = death_rate_mean[1]),
#          U5_death_rate_trend = U5_death_rate_mean - lag(U5_death_rate_mean, default = U5_death_rate_mean[1]))

# write_csv(df_all, file.path(WorkDir, "/2020_to_2030_v2/yearly_percent_change_indicators.csv"))


# #2015 % change 
# fin_df <- df %>%  mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) %>% 
#   mutate(PfPR_percent_change = (PfPR - PfPR[year =="2015"])/PfPR[year =="2015"]* 100, 
#     U5_PfPR_percent_change = (U5_PfPR - U5_PfPR[year =="2015"])/U5_PfPR[year =="2015"]* 100, 
#          incidence_percent_change = (incidence - incidence[year =="2015"])/incidence[year =="2015"]* 100, 
#                  U5_incidence_percent_change = (U5_incidence - U5_incidence[year =="2015"])/U5_incidence[year =="2015"]* 100,
#                       death_percent_change = (death_rate_mean - death_rate_mean[year =="2015"])/death_rate_mean[year =="2015"]* 100,
#                            U5_death_percent_change = (U5_death_rate_mean - U5_death_rate_mean[year =="2015"])/U5_death_rate_mean[year =="2015"]* 100)



#subset<- fin_df_2020 %>%  filter(str_detect(year, '2020|2030')) 

# write_csv(fin_df, file.path(WorkDir, "/2020_to_2030_v3/percent_change_indicators_2015_base.csv"))


# for (i in 1:3){
#   files <- list.files(path = simInDir, pattern = "*PAAR_2020_2030.csv|smc_bau_2020_2030.csv|smc_increase80_2020_2030.csv", full.names = T)
#   SMC_df <- sapply(files, read_csv, simplify = F)
# }