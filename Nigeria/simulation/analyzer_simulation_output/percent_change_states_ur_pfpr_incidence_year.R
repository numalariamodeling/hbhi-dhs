#This script processes simulation output to generate percent change in outcomes with a 2015, 2020 and 2019 baseline at the state and national level in Nigeria
# Created by Ifeoma Ozodiegwu 

#change file paths as needed 


###############################################################################
# create file paths 
###############################################################################



rm(list = ls())
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
TeamDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v5")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src")
simInDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v4")
dir.create(file.path(ProcessDir, "percent_change_tables"), showWarnings = FALSE)
PrintDir <- file.path(ProcessDir, "percent_change_tables")
dir.create(file.path(PrintDir, "national"), showWarnings = FALSE)
UpdatePrintDir <- file.path(ProcessDir, "percent_change_tables", "national")
dir.create(file.path(PrintDir, "SMC_areas"), showWarnings = FALSE)
SMC_areas <- file.path(PrintDir, "SMC_areas")
dir.create(file.path(PrintDir, "State"), showWarnings = FALSE)
StateDir <- file.path(PrintDir, "State")
source(file.path(ScriptDir,"DHS", "1_variables_scripts","generic_functions", "DHS_fun.R"))
source(file.path(ScriptDir, "simulation/analyzer_simulation_output", "functions_percent_change.R")) #reads in the percentage change and sum functions


#baseline year options used in the paper - 2020, 2019 and 2015 
year = 2019

###############################################################################
# national level % change 
###############################################################################

all_df  = list()

names = c("mean", "0", "1", "2", "3", "4")

for (i in 1:length(names)){

  scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

  for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = paste0("*annual_indicators_2020_2030_", names[i], ".csv"), full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
  }
    
    
    


#read in 2019 annual indicators

df_2019 <- read_csv(file.path(WorkDir, "2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc", "annual_indicators_2011_2020.csv")) %>% 
  rename(death_rate_mean_all_ages = death_rate_mean,
         death_rate_mean_U5 = U5_death_rate_mean)

df[['C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2030_v3/NGA projection scenario 0/annual_indicators_2020_2030.csv']] <- df_2019



 fin_df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR_all_ages, PfPR_U5,  incidence_all_ages, incidence_U5, death_rate_mean_all_ages, 
                                               death_rate_mean_U5) %>% 
  mutate(scenario = str_split(.id, "/", simplify = T)[, 10])

                          
#% change 
    if (year == 2020) {
      df_base  <- percent_change_fun_20(fin_df, FALSE)
      df_base$run_number <- paste0('run number', " ", names[i]) 
      write_csv(df_base, paste0(UpdatePrintDir, "/",  Sys.Date(), "_percent_change_indicators_", as.character(year), "_base_", names[i],".csv"))
    
    } else if (year == 2019) {
       df_base  <- percent_change_fun_19(fin_df, FALSE)
       df_base$run_number <- paste0('run number', " ", names[i]) 
       write_csv(df_base, paste0(UpdatePrintDir, "/",  Sys.Date(), "_percent_change_indicators_", as.character(year), "_base_", names[i],".csv"))
     
    } else {
      df_base  <- percent_change_fun_15(fin_df, FALSE)
      df_base$run_number <- paste0('run number', " ", names[i]) 
      write_csv(df_base, paste0(UpdatePrintDir, "/",  Sys.Date(), "_percent_change_indicators_", as.character(year), "_base_", names[i],".csv"))
    }


all_df[[i]] <- df_base

}



if (year == 2020){
all_df2 = all_df %>%  map(~filter(., (year == 2020| year == 2025| year == 2030) & run_number != "run number mean"))

df_com = plyr::ldply(all_df2, rbind) %>%  group_by(scenario, year) %>% summarise_at(vars(ends_with('_change')), list(min = min, max = max))

mean_df2 = all_df %>%  map(~filter(., (year == 2020| year == 2025| year == 2030) & run_number == "run number mean")) %>%  plyr::ldply(rbind)

mean_df2 = mean_df2 %>%  dplyr::select(scenario, year, ends_with('_change'))

fin_df = left_join(df_com, mean_df2)%>% dplyr::select(scenario, year, sort(names(.)))

#subsetting by year 
fin_df_2025 = fin_df %>%  filter(year == 2025) %>%   mutate(across(is.numeric, ~ round(., 1)))
fin_df_2030 = fin_df %>%  filter(year == 2030) %>%   mutate(across(is.numeric, ~ round(., 1)))



#manuscript tables
man_tables_pfpr = tibble::tibble(Scenario = c(1, 2, 3, 4), 
`All age PfPR percent change in 2025 relative to BAU 2020 (%)` = c(paste0(fin_df_2025$PfPR_percent_change, " (",
                                                                          fin_df_2025$PfPR_percent_change_max, ", ",fin_df_2025$PfPR_percent_change_min, ")")),

 `U5 PfPR percent change in 2025 relative to BAU 2020 (%)` = c(paste0(fin_df_2025$U5_PfPR_percent_change, " (",
                                                                     fin_df_2025$U5_PfPR_percent_change_max, ", ",fin_df_2025$U5_PfPR_percent_change_min, ")")),

`All age PfPR percent change in 2030 relative to BAU 2020 (%)` = c(paste0(fin_df_2030$PfPR_percent_change, " (",
                                                                          fin_df_2030$PfPR_percent_change_max, ", ", fin_df_2030$PfPR_percent_change_min, ")")),

`U5 PfPR percent change in 2030 relative to BAU 2020 (%)` = c(paste0(fin_df_2030$U5_PfPR_percent_change, " (",
                                                                     fin_df_2030$U5_PfPR_percent_change_max, ", ", fin_df_2030$U5_PfPR_percent_change_min, ")")))                                                                


man_tables_incidence = tibble::tibble(Scenario = c(1, 2, 3, 4), 
`All age incidence percent change in 2025 relative to BAU 2020 (%)` = c(paste0(fin_df_2025$incidence_percent_change, " (",
                                                                         fin_df_2025$incidence_percent_change_max, ", ",fin_df_2025$incidence_percent_change_min, ")")),

`U5 incidence percent change in 2025 relative to BAU 2020 (%)` = c(paste0(fin_df_2025$U5_incidence_percent_change, " (",
                                                                    fin_df_2025$U5_incidence_percent_change_max, ", ",fin_df_2025$U5_incidence_percent_change_min, ")")),

`All age incidence  percent change in 2030 relative to BAU 2020 (%)` = c(paste0(fin_df_2030$incidence_percent_change, " (",
                                                                         fin_df_2030$incidence_percent_change_max, ", ", fin_df_2030$incidence_percent_change_min, ")")),

`U5 incidence percent change in 2030 relative to BAU 2020 (%)` = c(paste0(fin_df_2030$U5_incidence_percent_change, " (",
                                                                    fin_df_2030$U5_incidence_percent_change_max, ", ", fin_df_2030$U5_incidence_percent_change_min, ")")))                                                                


man_tables_mortality = tibble::tibble(Scenario = c(1, 2, 3, 4), 
`All age mortality percent change in 2025 relative to BAU 2020 (%)` = c(paste0(fin_df_2025$death_percent_change, " (",
                                                                               fin_df_2025$death_percent_change_max, ", ",fin_df_2025$death_percent_change_min, ")")),

`U5 mortality percent change in 2025 relative to BAU 2020 (%)` = c(paste0(fin_df_2025$U5_death_percent_change, " (",
                                                                           fin_df_2025$U5_death_percent_change_max, ", ",fin_df_2025$U5_death_percent_change_min, ")")),

`All age mortality percent change in 2030 relative to BAU 2020 (%)` = c(paste0(fin_df_2030$death_percent_change, " (",
                                                                                fin_df_2030$death_percent_change_max, ", ", fin_df_2030$death_percent_change_min, ")")),

`U5 mortality percent change in 2030 relative to BAU 2020 (%)` = c(paste0(fin_df_2030$U5_death_percent_change, " (",
                                                                           fin_df_2030$U5_death_percent_change_max, ", ", fin_df_2030$U5_death_percent_change_min, ")")))                                                                


write_csv(fin_df, paste0(UpdatePrintDir, "/",  Sys.Date(), "_", as.character(year), "_base_2020_2025_2030_with_intervals", ".csv"))
write_csv(man_tables_pfpr, paste0(UpdatePrintDir, "/",  Sys.Date(), "_", as.character(year), "_pfpr_base_2020_2025_2030_with_intervals", ".csv"))
write_csv(man_tables_incidence, paste0(UpdatePrintDir, "/",  Sys.Date(), "_", as.character(year), "_incidence_base_2020_2025_2030_with_intervals", ".csv"))
write_csv(man_tables_mortality, paste0(UpdatePrintDir, "/",  Sys.Date(), "_", as.character(year), "_mortality_base_2020_2025_2030_with_intervals", ".csv"))

} else if (year == 2019){
  all_df2 = all_df %>%  map(~filter(., (year==2019|year == 2020| year == 2025| year == 2030) & run_number != "run number mean"))
  
  df_com = plyr::ldply(all_df2, rbind) %>%  group_by(scenario, year) %>% summarise_at(vars(ends_with('_change')), list(min = min, max = max))
  
  mean_df2 = all_df %>%  map(~filter(., (year==2019|year == 2020| year == 2025| year == 2030) & run_number == "run number mean")) %>%  plyr::ldply(rbind)


} else {
  all_df2 = all_df %>%  map(~filter(., (year ==2015 |year == 2020| year == 2025| year == 2030) & run_number != "run number mean"))
  
  df_com = plyr::ldply(all_df2, rbind) %>%  group_by(scenario, year) %>% summarise_at(vars(ends_with('_change')), list(min = min, max = max))
  
  mean_df2 = all_df %>%  map(~filter(., (year==2015|year == 2020| year == 2025| year == 2030) & run_number == "run number mean")) %>%  plyr::ldply(rbind)
  #fin_df = left_join(df_com, mean_df2)%>% dplyr::select(scenario, year, sort(names(.)))
}



###############################################################################
# SMC States
###############################################################################
all_df  = list()

names = c("mean", "0", "1", "2", "3", "4")

for (i in 1:length(names)){
      scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv")) %>%  filter(Scenario_no %in% c(1, 2, 3, 4))

      for (row in 1:nrow(scen_dat)){
        files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = paste0("*annual_indicators_each_LGA_", names[i], ".csv"), full.names = TRUE)
        df <- sapply(files, read_csv, simplify = F)
      }
    




files <- list.files(path = simInDir, pattern = "*_PAAR_2020_2030.csv", full.names = T, recursive = T)
SMC_df <- sapply(files, read_csv, simplify = F)



SMC_LGA <- map(SMC_df, ~ .x %>% 
      distinct(LGA)) 


# we use the sum custom function to compute indicators 
df_ls <- map2(df, SMC_LGA, sum_fun)

df <- plyr::ldply(df_ls, rbind)



df_all<- df %>% mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) 


fin_df <- percent_change_fun_20(df_all, FALSE) 
fin_df$run_number <- paste0('run number', " ", names[i]) 

write_csv(fin_df, paste0(SMC_areas,"/",  Sys.Date(), "_percent_change_indicators_2020_base_SMC_states", names[i], ".csv"))

all_df[[i]] <- fin_df 

}


all_df2 = all_df %>%  map(~filter(., (year == 2020| year == 2025| year == 2030) & run_number != "run number mean"))

df_com = plyr::ldply(all_df2, rbind) %>%  group_by(scenario, year) %>% summarise_at(vars(ends_with('_change')), list(min = min, max = max))

mean_df2 = all_df %>%  map(~filter(., (year == 2020| year == 2025| year == 2030) & run_number == "run number mean")) %>%  plyr::ldply(rbind)

mean_df2 = mean_df2 %>%  dplyr::select(scenario, year, ends_with('_change'))

fin_df = left_join(df_com, mean_df2)%>% 
  dplyr::select(scenario, year, sort(names(.)))


fin_df_2025 = fin_df %>%  filter(year == 2025, scenario !='NGA projection scenario 2') %>%   mutate(across(is.numeric, ~ round(., 1)))
fin_df_2030 = fin_df %>%  filter(year == 2030, scenario !='NGA projection scenario 2') %>%   mutate(across(is.numeric, ~ round(., 1)))


#manuscript table 
man_tables_SMC = tibble::tibble(Scenario = c(1, 3, 4), 
`U5 incidence percent change in 2025 relative to BAU 2020  (%)` = c(paste0(fin_df_2025$U5_incidence_percent_change, " (",
                     fin_df_2025$U5_incidence_percent_change_max, ", ",fin_df_2025$U5_incidence_percent_change_min, ")")),

`U5 mortality percent change in 2025 relative to BAU 2020 (%)` = c(paste0(fin_df_2025$U5_death_percent_change, " (",
                fin_df_2025$U5_death_percent_change_max, ", ",fin_df_2025$U5_death_percent_change_min, ")")),

`U5 incidence percent change in 2030 relative to BAU in 2020 (%)` = c(paste0(fin_df_2030$U5_incidence_percent_change, " (",
                     fin_df_2030$U5_incidence_percent_change_max, ", ", fin_df_2030$U5_incidence_percent_change_min, ")")),

`U5 mortality percent change in 2030 relative to BAU 2020 (%)` = c(paste0(fin_df_2030$U5_death_percent_change, " (",
                fin_df_2030$U5_death_percent_change_max, ", ", fin_df_2030$U5_death_percent_change_min, ")")))                                                                


write_csv(fin_df, paste0(SMC_areas, "/",  Sys.Date(), "_2020_base_2020_2025_2030_with_intervals_SMC_states", ".csv"))
write_csv(man_tables_SMC, paste0(SMC_areas, "/",  Sys.Date(), "_U5_incidence_deaths_2020_base_2020_2025_2030_with_intervals_SMC_states", ".csv"))




###############################################################################
# state analysis
###############################################################################

scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
    files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_", full.names = TRUE)
    files<- files[-(grep('_funder_|_2020_2030|each_LGA', files))]
    df <- sapply(files, read_csv, simplify = F)
}




df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR_all_ages, PfPR_U5,
                                               incidence_all_ages, incidence_U5, death_rate_1_all_ages,death_rate_2_all_ages,death_rate_1_U5, death_rate_2_U5)



df_all<- df %>% mutate(State =  str_split(.id, "indicators_", simplify = TRUE)[,2],
  State = str_sub(State, 1, str_length(State)-4), scenario = str_split(.id, "/", simplify = T)[, 10],
  death_rate_mean_all_ages= (death_rate_1_all_ages + death_rate_2_all_ages)/2, death_rate_mean_U5 = (death_rate_1_U5 + death_rate_2_U5)/2)





fin_df <- percent_change_fun_20(df_all, TRUE)


write_csv(fin_df, paste0(StateDir,  "/",Sys.Date(), "_percent_change_States_indicators_2020_base.csv"))



#create new variables to detects the column with the greatest reduction in indicators or the best scenario

find_df2 <- fin_df%>%  filter(year ==2020 | year == 2030 | year == 2025) %>%
  group_by(State, year) %>%
  mutate(pfpr_min = ifelse(PfPR_percent_change == min(PfPR_percent_change), 1, 0),
         U5_pfpr_min = ifelse(U5_PfPR_percent_change == min(U5_PfPR_percent_change), 1, 0),
         incidence_min = ifelse(U5_PfPR_percent_change == min(U5_PfPR_percent_change), 1, 0),
         U5_incidence_min = ifelse(U5_incidence_percent_change == min(U5_incidence_percent_change), 1, 0),
         death_min = ifelse(death_percent_change == min(death_percent_change), 1, 0),
         U5_death_min = ifelse(U5_death_percent_change == min(U5_death_percent_change), 1, 0))%>%
  rowwise() %>%
  mutate(sumVar = sum(c_across(pfpr_min:U5_death_min)),
         best_scenario = ifelse(sumVar == 6, scenario, "variable"))


write_csv(find_df2, paste0(StateDir, "/",  Sys.Date(), "percent_change_States_indicators_best_scenario_2020_2025_and_2030.csv"))


