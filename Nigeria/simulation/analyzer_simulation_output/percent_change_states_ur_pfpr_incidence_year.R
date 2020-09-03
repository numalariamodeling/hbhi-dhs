# create baseline file 
ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")

scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
    files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"], "state"), pattern = "*.csv", full.names = TRUE)
    df <- sapply(files, read_csv, simplify = F)
}

df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, U5_PfPR, incidence)



df_all<- df %>% mutate(State = str_split(.id, "_", simplify = TRUE)[,8], 
         State = str_sub(State, 1, str_length(State)-4), scenario = str_split(.id, "/", simplify = T)[, 10]) %>%  
  filter(year != "2020")

df_baseline <- df %>% mutate(State = str_split(.id, "_", simplify = TRUE)[,8], 
                             State = str_sub(State, 1, str_length(State)-4), scenario = str_split(.id, "/", simplify = T)[, 10]) %>%  
  filter(year == "2020" & scenario == "NGA projection scenario 1")

fin_df <- bind_rows(df_all, df_baseline) %>% group_by(State) %>% 
  mutate(PfPR_percent = (U5_PfPR - U5_PfPR[year =="2020"])/U5_PfPR[year =="2020"]* 100, 
         incidence_percent = (incidence - incidence[year =="2020"])/incidence[year =="2020"]* 100) %>% arrange(scenario)

         

write_csv(fin_df, file.path(WorkDir, "/2020_to_2030_v2/percent_change_States_U5_PfPR_All_age_incidence.csv"))

