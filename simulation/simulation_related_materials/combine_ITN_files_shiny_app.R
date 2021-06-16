library(tidyverse)

rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v3")
PrintDir<- file.path("C:/Users/ido0493/Documents/hbhi-nigeria-shiny-app/src/app/data")

scen_dat <- read.csv(file.path(WorkDir, "Intervention_scenarios_nigeria_v3.csv"))



for (row in 1:nrow(scen_dat)){
  files <- list.files(path = WorkDir, pattern = "*itn_", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}

df[[2]]= NULL

df[[1]]$scenario = "Scenario 1(Business as Usual)"
df[[2]]$scenario =  "Scenario 6(Considered for funding in the NSP)"
df[[3]]$scenario = "Scenario 3(Improved coverage)"
df[[4]]$scenario = "Scenario 4(Improved coverage)"
df[[5]]$scenario = "Scenario 5(Improved coverage)"
df[[6]]$scenario = "Scenario 2(High effective coverage)"

#create kill rate df 
df <- map(df, ~dplyr::select(.x, LGA, kill_rate, round, simday, scenario , year))

df <- map(df, ~dplyr::mutate(.x, kill_rate =kill_rate *100))

write.csv(df[[1]], file.path(PrintDir, 'ITN_killrate_scenario1.csv'))
