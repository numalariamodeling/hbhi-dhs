library(tidyverse)

rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v3")


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
