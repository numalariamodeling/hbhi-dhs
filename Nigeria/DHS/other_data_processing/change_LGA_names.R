library(tidyr)
library(stringr)
# change names in LGA files 

setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

ITN_files <- read.csv("bin/ITN_by_LGA.csv")
pfpr_2018 <- read.csv

itn_lga_age <- list.files(path = "results/archetype_sim_input/Intervention_files_LGA/ITN", 
                               pattern = "*.csv", full.names = TRUE)

itn_lga_age_v2 <- sapply(itn_lga_age, read.csv, simplify = F)

ITN_files$LGA_nu <- gsub("\\/", "-", ITN_files$LGA)
write.csv(ITN_files, "results/archetype_sim_input/Intervention_files_LGA/ITN/ITN_by_LGA.csv")


 