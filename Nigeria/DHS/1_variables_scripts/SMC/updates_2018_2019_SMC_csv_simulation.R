rm(list=ls())

setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")


## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape")

lapply(x, library, character.only = TRUE) #applying the library function to packages

smc <- read.csv("bin/smc_table/smc_df2_fin_by_round.csv", colClasses=c("month" = "character")) %>% mutate(adm1 = trimws(adm1), month = as.Date(month, format="%d/%m/%Y"))

smc$coverage = (smc$coverage_high_access * 0.5) + (smc$coverage_low_access*0.5)

smc_hag <- read.csv("bin/smc_table/SMC_overall_cov_Fhag_2018_2019.csv") %>% mutate(adm1 = trimws(adm1)) %>% 
  rename(coverage_updated = coverage)
summary(smc_hag$Fhag)

smc_2 <- left_join(smc, smc_hag, by = c("adm1", "round", "year"))

file_split <- split(smc_2, smc$year)

# Fhag is high access fraction
file_split$`2018` <- file_split$`2018` %>% mutate(coverage_high_access = ifelse(grepl('Jiga|Kat|Sok|Zam',adm1),1, coverage_high_access)) %>% 
                  mutate(coverage_low_access = (coverage_updated - Fhag)/(1 - Fhag),
                         coverage = ifelse(grepl('Jiga|Kat|Sok|Zam',adm1), coverage_updated, coverage))



file_split$`2019` <- file_split$`2019` %>% mutate(coverage_high_access = ifelse(grepl('Jiga|Kat|Sok|Yob|Zam',adm1),1, coverage_high_access)) %>% 
  mutate(coverage_low_access = (coverage_updated - Fhag)/(1 - Fhag),
         coverage = ifelse(grepl('Jiga|Kat|Sok|Yob|Zam',adm1), coverage_updated, coverage))

df <- dplyr::bind_rows(file_split)

df$start_sim <- as.Date("01/01/2010", format="%d/%m/%Y")




# df_2 <- df %>% mutate(days = month - start_sim, simday =substr(days, 1, 4)) %>%dplyr::select(-c(month, coverage, Fhag, start_sim, days)) 
# write.csv(df_2, "results/archetype_sim_input/Intervention_files_LGA/smc_cc_2010_2020.csv")

# making dataset for sim run 

# df_2 <- df  %>% mutate(month_new = as.numeric(substr(month, 6, 7)), 
#                                                  coverage_low_access = ifelse(is.na(coverage_low_access), 0.2, coverage_low_access)) %>% 
#   dplyr::select(-c(month, simday, month, Fhag, start_sim)) #coverage
# 
# summary(df_2$month_new)

write.csv(df, "bin/projection/s1/smc_LGA_v3.csv")
