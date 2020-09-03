rm(list=ls())

# install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("hadley/tidyverse")
## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape")

lapply(x, library, character.only = TRUE) #applying the library function to packages


file_path <- "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/projection_csvs/projection_v2"

in_10_fun <- function(x){ifelse(x < 0.80, pmin(0.30 +x, 0.80), x)} #function to increase by 10% 

cm <- read.csv(paste0(file_path, "/HS_placeholder.csv")) %>% 
  mutate_at(vars(ends_with("coverage")| ends_with("cases")), in_10_fun)
head(cm)
summary(cm$adult_coverage)


write.csv(cm, paste0(file_path, "/cm_scen2_30_v3.csv"))


itn <- read.csv(paste0(file_path, "/itn_scenario1.csv")) %>% 
  mutate_at(vars(ends_with("ITN_use")), list(~. +0.30))
head(itn)
head(itn_cc)

write.csv(itn, paste0(file_path, "/ITN_scen2_30_v3.csv"))

