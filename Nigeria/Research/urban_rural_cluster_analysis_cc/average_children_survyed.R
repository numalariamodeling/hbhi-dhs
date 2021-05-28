rm(list=ls())

#important to download the github version of tidyverse.Uncomment the script below to run
#install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("r-pkgs/usethis")

#devtools::install_github("hadley/tidyverse")

## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
#Mis15DIR <- file.path(DataDir, "data")

ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src", "DHS")
VarDir <- file.path(SrcDir, "1_variables_scripts")
ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
SimDir <- file.path(ProjectDir, 'simulation_input')





## -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(VarDir, "generic_functions", "DHS_fun.R"))




## -----------------------------------------
### Call in Chilo's function files 
## -----------------------------------------

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/src/Research/urban_rural_cluster_analysis_cc", "functions", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)



## -----------------------------------------
### Read in the data and subset 
## -----------------------------------------

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

dhs <-read.files( ".*NGPR7AFL.*\\.DTA", DataDir, read_dta) %>% map(~filter(.x, hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6)))


mis15 <-read.files( ".*NGPR71FL.*\\.DTA", DataDir, read_dta) %>% map(~filter(.x, hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6)))


mis10 <-read.files( ".*NGPR61FL.*\\.DTA", DataDir, read_dta) %>% map(~filter(.x, hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6)))





look_for(dhs2 [[1]], "visitor")

table(dhs2[[1]]$v135) # frequency table for smear test 

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_df <- dhs[[1]] %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6))
pfpr_df_mis15 <- mis15[[1]] %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6))
pfpr_df_mis10 <- mis10[[1]] %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6))


#df <- dplyr::bind_rows(pfpr_df, pfpr_df_mis15, pfpr_df_mis10)


#dhs2018 survey children per cluster histogram 
hist(table(pfpr_df$hv001))

#mis2015 survey children per cluster histogram
hist(table(pfpr_df_mis15$hv001))
     
#mis2010 survey children per cluster histogram
hist(table(pfpr_df_mis10$hv001))     

#averags
ave_chldrn_18 <- mean(table(pfpr_df$hv001))
ave_chldrn_15 <- mean(table(pfpr_df_mis15$hv001))
ave_chldrn_10 <- mean(table(pfpr_df_mis10$hv001))

