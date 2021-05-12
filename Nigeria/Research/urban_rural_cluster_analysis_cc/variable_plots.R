rm(list = ls())

#Reading in the necessary packages 
x <- c("tidyverse", "ggplot2",  "ggcorrplot", "gridExtra")

lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)
## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("mambrose" %in% user) {
  user_path <- file.path("C:/Users", user)
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "NU-malaria-team")
  NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  SrcDir <- file.path(NGDir, "src", "DHS")
  BinDir <- file.path(NGDir, "bin")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Box", "NU-malaria-team")
  NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  PlotDir <-file.path(ResultDir, "research_plots")
  BinDir <- file.path(NGDir, "bin")
  SrcDir <- file.path(NGDir, "src", "Research", "urban_rural_cluster_analysis_cc")
  ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
}

#Read in functions 

source(file.path(SrcDir, "functions", "log_reg_functions.R"))
# Read in data file 
histogram <- T 
type <- "urban/rural"

merged_df <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), header= TRUE)

df <- merged_df[ ,colnames(merged_df) 
                   %in% c("l_pop_den", "sex_f", "wealth_2", "edu_a", "hh_size", "ACT_use_u5", 
                          "hh_members_age", "humidindex", "annual_precipitation", "net_use", "build_count", "Rural_urban")]
  
densityPlot(~ df$edu_a, show.bw=TRUE, data=Prestige)

  
