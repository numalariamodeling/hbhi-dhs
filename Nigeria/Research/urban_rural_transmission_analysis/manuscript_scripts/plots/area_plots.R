user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[/]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
SimDir <- file.path(ProjectDir, 'simulation_input')
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <- file.path(NGDir, "data")
Rdata <- file.path(DataDir, "urban_malaria_rdata")
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src")
ResearchDir <- file.path(SrcDir,'Research', 'urban_rural_transmission_analysis')
DHSDir <- file.path(SrcDir, "DHS")
VarDir <- file.path(DHSDir, "1_variables_scripts")

# # Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt")
# 
lapply(x, library, character.only = TRUE) #applying the library function to packages

df1000m <- read.csv("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data/DHS/Computed_cluster_information/urban_malaria_covariates/pop_density_1000m_buffer_DHS_10_15_18.csv")
df2km <- read.csv("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data/DHS/Computed_cluster_information/urban_malaria_covariates/pop_density_2km_buffer_DHS_10_15_18.csv")
df2000m <- read.csv("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data/DHS/Computed_cluster_information/urban_malaria_covariates/pop_density_2000m_buffer_DHS_10_15_18.csv")
dfFB_1000m <- read.csv("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data/DHS/Computed_cluster_information/urban_malaria_covariates/pop_density_FB_1000m_buffer_DHS_10_15_18.csv")
dfFB_2000m <- read.csv("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data/DHS/Computed_cluster_information/urban_malaria_covariates/pop_density_FB_2000m_buffer_DHS_10_15_18.csv")

dfold <- read.csv("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data/DHS/Computed_cluster_information/cluster_data_all_years/Nigeria_2010_2018_clustered_final_dataset.csv")
dfold2010 <- dfold%>% filter(data_source == "mis2010")
dfold2015 <- dfold%>% filter(data_source == "mis2015")
dfold2018 <- dfold%>% filter(data_source == "dhs2018")

hist(df1000m$gpw_v4_population_density_rev11_2010_1_deg)
hist(df2km $gpw_v4_population_density_rev11_2010_1_deg)
hist(df2000m$gpw_v4_population_density_rev11_2010_1_deg)
hist(dfFB_1000m$gpw_v4_population_density_rev11_2010_1_deg)
hist(dfFB_2000m$gpw_v4_population_density_rev11_2010_1_deg)
hist(dfold2010$pop_den)
hist(dfold2010$pop_count)


hist(df1000m$gpw_v4_population_density_rev11_2015_1_deg)
hist(df2km $gpw_v4_population_density_rev11_2015_1_deg)
hist(df2000m$gpw_v4_population_density_rev11_2015_1_deg)
hist(dfFB_1000m$gpw_v4_population_density_rev11_2015_1_deg)
hist(dfFB_2000m$gpw_v4_population_density_rev11_2015_1_deg)
hist(dfold2015$pop_den)
hist(dfold2015$pop_count)


hist(df1000m$gpw_v4_population_density_rev11_2020_1_deg)
hist(df2km $gpw_v4_population_density_rev11_2020_1_deg)
hist(df2000m$gpw_v4_population_density_rev11_2020_1_deg)
hist(dfFB_1000m$nga_general_2020)
hist(dfFB_2000m$nga_general_2020)
hist(dfold2018$pop_den)
hist(dfold2018$pop_count)



#merging columns
melt1000m <- melt(df1000m, id.vars=c("X",'.id', 'ID'),var='pop_den')
meltdf2km <- melt(df2km, id.vars=c("X",'.id', 'ID'),var='pop_den')
meltdfFB_1000m <- melt(dfFB_1000m, id.vars=c("X",'.id', 'ID'),var='pop_den')
meltdfFB_2000m <- melt(dfFB_2000m, id.vars=c("X",'.id', 'ID'),var='pop_den')


#pop den for all years
hist(melt1000m$value)
hist(meltdf2km $value)
hist(meltdfFB_1000m$value)
hist(meltdfFB_2000m$value)


library(tidyverse)
dfold %>% group_by(pop_den) %>% count(pop_den) %>%
  ggplot(aes(pop_den = as.factor(pop_den), y  = n)) +
  geom_bar(stat = "identity")
