## These scripts are used to extract cluster level data and variables for urban settings in Nigeria 

rm(list=ls())
memory.limit(size = 50000)

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", "randomGLM", "ROCR", "caretEnsemble", "klaR", "naniar", "corrplot", 
       "lmtest", "Deducer", "lattice", "ResourceSelection", "rlist")

lapply(x, library, character.only = TRUE) #applying the library function to packages


## -----------------------------------------
### Paths
## -----------------------------------------

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <- file.path(NGDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
BinDir <- file.path(DataIn, "bin")
SrcDir <- file.path(NGDir, 'src', 'Research', 'urban_rural_transmission_analysis')



# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "Nigeria functions.R"))


## ----------------------------------------------------
### Read in PR  data (DHS 2010, 2015, 2018)  
## ----------------------------------------------------

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

dhs <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL|NGPR71FL|NGPR61FL', read_dta)  #reads in the PR files





## ----------------------------------------------------
### Geospatial covariates extraction 
## ----------------------------------------------------


dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 
files <- list.files(path = file.path(DataDir, "Raster_files") , pattern = "*deg.tif$", full.names = TRUE, recursive = TRUE)
files<- files[(grep('gpw_v4', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction 
vars <- c(0, 1000, 2000)


df <- map2(dhs, raster, get_crs)
df <- pmap(list(raster, df, vars[1]), extract_fun)
df <- plyr::ldply(df)
write.csv(df, file =file.path(DataIn, paste0('pop_density_', as.character(vars[1]), '_km_buffer', "_DHS_10_15_18.csv")))



dhs <- map2(dhs, raster, get_crs)
dhs <- pmap(list(raster, dhs, 2000), extract_fun)
write.csv(df, file =file.path(DataIn, paste0('pop_density_2km_buffer', "_DHS_10_15_18.csv")))



# # population density
# clu_pop_den <- read.csv("NGGC7BFL.csv")%>% 
#   dplyr::select(hv001 = DHSCLUST, pop_den = UN_Population_Density_2015)%>% 
#   mutate(l_pop_den = log(pop_den))
# 
# summary(clu_pop_den$l_pop_den)
# 
# #state names
# 
# states18 <- pfpr_df[,c("hv001", "shstate")]
# colnames(states18)[2]<- "state"
# states18 <- as_label(states18)
# 
# #regiosns
# regions18 <- pfpr_df[,c("hv001", "hv024")]
# colnames(regions18)[2]<- "region"
# regions18 <- as_label(regions18)
# 
# ##################### Extraction of data raster files using DHS points ##################
# #loading temperature raster file
# building_rastr <- raster(file.path(NGDir, "data/Raster_files/NGA_buildings_count.TIF"))
# plot(building_rastr)
# 
# #loading dhs 2018 points 
# dhs18_sp <- readOGR(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),)
# buffered_dhs18 <- buffer(dhs18_sf, width=2000) #buffered by 2km
# plot(buffered_dhs18)
# 
# ## S4 method for signature 'Raster,SpatialPolygons'
# bulding_count <- mask(building_rastr, buffered_dhs18)
# 
# 
# # bind the datasets 
# 
# dhs_clu <- left_join(clu_est, clu_wealth, by = "hv001") %>% 
#   left_join(., clu_u5_prop, by = "hv001") %>% 
#   left_join(., clu_edu, by = "hv001") %>% 
#   left_join(., clu_floor, by = "hv001") %>% 
#   left_join(., clu_wall, by = "hv001") %>% 
#   left_join(., clu_roof, by = "hv001") %>% 
#   left_join(., clu_housing_q, by = "hv001") %>% 
#   left_join(., clu_pfpr_itn_use, by = "hv001") %>% 
#   left_join(., clu_hh_members_age, by = "hv001") %>%
#   left_join(., pfpr_rural, by = "hv001") %>%
#   left_join(., clu_sex, by = "hv001") %>%
#   left_join(., clu_hh_size, by = "hv001") %>%
#   left_join(., clu_u5_care, by = "hv001") %>%
#   left_join(., clu_pop_den, by = "hv001") %>%
#   left_join(., states18, by = "hv001") %>%
#   left_join(., regions18, by = "hv001") 
# 
# dhs_clu['data_source'] = 'dhs2018'
# 
# 


