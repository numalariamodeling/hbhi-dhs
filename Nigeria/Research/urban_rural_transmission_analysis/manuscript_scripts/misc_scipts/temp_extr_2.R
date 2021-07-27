## These scripts are used to extract cluster level data and variables for urban settings in Nigeria 

rm(list=ls())
memory.limit(size = 56000)

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
## ----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'Temp_covereates')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
ResultDir <-file.path(ProjectDir, "results")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
RastDir <- file.path(DataDir, "Raster_files", "temp_erafall_monthly", "2018")
GlobDir <- file.path(NuDir, 'data', 'africa_health_district_climate', 'climate', 'global')
# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "Nigeria functions.R"))



## ----------------------------------------------------
### Read in PR  data (DHS 2010, 2015, 2018)  
## ----------------------------------------------------

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyze


## ----------------------------------------------------
### Geospatial covariates extraction 
## ----------------------------------------------------
dhs_18 <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs_15 <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs_10 <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

vars <- c(0, 1000, 2000, 3000, 4000)


#_______________________________2018 


#####################################August#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*08.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2018', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_18, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('aug_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}

#####################################September#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*09.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2018', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_18, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('sep_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


##################################Octomber##########################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*10.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2018', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_18, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('oct_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}

#####################################November#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*11.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2018', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_18, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('nov_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}

################################## December##########################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*12.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2018', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_18, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('dec_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}

#_______________________________2015

#####################################August#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*08.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2015', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_15, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('aug_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))
}

#####################################September#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*09.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2015', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_15, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('sep_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))
}

##################################Octomber##########################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*10.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2015', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_15, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('oct_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))
}

#####################################November#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*11.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2015', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_15, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('nov_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))
}

################################## December##########################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*12.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2015', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_15, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('dec_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))
}



#_______________________________2010

#####################################August#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*08.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2013', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_10, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('aug_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))
}

#####################################September#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*09.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2013', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_10, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('sep_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))
}

##################################Octomber##########################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*10.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2013', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_10, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('oct_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))
}

#####################################November#################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*11.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2013', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_10, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('nov_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))
}

################################## December##########################################################

files <- list.files(path =file.path(GlobDir ,"air_temp_era5")  ,pattern = "*12.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2013', files))]
raster<-sapply(files, raster, simplify = F)

# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs_10, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('dec_temp_era_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))
}

#END

