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
RastDir <- file.path(DataDir, "Raster_files")
dwnds <-  file.path(Drive, "Downloads")
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

#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir , "accessibility") ,pattern = "*GA.tiff$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('2015_accessibility', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('access_to_cities_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('access_to_cities_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# accessibility_to_cities_ extraction 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('access_to_cities_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))  
} 

############################## friction_decompressed_###############################################
#                              friction_decompressed
#                              friction_decompressed
#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir , "accessibility") ,pattern = "*GA.tiff$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('Decompressed', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('friction_decompressed_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('friction_decompressed_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('friction_decompressed_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))  
} 



############################## motorized_friction###############################################
#                              motorized_friction
#                              motorized_friction
#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir , "accessibility") ,pattern = "*GA.tiff$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('motorized_friction', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('motorized_friction_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('motorized_friction_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('motorized_friction_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))  
} 


############################## motorized_travel_time_to_healthcare ###############################################
#                              motorized_travel_time_to_healthcare
#                              motorized_travel_time_to_healthcare
#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir , "accessibility") ,pattern = "*GA.tiff$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('motorized_travel_', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('motorized_travel_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('motorized_travel_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('motorized_travel_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))  
} 


############################## walking_only_friction_surface ###############################################
#                              walking_only_friction_surface
#                              walking_only_friction_surface
#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir , "accessibility") ,pattern = "*GA.tiff$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('walking_only_friction_', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('walking_friction_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('walking_friction_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('walking_friction_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))  
} 



############################## walking_only_travel_time_to_healthcare ###############################################
#                              walking_only_travel_time_to_healthcare
#                              walking_only_travel_time_to_healthcare
#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir , "accessibility") ,pattern = "*GA.tiff$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('only_travel', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('walking_travel_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('walking_travel_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('walking_travel_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv")))  
} 


#########b###################################### building sensity #################

#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir , "NGA_buildings_v1_1") ,pattern = "*ity.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('NGA', files))]
raster<-sapply(files, raster, simplify = F)



# building density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('building_density_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('building_density_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('building_density_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv"))) 

}

#_________________________elevation 

#########b###################################### building sensity #################

#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(RastDir, "elevation") ,pattern = "*ELE.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('EL', files))]
raster<-sapply(files, raster, simplify = F)



# building density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('elevation_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[1]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('elevation_', as.character(vars[1]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('elevation_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv"))) 
  
}

#_________________________U5 POP DEN 

#########b###################################### U5 POP DENsity #################

#######################################################################################################


#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 2010. 2015 and 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files

files <- list.files(path = file.path(Drive, "Downloads") ,pattern = "*020.tif$", 
                    full.names = TRUE, recursive = TRUE)
files<- files[(grep('children', files))]
raster<-sapply(files, raster, simplify = F)



# building density extraction with just columbia data 
vars <- c(0, 1000, 2000, 3000, 4000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('pop_den_u5_FB_', as.character(vars[i]), 'm_buffer', "_DHS_18.csv")))
}


###################20015####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('pop_den_u5_FB_', as.character(vars[i]), 'm_buffer', "_DHS_15.csv")))  
} 

###################20010####################  
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE7BFL', shapefile) #read in DHS clusters 2018
#dhs <- read.files(DataDir, "*FL.shp$", 'NGGE71FL', shapefile) #read in DHS clusters 2015
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL', shapefile) #read in DHS clusters 2010

#loading raster files


# pop density extraction with just columbia data 

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  write.csv(df, file = file.path(DataIn, paste0('pop_den_u5_FB_', as.character(vars[i]), 'm_buffer', "_DHS_10.csv"))) 
  
}



