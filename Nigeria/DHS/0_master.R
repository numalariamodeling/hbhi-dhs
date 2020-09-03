### ==========================================================================================
### HBHI modelling - Nigeria: Estimating intervention coverage by LGA 
### August 2020, IDO
### ==========================================================================================
rm(list = ls())

SAVE <- TRUE

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
  SrcDir <- file.path(NGDir, "src", "DHS")
  BinDir <- file.path(NGDir, "bin")
  ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
  SimDir <- file.path(ProjectDir, 'simulation_input')
}


## -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "generic_functions", "map_fun.R"))




## -----------------------------------------
### Case Management  
## -----------------------------------------
Variable <- "CM"
subVariable <-"State" # for CM options (LGA, State, repDS, region)
smoothing <- TRUE 
smoothing_type <- "space-time" #other option (space)
plot <- TRUE 
source(file.path(SrcDir, "case_management", "functions", "cm_functions.R"))
#source(file.path(SrcDir, "case_management", "CM_DHS_estimates.R"))
 
## -----------------------------------------
### Other files 
## -----------------------------------------

# state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)
colnames(state_sf)[4] <- "State"

# LGA shape file
LGAshp <- readOGR(file.path(DataDir,"Nigeria_LGAs_shapefile_191016"), layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
LGA_clean_names <- clean_LGA_2(file.path(DataDir,"Nigeria_LGAs_shapefile_191016"), file.path(BinDir,"names/LGA_shp_pop_names.csv"))

# cluster locations 
NGAshplist<-read.files("*FL.*\\.shp$", DataDir, shapefile)
key_list <- lapply(NGAshplist, over.fun)

# rep DS file
rep_DS <- read.csv(file.path(BinDir, "rep_DS/representative_DS_orig60clusters.csv")) %>% dplyr::select(-X)

