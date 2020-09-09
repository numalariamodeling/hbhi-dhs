### ==========================================================================================
### HBHI modelling - Nigeria: Estimating intervention coverage by LGA 
### August 2020, IDO
### ==========================================================================================
rm(list = ls())


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
source(file.path(SrcDir, "generic_functions", "DHS_fun.R"))

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



# rep DS file
rep_DS <- read.csv(file.path(BinDir, "rep_DS/representative_DS_orig60clusters.csv")) %>% dplyr::select(-X)




## -----------------------------------------
### Variables  
## -----------------------------------------
var_df <- read.csv(file.path(SrcDir, "analysis_variables_requirements.csv"))
for (i in 1:nrow(var_df)){
      if (var_df[, "variable_to_run"][i] == TRUE){
      Variable <-  var_df[, "Variable"][i]
      subVariable <- var_df[, "subVariable"][i]
      smoothing <- var_df[, "smoothing"][i]
      smoothing_type <- var_df[, "smoothing_type"][i]
      plot <- var_df[, "plot"][i]
      SAVE <- var_df[, "SAVE"][i]
      
      # cluster locations 
      NGAshplist<-read.files("*FL.*\\.shp$", DataDir, shapefile)
      key_list <- lapply(NGAshplist, over.fun)
      
      source(file.path(SrcDir, var_df[, "fun_path"][i]))
      source(file.path(SrcDir, var_df[, "main_path"][i]))
      }
}

# check why there are holes in state map 

