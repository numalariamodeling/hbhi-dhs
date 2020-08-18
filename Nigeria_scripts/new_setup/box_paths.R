### ==========================================================================================
### HBHI modelling - Nigeria: Estimating intervention coverage by LGA 
### August 2020, IDO
### ==========================================================================================


SAVE <- TRUE

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("mambrose" %in% user) {
  user_path <- file.path("C:/Users", user)
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "NU-malaria-team")
  NGDir <-file.path(NuDir, "data/nigeria_dhs/data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  SrcDir <- file.path(NGDir, "src")
  BinDir <- file.path(NGDir, "bin")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Box/NU-malaria-team")
  NGDir <-file.path(NuDir, "data/nigeria_dhs/data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  SrcDir <- file.path(NGDir, "src")
  BinDir <- file.path(NGDir, "bin")
}


## -----------------------------------------
### Required custom functions and settings
## -----------------------------------------
source(file.path(SrcDir, "Nigeria functions.R"))


## -----------------------------------------
### Other files 
## -----------------------------------------

# LGA shape file 
LGAshp <- readOGR(file.path(DataDir,"Nigeria_LGAs_shapefile_191016"), layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")

# cluster locations 
NGAshplist  <-read.files("*FL.*\\.shp$", DataDir, shapefile)
key_list <- map(NGAshplist, over.fun)

# rep DS file
rep_DS <- read.csv(file.path(BinDir, "rep_DS/representative_DS_orig60clusters.csv")) %>% dplyr::select(-X)


## -----------------------------------------
### ITN 
## -----------------------------------------
Variable <- "ITN"
subVariable <-"U5" #options U5, 6-9, 10-18, > 18 
year <- ""
maps <- TRUE
source()
