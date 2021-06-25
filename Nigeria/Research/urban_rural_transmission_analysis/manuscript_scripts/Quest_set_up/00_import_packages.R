#Input the following commands in Terminal if its the first time running this script on quest
#module purge all
#module load R/3.5.1
#module load gdal/2.4.1
#module load proj
#module load geo

old_path <- Sys.getenv("PATH")
Sys.setenv(PATH = paste(old_path, "/projects/b1139/software/ImageMagick/7.1.0/bin", sep = ":"))
old_path <- Sys.getenv("PKG_CONFIG_PATH")
Sys.setenv(PKG_CONFIG_PATH = paste(old_path, "/projects/b1139/software/ImageMagick/7.1.0/lib/pkgconfig", sep = ":"))
old_path <- Sys.getenv("LD_LIBRARY_PATH")
Sys.setenv(LD_LIBRARY_PATH = paste(old_path, "/projects/b1139/software/ImageMagick/7.1.0/lib", sep = ":"))
install.packages("magick")


#Make sure we have the right packages
options(repos="http://cran.wustl.edu")
list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
                      "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
                      "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages



# set document path to current script path 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

