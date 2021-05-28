rm(list=ls())

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mudata2", "mapview")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data")

# reads in functions so we can alias it using funenv$
funEnv <- new.env()


sys.source(file = file.path("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

##################loading data###########

#Loading shapefiles

dhs18_sf <- st_read("DHS18GPS/NGGE7AFL.shp")
clust18 <- dhs18_sf [,c("DHSCLUST")]

mis15_sf <- st_read("MIS15GPS/NGGE71FL.shp")
clust15 <- mis15_sf [,c("DHSCLUST")]


mis10_sf <- st_read("MIS10GPS/NGGE61FL.shp")
clust10 <- mis10_sf [,c("DHSCLUST")]


lga <- st_read("Nigeria_LGAs_shapefile_191016/NGA_LGAs.SHP")
lga_state <- lga[,c("State")]


#uisng over fxn to extract state naames

 
library(sp)
library(rgdal)

cords18 <- dhs18_sf[,c("LATNUM", "LONGNUM")]
cords18 <- as.data.frame(cords18)
#and into Spatial
points <- SpatialPoints(cords18)
#SpatialPolygonDataFrame - I'm using a shapefile of UK counties
states <- readOGR(".", "Nigeria_LGAs_shapefile_191016/NGA_LGAs.SHP")
s <- shapefile("Nigeria_LGAs_shapefile_191016/NGA_LGAs.SHP")
#assume same proj as shapefile!
proj4string(points) <- proj4string(states)
#get county polygon point is in
result <- as.character(over(points, states)$State)

