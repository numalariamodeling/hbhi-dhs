#############################################################################################################
## cleaning the global environment, setting the working environment, loading libraries and custoom functions 
#############################################################################################################

rm(list=ls())

#setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs/data analysis") 

setwd("~/Box/NU-malaria-team/data/burkina_dhs/data analysis") 


#setwd("~/Box/NU-malaria-team/data/burkina_dhs/data analysis") 
#important to download the github version of tidyverse.Uncomment the script below to run
# install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("hadley/tidyverse")


# Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer", "plotrix", "ggrepel", "sf", "shinyjs", "tmap", "knitr", "purrr", "labelled")
lapply(x, library, character.only = TRUE) #applying the library function to packages
options(survey.lonely.psu="certainty") # this option allows admin units with only one cluster to be analyzed

#reads in custom functions 
source("src/BF DHS functions.R")


##############################################################################################
# --- reading and cleaning the datasets --- #
#############################################################################################

# DHS datasets 

BFfiles<-read.files(".*BFIR.*\\.DTA", ".*BFKR.*\\.DTA", ".*BFPR.*\\.DTA")


# cluster locations 
BFshpfiles <- list.files(pattern="*FL.*\\.shp$", full.names=TRUE, recursive=T)

BFshplist <- sapply(BFshpfiles,shapefile, simplify = F)

BFshplist_sf <- map(BFshplist, st_as_sf)

# DS file
DS_shape<- readOGR("data/burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

DS_shape <- spTransform(DS_shape,crs(BFshplist[[6]]))

DS_shape_sf <- st_as_sf(DS_shape)

#checking to see if cluster points into admin boundary 

# raw.plot.fun(DS_shape, BFshplist[[1]], "1993 Admin boundary")
# raw.plot.fun(DS_shape, BFshplist[[2]], "1998-99 Admin boundary")
# raw.plot.fun(DS_shape, BFshplist[[3]], "2003 Admin boundary")
# raw.plot.fun(DS_shape, BFshplist[[4]], "2010 Admin boundary")
# raw.plot.fun(DS_shape, BFshplist[[5]], "2014 Admin boundary")
# raw.plot.fun(DS_shape, BFshplist[[6]], "2017-18 Admin boundary")


# code to search through labels for specific names 

#look_for(BFfiles[[1]], "fansidar")


##################################################################################################
#  --- Mapping points to inherit associated health district  --- #
##################################################################################################


key_list <- map(BFshplist, over.fun)

key_list[[1]]$v001<-BFshplist[[1]]@data[,"DHSCLUST"] #1993 
key_list[[2]]$v001<-BFshplist[[2]]@data[,"DHSCLUST"] #1998-99
key_list[[3]]$v001<-BFshplist[[3]]@data[,"DHSCLUST"] #2003 
key_list[[4]]$v001<-BFshplist[[4]]@data[,"DHSCLUST"] #2010 
key_list[[5]]$v001<-BFshplist[[5]]@data[,"DHSCLUST"] #2014 
key_list[[6]]$v001<-BFshplist[[6]]@data[,"DHSCLUST"] #2017-18 


