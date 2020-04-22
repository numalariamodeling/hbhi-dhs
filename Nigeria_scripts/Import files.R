############################################################################################################
# ---- cleaning the global environment, setting the working environment and loading libraries --- #
############################################################################################################
# installr::updateR() #windows only


rm(list=ls())


setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")


#setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

#important to download the github version of tidyverse.Uncomment the script below to run
# install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("hadley/tidyverse")
## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape")

lapply(x, library, character.only = TRUE) #applying the library function to packages
      
# update.packages(oldPkgs = x)

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

# require(pacman)
# p_unlock()

source("src/Nigeria functions.R")



##############################################################################################
# --- reading and cleaning the datasets --- #
#############################################################################################
# DHS datasets 
NGAfiles<-read.files(".*NGIR.*\\.DTA", ".*NGKR.*\\.DTA", ".*NGPR.*\\.DTA")

#table(NGAfiles[[19]]$m49a_1)

#2003, 2008, 2010, 2013, 2015, 2018 

# NGAfiles_2<-read.files(".*NGIR.*\\.DTA", ".*NGKR.*\\.DTA", ".*NGHR.*\\.DTA") # reads in a different combination of files to be used for IRS analysis 


# cluster locations 
NGAshpfiles <- list.files(pattern="*FL.*\\.shp$", full.names=TRUE, recursive=T)

NGAshplist <- sapply(NGAshpfiles,shapefile, simplify = F)

NGAshplist_sf <- map(NGAshplist, st_as_sf)

# LGA shape file 

LGAshp <- readOGR("data/Nigeria_LGAs_shapefile_191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")



LGAshp_sf <- st_as_sf(LGAshp)

LGAshp_sf$LGA <- gsub("\\/", "-", LGAshp_sf$LGA)

LGA_cov <-  LGAshp_sf %>% mutate(LGA = ifelse(LGA == "kaita","Kaita", ifelse(LGA == "kiyawa", "Kiyawa", as.character(LGA))))


# LGA_Ad <- LGAshp_sf %>%  filter(State == "Adamawa")
# 
# 
# st_write(LGA_Ad, "Adamawa_LGA.csv")

head(LGAshp_sf)


#admin 1 shapefile 

admin1shp <- readOGR("bin/NGA_cnty_admin1", layer ="nga_polbnda_adm1_1m_salb", use_iconv=TRUE, encoding= "UTF-8")
admin1_sf <- st_as_sf(admin1shp)

# mics 2011 admin file 
#admin1shp_sf_ <- st_as_sf(admin1shp)%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Federal Capital Territory" = "FCT (Abuja)",
                                                                         #"Akwa lbom" = "Akwa ibom" ))%>% rename(State = ADM1_NAME)
# mics 2016 - 17 
#admin1shp_sf <- st_as_sf(admin1shp) %>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Federal Capital Territory" = "FCT Abuja",
                             #"Akwa lbom" = "Akwa Ibom" ))%>% rename(State = ADM1_NAME)

#head(admin1shp_sf)


#checking to see if cluster points into admin boundary 

# raw.plot.fun(LGAshp, NGAshplist[[1]], "1990 Admin boundary")
# raw.plot.fun(LGAshp, NGAshplist[[2]], "2003 Admin boundary")
# raw.plot.fun(LGAshp, NGAshplist[[3]], "2008 Admin boundary")
# raw.plot.fun(LGAshp, NGAshplist[[4]], "2010 Admin boundary")
# raw.plot.fun(LGAshp, NGAshplist[[5]], "2013 Admin boundary")
# raw.plot.fun(LGAshp, NGAshplist[[6]], "2015 Admin boundary")
# raw.plot.fun(LGAshp, NGAshplist[[7]], "2018 Admin boundary")

# code to read in representative archetypes

rep_DS <- read.csv("bin/rep_DS/representative_DS_orig60clusters.csv") %>% dplyr::select(-X)
head(rep_DS)


# rep_LGA <- read.csv("bin/rep_LGA.csv")
# rep_LGA_state <- rep_LGA %>% left_join(LGAshp_sf) %>% dplyr::select(repDS=LGA, State)
# head(rep_LGA_state)
# st_write(rep_LGA_state, "results/rep_DS/rep_LGA_state_info.csv")
# 
# admin1_map <- tmap.fun5(admin1shp_sf, "States", "ADM1_NAME")

#map
# rep_DS_LGA <- LGAshp_sf %>% left_join(rep_DS)
# head(rep_DS_LGA)

# rep_map <- tmap.fun4(rep_DS_LGA, "Representative DS", "color_bar", "repDS", col_vector)
# 
# color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
# 
# 
# library(RColorBrewer)
# n <- 22
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
# 
# 
# color_22 <- color[9:20]
# 
# library(randomcoloR)
# n <- 22
# palette <- distinctColorPalette(n)
# 
# display.brewer.all()
# 
# colors_n <- palette(rainbow(22))
# 
# 
# install.packages("viridis")
# library(viridis)
# new_col <- viridis_pal(option = "D")(22)
# 

# code to search through labels for specific names 

# look_for(NGAfiles[[19]], "antenatal")

# check <- NGAfiles[[18]] %>% dplyr::select(sh13, hv105, sh225, sh14, sh15)
# 
# tail(check)

# table(NGAfiles[[18]][, "sh225"])
# 
# table(NGAfiles[[17]][, "ml13e"])



# MICS datasets
# ch_files<-read.files2("*ch.sav","*hh.sav", "*wm.sav")




#child dataset 
# MICS_ch_NG <- read_sav("data/MICS/2007 MICS/Nigeria MICS 2007 SPSS Datasets/hh.sav")
# look_for(MICS_ch_NG)
# # val_labels(MICS_ch_NG)
# # str(MICS_ch_NGA$chweightkano)
# # str(MICS_ch_NG$PSU)
# 
# #subset 
# #case_df <- MICS_ch_NG %>% dplyr::select(HH1, HH2, UF8D:UF8Y, AG1D:AG1Y, AG2, CA6AA, CA10:CA13QQ, CA13DD, HH6,
                                        #HH7,HL4, chweight, chweightkano, chweightlagos)

#HH dataset 
# MICS_HH_NGA <- read_sav("data/MICS/Nigeria MICS5 Datasets/Nigeria MICS 2016-17 SPSS Datasets/hh.sav")
# # look_for(MICS_HH_NGA)
# # val_labels(MICS_HH_NGA)
# # table(MICS_HH_NGA$stratum)

# #subset 
# HH_df <- MICS_HH_NGA %>% dplyr::select(HH1,HH2,stratum, PSU)
# 
# # all MICS 
# all_MICS <- case_df %>% left_join(HH_df)
# head(all_MICS)



##################################################################################################
#  --- Mapping points to inherit associated health district  --- #
##################################################################################################


key_list <- map(NGAshplist, over.fun)

key_list[[1]]$v001<-NGAshplist[[1]]@data[,"DHSCLUST"] #1993 
key_list[[2]]$v001<-NGAshplist[[2]]@data[,"DHSCLUST"] #2003
key_list[[3]]$v001<-NGAshplist[[3]]@data[,"DHSCLUST"] #2008 
key_list[[4]]$v001<-NGAshplist[[4]]@data[,"DHSCLUST"] #2010 
key_list[[5]]$v001<-NGAshplist[[5]]@data[,"DHSCLUST"] #2013 
key_list[[6]]$v001<-NGAshplist[[6]]@data[,"DHSCLUST"] #2015 
key_list[[7]]$v001<-NGAshplist[[7]]@data[,"DHSCLUST"] #2018 


