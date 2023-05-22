rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ido0493" %in% user) {
  user_path <- file.path("C:/Users/", user)
  DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  dataDir <- file.path(DriveDir, "data")
  DHSdata <- file.path(dataDir, "DHS", "Downloads")
  shapefileDir <- file.path(dataDir, "nigeria", "shapefiles")
  DataIn <- file.path(DriveDir, "projects", "mathematical_model", "itn_model", "estimates_ITN_CM", "CM")
} else if  ("Chilo Chiziba" %in% user) {
  user_path <- file.path("C:/Users/", user)
  DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  dataDir <- file.path(DriveDir, "data")
  shapefileDir <- file.path(dataDir, "nigeria", "shapefiles")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, "OneDrive - Northwestern University", "urban_malaria")
  dataDir <- file.path(DriveDir, "data")
  shapefileDir <- file.path(dataDir, "nigeria", "shapefiles")
  DHSdata <- file.path(dataDir, "DHS", "Downloads", "NG_2021_MIS_12052022_1735_141460")
  RasterDir <- file.path(dataDir, "nigeria", "Raster_files", "rainfall_monthly", "2018")
  results <- file.path(DriveDir, "projects", "Malaria_prevalence", "Malaria_prevalence_Nigeria_Dec-2022", "outputs", "new_results")
}


## -----------------------------------------
### Required functions and settings
## -----------------------------------------
library(plyr)
library(tidyverse)
library(ggplot2)
library(haven)
library(labelled)
library(labelled)
library(labeling)
library(sf)
library(arsenal)
library(srvyr)
library(survey)
library(janitor)
library(ggrepel)
library(usmap)
library(ggspatial)
library(rgdal)
library(tmap)
library(ggpubr)
library(dplyr)
library(ggthemes)
library(patchwork)
library(raster)



theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed

## -----------------------------------------
### data analysis 
## -----------------------------------------


DHSKRdata <- read_dta(file.path(DHSdata, "NG_2013_DHS_06192019","NGKR6ADT", "NGKR6AFL.DTA")) 



#ACT_2013
data_KR4<- DHSKRdata  %>% mutate(wt=v005/1000000,strat=v022, id = v021)%>%
 dplyr::filter(b5 ==1 & h22 == 1 & ml13a ==1 |ml13b ==1 |
                          ml13c ==1 | ml13d ==1 | ml13e ==1 |ml13f==1| ml13g ==1 | ml13h == 1) %>%
  mutate(ACTs = ifelse(ml13e > 1, NA,  ml13e)) %>%
  drop_na(ACTs)


ACT_prop_13 <- data_KR4 %>% as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(v101) %>% summarize(ACT_prop = round(survey_mean(ACTs),2), 
                                total_number_ACT = survey_total()) %>%
  mutate(ACT_cut  = cut(ACT_prop *100,
                        breaks = c(0, 30, 40.00,60.00,80.00, 100),
                        labels = c("<30", "31-40", "41-60", "61-80", "80+"),
                        include.lowest = T), year = 2013)






#ACT_2015
DHSKRdata <- read_dta(file.path(DHSdata, "NG_2015_MIS_06192019","NGKR71DT", "NGKR71FL.DTA")) 


data_KR4<- DHSKRdata  %>% mutate(wt=v005/1000000,strat=v022, id = v021)%>%
  dplyr::filter(b5 ==1 & h22 == 1 & ml13a ==1 |ml13b ==1 |
                  ml13c ==1 | ml13d ==1 | ml13e ==1 |ml13f==1| ml13g ==1 | ml13h == 1) %>%
  mutate(ACTs = ifelse(ml13e > 1, NA,  ml13e)) %>%
  drop_na(ACTs)


ACT_prop_15 <- data_KR4 %>% as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(v101) %>% summarize(ACT_prop = round(survey_mean(ACTs),2), 
                               total_number_ACT = survey_total()) %>%
  mutate(ACT_cut  = cut(ACT_prop *100,
                        breaks = c(0, 30, 40.00,60.00,80.00, 100),
                        labels = c("<30", "31-40", "41-60", "61-80", "80+"),
                        include.lowest = T), year = 2015)



#ACT_2018
DHSKRdata <- read_dta(file.path(DHSdata, "NG_2018_DHS_11072019_1720_86355","NGKR7ADT", "NGKR7AFL.DTA")) 


data_KR4<- DHSKRdata  %>% mutate(wt=v005/1000000,strat=v022, id = v021)%>%
  dplyr::filter(b5 ==1 & h22 == 1 & ml13a ==1 |ml13b ==1 |
                  ml13c ==1 | ml13d ==1 | ml13e ==1 |ml13f==1| ml13g ==1 | ml13h == 1) %>%
  mutate(ACTs = ifelse(ml13e > 1, NA,  ml13e)) %>%
  drop_na(ACTs)


ACT_prop_18 <- data_KR4 %>% as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(v101) %>% summarize(ACT_prop = round(survey_mean(ACTs),2), 
                               total_number_ACT = survey_total()) %>%
  mutate(ACT_cut  = cut(ACT_prop *100,
                        breaks = c(0, 30, 40.00,60.00,80.00, 100),
                        labels = c("<30", "31-40", "41-60", "61-80", "80+"),
                        include.lowest = T), year = 2018)




#ACT_2018
DHSKRdata <- read_dta(file.path(DHSdata, "NG_2021_MIS_12052022_1735_141460","NGKR81FL", "NGKR81FL.DTA")) 


data_KR4<- DHSKRdata  %>% mutate(wt=v005/1000000,strat=v022, id = v021)%>%
  dplyr::filter(b5 ==1 & h22 == 1 & ml13a ==1 |ml13b ==1 |
                  ml13c ==1 | ml13d ==1 | ml13da==1| ml13e ==1|ml13aa==1|ml13ab==1 |ml13f==1| ml13h == 1) %>%
  mutate(ACTs = ifelse(ml13e > 1, NA,  ml13e)) %>%
  drop_na(ACTs)


ACT_prop_21 <- data_KR4 %>% as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(sregion) %>% summarize(ACT_prop = round(survey_mean(ACTs),2), 
                               total_number_ACT = survey_total()) %>%
  mutate(ACT_cut  = cut(ACT_prop *100,
                        breaks = c(0, 30, 40.00,60.00,80.00, 100),
                        labels = c("<30", "31-40", "41-60", "61-80", "80+"),
                        include.lowest = T), year = 2021) %>%  
  rename(v101 = sregion)

ACT_all <- rbind(ACT_prop_13, ACT_prop_15, ACT_prop_18, ACT_prop_21)

write_csv(ACT_all, file.path(DataIn, "CM_estimates_region_all_DHS.csv"))
