rm(list=ls())


#setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")


setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

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

dhs_list <- read.files(".*NGIR.*\\.DTA", ".*NGKR.*\\.DTA", ".*NGPR.*\\.DTA", "data/NG_2018_DHS_11072019_1720_86355")

pfpr_data <- dhs_list[[3]]

look_for(pfpr_data, "fever")

table(pfpr_data$hml32)

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_data <- pfpr_data %>% filter(hv042 == "1" & hv103 == "1" & hml32 %in% c(0, 1))
dim(pfpr_data)

#read in NG cluster data 

NGAshpfiles <- readOGR('data/NG_2018_DHS_11072019_1720_86355/NGGE7AFL', layer ="NGGE7AFL", use_iconv=TRUE, encoding= "UTF-8")

#read in admin 1 files 

admin1shp <- readOGR("bin/NGA_cnty_admin1", layer ="nga_polbnda_adm1_1m_salb", use_iconv=TRUE, encoding= "UTF-8")

plot(admin1shp)
plot(NGAshpfiles, add =T, col = 'red')

# map cluster points onto admin 1 and get cluster level id 
key_2018 <- over(SpatialPoints(coordinates(NGAshpfiles),proj4string = NGAshpfiles@proj4string), admin1shp)
head(key_2018)


key_2018$hv001<-NGAshpfiles@data[,"DHSCLUST"] 


pfpr_df <- left_join(pfpr_data, key_2018)
dim(pfpr_df)
summary(is.na(pfpr_df$hv001))





#estimate pfpr for rural areas at the cluster_level 

val_labels(pfpr_df$hv025)

pfpr_rural <- pfpr_df %>% filter(hv025 == 2) 
table(pfpr_rural$hv025)

pfpr_rural<- dataclean.para(pfpr_rural, hv005, hc1, hml32, 'hml32', 'p_test') 
hml32.svyd18_rural <- svydesign.fun(pfpr_rural)

#p_est_rural<-svyby(~p_test, by=~hv024, FUN=svymean, design=hml32.svyd18_rural,  na.rm=T) 

clu_18_rural <- result.clu.fun('p_test', 'hv001', design=hml32.svyd18_rural, pfpr_rural,"hv007") #microscopy 
head(clu_18_rural)

colnames(clu_18_rural)[1] <- "hv001"

# summary(clu_18_rural$p_test)
# 
#colnames(p_est_rural) <- c("hv024", "p_test_r", "se_r")
# 
geo_po <- pfpr_rural %>% dplyr::select(hv001,hv024) %>% distinct(hv001, hv024)
head(geo_po)
# 
# val_labels(pfpr_rural$hv024)
# 
pfpr_geo_po <- inner_join(clu_18_rural, geo_po)
head(pfpr_geo_po)
# 
# table(pfpr_geo_po$hv024)

# exp_r_pfpr <- split(pfpr_geo_po, pfpr_geo_po$hv024)
# summary(exp_r_pfpr[[3]]$p_test)
# head(exp_r_pfpr[[1]])
# colSums(exp_r_pfpr[[3]]==0)/nrow(exp_u_pfpr[[3]]) 





#estimate cluster SES for rural 

val_labels(pfpr_rural$hv270)

df <- recoder.wealth(pfpr_rural, quo(hv270))

table(df$hv270_new)

df<- dataclean.para(df, hv005, hc1, hv270_new, 'hv270_new', 'SES') 

hml32.svyd18_rural <- svydesign.fun(df)

rural_clu_SES <- result.clu.fun('SES', 'hv001', design=hml32.svyd18_rural, df,"hv007") #microscopy 
head(rural_clu_SES)

rural_clu_SES <- dplyr::select(rural_clu_SES, hv001 = DHSCLUST, SES)

# both pfpr and SES 
pfpr_SES <- left_join(pfpr_geo_po, rural_clu_SES)
head(pfpr_SES)

plot(pfpr_SES$SES, pfpr_SES$p_test)
cor(pfpr_SES$SES, pfpr_SES$p_test)

# estimate 

pfpr_data <- dhs_list[[3]]

look_for(pfpr_data, "fever")


#estimate pfpr for urban areas at the cluster_level 

pfpr_urban <- pfpr_df %>% filter(hv025 == 1) 
table(pfpr_urban$hv025)

pfpr_urban<- dataclean.para(pfpr_urban, hv005, hc1, hml32, 'hml32', 'p_test') 
hml32.svyd18 <- svydesign.fun(pfpr_urban)

clu_18_urban <- result.clu.fun('p_test', 'hv001', design=hml32.svyd18, pfpr_urban,"hv007") #microscopy 
head(clu_18_urban)

# p_est_urban<-svyby(~p_test, by=~hv024, FUN=svymean, design=hml32.svyd18,  na.rm=T) 
# 
# head(p_est_urban)

colnames(clu_18_urban)[1] <- "hv001"
# 
geo_po_u <- pfpr_urban %>% dplyr::select(hv001,hv024) %>% distinct(hv001, hv024)
head(geo_po_u)


pfpr_geo_po_u <- inner_join(clu_18_urban, geo_po_u) 
head(pfpr_geo_po_u)

# exp_u_pfpr <- split(pfpr_geo_po_u, pfpr_geo_po_u$hv024)
# 
# summary(exp_u_pfpr[[1]]$p_test)
# head(exp_u_pfpr[[1]])
# colSums(exp_u_pfpr[[6]]==0)/nrow(exp_u_pfpr[[6]]) 
# 
# summary(exp_u_pfpr[[2]]$p_test)
# summary(exp_u_pfpr[[3]]$p_test)
# summary(exp_u_pfpr[[4]]$p_test)
# summary(exp_u_pfpr[[5]]$p_test)
# summary(exp_u_pfpr[[6]]$p_test)
# 
# ur_pfpr <- left_join(p_est_rural, p_est_urban)
# ur_pfpr$pfprdiff <- ur_pfpr$p_test_r - ur_pfpr$p_test




#estimate cluster SES for urban

val_labels(pfpr_urban$hv270)

df <- recoder.wealth(pfpr_urban, quo(hv270))

table(df$hv270)

df<- dataclean.para(df, hv005, hc1, hv270_new, 'hv270_new', 'SES') 

hml32.svyd18_urban <- svydesign.fun(df)

urban_clu_SES <- result.clu.fun('SES', 'hv001', design=hml32.svyd18_urban, df,"hv007") #microscopy 
head(urban_clu_SES)

urban_clu_SES <- dplyr::select(urban_clu_SES, hv001 = DHSCLUST, SES)

# both pfpr and SES 
pfpr_SES <- left_join(pfpr_geo_po_u, urban_clu_SES)
head(pfpr_SES)

pfpr_SES_split <- split(pfpr_SES, pfpr_SES$hv024)

plot(pfpr_SES_split[[4]]$SES, pfpr_SES_split[[4]]$p_test)
cor(pfpr_SES_split[[4]]$SES, pfpr_SES_split[[4]]$p_test)

CIr(-0.4673912, 560, level = 0.95)
