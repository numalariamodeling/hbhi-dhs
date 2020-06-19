# imports and prepares DHS, LGA, state admin and population density files, 
# and maps DHS cluster points onto state admin files

# download SDSM packge from github

# install.packages("devtools")  
# devtools::install_github("statnmap/SDMSelect")

rm(list=ls())


#setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")


setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

#important to download the github version of tidyverse.Uncomment the script below to run
# install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("hadley/tidyverse")
## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "maptools", "spatstat", "SDMSelect", "INLA")

lapply(x, library, character.only = TRUE) #applying the library function to packages



# update.packages(oldPkgs = x)

options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

# require(pacman)
# p_unlock()

source("src/Nigeria functions.R")

dhs_list <- read.files(".*NGIR.*\\.DTA", ".*NGKR.*\\.DTA", ".*NGPR.*\\.DTA", "data/NG_2018_DHS_11072019_1720_86355")


#read in NG cluster data 

NGAshpfiles <- readOGR('data/NG_2018_DHS_11072019_1720_86355/NGGE7AFL', layer ="NGGE7AFL", use_iconv=TRUE, encoding= "UTF-8")

#read population data and create density file 


LGAshp <- readOGR("data/Nigeria_LGAs_shapefile_191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
head(LGAshp)


LGA_sf <- st_as_sf(LGAshp)
head(LGA_sf)


# read LGA population density and turn to raster for 2020 

pop_name <- "data/NGA_pop/gpw-v4-population-density-rev11_2020_30_sec_tif/gpw_v4_population_density_rev11_2020_30_sec.tif"
pop_raster=raster(pop_name)
pop_crop <- crop(pop_raster, LGAshp)
summary(pop_crop)
plot(pop_crop)
hist(pop_crop)

img <- as.im(pop_crop) #converts to im object 
img[img == 0] <- NA
pop.lg <- log(img) #convert pop density data to log 
summary(pop.lg, main=NULL, las=1)

plot(pop.lg)
hist(pop.lg)
 
NGA_pop <- read.csv("bin/nigeria_pop_density.csv") %>%  mutate(log_den = log(UN_2020_DS))
hist(NGA_pop$UN_2020_DS) #population density data is skewed 
hist(NGA_pop$log_den)
  
LGA_pop_den <- left_join(LGA_sf, NGA_pop,  by = "LGA")
# head(LGA_pop_den)
# 
# # LGAshp <- as_Spatial(LGA_pop_den)
# # head(LGAshp)
# 
# template <- raster(ext = extent(LGA_pop_den), crs = projection(LGA_pop_den))
# pop_rst <- rasterize(LGA_pop_den, template, field = "UN_2020_DS")
# plot(pop_rst)
# 
# img <- as.im(pop_rst) #converts to im object 
# 
# pop.lg <- log(img) #convert pop density data to log 
# summary(pop.lg, main=NULL, las=1)
# plot(pop.lg)

#read in LGA area files 

# str <- "data/NGA_pop/gpw-v4-admin-unit-center-points-population-estimates-rev11_nga_csv/gpw_v4_admin_unit_center_points_population_estimates_rev11_nga.csv"
# LGA_area <- read.csv(str) %>% dplyr::select(NAME1, NAME2, TOTAL_A_KM) %>% mutate(NAME2 = str_to_title(NAME2),
#                                                                                  NAME1 = str_to_title(NAME1))
# colnames(LGA_area)<- c("State", "LGA", "area_sq_km")
# head(LGA_area)
# write.csv(LGA_area, "bin/LGA_area.csv")
LGA_area<- read.csv("bin/LGA_area.csv")


LGA_sf_2 <- left_join(LGA_sf, LGA_area, by="LGA") %>%  mutate(area_sq_km = ifelse(LGA == "Wudil", 336.3673617, 
                                                                                  ifelse(LGA == "Ibadan North", 17.76379982,
                                                                                         area_sq_km)))

LGA_sf_2 <- as(LGA_sf_2, "Spatial")

ad <- as(LGA_sf_2, "owin") #converts admin 1 to owin object 

plot(LGA_sf_2)
plot(NGAshpfiles, add =T, col = 'red')

# map cluster points onto admin 2 and get cluster level id 
library(rgeos)
key_2018 <- over(SpatialPoints(coordinates(NGAshpfiles),proj4string = NGAshpfiles@proj4string), LGA_sf_2)
head(key_2018)
key_2018$hv001<-NGAshpfiles@data[,"DHSCLUST"]


#read in the nighttime lights for January 2020 

lgt_name<-'bin/night_lights/SVDNB_npp_20200101-20200131_75N060W_vcmcfg_v10_c202002111500.avg_rade9h.tif' 
lgt_raster=raster(lgt_name)
plot(lgt_raster)


# we want to generate the average night time lights by LGA

# crop the raster using the vector extent
lgt_crop <- crop(lgt_raster, LGAshp)
plot(lgt_crop, main = "Cropped lights extent")
# lgt_crop <- crop(lgt_crop, pop_crop)
# lgt_crop_res <- projectRaster(lgt_crop, pop_crop)


# This makes a table of mean night time lights by LGA 
# lgt_Extent<-extent(lgt_crop) #get extent

lgt_Ext<-raster::extract(lgt_crop,LGAshp, df = TRUE,fun = mean, na.rm = TRUE, cellnumbers=TRUE)  #extract avg night light value by LGA

colnames(lgt_Ext)[2]<- "avg_night_lights"

lgt_Ext$LGA<-LGAshp$LGA #get LGA ID 


# make map of LGA aggregated lights 

LGA_lgt_plot <- left_join(LGA_sf, lgt_Ext) %>% 
                        mutate(avg_night_lights = ifelse(avg_night_lights<=0, 0, avg_night_lights))# stop here and go to next section just for maps. continue if you want rasterized map
summary(LGA_lgt_plot$avg_night_lights)

NGA_pop <- dplyr::select(NGA_pop, LGA, UN_2020_DS, log_den)

LGA_lgt_plot_2 <- left_join(LGA_lgt_plot, NGA_pop)


template <- raster(ext = extent(LGA_lgt_plot), crs = projection(LGA_lgt_plot))
lgt_rst <- rasterize(LGA_lgt_plot, template, field = "avg_night_lights")
plot(lgt_rst)
hist(lgt_rst)

#remove negative values 
lgt_rst [lgt_rst < 0] <- NA

img <- as.im(lgt_rst) #converts to im object 

lgt.lg <- log(img) #convert night time lights data to log 
lgt.lg [lgt.lg  == 0] <- NA
summary(lgt.lg, main=NULL, las=1)
plot(lgt.lg)
hist(lgt.lg)


# we need to convert negative light values to 0 

lgt_ex_2 <- lgt_Ext %>%  mutate(avg_night_lights = ifelse(avg_night_lights < 0, 0, avg_night_lights),
                                log_lght = log(avg_night_lights))

hist(lgt_ex_2$log_lght)


# do the same for shapefile

plot <- LGA_lgt_plot %>%  mutate(avg_night_lights = ifelse(avg_night_lights < 0, 0, avg_night_lights))
class(plot$avg_night_lights)

LGA_lgt<- tm_shape(LGA_lgt_plot) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "avg_night_lights", textNA = "No data", 
              title = "Annual Average radiance composite images by LGA (2016) ", palette = "seq", breaks=c(0, 0.04, 0.1, 
                                                               0.9, 2, 6, 10, 15, 20, 25))+
  tm_layout(title = "", aes.palette = list(seq="RdYlBu"))

tmap_save(tm = LGA_lgt, filename = "results/malaria_DHS_paper/nightlights/LGA_lghts_aggregate_2016_annual.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)





# # estimate cluter_level pfpr in rural areas 
# 
# var_label(pfpr_rural$hc1)
# 
# 
# pfpr_rural <- pfpr_df %>% filter(hv025 == 2)
# pfpr_rural<- dataclean.para(pfpr_rural, hv005, hc1, hml32, 'hml32', 'p_test') 
# hml32.svyd18_rural <- svydesign.fun(pfpr_rural)
# 
# #p_est_rural<-svyby(~p_test, by=~hv024, FUN=svymean, design=hml32.svyd18_rural,  na.rm=T) 
# 
# clu_18_rural <- result.clu.fun('p_test', 'hv001', design=hml32.svyd18_rural, pfpr_rural,"hv007") #microscopy 
# head(clu_18_rural)
# 
# colnames(clu_18_rural)[1] <- "hv001"
# 
# # summary(clu_18_rural$p_test)
# # 
# #colnames(p_est_rural) <- c("hv024", "p_test_r", "se_r")
# # 
# geo_po <- pfpr_rural %>% dplyr::select(hv001,hv024) %>% distinct(hv001, hv024)
# head(geo_po)
# # 
# # val_labels(pfpr_rural$hv024)
# # 
# pfpr_geo_po <- inner_join(clu_18_rural, geo_po)
# head(pfpr_geo_po)
# # 
# # table(pfpr_geo_po$hv024)
# 
# # exp_r_pfpr <- split(pfpr_geo_po, pfpr_geo_po$hv024)
# # summary(exp_r_pfpr[[3]]$p_test)
# # head(exp_r_pfpr[[1]])
# # colSums(exp_r_pfpr[[3]]==0)/nrow(exp_u_pfpr[[3]]) 
# 
# 
# 
# 
# 
# #estimate cluster SES for rural 
# 
# val_labels(pfpr_rural$hv270)
# 
# df <- recoder.wealth(pfpr_rural, quo(hv270))
# 
# table(df$hv270_new)
# 
# df<- dataclean.para(df, hv005, hc1, hv270_new, 'hv270_new', 'SES') 
# 
# hml32.svyd18_rural <- svydesign.fun(df)
# 
# rural_clu_SES <- result.clu.fun('SES', 'hv001', design=hml32.svyd18_rural, df,"hv007") #microscopy 
# head(rural_clu_SES)
# 
# rural_clu_SES <- dplyr::select(rural_clu_SES, hv001 = DHSCLUST, SES)
# 
# # both pfpr and SES 
# pfpr_SES <- left_join(pfpr_geo_po, rural_clu_SES)
# head(pfpr_SES)
# 
# pfpr_SES_split <- split(pfpr_SES, pfpr_SES$hv024)
# 
# plot(pfpr_SES$SES, pfpr_SES$p_test)
# cor(pfpr_SES_split[[6]]$SES, pfpr_SES_split[[6]]$p_test)
# 
# 
# 
# 
# # estimate 
# 
# fever_data <- dhs_list[[2]]
# 
# look_for(fever_data, "fever")
# 
# table(fever_data$h22)
# 
# df <- recoder.gen(fever_data, quo(h22))
# table(df$h22_new)
# 
# df<- dataclean(df, h22_new, 'v005', 'h22_new', 'fever') 
# 
# hml32.svyd18_rural <- svydesign.fun(df)
# 
# rural_clu_fever <- result.clu.fun('fever', 'v001', design=hml32.svyd18_rural, df,"v007") #microscopy 
# head(rural_clu_fever)
# 
# 
# 
# 
# #estimate pfpr for urban areas at the cluster_level 
# 
# pfpr_urban <- pfpr_df %>% filter(hv025 == 1) 
# table(pfpr_urban$hv025)
# 
# pfpr_urban<- dataclean.para(pfpr_urban, hv005, hc1, hml32, 'hml32', 'p_test') 
# hml32.svyd18 <- svydesign.fun(pfpr_urban)
# 
# clu_18_urban <- result.clu.fun('p_test', 'hv001', design=hml32.svyd18, pfpr_urban,"hv007") #microscopy 
# head(clu_18_urban)
# 
# # p_est_urban<-svyby(~p_test, by=~hv024, FUN=svymean, design=hml32.svyd18,  na.rm=T) 
# # 
# # head(p_est_urban)
# 
# colnames(clu_18_urban)[1] <- "hv001"
# # 
# geo_po_u <- pfpr_urban %>% dplyr::select(hv001,hv024) %>% distinct(hv001, hv024)
# head(geo_po_u)
# 
# 
# pfpr_geo_po_u <- inner_join(clu_18_urban, geo_po_u) 
# head(pfpr_geo_po_u)
# 
# # exp_u_pfpr <- split(pfpr_geo_po_u, pfpr_geo_po_u$hv024)
# # 
# # summary(exp_u_pfpr[[1]]$p_test)
# # head(exp_u_pfpr[[1]])
# # colSums(exp_u_pfpr[[6]]==0)/nrow(exp_u_pfpr[[6]]) 
# # 
# # summary(exp_u_pfpr[[2]]$p_test)
# # summary(exp_u_pfpr[[3]]$p_test)
# # summary(exp_u_pfpr[[4]]$p_test)
# # summary(exp_u_pfpr[[5]]$p_test)
# # summary(exp_u_pfpr[[6]]$p_test)
# # 
# # ur_pfpr <- left_join(p_est_rural, p_est_urban)
# # ur_pfpr$pfprdiff <- ur_pfpr$p_test_r - ur_pfpr$p_test
# 
# 
# 
# 
# #estimate cluster SES for urban
# 
# val_labels(pfpr_urban$hv270)
# 
# df <- recoder.wealth(pfpr_urban, quo(hv270))
# 
# table(df$hv270)
# 
# df<- dataclean.para(df, hv005, hc1, hv270_new, 'hv270_new', 'SES') 
# 
# hml32.svyd18_urban <- svydesign.fun(df)
# 
# urban_clu_SES <- result.clu.fun('SES', 'hv001', design=hml32.svyd18_urban, df,"hv007") #microscopy 
# head(urban_clu_SES)
# 
# urban_clu_SES <- dplyr::select(urban_clu_SES, hv001 = DHSCLUST, SES)
# 
# # both pfpr and SES 
# pfpr_SES <- left_join(pfpr_geo_po_u, urban_clu_SES)
# head(pfpr_SES)
# 
# pfpr_SES_split <- split(pfpr_SES, pfpr_SES$hv024)
# 
# plot(pfpr_SES_split[[4]]$SES, pfpr_SES_split[[4]]$p_test)
# cor(pfpr_SES_split[[4]]$SES, pfpr_SES_split[[4]]$p_test)
# 
# library(psychometric)
# 
# CIr(-0.4673912, 560, level = 0.95)
