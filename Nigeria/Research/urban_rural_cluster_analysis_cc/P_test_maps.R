rm(list=ls())

x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "Sp", "sf", "tmap")



lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)

###################################################################################
####Directories
###################################################################################

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src", "DHS")
VarDir <- file.path(SrcDir, "1_variables_scripts")
ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")


#setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data")



###################################################################################
####Loading data
###################################################################################
# Load pre-clustered data:
clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), 
                               header = T, sep = ',')

#Loading cluster points

dhs18_sf <- st_read(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),)
mis15_sf <- st_read(file.path(DataDir,"NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),)
mis10_sf <- st_read(file.path(DataDir,"NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),)

# join dhs variables to cluster points by year 

df_18 <- clu_variales_10_18 %>% filter(data_source == "dhs2018") 
df_18_fin <- left_join(dhs18_sf, df_18, by = c("DHSCLUST" ="hv001"))
df_15 <- clu_variales_10_18 %>% filter(data_source == "mis2015") 
df_15_fin <- left_join(mis15_sf, df_15, by = c("DHSCLUST" ="hv001"))
df_10 <- clu_variales_10_18 %>% filter(data_source == "mis2010") 
df_10_fin <- left_join(mis10_sf, df_10, by = c("DHSCLUST" = "hv001"))
clu_variales_10_18 <- rbind(df_18_fin, df_15_fin, df_10_fin)


# Binarize response:
clu_variales_10_18$y <- ifelse(clu_variales_10_18$p_test < 0.1, 0,1)

#cleaning precip data
clu_variales_10_18 <- clu_variales_10_18%>% mutate(annual_precipitation = scale(clu_variales_10_18$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

clu_variales_10_18 <- clu_variales_10_18 %>% mutate(pop_den = na_if(pop_den, -9999))
clu_variales_10_18$annual_precipitation <- replace(clu_variales_10_18$annual_precipitation, which(clu_variales_10_18$annual_precipitation < 0), NA)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(log_pop_den = log(pop_den))







#sub setting variables of interest 
clu_variales_10_18 <- as.data.frame(clu_variales_10_18)
clu_variales_10_18 <- clu_variales_10_18[,c("p_test", "wealth_2", "edu_a", "hh_size",
                                            "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
                                            "annual_precipitation", "net_use", "Rural_urban", "data_source", "state", "build_count", "region", "LONGNUM", "LATNUM", "geometry")]


# Create reduced dataset for model 2:
# (filter out rows with NA values)


#filtering by residence tyoe
urbandataset <- clu_variales_10_18 %>% filter(Rural_urban == 1)
ruraldataset <-clu_variales_10_18 %>% filter(Rural_urban == 2)
comineddataset <- clu_variales_10_18

#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_2", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)

state_sf <- state_sf %>% filter(NAME_2 == "Eti-Osa") 


state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)
))

r_map_df <- left_join(state_sf, r_random_effects_, by =c("NAME_1" = "ID"))

#setting cluster points 
cluster_points <- st_as_sf(ruraldataset)# %>% 

p_test <- cluster_points[,c("p_test")]



r_map <- tm_shape(state_sf)+
  tm_polygons()+
  tm_text("NAME_2") +
  tm_shape(cluster_points, col = "p_test", midpoint =NA, palette = "-RdYlBu") +
  tm_symbols(col = "p_test",palette = "-RdYlBu")+
  tm_legend()


r_map

