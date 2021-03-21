rm(list=ls())

x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", "randomGLM", "ROCR", "caretEnsemble", "klaR", "naniar", "corrplot", 
       "lmtest", "Deducer", "lattice", "ResourceSelection", "INLA", "expp", "spatialreg", "spdep")

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

table(clu_variales_10_18$state)

# Binarize response:
clu_variales_10_18$y <- ifelse(clu_variales_10_18$p_test < 0.1, 0,1)

#cleaning precip data
clu_variales_10_18 <- clu_variales_10_18%>% mutate(annual_precipitation = scale(clu_variales_10_18$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

clu_variales_10_18 <- clu_variales_10_18 %>% mutate(pop_den = na_if(pop_den, -9999))
clu_variales_10_18$annual_precipitation <- replace(clu_variales_10_18$annual_precipitation, which(clu_variales_10_18$annual_precipitation < 0), NA)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(log_pop_den = log(pop_den))


#Loading shapefiles

dhs18_sf <- st_read(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"))

mis15_sf <- st_read(file.path(DataDir,"NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"))

mis10_sf <- st_read(file.path(DataDir,"NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"))

plot(mis10_sf$geometry)

summary(dhs18_sf)


# join dhs variables to shapefiles by year 

df_18 <- clu_variales_10_18 %>% filter(data_source == "dhs2018") 

df_18_fin <- left_join(dhs18_sf, df_18, by = c("DHSCLUST" ="hv001"))


df_15 <- clu_variales_10_18 %>% filter(data_source == "mis2015") 

df_15_fin <- left_join(mis15_sf, df_15, by = c("DHSCLUST" ="hv001"))


df_10 <- clu_variales_10_18 %>% filter(data_source == "mis2010") 

df_10_fin <- left_join(mis10_sf, df_10, by = c("DHSCLUST" = "hv001"))


df_10_18_fin <- rbind(df_18_fin, df_15_fin, df_10_fin)


#sub setting variables of interest 
df_10_18_fin <- df_10_18_fin[,c("LONGNUM", "LATNUM", "DHSCLUST", "y", "wealth_2", "edu_a", "hh_size",
                                            "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
                                            "annual_precipitation", "net_use", "Rural_urban", "data_source", "state", "build_count", "region")]


# Create reduced dataset for model 2:
# (filter out rows with NA values)
df_10_18_fin <- na.omit(df_10_18_fin)
table(df_10_18_fin$y)

#filtering by residence tyoe
urbandataset <- df_10_18_fin %>% filter(Rural_urban == 1)
ruraldataset <- df_10_18_fin %>% filter(Rural_urban == 2)
comineddataset <- df_10_18_fin



###################################################################################
####rural dataset
###################################################################################
  
library(INLA)

#regular rural model without random effect 
r_rmod <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
              hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
            data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE)) 

summary(r_rmod)

#model with random effects in state
r_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                   hh_members_age + sex_f + log(annual_precipitation) +log(build_count)+ + humidindex +f(state, model = "iid"), family = 'binomial',
                 data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE))

summary(r_iid)

#extracting state random effects 
r_random_effects <- r_iid$summary.random

#changing to dataframe
r_random_effects_<- do.call(cbind, r_random_effects)

#extracting factos 
r_random_effects_$state.ID <-str_to_title(r_random_effects_$state.ID)
r_random_effects_$state.ID <- factor(r_random_effects_$state.ID, levels=rev(r_random_effects_$state.ID))
r_random_effects_$state.ID<- trimws(r_random_effects_$state.ID)


#quick plot of random effect 
library(ggplot2)
r_fp <- ggplot(data=r_random_effects_, aes(x=state.ID, y=state.mean, ymin=state.0.025quant, ymax=state.0.975quant)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(r_fp)


#map 

#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)

state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)
                                                   ))

r_map_df <- left_join(state_sf, r_random_effects_, by =c("NAME_1" = "state.ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
r_map <- tm_shape(r_map_df)+
  tm_polygons(col = "state.mean", midpoint =NA, palette = "-RdYlGn")+
  tm_text("NAME_1")



###################################################################################
####urban dataset
###################################################################################
#regular urban model without random effect 
u_rmod <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
               hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
             data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE)) 

summary(u_rmod)

#model with random effects in state
u_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
              hh_members_age + sex_f + log(annual_precipitation) +log(build_count)+  humidindex +f(state, model = "iid"), family = 'binomial',
            data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE))

summary(u_iid)


# library(lme4)
# st_geometry(urbandataset) <- NULL
# 
# urbandataset$log_annual_precipitation<-log(urbandataset$annual_precipitation)
# urbandataset$log_build_count<-log(urbandataset$build_count)
# urbandataset$annual_precipitation<-NULL
# 
# urbandataset$build_count<-NULL
# u_lm <- glmer(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
#                 hh_members_age + sex_f + log_annual_precipitation +log_build_count+(1|state),family = binomial(link = "logit"),
#               data = urbandataset)
# 
# 
# 
# num_cols<-c("wealth_2", "edu_a", "net_use", "hh_size", "ACT_use_u5",
#             "hh_members_age", "sex_f", "log_annual_precipitation" , "log_build_count")
# 
# 
# urbandataset_scaled  <- urbandataset
# urbandataset_scaled [,num_cols] <- scale(urbandataset_scaled [,num_cols])
# m1_sc <- update(u_lm,data=urbandataset_scaled )
# summary(m1_sc)




u_lm <- glmer(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)+(1|state),family = binomial(link = "logit"),
              data = urbandataset_scaled)

summary(u_lm)

#extracting state random effects 
u_random_effects <- u_iid$summary.random

#changing to dataframe
u_random_effects_<- do.call(cbind, u_random_effects)

#extracting factos 
u_random_effects_$state.ID <-str_to_title(u_random_effects_$state.ID)
u_random_effects_$state.ID <- factor(u_random_effects_$state.ID, levels=rev(u_random_effects_$state.ID))
u_random_effects_$state.ID<- trimws(u_random_effects_$state.ID)

#quick plot of state random effect. South-West state have statistically significant effects
library(ggplot2)
u_fp <- ggplot(data=u_random_effects_, aes(x=state.ID, y=state.mean, ymin=state.0.025quant, ymax=state.0.975quant)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(u_fp)


#map
u_map_df <- left_join(state_sf, u_random_effects_, by =c("NAME_1" = "state.ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
u_map <- tm_shape(u_map_df)+
  tm_polygons(col = "state.mean", midpoint =NA, palette = "-RdYlGn", breaks =c(-1.5, -1.0, -0.5, 0, 0.2, 1.0, 1.5, 2.0))+
  tm_text("NAME_1")
