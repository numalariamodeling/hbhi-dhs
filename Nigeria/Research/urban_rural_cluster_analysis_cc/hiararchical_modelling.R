x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", "randomGLM", "ROCR", "caretEnsemble", "klaR", "naniar", "corrplot", 
       "lmtest", "Deducer", "lattice", "ResourceSelection", "INLA", "expp", "spatialreg", "spdep")

lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)

# set document path to current script path 
setwd("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data")
###################### Loading data################################################
# Load pre-clustered data:
clu_variales_10_18 <- read.csv("Nigeria_2010_2018_clustered_final_dataset.csv", 
                 header = T, sep = ',')

# Binarize response:
clu_variales_10_18$y <- ifelse(clu_variales_10_18$p_test < 0.1, 0,1)

#cleaning precip data
clu_variales_10_18 <- clu_variales_10_18%>% mutate(annual_precipitation = scale(clu_variales_10_18$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

clu_variales_10_18 <- clu_variales_10_18 %>% mutate(pop_den = na_if(pop_den, -9999))
clu_variales_10_18$annual_precipitation <- replace(clu_variales_10_18$annual_precipitation, which(clu_variales_10_18$annual_precipitation < 0), NA)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(log_pop_den = log(pop_den))


#Loading shapefiles

dhs18_sf <- st_read("NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp")

mis15_sf <- st_read("NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp")

mis10_sf <- st_read("NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp")

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
                                            "annual_precipitation", "net_use", "Rural_urban", "data_source")]


# Create reduced dataset for model 2:
# (filter out rows with NA values)
df_10_18_fin <- na.omit(df_10_18_fin)
table(df_10_18_fin$y)

#filtering by residence tyoe
urbandataset <- df_10_18_fin %>% filter(Rural_urban == 1)
ruraldataset <- df_10_18_fin %>% filter(Rural_urban == 2)
comineddataset <- df_10_18_fin



##############Fitting Simple model############################################
#to assess whether spatial random effects are really required when modeling 
#these data. The estimates of the random effects (i.e., posterior means)

#GETTING nearest neibour clusters
dat_df <- ruraldataset
dat_ds <- as.data.frame(ruraldataset)
dat_sf <- dat_ds[,c("LONGNUM", "LATNUM","DHSCLUST")]



table(dat_df$y)

coordinates(dat_sf) <- ~ LONGNUM + LATNUM
#neib <- knn2nb(knearneigh(dat_sf, k = 1))
neib  <- voronoi.polygons(SpatialPoints(dat_sf))
plot(neib)

                         
#neib <- dnearneigh(coordinates(dat_sf), 0, 10, longlat = TRUE)
#neib_df <- neighborsDataFrame(neib) 
summary(neib)


## Transform 'SpatialPolygonsDataFrame' object to 'sf' class
neib <- sf::st_as_sf(neib)

## Compute the neighbours list of class 'nb'
#if(is.null(neib)) neib <- spdep::poly2nb(neib)
nb.r <- poly2nb(neib, queen=T)


mat <- nb2mat(nb.r, style="B")
summary(mat)

plot(neib)
row = 1015
indx=dat_df[[row]]  # Determine adjacent districts (by row in shapefile)
indx

row.names(dat_df) <- NULL
#W_neib_df <- nb2mat(neib , style="B") 


#fitting Model that accounts for random effects that is non-spatial (iid), 

form_10to18 <- y ~ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 + pop_den + 
  hh_members_age + sex_f + annual_precipitation


all_iid <- inla(update(form_10to18, . ~. + f(DHSCLUST, model = "iid")),
                   data = as.data.frame(dat_df), family='binomial',
                   control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                   control.predictor = list(compute = TRUE)
)
summary(all_iid)


#fit the Simple INLA model with no random effects

dat_simp <- inla(y~wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 + pop_den + 
                   hh_members_age + sex_f + annual_precipitation, family='binomial',
                 data=dat_df,
                 control.family=list(link='logit'),
                 control.predictor=list(link=1, compute=TRUE),
                 control.compute=list(dic=TRUE, cpo=TRUE, waic=TRUE))
#examine the regular summary 
summary(dat_simp)


# Specify linear predictor

formula_nieb = y ~ edu_a + net_use + hh_size  + pop_den + 
  hh_members_age + sex_f + annual_precipitation + f(wealth_2, model ="bym", graph = mat)
# Run model
result = inla (formula_nieb ,family = " binomial ", data = dat_df)

summary(result)

