rm(list=ls())

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

#loading dataset

merged_df <- read.csv("data/Nigereia_2010_2018_clustered_final_dataset.csv", header= TRUE)


urbandataset <- merged_df %>% filter(Rural_urban == 1)
ruraldataset <- merged_df %>% filter(Rural_urban == 2)
comineddataset <- merged_df

#selecting variables of interest. replace datafrom with cluster of interest
df <- urbandataset[ ,colnames(urbandataset) 
                    %in% c("wealth_2", "u5_prop", "net_access","preg","edu_a","net_use_u5", 
                           "net_use_preg", "hh_size", "house_floor", "house_wall", 
                           "house_roof", "housing_qua","ACT_use_u5", "pop_den",
                           "l_pop_den","p_test", "humidindex", "Rural_urban")]
df2 <- df

#df2 <- df %>% mutate(scaled_hudix = scale(df$humidindex, center = T))

#colapsing p_test into 0 and 1 

df2 <- df2 %>% filter(p_test != "NA")
levels <- c(-2, (median(df2$p_test)), 1)
labels <- c("very low", "high")
df3 <- df2 %>% mutate(p_level = cut(p_test, levels, labels = labels))
df3 <- df3[!is.na(df3$p_level), ]


# create factor variables 
df3$wealth_2 <- factor(df3$wealth_2)
df3$u5_prop <- factor (df3$u5_prop)
df3$net_access <- factor(df3$net_access)
df3$preg <- factor(df3$preg) 
df3$edu_a <- factor (df3$edu_a)
df3$net_use_u5 <- factor(df3$net_use_u5)
df3$net_use_preg <- factor(df3$net_use_preg) 
df3$hh_size <- factor (df3$hh_size)
df3$ACT_use_u5 <- factor(df3$ACT_use_u5)
df3$pop_den <- factor(df3$pop_den) 
df3$l_pop_den <- factor(df3$l_pop_den) 
df3$p_test <- factor (df3$p_test)
df3$humidindex <- factor(df3$humidindex)
df3$Rural_urban <- factor(df3$Rural_urban) 

# obtain information about missing data 
inf <- mi.info(df3)


# use mice to impute and pool

imp <- mice(df3,n.imp=5,seed=1934)
summary(imp)

mydata <- imputationList(lapply(1:5, complete, x=imp))
summary(mydata) 