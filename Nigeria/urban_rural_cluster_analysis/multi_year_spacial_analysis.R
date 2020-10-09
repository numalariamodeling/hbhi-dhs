rm(list=ls())

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mudata2")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/Admin/Documents/NU - Malaria Modeling/Non Linear")

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed
#Loading shapefiles

read.files <- function(filepat1,path,fun) {
  filenames <- list.files(path = path, pattern = filepat1, recursive = TRUE, full.names = TRUE)
  sapply(filenames, fun, simplify = F)
}

NGAshplist<-read.files("*FL.*\\.shp$", getwd(), shapefile)

NGDir <-file.path(getwd(), "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")

#read pre clustered 2018 dhs data 

dhs_18 <- read.csv("all_cluster_kaps.csv", header= TRUE)
dhs_15 <- read.csv("mis_all.csv", header= TRUE)


#loading dta 
dhs_18 <- list.files(pattern = ".*NGPR.*\\.DTA", recursive = F, full.names = TRUE)
dhs_18 <- sapply(dhs_18, read_dta, simplify = F)


dhs_15 <- list.files(pattern = ".*MISPR.*\\.DTA", recursive = F, full.names = TRUE)
dhs_15 <- sapply(dhs_15, read_dta, simplify = F)

#read shapefile 

LGAshp <- readOGR("Nigeria_LGAs_shapefile_191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
LGAshp_sf <- st_as_sf(LGAshp)

#reading repDS

repDS <- read.csv("repDS_v2.csv", header= TRUE)

# 2018 transformations urban
DS_file <- LGAshp_sf 

pts_shp_18 <- st_as_sf(NGAshplist[[2]])

edu_wealth_p <- dhs_18[ ,colnames(dhs_18) 
                              %in% c("hv001", "wealth_2", "edu_a", "p_test")]

pts_file <- dplyr::left_join(pts_shp_18, edu_wealth_p, by=c("DHSCLUST" = "hv001")) #%>% filter(URBAN_RURA =="U") #%>% filter("hv001 < 2000) 
head(pts_file)
summary(is.na(pts_file$edu_a))
summary(pts_file$edu_a)
sd(pts_file$`Number of Participants`)

# 2015 transformations urban

DS_file_15 <- LGAshp_sf 

pts_shp_15 <- st_as_sf(NGAshplist[[1]])

edu_wealth_p <- dhs_15[ ,colnames(dhs_15) 
                        %in% c("hv001", "wealth_2", "edu_a", "p_test")]

pts_file_15 <- dplyr::left_join(pts_shp_15, edu_wealth_p, by=c("DHSCLUST" = "hv001")) #%>% filter(URBAN_RURA =="U") #%>% filter("hv001 < 2000) 
head(pts_file_15)
summary(is.na(pts_file_15$edu_a))
summary(pts_file$edu_a)
sd(pts_file_15$`Number of Participants`)


clus_est_18 <- tmap.fun3(DS_file, "edu_a",  "Prevalence", "edu_a 2018",
                       pts_file, "Number of Participants", "edu_a")

clus_est_18 <- tmap.fun4(DS_file, "U5 Malaria Prev (2018)", "Prevalence", "edu_a")


#2015 map


u_clu_map_15 <- tmap.clu4(admin1_sf, ptsfile=pts_file_15, "Number of Participants", "edu_a", "2015 cluster Malaria Nigeria")

mal_est_15 <- tmap.fun3(DS_file_15, "edu_a",  "Prevalence", "HH Malaria 2015",
                       pts_file_15, "Number of Participants", "edu_a")

mal_est_15 <- tmap.fun4(DS_file_15, "U5 Malaria (2015)", "Prevalence", "edu_a")


# creating a combined prevalence map for all years 
#row binding cluster points 
pts_merge <- rbind(pts_file,pts_file_15)

head(pts_merge)
summary(is.na(pts_merge$edu_a))
summary(pts_merge$edu_a)
summary(pts_merge$`Number of Participants`)
sd(pts_merge$`Number of Participants`)



#aggregated plot on state shape file 
u_clu_map_bindrows <- tmap.clu4(admin1_sf, ptsfile=pts_merge, "Number of Participants", "edu_a", "Malaria Prevalence 2015 - 2018 in Nigeria")


#creates aggreagted map of pfpr for all years using mean values 

#calculate mean of points to pfpr estimate over time 
pts_merge_test <- pts_merge %>% drop_na(edu_a)

pts100 <- st_is_within_distance(pts_merge_test, dist = 2000)
class(pts100)

res <- data.frame(id=1:length(pts100),x=NA, y=NA,mean_edu_a=NA)
res$x <- sapply(pts100, function(p){mean(pts_merge_test$LONGNUM[p])})
res$y <- sapply(pts100, function(p){mean(pts_merge_test$LATNUM[p])})
res$mean_edu_a<- sapply(pts100, function(p) #scaled by the proportion of participants in each nearby cluster 
{sum((pts_merge_test$`Number of Participants`[p]/sum(pts_merge_test$`Number of Participants`[p]) * pts_merge_test$edu_a[p]))})
res$`Number of Participants` <- sapply(pts100, function(p){mean(pts_merge_test$`Number of Participants`[p])}) 
res_2_edu_a <- res %>% distinct(x, y, .keep_all = TRUE)

coordinates(res_2_edu_a ) <- c("x","y")

proj4string(res_2_edu_a ) <- CRS('+proj=longlat +datum=WGS84 +no_defs')



res_sf_edu_a <- st_as_sf(res_2_edu_a )
head(res_sf_edu_a )

summary(is.na(res_sf_edu_a$mean_edu_a))
summary(res_sf_edu_a$mean_edu_a)
summary(res_sf_edu_a$`Number of Participants`)
sd(res_sf_edu_a$`Number of Participants`)



u_clu_map_aggregated <- tmap.clu4(admin1_sf, ptsfile=res_sf, "Number of Participants", "mean_edu_a", "Aggregated Education Prop 2015 - 2018 in Nigeria")

all_maps <- tmap_arrange(u_clu_map_15,u_clu_map,u_clu_map_bindrows,u_clu_map_aggregated)

tmap_save(tm = all_maps, filename = "results/edu_diff.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)




