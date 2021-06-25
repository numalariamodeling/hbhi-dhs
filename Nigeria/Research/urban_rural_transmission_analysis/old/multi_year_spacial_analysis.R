rm(list=ls())

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mudata2", "mapview")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/pc/Documents/NU - Malaria Modeling/Muilti year spacial analysis")

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

##################loading data###########

#Loading shapefiles

dhs18_sf <- st_read("DHS18GPS/NGGE7AFL.shp")

mis15_sf <- st_read("MIS15GPS/NGGE71FL.shp")

mis10_sf <- st_read("MIS10GPS/NGGE61FL.shp")

#loading preclusters and propotion varaibes

clu_variales_10_18 <- read.csv("final_dataset.csv", header= TRUE)

##creating prevelence classes
clu_variales_10_18 <- clu_variales_10_18 %>% filter(p_test != "NA")
levels <- c(-2, (median(clu_variales_10_18$p_test)), 1)
labels <- c(0, 1)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(p_level = cut(p_test, levels, labels = labels))
clu_variales_10_18 <- clu_variales_10_18[!is.na(clu_variales_10_18$p_level), ]

#readinf nigeria baoudaries 

LGAshp <- readOGR("Nigeria_LGAs_shapefile_191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
LGAshp_sf <- st_as_sf(LGAshp)
DS_file <- LGAshp_sf

#joining shapefiles with variables of interest DHIS18
clu_variales_18 <- clu_variales_10_18[,c("hv001", "wealth_2", "edu_a", "p_test", "p_level", "data_source")]

clu_bourdaries_18 <- dplyr::left_join(dhs18_sf, clu_variales_18, 
                             by=c("DHSCLUST" = "hv001")) #%>% filter(URBAN_RURA =="U") #%>% filter("hv001 < 2000) 



clu_bourdaries_18 <- clu_bourdaries_18 %>% filter(data_source == "dhs2018")

PLYS <- LGAshp_sf[,c("CNTRY_CODE")]
p_level_18 <- clu_bourdaries_18[,c("p_level")]
edu_level_18 <- clu_bourdaries_18[,c("edu_a")]

mapview::mapview(PLYS, zcol = "CNTRY_CODE") + 
  mapview::mapview(p_level_18 )


mapsfun <- function(adminfile,ptsfile, bubble_size, bubble_col, title) {
  tm_shape(adminfile) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(ptsfile)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size=bubble_size, col = bubble_col, alpha = 0.5,
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    #tm_shape(ptsfile[is.na(ptsfile$p_test),])+
    #tm_bubbles(textNA = "Missing", legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title=title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}


#joining shapefiles with variables of interest DHIS18
clu_variales_15 <- clu_variales_10_18 %>% filter(data_source == "mis2015")
clu_variales_15 <- clu_variales_15[,c("hv001", "wealth_2", "edu_a", "p_test", "p_level")]

clu_bourdaries_15 <- dplyr::left_join(mis15_sf, clu_variales_15, 
                                      by=c("DHSCLUST" = "hv001")) #%>% filter(URBAN_RURA =="U") #%>% filter("hv001 < 2000) 

p_level_15 <- clu_bourdaries_15[,c("p_level")]
edu_level_18 <- clu_bourdaries_15[,c("edu_a")]

mapview::mapview(PLYS, zcol = "CNTRY_CODE") + 
  mapview::mapview(p_level_15)

#plotting maps
u_clu_map_18 <- mapsfun(PLYS, p_level_18,  1, "black", "Education levels 2018")
u_clu_map_18

u_clu_map_15 <- mapsfun(PLYS, p_level_15,  1, "black", "Education levels 2015")
u_clu_map_15


arranged <- tmap_arrange(u_clu_map_15,u_clu_map_18)

# creating a combined prevalence map for all years 
#row binding cluster points 
pts_merge <- rbind(p_level_15, p_level_18)

#aggregated plot on state shape file 
u_clu_map_bindrows <- mapsfun(PLYS, pts_merge, 1, "brown", "Transmission Intersity Level 2015 - 2018 in Nigeria")
u_clu_map_bindrows

mapview::mapview(PLYS, zcol = "CNTRY_CODE") + 
  mapview::mapview(pts_merge)

#creates aggreagted map of pfpr for all years using mean values 

#calculate mean of points to pfpr estimate over time 
pts_merge_test <- pts_merge %>% drop_na(p_level)

pts_merge_test_coord <- pts_merge_test %>%
  dplyr::mutate(LATNUM = sf::st_coordinates(.)[,1],
                LONGNUM = sf::st_coordinates(.)[,2])


pts100 <- st_is_within_distance(pts_merge_test_coord, dist = 10)
class(pts100)

res <- data.frame(id=1:length(pts100),x=NA, y=NA,mean_p_level=NA)
res$x <- sapply(pts100, function(p){mean(pts_merge_test_coord$LONGNUM[p])})
res$y <- sapply(pts100, function(p){mean(pts_merge_test_coord$LATNUM[p])})
res$mean_p_level<- pts_merge_test_coord$p_test
#res$mean_p_level<- sapply(pts100, function(p) #scaled by the proportion of participants in each nearby cluster 
#{sum((pts_merge_test$`Number of Participants`[p]/sum(pts_merge_test$`Number of Participants`[p]) * pts_merge_test$p_level[p]))})
#res$`Number of Participants` <- sapply(pts100, function(p){mean(pts_merge_test$`Number of Participants`[p])}) 
res_2_p_level <- res %>% distinct(x, y, .keep_all = TRUE)

coordinates(res_2_p_level) <- c("x","y")

proj4string(res_2_p_level) <- CRS('+proj=longlat +datum=WGS84 +no_defs')



res_sf_p_level <- st_as_sf(res_2_p_level)
head(res_sf_p_level)

mapview::mapview(PLYS, zcol = "CNTRY_CODE") + 
  mapview::mapview(res_sf_p_level)


clu_proxi <- dplyr::left_join(res_sf_p_level,clu_variales_10_18, 
                                      by=c("id" = "hv001"))

clu_proxi_urban <- clu_proxi %>% filter(Rural_urban == 1)

clu_proxi_urban_p_level <- clu_proxi_urban[,c("geometry", "p_level")]

mapview::mapview(PLYS, zcol = "CNTRY_CODE") + 
  mapview::mapview(clu_proxi_urban_p_level)

u_clu_map_aggregated <- mapsfun(PLYS, res_sf_p_level, 1, "brown", "Aggregated Transmission 2010 - 2018 in Nigeria")
u_clu_map_aggregated 

all_maps <- tmap_arrange(u_clu_map_15,u_clu_map_18,u_clu_map_bindrows,u_clu_map_aggregated)
all_maps

tmap_save(tm = all_maps, filename = "Results/all_cluster/trans.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


