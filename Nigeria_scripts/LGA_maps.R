CM <- read.csv("bin/LGA_map_input/HS_by_LGA_v2_mid.csv") 
head(CM)

CM$LGA <- gsub("\\/", "-", CM$LGA)


cm_split <- split(CM, CM$year)

LGAshp <- readOGR("data/Nigeria_LGAs_shapefile_191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")

LGAshp_sf <- st_as_sf(LGAshp)
LGAshp_sf$LGA <- gsub("\\/", "-", LGAshp_sf$LGA)

LGA_cov <-  LGAshp_sf %>% mutate(LGA = ifelse(LGA == "kaita","Kaita", ifelse(LGA == "kiyawa", "Kiyawa", as.character(LGA))))

LGA_cov_2 <- left_join(LGA_cov, cm_split$`2018`, by = "LGA")
head(LGA_cov_2)

summary(LGA_cov_2$U5_coverage)


#map
eighteen <- tm_shape(LGA_cov_2) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "U5_coverage", textNA = "No data", 
              title = "", palette = "seq", breaks=c(0,0.2, 0.3, 
                                                               0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title = "2018 LGA CM", aes.palette = list(seq="RdYlBu")) 

CM_maps <-tmap_arrange(ten, thirteen, fifteen, eighteen)

tmap_save(tm = CM_maps, filename = "results/LGA_maps/CM/CM_2010_2018.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)