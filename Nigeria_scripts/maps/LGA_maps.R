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

# ITN

ITN <- read.csv('bin/LGA_map_input/projection/ITN_scen2_80.csv')
head(ITN)

ITN_split <- split(ITN, ITN$year)

LGA_cov_2 <- left_join(LGA_cov, ITN_split$`2023`, by = "LGA")
head(LGA_cov_2)

summary(LGA_cov_2$U5_ITN_use)

#map
ten <- tm_shape(LGA_cov_2) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "ten_eighteen_ITN_use", textNA = "No data", 
              title = "", palette = "seq", breaks=c(0,0.2, 0.3, 
                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title = "2023 ten to eighteen", aes.palette = list(seq="RdYlBu")) 

ITN_maps <-tmap_arrange(u5, six, ten, over_eight)

tmap_save(tm = ITN_maps, filename = "results/LGA_maps/ITN/ITN_scen2_2022.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

# SMC 

smc <- read.csv('bin/LGA_map_input/projection/smc_scen3_no_PAAR_v3.csv') 
head(smc)
summary(smc$year)

smc_split <- split(smc, smc$year)

smc_2020 <- smc_split$`2020`[smc_split$`2020`$round == 4,]
head(smc_2020)

smc_pop <- smc_2020 %>% dplyr::select(LGA)



# population by LGA 

ng_pop <- read.csv("bin/nigeria_LGA_pop.csv") %>%  dplyr::select(LGA, geopode.pop.0.4)
head(ng_pop)



smc_pop_2 <- anti_join(ng_pop,smc_pop, by="LGA") %>% summarise(sum(geopode.pop.0.4))



summary(is.na(smc_pop_2$geopode.pop.0.4))

LGA_cov_2 <- left_join(LGA_cov, smc_2020, by = "LGA") 
head(LGA_cov_2)


summary(LGA_cov_2$coverage_high_access)

#map
twenty_20_high  <- tm_shape(LGA_cov_2) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "coverage_high_access", textNA = "No data", 
              title = "", palette = "seq", breaks=c(0,0.2, 0.3, 
                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title = "2020 SMC high LGA ITN", aes.palette = list(seq="RdYlBu")) 

SMC_maps <- tmap_arrange(twenty_20_low,twenty_20_high) 
                         
                         # (twenty_15_low, 
                         # twenty_15_high,twenty_16_low,twenty_16_high)

tmap_save(tm = twenty_20_high, filename = "results/LGA_maps/SMC/SMC_core.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


# scenario 2 

scen2 <- read.csv('bin/LGA_map_input/projection/cm_scen3_v3.csv')
head(scen2)

cm_scen2 <- split(scen2, scen2$year)

#data_20 <- cm_scen2$`2020`[cm_scen2$`2025`$round == 12, ]

LGA_cov_2 <- left_join(LGA_cov, cm_scen2$`2022`, by = "LGA")
head(LGA_cov_2)

summary(LGA_cov_2$U5_coverage)


#map
twenty_22<- tm_shape(LGA_cov_2) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "severe_cases", textNA = "No data", 
              title = "", palette = "seq", breaks=c(0,0.2, 0.3, 
                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title = "scenario 6 2022 severe", aes.palette = list(seq="RdYlBu")) 

scne2_maps <- tmap_arrange(twenty_20, twenty_21, twenty_22, twenty_23, twenty_24, twenty_25)

tmap_save(tm = scne2_maps , filename = "results/LGA_maps/CM/severe_CM_scen6-7.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


# scenario 3

scen3 <- read.csv('bin/LGA_map_input/projection/cm_scen2_10_v2.csv')
head(scen3)
scen4 <- read.csv('bin/LGA_map_input/projection/cm_scen2_20_v2.csv')
scen4
scen5 <- read.csv('bin/LGA_map_input/projection/cm_scen2_30_v2.csv')
scen5


scen3$year <- 2020
scen4$year <- 2020
scen5$year <- 2020

LGA_cov_2 <- left_join(LGA_cov, scen5, by = "LGA")
head(LGA_cov_2)

summary(LGA_cov_2$U5_coverage)

#map
cm_scen5  <- tm_shape(LGA_cov_2) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "U5_coverage", textNA = "No data", 
              title = "", palette = "seq", breaks=c(0,0.2, 0.3, 
                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title = "scenario 5 LGA CM", aes.palette = list(seq="RdYlBu")) 

cm_maps <- tmap_arrange(cm_scen3, cm_scen4, cm_scen5)

tmap_save(tm = cm_maps , filename = "results/LGA_maps/CM/CM_scen3-5.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

#scenario 6 

scen6 <- read.csv('bin/LGA_map_input/projection/cm_scen3_v3.csv')
head(scen6)

cm_split <- split(scen6, scen6$year)



LGA_cov_2 <- left_join(LGA_cov, cm_split$`2025`, by = "LGA")
head(LGA_cov_2)

#map
scen6_2025  <- tm_shape(LGA_cov_2) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "U5_coverage", textNA = "No data", 
              title = "", palette = "seq", breaks=c(0,0.2, 0.3, 
                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title = "scenario 6 2025 LGA CM", aes.palette = list(seq="RdYlBu")) 

cm_maps <- tmap_arrange(scen6_2020, scen6_2021, scen6_2022,scen6_2023,scen6_2024, scen6_2025)

tmap_save(tm = cm_maps , filename = "results/LGA_maps/CM/CM_scen6_7.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

# parasite prevalence 

para_18 <- read.csv("bin/ds_para_prevalence_2018.csv")
head(para_18)

para_18$LGA <- gsub("\\/", "-", para_18$LGA)

LGA_cov_2 <- left_join(LGAshp_sf, para_18, by = "LGA")
head(LGA_cov_2)

summary(LGA_cov_2$p_test)

#map
para_18  <- tm_shape(LGA_cov_2) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "", palette = "seq", breaks=c(0,0.2, 0.3, 
                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title = "2018", aes.palette = list(seq="-RdYlBu")) 

tmap_save(tm = para_18 , filename = "results/LGA_maps/para_prev/2018_pfpr.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


