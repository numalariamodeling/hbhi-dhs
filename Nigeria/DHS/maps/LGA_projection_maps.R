#case management 

HS_LGA_2018 <- read.csv("bin/baseline_scenario/HS_placeholder.csv") %>% dplyr::select(-State) 
head(HS_LGA_2018)

#HS_LGA_2018$LGA <- gsub("\\-", "/", HS_LGA_2018$LGA)

#HS_LGA_2018  <- mutate(LGA = ifelse(LGA == "Ado/Odo/Ota", Ado-Odo/Ota, LGA))



DS_file <- LGAshp_sf %>% left_join(HS_LGA_2018)

head(DS_file)

HS_LGA_2018 <- tmap.fun4(DS_file, 'Baseline HS by LGA',
                                  'HS coverage',  'U5_coverage')



tmap_save(tm =HS_LGA_2018, filename = "results/maps/LGA/HS_LGA.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

#ITN 

#2020 # need to check number of LGA. Not correct 
ITN_LGA_2018 <- read.csv("bin/baseline_scenario/itn_scenario1_v2.csv") 

ITN_LGA_2018 <- split(ITN_LGA_2018, ITN_LGA_2018$year)

LGAshp_sf_ls <- list(LGAshp_sf)

ITN_LGA_2018 <- map2(LGAshp_sf_ls, ITN_LGA_2018, left_join)

table(ITN_LGA_2018[[1]]$year)

ITN_LGA_2020 <- tmap.fun4(ITN_LGA_2018[[1]], 'Baseline ITN by LGA',
                         'ITN coverage in eligible areas (2020)',  'U5_ITN_use')

ITN_LGA_2021 <- tmap.fun4(ITN_LGA_2018[[2]], 'Baseline ITN by LGA',
                          'ITN coverage in eligible areas (2021)',  'U5_ITN_use')

ITN_LGA_2022 <- tmap.fun4(ITN_LGA_2018[[3]], 'Baseline ITN by LGA',
                          'ITN coverage in eligible areas (2022)',  'U5_ITN_use')

ITN_LGA_2023 <- tmap.fun4(ITN_LGA_2018[[4]], 'Baseline ITN by LGA',
                          'ITN coverage in eligible areas (2023)',  'U5_ITN_use')

ITN_LGA_2024 <- tmap.fun4(ITN_LGA_2018[[5]], 'Baseline ITN by LGA',
                          'ITN coverage in eligible areas (2024)',  'U5_ITN_use')

ITN_LGA_2025 <- tmap.fun4(ITN_LGA_2018[[6]], 'Baseline ITN by LGA',
                          'ITN coverage in eligible areas (2025)',  'U5_ITN_use')

all_ITN <- tmap_arrange(ITN_LGA_2020, ITN_LGA_2021, ITN_LGA_2022, ITN_LGA_2023, ITN_LGA_2024, ITN_LGA_2025)

tmap_save(tm =all_ITN, filename = "results/maps/LGA/ITN_LGA.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

