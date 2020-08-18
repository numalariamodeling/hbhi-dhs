rm(list=ls())


setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")


#setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape")

lapply(x, library, character.only = TRUE) #applying the library function to packages

source("src/maps/map_fun.R")


# read in LGA shape file 


LGA <- clean_LGA("data/Nigeria_LGAs_shapefile_191016", "bin/names/LGA_shp_pop_names.csv")

#state <- LGA%>%  dplyr::select(State, LGA)




file_path <- "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/projection_csvs/2010_2020_LGA_intervention_files/ITN"
#path <- "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2025_v6"

# SMC

# smc <- read.csv(paste0(path, "/smc_scen2_80.csv"))
# head(smc)
# summary(smc$year)
# 
# smc_2 <- smc %>% mutate(overall_coverage = coverage_high_access* 0.5 + coverage_low_access* 0.5) %>%
#   group_by(LGA, year) %>%
#   summarise(coverage = mean(overall_coverage)) %>%
#   ungroup(LGA) %>%  
#   mutate(LGA = trimws(LGA), LGA = case_when(LGA == "kiyawa"~ "Kiyawa", LGA == "kaita" ~"Kaita", TRUE ~ LGA),
#          LGA = str_replace_all(LGA, "/", "-")) #%>%  
#   #dplyr::select(-c(LGA))
# 
# head(smc_2)
# 
# smc_split <- split(smc_2, smc_2$year)
# 
# 
# LGAshp <- list(LGAshp_sf_2)
# 
# join<- map2(LGAshp,smc_split,left_join)



# # CM
# cm <- read.csv(paste0(path,"/cm_scen3_v3.csv")) %>% mutate(LGA = trimws(LGA), LGA = str_replace_all(LGA, "/", "-")) #%>%  dplyr::select(-c(State))
# head(cm)
# 
# cm_split <- split(cm, cm$year)
# # 
# LGAshp <- list(LGAshp_sf_2)
# 
# # join <- left_join(LGAshp_sf_2, cm, by = "LGA") 
# # join <- list(join)
# 
# join <- map2(LGAshp, cm_split, left_join)

# ITN

itn <- read.csv(paste0(file_path, "/ITN_by_LGA_v6.csv")) #%>%  
  #mutate(coverage = rowMeans(dplyr::select(.,ends_with('ITN_use')),na.rm = TRUE ))
head(itn)

if(nrow(itn) > 774) {
  itn_split <- split(itn, itn$year)
  LGA <- list(LGA)
  join <- map2(LGA, itn_split, left_join)
} else {
  join <- left_join(LGA, itn)
}

# #funder map 
# 
# admin1shp <- readOGR("bin/NGA_cnty_admin1", layer ="nga_polbnda_adm1_1m_salb", use_iconv=TRUE, encoding= "UTF-8")
# admin1_sf <- st_as_sf(admin1shp)
# head(admin1_sf)
# 
# LGA_funder <- read.csv(paste0(path, "/LGA_funder.csv")) %>% 
#   mutate(LGA = str_replace_all(LGA, "/", "-"), LGA = case_when(LGA == "kiyawa"~ "Kiyawa", LGA == "kaita" ~"Kaita", TRUE ~ LGA))
# LGA_funder <- left_join(LGA_funder, state) %>%dplyr::select(-c(geometry))
# head(LGA_funder)
# join <- left_join(admin1_sf, LGA_funder, by = c("ADM1_NAME" = "State"))
# head(join)
# join <- list(join)

# map

map_val <- list("U5_ITN_use")
var <- list("itn")
maps <- pmap(list(join, map_val, var), map_fun)
arrange_maps <- do.call(tmap_arrange, maps)
arrange_maps

# output 

subDir <- "results/LGA_maps/ITN"
dir.create(file.path(getwd(), subDir), showWarnings = FALSE)

tmap_save(tm =arrange_maps, filename = paste0(subDir,"/U5_ITN_2010_2018.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


