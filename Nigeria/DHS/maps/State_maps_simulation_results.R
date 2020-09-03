rm(list=ls())

setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape")

lapply(x, library, character.only = TRUE) #applying the library function to packages

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("src", "Nigeria functions.R"), envir = funEnv, toplevel.env = funEnv)

path_sim = 'C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2025_v7'



# simulation LGA results 

ai_LGA <- list.files(path = path_sim, pattern = "_each_LGA.csv", recursive = TRUE, full.names = TRUE)

ai_LGA <- sapply(ai_LGA, read.csv, simplify = F)

ai_LGA <-lapply(ai_LGA, subset, year == 2025)




# read in LGA shape file 

LGAshp <- readOGR("data/Nigeria_LGAs_shapefile_191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")

LGAshp <- st_as_sf(LGAshp)

LGA_names <- read.csv("bin/names/LGA_shp_pop_names.csv")

LGAshp <- left_join(LGAshp, LGA_names, by=c("LGA" = "LGA_shape")) %>% mutate(LGA = str_replace_all(LGA_nga_pop, "/", "-")) %>%  list()

ai_LGA_state <- map2(LGAshp, ai_LGA, left_join)

names(ai_LGA_state) <- c("Scenario 1", "Scenario 2", "Scenario 3", "Scenario 4", "Scenario 5", "Scenario 6", "Scenario 7")

ai_LGA_state <- purrr::imap(ai_LGA_state, ~mutate(.x, meta_information = .y))



# map 
map_val <- list("U5_PfPR")
maps <- map2(ai_LGA_state, map_val, funEnv$map_fun)
arrange_maps <- do.call(tmap_arrange, maps)
arrange_maps

tmap_save(tm = arrange_maps, filename = "results/State_maps/simulation results/State_U5_PfPR.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)