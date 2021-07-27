## -----------------------------------------
### Paths
## ----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
Covdir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', "geospatial_covariates")
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'Temp_covereates')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
ResultDir <-file.path(ProjectDir, "results", "research_plots", "histograms")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
RastDir <- file.path(DataDir, "Raster_files")
# -----------------------------------------


# # Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "readtext", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt", "reshape", "ggpubr" )
# 
lapply(x, library, character.only = TRUE) #applying the library function to packages


file.reader2 <- function(filename1, filename2,filename3, filename4, filename5){
  df1 <- read.csv(file.path(Covdir, filename1),
                  header = T, sep = ',') %>% subset(select = c(1, 3, 4))
  df2 <- read.csv(file.path(Covdir, filename2),
                  header = T, sep = ',') %>% subset(select = c(1, 3, 4))
  df3 <- read.csv(file.path(Covdir, filename3),
                  header = T, sep = ',') %>% subset(select = c(1, 3, 4))
  df4 <- read.csv(file.path(Covdir, filename4),
                  header = T, sep = ',') %>% subset(select = c(1, 3, 4))
  df5 <- read.csv(file.path(Covdir, filename5),
                  header = T, sep = ',') %>% subset(select = c(1, 3, 4))
  df <- left_join(df1, df2, by = c("hv001", "dhs_year")) %>%
    left_join(., df3, by = c("hv001", "dhs_year")) %>%
    left_join(., df4, by = c("hv001", "dhs_year")) %>%
    left_join(., df5, by = c("hv001", "dhs_year")) %>% subset(select = c(-3))
  as.data.frame(colSums(is.na(df)))
  
}

###access_to_cities###time to travel to city
x <- list("access_to_cities_0m_DHS_10_15_18.csv", "access_to_cities_1000m_DHS_10_15_18.csv",
          "access_to_cities_2000m_DHS_10_15_18.csv", "access_to_cities_3000m_DHS_10_15_18.csv",
          "access_to_cities_4000m_DHS_10_15_18.csv")
access_to <- do.call(file.reader2, x)


###building desity
x <- list("building_density_0m_DHS_10_15_18.csv", "building_density_1000m_DHS_10_15_18.csv",
          "building_density_2000m_DHS_10_15_18.csv", "building_density_3000m_DHS_10_15_18.csv",
          "building_density_4000m_DHS_10_15_18.csv")

building_density <- do.call(file.reader2, x)

###dominant_vector
x <- list("dominant_vector_0m_DHS_10_15_18.csv", "dominant_vector_1000m_DHS_10_15_18.csv",
          "dominant_vector_2000m_DHS_10_15_18.csv", "dominant_vector_3000m_DHS_10_15_18.csv",
          "dominant_vector_4000m_DHS_10_15_18.csv")

dominant_vector <- do.call(file.reader2, x)

###elevation
x <- list("elevation_0m_DHS_10_15_18.csv", "elevation_1000m_DHS_10_15_18.csv",
          "elevation_2000m_DHS_10_15_18.csv", "elevation_3000m_DHS_10_15_18.csv",
          "elevation_4000m_DHS_10_15_18.csv")

elevation <- do.call(file.reader2, x)

###friction_decompressed
x <- list("friction_decompressed_0m_DHS_10_15_18.csv", "friction_decompressed_1000m_DHS_10_15_18.csv",
          "friction_decompressed_2000m_DHS_10_15_18.csv", "friction_decompressed_3000m_DHS_10_15_18.csv",
          "friction_decompressed_4000m_DHS_10_15_18.csv")

friction_decompressed <- do.call(file.reader2, x)


###housing_2000
x <- list("housing_2000_0m_DHS_10_15_18.csv", "housing_2000_1000m_DHS_10_15_18.csv",
          "housing_2000_2000m_DHS_10_15_18.csv", "housing_2000_3000m_DHS_10_15_18.csv",
          "housing_2000_4000m_DHS_10_15_18.csv")

housing_2000 <- do.call(file.reader2, x)

##housing_2015
x <- list("housing_2015_0m_DHS_10_15_18.csv", "housing_2015_1000m_DHS_10_15_18.csv",
          "housing_2015_2000m_DHS_10_15_18.csv", "housing_2015_3000m_DHS_10_15_18.csv",
          "housing_2015_4000m_DHS_10_15_18.csv")

housing_2015 <- do.call(file.reader2, x)

###motorized_friction
x <- list("motorized_friction_0m_DHS_10_15_18.csv", "motorized_friction_1000m_DHS_10_15_18.csv",
          "motorized_friction_2000m_DHS_10_15_18.csv", "motorized_friction_3000m_DHS_10_15_18.csv",
          "motorized_friction_4000m_DHS_10_15_18.csv")

motorized_friction <- do.call(file.reader2, x)


###motorized_travel
x <- list("motorized_travel_0m_DHS_10_15_18.csv", "motorized_travel_1000m_DHS_10_15_18.csv",
          "motorized_travel_2000m_DHS_10_15_18.csv", "motorized_travel_3000m_DHS_10_15_18.csv",
          "motorized_travel_4000m_DHS_10_15_18.csv")

motorized_travel <- do.call(file.reader2, x)

###pop_den_U5_FB
x <- list("pop_den_U5_FB_0m_DHS_10_15_18.csv", "pop_den_U5_FB_1000m_DHS_10_15_18.csv",
          "pop_den_U5_FB_2000m_DHS_10_15_18.csv", "pop_den_U5_FB_3000m_DHS_10_15_18.csv",
          "pop_den_U5_FB_4000m_DHS_10_15_18.csv")

pop_den_U5 <- do.call(file.reader2, x)

###pop_density
x <- list("pop_density_0m_DHS_10_15_18.csv", "pop_density_1000m_DHS_10_15_18.csv",
          "pop_density_2000m_DHS_10_15_18.csv", "pop_density_3000m_DHS_10_15_18.csv",
          "pop_density_4000m_DHS_10_15_18.csv")

pop_den <- do.call(file.reader2, x)

###pop_density_FB
x <- list("pop_density_FB_0m_DHS_10_15_18.csv", "pop_density_FB_1000m_DHS_10_15_18.csv",
          "pop_density_FB_2000m_DHS_10_15_18.csv", "pop_density_FB_3000m_DHS_10_15_18.csv",
          "pop_density_FB_4000m_DHS_10_15_18.csv")

pop_den_fb <- do.call(file.reader2, x)


###secondary_vector
x <- list("secondary_vector_0m_DHS_10_15_18.csv", "secondary_vector_1000m_DHS_10_15_18.csv",
          "secondary_vector_2000m_DHS_10_15_18.csv", "secondary_vector_3000m_DHS_10_15_18.csv",
          "secondary_vector_4000m_DHS_10_15_18.csv")

secondary_vector <- do.call(file.reader2, x)

###temp
x <- list("temp_0m_DHS_10_15_18.csv", "temp_1000m_DHS_10_15_18.csv",
          "temp_2000m_DHS_10_15_18.csv", "temp_3000m_DHS_10_15_18.csv",
          "temp_4000m_DHS_10_15_18.csv")

temp <- do.call(file.reader2, x)

###walking_friction
x <- list("walking_friction_0m_DHS_10_15_18.csv", "walking_friction_1000m_DHS_10_15_18.csv",
          "walking_friction_2000m_DHS_10_15_18.csv", "walking_friction_3000m_DHS_10_15_18.csv",
          "walking_friction_4000m_DHS_10_15_18.csv")

walking_friction <- do.call(file.reader2, x)

###walking_travel
x <- list("walking_travel_0m_DHS_10_15_18.csv", "walking_travel_1000m_DHS_10_15_18.csv",
          "walking_travel_2000m_DHS_10_15_18.csv", "walking_travel_3000m_DHS_10_15_18.csv",
          "walking_travel_4000m_DHS_10_15_18.csv")

walking_travel <- do.call(file.reader2, x)


###precip

x <- list("prec_all_yrs_0m_DHS_10_15_18.csv", "prec_all_yrs_1000m_DHS_10_15_18.csv",
          "prec_all_yrs_2000m_DHS_10_15_18.csv", "prec_all_yrs_3000m_DHS_10_15_18.csv",
          "prec_all_yrs_4000m_DHS_10_15_18.csv")

prec_all_yrs <- do.call(file.reader2, x)

#binding_vars

na_df <- dplyr::bind_rows(access_to, building_density, dominant_vector, elevation, housing_2000,
                          housing_2015, motorized_friction, motorized_travel, pop_den_U5, pop_den, 
                          pop_den_fb, secondary_vector, temp, walking_friction, walking_travel, prec_all_yrs)

write.csv(na_df, file.path(Covdir, paste0("na_counts.csv")))

#end
