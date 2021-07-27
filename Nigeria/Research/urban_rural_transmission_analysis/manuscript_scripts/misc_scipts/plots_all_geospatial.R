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

file.reader <- function(filename){
  df <- read.csv(file.path(Covdir, filename),
                 header = T, sep = ',')
  ggplot(df, aes_string(x=names(df)[3])) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(text = element_text(size=16))+
    labs (title = names(df)[3], x = "values")+
    xlab("") 
  
}

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
  melteddf <- melt(df, id=names(df)[1], na.rm=T)
  ggplot(melteddf, aes_string(x= "value", fill = "variable", color = "variable")) +
    geom_freqpoly(size = 2) + ggtitle("...") + 
    theme_classic()+
    theme(text = element_text(size=16))
  
}

ggsave.fun <- function(x, saveas){
  plot_list <- lapply(x, FUN = file.reader)
  variables <- ggarrange(plotlist=plot_list, nrow =1, ncol=5)
  ggsave(paste0(ResultDir, '/', Sys.Date(),  saveas), variables, width=13, height=7.5)
}

ggsave.fun2 <- function(x, save_as){
  combine <- do.call(file.reader2, x)
  ggsave(paste0(ResultDir, '/', Sys.Date(),  save_as), combine, width=13, height=7.5)
}

###access_to_cities###time to travel to city
x <- list("access_to_cities_0m_DHS_10_15_18.csv", "access_to_cities_1000m_DHS_10_15_18.csv",
          "access_to_cities_2000m_DHS_10_15_18.csv", "access_to_cities_3000m_DHS_10_15_18.csv",
          "access_to_cities_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "access_to_cities_seperated.pdf")

combined <- ggsave.fun2(x, "access_to_cities_combined.pdf")


###building desity
x <- list("building_density_0m_DHS_10_15_18.csv", "building_density_1000m_DHS_10_15_18.csv",
          "building_density_2000m_DHS_10_15_18.csv", "building_density_3000m_DHS_10_15_18.csv",
          "building_density_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "building_density_seperated.pdf")

combined <- ggsave.fun2(x, "building_density_combined.pdf")

###dominant_vector
x <- list("dominant_vector_0m_DHS_10_15_18.csv", "dominant_vector_1000m_DHS_10_15_18.csv",
          "dominant_vector_2000m_DHS_10_15_18.csv", "dominant_vector_3000m_DHS_10_15_18.csv",
          "dominant_vector_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "dominant_vector_seperated.pdf")

combined <- ggsave.fun2(x, "dominant_vector_combined.pdf")

###elevation
x <- list("elevation_0m_DHS_10_15_18.csv", "elevation_1000m_DHS_10_15_18.csv",
          "elevation_2000m_DHS_10_15_18.csv", "elevation_3000m_DHS_10_15_18.csv",
          "elevation_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "elevation_seperated.pdf")

combined <- ggsave.fun2(x, "elevation_combined.pdf")

###elevation 2

x <- list("elevation_0m_DHS_10_15_18.csv", "elevation_1000m_DHS_10_15_18.csv",
          "elevation_2000m_DHS_10_15_18.csv", "elevation_3000m_DHS_10_15_18.csv",
          "elevation_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "elevation_seperated.pdf")

combined <- ggsave.fun2(x, "elevation_combined.pdf")


###friction_decompressed
x <- list("friction_decompressed_0m_DHS_10_15_18.csv", "friction_decompressed_1000m_DHS_10_15_18.csv",
          "friction_decompressed_2000m_DHS_10_15_18.csv", "friction_decompressed_3000m_DHS_10_15_18.csv",
          "friction_decompressed_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "friction_decompressed_seperated.pdf")

combined <- ggsave.fun2(x, "friction_decompressed_combined.pdf")


###housing_2000
x <- list("housing_2000_0m_DHS_10_15_18.csv", "housing_2000_1000m_DHS_10_15_18.csv",
          "housing_2000_2000m_DHS_10_15_18.csv", "housing_2000_3000m_DHS_10_15_18.csv",
          "housing_2000_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "housing_2000_seperated.pdf")

combined <- ggsave.fun2(x, "housing_2000_combined.pdf")

##housing_2015
x <- list("housing_2015_0m_DHS_10_15_18.csv", "housing_2015_1000m_DHS_10_15_18.csv",
          "housing_2015_2000m_DHS_10_15_18.csv", "housing_2015_3000m_DHS_10_15_18.csv",
          "housing_2015_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "housing_2015_seperated.pdf")

combined <- ggsave.fun2(x, "housing_2015_combined.pdf")

###motorized_friction
x <- list("motorized_friction_0m_DHS_10_15_18.csv", "motorized_friction_1000m_DHS_10_15_18.csv",
          "motorized_friction_2000m_DHS_10_15_18.csv", "motorized_friction_3000m_DHS_10_15_18.csv",
          "motorized_friction_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "motorized_friction_seperated.pdf")

combined <- ggsave.fun2(x, "motorized_friction_combined.pdf")


###motorized_travel
x <- list("motorized_travel_0m_DHS_10_15_18.csv", "motorized_travel_1000m_DHS_10_15_18.csv",
          "motorized_travel_2000m_DHS_10_15_18.csv", "motorized_travel_3000m_DHS_10_15_18.csv",
          "motorized_travel_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "motorized_travel_seperated.pdf")

combined <- ggsave.fun2(x, "motorized_travel_combined.pdf")

###pop_den_U5_FB
x <- list("pop_den_U5_FB_0m_DHS_10_15_18.csv", "pop_den_U5_FB_1000m_DHS_10_15_18.csv",
          "pop_den_U5_FB_2000m_DHS_10_15_18.csv", "pop_den_U5_FB_3000m_DHS_10_15_18.csv",
          "pop_den_U5_FB_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "pop_den_U5_FB_seperated.pdf")

combined <- ggsave.fun2(x, "pop_den_U5_FB_combined.pdf")

###pop_density
x <- list("pop_density_0m_DHS_10_15_18.csv", "pop_density_1000m_DHS_10_15_18.csv",
          "pop_density_2000m_DHS_10_15_18.csv", "pop_density_3000m_DHS_10_15_18.csv",
          "pop_density_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "pop_density_seperated.pdf")

combined <- ggsave.fun2(x, "pop_density_combined.pdf")

###pop_density_FB
x <- list("pop_density_FB_0m_DHS_10_15_18.csv", "pop_density_FB_1000m_DHS_10_15_18.csv",
          "pop_density_FB_2000m_DHS_10_15_18.csv", "pop_density_FB_3000m_DHS_10_15_18.csv",
          "pop_density_FB_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "pop_density_FB_seperated.pdf")

combined <- ggsave.fun2(x, "pop_density_FB_combined.pdf")


###secondary_vector
x <- list("secondary_vector_0m_DHS_10_15_18.csv", "secondary_vector_1000m_DHS_10_15_18.csv",
          "secondary_vector_2000m_DHS_10_15_18.csv", "secondary_vector_3000m_DHS_10_15_18.csv",
          "secondary_vector_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "secondary_vector_seperated.pdf")

combined <- ggsave.fun2(x, "secondary_vector_combined.pdf")

###temp
x <- list("temp_0m_DHS_10_15_18.csv", "temp_1000m_DHS_10_15_18.csv",
          "temp_2000m_DHS_10_15_18.csv", "temp_3000m_DHS_10_15_18.csv",
          "temp_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "temp_seperated.pdf")

combined <- ggsave.fun2(x, "temp_combined.pdf")

###walking_friction
x <- list("walking_friction_0m_DHS_10_15_18.csv", "walking_friction_1000m_DHS_10_15_18.csv",
          "walking_friction_2000m_DHS_10_15_18.csv", "walking_friction_3000m_DHS_10_15_18.csv",
          "walking_friction_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "walking_friction_seperated.pdf")

combined <- ggsave.fun2(x, "walking_friction_combined.pdf")

###walking_travel
x <- list("walking_travel_0m_DHS_10_15_18.csv", "walking_travel_1000m_DHS_10_15_18.csv",
          "walking_travel_2000m_DHS_10_15_18.csv", "walking_travel_3000m_DHS_10_15_18.csv",
          "walking_travel_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "walking_travel_seperated.pdf")

combined <- ggsave.fun2(x, "walking_travel_combined.pdf")


###precip

x <- list("prec_all_yrs_0m_DHS_10_15_18.csv", "prec_all_yrs_1000m_DHS_10_15_18.csv",
          "prec_all_yrs_2000m_DHS_10_15_18.csv", "prec_all_yrs_3000m_DHS_10_15_18.csv",
          "prec_all_yrs_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "prec_all_yrs_seperated.pdf")

combined <- ggsave.fun2(x, "prec_all_yrs_combined.pdf")


###temp era

x <- list("temp_era_0m_DHS_10_15_18.csv", "temp_era_1000m_DHS_10_15_18.csv",
          "temp_era_2000m_DHS_10_15_18.csv", "temp_era_3000m_DHS_10_15_18.csv",
          "temp_era_4000m_DHS_10_15_18.csv")

separate <- ggsave.fun(x, "temp_era_seperated.pdf")

combined <- ggsave.fun2(x, "temp_era_combined.pdf")
#end


