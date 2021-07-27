
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
Covdir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', "geospatial_covariates")
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'Temp_covereates')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
ResultDir <-file.path(ProjectDir, "results")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
RastDir <- file.path(DataDir, "Raster_files")
# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "Nigeria functions.R"))



# # Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "readtext", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt", "reshape", "ggpubr")


###################################################################################
####Loading data

## ----------------------------------------------------
### Read in PR  data (DHS 2010, 2015, 2018)  
## ----------------------------------------------------
dhs <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL|NGPR71FL|NGPR61FL', read_dta)  #reads in the PR files

dhs_10 <- dhs[[1]] 
dhs_15 <- dhs[[2]] 
dhs_18 <- dhs[[3]]


pfpr_df_18 <- dhs_18 %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6) & hml16 <59 & hv025 ==1)
pfpr_df_18 <-pfpr_df_18[!duplicated(pfpr_df_18$hv001),]

pfpr_df_15 <- dhs_15 %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6) & hml16 <59 & hv025 ==1)
pfpr_df_15 <-pfpr_df_15[!duplicated(pfpr_df_15$hv001),]

pfpr_df_10 <- dhs_10 %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6) & hml16 <59 & hv025 ==1)
pfpr_df_10 <-pfpr_df_10[!duplicated(pfpr_df_10$hv001),]

pfpr_df_18 <- pfpr_df_18[,c("hv001")]
pfpr_df_15 <- pfpr_df_15[,c("hv001")]
pfpr_df_10 <- pfpr_df_10[,c("hv001")]


###################################################################################
# Load pre-extracted pop den data:
#functions 

#fxn for the range for the daata points for 2010 clusters

extr2010.fun <- function(csv, varname){
  filename <- read.csv(file.path(DataIn, csv),
                       header = T, sep = ',') %>% 
    subset(X %in% c(1:239)) %>% rename_at(4,~varname)
  left_join(pfpr_df_10, filename, by = c("hv001"="ID")) %>% 
      subset(select = c(1, 3, 4)) %>% mutate(dhs_year = 2010)
}

extr2015.fun <- function(csv, varname){
  filename <- read.csv(file.path(DataIn, csv),
                       header = T, sep = ',') %>% 
    subset(X %in% c(240:(239+326))) %>% rename_at(5,~varname)
  left_join(pfpr_df_15, filename, by = c("hv001"="ID")) %>% 
    subset(select = c(1, 3, 5)) %>% mutate(dhs_year = 2015)
}

extr2018.fun <- function(csv, varname){
  filename <- read.csv(file.path(DataIn, csv),
                       header = T, sep = ',') %>%
    subset(X %in% c((239+326+1):(239+326+1+1389))) %>% rename_at(6,~varname) 
  left_join(pfpr_df_18, filename, by = c("hv001"="ID")) %>% 
    subset(select = c(1, 3, 6)) %>% mutate(dhs_year = 2018)
}

bind.fun <- function(df1, df2, df3){
  dplyr::bind_rows(df1, df2, df3)
}
#####pop density 0m buffer
pop_density_0m_10 <- extr2010.fun("pop_density_0m_buffer_DHS_10_15_18_30sec.csv", "pop_density_0m")
pop_density_0m_15 <- extr2015.fun("pop_density_0m_buffer_DHS_10_15_18_30sec.csv", "pop_density_0m")
pop_density_0m_18 <- extr2018.fun("pop_density_0m_buffer_DHS_10_15_18_30sec.csv", "pop_density_0m")


pop_density_0m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_0m_10, pop_density_0m_15, pop_density_0m_18)
write.csv(pop_density_0m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_0m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 1000m buffer
pop_density_1000m_10 <- extr2010.fun("pop_density_1000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_1000m")
pop_density_1000m_15 <- extr2015.fun("pop_density_1000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_1000m")
pop_density_1000m_18 <- extr2018.fun("pop_density_1000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_1000m")


pop_density_1000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_1000m_10, pop_density_1000m_15, pop_density_1000m_18)
write.csv(pop_density_1000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_1000m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 2000m buffer
pop_density_2000m_10 <- extr2010.fun("pop_density_2000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_2000m")
pop_density_2000m_15 <- extr2015.fun("pop_density_2000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_2000m")
pop_density_2000m_18 <- extr2018.fun("pop_density_2000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_2000m")


pop_density_2000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_2000m_10, pop_density_2000m_15, pop_density_2000m_18)
write.csv(pop_density_2000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_2000m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 3000m buffer
pop_density_3000m_10 <- extr2010.fun("pop_density_3000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_3000m")
pop_density_3000m_15 <- extr2015.fun("pop_density_3000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_3000m")
pop_density_3000m_18 <- extr2018.fun("pop_density_3000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_3000m")


pop_density_3000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_3000m_10, pop_density_3000m_15, pop_density_3000m_18)
write.csv(pop_density_3000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_3000m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 4000m buffer
pop_density_4000m_10 <- extr2010.fun("pop_density_4000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_4000m")
pop_density_4000m_15 <- extr2015.fun("pop_density_4000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_4000m")
pop_density_4000m_18 <- extr2018.fun("pop_density_4000m_buffer_DHS_10_15_18_30sec.csv", "pop_density_4000m")


pop_density_4000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_4000m_10, pop_density_4000m_15, pop_density_4000m_18)
write.csv(pop_density_4000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_4000m_DHS_10_15_18.csv")),row.names = FALSE)

#____________________________________________________________________________________
#Facebook population density



#####pop density 0m buffer
pop_density_FB_0m_10 <- extr2010.fun("pop_density_FB_0m_buffer_DHS_10_15_18.csv", "pop_density_fb_0m")
pop_density_FB_0m_15 <- extr2015.fun("pop_density_FB_0m_buffer_DHS_10_15_18.csv", "pop_density_fb_0m")
pop_density_FB_0m_18 <- extr2018.fun("pop_density_FB_0m_buffer_DHS_10_15_18.csv", "pop_density_fb_0m")


pop_density_FB_0m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_FB_0m_10, pop_density_FB_0m_15, pop_density_FB_0m_18)
write.csv(pop_density_FB_0m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_FB_0m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 1000m buffer
pop_density_FB_1000m_10 <- extr2010.fun("pop_density_FB_1000m_buffer_DHS_10_15_18.csv", "pop_density_fb_1000m")
pop_density_FB_1000m_15 <- extr2015.fun("pop_density_FB_1000m_buffer_DHS_10_15_18.csv", "pop_density_fb_1000m")
pop_density_FB_1000m_18 <- extr2018.fun("pop_density_FB_1000m_buffer_DHS_10_15_18.csv", "pop_density_fb_1000m")


pop_density_FB_1000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_FB_1000m_10, pop_density_FB_1000m_15, pop_density_FB_1000m_18)
write.csv(pop_density_FB_1000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_FB_1000m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 2000m buffer
pop_density_FB_2000m_10 <- extr2010.fun("pop_density_FB_2000m_buffer_DHS_10_15_18.csv", "pop_density_fb_2000m")
pop_density_FB_2000m_15 <- extr2015.fun("pop_density_FB_2000m_buffer_DHS_10_15_18.csv", "pop_density_fb_2000m")
pop_density_FB_2000m_18 <- extr2018.fun("pop_density_FB_2000m_buffer_DHS_10_15_18.csv", "pop_density_fb_2000m")


pop_density_FB_2000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_FB_2000m_10, pop_density_FB_2000m_15, pop_density_FB_2000m_18)
write.csv(pop_density_FB_2000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_FB_2000m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 3000m buffer
pop_density_FB_3000m_10 <- extr2010.fun("pop_density_FB_3000m_buffer_DHS_10_15_18.csv", "pop_density_fb_3000m")
pop_density_FB_3000m_15 <- extr2015.fun("pop_density_FB_3000m_buffer_DHS_10_15_18.csv", "pop_density_fb_3000m")
pop_density_FB_3000m_18 <- extr2018.fun("pop_density_FB_3000m_buffer_DHS_10_15_18.csv", "pop_density_fb_3000m")


pop_density_FB_3000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_FB_3000m_10, pop_density_FB_3000m_15, pop_density_FB_3000m_18)
write.csv(pop_density_FB_3000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_FB_3000m_DHS_10_15_18.csv")),row.names = FALSE)


#####pop density 4000m buffer
pop_density_FB_4000m_10 <- extr2010.fun("pop_density_FB_4000m_buffer_DHS_10_15_18.csv", "pop_density_fb_4000m")
pop_density_FB_4000m_15 <- extr2015.fun("pop_density_FB_4000m_buffer_DHS_10_15_18.csv", "pop_density_fb_4000m")
pop_density_FB_4000m_18 <- extr2018.fun("pop_density_FB_4000m_buffer_DHS_10_15_18.csv", "pop_density_fb_4000m")


pop_density_FB_4000m_DHS_10_15_18 <- dplyr::bind_rows(pop_density_FB_4000m_10, pop_density_FB_4000m_15, pop_density_FB_4000m_18)
write.csv(pop_density_FB_4000m_DHS_10_15_18, file.path(Covdir, paste0("pop_density_FB_4000m_DHS_10_15_18.csv")),row.names = FALSE)


