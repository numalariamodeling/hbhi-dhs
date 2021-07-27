#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", "randomGLM", "ROCR", "caretEnsemble", "klaR", "naniar", "corrplot", 
       "lmtest", "Deducer", "lattice", "ResourceSelection")

lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)


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
ResultDir <-file.path(ProjectDir, "results")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
RastDir <- file.path(DataDir, "Raster_files")
# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "Nigeria functions.R"))



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


##functions

#CSV data loader

load2010.fun <- function(csv, varname){
        filename <- read.csv(file.path(DataIn, csv),
                             header = T, sep = ',') %>% mutate(dhs_year = 2010) %>% 
                rename_at(4,~varname)
}

load2015.fun <- function(csv, varname){
        filename <- read.csv(file.path(DataIn, csv),
                             header = T, sep = ',') %>% mutate(dhs_year = 2015) %>% 
                rename_at(4,~varname)
}

load2018.fun <- function(csv, varname){
        filename <- read.csv(file.path(DataIn, csv),
                             header = T, sep = ',') %>% mutate(dhs_year = 2018) %>% 
                rename_at(4,~varname)
}


############################## Prevalence of improved housing in ##########################
###############################sub-saharan africa, in 2000 ################################3

######## 0m buffer##########
housing_2000_0m_10 <- load2010.fun("housing_2000_0m_buffer_DHS_10.csv","housing_2000_0m")
housing_2000_0m_15 <- load2015.fun("housing_2000_0m_buffer_DHS_15.csv","housing_2000_0m")
housing_2000_0m_18 <- load2018.fun("housing_2000_0m_buffer_DHS_18.csv","housing_2000_0m")


housing_2000_0m_10 <- left_join(pfpr_df_10, housing_2000_0m_10, by = c("hv001"="ID"))
housing_2000_0m_15 <- left_join(pfpr_df_15, housing_2000_0m_15, by = c("hv001"="ID"))
housing_2000_0m_18 <- left_join(pfpr_df_18, housing_2000_0m_18, by = c("hv001"="ID"))

housing_2000_0m_DHS_10_15_18 <- dplyr::bind_rows(housing_2000_0m_10, housing_2000_0m_15, housing_2000_0m_18)

housing_2000_0m_DHS_10_15_18 <- housing_2000_0m_DHS_10_15_18[,c("hv001", ".id","housing_2000_0m", "dhs_year")]

write.csv(housing_2000_0m_DHS_10_15_18, file.path(Covdir, paste0("housing_2000_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

housing_2000_1000m_10 <- load2010.fun("housing_2000_1000m_buffer_DHS_10.csv","housing_2000_1000m")
housing_2000_1000m_15 <- load2015.fun("housing_2000_1000m_buffer_DHS_15.csv","housing_2000_1000m")
housing_2000_1000m_18 <- load2018.fun("housing_2000_1000m_buffer_DHS_18.csv","housing_2000_1000m")


housing_2000_1000m_10 <- left_join(pfpr_df_10, housing_2000_1000m_10, by = c("hv001"="ID"))
housing_2000_1000m_15 <- left_join(pfpr_df_15, housing_2000_1000m_15, by = c("hv001"="ID"))
housing_2000_1000m_18 <- left_join(pfpr_df_18, housing_2000_1000m_18, by = c("hv001"="ID"))

housing_2000_1000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2000_1000m_10, housing_2000_1000m_15, housing_2000_1000m_18)

housing_2000_1000m_DHS_10_15_18 <- housing_2000_1000m_DHS_10_15_18[,c("hv001", ".id","housing_2000_1000m", "dhs_year")]

write.csv(housing_2000_1000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2000_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


housing_2000_2000m_10 <- load2010.fun("housing_2000_2000m_buffer_DHS_10.csv","housing_2000_2000m")
housing_2000_2000m_15 <- load2015.fun("housing_2000_2000m_buffer_DHS_15.csv","housing_2000_2000m")
housing_2000_2000m_18 <- load2018.fun("housing_2000_2000m_buffer_DHS_18.csv","housing_2000_2000m")


housing_2000_2000m_10 <- left_join(pfpr_df_10, housing_2000_2000m_10, by = c("hv001"="ID"))
housing_2000_2000m_15 <- left_join(pfpr_df_15, housing_2000_2000m_15, by = c("hv001"="ID"))
housing_2000_2000m_18 <- left_join(pfpr_df_18, housing_2000_2000m_18, by = c("hv001"="ID"))

housing_2000_2000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2000_2000m_10, housing_2000_2000m_15, housing_2000_2000m_18)

housing_2000_2000m_DHS_10_15_18 <- housing_2000_2000m_DHS_10_15_18[,c("hv001", ".id","housing_2000_2000m", "dhs_year")]

write.csv(housing_2000_2000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2000_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



housing_2000_3000m_10 <- load2010.fun("housing_2000_3000m_buffer_DHS_10.csv","housing_2000_3000m")
housing_2000_3000m_15 <- load2015.fun("housing_2000_3000m_buffer_DHS_15.csv","housing_2000_3000m")
housing_2000_3000m_18 <- load2018.fun("housing_2000_3000m_buffer_DHS_18.csv","housing_2000_3000m")


housing_2000_3000m_10 <- left_join(pfpr_df_10, housing_2000_3000m_10, by = c("hv001"="ID"))
housing_2000_3000m_15 <- left_join(pfpr_df_15, housing_2000_3000m_15, by = c("hv001"="ID"))
housing_2000_3000m_18 <- left_join(pfpr_df_18, housing_2000_3000m_18, by = c("hv001"="ID"))

housing_2000_3000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2000_3000m_10, housing_2000_3000m_15, housing_2000_3000m_18)

housing_2000_3000m_DHS_10_15_18 <- housing_2000_3000m_DHS_10_15_18[,c("hv001", ".id","housing_2000_3000m", "dhs_year")]

write.csv(housing_2000_3000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2000_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


housing_2000_4000m_10 <- load2010.fun("housing_2000_4000m_buffer_DHS_10.csv","housing_2000_4000m")
housing_2000_4000m_15 <- load2015.fun("housing_2000_4000m_buffer_DHS_15.csv","housing_2000_4000m")
housing_2000_4000m_18 <- load2018.fun("housing_2000_4000m_buffer_DHS_18.csv","housing_2000_4000m")


housing_2000_4000m_10 <- left_join(pfpr_df_10, housing_2000_4000m_10, by = c("hv001"="ID"))
housing_2000_4000m_15 <- left_join(pfpr_df_15, housing_2000_4000m_15, by = c("hv001"="ID"))
housing_2000_4000m_18 <- left_join(pfpr_df_18, housing_2000_4000m_18, by = c("hv001"="ID"))

housing_2000_4000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2000_4000m_10, housing_2000_4000m_15, housing_2000_4000m_18)

housing_2000_4000m_DHS_10_15_18 <- housing_2000_4000m_DHS_10_15_18[,c("hv001", ".id","housing_2000_4000m", "dhs_year")]

write.csv(housing_2000_4000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2000_4000m_DHS_10_15_18.csv")),row.names = FALSE)



############################## Prevalence of improved housing in ##########################
###############################sub-saharan africa, in 2015 ################################3

######## 0m buffer##########
housing_2015_0m_10 <- load2010.fun("housing_0m_buffer_DHS_10.csv","housing_2015_0m")
housing_2015_0m_15 <- load2015.fun("housing_0m_buffer_DHS_15.csv","housing_2015_0m")
housing_2015_0m_18 <- load2018.fun("housing_0m_buffer_DHS_18.csv","housing_2015_0m")


housing_2015_0m_10 <- left_join(pfpr_df_10, housing_2015_0m_10, by = c("hv001"="ID"))
housing_2015_0m_15 <- left_join(pfpr_df_15, housing_2015_0m_15, by = c("hv001"="ID"))
housing_2015_0m_18 <- left_join(pfpr_df_18, housing_2015_0m_18, by = c("hv001"="ID"))

housing_2015_0m_DHS_10_15_18 <- dplyr::bind_rows(housing_2015_0m_10, housing_2015_0m_15, housing_2015_0m_18)

housing_2015_0m_DHS_10_15_18 <- housing_2015_0m_DHS_10_15_18[,c("hv001", ".id","housing_2015_0m", "dhs_year")]

write.csv(housing_2015_0m_DHS_10_15_18, file.path(Covdir, paste0("housing_2015_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

housing_2015_1000m_10 <- load2010.fun("housing_1000m_buffer_DHS_10.csv","housing_2015_1000m")
housing_2015_1000m_15 <- load2015.fun("housing_1000m_buffer_DHS_15.csv","housing_2015_1000m")
housing_2015_1000m_18 <- load2018.fun("housing_1000m_buffer_DHS_18.csv","housing_2015_1000m")


housing_2015_1000m_10 <- left_join(pfpr_df_10, housing_2015_1000m_10, by = c("hv001"="ID"))
housing_2015_1000m_15 <- left_join(pfpr_df_15, housing_2015_1000m_15, by = c("hv001"="ID"))
housing_2015_1000m_18 <- left_join(pfpr_df_18, housing_2015_1000m_18, by = c("hv001"="ID"))

housing_2015_1000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2015_1000m_10, housing_2015_1000m_15, housing_2015_1000m_18)

housing_2015_1000m_DHS_10_15_18 <- housing_2015_1000m_DHS_10_15_18[,c("hv001", ".id","housing_2015_1000m", "dhs_year")]

write.csv(housing_2015_1000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2015_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


housing_2015_2000m_10 <- load2010.fun("housing_2000m_buffer_DHS_10.csv","housing_2015_2000m")
housing_2015_2000m_15 <- load2015.fun("housing_2000m_buffer_DHS_15.csv","housing_2015_2000m")
housing_2015_2000m_18 <- load2018.fun("housing_2000m_buffer_DHS_18.csv","housing_2015_2000m")


housing_2015_2000m_10 <- left_join(pfpr_df_10, housing_2015_2000m_10, by = c("hv001"="ID"))
housing_2015_2000m_15 <- left_join(pfpr_df_15, housing_2015_2000m_15, by = c("hv001"="ID"))
housing_2015_2000m_18 <- left_join(pfpr_df_18, housing_2015_2000m_18, by = c("hv001"="ID"))

housing_2015_2000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2015_2000m_10, housing_2015_2000m_15, housing_2015_2000m_18)

housing_2015_2000m_DHS_10_15_18 <- housing_2015_2000m_DHS_10_15_18[,c("hv001", ".id","housing_2015_2000m", "dhs_year")]

write.csv(housing_2015_2000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2015_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



housing_2015_3000m_10 <- load2010.fun("housing_3000m_buffer_DHS_10.csv","housing_2015_3000m")
housing_2015_3000m_15 <- load2015.fun("housing_3000m_buffer_DHS_15.csv","housing_2015_3000m")
housing_2015_3000m_18 <- load2018.fun("housing_3000m_buffer_DHS_18.csv","housing_2015_3000m")


housing_2015_3000m_10 <- left_join(pfpr_df_10, housing_2015_3000m_10, by = c("hv001"="ID"))
housing_2015_3000m_15 <- left_join(pfpr_df_15, housing_2015_3000m_15, by = c("hv001"="ID"))
housing_2015_3000m_18 <- left_join(pfpr_df_18, housing_2015_3000m_18, by = c("hv001"="ID"))

housing_2015_3000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2015_3000m_10, housing_2015_3000m_15, housing_2015_3000m_18)

housing_2015_3000m_DHS_10_15_18 <- housing_2015_3000m_DHS_10_15_18[,c("hv001", ".id","housing_2015_3000m", "dhs_year")]

write.csv(housing_2015_3000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2015_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


housing_2015_4000m_10 <- load2010.fun("housing_4000m_buffer_DHS_10.csv","housing_2015_4000m")
housing_2015_4000m_15 <- load2015.fun("housing_4000m_buffer_DHS_15.csv","housing_2015_4000m")
housing_2015_4000m_18 <- load2018.fun("housing_4000m_buffer_DHS_18.csv","housing_2015_4000m")


housing_2015_4000m_10 <- left_join(pfpr_df_10, housing_2015_4000m_10, by = c("hv001"="ID"))
housing_2015_4000m_15 <- left_join(pfpr_df_15, housing_2015_4000m_15, by = c("hv001"="ID"))
housing_2015_4000m_18 <- left_join(pfpr_df_18, housing_2015_4000m_18, by = c("hv001"="ID"))

housing_2015_4000m_DHS_10_15_18 <- dplyr::bind_rows(housing_2015_4000m_10, housing_2015_4000m_15, housing_2015_4000m_18)

housing_2015_4000m_DHS_10_15_18 <- housing_2015_4000m_DHS_10_15_18[,c("hv001", ".id","housing_2015_4000m", "dhs_year")]

write.csv(housing_2015_4000m_DHS_10_15_18, file.path(Covdir, paste0("housing_2015_4000m_DHS_10_15_18.csv")),row.names = FALSE)



############################## Dominant malaria vector species globally, 2010##########################
################################################################### ################################

######## 0m buffer##########
dominant_vector_0m_10 <- load2010.fun("dominant_vector_0m_buffer_DHS_10.csv", "dominant_vector_0m")
dominant_vector_0m_15 <- load2015.fun("dominant_vector_0m_buffer_DHS_15.csv", "dominant_vector_0m")
dominant_vector_0m_18 <- load2018.fun("dominant_vector_0m_buffer_DHS_18.csv", "dominant_vector_0m")


dominant_vector_0m_10 <- left_join(pfpr_df_10, dominant_vector_0m_10, by = c("hv001"="ID"))
dominant_vector_0m_15 <- left_join(pfpr_df_15, dominant_vector_0m_15, by = c("hv001"="ID"))
dominant_vector_0m_18 <- left_join(pfpr_df_18, dominant_vector_0m_18, by = c("hv001"="ID"))

dominant_vector_0m_DHS_10_15_18 <- dplyr::bind_rows(dominant_vector_0m_10, dominant_vector_0m_15, dominant_vector_0m_18)

dominant_vector_0m_DHS_10_15_18 <- subset(dominant_vector_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(dominant_vector_0m_DHS_10_15_18, file.path(Covdir, paste0("dominant_vector_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

dominant_vector_1000m_10 <- load2010.fun("dominant_vector_1000m_buffer_DHS_10.csv", "dominant_vector_1000m")
dominant_vector_1000m_15 <- load2015.fun("dominant_vector_1000m_buffer_DHS_15.csv", "dominant_vector_1000m")
dominant_vector_1000m_18 <- load2018.fun("dominant_vector_1000m_buffer_DHS_18.csv", "dominant_vector_1000m")


dominant_vector_1000m_10 <- left_join(pfpr_df_10, dominant_vector_1000m_10, by = c("hv001"="ID"))
dominant_vector_1000m_15 <- left_join(pfpr_df_15, dominant_vector_1000m_15, by = c("hv001"="ID"))
dominant_vector_1000m_18 <- left_join(pfpr_df_18, dominant_vector_1000m_18, by = c("hv001"="ID"))

dominant_vector_1000m_DHS_10_15_18 <- dplyr::bind_rows(dominant_vector_1000m_10, dominant_vector_1000m_15, dominant_vector_1000m_18)

dominant_vector_1000m_DHS_10_15_18 <- subset(dominant_vector_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(dominant_vector_1000m_DHS_10_15_18, file.path(Covdir, paste0("dominant_vector_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


dominant_vector_2000m_10 <- load2010.fun("dominant_vector_2000m_buffer_DHS_10.csv", "dominant_vector_2000m")
dominant_vector_2000m_15 <- load2015.fun("dominant_vector_2000m_buffer_DHS_15.csv", "dominant_vector_2000m")
dominant_vector_2000m_18 <- load2018.fun("dominant_vector_2000m_buffer_DHS_18.csv", "dominant_vector_2000m")


dominant_vector_2000m_10 <- left_join(pfpr_df_10, dominant_vector_2000m_10, by = c("hv001"="ID"))
dominant_vector_2000m_15 <- left_join(pfpr_df_15, dominant_vector_2000m_15, by = c("hv001"="ID"))
dominant_vector_2000m_18 <- left_join(pfpr_df_18, dominant_vector_2000m_18, by = c("hv001"="ID"))

dominant_vector_2000m_DHS_10_15_18 <- dplyr::bind_rows(dominant_vector_2000m_10, dominant_vector_2000m_15, dominant_vector_2000m_18)

dominant_vector_2000m_DHS_10_15_18 <- subset(dominant_vector_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(dominant_vector_2000m_DHS_10_15_18, file.path(Covdir, paste0("dominant_vector_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



dominant_vector_3000m_10 <- load2010.fun("dominant_vector_3000m_buffer_DHS_10.csv", "dominant_vector_3000m")
dominant_vector_3000m_15 <- load2015.fun("dominant_vector_3000m_buffer_DHS_15.csv", "dominant_vector_3000m")
dominant_vector_3000m_18 <- load2018.fun("dominant_vector_3000m_buffer_DHS_18.csv", "dominant_vector_3000m")


dominant_vector_3000m_10 <- left_join(pfpr_df_10, dominant_vector_3000m_10, by = c("hv001"="ID"))
dominant_vector_3000m_15 <- left_join(pfpr_df_15, dominant_vector_3000m_15, by = c("hv001"="ID"))
dominant_vector_3000m_18 <- left_join(pfpr_df_18, dominant_vector_3000m_18, by = c("hv001"="ID"))

dominant_vector_3000m_DHS_10_15_18 <- dplyr::bind_rows(dominant_vector_3000m_10, dominant_vector_3000m_15, dominant_vector_3000m_18)

dominant_vector_3000m_DHS_10_15_18 <- subset(dominant_vector_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(dominant_vector_3000m_DHS_10_15_18, file.path(Covdir, paste0("dominant_vector_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


dominant_vector_4000m_10 <- load2010.fun("dominant_vector_4000m_buffer_DHS_10.csv", "dominant_vector_4000m")
dominant_vector_4000m_15 <- load2015.fun("dominant_vector_4000m_buffer_DHS_15.csv", "dominant_vector_4000m")
dominant_vector_4000m_18 <- load2018.fun("dominant_vector_4000m_buffer_DHS_18.csv", "dominant_vector_4000m")


dominant_vector_4000m_10 <- left_join(pfpr_df_10, dominant_vector_4000m_10, by = c("hv001"="ID"))
dominant_vector_4000m_15 <- left_join(pfpr_df_15, dominant_vector_4000m_15, by = c("hv001"="ID"))
dominant_vector_4000m_18 <- left_join(pfpr_df_18, dominant_vector_4000m_18, by = c("hv001"="ID"))

dominant_vector_4000m_DHS_10_15_18 <- dplyr::bind_rows(dominant_vector_4000m_10, dominant_vector_4000m_15, dominant_vector_4000m_18)

dominant_vector_4000m_DHS_10_15_18 <- subset(dominant_vector_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(dominant_vector_4000m_DHS_10_15_18, file.path(Covdir, paste0("dominant_vector_4000m_DHS_10_15_18.csv")),row.names = FALSE)



############################## Secondary malaria vector species globally, 2010##########################
################################################################### ################################

######## 0m buffer##########
secondary_vector_0m_10 <- load2010.fun("secondary_vector_0m_buffer_DHS_10.csv", "secondary_vector_0m")
secondary_vector_0m_15 <- load2015.fun("secondary_vector_0m_buffer_DHS_15.csv", "secondary_vector_0m")
secondary_vector_0m_18 <- load2018.fun("secondary_vector_0m_buffer_DHS_18.csv", "secondary_vector_0m")


secondary_vector_0m_10 <- left_join(pfpr_df_10, secondary_vector_0m_10, by = c("hv001"="ID"))
secondary_vector_0m_15 <- left_join(pfpr_df_15, secondary_vector_0m_15, by = c("hv001"="ID"))
secondary_vector_0m_18 <- left_join(pfpr_df_18, secondary_vector_0m_18, by = c("hv001"="ID"))

secondary_vector_0m_DHS_10_15_18 <- dplyr::bind_rows(secondary_vector_0m_10, secondary_vector_0m_15, secondary_vector_0m_18)

secondary_vector_0m_DHS_10_15_18 <- subset(secondary_vector_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(secondary_vector_0m_DHS_10_15_18, file.path(Covdir, paste0("secondary_vector_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

secondary_vector_1000m_10 <- load2010.fun("secondary_vector_1000m_buffer_DHS_10.csv", "secondary_vector_1000m")
secondary_vector_1000m_15 <- load2015.fun("secondary_vector_1000m_buffer_DHS_15.csv", "secondary_vector_1000m")
secondary_vector_1000m_18 <- load2018.fun("secondary_vector_1000m_buffer_DHS_18.csv", "secondary_vector_1000m")


secondary_vector_1000m_10 <- left_join(pfpr_df_10, secondary_vector_1000m_10, by = c("hv001"="ID"))
secondary_vector_1000m_15 <- left_join(pfpr_df_15, secondary_vector_1000m_15, by = c("hv001"="ID"))
secondary_vector_1000m_18 <- left_join(pfpr_df_18, secondary_vector_1000m_18, by = c("hv001"="ID"))

secondary_vector_1000m_DHS_10_15_18 <- dplyr::bind_rows(secondary_vector_1000m_10, secondary_vector_1000m_15, secondary_vector_1000m_18)

secondary_vector_1000m_DHS_10_15_18 <- subset(secondary_vector_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(secondary_vector_1000m_DHS_10_15_18, file.path(Covdir, paste0("secondary_vector_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


secondary_vector_2000m_10 <- load2010.fun("secondary_vector_2000m_buffer_DHS_10.csv", "secondary_vector_2000m")
secondary_vector_2000m_15 <- load2015.fun("secondary_vector_2000m_buffer_DHS_15.csv", "secondary_vector_2000m")
secondary_vector_2000m_18 <- load2018.fun("secondary_vector_2000m_buffer_DHS_18.csv", "secondary_vector_2000m")


secondary_vector_2000m_10 <- left_join(pfpr_df_10, secondary_vector_2000m_10, by = c("hv001"="ID"))
secondary_vector_2000m_15 <- left_join(pfpr_df_15, secondary_vector_2000m_15, by = c("hv001"="ID"))
secondary_vector_2000m_18 <- left_join(pfpr_df_18, secondary_vector_2000m_18, by = c("hv001"="ID"))

secondary_vector_2000m_DHS_10_15_18 <- dplyr::bind_rows(secondary_vector_2000m_10, secondary_vector_2000m_15, secondary_vector_2000m_18)

secondary_vector_2000m_DHS_10_15_18 <- subset(secondary_vector_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(secondary_vector_2000m_DHS_10_15_18, file.path(Covdir, paste0("secondary_vector_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



secondary_vector_3000m_10 <- load2010.fun("secondary_vector_3000m_buffer_DHS_10.csv", "secondary_vector_2000m")
secondary_vector_3000m_15 <- load2015.fun("secondary_vector_3000m_buffer_DHS_15.csv", "secondary_vector_2000m")
secondary_vector_3000m_18 <- load2018.fun("secondary_vector_3000m_buffer_DHS_18.csv", "secondary_vector_2000m")


secondary_vector_3000m_10 <- left_join(pfpr_df_10, secondary_vector_3000m_10, by = c("hv001"="ID"))
secondary_vector_3000m_15 <- left_join(pfpr_df_15, secondary_vector_3000m_15, by = c("hv001"="ID"))
secondary_vector_3000m_18 <- left_join(pfpr_df_18, secondary_vector_3000m_18, by = c("hv001"="ID"))

secondary_vector_3000m_DHS_10_15_18 <- dplyr::bind_rows(secondary_vector_3000m_10, secondary_vector_3000m_15, secondary_vector_3000m_18)

secondary_vector_3000m_DHS_10_15_18 <- subset(secondary_vector_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(secondary_vector_3000m_DHS_10_15_18, file.path(Covdir, paste0("secondary_vector_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


secondary_vector_4000m_10 <- load2010.fun("secondary_vector_4000m_buffer_DHS_10.csv", "secondary_vector_4000m")
secondary_vector_4000m_15 <- load2015.fun("secondary_vector_4000m_buffer_DHS_15.csv", "secondary_vector_4000m")
secondary_vector_4000m_18 <- load2018.fun("secondary_vector_4000m_buffer_DHS_18.csv", "secondary_vector_4000m")


secondary_vector_4000m_10 <- left_join(pfpr_df_10, secondary_vector_4000m_10, by = c("hv001"="ID"))
secondary_vector_4000m_15 <- left_join(pfpr_df_15, secondary_vector_4000m_15, by = c("hv001"="ID"))
secondary_vector_4000m_18 <- left_join(pfpr_df_18, secondary_vector_4000m_18, by = c("hv001"="ID"))

secondary_vector_4000m_DHS_10_15_18 <- dplyr::bind_rows(secondary_vector_4000m_10, secondary_vector_4000m_15, secondary_vector_4000m_18)

secondary_vector_4000m_DHS_10_15_18 <- subset(secondary_vector_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(secondary_vector_4000m_DHS_10_15_18, file.path(Covdir, paste0("secondary_vector_4000m_DHS_10_15_18.csv")),row.names = FALSE)


############################## A global map of travel time to cities to assess#########################
##############inequalities in accessibility in 2015; predicted travel####################
################################time (minutes) to nearest city##################
 
######## 0m buffer##########
access_to_cities_0m_10 <- load2010.fun("access_to_cities_0m_buffer_DHS_10.csv", "minutes_to_city_0m")
access_to_cities_0m_15 <- load2015.fun("access_to_cities_0m_buffer_DHS_15.csv", "minutes_to_city_0m")
access_to_cities_0m_18 <- load2018.fun("access_to_cities_0m_buffer_DHS_18.csv", "minutes_to_city_0m")


access_to_cities_0m_10 <- left_join(pfpr_df_10, access_to_cities_0m_10, by = c("hv001"="ID"))
access_to_cities_0m_15 <- left_join(pfpr_df_15, access_to_cities_0m_15, by = c("hv001"="ID"))
access_to_cities_0m_18 <- left_join(pfpr_df_18, access_to_cities_0m_18, by = c("hv001"="ID"))

access_to_cities_0m_DHS_10_15_18 <- dplyr::bind_rows(access_to_cities_0m_10, access_to_cities_0m_15, access_to_cities_0m_18)

access_to_cities_0m_DHS_10_15_18 <- subset(access_to_cities_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(access_to_cities_0m_DHS_10_15_18, file.path(Covdir, paste0("access_to_cities_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

access_to_cities_1000m_10 <- load2010.fun("access_to_cities_1000m_buffer_DHS_10.csv", "minutes_to_city_1000m")
access_to_cities_1000m_15 <- load2015.fun("access_to_cities_1000m_buffer_DHS_15.csv", "minutes_to_city_1000m")
access_to_cities_1000m_18 <- load2018.fun("access_to_cities_1000m_buffer_DHS_18.csv", "minutes_to_city_1000m")


access_to_cities_1000m_10 <- left_join(pfpr_df_10, access_to_cities_1000m_10, by = c("hv001"="ID"))
access_to_cities_1000m_15 <- left_join(pfpr_df_15, access_to_cities_1000m_15, by = c("hv001"="ID"))
access_to_cities_1000m_18 <- left_join(pfpr_df_18, access_to_cities_1000m_18, by = c("hv001"="ID"))

access_to_cities_1000m_DHS_10_15_18 <- dplyr::bind_rows(access_to_cities_1000m_10, access_to_cities_1000m_15, access_to_cities_1000m_18)

access_to_cities_1000m_DHS_10_15_18 <- subset(access_to_cities_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(access_to_cities_1000m_DHS_10_15_18, file.path(Covdir, paste0("access_to_cities_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


access_to_cities_2000m_10 <- load2010.fun("access_to_cities_2000m_buffer_DHS_10.csv", "minutes_to_city_2000m")
access_to_cities_2000m_15 <- load2015.fun("access_to_cities_2000m_buffer_DHS_15.csv", "minutes_to_city_2000m")
access_to_cities_2000m_18 <- load2018.fun("access_to_cities_2000m_buffer_DHS_18.csv", "minutes_to_city_2000m")


access_to_cities_2000m_10 <- left_join(pfpr_df_10, access_to_cities_2000m_10, by = c("hv001"="ID"))
access_to_cities_2000m_15 <- left_join(pfpr_df_15, access_to_cities_2000m_15, by = c("hv001"="ID"))
access_to_cities_2000m_18 <- left_join(pfpr_df_18, access_to_cities_2000m_18, by = c("hv001"="ID"))

access_to_cities_2000m_DHS_10_15_18 <- dplyr::bind_rows(access_to_cities_2000m_10, access_to_cities_2000m_15, access_to_cities_2000m_18)

access_to_cities_2000m_DHS_10_15_18 <- subset(access_to_cities_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(access_to_cities_2000m_DHS_10_15_18, file.path(Covdir, paste0("access_to_cities_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



access_to_cities_3000m_10 <- load2010.fun("access_to_cities_3000m_buffer_DHS_10.csv", "minutes_to_city_3000m")
access_to_cities_3000m_15 <- load2015.fun("access_to_cities_3000m_buffer_DHS_15.csv", "minutes_to_city_3000m")
access_to_cities_3000m_18 <- load2018.fun("access_to_cities_3000m_buffer_DHS_18.csv", "minutes_to_city_3000m")


access_to_cities_3000m_10 <- left_join(pfpr_df_10, access_to_cities_3000m_10, by = c("hv001"="ID"))
access_to_cities_3000m_15 <- left_join(pfpr_df_15, access_to_cities_3000m_15, by = c("hv001"="ID"))
access_to_cities_3000m_18 <- left_join(pfpr_df_18, access_to_cities_3000m_18, by = c("hv001"="ID"))

access_to_cities_3000m_DHS_10_15_18 <- dplyr::bind_rows(access_to_cities_3000m_10, access_to_cities_3000m_15, access_to_cities_3000m_18)

access_to_cities_3000m_DHS_10_15_18 <- subset(access_to_cities_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(access_to_cities_3000m_DHS_10_15_18, file.path(Covdir, paste0("access_to_cities_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


access_to_cities_4000m_10 <- load2010.fun("access_to_cities_4000m_buffer_DHS_10.csv", "minutes_to_city_4000m")
access_to_cities_4000m_15 <- load2015.fun("access_to_cities_4000m_buffer_DHS_15.csv", "minutes_to_city_4000m")
access_to_cities_4000m_18 <- load2018.fun("access_to_cities_4000m_buffer_DHS_18.csv", "minutes_to_city_4000m")


access_to_cities_4000m_10 <- left_join(pfpr_df_10, access_to_cities_4000m_10, by = c("hv001"="ID"))
access_to_cities_4000m_15 <- left_join(pfpr_df_15, access_to_cities_4000m_15, by = c("hv001"="ID"))
access_to_cities_4000m_18 <- left_join(pfpr_df_18, access_to_cities_4000m_18, by = c("hv001"="ID"))

access_to_cities_4000m_DHS_10_15_18 <- dplyr::bind_rows(access_to_cities_4000m_10, access_to_cities_4000m_15, access_to_cities_4000m_18)

access_to_cities_4000m_DHS_10_15_18 <- subset(access_to_cities_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(access_to_cities_4000m_DHS_10_15_18, file.path(Covdir, paste0("access_to_cities_4000m_DHS_10_15_18.csv")),row.names = FALSE)

############################## 2015_friction_surface_v1_Decompressed_NGA#########################
################################################################################################


######## friction_decompressed 0m buffer##########

friction_decompressed_0m_10 <- load2010.fun("friction_decompressed_0m_buffer_DHS_10.csv", "minutes_travel_metre_2015_0m")
friction_decompressed_0m_15 <- load2015.fun("friction_decompressed_0m_buffer_DHS_15.csv", "minutes_travel_metre_2015_0m")
friction_decompressed_0m_18 <- load2018.fun("friction_decompressed_0m_buffer_DHS_18.csv", "minutes_travel_metre_2015_0m")


friction_decompressed_0m_10 <- left_join(pfpr_df_10, friction_decompressed_0m_10, by = c("hv001"="ID"))
friction_decompressed_0m_15 <- left_join(pfpr_df_15, friction_decompressed_0m_15, by = c("hv001"="ID"))
friction_decompressed_0m_18 <- left_join(pfpr_df_18, friction_decompressed_0m_18, by = c("hv001"="ID"))

friction_decompressed_0m_DHS_10_15_18 <- dplyr::bind_rows(friction_decompressed_0m_10, friction_decompressed_0m_15, friction_decompressed_0m_18)

friction_decompressed_0m_DHS_10_15_18 <- subset(friction_decompressed_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(friction_decompressed_0m_DHS_10_15_18, file.path(Covdir, paste0("friction_decompressed_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

friction_decompressed_1000m_10 <- load2010.fun("friction_decompressed_1000m_buffer_DHS_10.csv", "minutes_travel_metre_2015_1000m")
friction_decompressed_1000m_15 <- load2015.fun("friction_decompressed_1000m_buffer_DHS_15.csv", "minutes_travel_metre_2015_1000m")
friction_decompressed_1000m_18 <- load2018.fun("friction_decompressed_1000m_buffer_DHS_18.csv", "minutes_travel_metre_2015_1000m")


friction_decompressed_1000m_10 <- left_join(pfpr_df_10, friction_decompressed_1000m_10, by = c("hv001"="ID"))
friction_decompressed_1000m_15 <- left_join(pfpr_df_15, friction_decompressed_1000m_15, by = c("hv001"="ID"))
friction_decompressed_1000m_18 <- left_join(pfpr_df_18, friction_decompressed_1000m_18, by = c("hv001"="ID"))

friction_decompressed_1000m_DHS_10_15_18 <- dplyr::bind_rows(friction_decompressed_1000m_10, friction_decompressed_1000m_15, friction_decompressed_1000m_18)

friction_decompressed_1000m_DHS_10_15_18 <- subset(friction_decompressed_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(friction_decompressed_1000m_DHS_10_15_18, file.path(Covdir, paste0("friction_decompressed_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


friction_decompressed_2000m_10 <- load2010.fun("friction_decompressed_2000m_buffer_DHS_10.csv", "minutes_travel_metre_2015_2000m")
friction_decompressed_2000m_15 <- load2015.fun("friction_decompressed_2000m_buffer_DHS_15.csv", "minutes_travel_metre_2015_2000m")
friction_decompressed_2000m_18 <- load2018.fun("friction_decompressed_2000m_buffer_DHS_18.csv", "minutes_travel_metre_2015_2000m")


friction_decompressed_2000m_10 <- left_join(pfpr_df_10, friction_decompressed_2000m_10, by = c("hv001"="ID"))
friction_decompressed_2000m_15 <- left_join(pfpr_df_15, friction_decompressed_2000m_15, by = c("hv001"="ID"))
friction_decompressed_2000m_18 <- left_join(pfpr_df_18, friction_decompressed_2000m_18, by = c("hv001"="ID"))

friction_decompressed_2000m_DHS_10_15_18 <- dplyr::bind_rows(friction_decompressed_2000m_10, friction_decompressed_2000m_15, friction_decompressed_2000m_18)

friction_decompressed_2000m_DHS_10_15_18 <- subset(friction_decompressed_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(friction_decompressed_2000m_DHS_10_15_18, file.path(Covdir, paste0("friction_decompressed_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



friction_decompressed_3000m_10 <- load2010.fun("friction_decompressed_3000m_buffer_DHS_10.csv", "minutes_travel_metre_2015_3000m")
friction_decompressed_3000m_15 <- load2015.fun("friction_decompressed_3000m_buffer_DHS_15.csv", "minutes_travel_metre_2015_3000m")
friction_decompressed_3000m_18 <- load2018.fun("friction_decompressed_3000m_buffer_DHS_18.csv", "minutes_travel_metre_2015_3000m")


friction_decompressed_3000m_10 <- left_join(pfpr_df_10, friction_decompressed_3000m_10, by = c("hv001"="ID"))
friction_decompressed_3000m_15 <- left_join(pfpr_df_15, friction_decompressed_3000m_15, by = c("hv001"="ID"))
friction_decompressed_3000m_18 <- left_join(pfpr_df_18, friction_decompressed_3000m_18, by = c("hv001"="ID"))

friction_decompressed_3000m_DHS_10_15_18 <- dplyr::bind_rows(friction_decompressed_3000m_10, friction_decompressed_3000m_15, friction_decompressed_3000m_18)

friction_decompressed_3000m_DHS_10_15_18 <- subset(friction_decompressed_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(friction_decompressed_3000m_DHS_10_15_18, file.path(Covdir, paste0("friction_decompressed_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


friction_decompressed_4000m_10 <- load2010.fun("friction_decompressed_4000m_buffer_DHS_10.csv", "minutes_travel_metre_2015_4000m")
friction_decompressed_4000m_15 <- load2015.fun("friction_decompressed_4000m_buffer_DHS_15.csv", "minutes_travel_metre_2015_4000m")
friction_decompressed_4000m_18 <- load2018.fun("friction_decompressed_4000m_buffer_DHS_18.csv", "minutes_travel_metre_2015_4000m")


friction_decompressed_4000m_10 <- left_join(pfpr_df_10, friction_decompressed_4000m_10, by = c("hv001"="ID"))
friction_decompressed_4000m_15 <- left_join(pfpr_df_15, friction_decompressed_4000m_15, by = c("hv001"="ID"))
friction_decompressed_4000m_18 <- left_join(pfpr_df_18, friction_decompressed_4000m_18, by = c("hv001"="ID"))

friction_decompressed_4000m_DHS_10_15_18 <- dplyr::bind_rows(friction_decompressed_4000m_10, friction_decompressed_4000m_15, friction_decompressed_4000m_18)

friction_decompressed_4000m_DHS_10_15_18 <- subset(friction_decompressed_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(friction_decompressed_4000m_DHS_10_15_18, file.path(Covdir, paste0("friction_decompressed_4000m_DHS_10_15_18.csv")),row.names = FALSE)

############################## 2020_motorized_friction_surface_NGA#########################
################################################################################################


######## motorized_friction 0m buffer##########

motorized_friction_0m_10 <- load2010.fun("motorized_friction_0m_buffer_DHS_10.csv", "minutes_travel_metre_2019_0m")
motorized_friction_0m_15 <- load2015.fun("motorized_friction_0m_buffer_DHS_15.csv", "minutes_travel_metre_2019_0m")
motorized_friction_0m_18 <- load2018.fun("motorized_friction_0m_buffer_DHS_18.csv", "minutes_travel_metre_2019_0m")


motorized_friction_0m_10 <- left_join(pfpr_df_10, motorized_friction_0m_10, by = c("hv001"="ID"))
motorized_friction_0m_15 <- left_join(pfpr_df_15, motorized_friction_0m_15, by = c("hv001"="ID"))
motorized_friction_0m_18 <- left_join(pfpr_df_18, motorized_friction_0m_18, by = c("hv001"="ID"))

motorized_friction_0m_DHS_10_15_18 <- dplyr::bind_rows(motorized_friction_0m_10, motorized_friction_0m_15, motorized_friction_0m_18)

motorized_friction_0m_DHS_10_15_18 <- subset(motorized_friction_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_friction_0m_DHS_10_15_18, file.path(Covdir, paste0("motorized_friction_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

motorized_friction_1000m_10 <- load2010.fun("motorized_friction_1000m_buffer_DHS_10.csv", "minutes_travel_metre_2019_1000m")
motorized_friction_1000m_15 <- load2015.fun("motorized_friction_1000m_buffer_DHS_15.csv", "minutes_travel_metre_2019_1000m")
motorized_friction_1000m_18 <- load2018.fun("motorized_friction_1000m_buffer_DHS_18.csv", "minutes_travel_metre_2019_1000m")


motorized_friction_1000m_10 <- left_join(pfpr_df_10, motorized_friction_1000m_10, by = c("hv001"="ID"))
motorized_friction_1000m_15 <- left_join(pfpr_df_15, motorized_friction_1000m_15, by = c("hv001"="ID"))
motorized_friction_1000m_18 <- left_join(pfpr_df_18, motorized_friction_1000m_18, by = c("hv001"="ID"))

motorized_friction_1000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_friction_1000m_10, motorized_friction_1000m_15, motorized_friction_1000m_18)

motorized_friction_1000m_DHS_10_15_18 <- subset(motorized_friction_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_friction_1000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_friction_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


motorized_friction_2000m_10 <- load2010.fun("motorized_friction_2000m_buffer_DHS_10.csv", "minutes_travel_metre_2019_2000m")
motorized_friction_2000m_15 <- load2015.fun("motorized_friction_2000m_buffer_DHS_15.csv", "minutes_travel_metre_2019_2000m")
motorized_friction_2000m_18 <- load2018.fun("motorized_friction_2000m_buffer_DHS_18.csv", "minutes_travel_metre_2019_2000m")


motorized_friction_2000m_10 <- left_join(pfpr_df_10, motorized_friction_2000m_10, by = c("hv001"="ID"))
motorized_friction_2000m_15 <- left_join(pfpr_df_15, motorized_friction_2000m_15, by = c("hv001"="ID"))
motorized_friction_2000m_18 <- left_join(pfpr_df_18, motorized_friction_2000m_18, by = c("hv001"="ID"))

motorized_friction_2000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_friction_2000m_10, motorized_friction_2000m_15, motorized_friction_2000m_18)

motorized_friction_2000m_DHS_10_15_18 <- subset(motorized_friction_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_friction_2000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_friction_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



motorized_friction_3000m_10 <- load2010.fun("motorized_friction_3000m_buffer_DHS_10.csv", "minutes_travel_metre_2019_3000m")
motorized_friction_3000m_15 <- load2015.fun("motorized_friction_3000m_buffer_DHS_15.csv", "minutes_travel_metre_2019_3000m")
motorized_friction_3000m_18 <- load2018.fun("motorized_friction_3000m_buffer_DHS_18.csv", "minutes_travel_metre_2019_3000m")


motorized_friction_3000m_10 <- left_join(pfpr_df_10, motorized_friction_3000m_10, by = c("hv001"="ID"))
motorized_friction_3000m_15 <- left_join(pfpr_df_15, motorized_friction_3000m_15, by = c("hv001"="ID"))
motorized_friction_3000m_18 <- left_join(pfpr_df_18, motorized_friction_3000m_18, by = c("hv001"="ID"))

motorized_friction_3000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_friction_3000m_10, motorized_friction_3000m_15, motorized_friction_3000m_18)

motorized_friction_3000m_DHS_10_15_18 <- subset(motorized_friction_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_friction_3000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_friction_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


motorized_friction_4000m_10 <- load2010.fun("motorized_friction_4000m_buffer_DHS_10.csv", "minutes_travel_metre_2019_4000m")
motorized_friction_4000m_15 <- load2015.fun("motorized_friction_4000m_buffer_DHS_15.csv", "minutes_travel_metre_2019_4000m")
motorized_friction_4000m_18 <- load2018.fun("motorized_friction_4000m_buffer_DHS_18.csv", "minutes_travel_metre_2019_4000m")


motorized_friction_4000m_10 <- left_join(pfpr_df_10, motorized_friction_4000m_10, by = c("hv001"="ID"))
motorized_friction_4000m_15 <- left_join(pfpr_df_15, motorized_friction_4000m_15, by = c("hv001"="ID"))
motorized_friction_4000m_18 <- left_join(pfpr_df_18, motorized_friction_4000m_18, by = c("hv001"="ID"))

motorized_friction_4000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_friction_4000m_10, motorized_friction_4000m_15, motorized_friction_4000m_18)

motorized_friction_4000m_DHS_10_15_18 <- subset(motorized_friction_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_friction_4000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_friction_4000m_DHS_10_15_18.csv")),row.names = FALSE)



############################## 2020_motorized_travel_surface_NGA#########################
################################################################################################


######## motorized_travel 0m buffer##########

motorized_travel_0m_10 <- load2010.fun("motorized_travel_0m_buffer_DHS_10.csv", "motorized_travel_healthcare_2019_0m")
motorized_travel_0m_15 <- load2015.fun("motorized_travel_0m_buffer_DHS_15.csv", "motorized_travel_healthcare_2019_0m")
motorized_travel_0m_18 <- load2018.fun("motorized_travel_0m_buffer_DHS_18.csv", "motorized_travel_healthcare_2019_0m")


motorized_travel_0m_10 <- left_join(pfpr_df_10, motorized_travel_0m_10, by = c("hv001"="ID"))
motorized_travel_0m_15 <- left_join(pfpr_df_15, motorized_travel_0m_15, by = c("hv001"="ID"))
motorized_travel_0m_18 <- left_join(pfpr_df_18, motorized_travel_0m_18, by = c("hv001"="ID"))

motorized_travel_0m_DHS_10_15_18 <- dplyr::bind_rows(motorized_travel_0m_10, motorized_travel_0m_15, motorized_travel_0m_18)

motorized_travel_0m_DHS_10_15_18 <- subset(motorized_travel_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_travel_0m_DHS_10_15_18, file.path(Covdir, paste0("motorized_travel_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

motorized_travel_1000m_10 <- load2010.fun("motorized_travel_1000m_buffer_DHS_10.csv", "motorized_travel_healthcare_2019_1000m")
motorized_travel_1000m_15 <- load2015.fun("motorized_travel_1000m_buffer_DHS_15.csv", "motorized_travel_healthcare_2019_1000m")
motorized_travel_1000m_18 <- load2018.fun("motorized_travel_1000m_buffer_DHS_18.csv", "motorized_travel_healthcare_2019_1000m")


motorized_travel_1000m_10 <- left_join(pfpr_df_10, motorized_travel_1000m_10, by = c("hv001"="ID"))
motorized_travel_1000m_15 <- left_join(pfpr_df_15, motorized_travel_1000m_15, by = c("hv001"="ID"))
motorized_travel_1000m_18 <- left_join(pfpr_df_18, motorized_travel_1000m_18, by = c("hv001"="ID"))

motorized_travel_1000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_travel_1000m_10, motorized_travel_1000m_15, motorized_travel_1000m_18)

motorized_travel_1000m_DHS_10_15_18 <- subset(motorized_travel_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_travel_1000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_travel_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


motorized_travel_2000m_10 <- load2010.fun("motorized_travel_2000m_buffer_DHS_10.csv", "motorized_travel_healthcare_2019_2000m")
motorized_travel_2000m_15 <- load2015.fun("motorized_travel_2000m_buffer_DHS_15.csv", "motorized_travel_healthcare_2019_2000m")
motorized_travel_2000m_18 <- load2018.fun("motorized_travel_2000m_buffer_DHS_18.csv", "motorized_travel_healthcare_2019_2000m")


motorized_travel_2000m_10 <- left_join(pfpr_df_10, motorized_travel_2000m_10, by = c("hv001"="ID"))
motorized_travel_2000m_15 <- left_join(pfpr_df_15, motorized_travel_2000m_15, by = c("hv001"="ID"))
motorized_travel_2000m_18 <- left_join(pfpr_df_18, motorized_travel_2000m_18, by = c("hv001"="ID"))

motorized_travel_2000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_travel_2000m_10, motorized_travel_2000m_15, motorized_travel_2000m_18)

motorized_travel_2000m_DHS_10_15_18 <- subset(motorized_travel_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_travel_2000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_travel_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



motorized_travel_3000m_10 <- load2010.fun("motorized_travel_3000m_buffer_DHS_10.csv", "motorized_travel_healthcare_2019_3000m")
motorized_travel_3000m_15 <- load2015.fun("motorized_travel_3000m_buffer_DHS_15.csv", "motorized_travel_healthcare_2019_3000m")
motorized_travel_3000m_18 <- load2018.fun("motorized_travel_3000m_buffer_DHS_18.csv", "motorized_travel_healthcare_2019_3000m")


motorized_travel_3000m_10 <- left_join(pfpr_df_10, motorized_travel_3000m_10, by = c("hv001"="ID"))
motorized_travel_3000m_15 <- left_join(pfpr_df_15, motorized_travel_3000m_15, by = c("hv001"="ID"))
motorized_travel_3000m_18 <- left_join(pfpr_df_18, motorized_travel_3000m_18, by = c("hv001"="ID"))

motorized_travel_3000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_travel_3000m_10, motorized_travel_3000m_15, motorized_travel_3000m_18)

motorized_travel_3000m_DHS_10_15_18 <- subset(motorized_travel_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_travel_3000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_travel_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


motorized_travel_4000m_10 <- load2010.fun("motorized_travel_4000m_buffer_DHS_10.csv", "motorized_travel_healthcare_2019_4000m")
motorized_travel_4000m_15 <- load2015.fun("motorized_travel_4000m_buffer_DHS_15.csv", "motorized_travel_healthcare_2019_4000m")
motorized_travel_4000m_18 <- load2018.fun("motorized_travel_4000m_buffer_DHS_18.csv", "motorized_travel_healthcare_2019_4000m")


motorized_travel_4000m_10 <- left_join(pfpr_df_10, motorized_travel_4000m_10, by = c("hv001"="ID"))
motorized_travel_4000m_15 <- left_join(pfpr_df_15, motorized_travel_4000m_15, by = c("hv001"="ID"))
motorized_travel_4000m_18 <- left_join(pfpr_df_18, motorized_travel_4000m_18, by = c("hv001"="ID"))

motorized_travel_4000m_DHS_10_15_18 <- dplyr::bind_rows(motorized_travel_4000m_10, motorized_travel_4000m_15, motorized_travel_4000m_18)

motorized_travel_4000m_DHS_10_15_18 <- subset(motorized_travel_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(motorized_travel_4000m_DHS_10_15_18, file.path(Covdir, paste0("motorized_travel_4000m_DHS_10_15_18.csv")),row.names = FALSE)


############################## 2020_walking_friction_surface_NGA#########################
################################################################################################


######## walking_friction 0m buffer##########

walking_friction_0m_10 <- load2010.fun("walking_friction_0m_buffer_DHS_10.csv", "minutes_walking_metre_0m")
walking_friction_0m_15 <- load2015.fun("walking_friction_0m_buffer_DHS_15.csv", "minutes_walking_metre_0m")
walking_friction_0m_18 <- load2018.fun("walking_friction_0m_buffer_DHS_18.csv", "minutes_walking_metre_0m")


walking_friction_0m_10 <- left_join(pfpr_df_10, walking_friction_0m_10, by = c("hv001"="ID"))
walking_friction_0m_15 <- left_join(pfpr_df_15, walking_friction_0m_15, by = c("hv001"="ID"))
walking_friction_0m_18 <- left_join(pfpr_df_18, walking_friction_0m_18, by = c("hv001"="ID"))

walking_friction_0m_DHS_10_15_18 <- dplyr::bind_rows(walking_friction_0m_10, walking_friction_0m_15, walking_friction_0m_18)

walking_friction_0m_DHS_10_15_18 <- subset(walking_friction_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_friction_0m_DHS_10_15_18, file.path(Covdir, paste0("walking_friction_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

walking_friction_1000m_10 <- load2010.fun("walking_friction_1000m_buffer_DHS_10.csv", "minutes_walking_metre_1000m")
walking_friction_1000m_15 <- load2015.fun("walking_friction_1000m_buffer_DHS_15.csv", "minutes_walking_metre_1000m")
walking_friction_1000m_18 <- load2018.fun("walking_friction_1000m_buffer_DHS_18.csv", "minutes_walking_metre_1000m")


walking_friction_1000m_10 <- left_join(pfpr_df_10, walking_friction_1000m_10, by = c("hv001"="ID"))
walking_friction_1000m_15 <- left_join(pfpr_df_15, walking_friction_1000m_15, by = c("hv001"="ID"))
walking_friction_1000m_18 <- left_join(pfpr_df_18, walking_friction_1000m_18, by = c("hv001"="ID"))

walking_friction_1000m_DHS_10_15_18 <- dplyr::bind_rows(walking_friction_1000m_10, walking_friction_1000m_15, walking_friction_1000m_18)

walking_friction_1000m_DHS_10_15_18 <- subset(walking_friction_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_friction_1000m_DHS_10_15_18, file.path(Covdir, paste0("walking_friction_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


walking_friction_2000m_10 <- load2010.fun("walking_friction_2000m_buffer_DHS_10.csv", "minutes_walking_metre_2000m")
walking_friction_2000m_15 <- load2015.fun("walking_friction_2000m_buffer_DHS_15.csv", "minutes_walking_metre_2000m")
walking_friction_2000m_18 <- load2018.fun("walking_friction_2000m_buffer_DHS_18.csv", "minutes_walking_metre_2000m")


walking_friction_2000m_10 <- left_join(pfpr_df_10, walking_friction_2000m_10, by = c("hv001"="ID"))
walking_friction_2000m_15 <- left_join(pfpr_df_15, walking_friction_2000m_15, by = c("hv001"="ID"))
walking_friction_2000m_18 <- left_join(pfpr_df_18, walking_friction_2000m_18, by = c("hv001"="ID"))

walking_friction_2000m_DHS_10_15_18 <- dplyr::bind_rows(walking_friction_2000m_10, walking_friction_2000m_15, walking_friction_2000m_18)

walking_friction_2000m_DHS_10_15_18 <- subset(walking_friction_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_friction_2000m_DHS_10_15_18, file.path(Covdir, paste0("walking_friction_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



walking_friction_3000m_10 <- load2010.fun("walking_friction_3000m_buffer_DHS_10.csv", "minutes_walking_metre_3000m")
walking_friction_3000m_15 <- load2015.fun("walking_friction_3000m_buffer_DHS_15.csv", "minutes_walking_metre_3000m")
walking_friction_3000m_18 <- load2018.fun("walking_friction_3000m_buffer_DHS_18.csv", "minutes_walking_metre_3000m")


walking_friction_3000m_10 <- left_join(pfpr_df_10, walking_friction_3000m_10, by = c("hv001"="ID"))
walking_friction_3000m_15 <- left_join(pfpr_df_15, walking_friction_3000m_15, by = c("hv001"="ID"))
walking_friction_3000m_18 <- left_join(pfpr_df_18, walking_friction_3000m_18, by = c("hv001"="ID"))

walking_friction_3000m_DHS_10_15_18 <- dplyr::bind_rows(walking_friction_3000m_10, walking_friction_3000m_15, walking_friction_3000m_18)

walking_friction_3000m_DHS_10_15_18 <- subset(walking_friction_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_friction_3000m_DHS_10_15_18, file.path(Covdir, paste0("walking_friction_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


walking_friction_4000m_10 <- load2010.fun("walking_friction_4000m_buffer_DHS_10.csv", "minutes_walking_metre_4000m")
walking_friction_4000m_15 <- load2015.fun("walking_friction_4000m_buffer_DHS_15.csv", "minutes_walking_metre_4000m")
walking_friction_4000m_18 <- load2018.fun("walking_friction_4000m_buffer_DHS_18.csv", "minutes_walking_metre_4000m")


walking_friction_4000m_10 <- left_join(pfpr_df_10, walking_friction_4000m_10, by = c("hv001"="ID"))
walking_friction_4000m_15 <- left_join(pfpr_df_15, walking_friction_4000m_15, by = c("hv001"="ID"))
walking_friction_4000m_18 <- left_join(pfpr_df_18, walking_friction_4000m_18, by = c("hv001"="ID"))

walking_friction_4000m_DHS_10_15_18 <- dplyr::bind_rows(walking_friction_4000m_10, walking_friction_4000m_15, walking_friction_4000m_18)

walking_friction_4000m_DHS_10_15_18 <- subset(walking_friction_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_friction_4000m_DHS_10_15_18, file.path(Covdir, paste0("walking_friction_4000m_DHS_10_15_18.csv")),row.names = FALSE)




############################## 2020_walking_travel_surface_NGA#########################
################################################################################################


######## walking_travel 0m buffer##########


walking_travel_0m_10 <- load2010.fun("walking_travel_0m_buffer_DHS_10.csv", "minutes_walking_healthcare_0m")
walking_travel_0m_15 <- load2015.fun("walking_travel_0m_buffer_DHS_15.csv", "minutes_walking_healthcare_0m")
walking_travel_0m_18 <- load2018.fun("walking_travel_0m_buffer_DHS_18.csv", "minutes_walking_healthcare_0m")


walking_travel_0m_10 <- left_join(pfpr_df_10, walking_travel_0m_10, by = c("hv001"="ID"))
walking_travel_0m_15 <- left_join(pfpr_df_15, walking_travel_0m_15, by = c("hv001"="ID"))
walking_travel_0m_18 <- left_join(pfpr_df_18, walking_travel_0m_18, by = c("hv001"="ID"))

walking_travel_0m_DHS_10_15_18 <- dplyr::bind_rows(walking_travel_0m_10, walking_travel_0m_15, walking_travel_0m_18)

walking_travel_0m_DHS_10_15_18 <- subset(walking_travel_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_travel_0m_DHS_10_15_18, file.path(Covdir, paste0("walking_travel_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

walking_travel_1000m_10 <- load2010.fun("walking_travel_1000m_buffer_DHS_10.csv", "minutes_walking_healthcare_1000m")
walking_travel_1000m_15 <- load2015.fun("walking_travel_1000m_buffer_DHS_15.csv", "minutes_walking_healthcare_1000m")
walking_travel_1000m_18 <- load2018.fun("walking_travel_1000m_buffer_DHS_18.csv", "minutes_walking_healthcare_1000m")


walking_travel_1000m_10 <- left_join(pfpr_df_10, walking_travel_1000m_10, by = c("hv001"="ID"))
walking_travel_1000m_15 <- left_join(pfpr_df_15, walking_travel_1000m_15, by = c("hv001"="ID"))
walking_travel_1000m_18 <- left_join(pfpr_df_18, walking_travel_1000m_18, by = c("hv001"="ID"))

walking_travel_1000m_DHS_10_15_18 <- dplyr::bind_rows(walking_travel_1000m_10, walking_travel_1000m_15, walking_travel_1000m_18)

walking_travel_1000m_DHS_10_15_18 <- subset(walking_travel_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_travel_1000m_DHS_10_15_18, file.path(Covdir, paste0("walking_travel_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


walking_travel_2000m_10 <- load2010.fun("walking_travel_2000m_buffer_DHS_10.csv", "minutes_walking_healthcare_2000m")
walking_travel_2000m_15 <- load2015.fun("walking_travel_2000m_buffer_DHS_15.csv", "minutes_walking_healthcare_2000m")
walking_travel_2000m_18 <- load2018.fun("walking_travel_2000m_buffer_DHS_18.csv", "minutes_walking_healthcare_2000m")


walking_travel_2000m_10 <- left_join(pfpr_df_10, walking_travel_2000m_10, by = c("hv001"="ID"))
walking_travel_2000m_15 <- left_join(pfpr_df_15, walking_travel_2000m_15, by = c("hv001"="ID"))
walking_travel_2000m_18 <- left_join(pfpr_df_18, walking_travel_2000m_18, by = c("hv001"="ID"))

walking_travel_2000m_DHS_10_15_18 <- dplyr::bind_rows(walking_travel_2000m_10, walking_travel_2000m_15, walking_travel_2000m_18)

walking_travel_2000m_DHS_10_15_18 <- subset(walking_travel_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_travel_2000m_DHS_10_15_18, file.path(Covdir, paste0("walking_travel_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



walking_travel_3000m_10 <- load2010.fun("walking_travel_3000m_buffer_DHS_10.csv", "minutes_walking_healthcare_3000m")
walking_travel_3000m_15 <- load2015.fun("walking_travel_3000m_buffer_DHS_15.csv", "minutes_walking_healthcare_3000m")
walking_travel_3000m_18 <- load2018.fun("walking_travel_3000m_buffer_DHS_18.csv", "minutes_walking_healthcare_3000m")


walking_travel_3000m_10 <- left_join(pfpr_df_10, walking_travel_3000m_10, by = c("hv001"="ID"))
walking_travel_3000m_15 <- left_join(pfpr_df_15, walking_travel_3000m_15, by = c("hv001"="ID"))
walking_travel_3000m_18 <- left_join(pfpr_df_18, walking_travel_3000m_18, by = c("hv001"="ID"))

walking_travel_3000m_DHS_10_15_18 <- dplyr::bind_rows(walking_travel_3000m_10, walking_travel_3000m_15, walking_travel_3000m_18)

walking_travel_3000m_DHS_10_15_18 <- subset(walking_travel_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_travel_3000m_DHS_10_15_18, file.path(Covdir, paste0("walking_travel_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


walking_travel_4000m_10 <- load2010.fun("walking_travel_4000m_buffer_DHS_10.csv", "minutes_walking_healthcare_4000m")
walking_travel_4000m_15 <- load2015.fun("walking_travel_4000m_buffer_DHS_15.csv", "minutes_walking_healthcare_4000m")
walking_travel_4000m_18 <- load2018.fun("walking_travel_4000m_buffer_DHS_18.csv", "minutes_walking_healthcare_4000m")


walking_travel_4000m_10 <- left_join(pfpr_df_10, walking_travel_4000m_10, by = c("hv001"="ID"))
walking_travel_4000m_15 <- left_join(pfpr_df_15, walking_travel_4000m_15, by = c("hv001"="ID"))
walking_travel_4000m_18 <- left_join(pfpr_df_18, walking_travel_4000m_18, by = c("hv001"="ID"))

walking_travel_4000m_DHS_10_15_18 <- dplyr::bind_rows(walking_travel_4000m_10, walking_travel_4000m_15, walking_travel_4000m_18)

walking_travel_4000m_DHS_10_15_18 <- subset(walking_travel_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(walking_travel_4000m_DHS_10_15_18, file.path(Covdir, paste0("walking_travel_4000m_DHS_10_15_18.csv")),row.names = FALSE)


#_______________________Bulding density 


######## building_density 0m buffer##########


building_density_0m_10 <- load2010.fun("building_density_0m_buffer_DHS_10.csv", "building_density_0m")
building_density_0m_15 <- load2015.fun("building_density_0m_buffer_DHS_15.csv", "building_density_0m")
building_density_0m_18 <- load2018.fun("building_density_0m_buffer_DHS_18.csv", "building_density_0m")


building_density_0m_10 <- left_join(pfpr_df_10, building_density_0m_10, by = c("hv001"="ID"))
building_density_0m_15 <- left_join(pfpr_df_15, building_density_0m_15, by = c("hv001"="ID"))
building_density_0m_18 <- left_join(pfpr_df_18, building_density_0m_18, by = c("hv001"="ID"))

building_density_0m_DHS_10_15_18 <- dplyr::bind_rows(building_density_0m_10, building_density_0m_15, building_density_0m_18)

building_density_0m_DHS_10_15_18 <- subset(building_density_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(building_density_0m_DHS_10_15_18, file.path(Covdir, paste0("building_density_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

building_density_1000m_10 <- load2010.fun("building_density_1000m_buffer_DHS_10.csv", "building_density_1000m")
building_density_1000m_15 <- load2015.fun("building_density_1000m_buffer_DHS_15.csv", "building_density_1000m")
building_density_1000m_18 <- load2018.fun("building_density_1000m_buffer_DHS_18.csv", "building_density_1000m")


building_density_1000m_10 <- left_join(pfpr_df_10, building_density_1000m_10, by = c("hv001"="ID"))
building_density_1000m_15 <- left_join(pfpr_df_15, building_density_1000m_15, by = c("hv001"="ID"))
building_density_1000m_18 <- left_join(pfpr_df_18, building_density_1000m_18, by = c("hv001"="ID"))

building_density_1000m_DHS_10_15_18 <- dplyr::bind_rows(building_density_1000m_10, building_density_1000m_15, building_density_1000m_18)

building_density_1000m_DHS_10_15_18 <- subset(building_density_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(building_density_1000m_DHS_10_15_18, file.path(Covdir, paste0("building_density_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


building_density_2000m_10 <- load2010.fun("building_density_2000m_buffer_DHS_10.csv", "building_density_2000m")
building_density_2000m_15 <- load2015.fun("building_density_2000m_buffer_DHS_15.csv", "building_density_2000m")
building_density_2000m_18 <- load2018.fun("building_density_2000m_buffer_DHS_18.csv", "building_density_2000m")


building_density_2000m_10 <- left_join(pfpr_df_10, building_density_2000m_10, by = c("hv001"="ID"))
building_density_2000m_15 <- left_join(pfpr_df_15, building_density_2000m_15, by = c("hv001"="ID"))
building_density_2000m_18 <- left_join(pfpr_df_18, building_density_2000m_18, by = c("hv001"="ID"))

building_density_2000m_DHS_10_15_18 <- dplyr::bind_rows(building_density_2000m_10, building_density_2000m_15, building_density_2000m_18)

building_density_2000m_DHS_10_15_18 <- subset(building_density_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(building_density_2000m_DHS_10_15_18, file.path(Covdir, paste0("building_density_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



building_density_3000m_10 <- load2010.fun("building_density_3000m_buffer_DHS_10.csv", "building_density_3000m")
building_density_3000m_15 <- load2015.fun("building_density_3000m_buffer_DHS_15.csv", "building_density_3000m")
building_density_3000m_18 <- load2018.fun("building_density_3000m_buffer_DHS_18.csv", "building_density_3000m")


building_density_3000m_10 <- left_join(pfpr_df_10, building_density_3000m_10, by = c("hv001"="ID"))
building_density_3000m_15 <- left_join(pfpr_df_15, building_density_3000m_15, by = c("hv001"="ID"))
building_density_3000m_18 <- left_join(pfpr_df_18, building_density_3000m_18, by = c("hv001"="ID"))

building_density_3000m_DHS_10_15_18 <- dplyr::bind_rows(building_density_3000m_10, building_density_3000m_15, building_density_3000m_18)

building_density_3000m_DHS_10_15_18 <- subset(building_density_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(building_density_3000m_DHS_10_15_18, file.path(Covdir, paste0("building_density_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


building_density_4000m_10 <- load2010.fun("building_density_4000m_buffer_DHS_10.csv", "building_density_4000m")
building_density_4000m_15 <- load2015.fun("building_density_4000m_buffer_DHS_15.csv", "building_density_4000m")
building_density_4000m_18 <- load2018.fun("building_density_4000m_buffer_DHS_18.csv", "building_density_4000m")


building_density_4000m_10 <- left_join(pfpr_df_10, building_density_4000m_10, by = c("hv001"="ID"))
building_density_4000m_15 <- left_join(pfpr_df_15, building_density_4000m_15, by = c("hv001"="ID"))
building_density_4000m_18 <- left_join(pfpr_df_18, building_density_4000m_18, by = c("hv001"="ID"))

building_density_4000m_DHS_10_15_18 <- dplyr::bind_rows(building_density_4000m_10, building_density_4000m_15, building_density_4000m_18)

building_density_4000m_DHS_10_15_18 <- subset(building_density_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(building_density_4000m_DHS_10_15_18, file.path(Covdir, paste0("building_density_4000m_DHS_10_15_18.csv")),row.names = FALSE)


#__________________


############################## 2020_elevation_surface_NGA#########################
################################################################################################


######## elevation 0m buffer##########


elevation_0m_10 <- load2010.fun("elevation_0m_buffer_DHS_10.csv", "elev_0m")
elevation_0m_15 <- load2015.fun("elevation_0m_buffer_DHS_15.csv", "elev_0m")
elevation_0m_18 <- load2018.fun("elevation_0m_buffer_DHS_18.csv", "elev_0m")


elevation_0m_10 <- left_join(pfpr_df_10, elevation_0m_10, by = c("hv001"="ID"))
elevation_0m_15 <- left_join(pfpr_df_15, elevation_0m_15, by = c("hv001"="ID"))
elevation_0m_18 <- left_join(pfpr_df_18, elevation_0m_18, by = c("hv001"="ID"))

elevation_0m_DHS_10_15_18 <- dplyr::bind_rows(elevation_0m_10, elevation_0m_15, elevation_0m_18)

elevation_0m_DHS_10_15_18 <- subset(elevation_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(elevation_0m_DHS_10_15_18, file.path(Covdir, paste0("elevation_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

elevation_1000m_10 <- load2010.fun("elevation_1000m_buffer_DHS_10.csv", "elev_1000m")
elevation_1000m_15 <- load2015.fun("elevation_1000m_buffer_DHS_15.csv", "elev_1000m")
elevation_1000m_18 <- load2018.fun("elevation_1000m_buffer_DHS_18.csv", "elev_1000m")


elevation_1000m_10 <- left_join(pfpr_df_10, elevation_1000m_10, by = c("hv001"="ID"))
elevation_1000m_15 <- left_join(pfpr_df_15, elevation_1000m_15, by = c("hv001"="ID"))
elevation_1000m_18 <- left_join(pfpr_df_18, elevation_1000m_18, by = c("hv001"="ID"))

elevation_1000m_DHS_10_15_18 <- dplyr::bind_rows(elevation_1000m_10, elevation_1000m_15, elevation_1000m_18)

elevation_1000m_DHS_10_15_18 <- subset(elevation_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(elevation_1000m_DHS_10_15_18, file.path(Covdir, paste0("elevation_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


elevation_2000m_10 <- load2010.fun("elevation_2000m_buffer_DHS_10.csv", "elev_2000m")
elevation_2000m_15 <- load2015.fun("elevation_2000m_buffer_DHS_15.csv", "elev_2000m")
elevation_2000m_18 <- load2018.fun("elevation_2000m_buffer_DHS_18.csv", "elev_2000m")


elevation_2000m_10 <- left_join(pfpr_df_10, elevation_2000m_10, by = c("hv001"="ID"))
elevation_2000m_15 <- left_join(pfpr_df_15, elevation_2000m_15, by = c("hv001"="ID"))
elevation_2000m_18 <- left_join(pfpr_df_18, elevation_2000m_18, by = c("hv001"="ID"))

elevation_2000m_DHS_10_15_18 <- dplyr::bind_rows(elevation_2000m_10, elevation_2000m_15, elevation_2000m_18)

elevation_2000m_DHS_10_15_18 <- subset(elevation_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(elevation_2000m_DHS_10_15_18, file.path(Covdir, paste0("elevation_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



elevation_3000m_10 <- load2010.fun("elevation_3000m_buffer_DHS_10.csv", "elev_3000m")
elevation_3000m_15 <- load2015.fun("elevation_3000m_buffer_DHS_15.csv", "elev_3000m")
elevation_3000m_18 <- load2018.fun("elevation_3000m_buffer_DHS_18.csv", "elev_3000m")


elevation_3000m_10 <- left_join(pfpr_df_10, elevation_3000m_10, by = c("hv001"="ID"))
elevation_3000m_15 <- left_join(pfpr_df_15, elevation_3000m_15, by = c("hv001"="ID"))
elevation_3000m_18 <- left_join(pfpr_df_18, elevation_3000m_18, by = c("hv001"="ID"))

elevation_3000m_DHS_10_15_18 <- dplyr::bind_rows(elevation_3000m_10, elevation_3000m_15, elevation_3000m_18)

elevation_3000m_DHS_10_15_18 <- subset(elevation_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(elevation_3000m_DHS_10_15_18, file.path(Covdir, paste0("elevation_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


elevation_4000m_10 <- load2010.fun("elevation_4000m_buffer_DHS_10.csv", "elev_4000m")
elevation_4000m_15 <- load2015.fun("elevation_4000m_buffer_DHS_15.csv", "elev_4000m")
elevation_4000m_18 <- load2018.fun("elevation_4000m_buffer_DHS_18.csv", "elev_4000m")


elevation_4000m_10 <- left_join(pfpr_df_10, elevation_4000m_10, by = c("hv001"="ID"))
elevation_4000m_15 <- left_join(pfpr_df_15, elevation_4000m_15, by = c("hv001"="ID"))
elevation_4000m_18 <- left_join(pfpr_df_18, elevation_4000m_18, by = c("hv001"="ID"))

elevation_4000m_DHS_10_15_18 <- dplyr::bind_rows(elevation_4000m_10, elevation_4000m_15, elevation_4000m_18)

elevation_4000m_DHS_10_15_18 <- subset(elevation_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(elevation_4000m_DHS_10_15_18, file.path(Covdir, paste0("elevation_4000m_DHS_10_15_18.csv")),row.names = FALSE)


#__________________


############################## U5 Pop density #########################
################################################################################################


######## om buffer U5 Pop density ##########


pop_den_U5_FB_0m_10 <- load2010.fun("pop_den_U5_FB_0m_buffer_DHS_10.csv", "pop_den_U5_FB_0m")
pop_den_U5_FB_0m_15 <- load2015.fun("pop_den_U5_FB_0m_buffer_DHS_15.csv", "pop_den_U5_FB_0m")
pop_den_U5_FB_0m_18 <- load2018.fun("pop_den_U5_FB_0m_buffer_DHS_18.csv", "pop_den_U5_FB_0m")


pop_den_U5_FB_0m_10 <- left_join(pfpr_df_10, pop_den_U5_FB_0m_10, by = c("hv001"="ID"))
pop_den_U5_FB_0m_15 <- left_join(pfpr_df_15, pop_den_U5_FB_0m_15, by = c("hv001"="ID"))
pop_den_U5_FB_0m_18 <- left_join(pfpr_df_18, pop_den_U5_FB_0m_18, by = c("hv001"="ID"))

pop_den_U5_FB_0m_DHS_10_15_18 <- dplyr::bind_rows(pop_den_U5_FB_0m_10, pop_den_U5_FB_0m_15, pop_den_U5_FB_0m_18)

pop_den_U5_FB_0m_DHS_10_15_18 <- subset(pop_den_U5_FB_0m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(pop_den_U5_FB_0m_DHS_10_15_18, file.path(Covdir, paste0("pop_den_U5_FB_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########

pop_den_U5_FB_1000m_10 <- load2010.fun("pop_den_U5_FB_1000m_buffer_DHS_10.csv", "pop_den_U5_FB_1000m")
pop_den_U5_FB_1000m_15 <- load2015.fun("pop_den_U5_FB_1000m_buffer_DHS_15.csv", "pop_den_U5_FB_1000m")
pop_den_U5_FB_1000m_18 <- load2018.fun("pop_den_U5_FB_1000m_buffer_DHS_18.csv", "pop_den_U5_FB_1000m")


pop_den_U5_FB_1000m_10 <- left_join(pfpr_df_10, pop_den_U5_FB_1000m_10, by = c("hv001"="ID"))
pop_den_U5_FB_1000m_15 <- left_join(pfpr_df_15, pop_den_U5_FB_1000m_15, by = c("hv001"="ID"))
pop_den_U5_FB_1000m_18 <- left_join(pfpr_df_18, pop_den_U5_FB_1000m_18, by = c("hv001"="ID"))

pop_den_U5_FB_1000m_DHS_10_15_18 <- dplyr::bind_rows(pop_den_U5_FB_1000m_10, pop_den_U5_FB_1000m_15, pop_den_U5_FB_1000m_18)

pop_den_U5_FB_1000m_DHS_10_15_18 <- subset(pop_den_U5_FB_1000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(pop_den_U5_FB_1000m_DHS_10_15_18, file.path(Covdir, paste0("pop_den_U5_FB_1000m_DHS_10_15_18.csv")),row.names = FALSE)

######## 2000m buffer##########


pop_den_U5_FB_2000m_10 <- load2010.fun("pop_den_U5_FB_2000m_buffer_DHS_10.csv", "pop_den_U5_FB_2000m")
pop_den_U5_FB_2000m_15 <- load2015.fun("pop_den_U5_FB_2000m_buffer_DHS_15.csv", "pop_den_U5_FB_2000m")
pop_den_U5_FB_2000m_18 <- load2018.fun("pop_den_U5_FB_2000m_buffer_DHS_18.csv", "pop_den_U5_FB_2000m")


pop_den_U5_FB_2000m_10 <- left_join(pfpr_df_10, pop_den_U5_FB_2000m_10, by = c("hv001"="ID"))
pop_den_U5_FB_2000m_15 <- left_join(pfpr_df_15, pop_den_U5_FB_2000m_15, by = c("hv001"="ID"))
pop_den_U5_FB_2000m_18 <- left_join(pfpr_df_18, pop_den_U5_FB_2000m_18, by = c("hv001"="ID"))

pop_den_U5_FB_2000m_DHS_10_15_18 <- dplyr::bind_rows(pop_den_U5_FB_2000m_10, pop_den_U5_FB_2000m_15, pop_den_U5_FB_2000m_18)

pop_den_U5_FB_2000m_DHS_10_15_18 <- subset(pop_den_U5_FB_2000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(pop_den_U5_FB_2000m_DHS_10_15_18, file.path(Covdir, paste0("pop_den_U5_FB_2000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 3000m buffer##########



pop_den_U5_FB_3000m_10 <- load2010.fun("pop_den_U5_FB_3000m_buffer_DHS_10.csv", "pop_den_U5_FB_3000m")
pop_den_U5_FB_3000m_15 <- load2015.fun("pop_den_U5_FB_3000m_buffer_DHS_15.csv", "pop_den_U5_FB_3000m")
pop_den_U5_FB_3000m_18 <- load2018.fun("pop_den_U5_FB_3000m_buffer_DHS_18.csv", "pop_den_U5_FB_3000m")


pop_den_U5_FB_3000m_10 <- left_join(pfpr_df_10, pop_den_U5_FB_3000m_10, by = c("hv001"="ID"))
pop_den_U5_FB_3000m_15 <- left_join(pfpr_df_15, pop_den_U5_FB_3000m_15, by = c("hv001"="ID"))
pop_den_U5_FB_3000m_18 <- left_join(pfpr_df_18, pop_den_U5_FB_3000m_18, by = c("hv001"="ID"))

pop_den_U5_FB_3000m_DHS_10_15_18 <- dplyr::bind_rows(pop_den_U5_FB_3000m_10, pop_den_U5_FB_3000m_15, pop_den_U5_FB_3000m_18)

pop_den_U5_FB_3000m_DHS_10_15_18 <- subset(pop_den_U5_FB_3000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(pop_den_U5_FB_3000m_DHS_10_15_18, file.path(Covdir, paste0("pop_den_U5_FB_3000m_DHS_10_15_18.csv")),row.names = FALSE)


######## 4000m buffer##########


pop_den_U5_FB_4000m_10 <- load2010.fun("pop_den_U5_FB_4000m_buffer_DHS_10.csv", "pop_den_U5_FB_4000m")
pop_den_U5_FB_4000m_15 <- load2015.fun("pop_den_U5_FB_4000m_buffer_DHS_15.csv", "pop_den_U5_FB_4000m")
pop_den_U5_FB_4000m_18 <- load2018.fun("pop_den_U5_FB_4000m_buffer_DHS_18.csv", "pop_den_U5_FB_4000m")


pop_den_U5_FB_4000m_10 <- left_join(pfpr_df_10, pop_den_U5_FB_4000m_10, by = c("hv001"="ID"))
pop_den_U5_FB_4000m_15 <- left_join(pfpr_df_15, pop_den_U5_FB_4000m_15, by = c("hv001"="ID"))
pop_den_U5_FB_4000m_18 <- left_join(pfpr_df_18, pop_den_U5_FB_4000m_18, by = c("hv001"="ID"))

pop_den_U5_FB_4000m_DHS_10_15_18 <- dplyr::bind_rows(pop_den_U5_FB_4000m_10, pop_den_U5_FB_4000m_15, pop_den_U5_FB_4000m_18)

pop_den_U5_FB_4000m_DHS_10_15_18 <- subset(pop_den_U5_FB_4000m_DHS_10_15_18, select = c(1, 3, 4, 5))

write.csv(pop_den_U5_FB_4000m_DHS_10_15_18, file.path(Covdir, paste0("pop_den_U5_FB_4000m_DHS_10_15_18.csv")),row.names = FALSE)

###End