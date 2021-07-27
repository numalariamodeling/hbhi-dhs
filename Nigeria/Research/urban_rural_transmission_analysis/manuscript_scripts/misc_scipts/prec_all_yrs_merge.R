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


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyze
## ----------------------------------------------------
### Read in PR  data (DHS 2010, 2015, 2018)  

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

pfpr_df_18 <- pfpr_df_18[,c("hv001", "hv006")] %>% mutate(dhs_year = 2018)
pfpr_df_15 <- pfpr_df_15[,c("hv001", "hv006")] %>% mutate(dhs_year = 2015)
pfpr_df_10 <- pfpr_df_10[,c("hv001", "hv006")] %>% mutate(dhs_year = 2010)

######## loading survey month temperatures

dhs_2010_aug <- pfpr_df_10  %>% filter(hv006 == 8)
dhs_2010_sep <- pfpr_df_10  %>% filter(hv006 == 9)
dhs_2010_oct <- pfpr_df_10  %>% filter(hv006 == 10)
dhs_2010_nov <- pfpr_df_10  %>% filter(hv006 == 11)
dhs_2010_dec <- pfpr_df_10  %>% filter(hv006 == 12)

dhs_2015_aug <- pfpr_df_15  %>% filter(hv006 == 8)
dhs_2015_sep <- pfpr_df_15  %>% filter(hv006 == 9)
dhs_2015_oct <- pfpr_df_15  %>% filter(hv006 == 10)
dhs_2015_nov <- pfpr_df_15  %>% filter(hv006 == 11)
dhs_2015_dec <- pfpr_df_15  %>% filter(hv006 == 12)
                                                                             
dhs_2018_aug <- pfpr_df_18  %>% filter(hv006 == 8)
dhs_2018_sep <- pfpr_df_18  %>% filter(hv006 == 9)
dhs_2018_oct <- pfpr_df_18  %>% filter(hv006 == 10)
dhs_2018_nov <- pfpr_df_18  %>% filter(hv006 == 11)
dhs_2018_dec <- pfpr_df_18  %>% filter(hv006 == 12)

## ----------------------------------------------------
#CSV data loader

loadtemp.fun <- function(filename){
        read.csv(file.path(DataIn, filename),
                 header = T, sep = ',') %>% rename_at(4,~"prec_all_yrs_0m")
}



######## 0m buffer##########
#August
aug_prec_all_yrs0m_10 <- loadtemp.fun("aug_prec_all_yrs0m_buffer_DHS_10.csv")
aug_prec_all_yrs0m_15 <- loadtemp.fun("aug_prec_all_yrs0m_buffer_DHS_15.csv")        
aug_prec_all_yrs0m_18 <- loadtemp.fun("aug_prec_all_yrs0m_buffer_DHS_18.csv")

#SEPT
sep_prec_all_yrs0m_10 <- loadtemp.fun("sep_prec_all_yrs0m_buffer_DHS_10.csv")
sep_prec_all_yrs0m_15 <- loadtemp.fun("sep_prec_all_yrs0m_buffer_DHS_15.csv")        
sep_prec_all_yrs0m_18 <- loadtemp.fun("sep_prec_all_yrs0m_buffer_DHS_18.csv")

#Oct
oct_prec_all_yrs0m_10 <- loadtemp.fun("oct_prec_all_yrs0m_buffer_DHS_10.csv")
oct_prec_all_yrs0m_15 <- loadtemp.fun("oct_prec_all_yrs0m_buffer_DHS_15.csv")        
oct_prec_all_yrs0m_18 <- loadtemp.fun("oct_prec_all_yrs0m_buffer_DHS_18.csv")

#Nove
nov_prec_all_yrs0m_10 <- loadtemp.fun("nov_prec_all_yrs0m_buffer_DHS_10.csv")
nov_prec_all_yrs0m_15 <- loadtemp.fun("nov_prec_all_yrs0m_buffer_DHS_15.csv")        
nov_prec_all_yrs0m_18 <- loadtemp.fun("nov_prec_all_yrs0m_buffer_DHS_18.csv")

#Dec
dec_prec_all_yrs0m_10 <- loadtemp.fun("dec_prec_all_yrs0m_buffer_DHS_10.csv")
dec_prec_all_yrs0m_15 <- loadtemp.fun("dec_prec_all_yrs0m_buffer_DHS_15.csv")        
dec_prec_all_yrs0m_18 <- loadtemp.fun("dec_prec_all_yrs0m_buffer_DHS_18.csv")

###binding and merging 0m buffer
#2010
prec_all_yrs0m_aug_10 <- left_join(dhs_2010_aug, aug_prec_all_yrs0m_10, by = c("hv001"="ID"))
prec_all_yrs0m_sep_10 <- left_join(dhs_2010_sep, sep_prec_all_yrs0m_10, by = c("hv001"="ID"))
prec_all_yrs0m_oct_10 <- left_join(dhs_2010_oct, oct_prec_all_yrs0m_10, by = c("hv001"="ID"))
prec_all_yrs0m_nov_10 <- left_join(dhs_2010_nov, nov_prec_all_yrs0m_10, by = c("hv001"="ID"))
prec_all_yrs0m_dec_10 <- left_join(dhs_2010_dec, dec_prec_all_yrs0m_10, by = c("hv001"="ID"))

prec_all_yrs0m_DHS_10 <- dplyr::bind_rows(prec_all_yrs0m_aug_10, prec_all_yrs0m_sep_10, prec_all_yrs0m_oct_10,
                                   prec_all_yrs0m_nov_10, prec_all_yrs0m_dec_10)

#2015
prec_all_yrs0m_aug_15 <- left_join(dhs_2015_aug, aug_prec_all_yrs0m_15, by = c("hv001"="ID"))
prec_all_yrs0m_sep_15 <- left_join(dhs_2015_sep, sep_prec_all_yrs0m_15, by = c("hv001"="ID"))
prec_all_yrs0m_oct_15 <- left_join(dhs_2015_oct, oct_prec_all_yrs0m_15, by = c("hv001"="ID"))
prec_all_yrs0m_nov_15 <- left_join(dhs_2015_nov, nov_prec_all_yrs0m_15, by = c("hv001"="ID"))
prec_all_yrs0m_dec_15 <- left_join(dhs_2015_dec, dec_prec_all_yrs0m_15, by = c("hv001"="ID"))

prec_all_yrs0m_DHS_15 <- dplyr::bind_rows(prec_all_yrs0m_aug_15, prec_all_yrs0m_sep_15, prec_all_yrs0m_oct_15,
                                   prec_all_yrs0m_nov_15, prec_all_yrs0m_dec_15)

#2018
prec_all_yrs0m_aug_18 <- left_join(dhs_2018_aug, aug_prec_all_yrs0m_18, by = c("hv001"="ID"))
prec_all_yrs0m_sep_18 <- left_join(dhs_2018_sep, sep_prec_all_yrs0m_18, by = c("hv001"="ID"))
prec_all_yrs0m_oct_18 <- left_join(dhs_2018_oct, oct_prec_all_yrs0m_18, by = c("hv001"="ID"))
prec_all_yrs0m_nov_18 <- left_join(dhs_2018_nov, nov_prec_all_yrs0m_18, by = c("hv001"="ID"))
prec_all_yrs0m_dec_18 <- left_join(dhs_2018_dec, dec_prec_all_yrs0m_18, by = c("hv001"="ID"))

prec_all_yrs0m_DHS_18 <- dplyr::bind_rows(prec_all_yrs0m_aug_18, prec_all_yrs0m_sep_18, prec_all_yrs0m_oct_18,
                                   prec_all_yrs0m_nov_18, prec_all_yrs0m_dec_18)

##final 0m buffer 
prec_all_yrs0m_DHS_10_15_18 <- dplyr::bind_rows(prec_all_yrs0m_DHS_10, prec_all_yrs0m_DHS_15, prec_all_yrs0m_DHS_18)

prec_all_yrs0m_DHS_10_15_18 <- prec_all_yrs0m_DHS_10_15_18[,c("hv001", ".id", "prec_all_yrs_0m", "dhs_year")]

write.csv(prec_all_yrs0m_DHS_10_15_18, file.path(Covdir, paste0("prec_all_yrs_0m_DHS_10_15_18.csv")),row.names = FALSE)

######## 1000m buffer##########
loadtemp.fun <- function(filename){
        read.csv(file.path(DataIn, filename),
                 header = T, sep = ',') %>% rename_at(4,~"prec_all_yrs_1000m")
}
#August
aug_prec_all_yrs1000m_10 <- loadtemp.fun("aug_prec_all_yrs1000m_buffer_DHS_10.csv")
aug_prec_all_yrs1000m_15 <- loadtemp.fun("aug_prec_all_yrs1000m_buffer_DHS_15.csv")        
aug_prec_all_yrs1000m_18 <- loadtemp.fun("aug_prec_all_yrs1000m_buffer_DHS_18.csv")

#SEPT
sep_prec_all_yrs1000m_10 <- loadtemp.fun("sep_prec_all_yrs1000m_buffer_DHS_10.csv")
sep_prec_all_yrs1000m_15 <- loadtemp.fun("sep_prec_all_yrs1000m_buffer_DHS_15.csv")        
sep_prec_all_yrs1000m_18 <- loadtemp.fun("sep_prec_all_yrs1000m_buffer_DHS_18.csv")

#Oct
oct_prec_all_yrs1000m_10 <- loadtemp.fun("oct_prec_all_yrs1000m_buffer_DHS_10.csv")
oct_prec_all_yrs1000m_15 <- loadtemp.fun("oct_prec_all_yrs1000m_buffer_DHS_15.csv")        
oct_prec_all_yrs1000m_18 <- loadtemp.fun("oct_prec_all_yrs1000m_buffer_DHS_18.csv")

#Nove
nov_prec_all_yrs1000m_10 <- loadtemp.fun("nov_prec_all_yrs1000m_buffer_DHS_10.csv")
nov_prec_all_yrs1000m_15 <- loadtemp.fun("nov_prec_all_yrs1000m_buffer_DHS_15.csv")        
nov_prec_all_yrs1000m_18 <- loadtemp.fun("nov_prec_all_yrs1000m_buffer_DHS_18.csv")

#Dec
dec_prec_all_yrs1000m_10 <- loadtemp.fun("dec_prec_all_yrs1000m_buffer_DHS_10.csv")
dec_prec_all_yrs1000m_15 <- loadtemp.fun("dec_prec_all_yrs1000m_buffer_DHS_15.csv")        
dec_prec_all_yrs1000m_18 <- loadtemp.fun("dec_prec_all_yrs1000m_buffer_DHS_18.csv")


###binding and merging 1000m buffer
#2010
prec_all_yrs1000m_aug_10 <- left_join(dhs_2010_aug, aug_prec_all_yrs1000m_10, by = c("hv001"="ID"))
prec_all_yrs1000m_sep_10 <- left_join(dhs_2010_sep, sep_prec_all_yrs1000m_10, by = c("hv001"="ID"))
prec_all_yrs1000m_oct_10 <- left_join(dhs_2010_oct, oct_prec_all_yrs1000m_10, by = c("hv001"="ID"))
prec_all_yrs1000m_nov_10 <- left_join(dhs_2010_nov, nov_prec_all_yrs1000m_10, by = c("hv001"="ID"))
prec_all_yrs1000m_dec_10 <- left_join(dhs_2010_dec, dec_prec_all_yrs1000m_10, by = c("hv001"="ID"))

prec_all_yrs1000m_DHS_10 <- dplyr::bind_rows(prec_all_yrs1000m_aug_10, prec_all_yrs1000m_sep_10, prec_all_yrs1000m_oct_10,
                                   prec_all_yrs1000m_nov_10, prec_all_yrs1000m_dec_10)

#2015
prec_all_yrs1000m_aug_15 <- left_join(dhs_2015_aug, aug_prec_all_yrs1000m_15, by = c("hv001"="ID"))
prec_all_yrs1000m_sep_15 <- left_join(dhs_2015_sep, sep_prec_all_yrs1000m_15, by = c("hv001"="ID"))
prec_all_yrs1000m_oct_15 <- left_join(dhs_2015_oct, oct_prec_all_yrs1000m_15, by = c("hv001"="ID"))
prec_all_yrs1000m_nov_15 <- left_join(dhs_2015_nov, nov_prec_all_yrs1000m_15, by = c("hv001"="ID"))
prec_all_yrs1000m_dec_15 <- left_join(dhs_2015_dec, dec_prec_all_yrs1000m_15, by = c("hv001"="ID"))

prec_all_yrs1000m_DHS_15 <- dplyr::bind_rows(prec_all_yrs1000m_aug_15, prec_all_yrs1000m_sep_15, prec_all_yrs1000m_oct_15,
                                   prec_all_yrs1000m_nov_15, prec_all_yrs1000m_dec_15)

#2018
prec_all_yrs1000m_aug_18 <- left_join(dhs_2018_aug, aug_prec_all_yrs1000m_18, by = c("hv001"="ID"))
prec_all_yrs1000m_sep_18 <- left_join(dhs_2018_sep, sep_prec_all_yrs1000m_18, by = c("hv001"="ID"))
prec_all_yrs1000m_oct_18 <- left_join(dhs_2018_oct, oct_prec_all_yrs1000m_18, by = c("hv001"="ID"))
prec_all_yrs1000m_nov_18 <- left_join(dhs_2018_nov, nov_prec_all_yrs1000m_18, by = c("hv001"="ID"))
prec_all_yrs1000m_dec_18 <- left_join(dhs_2018_dec, dec_prec_all_yrs1000m_18, by = c("hv001"="ID"))

prec_all_yrs1000m_DHS_18 <- dplyr::bind_rows(prec_all_yrs1000m_aug_18, prec_all_yrs1000m_sep_18, prec_all_yrs1000m_oct_18,
                                   prec_all_yrs1000m_nov_18, prec_all_yrs1000m_dec_18)

##final 1000m buffer 
prec_all_yrs1000m_DHS_10_15_18 <- dplyr::bind_rows(prec_all_yrs1000m_DHS_10, prec_all_yrs1000m_DHS_15, prec_all_yrs1000m_DHS_18)

prec_all_yrs1000m_DHS_10_15_18 <- prec_all_yrs1000m_DHS_10_15_18[,c("hv001", ".id", "prec_all_yrs_1000m", "dhs_year")]

write.csv(prec_all_yrs1000m_DHS_10_15_18, file.path(Covdir, paste0("prec_all_yrs_1000m_DHS_10_15_18.csv")),row.names = FALSE)




######## 2000m buffer##########

loadtemp.fun <- function(filename){
        read.csv(file.path(DataIn, filename),
                 header = T, sep = ',') %>% rename_at(4,~"prec_all_yrs_2000m")
}
#August
aug_prec_all_yrs2000m_10 <- loadtemp.fun("aug_prec_all_yrs2000m_buffer_DHS_10.csv")
aug_prec_all_yrs2000m_15 <- loadtemp.fun("aug_prec_all_yrs2000m_buffer_DHS_15.csv")        
aug_prec_all_yrs2000m_18 <- loadtemp.fun("aug_prec_all_yrs2000m_buffer_DHS_18.csv")

#SEPT
sep_prec_all_yrs2000m_10 <- loadtemp.fun("sep_prec_all_yrs2000m_buffer_DHS_10.csv")
sep_prec_all_yrs2000m_15 <- loadtemp.fun("sep_prec_all_yrs2000m_buffer_DHS_15.csv")        
sep_prec_all_yrs2000m_18 <- loadtemp.fun("sep_prec_all_yrs2000m_buffer_DHS_18.csv")

#Oct
oct_prec_all_yrs2000m_10 <- loadtemp.fun("oct_prec_all_yrs2000m_buffer_DHS_10.csv")
oct_prec_all_yrs2000m_15 <- loadtemp.fun("oct_prec_all_yrs2000m_buffer_DHS_15.csv")        
oct_prec_all_yrs2000m_18 <- loadtemp.fun("oct_prec_all_yrs2000m_buffer_DHS_18.csv")

#Nove
nov_prec_all_yrs2000m_10 <- loadtemp.fun("nov_prec_all_yrs2000m_buffer_DHS_10.csv")
nov_prec_all_yrs2000m_15 <- loadtemp.fun("nov_prec_all_yrs2000m_buffer_DHS_15.csv")        
nov_prec_all_yrs2000m_18 <- loadtemp.fun("nov_prec_all_yrs2000m_buffer_DHS_18.csv")

#Dec
dec_prec_all_yrs2000m_10 <- loadtemp.fun("dec_prec_all_yrs2000m_buffer_DHS_10.csv")
dec_prec_all_yrs2000m_15 <- loadtemp.fun("dec_prec_all_yrs2000m_buffer_DHS_15.csv")        
dec_prec_all_yrs2000m_18 <- loadtemp.fun("dec_prec_all_yrs2000m_buffer_DHS_18.csv")


#rename column with same variable name


###binding and merging 2000m buffer
#2010
prec_all_yrs2000m_aug_10 <- left_join(dhs_2010_aug, aug_prec_all_yrs2000m_10, by = c("hv001"="ID"))
prec_all_yrs2000m_sep_10 <- left_join(dhs_2010_sep, sep_prec_all_yrs2000m_10, by = c("hv001"="ID"))
prec_all_yrs2000m_oct_10 <- left_join(dhs_2010_oct, oct_prec_all_yrs2000m_10, by = c("hv001"="ID"))
prec_all_yrs2000m_nov_10 <- left_join(dhs_2010_nov, nov_prec_all_yrs2000m_10, by = c("hv001"="ID"))
prec_all_yrs2000m_dec_10 <- left_join(dhs_2010_dec, dec_prec_all_yrs2000m_10, by = c("hv001"="ID"))

prec_all_yrs2000m_DHS_10 <- dplyr::bind_rows(prec_all_yrs2000m_aug_10, prec_all_yrs2000m_sep_10, prec_all_yrs2000m_oct_10,
                                   prec_all_yrs2000m_nov_10, prec_all_yrs2000m_dec_10)

#2015
prec_all_yrs2000m_aug_15 <- left_join(dhs_2015_aug, aug_prec_all_yrs2000m_15, by = c("hv001"="ID"))
prec_all_yrs2000m_sep_15 <- left_join(dhs_2015_sep, sep_prec_all_yrs2000m_15, by = c("hv001"="ID"))
prec_all_yrs2000m_oct_15 <- left_join(dhs_2015_oct, oct_prec_all_yrs2000m_15, by = c("hv001"="ID"))
prec_all_yrs2000m_nov_15 <- left_join(dhs_2015_nov, nov_prec_all_yrs2000m_15, by = c("hv001"="ID"))
prec_all_yrs2000m_dec_15 <- left_join(dhs_2015_dec, dec_prec_all_yrs2000m_15, by = c("hv001"="ID"))

prec_all_yrs2000m_DHS_15 <- dplyr::bind_rows(prec_all_yrs2000m_aug_15, prec_all_yrs2000m_sep_15, prec_all_yrs2000m_oct_15,
                                   prec_all_yrs2000m_nov_15, prec_all_yrs2000m_dec_15)

#2018
prec_all_yrs2000m_aug_18 <- left_join(dhs_2018_aug, aug_prec_all_yrs2000m_18, by = c("hv001"="ID"))
prec_all_yrs2000m_sep_18 <- left_join(dhs_2018_sep, sep_prec_all_yrs2000m_18, by = c("hv001"="ID"))
prec_all_yrs2000m_oct_18 <- left_join(dhs_2018_oct, oct_prec_all_yrs2000m_18, by = c("hv001"="ID"))
prec_all_yrs2000m_nov_18 <- left_join(dhs_2018_nov, nov_prec_all_yrs2000m_18, by = c("hv001"="ID"))
prec_all_yrs2000m_dec_18 <- left_join(dhs_2018_dec, dec_prec_all_yrs2000m_18, by = c("hv001"="ID"))

prec_all_yrs2000m_DHS_18 <- dplyr::bind_rows(prec_all_yrs2000m_aug_18, prec_all_yrs2000m_sep_18, prec_all_yrs2000m_oct_18,
                                   prec_all_yrs2000m_nov_18, prec_all_yrs2000m_dec_18)

##final 2000m buffer 
prec_all_yrs2000m_DHS_10_15_18 <- dplyr::bind_rows(prec_all_yrs2000m_DHS_10, prec_all_yrs2000m_DHS_15, prec_all_yrs2000m_DHS_18)


prec_all_yrs2000m_DHS_10_15_18 <- prec_all_yrs2000m_DHS_10_15_18[,c("hv001", ".id", "prec_all_yrs_2000m", "dhs_year")]

write.csv(prec_all_yrs2000m_DHS_10_15_18, file.path(Covdir, paste0("prec_all_yrs_2000m_DHS_10_15_18.csv")),row.names = FALSE)



######## 3000m buffer##########
loadtemp.fun <- function(filename){
        read.csv(file.path(DataIn, filename),
                 header = T, sep = ',') %>% rename_at(4,~"prec_all_yrs_3000m")
}
#August
aug_prec_all_yrs3000m_10 <- loadtemp.fun("aug_prec_all_yrs3000m_buffer_DHS_10.csv")
aug_prec_all_yrs3000m_15 <- loadtemp.fun("aug_prec_all_yrs3000m_buffer_DHS_15.csv")        
aug_prec_all_yrs3000m_18 <- loadtemp.fun("aug_prec_all_yrs3000m_buffer_DHS_18.csv")

#SEPT
sep_prec_all_yrs3000m_10 <- loadtemp.fun("sep_prec_all_yrs3000m_buffer_DHS_10.csv")
sep_prec_all_yrs3000m_15 <- loadtemp.fun("sep_prec_all_yrs3000m_buffer_DHS_15.csv")        
sep_prec_all_yrs3000m_18 <- loadtemp.fun("sep_prec_all_yrs3000m_buffer_DHS_18.csv")

#Oct
oct_prec_all_yrs3000m_10 <- loadtemp.fun("oct_prec_all_yrs3000m_buffer_DHS_10.csv")
oct_prec_all_yrs3000m_15 <- loadtemp.fun("oct_prec_all_yrs3000m_buffer_DHS_15.csv")        
oct_prec_all_yrs3000m_18 <- loadtemp.fun("oct_prec_all_yrs3000m_buffer_DHS_18.csv")

#Nove
nov_prec_all_yrs3000m_10 <- loadtemp.fun("nov_prec_all_yrs3000m_buffer_DHS_10.csv")
nov_prec_all_yrs3000m_15 <- loadtemp.fun("nov_prec_all_yrs3000m_buffer_DHS_15.csv")        
nov_prec_all_yrs3000m_18 <- loadtemp.fun("nov_prec_all_yrs3000m_buffer_DHS_18.csv")

#Dec
dec_prec_all_yrs3000m_10 <- loadtemp.fun("dec_prec_all_yrs3000m_buffer_DHS_10.csv")
dec_prec_all_yrs3000m_15 <- loadtemp.fun("dec_prec_all_yrs3000m_buffer_DHS_15.csv")        
dec_prec_all_yrs3000m_18 <- loadtemp.fun("dec_prec_all_yrs3000m_buffer_DHS_18.csv")

###binding and merging 3000m buffer
#2010
prec_all_yrs3000m_aug_10 <- left_join(dhs_2010_aug, aug_prec_all_yrs3000m_10, by = c("hv001"="ID"))
prec_all_yrs3000m_sep_10 <- left_join(dhs_2010_sep, sep_prec_all_yrs3000m_10, by = c("hv001"="ID"))
prec_all_yrs3000m_oct_10 <- left_join(dhs_2010_oct, oct_prec_all_yrs3000m_10, by = c("hv001"="ID"))
prec_all_yrs3000m_nov_10 <- left_join(dhs_2010_nov, nov_prec_all_yrs3000m_10, by = c("hv001"="ID"))
prec_all_yrs3000m_dec_10 <- left_join(dhs_2010_dec, dec_prec_all_yrs3000m_10, by = c("hv001"="ID"))

prec_all_yrs3000m_DHS_10 <- dplyr::bind_rows(prec_all_yrs3000m_aug_10, prec_all_yrs3000m_sep_10, prec_all_yrs3000m_oct_10,
                                   prec_all_yrs3000m_nov_10, prec_all_yrs3000m_dec_10)

#2015
prec_all_yrs3000m_aug_15 <- left_join(dhs_2015_aug, aug_prec_all_yrs3000m_15, by = c("hv001"="ID"))
prec_all_yrs3000m_sep_15 <- left_join(dhs_2015_sep, sep_prec_all_yrs3000m_15, by = c("hv001"="ID"))
prec_all_yrs3000m_oct_15 <- left_join(dhs_2015_oct, oct_prec_all_yrs3000m_15, by = c("hv001"="ID"))
prec_all_yrs3000m_nov_15 <- left_join(dhs_2015_nov, nov_prec_all_yrs3000m_15, by = c("hv001"="ID"))
prec_all_yrs3000m_dec_15 <- left_join(dhs_2015_dec, dec_prec_all_yrs3000m_15, by = c("hv001"="ID"))

prec_all_yrs3000m_DHS_15 <- dplyr::bind_rows(prec_all_yrs3000m_aug_15, prec_all_yrs3000m_sep_15, prec_all_yrs3000m_oct_15,
                                   prec_all_yrs3000m_nov_15, prec_all_yrs3000m_dec_15)

#2018
prec_all_yrs3000m_aug_18 <- left_join(dhs_2018_aug, aug_prec_all_yrs3000m_18, by = c("hv001"="ID"))
prec_all_yrs3000m_sep_18 <- left_join(dhs_2018_sep, sep_prec_all_yrs3000m_18, by = c("hv001"="ID"))
prec_all_yrs3000m_oct_18 <- left_join(dhs_2018_oct, oct_prec_all_yrs3000m_18, by = c("hv001"="ID"))
prec_all_yrs3000m_nov_18 <- left_join(dhs_2018_nov, nov_prec_all_yrs3000m_18, by = c("hv001"="ID"))
prec_all_yrs3000m_dec_18 <- left_join(dhs_2018_dec, dec_prec_all_yrs3000m_18, by = c("hv001"="ID"))

prec_all_yrs3000m_DHS_18 <- dplyr::bind_rows(prec_all_yrs3000m_aug_18, prec_all_yrs3000m_sep_18, prec_all_yrs3000m_oct_18,
                                   prec_all_yrs3000m_nov_18, prec_all_yrs3000m_dec_18)

##final 3000m buffer 
prec_all_yrs3000m_DHS_10_15_18 <- dplyr::bind_rows(prec_all_yrs3000m_DHS_10, prec_all_yrs3000m_DHS_15, prec_all_yrs3000m_DHS_18)


prec_all_yrs3000m_DHS_10_15_18 <- prec_all_yrs3000m_DHS_10_15_18[,c("hv001", ".id", "prec_all_yrs_3000m", "dhs_year")]

write.csv(prec_all_yrs3000m_DHS_10_15_18, file.path(Covdir, paste0("prec_all_yrs_3000m_DHS_10_15_18.csv")),row.names = FALSE)




######## 4000m buffer##########
loadtemp.fun <- function(filename){
        read.csv(file.path(DataIn, filename),
                 header = T, sep = ',') %>% rename_at(4,~"prec_all_yrs_4000m")
}
#August
aug_prec_all_yrs4000m_10 <- loadtemp.fun("aug_prec_all_yrs4000m_buffer_DHS_10.csv")
aug_prec_all_yrs4000m_15 <- loadtemp.fun("aug_prec_all_yrs4000m_buffer_DHS_15.csv")        
aug_prec_all_yrs4000m_18 <- loadtemp.fun("aug_prec_all_yrs4000m_buffer_DHS_18.csv")

#SEPT
sep_prec_all_yrs4000m_10 <- loadtemp.fun("sep_prec_all_yrs4000m_buffer_DHS_10.csv")
sep_prec_all_yrs4000m_15 <- loadtemp.fun("sep_prec_all_yrs4000m_buffer_DHS_15.csv")        
sep_prec_all_yrs4000m_18 <- loadtemp.fun("sep_prec_all_yrs4000m_buffer_DHS_18.csv")

#Oct
oct_prec_all_yrs4000m_10 <- loadtemp.fun("oct_prec_all_yrs4000m_buffer_DHS_10.csv")
oct_prec_all_yrs4000m_15 <- loadtemp.fun("oct_prec_all_yrs4000m_buffer_DHS_15.csv")        
oct_prec_all_yrs4000m_18 <- loadtemp.fun("oct_prec_all_yrs4000m_buffer_DHS_18.csv")

#Nove
nov_prec_all_yrs4000m_10 <- loadtemp.fun("nov_prec_all_yrs4000m_buffer_DHS_10.csv")
nov_prec_all_yrs4000m_15 <- loadtemp.fun("nov_prec_all_yrs4000m_buffer_DHS_15.csv")        
nov_prec_all_yrs4000m_18 <- loadtemp.fun("nov_prec_all_yrs4000m_buffer_DHS_18.csv")

#Dec
dec_prec_all_yrs4000m_10 <- loadtemp.fun("dec_prec_all_yrs4000m_buffer_DHS_10.csv")
dec_prec_all_yrs4000m_15 <- loadtemp.fun("dec_prec_all_yrs4000m_buffer_DHS_15.csv")        
dec_prec_all_yrs4000m_18 <- loadtemp.fun("dec_prec_all_yrs4000m_buffer_DHS_18.csv")


#rename column with same variable name


###binding and merging 4000m buffer
#2010
prec_all_yrs4000m_aug_10 <- left_join(dhs_2010_aug, aug_prec_all_yrs4000m_10, by = c("hv001"="ID"))
prec_all_yrs4000m_sep_10 <- left_join(dhs_2010_sep, sep_prec_all_yrs4000m_10, by = c("hv001"="ID"))
prec_all_yrs4000m_oct_10 <- left_join(dhs_2010_oct, oct_prec_all_yrs4000m_10, by = c("hv001"="ID"))
prec_all_yrs4000m_nov_10 <- left_join(dhs_2010_nov, nov_prec_all_yrs4000m_10, by = c("hv001"="ID"))
prec_all_yrs4000m_dec_10 <- left_join(dhs_2010_dec, dec_prec_all_yrs4000m_10, by = c("hv001"="ID"))

prec_all_yrs4000m_DHS_10 <- dplyr::bind_rows(prec_all_yrs4000m_aug_10, prec_all_yrs4000m_sep_10, prec_all_yrs4000m_oct_10,
                                   prec_all_yrs4000m_nov_10, prec_all_yrs4000m_dec_10)

#2015
prec_all_yrs4000m_aug_15 <- left_join(dhs_2015_aug, aug_prec_all_yrs4000m_15, by = c("hv001"="ID"))
prec_all_yrs4000m_sep_15 <- left_join(dhs_2015_sep, sep_prec_all_yrs4000m_15, by = c("hv001"="ID"))
prec_all_yrs4000m_oct_15 <- left_join(dhs_2015_oct, oct_prec_all_yrs4000m_15, by = c("hv001"="ID"))
prec_all_yrs4000m_nov_15 <- left_join(dhs_2015_nov, nov_prec_all_yrs4000m_15, by = c("hv001"="ID"))
prec_all_yrs4000m_dec_15 <- left_join(dhs_2015_dec, dec_prec_all_yrs4000m_15, by = c("hv001"="ID"))

prec_all_yrs4000m_DHS_15 <- dplyr::bind_rows(prec_all_yrs4000m_aug_15, prec_all_yrs4000m_sep_15, prec_all_yrs4000m_oct_15,
                                   prec_all_yrs4000m_nov_15, prec_all_yrs4000m_dec_15)

#2018
prec_all_yrs4000m_aug_18 <- left_join(dhs_2018_aug, aug_prec_all_yrs4000m_18, by = c("hv001"="ID"))
prec_all_yrs4000m_sep_18 <- left_join(dhs_2018_sep, sep_prec_all_yrs4000m_18, by = c("hv001"="ID"))
prec_all_yrs4000m_oct_18 <- left_join(dhs_2018_oct, oct_prec_all_yrs4000m_18, by = c("hv001"="ID"))
prec_all_yrs4000m_nov_18 <- left_join(dhs_2018_nov, nov_prec_all_yrs4000m_18, by = c("hv001"="ID"))
prec_all_yrs4000m_dec_18 <- left_join(dhs_2018_dec, dec_prec_all_yrs4000m_18, by = c("hv001"="ID"))

prec_all_yrs4000m_DHS_18 <- dplyr::bind_rows(prec_all_yrs4000m_aug_18, prec_all_yrs4000m_sep_18, prec_all_yrs4000m_oct_18,
                                   prec_all_yrs4000m_nov_18, prec_all_yrs4000m_dec_18)

##final 4000m buffer 
prec_all_yrs4000m_DHS_10_15_18 <- dplyr::bind_rows(prec_all_yrs4000m_DHS_10, prec_all_yrs4000m_DHS_15, prec_all_yrs4000m_DHS_18)


prec_all_yrs4000m_DHS_10_15_18 <- prec_all_yrs4000m_DHS_10_15_18[,c("hv001", ".id", "prec_all_yrs_4000m", "dhs_year")]

write.csv(prec_all_yrs4000m_DHS_10_15_18, file.path(Covdir, paste0("prec_all_yrs_4000m_DHS_10_15_18.csv")),row.names = FALSE)

