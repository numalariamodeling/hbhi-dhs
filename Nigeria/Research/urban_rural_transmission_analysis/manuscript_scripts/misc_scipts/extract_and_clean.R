## These scripts are used to extract cluster level data and variables for urban settings in Nigeria 
rm(list=ls())
memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
GeoDir <- file.path(Rdata, "geospatial_covariates")
ResultDir <-file.path(ProjectDir, "results")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
dwnlds <- NuDir <- file.path(Drive, "Downloads", "rasters")



# -----------------------------------------
### Required functions and settings
## -----------------------------------------
source(file.path(SrcDir, "functions", "Nigeria functions.R"))
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed



#read in DHS clusters 

dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 
dhs <- map(dhs, st_as_sf) %>%  map(~filter(.x, URBAN_RURA == "U")) %>% map(sf:::as_Spatial)

#fxn for the range for the daata points for 2010 clusters

clean2010.fun <- function(csv, varname){
  csv 
  dff <- csv
  dff %>% mutate(row_names = row.names(dff))%>%
    subset(row_names %in% c(1:81)) %>% rename_at(3,~varname)%>%
    subset(select = c(2, 1, 3)) %>% mutate(dhs_year = 2010)%>% rename_at(1,~"hv001")
}

clean2015.fun <- function(csv, varname){
  csv 
  dff <- csv
  dff %>% mutate(row_names = row.names(dff))%>%
    subset(row_names %in% c(82:(81+136))) %>% rename_at(4,~varname)%>%
    subset(select = c(2, 1,3)) %>% mutate(dhs_year = 2015)%>% rename_at(1,~"hv001")
}

clean2018.fun <- function(csv, varname){
  csv 
  dff <- csv
  dff %>% mutate(dhs_year = regmatches(names(dhs), regexpr('20[0-3]+', names(dhs)))) %>%
    subset(select = c(2, 1, 3))%>% rename_at(1,~"hv001")%>% rename_at(2,~varname)
}

bind.fun <- function(df1, df2, df3){
  rbind(df1, df2) 
}

extrclean.fun <- function(csv, varname){
  csv 
  dff <- csv
  dff %>% mutate(row_names = row.names(dff))%>% rename_at(4,~varname)%>%
    subset(select = c(2, 1, 3)) %>% mutate(dhs_year = 2015) 
}

extract_fun2 <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>% mutate(dhs_year = dhs$DHSYEAR)%>% 
    rename_at(3,~"varname") %>% subset(select = c(2, 3, 4))
  
}

extract_fun <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>% mutate(dhs_year = dhs$DHSYEAR)%>% 
    mutate(hv001 = dhs$DHSCLUST)%>% rename_at(3,~"varname")%>% subset(select = c(4, 2, 3))
  
}

extrclean.fun <- function(csv, varname){
  csv 
  dff <- csv
  dff %>% rename_at(2,~varname)%>% rename_at(1,~"hv001")
}

extrclean2.fun <- function(csv, varname){
  csv 
  dff <- csv
  dff %>% 
  dff %>% rename_at(2,~varname)%>% rename_at(1,~"hv001")
  
}

# pop density extraction with general FB data 

files <- list.files(path = file.path(dwnlds) , pattern = "*NGA.tiff$", full.names = TRUE, recursive = TRUE)
files<- files[(grep('2010_Dominant', files))]
raster<-sapply(files, raster, simplify = F)


vars <- c(0, 1000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df) 
  var_name <- paste0('dom_vec_', as.character(vars[i]), 'm')
  df <- extrclean.fun(df, var_name)
  write.csv(df, file =file.path(dwnlds, paste0('dom_vec_', as.character(vars[i]), 
                                                'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
}

vars <- c(0)

#pop density raster files
files <- list.files(path = file.path(dwnlds) , pattern = "*deg.tif$", full.names = TRUE, recursive = TRUE)
files<- files[(grep('gpw_v4', files))]
raster<-sapply(files, raster, simplify = F)



# pop density extraction with just columbia data 
vars <- c(0, 1000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  var_name <- paste0('pop_den_', as.character(vars[i]), 'm')
  df <- extrclean.fun(df, var_name)
  write.csv(df, file =file.path(dwnlds, paste0('pop_density_', as.character(vars[i]), 'm_buffer', "_DHS_10_15_18.csv")))
}


