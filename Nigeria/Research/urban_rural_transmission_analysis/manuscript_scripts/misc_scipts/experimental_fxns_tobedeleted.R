
####Temperature

dhs_hh <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL|NGPR71FL|NGPR61FL', read_dta)  #reads in the PR files
dhs <- read.files(DataDir, "*FL.shp$", 'NGGE61FL|NGGE71FL|NGGE7BFL', shapefile) #read in DHS clusters 
dhs <- map(dhs, st_as_sf) %>%  map(~filter(.x, URBAN_RURA == "U")) #%>% map(sf:::as_Spatial)




######## Survey monthS fxn shopefile, 1 for 2010, 2 for 2015, and 3 for 2018
srvymnth.fun <- function(list_id, srvymnth){
  hh_survey <- dhs_hh[[list_id]]%>% filter(hv025 ==1)
  hh_survey <- hh_survey[!duplicated(hh_survey$hv001),]
  hh_survey <- hh_survey[,c("hv001", "hv006")] %>% rename_at(1,~"DHSCLUST")%>% filter(hv006 == srvymnth)
  merge(dhs[[list_id]], hh_survey)  %>% (sf:::as_Spatial)
  
}

#August
dhs_2018_aug <- srvymnth.fun(3,8) 

#septemmber
dhs_2010_sep <- srvymnth.fun(2,12)
dhs_2018_sep <- srvymnth.fun(3,9)

#Oct
dhs_2010_oct <- srvymnth.fun(1,10) 
dhs_2015_oct <- srvymnth.fun(2,10) 
dhs_2018_oct <- srvymnth.fun(3,10)

#nov
dhs_2010_nov <- srvymnth.fun(1,11) 
dhs_2015_nov <- srvymnth.fun(2,11) 
dhs_2018_nov <- srvymnth.fun(3,11)

#dec
dhs_2010_dec <- srvymnth.fun(1,12) 
dhs_2018_dec <- srvymnth.fun(3,12)

dhs <- list(dhs_2018_aug, dhs_2018_sep, dhs_2010_oct, dhs_2015_oct, dhs_2018_oct, dhs_2010_nov,
              dhs_2015_nov, dhs_2018_nov, dhs_2010_dec, dhs_2018_dec)

raster_2018_aug <- raster(file.path(RastDir, "temperature_monthly", "2018", "all_years_pre_2019_temperature",
                                    "all_years_pre_2019_temperature_month_08.tif"))
raster_2018_sep <- raster(file.path(RastDir, "temperature_monthly", "2018", "all_years_pre_2019_temperature",
                                    "all_years_pre_2019_temperature_month_09.tif"))
raster_2010_oct <- raster(file.path(RastDir, "temperature_monthly", "2010", "all_years_pre_2011_temperature",
                                    "all_years_pre_2011_temperature_month_10.tif"))
raster_2015_oct <- raster(file.path(RastDir, "temperature_monthly", "2015", "all_years_pre_2016_temperature",
                                   "all_years_pre_2016_temperature_month_10.tif"))
raster_2018_oct <- raster(file.path(RastDir, "temperature_monthly", "2018", "all_years_pre_2019_temperature",
                                    "all_years_pre_2019_temperature_month_10.tif"))
raster_2010_nov <- raster(file.path(RastDir, "temperature_monthly", "2010", "all_years_pre_2011_temperature",
                                    "all_years_pre_2011_temperature_month_11.tif"))
raster_2015_nov <- raster(file.path(RastDir, "temperature_monthly", "2015", "all_years_pre_2016_temperature",
                                    "all_years_pre_2016_temperature_month_11.tif"))
raster_2018_nov <- raster(file.path(RastDir, "temperature_monthly", "2018", "all_years_pre_2019_temperature",
                                    "all_years_pre_2019_temperature_month_11.tif"))
raster_2010_dec <- raster(file.path(RastDir, "temperature_monthly", "2010", "all_years_pre_2011_temperature",
                                    "all_years_pre_2011_temperature_month_12.tif"))
raster_2018_dec <- raster(file.path(RastDir, "temperature_monthly", "2018", "all_years_pre_2019_temperature",
                                    "all_years_pre_2019_temperature_month_12.tif"))

raster <- list(raster_2018_aug, raster_2018_sep, raster_2010_oct, raster_2015_oct, raster_2018_oct,
              raster_2010_nov, raster_2015_nov, raster_2018_nov, raster_2010_dec, raster_2018_dec)

raster <-sapply(files, raster, simplify = F)

vars <- c(0, 1000)

for (i in 1:length(vars)) {
  df <- map2(dhs, raster, get_crs)
  df <- pmap(list(raster, df, vars[i]), extract_fun)
  df <- plyr::ldply(df)
  var_name <- paste0('temp_exp_', as.character(vars[i]), 'm')
  df <- extrclean.fun(df, var_name)
  write.csv(df, file = file.path(GeoDir, paste0('temp_exp_', as.character(vars[i]), 
                                                'm_buffer', "_DHS_10_15_18.csv")),row.names = FALSE)
}
