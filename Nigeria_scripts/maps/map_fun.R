clean_LGA <- function(filepath, namefilepath) {
  
  LGAshp <- readOGR(filepath, layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
  
  LGAshp_sf <- st_as_sf(LGAshp)
  
  LGA_names <- read.csv(namefilepath)
  
  left_join(LGAshp_sf, LGA_names, by=c("LGA" = "LGA_shape")) %>% 
    mutate(LGA = str_replace_all(LGA, "/", "-"), LGA = case_when(LGA == "kiyawa"~ "Kiyawa", LGA == "kaita" ~"Kaita", TRUE ~ LGA)) 
  
}



map_fun <- function(shpfile, map_val, var) {
  year <- unique(na.omit(shpfile$year))
  tm_shape(shpfile) + #this is the health district shapfile with LLIn info
    tm_polygons(col = map_val, textNA = "No data", 
                title = "", palette = "seq", breaks=c(0, 0.1, 0.2, 
    0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
    tm_layout(title =paste0(year, " ", var, " ", "dhs"),
              aes.palette = list(seq="RdYlBu")) 
}
