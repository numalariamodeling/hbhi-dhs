dir.create(file.path(ResultDir, "MMC_presentation"), showWarnings = TRUE)
print_path <- file.path(ResultDir, "MMC_presentation")


NGAshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_0", use_iconv=TRUE, encoding= "UTF-8")
plot(NGAshp, col = "seagreen3")

LGAshp <- readOGR(file.path(DataDir,"Nigeria_LGAs_shapefile_191016"), layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
plot(LGAshp, col = "seagreen3")
LGA_sf <- st_as_sf(LGAshp)
head(LGA_sf)


map <- tm_shape(LGA_sf)+
  tm_polygons(col = "LGA", palette = "Paired")+ 
  tmap_options(max.categories = 774)

tmap_save(tm =map, filename = paste0(print_path,"/Nigeria_LGA_colors.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


Stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(Stateshp)
head(state_sf)
map <- tm_shape(state_sf)+
  tm_polygons(col = "NAME_1", palette = "viridis")+ 
  tmap_options(max.categories = 37)+
  tm_layout(legend.show=F)+ 
 tm_text("NAME_1")
tmap_save(tm =map, filename = paste0(print_path,"/Nigeria_state_names.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


scenarios <- read_csv(paste0(ProjectDir, "/scenarios/NGA _scenarios_for_mapping.csv"))
head(scenarios)

LGA_scenarios <- left_join(LGA_sf, scenarios)

map <- tm_shape(LGA_scenarios)+
  tm_polygons(col = "Questions", palette = "viridis")+ 
  tmap_options(max.categories = 37)+
  tm_layout(legend.show=T)
tmap_save(tm =map, filename = paste0(print_path,"/Nigeria_questions.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


#repDS map

LGA <- left_join(LGA_clean_names, rep_DS)

map <- tm_shape(LGA)+
  tm_polygons(col = "repDS", palette = "Accent")+ 
  tmap_options(max.categories = 37)+
  tm_layout(legend.show=T)
tmap_save(tm =map, filename = paste0(print_path,"/Nigeria_repDS_map.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



pdf(file=paste0(print_path,"/", "Nigeria.pdf"))
plot(Stateshp, col = "seagreen3")
dev.off()

tmaptools::palette_explorer()

