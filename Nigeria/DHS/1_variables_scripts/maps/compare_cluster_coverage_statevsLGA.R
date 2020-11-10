library(nngeo)
library(sf)


plot(stateshp)


pts <- NGAshplist[[8]]
key <- key_list[[8]] %>%dplyr::select(DHSCLUST, LGA) %>%  group_by(LGA) %>%  summarise(num_cluster =n()) %>%  drop_na() %>% dplyr::select(-geometry) %>% 
head(key)
LGA_cluster_num <- left_join(LGA_clean_names, key) %>%  mutate(cluster_cat = ifelse(num_cluster<=1|is.na(num_cluster), 1, NA))
head(LGA_cluster_num, 10)

# p2 = sf::st_join(pts, LGA, join = st_nn, maxdist = 10)
# l = st_connect(pts, LGA)
# plot(st_geometry(LGA))
# plot(st_geometry(pts), add = TRUE)
# plot(st_geometry(l), col = "red", lwd = 2, add = TRUE)

map <- tm_shape(state_sf)+
  tm_fill(col = "grey80")+ 
  tm_borders(col = "grey40", lwd = 1, lty = "solid", alpha = 1)+
  tm_shape(pts)+ 
  tm_dots(size = 0.05, col = "red", border.col = "black")
  

map_2 <- tm_shape(LGA_cluster_num )+
  tm_fill(col = "cluster_cat")+ 
  tm_borders(col = "grey40", lwd = 1, lty = "solid", alpha = 1)+
  tm_shape(pts)+ 
  tm_dots(size = 0.05, col = "red", border.col = "black") 

arrange_maps <- tmap_arrange(map, map_2)
arrange_maps 

tmap_save(tm =arrange_maps, filename = file.path(ProjectDir, "project_notes/publication/cluster_coverage_comparsion_statevsLGA_2018.pdf"), width=13, height=13, units ="in", asp=0,
                     paper ="A4r", useDingbats=FALSE)


#Burkina

DS_shape<- readOGR("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")
plot(DS_shape)

DS_shape_sf <- st_as_sf
head(DS_shape_sf)


province <- readOGR("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_shapefiles/BFA_adm_shp", layer ="BFA_adm2",
                    use_iconv=TRUE, encoding= "UTF-8")
plot(province)

pts <- st_as_sf(BFshplist_sf[[6]])
head(pts)


map <- tm_shape(province)+
  tm_fill(col = "grey95")+ 
  tm_borders(col = "grey40", lwd = 1, lty = "solid", alpha = 1)+
  tm_shape(pts)+ 
  tm_dots(size = 0.1, col = "red", border.col = "black")


map_2 <- tm_shape(DS_shape_sf)+
  tm_fill(col = "grey95")+ 
  tm_borders(col = "grey40", lwd = 1, lty = "solid", alpha = 1)+
  tm_shape(pts)+ 
  tm_dots(size = 0.1, col = "red", border.col = "black")

arrange_maps <- tmap_arrange(map, map_2)

tmap_save(tm =arrange_maps, filename = file.path(ProjectDir, "project_notes/publication/cluster_coverage_comparison_BFA_provincevsDistrict_2018.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

head(key_list)
key <- key_list[[7]] %>% drop_na() %>%  group_by(LGA) %>% summarise(n())
summary(key$`n()`)

