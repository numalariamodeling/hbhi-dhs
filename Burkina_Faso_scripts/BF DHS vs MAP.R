setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs/MAP_District_Summary_Shapefiles") 
setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs") 

#functions 
IDW.fun <- function(col, legtitle){
  tm_shape(BFshplist_mapl) + #this is the health district shapfile with LLIn info
    tm_polygons(col = col, textNA = "No data",
                title = legtitle, palette = "seq", breaks=c(0, 0.2, 0.3,
                                                            0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(aes.palette = list(seq="-RdYlBu"))
}

DHS.fun <- function(DSshape, col,legtitle){
  tm_shape(DSshape) + #this is the health district shapfile with LLIn info
    tm_polygons(col = col, textNA = "No data",
                title = legtitle, palette = "seq", breaks=c(0, 0.2, 0.3,
                                                            0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
    tm_layout(aes.palette = list(seq="-RdYlBu"))
}

#reading in files nad converting to sf object 
BFshpfiles_map <- list.files(pattern="\\.shp$", full.names=TRUE, recursive=F)
BFshplist_map<-sapply(BFshpfiles_map,shapefile, simplify = F)

BFshplist_mapl <-st_as_sf(BFshplist_map[[2]])


BF2010_dhs<-read_csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/parasitemia/pos_test_BF10DS.csv")
BF2014_dhs<-read_csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/parasitemia/pos_test_BF14DS.csv")
BF2018_dhs<-read_csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/parasitemia/2017/pfpr no time/para2017_microscopy.csv")

DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

DS_shape <-st_as_sf(DS_shape)


dhs_2010.BF <-DS_shape%>%left_join(BF2010_dhs)
dhs_2014.BF <-DS_shape%>%left_join(BF2014_dhs)
dhs_2018.BF <-DS_shape%>%left_join(BF2018_dhs)



#plotting and saving 
map_dhs_2010BF <- DHS.fun(dhs_2010.BF, "p_test", "BF DHS survey points, 2010")
map_dhs_2014BF <- DHS.fun(dhs_2014.BF, "p_test", "BF DHS survey points, 2014")
map_dhs_2018BF <- DHS.fun(dhs_2018.BF, "PfPr", "BF DHS survey points, 2018")


IDW_pfpr2010 <- IDW.fun("map2_2010", "BF MAP survey points, 2010")
IDW_pfpr2014 <- IDW.fun("map2_2014", "BF MAP survey points, 2014")
IDW_pfpr2018 <- IDW.fun("map2_2018",  "BF MAP survey points, 2018")


MAPvDHS <-tmap_arrange(map_dhs_2010BF, IDW_pfpr2010, map_dhs_2014BF, IDW_pfpr2014, map_dhs_2018BF,IDW_pfpr2018)

tmap_save(tm = MAPvDHS, filename = "MAPvsDHSPfPr.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)

