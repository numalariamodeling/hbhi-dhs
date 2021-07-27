library(raster); library(readr); library(sp)

####read csv of health facilities and convert to sp dataframe 
Mydata <- read.csv("C:/Users/ido0493/Box/NU-malaria-team/data/africa_health_facility_locations/Burkina Faso_facilities_DS.csv", header = T, sep = ",")

xy <- Mydata[,c(6,7)]

hf_shp <- SpatialPointsDataFrame(coords = xy, data = Mydata,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

####read in health district values and transform project to long and lat 
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

DS_shape_W <- spTransform(DS_shape,crs(hf_shp))

####read in the medical treatment points folder 
pts_m14 <- readOGR("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2014/shape", layer = "medtreat_2014_cluster")
pts_m17 <- readOGR("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2017/shape", layer = "medtreat_2017_cluster")

####quality checks for the shape files 
plot(DS_shape_W)
plot(hf_shp, add=T, col=4)
plot(pts_m14, add=T)


####spatial overlay to grab the names of the district 
key_hf <- over(hf_shp, DS_shape_W [, "NOMDEP"])
CRS.new <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
proj4string(pts_m14) <- CRS.new
proj4string(DS_shape_W) <- CRS.new

key_pm14 <- over(pts_m14, DS_shape_W [, "NOMDEP"])



crs(pts_m14)
crs(DS_shape_W)
crs(hf_shp)

####add the names of the district to the health facility 
hf_shp$NOMDEP <- key_hf$NOMDEP


