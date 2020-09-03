# point pattern analysis for pfpr


pfpr_data <- dhs_list[[3]] # uses the DHS person recode dataset 

look_for(dhs_list[[3]], "smear")

table(pfpr_data$hml32) # frequency table for smear test 

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_data <- pfpr_data %>% filter(hv042 == "1" & hv103 == "1" & hml32 %in% c(0, 1))
dim(pfpr_data)

# joins pfpr data to admin 1 data 

pfpr_df <- left_join(pfpr_data, key_2018)
dim(pfpr_df)
summary(is.na(pfpr_df$hv001))

summary(pfpr_df$hv005/1000000)


# prepare point pattern analysis files for rural areas 

val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural

pfpr_place <- zap_labels(pfpr_df %>% filter(hv025 == 2, hml32 == 1) %>%  dplyr::select(hv001, hml32, hv005))

table(pfpr_place$hml32)

pfpr_place$hv005 <- pfpr_place$hv005/1000000
                

#hml32 == 1 is positive smear test, can change to urban and rural by changing values 
head(pfpr_place)
table(pfpr_place$hml32)

sf.ng.shp <- st_as_sf(NGAshpfiles) #converts the DHS cluster sf object 
head(sf.ng.shp)

ng.place <- sf.ng.shp %>%  filter(URBAN_RURA == "R") #can change to rural or urban 
head(ng.place)

colnames(ng.place)[4] <- "hv001"


#the pfpr dataset inherits the geometry from the cluster points
pfpr_place <- left_join(pfpr_place, ng.place, by = "hv001")
head(pfpr_place)

place_shp <- st_as_sf(pfpr_place) # the pfpr dataset is converted to an sf object using geometery 
head(place_shp)


place_ppp<- as(place_shp, "Spatial") #converts to spatial points data frame 

place_ppp <- as(place_ppp, "ppp") #creates point layer in a ppp format 

place_ppp$n #check

head(pfpr_place) #check

table(pfpr_place$URBAN_RURA)#check




#point pattern analysis 

Window(place_ppp) <- ad #binds admin 1 window to pfpr points data 

plot(place_ppp, which.marks="hml32", main=NULL, cols=rgb(0,0,0,.2), pch=20)

wt<-place_ppp

marks(place_ppp)  <- NULL #removes attribute data 


#converts the admin1 file to a raster based on shape area and then a tessllated object 

template <- raster(ext = extent(admin1shp), crs = projection(admin1shp)) #having to use state-level raster instead of LGA because of no shape area column 

nga_rst <- rasterize(admin1shp, template, field = "Shape_Area")

plot(nga_rst)



E <- tess(image=nga_rst) #create a polygon with shapes that don't overlap 

plot(E, main="", las = 1) #plots tessllated object, las = 1 is for position of labels 

Q <- quadratcount(place_ppp, tess = E)  # computes the number of points per quadrat area,i.e number of points in each admin 1 unit
Q.d <- intensity(Q) # Compute density as the number of points per unit area 
Q.d


plot(intensity(Q, image=TRUE), las=1, main=NULL)
plot(place_ppp, pch=20, cex=0.6, col=rgb(1,1,1,.5), add=TRUE)

cl <-  interp.colours(c("lightyellow", "orange" ,"red"), E$n)
plot( intensity(Q, image=TRUE), las=1, col=cl, main=NULL)
plot(place_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE, title("Rural point pattern"))

#kernel density 
K1 <- density(place_ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)


# Compute rho using the ratio method
rho <- rhohat(place_ppp, pop.lg, method="ratio",  weights=wt$marks$hv005) # weights=wts$marks$hv005 



# Generate rho vs covariate plot
plot(rho, las=1, main="Urban Positive PfPR density vs Population density", 
     legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0)))

#predicted density controlled by pop density 
pred <- predict(rho)
cl<- interp.colours(c("lightyellow", "orange" ,"red"), 100) # Create color scheme
plot(pred, col=cl, las=1, main= "Urban Positive PfPR density controlled for nightime lights")


# Create the Poisson point process model
PPM1 <- ppm(place_ppp ~ im_lgt)
# Plot the relationship
plot(effectfun(PPM1, "im_lgt", se.fit=TRUE), main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))
