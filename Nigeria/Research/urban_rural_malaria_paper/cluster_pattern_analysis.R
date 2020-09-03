# clean and select pfpr data 

pfpr_data <- dhs_list[[3]] # uses the DHS person recode dataset 

look_for(dhs_list[[3]], "smear")

table(pfpr_data$hml32) # frequency table for smear test 

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_data <- pfpr_data %>% filter(hv042 == "1" & hv103 == "1" & hml32 %in% c(0, 1))
dim(pfpr_data)

# joins pfpr data to admin 1 data 

pfpr_df <- left_join(pfpr_data, key_2018)
head(pfpr_df)
head(key_2018)
summary(is.na(pfpr_df$hv001))

summary(pfpr_df$hv005/1000000)


# prep dataset for cluster level analysis 

val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural

pfpr_place <- pfpr_df %>% filter(hv025 == 1)
head(pfpr_place)

wt_cluster <- pfpr_place %>% group_by(hv001) %>%  distinct(hv005)
head(wt_cluster)

pfpr_place<- dataclean.para(pfpr_place, hv005, hc1, hml32, 'hml32', 'p_test') 


svyd18 <- svydesign.fun(pfpr_place)



clu_est <- result.fun('p_test', 'hv001', design=svyd18, pfpr_place, "hv007")
head(clu_est)

# write.csv(ds_est, 'bin/ds_para_prevalence_2018.csv')

clu_est <- left_join(clu_est, wt_cluster, by = "hv001")

clu_high <- clu_est[clu_est$p_test > 0.1, ]
head(clu_high)

# cluster location 
sf.ng.shp <- st_as_sf(NGAshpfiles) #converts the DHS cluster sf object 
head(sf.ng.shp)

ng.place <- sf.ng.shp %>%  filter(URBAN_RURA == "U") #can change to rural or urban 
head(ng.place)

colnames(clu_high)[1] <- "DHSCLUST"

pfpr_place <- left_join(clu_high, ng.place, by = "DHSCLUST")
head(pfpr_place)

###### stop here if doing only KS test, G function test etc 
place_shp <- st_as_sf(pfpr_place) # the pfpr dataset is converted to an sf object using geometery 
head(place_shp)


place_ppp<- as(place_shp, "Spatial") #converts to spatial points data frame 

place_ppp <- as(place_ppp, "ppp") #creates point layer in a ppp format 

npoints(place_ppp)

place_ppp$n #check

head(pfpr_place) #check

table(pfpr_place$URBAN_RURA)#check



#point pattern analysis 

Window(place_ppp) <- ad #binds admin 1 window to pfpr points data 

plot(place_ppp, which.marks="p_test", main=NULL, cols=rgb(0,0,0,.2), pch=20)

wt<-place_ppp

marks(place_ppp)  <- NULL #removes attribute data 


#converts the admin2 file to a raster based on shape area and then a tessllated object 

template <- raster(ext = extent(LGA_sf_2), crs = projection(LGA_sf_2)) #having to use state-level raster instead of LGA because of no shape area column 

nga_rst <- rasterize(LGA_sf_2, template, field = "area_sq_km")

plot(nga_rst)


E <- tess(image=nga_rst) #create a polygon with shapes that don't overlap 

plot(E, main="", las = 1) #plots tessllated object, las = 1 is for position of labels 

Q <- quadratcount(place_ppp, tess = E)  # computes the number of points per quadrat area,i.e number of points in each admin unit
Q.d <- intensity(Q) # Compute density as the number of points per unit area 
Q.d


plot(intensity(Q, image=TRUE), las=1, main=NULL)
plot(place_ppp, pch=20, cex=0.6, col=rgb(1,1,1,.5), add=TRUE)

cl <-  interp.colours(c("lightyellow", "orange" ,"red"), E$n)
plot(intensity(Q, image=TRUE), las=1, col=cl, main=NULL)
plot(place_ppp, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE, title("High transmission cluster point pattern"))


#kernel density 
K1 <- density(place_ppp) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)


# Compute rho using the ratio method
rho <- rhohat(place_ppp,lgt.lg, method="ratio",weights=wt$marks$hv005/1000000, positiveCI = TRUE) # weights=wts$marks$hv005 


# Generate rho vs covariate plot
plot(rho, las=1, main="High transmission cluster normalized density", 
     legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0)))


#predicted density controlled by pop density 
pred <- predict(rho)
cl<- interp.colours(c("lightyellow", "orange" ,"red"), 100) # Create color scheme
plot(pred, col=cl, las=1, main= "Predicted high transmission cluster intensity")


# Create the Poisson point process model
PPM1 <- ppm(place_ppp ~ lgt.lg)
# Plot the relationship
plot(effectfun(PPM1, "lgt.lg", se.fit=TRUE), main=NULL, las=1, legendargs=list(cex=0.8, xpd=TRUE, inset=c(1.01, 0) ))

#compares the bivriate model with the null model 
PPM0 <- ppm(place_ppp ~ 1)
PPM0

anova(PPM0, PPM1, test="LRT")

# test of complete spatial randomness (CSR)

# prepare the data 
place_shp <- st_as_sf(pfpr_place) %>% dplyr::select(geometry, p_test)
head(place_shp)


place_ppp<- as(place_shp, "Spatial") #converts to spatial points data frame 

place_ppp <- as(place_ppp, "ppp") #creates point layer in a ppp format 

Window(place_ppp) <- ad

marks(place_ppp)

plot(place_ppp)

#CSR chi-square test with state level quadrat using MonteCarlo
quadrat.test(place_ppp, tess = E, method = "MonteCarlo")


# Kolmogorov-Smirnov test for CSR 

ks <- cdf.test(place_ppp, "x")
plot(ks)


pval <- ks$p.value
pval

ds <- density(place_ppp)
plot(ds)
k <- cdf.test(place_ppp,ds)
plot(k)

# g function #measures the distribution of distances from an arbitrary event to its nearest event
gtest <- Gest(place_ppp)
gtest

plot(gtest)

# f function #measures the distribution of all distances from an arbitrary point to its nearest event (i.e. uses empty space distances)

ftest <- Fest(place_ppp)
ftest

plot(ftest)


# K function measures the number of events found up to a given distance of any particular event (i.e. uses pairwise distances)

ktest <- Kest(place_ppp)
ktest

plot(ktest, lwd =2)


# another way to test for clustering/dispersion 

ann.p <- mean(nndist(place_ppp, k=1))
ann.p

n <- 599L               # Number of simulations
ann.r <- vector(length = n) # Create an empty object to be used to store simulated ANN values
for (i in 1:n){
  rand.p   <- rpoint(n=place_ppp$n, win=ad)  # Generate random point locations
  ann.r[i] <- mean(nndist(rand.p, k=1))  # Tally the ANN values
}


plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")


n <- 599L
ann.r <- vector(length=n)
for (i in 1:n){
  rand.p   <- rpoint(n=place_ppp$n, f=img) 
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

Window(rand.p) <- ad  # Replace raster mask with ad window
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

png(filename = "results/malaria_DHS_paper/CSR_plots/urban_montecarlo.png")
hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")
dev.off()
