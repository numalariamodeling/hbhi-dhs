#############################################################################################
####
#### Example code for using Small Area Estimation for smooth estimates of ITN in Burkina Faso using 2014 and 2017 MIS
####
#### compares three models
#### 
#### Last Updated: Ocotober 4, 2018 
####
#### Author: Ifeoma Ozodiegwu 
####

#################################
# -- clearing out the memory -- #
#################################
rm(list=ls())

#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)

#update.packages("INLA", dep=TRUE)
############################################
# -- Reading in the libraries Libraries -- #
############################################
# I can't remember if these are dat necessary....
library(INLA);library(dplyr);library(readr)
library(maptools); library(plotrix); library(raster);library(sp)
library(classInt); library(RColorBrewer); 
library(grDevices); library(rgdal)
library(mapproj); library(lubridate); library(grid)
library(gridExtra);library(spdep); library(rgeos)


#######################################
# -- Setting the working directory -- #
#######################################
setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs")



################################
# -- Read in the GPS points -- #
################################
pts_14 <-readOGR("BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL")
pts_17 <- readOGR("BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL", layer = "BFGE7AFL")

################################
# -- Read in the Shapefiles -- #
################################
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

DS_merge<-data.frame(NOMDEP=DS_shape@data[,"NOMDEP"])

#DS is in the utm + zone 30 & points is in long + lat. lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,crs(pts_14))


#check map projections again 
crs(DS_shape_W)
crs(pts_14)


#Check if the GPS points  are contained in the polygons 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2014 clusters')
plot(pts_14,add=T,col=4) #this is dhs 14


# adding a row number to the shape2 object, will be handy for plotting later on
DS_shape_W@data$row_num<-1:nrow(DS_shape_W@data)


# create a key for making sure the LGAs are in the right order for analysis and plotting
key<-DS_shape_W@data[,c("NOMDEP","row_num")]


##########################################################
# -- read in the horvitz thompason processed DHS data -- #
##########################################################

dat<-read_csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/ITN/LLIN_use_BF14DS.csv")
dat_A <- read_csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/ITN/LLIN_adult_use_BF14DS.csv")
dat2 <- read_csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/ITN/LLIN_use_BF17DS.csv")
dat2_A <- read_csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/ITN/LLIN_adult_use_BF17DS.csv")


ITN14<-DS_merge%>%left_join(dat)
ITN14_A<-DS_merge%>%left_join(dat_A)
ITN17<-DS_merge%>%left_join(dat2)
ITN17_A<-DS_merge%>%left_join(dat2_A)



# handy functions for transforming the data
logit<-function(x){
  log(x/(1-x))
}


expit<-function(x){
  exp(x)/(1+exp(x))
}

#######################################################################
# -- Setting up the transformed variables, variance=0 is a problem -- #
#######################################################################
summary(ITN14$se)
summary(ITN14_A$se)
summary(ITN17$se)
summary(ITN17_A$se)


ITN_transform <- function(filename) {
  file2<-filename%>%mutate(logit_ITN=ifelse(!is.na(LLIN_use),logit(LLIN_use),NA),
                          var_logit_ITN=(se^2)/(LLIN_use^2*(1-LLIN_use)^2),
                          var_logit_ITN=ifelse(se<0.00001,NA,var_logit_ITN),
                          logit_ITN=ifelse(is.na(var_logit_ITN),NA,logit_ITN))
}

ITN14_A_ <- ITN_transform(ITN14_A)
ITN17_ <- ITN_transform(ITN17)
ITN17_A_ <- ITN_transform(ITN17_A)

summary(ITN14_A_$var_logit_ITN)
summary(ITN17_A_$var_logit_ITN)
summary(ITN17_$se)

# merge in key and put data in right order
dim(ITN14_1)
dat<-ITN14_1%>%left_join(key)%>%arrange(row_num)
dim(ITN14_1)

dim(ITN14_A_)
dat<-ITN14_A_%>%left_join(key)%>%arrange(row_num)
dim(ITN14_A_)

dim(ITN17_)
dat<-ITN17_%>%left_join(key)%>%arrange(row_num)
dim(ITN17_)

dim(ITN17_A_)
dat<-ITN17_A_%>%left_join(key)%>%arrange(row_num)
dim(ITN17_A_)

#####################################################################
# -- setting up the spatial prior distribution for the smoothing -- #
#####################################################################

# this takes the shape file and figures out which areas are neighbors
nb.r <- poly2nb(DS_shape_W, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix

# to double check that they are finding the neighbors, I like to plot a 
# random LGA in blue and then the neighbors in red.
row = 50
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx

# Plot the LGA and highlight the neighbors
plot(DS_shape_W)
plot(DS_shape_W[row,], col='blue', add=T)
plot(DS_shape_W[indx,], col='red', add=T)
# it works!

################################################
# -- Setting up for spatial smoothing model -- #
################################################

a<-1
b<-5e-05

# adding a few models for comparison #
smoothing.model.1 <- outcome ~ f(row_num, model="iid", param=c(a,b)) 
smoothing.model.2 <- outcome ~ f(row_num, model="bym",graph=mat, param=c(a,b)) 
smoothing.model.3 <- outcome ~ f(row_num, model="bym2",graph=mat, param=c(a,b)) #this model no longer works in INLA with the SE#

# setting up the outcome #
ITN14_1$outcome<-dat$logit_ITN 
ITN14_1$prec<-1/dat$var_logit_ITN
ITN14_1$row_num <-ITN14_1$X1

ITN14_A_$outcome<-dat$logit_ITN 
ITN14_A_$prec<-1/dat$var_logit_ITN
ITN14_A_$row_num <-ITN14_A_$X1

ITN17_$outcome<-ITN17_$logit_ITN 
ITN17_$prec<-1/ITN17_$var_logit_ITN
ITN17_$row_num <-ITN17_$X1

ITN17_A_$outcome<-ITN17_A_$logit_ITN 
ITN17_A_$prec<-1/ITN17_A_$var_logit_ITN
ITN17_A_$row_num <-ITN17_A_$X1

# fitting model 1 #
system.time({
  mod1 <- inla(smoothing.model.1,
               family = "gaussian",
               data = ITN14_1,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
               control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
               scale=prec)
  
})#2.51s

# fitting model 2 #

Ina.fun <- function(filename){
  mod2 <- inla(smoothing.model.2,
               family = "gaussian",
               data=filename,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
               control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
               scale=prec)
  
}

mod2_ITN14_A_ <- Ina.fun(ITN14_A_)
mod2_ITN17_A_ <- Ina.fun(ITN17_A_)

system.time({
  mod2 <- inla(smoothing.model.2,
               family = "gaussian",
               data =ITN14_A_,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
               control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
               scale=prec)
  
})#10.14

# fitting model 3 #
system.time({
  mod3 <- inla(smoothing.model.3,
               family = "gaussian",
               data =dat,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
               control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
               scale=prec)
  
})#20.77s

#############################################################################
#### --- trying out version without incorporating the standard error --- ####
#############################################################################


# fitting model 4 #
dat$outcome<-logit(dat$BMI)
dat$outcome<-ifelse(dat$BMI==0,NA,dat$outcome)

# model 4 #
system.time({
  mod4 <- inla(smoothing.model.1,
               family = "gaussian",
               data =dat,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))#,
  # control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
  # scale=prec)
  
})#3.26s

# model 5 #
system.time({
  mod5 <- inla(smoothing.model.2,
               family = "gaussian",
               data =dat,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))#,
  # control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
  # scale=prec)
  
})#4.64s

# model 6 #
system.time({
  mod6 <- inla(smoothing.model.3,
               family = "gaussian",
               data =dat,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T))#,
  # control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
  # scale=prec)
  
})#17.22s

###########################
# -- selecting a model -- #
###########################
mod1$dic$dic
mod2$dic$dic # prefered
mod3$dic$dic
# -- #
mod4$dic$dic
mod5$dic$dic
mod6$dic$dic # prefered


mod1$waic$waic
mod2$waic$waic # prefered
mod3$waic$waic
# -- #
mod4$waic$waic
mod5$waic$waic
mod6$waic$waic # preferec

df1<- merge(shape2, prevalence, by.x = "NAME_2", by.y = "NAME_2", no.dups = FALSE)
writeOGR(df1, "displacement/Polygon", layer = "Polygon", driver="ESRI Shapefile")

#creating data frame of all estimates for model 2
saep.est = expit(mod2_ITN14_A_$summary.fitted.values$`0.5quant`)
saep.up = expit(mod2_ITN14_A_$summary.fitted.values$`0.975quant`)
saep.low = expit(mod2_ITN14_A_$summary.fitted.values$`0.025quant`)

saep.est = expit(mod2_ITN17_A_$summary.fitted.values$`0.5quant`)
saep.up = expit(mod2_ITN17_A_$summary.fitted.values$`0.975quant`)
saep.low = expit(mod2_ITN17_A_$summary.fitted.values$`0.025quant`)


#creating the data frame of all estimates for model 6
saep.est2 = expit(mod6$summary.fitted.values$`0.5quant`)
saep.up2 = expit(mod6$summary.fitted.values$`0.975quant`)
saep.low2 = expit(mod6$summary.fitted.values$`0.025quant`)

#Generating estimate file model 2 
prevalence = data.frame(ITN17_A_,saep.est,saep.low, saep.up)
write_csv(as.data.frame(prevalence), paste0("BFITN17_Adult_compsurvey",today(),".csv"))

#Generating estimate file model 6
prevalence2 = data.frame(dat,saep.est2,saep.low2, saep.up2)
write_csv(as.data.frame(prevalence2), paste0("Data/AllPrevalence_mod6_",today(),".csv"))

#cpo score
cpo.score=sum(log(mod1$cpo$cpo), na.rm=TRUE)
cpo.score

#########################################
# -- Plotting the original estimates -- #
#########################################

png(paste0("Figures/NGA_HT_Estimates_",
           format(today(), '%Y%m%d'),".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar = dat$BMI

brks=seq(0,0.8,length=11)
nclr<-length(brks)-1

plotclr<-rev(brewer.pal(nclr,"RdYlBu"))
colornum<-findInterval(plotvar, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)
plot(shape1,border="black",lwd=2,add=T)
plot(shape0,border="black",lwd=3,add=T)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0,0.8,length=3)*100,"%"),
             align="rb",cex=1.4) 
text(8.5,14.1,"Overweight (<=25kg/m2)",cex=2)
dev.off()

##############################################################

png(paste0("Figures/NGA_SAE_Estimates_mod2_",
           format(today(), '%Y%m%d'),".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar = expit(mod2$summary.fitted.values$`0.5quant`)


brks=seq(0,0.8,length=11)
nclr<-length(brks)-1

plotclr<-rev(brewer.pal(nclr,"RdYlBu"))
colornum<-findInterval(plotvar, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)
plot(shape1,border="black",lwd=2,add=T)
plot(shape0,border="black",lwd=3,add=T)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0,0.8,length=3)*100,"%"),
             align="rb",cex=1.4)
text(8.5,14.1,"Overweight (<=25kg/m2)",cex=2)
dev.off()


##############################################################

png(paste0("Figures/NGA_SAE_Estimates_mod6_",
           format(today(), '%Y%m%d'),".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar = expit(mod6$summary.fitted.values$`0.5quant`)


brks=seq(0,0.8,length=11)
nclr<-length(brks)-1

plotclr<-rev(brewer.pal(nclr,"RdYlBu"))
colornum<-findInterval(plotvar, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)
plot(shape1,border="black",lwd=2,add=T)
plot(shape0,border="black",lwd=3,add=T)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0,0.8,length=3)*100,"%"),
             align="rb",cex=1.4)
text(8.5,14.1,"Overweight (<=25kg/m2)",cex=2)
dev.off()
########################################################################

########################################################################
########################################################################
####  I don't have the population data but we need to compare
####  model 2 output and model 6 output in terms of how well they do 
####  when aggregated and compared to the state-level estimate
########################################################################
########################################################################

################## DID NOT RUN THIS PART ################################

###### code for aggregating LGA estimates for model 2###################

#reading in LGA data 
pop <-read_csv("Data/LGA_level_population.csv")

#creating state population column and placing state population in column  
pop$state_pop <- NA 
pop$state_pop <- ifelse(is.na(pop$NAME_2), pop$Female, NA)

#removing state population total from column
pop$Female <- ifelse(is.na(pop$NAME_2), NA, pop$Female)

#subsetting population data frame to create LGA population data and state population datasets 
Lga <- subset.data.frame(pop,!is.na(pop$Female))
statepop <-subset.data.frame(pop,!is.na(pop$state_pop))

#writing data to check if both prevalence and LGA data match 
new <- merge.data.frame(prevalence, Lga)
write_csv(Lga, "Data/Lga.csv")
write_csv(new, "Data/new_data.csv")

#renaming female population variable 
new$female_pop <- new$Female
new$Female <- NULL

#multiplying the SAE estimates by population 
new$weigh_est <- new$saep.est*new$female_pop

#aggregating the product of the SAE estimates and LGA population
firstest <- aggregate(new$weigh_est, by=list(NAME_1=new$NAME_1), FUN=sum)

#creating a file with aggregated products and state population 
final_state_est <- merge.data.frame(firstest, statepop)

#dividing the aggregated products by state population
final_state_est$state_est <- firstest$x/statepop$state_pop

#saving all to file
write_csv(final_state_est, "Data/fina_state_est.csv")

#############################################################
###### code for aggregating state estimates for model 6 ###################

#reading in LGA data 
pop <-read_csv("Data/LGA_level_population.csv")

#creating state population column and placing state population in column  
pop$state_pop <- NA 
pop$state_pop <- ifelse(is.na(pop$NAME_2), pop$Female, NA)

#removing state population total from column
pop$Female <- ifelse(is.na(pop$NAME_2), NA, pop$Female)

#subsetting population data frame to create LGA population data and state population datasets 
Lga <- subset.data.frame(pop,!is.na(pop$Female))
statepop <-subset.data.frame(pop,!is.na(pop$state_pop))

#writing data to check if both prevalence and LGA data match 
new <- merge.data.frame(prevalence2, Lga)
write_csv(Lga, "Data/Lga.csv")
write_csv(new, "Data/new_data.csv")

#renaming female population variable 
new$female_pop <- new$Female
new$Female <- NULL

#multiplying the SAE estimates by population 
new$weigh_est <- new$saep.est*new$female_pop

#aggregating the product of the SAE estimates and LGA population
firstest <- aggregate(new$weigh_est, by=list(NAME_1=new$NAME_1), FUN=sum)

#creating a file with aggregated products and state population 
final_state_est <- merge.data.frame(firstest, statepop)

#dividing the aggregated products by state population
final_state_est$state_est <- firstest$x/statepop$state_pop

#saving all to file
write_csv(final_state_est, "Data/fina_state_est2.csv")


#########################################################################

###################################################################

#Setting up the polygon areas

#setting the coordinate system of the admin2 areas
crs(shape2)

###Calculating the area of each polygon
shape2$area_sqkm <- area(shape2) / 1000000
shape2$area_sqkm