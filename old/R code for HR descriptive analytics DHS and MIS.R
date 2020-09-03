#############################################################################################
####
#### Code for exploration and descriptive analysis of malaria-related variables in the DHS/MIS Household
#### recode files in Nigeria
#### 
#### Last Updated: August 29, 2019
####
#### Author:Ifeoma Ozodiegwu
#### Estimated variables:

#######################################################################
#1. We read in the datasets that we will use and start data processing 
#######################################################################
rm(list=ls())

################################################
#---Getting and setting working directory---#
################################################
getwd()
#setwd("/Users/ifeomaozodiegwu/Box/NU-malaria-team/data/burkina_dhs")
setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs")

##############################################
#---Some installations---#
#############################################
####devtools::install_github("tidyverse/tidyr")
####devtools::install_github("geocompr/geocompkg")
############################################
# -- Reading in the Libraries -- #
############################################
library(tidyverse);library(survey);library(haven);library(ggplot2); library(purrr);
library(summarytools); library(stringr); library(sp); library(rgdal); library(raster); 
library(lubridate);library(RColorBrewer); library(plotrix); library(ggrepel)
library(sf);library(shinyjs);library(tmap); library(knitr) 
options(survey.lonely.psu="adjust") # an important option for LGAs with only one cluster

####capabilities() #use this in mac to troubleshoot summarytools 

###removes the plyr package that was attached earlier.dplyr does not work well when plyr is attached.  
#detach("package:plyr", unload=TRUE)

###########################################################
#Reading all Burkina individual recode data DHS, MIS data#
###########################################################
####this reads all the dta files with a BFIR anywhere in their name in the main 
####and subdirectories in the working directorey
in_files <- list.files(pattern = ".*BFHR.*\\.DTA", recursive = TRUE)

####this removes the .dta in the file names in order to use in the loop 
filenames <- str_sub(in_files, end = -5)

###this reads the individual files as seperate objects in R 
for(i in filenames){
  filepath <- file.path(paste(i,".dta",sep=""))
  assign(i, read_dta(filepath))
}

#####################################################################################
#Renaming dataframes for easy exploration and checking the validity of the import 
#####################################################################################
bfhrdhs93 <- `BF_1993_DHS_06192019/BFHR21DT/BFHR21FL`
bfhrdhs98 <- `BF_1998-99_DHS_06192019/BFHR31DT/BFHR31FL`
bfhrdhs03 <- `BF_2003_DHS_06192019/BFHR43DT/BFHR43FL`
bfhrdhs10 <- `BF_2010_DHS_06192019/BFHR62DT/BFHR62FL`
bfhrmis14 <- `BF_2014_MIS_06192019/BFHR70DT/BFHR70FL`
bfhrmis18 <- `BF_2017-18_MIS_07252019_1531_86355/BFHR7ADT/BFHR7AFL`


###########################################################################################
#---We read in the GPS points and shapefiles that will used in subsequent analysis---#
###########################################################################################
####GPS points 
pts_93<- readOGR("BF_1993_DHS_06192019/BFGE23FL", layer = "BFGE23FL") #this is the 93 BF 
pts_98 <- readOGR("BF_1998-99_DHS_06192019/BFGE32FL", layer = "BFGE32FL") #this is 98 BF 
pts_10 <-readOGR("BF_2010_DHS_06192019/BFGE61FL", layer = "BFGE61FL")
pts_14 <- readOGR("BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL")

####shapefiles for the health districts 
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

####we convert the sp objects to sf object for easy plotting with tmap later 
pt_sf <-st_as_sf(pts_93)
pt_sf_98 <-st_as_sf(pts_98)
pt_sf_10 <-st_as_sf(pts_10)
pt_sf_14 <- st_as_sf(pts_14)
DSshape_sf<-st_as_sf(DS_shape)


####Adding a row number to the DS_shape object and the pts data, will be handy for plotting later on
DS_shape@data$row_num<-1:nrow(DS_shape@data)
pts_93@data$row_num <- 1:nrow(pts_93@data)
pts_98@data$row_num <- 1:nrow(pts_98@data)
pts_10@data$row_num <- 1:nrow(pts_10@data)
pts_14@data$row_num <- 1:nrow(pts_14@data)


###################################################################
#---Quality checks and reading in new columns---#
##################################################################
#### DS is in the utm + zone 30 & points is in long + lat. lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,crs(pts_93))

####check map projections again 
crs(DS_shape_W)
crs(pts_93)
crs(pts_98)
crs(pts_10)
crs(pts_14)
####everything looks good now 

#### Check if the GPS points  are contained in the polygons 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 93 clusters')
plot(pts_93,add=T,col=4) #this mis 93 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 98 clusters')
plot(pts_98,add=T,col=4) #this is mis 98 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 98 clusters')
plot(pts_10,add=T,col=4) #this is dhs 2010 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 98 clusters')
plot(pts_14,add=T,col=4) #this is mis 2014  
#### everything looks good 

# Now add name of the individual regions to the map to cross-check which admin areas have no clusters
text(DS_shape_W, DS_shape_W@data$NOMDEP, cex=0.75)

####set up an empty dataframe for the health district estimates. 
####this way there will be a row in the data for the health districts with no clusters
DS_merge<-data.frame(NOMDEP=DS_shape_W@data[,"NOMDEP"])

####this creates a dataframe with two cluster variables, one named v001 and another DHSCLUST for merging later 
pts_93_merge<-data.frame(v001=pts_93@data$DHSCLUST, DHSCLUST=pts_93@data$DHSCLUST) # this is mis 93
pts_98_merge<-data.frame(v001=pts_98@data$DHSCLUST, DHSCLUST=pts_98@data$DHSCLUST) #this is mis 98  
pts_10_merge<-data.frame(v001=pts_10@data$DHSCLUST, DHSCLUST=pts_10@data$DHSCLUST) #this is dhs 10 
pts_14_merge<-data.frame(v001=pts_14@data$DHSCLUST, DHSCLUST=pts_14@data$DHSCLUST) #this is mis 14

###################################################################
#---mapping points to inherit associated health district---#
##################################################################
####if pts@data is 230 by 21 and district is 70 by 11, then the corresponding data frame will be 230 by 11
dim(pts_93@data)  # this is for mis 93 
dim(DS_shape_W@data)
key_93<-over(SpatialPoints(coordinates(pts_93),proj4string = pts_93@proj4string), DS_shape_W)
dim(key_93)
length(unique(key_93$NOMDEP))

# add in the cluster variable
key_93$v001<-pts_93@data[,"DHSCLUST"]

####the dimensions of the pts data is 210 by 21 and of the DS is 70 by 11, key_98 will be 210 by 11 
dim(pts_98@data) # this is for mis 98 
dim(DS_shape_W@data)
key_98<-over(SpatialPoints(coordinates(pts_98),proj4string = pts_98@proj4string), DS_shape_W)
dim(key_98)
length(unique(key_98$NOMDEP))

# add in the cluster variable
key_98$v001<-pts_98@data[,"DHSCLUST"]


####the dimensions of the pts data is 573 by 21 and of the DS is 70 by 11, key_10 will be 573 by 11 
dim(pts_10@data) # this is for dhs 2010
dim(DS_shape_W@data)
key_10<-over(SpatialPoints(coordinates(pts_10),proj4string = pts_10@proj4string), DS_shape_W)
dim(key_10)
length(unique(key_10$NOMDEP))

# add in the cluster variable
key_10$v001<-pts_10@data[,"DHSCLUST"]


####the dimensions of the pts data is 252 by 21 and of the DS is 70 by 11, key_14 will be 252 by 11 
dim(pts_14@data) # this is for mis 2014
dim(DS_shape_W@data)
key_14<-over(SpatialPoints(coordinates(pts_14),proj4string = pts_14@proj4string), DS_shape_W)
dim(key_14)
length(unique(key_14$NOMDEP))

# add in the cluster variable
key_14$v001<-pts_14@data[,"DHSCLUST"]

##############################################################################################################
#---data exploration - Let's see what malaria-related variables are in the household recode files---#
###############################################################################################################
####This function provides a logical response to whether a column exists in a dataset 
####This function selects variables from a df that starts with specified names and creates a new dataframe 
fun1 <- function(df, ...){
  df2 <- df %>% 
    dplyr::select(map(c(...), 
                      starts_with, 
                      vars = colnames(.)) %>% 
                    unlist()) 
}     


####Applying the function to each dataset 
hrdhs10 <- fun1(bfhrdhs10, 'hv227', 'hv253')
hrmis14 <- fun1(bfhrmis14, 'hv227', 'hv253')

###creating summary table with summary tools 
view(dfSummary(hrdhs10))
view(dfSummary(hrmis14))

###################################################################
#---Recoding variables---# 
###################################################################
####first we establish a recoder to recode variables using a function.
recoder <- function(x){
  ifelse(x == 3| x == 4 | x == 5 | x == 6 | x == 7 | x == 8| x == 9, NA,ifelse(x == 0, 0, 1))
}


####this is for the 2010 dataset 
hrdhs10 <- bfhrdhs10%>%
  mutate_at(vars(contains('hv253')), recoder)%>%  #recodes (missing - 8, don't know - 9)
  rename(v001 = hv001)

####this is for the 2010 dataset 
hrmis14 <- bfhrmis14%>%
  mutate_at(vars(contains('hv253')), recoder)%>% #recodes (missing - 8)
    rename(v001 = hv001)

################################################################################################
# --Now let's join the DHS/MIS data to the key that contains the names of each health district-- #
################################################################################################
####a few descriptive stats for dhs 2010 
dim(hrdhs10)# how many rows?
dim(key_10) #how many columns?
summary(hrdhs10$v001)# the cluster variable
summary(key_10$v001)
length(unique(hrdhs10$v001))
length(unique(key_10$v001)) # it looks like the cluster variables match!
hrdhs10<-hrdhs10%>%left_join(key_10)



####should still have the same number of rows and a few new columns
dim(hrdhs10) 
####this is the dataset that will be used for subsequent 2010 computations 


####a few descriptive stats for mis 2014 
dim(hrmis14)# how many rows?
dim(key_14) #how many columns?
summary(hrmis14$v001)# the cluster variable
summary(key_14$v001)
length(unique(hrmis14$v001))
length(unique(key_14$v001)) # it looks like the cluster variables match!
hrmis14<-hrmis14%>%left_join(key_14)

####should still have the same number of rows and a few new columns
dim(hrmis14) 
####this is the dataset that will be used for subsequent 2014 computations

################################################################################################
#2. In this step we start using the created datasets in the computations and mapping 
################################################################################################


##############################################################################################################
# -- setting up the survey related variables and survey design object for U5 microscopy positive proportion -- #
###############################################################################################################
#### We start with the 2010 DHS PR dataset 
dhs10_hv253 <- hrdhs10 %>%mutate(wt=hv005/1000000,strat=hv022,
                                 id=hv021, num_HH=1)%>% #column for # of HH to compute the totals by DS and cluster 
              rename(IRS = hv253)%>%
              filter(!is.na(wt))


hv253.svydesign <- svydesign(id= ~id,
                             strata=~strat,nest=T, 
                             weights= ~wt, data=dhs10_hv253)


#### This is for 2014 MIS PR dataset 
mis14_hv253 <- hrmis14 %>%mutate(wt=hv005/1000000,strat=hv022,
                                 id=hv021, num_HH=1)%>% #column for # of HH to compute the totals by DS and cluster
               rename(IRS = hv253)%>%
               filter(!is.na(wt))



hv253.svyd14 <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis14_hv253)


########################################################################################
# -- Proportion of households that got IRS by health district -- #
########################################################################################
####This calculates the proportion of household that received IRS by health district for 2010 BF DHS 
system.time({
  IRS <-svyby(formula=~IRS,# which variable do we want estimate
                   by=~NOMDEP, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=hv253.svydesign, # my svy design object
                   na.rm=T)
})



####This calculates the proportion of household that received IRS by health district for 2014 BF MIS 
system.time({
  IRS_14<-svyby(formula=~IRS,# which variable do we want estimate
                   by=~NOMDEP, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=hv253.svyd14, # my svy design object
                   na.rm=T)
}) # this one takes 25 seconds on my desktop


#########################################################################
# -- Number of HH within each health district -- #
#########################################################################
####This computes the total number of HH within each surveyed cluster for 2010 BF DHS 
system.time({
  num_DS_10<-svyby(formula=~num_HH,# which variable do we want estimate
                    by=~NOMDEP, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                    design=hv253.svydesign, # my svy design object
                    na.rm=T)
}) # this one takes ~19 seconds on my desktop

####This computes the total number of HH within each surveyed cluster for 2014 BF MIS 
system.time({
  num_DS_14<-svyby(formula=~num_HH,# which variable do we want estimate
                    by=~NOMDEP, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                    design=hv253.svyd14, # my svy design object
                    na.rm=T)
}) 

#########################################################################
# -- Number of HH within each cluster -- #
#########################################################################
####This computes the total number of HH within each surveyed cluster for 2010 BF DHS 
system.time({
  num_clu_10<-svyby(formula=~num_HH,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                   design=hv253.svydesign, # my svy design object
                   na.rm=T)
}) # this one takes ~19 seconds on my desktop

####This computes the total number of HH within each surveyed cluster for 2014 BF MIS 
system.time({
  num_clu_14<-svyby(formula=~num_HH,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                   design=hv253.svyd14, # my svy design object
                   na.rm=T)
}) 

#########################################################################
# -- Proportion of HH within each cluster -- #
#########################################################################
####This computes the proportion of HH within each surveyed cluster for 2010 BF DHS 
system.time({
  IRS_clu_10<-svyby(formula=~IRS,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                    design=hv253.svydesign, # my svy design object
                    na.rm=T)
}) # this one takes ~19 seconds on my desktop

####This computes the total number of HH within each surveyed cluster for 2014 BF MIS 
system.time({
  IRS_clu_14<-svyby(formula=~IRS,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                    design=hv253.svyd14, # my svy design object
                    na.rm=T)
}) 

#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in IRS_clu_10 to standard error in preparation for the joining procedure 
colnames(IRS_clu_10)[3]<- "standard_error" #for 2010 dataset 

####renaming 'se' colname in IRS_clu_14 to standard error in preparation for the joining procedure 
colnames(IRS_clu_14)[3]<- "standard_error" #for 2014 dataset 


####we want to merge with the full list of health districts
#we start with the HH with IRS by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_10<-DS_merge%>%left_join(IRS) #2010 BF DHS
DS_merge_14<-DS_merge%>%left_join(IRS_14) #next the 2014 BF MIS

####we want to merge with the full list of health districts with the district level estimates of the number of children
DS_num_10<-DS_merge%>%left_join(num_DS_10)%>%
  rename(`Number of HH` = num_HH)

DS_num_10$`Number of HH` <-  round(DS_num_10$`Number of HH`, 0)

DS_num_14<-DS_merge%>%left_join(num_DS_14)%>% #next the 2014 BF MIS
  rename(`Number of HH` = num_HH)

DS_num_14$`Number of HH` <-  round(DS_num_14$`Number of HH`, 0)

colnames(DS_num_10)[3]<- "standard_error"
colnames(DS_num_14)[3]<- "standard_error"

####next we merge the health district proportion of HH with IRS with number of HH
DS_IRS_10<-DS_merge_10%>%left_join(DS_num_10)
DS_IRS_14<-DS_merge_14%>%left_join(DS_num_14)

####next we merge the number of HH within each surveyed cluster  
####with the proportion of HH that recieved IRS by surveyed cluster
#this is for 2017 
pts_est_IRS<- IRS_clu_10%>%left_join(num_clu_10) 
pts_merge_IRS<-pts_10_merge%>%left_join(pts_est_IRS)%>%
  mutate(`HH in DHS clusters` = round(num_HH, 0))


#this is for 2014
pts_est_IRS_14<-IRS_clu_14%>%left_join(num_clu_14) 
pts_merge_IRS_14<-pts_14_merge%>%left_join(pts_est_IRS_14)%>%
  mutate(`HH in DHS clusters` = round(num_HH, 0))


####creates a csv file of the proportion of children with IRS by health district 
write.csv(DS_IRS_10,file="outputs/IRS_BF10DS.csv") #for the 2010 BF MIS
write.csv(DS_IRS_14,file="outputs/IRS_BF14DS.csv") #for the 2014 BF MIS


#####################################
# -- Now let's map our findings -- #
#####################################
####merging the IRS HH stats to the sf DS shape file created earlier 
DS_shape_10 <- merge(DSshape_sf, DS_IRS_10, by = "NOMDEP") #this is for 2010 BF DHS 
DS_shape_14 <- merge(DSshape_sf, DS_IRS_14, by = "NOMDEP") #this is for 2014 BF MIS 

####merging the IRS HH to the points sf file created earlier 
pt_10 <- merge(pt_sf_10,pts_merge_IRS, by = "DHSCLUST")%>% #for 2010 BF DHS 
  mutate(`HH sprayed with IRS by cluster`= IRS)
pt_14 <- merge(pt_sf_10,pts_merge_IRS_14, by = "DHSCLUST")%>% #for 2014 BF MIS 
  mutate(`HH sprayed with IRS by cluster`= IRS)


st_write(pt_10, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IRS_2010/IRS_2010_cluster.shp")
st_write(pt_14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IRS_2014/IRS_2014_cluster.shp")

####Note to self, include cluster level month of survey in the cluster folders tomorrow 

#this is for 2010 BF DHS 
IRS_2010_map <- tm_shape(DS_shape_10) + #this is the health district shapfile with test result info
  tm_polygons(col = "IRS", textNA = "No data", 
              title = "Proportion of HH that received IRS", palette = "seq")+
  tm_layout(main.title="Proportion of HH that received IRS by Health Districts (2010)",
            main.title.position = c("center", "top"), aes.palette = list(seq = "-RdYlBu"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_10)+ #this is the points shape file with test result and number of kids info by cluster 
  tm_bubbles(size="HH in DHS clusters", col = "HH sprayed with IRS by cluster", 
             border.col= "black", palette="seq")+
  tm_layout(aes.palette = list(seq = "-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")

