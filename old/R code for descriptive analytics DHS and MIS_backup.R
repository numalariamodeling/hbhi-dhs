#############################################################################################
####
#### Code for exploration and descriptive analysis of malaria-related variables in the DHS/MIS Individual 
#### recode files in Nigeria
#### 
#### Last Updated: August 16, 2019
####
#### Author:Ifeoma Ozodiegwu

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
in_files <- list.files(pattern = ".*BFIR.*\\.DTA", recursive = TRUE)

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
bf17mis <- `BF_2017-18_MIS_07252019_1531_86355/BFIR7ADT/BFIR7AFL`#2017 BF MIS  
bf14mis <- `BF_2014_MIS_06192019/BFIR70DT/BFIR70FL` #2014 BF MIS 
bf10dhs <- `BF_2010_DHS_06192019/BFIR62DT/BFIR62FL`
bf98dhs <- `BF_1998-99_DHS_06192019/BFIR31DT/BFIR31FL`
bf93dhs <-`BF_1993_DHS_06192019/BFIR21DT/BFIR21FL`

###########################################################################################
#---We read in the GPS points and shapefiles that will used in subsequent analysis---#
###########################################################################################
####GPS points 
pts<- readOGR("BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL", layer = "BFGE7AFL") #this is 2017 BF 
pts_14 <- readOGR("BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL") #this is 2014 BF 

####shapefiles for the health districts 
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

####we convert the sp objects to sf object for easy plotting with tmap later 
pt_sf <-st_as_sf(pts)
pt_sf_14 <-st_as_sf(pts_14)
DSshape_sf<-st_as_sf(DS_shape)


####Adding a row number to the DS_shape object and the pts data, will be handy for plotting later on
DS_shape@data$row_num<-1:nrow(DS_shape@data)
pts@data$row_num <- 1:nrow(pts@data)
pts_14@data$row_num <- 1:nrow(pts_14@data)

###################################################################
#---Quality checks and reading in new columns---#
##################################################################
#### DS is in the utm + zone 30 & points is in long + lat. lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,crs(pts))

####check map projections again 
crs(DS_shape_W)
crs(pts)
crs(pts_14)
####everything looks good now 

#### Check if the GPS points  are contained in the polygons 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2017 clusters')
plot(pts,add=T,col=4) #this mis 17 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2014 clusters')
plot(pts_14,add=T,col=4) #this is mis 14 
#### everything looks good 

# Now add name of the individual regions to the map to cross-check which admin areas have no clusters
text(DS_shape_W, DS_shape_W@data$NOMDEP, cex=0.75)

####set up an empty dataframe for the health district estimates. 
####this way there will be a row in the data for the health districts with no clusters
DS_merge<-data.frame(NOMDEP=DS_shape_W@data[,"NOMDEP"])

####this creates a dataframe with two cluster variables, one named v001 and another DHSCLUST for merging later 
pts_merge<-data.frame(v001=pts@data$DHSCLUST, DHSCLUST=pts@data$DHSCLUST) # this is mis 17 
pts_14_merge<-data.frame(v001=pts_14@data$DHSCLUST, DHSCLUST=pts_14@data$DHSCLUST) #this is mis 14 

###################################################################
#---mapping points to inherit associated health district---#
##################################################################
####if pts@data is 245 by 20 and district is 70 by 10, then the corresponding data frame will be 245 by 10
dim(pts@data)  # this is for mis 17 
dim(DS_shape_W@data)
key<-over(SpatialPoints(coordinates(pts),proj4string = pts@proj4string), DS_shape_W)
dim(key)
length(unique(key$NOMDEP))

# add in the cluster variable
key$v001<-pts@data[,"DHSCLUST"]

####the dimensions of the pts data is 252 by 21 and of the DS is 70 by 11, key_14 will be 245 by 10 
dim(pts_14@data) # this is for mis 14 
dim(DS_shape_W@data)
key_14<-over(SpatialPoints(coordinates(pts_14),proj4string = pts_14@proj4string), DS_shape_W)
dim(key_14)
length(unique(key_14$NOMDEP))

# add in the cluster variable
key_14$v001<-pts_14@data[,"DHSCLUST"]

##############################################################################################################
#---data exploration - Let's see what malaria-related variables are in the individual recode files---#
###############################################################################################################
####This function provides a logical response to whether a column exists in a dataset 
fun0 <- function(df, ...) {
   c(...) %in% colnames(df) 
   }

####We start with the 2017 BF MIS and assess what malaria variables are within the dataset  
fun0(bf17mis, 'b2_01', 'ml25a_1','ml0_1', 's405a', 's405b', 'h22_1', 'h32z_1', 'ml13e_1', 'm49a', 'v459', 'ml101', 'h46b_1')
fun0(bf14mis, 'b2_01', 'ml0_1', 's405a', 's405b', 'h22_1', 'h32z_1', 'ml13e_1', 'v024', 's307a', 's127', 's129', 's307d')


##############################################################################################################
#---Let's create the needed variables and summary table of malaria-related variables---#
###############################################################################################################
####This function selects variables from a df that starts with specified names and creates a new dataframe 
fun1 <- function(df, ...){
  df2 <- df %>% 
    dplyr::select(map(c(...), 
               starts_with, 
               vars = colnames(.)) %>% 
           unlist()) 
}     
 
####Compute the month of survey variable based on the century month code (See DHS recode manual for details)
bf17mis$MM = bf17mis$v008 - ((bf17mis$v007 - 1900) * 12)
table(bf17mis$MM)

bf14mis$MM = bf14mis$v008 - ((bf14mis$v007 - 1900) * 12)
table(bf14mis$MM)

####Applying the function to each dataset 
###We start with the 2017 and 2014 BF MIS dataset
mis17 <- fun1(bf17mis, 'b2', 's111','ml25a', 'ml20a', 'ml0', 'h22', 'h32z', 'h46a', 'ml13e','ml13a','m49a','v459','ml101','ml2','MM', 'v007') 
mis14 <- fun1(bf14mis, 'b2', 'ml0', 'h22', 'h32z', 'ml13e', 's127', 's307','MM', 'v007', 's129')


####these are some of the important malaria variables 
####malaria_mis_var <- fun1(bmis_14, 'b2', 'ml0', 's405a', 's405b', 'h22', 'h32z', 'ml13e', 'S307')

###creating summary table with summary tools 
view(dfSummary(mis17))
view(dfSummary(mis14))

###################################################################
#---Recoding variables---# 
###################################################################
####first we establish a recoder to recode variables using function: 3 = only untreated nets to 0 (no treated net) 
####Fever variables has an '8'= don't know next we apply to the 2017 dataset  
recoder <- function(x){
  ifelse(x == 8, NA,ifelse(x == 3 | x == 0, 0, 1))
}

####this is for the 2017 dataset 
mis17_r <- bf17mis%>%#this is the 2017 dataset 
  mutate_at(vars(contains('ml0')), recoder)%>% #recodes for LLINs 
  mutate_at(vars(contains('h22')), recoder) %>%   #recodes for the fever variables 
  mutate_at(vars('m49a_1'), recoder)%>% #recodes for the IPTp with sp/fansidar 
  mutate_at(vars('ml101'), recoder) #recodes for LLIN use by adult women 

 
####next to the 2014 dataset 
mis14_r <- bf14mis %>%
  mutate_at(vars(contains('ml0')), recoder)%>% #recodes for LLINs 
  mutate_at(vars(contains('h22')), recoder)    #recodes for the fever variables 


################################################################################################
# --Now let's join the MIS data to the key that contains the names of each health district-- #
################################################################################################
####a few descriptive stats for mis 17 
dim(mis17_r)# how many rows?
dim(key) #how many columns?
summary(mis17_r$v001)# the cluster variable
summary(key$v001)
length(unique(mis17_r$v001))
length(unique(key$v001)) # it looks like the cluster variables match!
mis17_r<-mis17_r%>%left_join(key)


####should still have the same number of rows and a few new columns
dim(mis17_r) 
####this is the dataset that will be used for subsequent 2017 computations that don't require long format 



####a few descriptive stats for mis 14 
dim(mis14_r)# how many rows?
dim(key_14) #how many columns?
summary(mis14_r$v001)# the cluster variable
summary(key_14$v001)
length(unique(mis14_r$v001))
length(unique(key_14$v001)) #
mis14_r<-mis14_r%>%left_join(key_14)

####should still have the same number of rows and a few new columns
dim(mis14_r) 
####this is the dataset that will be used for subsequent 2014 computations that don't require long format  


###########################################################################################
#---Changing data into long format to aggregate observations across children---# 
###########################################################################################
####This is the dataset for analysis that require long formats. We start with 2017 and select variables we need 
mis17_l<- mis17_r%>%dplyr::select(b2_01:b2_04, ml0_1:ml0_4, h22_1:h22_4,h32z_1:h32z_3,ml13e_1:ml13e_3,
                                  caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
                    mutate_at(vars(contains('h22')), as.numeric)%>%
                    mutate_at(vars(contains('h32z')), as.numeric)%>%
                   mutate_at(vars(contains('ml13e')), as.numeric)%>%
            pivot_longer(cols = ml0_1:ml13e_3, names_to = "var", values_to = "count")


#### this is LLIN for mis 2014
mis14_l<- mis14_r%>%dplyr::select(b2_01:b2_04, ml0_1:ml0_4,h22_1:h22_4,h32z_1:h32z_3, ml13e_1:ml13e_3,s307a,
                                  caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
               mutate_at(vars(contains('h22')), as.numeric)%>%
               mutate_at(vars(contains('h32z')), as.numeric)%>%
              mutate_at(vars(contains('ml13e')), as.numeric)%>%
              mutate_at(vars("s307a"), as.numeric)%>%
            pivot_longer(cols = ml0_1:s307a, names_to = "var", values_to = "count")


################################################################################################
#2. In this step we start using the created datasets in the long or record format as applicable 
################################################################################################



####################################################################################
# -- setting up the survey related variables and survey design object for U5 LLIN -- #
####################################################################################
#### selecting only LLIN variables for analysis and other data processing 
mis17_ml0<-mis17_l[mis17_l$var == "ml0_1"|mis17_l$var == "ml0_2"|mis17_l$var == "ml0_3"|mis17_l$var == "ml0_4",] %>%
            mutate(wt=v005/1000000,strat=v022,
            id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
             rename(LLIN_use = count) %>%
            filter(!is.na(wt))%>% 
            filter(!is.na(LLIN_use))


my.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis17_ml0)


#### This is for LLIN mis 2014 
mis14_ml0<-mis14_l[mis14_l$var == "ml0_1"|mis14_l$var == "ml0_2"|mis14_l$var == "ml0_3"|mis14_l$var == "ml0_4",] %>%
               mutate(wt=v005/1000000,strat=v022,
              id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster
                rename(LLIN_use = count) %>%
                filter(!is.na(wt))%>% 
                filter(!is.na(LLIN_use))


svydesign_14 <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis14_ml0)

########################################################################################
# -- Proportion of U5 children who slept under a treated bednet by health district -- #
########################################################################################
####This calculates the proportion of children that slept under a bednet by health district for 2017 BF MIS 
system.time({
  Bednet<-svyby(formula=~LLIN_use,# which variable do we want estimate
                by=~NOMDEP, # by which variable
                FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                design=my.svydesign, # my svy design object
                na.rm=T)
})

####This computes the proportion of children that slept under a bednet by health district for 2014 BF MIS 
system.time({
  Bednet_14<-svyby(formula=~LLIN_use,# which variable do we want estimate
                   by=~NOMDEP, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=svydesign_14, # my svy design object
                   na.rm=T)
}) # this one takes 25 seconds on my desktop

#########################################################################
# -- Number of U5 children wwithin each cluster -- #
#########################################################################
####This computes the total number of under-five children within each surveyed cluster for 2017 BF MIS 
system.time({
  num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                 design=my.svydesign, # my svy design object
                 na.rm=T)
}) # this one takes 73 seconds on my desktop


####This computes the total number of U5 children within each surveyed cluster for 2014 BF MIS 
system.time({
  num_clu_14<-svyby(formula=~num_kids,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                    design=svydesign_14, # my svy design object
                    na.rm=T)
}) # this one takes 73 seconds on my desktop

#####################################################################################
# -- Proportion of U5 children who slept under a treated bednet by cluster -- #
####################################################################################
####This computes the proportion of U5 children that slept under a bednet by surveyed cluster for 2017 BF MIS 
system.time({
  Bednet_clu<-svyby(formula=~LLIN_use,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                    design=my.svydesign, # my svy design object
                    na.rm=T)
  
}) # this one takes 67 seconds on my desktop

system.time({
  Bednet_clu_14<-svyby(formula=~LLIN_use,# which variable do we want estimate
                       by=~v001, # by which variable
                       FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                       design=svydesign_14, # my svy design object
                       na.rm=T)
  
}) # this one takes 67 seconds on my desktop

#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu)[3]<- "standard_error" #for 2017 dataset 

####renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu_14)[3]<- "standard_error" #for 2014 dataset 


####we want to merge with the full list of health districts
#we start with the proportion of children that slept under a bednet by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_n<-DS_merge%>%left_join(Bednet)
DS_merge_14<-DS_merge%>%left_join(Bednet_14) #next the 2014 BF MIS


####next we merge the number of under-five children within each surveyed cluster  
####with the number of under-five children that slept under a bednet by surveyed cluster
#this is for 2017 
pts_estimates<-num_clu%>%left_join(Bednet_clu) 
pts_merge_n<-pts_merge%>%left_join(pts_estimates)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`U5 in DHS clusters` = round(num_kids, 0))


#this is for 2014
pts_estimates_14<-num_clu_14%>%left_join(Bednet_clu_14) 
pts_merge_14<-pts_14_merge%>%left_join(pts_estimates_14)%>%
  mutate(DHSCLUST = pts_14_merge$v001)%>%
  mutate(`U5 in DHS clusters` = round(num_kids, 0))


####creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_n,file="outputs/LLIN_use_BF17DS.csv") #for the 2017 BF MIS
write.csv(DS_merge_14,file="outputs/LLIN_use_BF14DS.csv") #for the 2014 BF MIS

#####################################
# -- Now let's map our findings -- #
#####################################
####merging the LLIN stats to the sf DS shape file created earlier 
DS_shape_n <- merge(DSshape_sf, DS_merge_n, by = "NOMDEP") #this is for 2017 BF MIS  
DS_shape_14 <- merge(DSshape_sf, DS_merge_14, by = "NOMDEP") #this is for 2014 BF MIS 

####merging the LLIN stats to the points sf file created earlier 
pt_n <- merge(pt_sf,pts_merge_n, by = "DHSCLUST")%>% #for 2017 BF MIS 
  mutate(`LLIN use prevalence by cluster`=LLIN_use)
pt_n_14 <- merge(pt_sf_14,pts_merge_14, by = "DHSCLUST")%>% #for 2014 BF MIS 
  mutate(`LLIN use prevalence by cluster`=LLIN_use)


#st_write(pt_n, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_2017/LLIN_2017_cluster.shp")
#st_write(pt_n_14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_2014/LLIN_2014_cluster.shp")

#this is for 2017 BF 
LLIN_2017_map <- tm_shape(DS_shape_n) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="LLIN use among children under age 5 by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_n)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")

#this is for 2014 BF 
LLIN_2014_map <- tm_shape(DS_shape_14) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="LLIN use among children under age 5 by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_n_14)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


####print map 
LLIN_2017_map
LLIN_2014_map


####save maps 
tmap_save(tm = LLIN_2017_map, filename = "outputs/LLIN_map_2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = LLIN_2014_map, filename = "outputs/LLIN_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")







####################################################################################################
# -- setting up the survey related variables and survey design object for women's LLIN access -- #
####################################################################################################
#### selecting only adult LLIN access variables for analysis and other data processing.
####This is for mis 2017 
mis17_v459<-mis17_r%>%
  rename(LLIN_adult = v459)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of kids so that we can compute the totals by cluster 
          filter(!is.na(wt)) 
        


v459.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis17_v459)


####This is for mis 2014 
mis14_s127<-mis14_r%>%
  rename(LLIN_adult = s127)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  filter(!is.na(wt)) 


s127.svydesign <- svydesign(id= ~id,
                            strata=~strat,nest=T, 
                            weights= ~wt, data=mis14_s127)
#############################################################################
# -- Proportion of women who have a bednet by health district -- #
#############################################################################
####This calculates the proportion of women that slept under a bednet by health district for 2017 BF MIS 
system.time({
  Bednet_v459<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                by=~NOMDEP, # by which variable
                FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                design=v459.svydesign, # my svy design object
                na.rm=T)
})

####this is for 2014 BF MIS 
system.time({
  Bednet_s127<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                     by=~NOMDEP, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=s127.svydesign, # my svy design object
                     na.rm=T)
})
#########################################################################
# -- Number of women in each cluster -- #
#########################################################################
####This computes the total number of women respondents within each surveyed cluster for 2017 BF MIS 
system.time({
  v459_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                 design=v459.svydesign, # my svy design object
                 na.rm=T)
}) # this one takes 73 seconds on my desktop


####this is 2014 BF MIS 
system.time({
  s127_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                      design=s127.svydesign, # my svy design object
                      na.rm=T)
}) # this one takes 73 seconds on my desktop


#########################################################################
# -- Proportion of women who have a bednet by cluster -- #
#########################################################################
####This computes the proportion of women that own a bednet by surveyed cluster for 2017 BF MIS 
system.time({
  Bednet_clu_v459<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                    design=v459.svydesign, # my svy design object
                    na.rm=T)
  
}) # this one takes 67 seconds on my desktop

####this is for the 2014 BF MIS 
system.time({
  Bednet_clu_s127<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                         design=s127.svydesign, # my svy design object
                         na.rm=T)
  
}) # this one takes 67 seconds on my desktop


#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu_v459)[3]<- "standard_error" #for 2017 dataset 
colnames(Bednet_clu_s127)[3]<- "standard_error" #for 2014 dataset 

####we want to merge with the full list of health districts
#we start with the proportion of children that slept under a bednet by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_v459<-DS_merge%>%left_join(Bednet_v459)
DS_merge_s127<-DS_merge%>%left_join(Bednet_s127) #this is for the 2014 BF MIS 

####next we merge the number of under-five children within each surveyed cluster  
####with the number of under-five children that slept under a bednet by surveyed cluster
#this is for 2017 
pts_est_v459<-v459_num_clu%>%left_join(Bednet_clu_v459) 
pts_merge_v459<-pts_merge%>%left_join(pts_est_v459)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`Women in DHS clusters` = round(num_f, 0))

#this is for 2014 
pts_est_s127<-s127_num_clu%>%left_join(Bednet_clu_s127) 
pts_merge_s127<-pts_merge%>%left_join(pts_est_s127)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`Women in DHS clusters` = round(num_f, 0))

####creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_v459,file="outputs/LLIN_adultW_BF17DS.csv") #for the 2017 BF MIS
write.csv(DS_merge_s127,file="outputs/LLIN_adultW_BF14DS.csv") #for the 2014 BF MIS

#####################################
# -- Now let's map our findings -- #
#####################################
####merging the LLIN stats to the sf DS shape file created earlier 
DS_shape_v459 <- merge(DSshape_sf, DS_merge_v459, by = "NOMDEP") #this is for 2017 BF MIS  
DS_shape_s127 <- merge(DSshape_sf, DS_merge_s127, by = "NOMDEP") #this is for 2014 BF MIS 

####merging the LLIN stats to the points sf file created earlier 
pt_v459 <- merge(pt_sf,pts_merge_v459, by = "DHSCLUST")%>% #this is for 2017 BF MIS 
  mutate(`Adult women LLIN access prevalence by cluster`=LLIN_adult)

pt_s127 <- merge(pt_sf,pts_merge_s127, by = "DHSCLUST")%>% #this is for 2014 BF MIS 
  mutate(`Adult women LLIN access prevalence by cluster`=LLIN_adult)

#st_write(pt_v459,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adultW_2017/LLIN_adultW17.shp")
#st_write(pt_s127,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adultW_2014/LLIN_adultW14.shp")


#this is for 2017 BF 
LLIN_adultW_2017_map <- tm_shape(DS_shape_v459) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_adult", textNA = "No data", 
              title = "LLIN access prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN access prevalence by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_v459)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN access prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#this is for 2014 BF 
LLIN_adultW_2014_map <- tm_shape(DS_shape_s127) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_adult", textNA = "No data", 
              title = "LLIN access prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                             0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN access prevalence by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_s127)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN access prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


####print map 
LLIN_adultW_2017_map
LLIN_adultW_2014_map

####save maps 
tmap_save(tm = LLIN_adultW_2017_map, filename = "outputs/LLIN_adultW_map2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = LLIN_adultW_2014_map, filename = "outputs/LLIN_adultW_map2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")





####################################################################################################
# -- setting up the survey related variables and survey design object for women's LLIN use -- #
####################################################################################################
#### selecting only adult LLIN access variables for analysis and other data processing.
####This is for mis 2017 
mis17_ml101<-mis17_r%>%
  rename(LLIN_use_a = ml101)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  filter(!is.na(wt)) 



ml101.svydesign <- svydesign(id= ~id,
                            strata=~strat,nest=T, 
                            weights= ~wt, data=mis17_ml101)


####This is for mis 2014 
mis14_s129<-mis14_r%>%
  rename(LLIN_use_a = s129)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  filter(!is.na(wt)) 


s129.svydesign <- svydesign(id= ~id,
                            strata=~strat,nest=T, 
                            weights= ~wt, data=mis14_s129)
#############################################################################
# -- Proportion of women who use a bednet by health district -- #
#############################################################################
####This calculates the proportion of women that slept under a bednet by health district for 2017 BF MIS 
system.time({
  Bednet_ml101<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                     by=~NOMDEP, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design= ml101.svydesign, # my svy design object
                     na.rm=T)
})

####this is for 2014 BF MIS 
system.time({
  Bednet_s129<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                     by=~NOMDEP, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=s129.svydesign, # my svy design object
                     na.rm=T)
})
#########################################################################
# -- Number of women in each cluster -- #
#########################################################################
####This computes the total number of women respondents within each surveyed cluster for 2017 BF MIS 
system.time({
 ml101_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                      design=ml101.svydesign, # my svy design object
                      na.rm=T)
}) # this one takes 73 seconds on my desktop


####this is 2014 BF MIS 
system.time({
  s129_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                      design=s129.svydesign, # my svy design object
                      na.rm=T)
}) # this one takes 73 seconds on my desktop


#########################################################################
# -- Proportion of women who slept under a bednet by cluster -- #
#########################################################################
####This computes the proportion of women that slept under a bednet by surveyed cluster for 2017 BF MIS 
system.time({
  Bednet_clu_ml101<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                         design=ml101.svydesign, # my svy design object
                         na.rm=T)
  
}) # this one takes 67 seconds on my desktop

####this is for the 2014 BF MIS 
system.time({
  Bednet_clu_s129<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                         design=s129.svydesign, # my svy design object
                         na.rm=T)
  
}) # this one takes 67 seconds on my desktop


#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu_ml101)[3]<- "standard_error" #for 2017 dataset 
colnames(Bednet_clu_s129)[3]<- "standard_error" #for 2014 dataset 

####we want to merge with the full list of health districts
#we start with the proportion of children that slept under a bednet by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_ml101<-DS_merge%>%left_join(Bednet_ml101)
DS_merge_s129<-DS_merge%>%left_join(Bednet_s129) #this is for the 2014 BF MIS 

####next we merge the number of under-five children within each surveyed cluster  
####with the number of under-five children that slept under a bednet by surveyed cluster
#this is for 2017 
pts_est_ml101<-ml101_num_clu%>%left_join(Bednet_clu_ml101) 
pts_merge_ml101<-pts_merge%>%left_join(pts_est_ml101)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`Women in DHS clusters` = round(num_f, 0))

#this is for 2014 
pts_est_s129<-s129_num_clu%>%left_join(Bednet_clu_s129) 
pts_merge_s129<-pts_merge%>%left_join(pts_est_s129)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`Women in DHS clusters` = round(num_f, 0))

####creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_ml101,file="outputs/LLIN_adult_use_BF17DS.csv") #for the 2017 BF MIS
write.csv(DS_merge_s129,file="outputs/LLIN_adult_use_BF14DS.csv") #for the 2014 BF MIS

#####################################
# -- Now let's map our findings -- #
#####################################
####merging the LLIN stats to the sf DS shape file created earlier 
DS_shape_ml101 <- merge(DSshape_sf, DS_merge_ml101, by = "NOMDEP") #this is for 2017 BF MIS  
DS_shape_s129 <- merge(DSshape_sf, DS_merge_s129, by = "NOMDEP") #this is for 2014 BF MIS 

####merging the LLIN stats to the points sf file created earlier 
pt_ml101 <- merge(pt_sf,pts_merge_ml101, by = "DHSCLUST")%>% #this is for 2017 BF MIS 
  mutate(`Adult women LLIN use prevalence by cluster`=LLIN_use_a)


pt_s129 <- merge(pt_sf,pts_merge_s129, by = "DHSCLUST")%>% #this is for 2014 BF MIS 
  mutate(`Adult women LLIN use prevalence by cluster`=LLIN_use_a)


#st_write(pt_ml101,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adult_use_2017/LLIN_adultuse17.shp")
#st_write(pt_s129,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adult_use_2014/LLIN_adultuse14.shp")


#this is for 2017 BF 
LLIN_adult_use_2017_map <- tm_shape(DS_shape_ml101) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use_a", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                             0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN use prevalence by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_ml101)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#this is for 2014 BF 
LLIN_adult_use_2014_map <- tm_shape(DS_shape_s129) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use_a", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                             0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN use prevalence by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_s129)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


####print map 
LLIN_adult_use_2017_map
LLIN_adult_use_2014_map

####save maps 
tmap_save(tm = LLIN_adult_use_2017_map, filename = "outputs/LLIN_adult_use_map2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = LLIN_adult_use_2014_map, filename = "outputs/LLIN_adult_use_map2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")






##########################################################################
#---fever mapping and estimation---#
##########################################################################



#############################################################################################
# -- setting up the survey related variables and survey design object for fever among U5-- #
#############################################################################################
#### This is for LLIN mis 2017. we are using the long format data we created earlier  
mis17_h22<-mis17_l[mis17_l$var == "h22_1"|mis17_l$var == "h22_2"|mis17_l$var == "h22_3"|mis17_l$var == "h22_4",]%>%
             mutate(wt=v005/1000000,strat=v022,
            id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
              rename(fever = count)%>%
            filter(!is.na(wt))%>% 
            filter(!is.na(fever))

f.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                       weights= ~wt, data=mis17_h22)

#### This is for fever mis 2014 
mis14_h22<-mis14_l[mis14_l$var == "h22_1"|mis14_l$var == "h22_2"|mis14_l$var == "h22_3"|mis14_l$var == "h22_4",]%>%
                  mutate(wt=v005/1000000,strat=v022,
                  id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster  
                  rename(fever = count) %>%
                filter(!is.na(wt))%>% 
                filter(!is.na(fever))


f14.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                       weights= ~wt, data=mis14_h22)

###################################################################################################
#---fever estimation and mapping ---#
#################################################################################################


#############################################################################
# -- Proportion of U5 children with fever by health district -- #
#############################################################################
####This calculates the proportion of children with fever by health district for 2017 BF MIS 
system.time({
fever<-svyby(formula=~fever,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=f.svydesign, # my svy design object
                 na.rm=T)
})

####This calculates the proportion of children with fever by health district for 2014 BF MIS 
system.time({
fever_14<-svyby(formula=~fever,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=f14.svydesign, # my svy design object
                 na.rm=T)
})

#########################################################################
# -- Number of U5 represented in the question on fever by cluster -- #
#########################################################################
####This computes the total number of under-five children within each surveyed cluster for 2017 BF MIS 
system.time({
f_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                by=~v001, # by which variable
                FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                design=f.svydesign, # my svy design object
                na.rm=T)
}) # this one takes 83 seconds on my desktop

####This computes the total number of under-five children within each surveyed cluster for 2014 BF MIS 
system.time({
f14_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                by=~v001, # by which variable
                FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                design=f14.svydesign, # my svy design object
                na.rm=T)
}) # this one takes 83 seconds on my desktop


#########################################################################
# -- Proportion of U5 children who had fever by cluster -- #
#########################################################################
####This computes the proportion of U5 children who had fever by surveyed cluster for 2017 BF MIS 
system.time({
 fever_clu<-svyby(formula=~fever,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=f.svydesign, # my svy design object
                 na.rm=T)
})

####This computes the proportion of U5 children who had fever by surveyed cluster for 2014 BF MIS 
system.time({
 fever14_clu<-svyby(formula=~fever,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=f14.svydesign, # my svy design object
                 na.rm=T)
})

#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in fever_clu to standard error in preparation for the joining procedure 
colnames(fever_clu)[3]<- "standard_error"
colnames(fever14_clu)[3]<- "standard_error"

####we want to merge with the full list of health districts
#we start with the proportion of children with fever by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_f<-DS_merge%>%left_join(fever)
#we do the same for the 2014 BF MIS 
DS_merge_f14<-DS_merge%>%left_join(fever_14)

####next we merge the number of under-five children within each surveyed cluster  
####with the number of under-five children that had fever by surveyed cluster
#this is for 2017 
pts_est_f<-f_num_clu%>%left_join(fever_clu) 
#this is for 2014 
pts_est_f14<-f14_num_clu%>%left_join( fever14_clu) 


#next we merge the full list of pts/clusters with cluster level estimates of # of children and children with fever
#this is for 2017 
pts_merge_f<-pts_merge%>%left_join(pts_est_f)%>%
             mutate(DHSCLUST = pts_merge$v001)%>%
            mutate(`U5 in DHS clusters` = round(num_kids, 0))

#this is for 2014 
pts_merge_f14<-pts_merge%>%left_join(pts_est_f14)%>%
             mutate(DHSCLUST = pts_merge$v001)%>%
            mutate(`U5 in DHS clusters` = round(num_kids, 0))

####creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_f,file="outputs/fever_BF17DS.csv") #for the 2017 BF MIS
write.csv(DS_merge_f14,file="outputs/fever_BF14DS.csv") #for the 2014 BF MIS


#####################################
# -- Now let's map our findings -- #
#####################################
####merging the fever stats to the sf DS shape file 
DS_shape_f <- merge(DSshape_sf, DS_merge_f, by = "NOMDEP") #this is for 2017 BF MIS 
DS_shape_f14 <- merge(DSshape_sf, DS_merge_f14, by = "NOMDEP") #this is for 2014 BF MIS 
#merging the points file to the points cluster values 
pt_f <- merge(pt_sf,pts_merge_f, by = "DHSCLUST")%>% #this is for 2017 BF MIS 
  mutate(`Fever status by cluster`=fever)

pt_f14 <- merge(pt_sf_14,pts_merge_f14, by = "DHSCLUST")%>%  #this is for 2014 BF MIS 
  mutate(`Fever status by cluster`=fever)

#st_write(pt_f, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Fever_2017/fever_2017_cluster.csv")
#st_write(pt_f14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Fever_2014/fever_2014_cluster.csv")

#this is for 2017 BF 
fever_2017_map <- tm_shape(DS_shape_f) + #this is the health district shapfile with LLIN info
  tm_polygons(col = "fever", textNA = "No data", 
              title = "Fever prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Fever status among children under age 5 by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_f)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Fever status by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")

#this is for 2014 BF 
fever_2014_map <- tm_shape(DS_shape_f14) + #this is the health district shapfile with LLIN info
  tm_polygons(col = "fever", textNA = "No data", 
              title = "Fever prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Fever status among children under age 5 by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_f14)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Fever status by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")

####print map 
fever_2017_map
fever_2014_map

####saving map
tmap_save(tm = fever_2017_map, filename = "outputs/fever_map_2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = fever_2014_map, filename = "outputs/fever_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")










##########################################################################
#---medical treatment for fever mapping and estimation---#
##########################################################################

#####################################################################################################
# -- setting up the survey related variables and survey design object for fever medical treatment-- #
####################################################################################################
#### This is for medical treatment for fever mis 2017 
mis17_h32z<-mis17_l[mis17_l$var == "h32z_1"|mis17_l$var == "h32z_2"|mis17_l$var == "h32z_3",]%>%
                mutate(wt=v005/1000000,
                strat=v022,
                id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster  
                rename(med_fever = count)%>%
                filter(!is.na(wt))%>% 
                filter(!is.na(med_fever))


med.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                       weights= ~wt, data=mis17_h32z)

#### This is for medical treatment for fever mis 2014
mis14_h32z<-mis14_l[mis14_l$var == "h32z_1"|mis14_l$var == "h32z_2"|mis14_l$var == "h32z_3",]%>%
               mutate(wt=v005/1000000,
              strat=v022,
              id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
              rename(med_fever = count)%>%
              filter(!is.na(wt))%>% 
              filter(!is.na(med_fever))


med14.svydesign <- svydesign(id= ~id,
                           strata=~strat,nest=T, 
                           weights= ~wt, data=mis14_h32z)

###################################################################################################
#---medical treatment estimation and mapping ---#
#################################################################################################


#####################################################################################################
# -- Proportion of U5 children with fever that received medical treatment by health district -- #
#####################################################################################################
####This calculates the proportion of children with fever that received medical treatment 
####by health district for 2017 BF MIS 
system.time({
medical<-svyby(formula=~med_fever,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=med.svydesign, # my svy design object
                 na.rm=T)
})

####This calculates the proportion of children with fever that received medical treatment 
####by health district for 2014 BF MIS 
system.time({
  medical_14<-svyby(formula=~med_fever,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=med14.svydesign, # my svy design object
                 na.rm=T)
})

######################################################################################################
# -- Number of U5 represented in the question on medical treatment for fever by cluster -- #
####################################################################################################
####This computes the total number of under-five children within each surveyed cluster for 2017 BF MIS 
system.time({
m_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                by=~v001, # by which variable
                FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                design=med.svydesign, # my svy design object
                na.rm=T)
}) 

####For 2014 BF MIS 
system.time({
  m14_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                   design=med14.svydesign, # my svy design object
                   na.rm=T)
}) 

####alternative script to compute survey totals by cluster 
#####num_u5 <-svytotal(x = ~interaction(num_kids, v001), design = my.svydesignna.rm = TRUE)


#############################################################################################
# -- Proportion of U5 children who recieved medical treatment for fever by cluster -- #
#############################################################################################
####This computes the proportion of U5 who received medical treatment for fever by surveyed cluster for 2017 BF MIS 
system.time({
 med_clu<-svyby(formula=~med_fever,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=med.svydesign, # my svy design object
                 na.rm=T)
})

####This computes the proportion of U5 who received medical treatment for fever by surveyed cluster for 2014 BF MIS 
system.time({
  med14_clu<-svyby(formula=~med_fever,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=med14.svydesign, # my svy design object
                 na.rm=T)
})

#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(med_clu)[3]<- "standard_error" #for 2017 
colnames(med14_clu)[3]<- "standard_error" #for 2014 


####we want to merge with the full list of health districts
#we start with the proportion of children with medical treatment for fever by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_m<-DS_merge%>%left_join(medical) #for 2017 
DS_merge_m14<-DS_merge%>%left_join(medical_14) #for 2014

####next we merge the number of under-five children within each surveyed cluster  
####with the number of under-five children that had medical treatment for fever by surveyed cluster
#this is for 2017 
pts_est_m<-m_num_clu%>%left_join(med_clu) 
#this is for 2014
pts_est_m14<-m14_num_clu%>%left_join(med14_clu) 

#next we merge the full list of pts/clusters with cluster level estimates of # of children and children with fever
#this is for 2017 
pts_merge_m<-pts_merge%>%left_join(pts_est_m)%>%
             mutate(DHSCLUST = pts_merge$v001)%>%
            mutate(`U5 in DHS clusters` = round(num_kids,0))

#this is for 2014 
pts_merge_m14<-pts_merge%>%left_join(pts_est_m14)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`U5 in DHS clusters` = round(num_kids,0))

####creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_m,file="outputs/med_BF17DS.csv") #for the 2017 BF MIS
write.csv(DS_merge_m14,file="outputs/med_BF14DS.csv") #for the 2014 BF MIS

#####################################
# -- Now let's map our findings -- #
#####################################
####merging the health district fever stats to the sf DS shape file 
DS_shape_m <- merge(DSshape_sf, DS_merge_m, by = "NOMDEP") #this is for 2017 BF MIS 
DS_shape_m14 <- merge(DSshape_sf, DS_merge_m14, by = "NOMDEP") #this is for 2014 BF MIS 

#merging the points file to the points cluster values 
pt_m <- merge(pt_sf,pts_merge_m, by = "DHSCLUST")%>% #this is for 2017 BF MIS 
  mutate(`Medical treatment for fever by cluster`=med_fever)
pt_m14 <- merge(pt_sf_14,pts_merge_m14, by = "DHSCLUST")%>% #this is for 2014 BF MIS 
  mutate(`Medical treatment for fever by cluster`=med_fever)

#st_write(pt_m, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2017/medtreat_2017_cluster.csv")
#st_write(pt_m14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2014/medtreat_2014_cluster.csv")

#this is for 2017 BF 
med_2017_map <- tm_shape(DS_shape_m) + #this is the health district shapfile with medical treatment info
  tm_polygons(col = "med_fever", textNA = "No data", 
              title = "Prevalence of medical treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Medical treatment for fever among children under age 5 by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_m)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Medical treatment for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")

#this is for 2014 BF 
med_2014_map <- tm_shape(DS_shape_m14) + #this is the health district shapfile with medical treatment info
  tm_polygons(col = "med_fever", textNA = "No data", 
              title = "Prevalence of medical treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                                0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Medical treatment for fever among children under age 5 by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_m14)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Medical treatment for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")

####print map 
med_2017_map
med_2014_map


####saving map
tmap_save(tm = med_2017_map, filename = "outputs/medfever_map_2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = med_2014_map, filename = "outputs/medfever_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")





#####################################################################################################
# -- setting up the survey related variables and survey design object for ACT taken for fever-- #
####################################################################################################
#### This is for ACT for fever mis 2017 
mis17_act<-mis17_l[mis17_l$var == "ml13e_1"|mis17_l$var == "ml13e_2"|mis17_l$var == "ml13e_3",]%>%
            mutate(wt=v005/1000000,
            strat=v022,
            id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
            rename(act = count)%>%
            filter(!is.na(wt))%>% 
            filter(!is.na(act))


act.svydesign <- svydesign(id= ~id,
                           strata=~strat,nest=T, 
                           weights= ~wt, data=mis17_act)

#### This is for ACT for fever mis 2014 
mis14_act<-mis14_l[mis14_l$var == "ml13e_1"|mis14_l$var == "ml13e_2"|mis14_l$var == "ml13e_3",]%>%
           mutate(wt=v005/1000000,
          strat=v022,
          id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster  
          rename(act = count)%>%   
          filter(!is.na(wt))%>% 
          filter(!is.na(act))


act14.svydesign <- svydesign(id= ~id,
                           strata=~strat,nest=T, 
                           weights= ~wt, data=mis14_act)

###################################################################################################
#---act for fever estimation and mapping ---#
#################################################################################################


#####################################################################################################
# -- Proportion of U5 children that received act for fever by health district -- #
#####################################################################################################
####This calculates the proportion of children with fever that received act for fever
####by health district for 2017 BF MIS 
system.time({
  act<-svyby(formula=~act,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=act.svydesign, # my svy design object
                 na.rm=T)
})


####This calculates the proportion of children with fever that received act for fever
####by health district for 2014 BF MIS 
system.time({
  act14<-svyby(formula=~act,# which variable do we want estimate
             by=~NOMDEP, # by which variable
             FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
             design=act14.svydesign, # my svy design object
             na.rm=T)
})


######################################################################################################
# -- Number of U5 represented in the question on ACT for fever by cluster -- #
####################################################################################################
####This computes the total number of under-five children within each surveyed cluster for this variable 2017 BF MIS 
system.time({
  act_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                   design=act.svydesign, # my svy design object
                   na.rm=T)
}) 

####This computes the total number of under-five children within each surveyed cluster for this variable 2014 BF MIS 
system.time({
  act14_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                     by=~v001, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                     design=act14.svydesign, # my svy design object
                     na.rm=T)
}) 


#############################################################################################
# -- Proportion of U5 children who received act for fever by cluster -- #
#############################################################################################
####This computes the proportion of U5 who received ACT for fever by surveyed cluster for 2017 BF MIS 
system.time({
  act_clu<-svyby(formula=~act,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=act.svydesign, # my svy design object
                 na.rm=T)
})

####This computes the proportion of U5 who received ACT for fever by surveyed cluster for 2014 BF MIS 
system.time({
  act14_clu<-svyby(formula=~act,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=act14.svydesign, # my svy design object
                 na.rm=T)
})

#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(act_clu)[3]<- "standard_error" #for 2017 
colnames(act14_clu)[3]<- "standard_error" #for 2014

####we want to merge with the full list of health districts
#we start with the proportion of children that used ACT for fever by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_act<-DS_merge%>%left_join(act) #for 2017 
DS_merge_act14<-DS_merge%>%left_join(act14) #for 2014 

####next we merge the number of under-five children within each surveyed cluster  
####with the number of under-five children that had ACT for fever by surveyed cluster
pts_est_act<-act_num_clu%>%left_join(act_clu) #this is for 2017
pts_est_act14<-act14_num_clu%>%left_join(act14_clu) #this is for 2014

####next we merge the full list of pts/clusters with cluster level estimates of # of children and 
####children with ACT for fever
#this is for 2017 
pts_merge_act<-pts_merge%>%left_join(pts_est_act)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`U5 in DHS clusters` = round(num_kids,0))


#this is for 2014 
pts_merge_act14<-pts_merge%>%left_join(pts_est_act14)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`U5 in DHS clusters` = round(num_kids,0))

####creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_act,file="outputs/act_BF17DS.csv") #for the 2017 BF MIS
write.csv(DS_merge_act14,file="outputs/act_BF14DS.csv") #for the 2014 BF MIS

#####################################
# -- Now let's map our findings -- #
#####################################
####merging the health district fever stats to the sf DS shape file 
DS_shape_act <- merge(DSshape_sf, DS_merge_act, by = "NOMDEP") #this is for 2017 BF MIS 
DS_shape_act14 <- merge(DSshape_sf, DS_merge_act14, by = "NOMDEP") #this is for 2014 BF MIS 

#merging the points file to the points cluster values 
pt_act <- merge(pt_sf,pts_merge_act, by = "DHSCLUST")%>% #this is for 2017 BF MIS 
  mutate(`ACT for fever by cluster`=act)

pt_act14 <- merge(pt_sf,pts_merge_act14, by = "DHSCLUST")%>% #this is for 2014 BF MIS 
  mutate(`ACT for fever by cluster`=act)

#st_write(pt_act, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/ACT_fever_2014/act_17.csv")
#st_write(pt_act14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/ACT_fever_2017/act_14.csv")

#this is for 2017 BF 
ACT_2017_map <- tm_shape(DS_shape_act) + #this is the health district shapfile with ACT info
  tm_polygons(col = "act", textNA = "No data", 
              title = "Prevalence of ACT treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                                0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="ACT for fever among children under age 5 by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_act)+ #this is the points shape file with ACT for fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "ACT for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")

#this is for 2014 BF 
ACT_2014_map <- tm_shape(DS_shape_act14) + #this is the health district shapfile with ACT info
  tm_polygons(col = "act", textNA = "No data", 
              title = "Prevalence of ACT treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                            0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="ACT for fever among children under age 5 by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_act14)+ #this is the points shape file with ACT for fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "ACT for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


####print map 
ACT_2017_map
ACT_2014_map

####saving map
tmap_save(tm = ACT_2017_map, filename = "outputs/ACTfever_map_2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = ACT_2017_map, filename = "outputs/ACTfever_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")










####################################################################################################################
# -- setting up the survey related variables and survey design object for sp/fansidar taken for IPTp-- #
####################################################################################################################
#### This is for sp/fansidar for IPTp mis 2017 
mis17_spf<-mis17_r%>% mutate(wt=v005/1000000, strat=v022,
         id=v021, num_f=1)%>% #column for the number of women so that we can compute the totals by cluster 
          rename(sp_f = m49a_1)%>%
           filter(!is.na(wt))


spf17.svydesign <- svydesign(id= ~id,
                           strata=~strat,nest=T, 
                           weights= ~wt, data=mis17_spf)


#### This is for sp/fansidar for IPTp mis 2014 
mis14_spf<-mis14_l[mis14_l$var == "s307a",]%>%
  mutate(wt=v005/1000000,
         strat=v022,
         id=v021, num_f=1)%>% #column for the number of women so that we can compute the totals by cluster  
  rename(sp_f = count)%>%   
  filter(!is.na(wt))%>% 
  filter(!is.na(sp_f))

spf.svydesign <- svydesign(id= ~id,
                             strata=~strat,nest=T, 
                             weights= ~wt, data=mis14_spf)
###################################################################################################
#---IPTp with sp/fansidar estimation and mapping ---#
#################################################################################################


#####################################################################################################
# -- Proportion of women that received IPTp with sp/fanisdar for malaria by health district -- #
#####################################################################################################
####This calculates the proportion of women that received IPTp with sp/fansidar for malaria
####by health district for 2017 BF MIS 
system.time({
  iptp_17<-svyby(formula=~sp_f,# which variable do we want estimate
              by=~NOMDEP, # by which variable
              FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
              design=spf17.svydesign, # my svy design object
              na.rm=T)
})


####This calculates the proportion of women that received IPTp with sp/fansidar for malaria
####by health district for 2014 BF MIS 
system.time({
  iptp<-svyby(formula=~sp_f,# which variable do we want estimate
             by=~NOMDEP, # by which variable
             FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
             design=spf.svydesign, # my svy design object
             na.rm=T)
})

####stopped here
######################################################################################################
# -- Number of women that responded to the question on IPTp with sp/fansidar for fever by cluster -- #
####################################################################################################
####This computes the total number of women within each surveyed cluster for this variable 2017 BF MIS 
system.time({
  iptp17_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                      design=spf17.svydesign, # my svy design object
                      na.rm=T)
}) 



####This computes the total number of women within each surveyed cluster for this variable 2014 BF MIS 
system.time({
  iptp_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                     by=~v001, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                     design=spf.svydesign, # my svy design object
                     na.rm=T)
}) 

#############################################################################################
# -- Proportion of women that received IPTp with sp/fansidar for malaria by cluster -- #
#############################################################################################
####This computes the number of women that received sp/fansidar for malaria by surveyed cluster for 2017 BF MIS 
system.time({
  iptp17_clu<-svyby(formula=~sp_f,# which variable do we want estimate
                  by=~v001, # by which variable
                  FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                  design=spf17.svydesign, # my svy design object
                  na.rm=T)
})




####This computes the number of women that received sp/fansidar for malaria by surveyed cluster for 2014 BF MIS 
system.time({
  iptp_clu<-svyby(formula=~sp_f,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=spf.svydesign, # my svy design object
                 na.rm=T)
})


#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(iptp17_clu)[3]<- "standard_error" #for 2017
colnames(iptp_clu)[3]<- "standard_error" #for 2014

####we want to merge with the full list of health districts
#we start with the proportion of women that received IPTp with sp/fansidar by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_iptp17<-DS_merge%>%left_join(iptp_17) #for 2017
DS_merge_iptp<-DS_merge%>%left_join(iptp) #for 2014 

####next we merge the number of women within each surveyed cluster  
####with the number of under-five children that had IPTp with sp/fansidar for malaria by surveyed cluster
pts_est_iptp17<-iptp17_num_clu%>%left_join(iptp17_clu) #this is for 2017
pts_est_iptp<-iptp_num_clu%>%left_join(iptp_clu) #this is for 2014


####next we merge the full list of pts/clusters with cluster level estimates of # of children and 
####children with ACT for fever
#this is for 2017
pts_merge_iptp17<-pts_merge%>%left_join(pts_est_iptp17)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`Women in DHS clusters` = round(num_f,0))


#this is for 2014 
pts_merge_iptp<-pts_merge%>%left_join(pts_est_iptp)%>%
  mutate(DHSCLUST = pts_merge$v001)%>%
  mutate(`Women in DHS clusters` = round(num_f,0))


####creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_iptp17,file="outputs/IPTp_spf_BF17DS.csv") #for the 2017 BF MIS
write.csv(DS_merge_iptp,file="outputs/IPTp_spf_BF14DS.csv") #for the 2014 BF MIS

#####################################
# -- Now let's map our findings -- #
#####################################
####merging the health district fever stats to the sf DS shape file 
DS_shape_iptp17 <- merge(DSshape_sf, DS_merge_iptp17, by = "NOMDEP") #this is for 2017 BF MIS 
DS_shape_iptp <- merge(DSshape_sf, DS_merge_iptp, by = "NOMDEP") #this is for 2014 BF MIS 

####merging the points file to the points cluster values 
pt_iptp17 <- merge(pt_sf,pts_merge_iptp17, by = "DHSCLUST")%>% #this is for 2017 BF MIS 
  mutate(`IPTp with sp/fansidar by cluster`=sp_f)

pt_iptp <- merge(pt_sf,pts_merge_iptp, by = "DHSCLUST")%>% #this is for 2014 BF MIS 
  mutate(`IPTp with sp/fansidar by cluster`=sp_f)

####writing the cluster-level values to a csv file 
#st_write(pt_iptp17,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IPTP_spf_2017/iptp17.csv")
#st_write(pt_iptp,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IPTP_spf_2014/iptp14.csv")




#this is for 2017 BF 
IPTP_2017_map <- tm_shape(DS_shape_iptp17) + #this is the health district shapfile with IPTp with sp/fansidar
  tm_polygons(col = "sp_f", textNA = "No data", 
              title = "IPTP with sp/fansidar prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                       0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="IPTp with sp/fansidar by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_iptp17)+ #this is the points shape file with IPTp with sp/fansidar and number of women info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "IPTp with sp/fansidar by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#this is for 2014 BF 
IPTP_2014_map <- tm_shape(DS_shape_iptp) + #this is the health district shapfile with IPTp with sp/fansidar
  tm_polygons(col = "sp_f", textNA = "No data", 
              title = "IPTP with sp/fansidar prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                            0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="IPTp with sp/fansidar by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_iptp)+ #this is the points shape file with IPTp with sp/fansidar and number of women info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "IPTp with sp/fansidar by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


####print map
IPTP_2017_map
IPTP_2014_map

####saving map
tmap_save(tm = IPTP_2017_map, filename = "outputs/IPTP_spf_map_2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = IPTP_2014_map, filename = "outputs/IPTP_spf_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")





#####################################################################################################
#---Mapping the month of the survey---#
#####################################################################################################
####First we create column names and contigency table of cluster number and month of survey for the 2017 BF MIS  
oldnames=c('1', '2', '3', '11', '12')
newnames=c('January', 'February', 'March', 'November','December')

mis17_df <- mis17_r%>% dplyr:: select(v001, MM)%>%
  group_by(v001, MM) %>%
  summarise(n=n())%>%
  spread(MM, n)%>%
  rename_at(vars(oldnames), ~newnames)%>%
  mutate




####we want to rename the columns to their corresponding month 
oldnames=c('1', '2', '3', '11', '12')
newnames=c('January', 'February', 'March', 'November','December')

####we save the crosstab as a dataframe, create the DHSCLUST variable for the join and rename
new_df<- cross_month%>%as.data.frame.matrix(cross_month)%>%
          mutate(DHSCLUST = 1:nrow(cross_month))%>% 
          rename_at(vars(oldnames), ~newnames)%>%
          mutate(January = ifelse(January > 0, 'January', 0))%>%
          mutate(February = ifelse(February > 0, 'February', 0))%>%
          mutate(March = ifelse(March > 0, 'March', 0))%>%
          mutate(November = ifelse(March > 0, 'November', 0))%>%
          mutate(December = ifelse(March > 0, 'December', 0))

sur_month <- merge(pt_sf,new_df, by = "DHSCLUST") #this is for 2017 BF MIS 
 
summary(pt_sf$DHSCLUST)
summary(new_df$DHSCLUST)