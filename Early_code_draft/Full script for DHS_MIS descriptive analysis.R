rm(list=ls())
  
setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs") 


## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
      "lubridate", "RColorBrewer", "plotrix", "ggrepel", "sf", "shinyjs", "tmap", "knitr")
lapply(x, library, character.only = TRUE) #applying the library function to packages
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


## Reading in all BF indvidual, household and person recode files 


in_files_I <- list.files(pattern = ".*BFIR.*\\.DTA", recursive = TRUE)
in_files_H <- list.files(pattern = ".*BFHR.*\\.DTA", recursive = TRUE)
in_files_P <- list.files(pattern = ".*BFPR.*\\.DTA", recursive = TRUE)

filenames_I <- str_sub(in_files_I, end = -5)
filenames_H <- str_sub(in_files_H, end = -5)
filenames_P <- str_sub(in_files_P, end = -5)

filenames <- rbind(filenames_I, filenames_H, filenames_P)

for(i in filenames){
  filepath <- file.path(paste(i,".dta",sep=""))
  assign(i, read_dta(filepath))
}


## Renaming files 


#this is for individual recode 
bf10dhs <- `BF_2010_DHS_06192019/BFIR62DT/BFIR62FL` 
bf14mis <- `BF_2014_MIS_06192019/BFIR70DT/BFIR70FL`
bf17mis <- `BF_2017-18_MIS_07252019_1531_86355/BFIR7ADT/BFIR7AFL`

#this is for the person recode 
bfprdhs10 <- `BF_2010_DHS_06192019/BFPR62DT/BFPR62FL`
bfprmis14 <- `BF_2014_MIS_06192019/BFPR70DT/BFPR70FL`
bfprmis18 <- `BF_2017-18_MIS_07252019_1531_86355/BFPR7ADT/BFPR7AFL`

#this is the household recode 
bfhrdhs10 <- `BF_2010_DHS_06192019/BFHR62DT/BFHR62FL`
bfhrmis14 <- `BF_2014_MIS_06192019/BFHR70DT/BFHR70FL`
bfhrmis18 <- `BF_2017-18_MIS_07252019_1531_86355/BFHR7ADT/BFHR7AFL`


## Reading in the shapefiles of GPS points and district boundaries for purposes of mapping and estimation 


#GPS points
pts_10 <-readOGR("BF_2010_DHS_06192019/BFGE61FL", layer = "BFGE61FL")
pts_14 <- readOGR("BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL")
pts<- readOGR("BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL", layer = "BFGE7AFL")

#DS file
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

#converting the sp objects to sf object for easy plotting with tmap later 
pt_sf_10 <-st_as_sf(pts_10)
pt_sf_14 <- st_as_sf(pts_14)
pt_sf_17 <-st_as_sf(pts)
DSshape_sf<-st_as_sf(DS_shape)

## Quality checks and creating new files 

#DS is in the utm + zone 30 & points is in long + lat. lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,crs(pts))

#check map projections again 
crs(DS_shape_W)
crs(pts)

#Check if the GPS points  are contained in the polygons 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2010 clusters')
plot(pts_10,add=T,col=4) #this is dhs 10
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with MIS 2014 clusters')
plot(pts_14,add=T,col=4) #this is mis 14 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2017 clusters')
plot(pts,add=T,col=4) #this mis 17 

#set up an empty dataframe with the DS names so DS with no estimates can be identified 
DS_merge<-data.frame(NOMDEP=DS_shape_W@data[,"NOMDEP"])

## Mapping points to inherit associated health district


#the dimensions of the pts data is 573 by 20 and of the DS is 70 by 10, key_10 will be 573 by 10 
dim(pts_10@data) # this is for dhs 10
dim(DS_shape_W@data)
key_10<-over(SpatialPoints(coordinates(pts_10),proj4string = pts_10@proj4string), DS_shape_W)
dim(key_10)
length(unique(key_10$NOMDEP))

# add in the cluster variable
key_10$v001<-pts_10@data[,"DHSCLUST"]

#the dimensions of the pts data is 252 by 20 and of the DS is 70 by 10, key_14 will be 252 by 10
dim(pts_14@data) # this is for mis 14 
dim(DS_shape_W@data)
key_14<-over(SpatialPoints(coordinates(pts_14),proj4string = pts_14@proj4string), DS_shape_W)
dim(key_14)
length(unique(key_14$NOMDEP))

#add in the cluster variable
key_14$v001<-pts_14@data[,"DHSCLUST"]

#if pts@data is 245 by 20 and district is 70 by 10, then the corresponding data frame will be 245 by 10
dim(pts@data)  # this is for mis 17 
dim(DS_shape_W@data)
key<-over(SpatialPoints(coordinates(pts),proj4string = pts@proj4string), DS_shape_W)
dim(key)
length(unique(key$NOMDEP))

#add in the cluster variable
key$v001<-pts@data[,"DHSCLUST"]


## Creating and Recoding variables 


#creating needed variables 
bfprdhs10 <- bfprdhs10%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% #dhs pr 2010
  dplyr::rename(v001 = hv001) %>%
  mutate(YYYY = (floor((hv008 - 1)/12)+1900))%>%
  mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
  mutate(time2 = str_c(MM, YYYY, sep = '-'))

bfprmis14 <- bfprmis14%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>%  #dhs pr 2010 
  dplyr::rename(v001 = hv001)%>%
  mutate(YYYY = (floor((hv008 - 1)/12)+1900))%>% 
  mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
  mutate(time2 = str_c(MM, YYYY, sep = '-'))


#first we establish a recoder to recode variables using a function 
recoder <- function(x){
  ifelse(x == 8, NA,ifelse(x == 3 | x == 0, 0, 1))
}

#next to the 2014 IR dataset 
mis14_r <- bf14mis %>%
  mutate_at(vars(contains('ml0')), recoder)%>% #recodes for LLINs 
  mutate_at(vars(contains('h22')), recoder)    #recodes for the fever variables 

#check if recoding works 
table(mis14_r$ml0_1)
table(bf14mis$ml0_1)

#this is for the 2017 IR dataset 
mis17_r <- bf17mis%>%#this is the 2017 dataset 
  mutate_at(vars(contains('ml0')), recoder)%>% #recodes for LLINs 
  mutate_at(vars(contains('h22')), recoder) %>%   #recodes for the fever variables 
  mutate_at(vars('m49a_1'), recoder)%>% #recodes for the IPTp with sp/fansidar 
  mutate_at(vars('ml101'), recoder) #recodes for LLIN use by adult women 

#this is for the 2010 HR dataset 
hrdhs10 <- bfhrdhs10%>%
  mutate_at(vars(contains('hv253')), recoder)%>%  #recodes (missing - 8, don't know - 9)
  rename(v001 = hv001)

#this is for the 2014 HR dataset 
hrmis14 <- bfhrmis14%>%
  mutate_at(vars(contains('hv253')), recoder)%>% #recodes (missing - 8)
  rename(v001 = hv001)

#this is for the 2010 PR dataset 
prdhs10 <- bfprdhs10%>%
  mutate_at(vars(contains('hml35')), recoder) #recodes for rapid test results 

####this is for the 2014 PR dataset 
prmis14 <- bfprmis14%>% 
  mutate_at(vars(contains('hml32')), recoder)%>% #recodes for smear test results  
  mutate_at(vars(contains('hml35')), recoder) #recodes for rapid test results  


## Joining the MIS data to the key that contains the names of each health district


#we start with the IR datasets 
#a few descriptive stats for IR dhs 2010  
dim(bf10dhs)# how many rows?
dim(key_10) #how many columns?
length(unique(bf10dhs$v001))
length(unique(key_10$v001)) #
bf10dhs_work<-bf10dhs%>%left_join(key_10)

#should still have the same number of rows and a few new columns
dim(bf10dhs_work) #this is the dataset to be used for subsequent 2010 computations that don't require long format  

#a few descriptive stats for IR mis 14 
dim(mis14_r)# how many rows?
dim(key_14) #how many columns?
length(unique(mis14_r$v001))
length(unique(key_14$v001)) #
mis14_r<-mis14_r%>%left_join(key_14)

####should still have the same number of rows and a few new columns
dim(mis14_r) #this is the dataset that will be used for subsequent 2014 computations that don't require long format  

#a few descriptive stats for IR mis 17 
dim(mis17_r)# how many rows?
dim(key) #how many columns?
length(unique(mis17_r$v001))
length(unique(key$v001)) # it looks like the cluster variables match!
mis17_r<-mis17_r%>%left_join(key)

#should still have the same number of rows and a few new columns
dim(mis17_r) 
#this is the dataset that will be used for subsequent 2017 computations that don't require long format


#Next we do the same joining procedure for the HR datasets 
#a few descriptive stats for HR dhs 2010 
dim(hrdhs10)# how many rows?
dim(key_10) #how many columns?
length(unique(hrdhs10$v001))
length(unique(key_10$v001)) # it looks like the cluster variables match!
hrdhs10<-hrdhs10%>%left_join(key_10)

#should still have the same number of rows and a few new columns
dim(hrdhs10) #this is the dataset that will be used for subsequent 2010 computations 

#a few descriptive stats for HR mis 2014 
dim(hrmis14)# how many rows?
dim(key_14) #how many columns?
length(unique(hrmis14$v001))
length(unique(key_14$v001)) # it looks like the cluster variables match!
hrmis14<-hrmis14%>%left_join(key_14)

#should still have the same number of rows and a few new columns
dim(hrmis14) #this is the dataset that will be used for subsequent 2014 computations

#finally the key datasets and dhs/mis datasets are joined for the PR datasets 
#a few descriptive stats for dhs PR 2010 
dim(prdhs10)# how many rows?
dim(key_10) #how many columns?
length(unique(prdhs10$v001))
length(unique(key_10$v001)) # it looks like the cluster variables match!
prdhs10<-prdhs10%>%left_join(key_10)

#should still have the same number of rows and a few new columns
dim(prdhs10) #this is the dataset to be used for subsequent 2010 computations that don't require long format 


#a few descriptive stats for mis PR 2014 
dim(prmis14)# how many rows?
dim(key_14) #how many columns?
length(unique(prmis14$v001))
length(unique(key_14$v001)) # it looks like the cluster variables match!
prmis14<-prmis14%>%left_join(key_14)

#should still have the same number of rows and a few new columns
dim(prmis14) #this is the dataset to be used for subsequent 2014 computations that don't require long format 

#checking the number of NAs and numbers in the age of child in months variable 
summary(is.na(prmis14$hc1))



## Changing the IR dataset to long format to aggregate observations across children 


#this is the dataset for analysis involving U5 for mis 2014
mis14_l<- mis14_r%>%dplyr::select(b2_01:b2_04, ml0_1:ml0_4,h22_1:h22_4,h32z_1:h32z_3, ml13e_1:ml13e_3,s307a,
                                  caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  mutate_at(vars(contains('h22')), as.numeric)%>%
  mutate_at(vars(contains('h32z')), as.numeric)%>%
  mutate_at(vars(contains('ml13e')), as.numeric)%>%
  mutate_at(vars("s307a"), as.numeric)%>%
  pivot_longer(cols = ml0_1:s307a, names_to = "var", values_to = "count")


#this is for 2017
mis17_l<- mis17_r%>%dplyr::select(b2_01:b2_04, ml0_1:ml0_4, h22_1:h22_4,h32z_1:h32z_3,ml13e_1:ml13e_3,
                                  caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  mutate_at(vars(contains('h22')), as.numeric)%>%
  mutate_at(vars(contains('h32z')), as.numeric)%>%
  mutate_at(vars(contains('ml13e')), as.numeric)%>%
  pivot_longer(cols = ml0_1:ml13e_3, names_to = "var", values_to = "count")



# U5 LLIN use

## Setting up the survey-related variables and survey design object for U5 LLIN


#selecting only LLIN variables for analysis and other data processing for 2014 IR BF MIS
mis14_ml0<-mis14_l[mis14_l$var == "ml0_1"|mis14_l$var == "ml0_2"|mis14_l$var == "ml0_3"|mis14_l$var == "ml0_4",] %>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster
  rename(LLIN_use = count) %>%
  filter(!is.na(wt))%>% 
  filter(!is.na(LLIN_use))


svydesign_14 <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis14_ml0)


#selecting only LLIN variables for analysis and other data processing for 2017 IR BF MIS 
mis17_ml0<-mis17_l[mis17_l$var == "ml0_1"|mis17_l$var == "ml0_2"|mis17_l$var == "ml0_3"|mis17_l$var == "ml0_4",] %>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  rename(LLIN_use = count) %>%
  filter(!is.na(wt))%>% 
  filter(!is.na(LLIN_use))


my.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis17_ml0)


## Proportion of U5 children who slept under a treated bednet by health district


#This formula computes the proportion of children that slept under a bednet by health district for 2014 BF MIS 
system.time({
  Bednet_14<-svyby(formula=~LLIN_use,# which variable do we want estimate
                   by=~NOMDEP, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=svydesign_14, # my svy design object
                   na.rm=T)
}) 

head(Bednet_14)

#This formula computes the proportion of children that slept under a bednet by health district for 2017 BF MIS 
system.time({
  Bednet<-svyby(formula=~LLIN_use,# which variable do we want estimate
                by=~NOMDEP, # by which variable
                FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                design=my.svydesign, # my svy design object
                na.rm=T)
})

head(Bednet)


## Number of U5 children within each cluster 


#This formula computes the total number of U5 children within each surveyed cluster for 2014 IR BF MIS 
system.time({
  num_clu_14<-svyby(formula=~num_kids,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                    design=svydesign_14, # my svy design object
                    na.rm=T) %>% 
    mutate(`U5 in DHS clusters` = round(num_kids, 0))
  
}) 

head(num_clu_14)

#This formula computes the total number of under-five children within each surveyed cluster for 2017 IR BF MIS 
system.time({
  num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                 design=my.svydesign, # my svy design object
                 na.rm=T)%>% 
    mutate(`U5 in DHS clusters` = round(num_kids, 0))
})

head(num_clu)


## Proportion of U5 children who slept under a treated bednet by cluster

#This is for the 2014 IR BF MIS 
system.time({
  Bednet_clu_14<-svyby(formula=~LLIN_use,# which variable do we want estimate
                       by=~v001, # by which variable
                       FUN=svymean, # what is the function (svymean gives the HT estimate and variance)
                       design=svydesign_14, # my svy design object
                       na.rm=T)
  
}) # this one takes 67 seconds on my desktop

head(Bednet_clu_14)

#This is for 2017 IR BF MIS 
system.time({
  Bednet_clu<-svyby(formula=~LLIN_use,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                    design=my.svydesign, # my svy design object
                    na.rm=T)
  
}) # this one takes 67 seconds on my desktop

head(Bednet_clu)


## Dataset clean-up for merging and mapping LLIN variables 


#renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu_14)[3]<- "standard_error" #for 2014 IR BF MIS dataset 

#renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu)[3]<- "standard_error" #for 2017 IR BF MIS dataset 

#we want to merge with the bednet DS estimates with full list of DS to identify DS with no estimates
dim(DS_merge)
DS_merge_n<-DS_merge%>%left_join(Bednet) #the 2017 IR BF MIS 
DS_merge_14<-DS_merge%>%left_join(Bednet_14) #2014 IR BF MIS

#next we merge the # of U5 children in each cluster with the # of U5 children that slept under a bednet by cluster
pts_estimates_14<-num_clu_14%>%left_join(Bednet_clu_14)%>% #this is for 2014 IR BF MIS 
  rename (DHSCLUST = v001)


pts_estimates<-num_clu%>%left_join(Bednet_clu) %>%  #this is 2017 IR BF MIS 
  rename(DHSCLUST = v001)


#creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_n,file="outputs/new/LLIN_use_BF17DS.csv") #for the 2017 IR BF MIS
write.csv(DS_merge_14,file="outputs/new/LLIN_use_BF14DS.csv") #for the 2014 IR BF MIS


## U5 LLIN maps 


#merging the LLIN stats to the sf DS shape file created earlier 
DS_shape_14 <- DSshape_sf%>%left_join(DS_merge_14) #this is for 2014 BF MIS 
DS_shape_n <- DSshape_sf%>%left_join(DS_merge_n) #this is for 2017 BF MIS  


#merging the LLIN stats to the points sf file created earlier 
pt_n_14 <- pt_sf_14%>%left_join(pts_estimates_14)%>% #2014 BF MIS 
  rename(`LLIN use prevalence by cluster`= LLIN_use)
pt_n <- pt_sf_17%>%left_join(pts_estimates)%>% #for 2017 BF MIS 
  rename(`LLIN use prevalence by cluster`= LLIN_use)


#this creates shapefiles with cluster-level estimates
st_write(pt_n, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_2017/LLIN_2017_cluster.csv")
st_write(pt_n_14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_2014/LLIN_2014_cluster.csv")


#this is for 2014 BF 
LLIN_2014_map <- tm_shape(DS_shape_14) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="LLIN use among U5 children by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_n_14)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#this is for 2017 BF 
LLIN_2017_map <- tm_shape(DS_shape_n) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="LLIN use among U5 children by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_n)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")



#print map
LLIN_2014_map
LLIN_2017_map



#save maps 
tmap_save(tm = LLIN_2014_map, filename = "outputs/LLIN_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
tmap_save(tm = LLIN_2017_map, filename = "outputs/LLIN_map_2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



#  Adult women's LLIN access

## Setting up the survey related variables and survey design object for women's LLIN access


#selecting only adult LLIN access variables for analysis and other data processing
mis14_s127<-mis14_r%>%  #This is for 2014 IR BF MIS 
  rename(LLIN_adult = s127)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of women so that we can compute the totals by cluster 
  filter(!is.na(wt)) 


s127.svydesign <- svydesign(id= ~id,
                            strata=~strat,nest=T, 
                            weights= ~wt, data=mis14_s127)



mis17_v459<-mis17_r%>%  #This is for 2017 IR BF MIS 
  rename(LLIN_adult = v459)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of women so that we can compute the totals by cluster 
  filter(!is.na(wt)) 


v459.svydesign <- svydesign(id= ~id,
                            strata=~strat,nest=T, 
                            weights= ~wt, data=mis17_v459)


## Proportion of women who own a bednet by health district


#this is for 2014 IR BF MIS 
system.time({
  Bednet_s127<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                     by=~NOMDEP, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=s127.svydesign, # my svy design object
                     na.rm=T)
})

head(Bednet_s127)

#this is for 2017 IR BF MIS 
system.time({
  Bednet_v459<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                     by=~NOMDEP, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=v459.svydesign, # my svy design object
                     na.rm=T)
})

head(Bednet_v459)


## Number of women in each cluster


#this is 2014 IR BF MIS 
system.time({
  s127_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                      design=s127.svydesign, # my svy design object
                      na.rm=T)%>%
    mutate(`Women in DHS clusters` = round(num_f, 0))
})

head(s127_num_clu)

#this for 2017 IR BF MIS 
system.time({
  v459_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                      design=v459.svydesign, # my svy design object
                      na.rm=T)%>%
    mutate(`Women in DHS clusters` = round(num_f, 0))
}) 

head(v459_num_clu)


## Proportion of women who own a bednet by cluster


#this is for the 2014 IR BF MIS 
system.time({
  Bednet_clu_s127<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the HT estimate and variance)
                         design=s127.svydesign, # my svy design object
                         na.rm=T)
  
})

head(Bednet_clu_s127)

#this is for the 2017 IR BF MIS
system.time({
  Bednet_clu_v459<-svyby(formula=~LLIN_adult,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the HT estimate and variance)
                         design=v459.svydesign, # my svy design object
                         na.rm=T)
  
})

head(Bednet_clu_v459)


## Dataset clean-up for merging and mapping adult women's LLIN access 

#renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu_s127)[3]<- "standard_error" #2014 IR BF MIS 
colnames(Bednet_clu_v459)[3]<- "standard_error" #2017 IR BF MIS 


#we want to merge with the full list of health districts
#we start with the proportion of children that slept under a bednet by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_s127<-DS_merge%>%left_join(Bednet_s127) #2014 IR BF MIS 
DS_merge_v459<-DS_merge%>%left_join(Bednet_v459) #2017 IR BF MIS 


#next we merge the # of women in each cluster with the number women that slept under a bednet by cluster
pts_est_s127<-s127_num_clu%>%left_join(Bednet_clu_s127)%>% #2014 IR BF MIS 
                             mutate(DHSCLUST = v001)


pts_est_v459<-v459_num_clu%>%left_join(Bednet_clu_v459)%>% #2017 IR BF MIS 
                              mutate(DHSCLUST = v001)

#creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_s127,file="outputs/LLIN_adultW_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_v459,file="outputs/LLIN_adultW_BF17DS.csv") #2017 IR BF MIS



## Adult women's LLIN access maps 

#merging the LLIN stats to the sf DS shape file created earlier 
DS_shape_s127 <- DSshape_sf%>%left_join(DS_merge_s127) #2014 IR BF MIS 
DS_shape_v459 <- DSshape_sf%>%left_join(DS_merge_v459) #2017 IR BF MIS  


#merging the LLIN stats to the points sf file created earlier 
pt_s127 <- pt_sf_14%>%left_join(pts_est_s127)%>% #2014 IR BF MIS 
                   rename(`Adult women LLIN access prevalence by cluster`=LLIN_adult)

pt_v459 <- pt_sf_17%>%left_join(pts_est_v459)%>% #2017 IR BF MIS 
            rename(`Adult women LLIN access prevalence by cluster`=LLIN_adult)


#this creates shapefiles with cluster-level estimates of the % of women that slept under a bednet 
st_write(pt_v459,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adultW_2017/LLIN_adultW17.csv")
st_write(pt_s127,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adultW_2014/LLIN_adultW14.csv")


#this is for 2014 IR BF MIS 
LLIN_adultW_2014_map <- tm_shape(DS_shape_s127) + #this is the health district shapefile with LLIn info
  tm_polygons(col = "LLIN_adult", textNA = "No data", 
              title = "LLIN access prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                             0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN access by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_s127)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN access prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#this is for 2017 IR BF MIS 
LLIN_adultW_2017_map <- tm_shape(DS_shape_v459) + #this is the health district shapefile with LLIn info
  tm_polygons(col = "LLIN_adult", textNA = "No data", 
              title = "LLIN access prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                             0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN access by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_v459)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN access prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")




#print map 
LLIN_adultW_2014_map
LLIN_adultW_2017_map


#save maps 
tmap_save(tm = LLIN_adultW_2014_map, filename = "outputs/LLIN_adultW_map2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = LLIN_adultW_2017_map, filename = "outputs/LLIN_adultW_map2017.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)



# Adult women's LLIN use

## Setting up the survey related variables and survey design object for women's LLIN use

mis14_s129<-mis14_r%>% #this is for 2014 IR BF MIS 
  rename(LLIN_use_a = s129)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  filter(!is.na(wt)) 


s129.svydesign <- svydesign(id= ~id,
                            strata=~strat,nest=T, 
                            weights= ~wt, data=mis14_s129)

mis17_ml101<-mis17_r%>% # this is for 2017 IR BF MIS 
  rename(LLIN_use_a = ml101)%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_f=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  filter(!is.na(wt)) 



ml101.svydesign <- svydesign(id= ~id,
                             strata=~strat,nest=T, 
                             weights= ~wt, data=mis17_ml101)


## Proportion of women who use a bednet by health district


#this is for 2014 IR BF MIS 
system.time({
  Bednet_s129<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                     by=~NOMDEP, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=s129.svydesign, # my svy design object
                     na.rm=T)
})

head(Bednet_s129)

#this is for 2017 IR BF MIS
system.time({
  Bednet_ml101<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                      by=~NOMDEP, # by which variable
                      FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                      design= ml101.svydesign, # my svy design object
                      na.rm=T)
})

head(Bednet_ml101)



## Number of women in each cluster


#this computes the total number of women respondents within each surveyed cluster for 2014 IR BF MIS 
system.time({
  s129_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                      design=s129.svydesign, # my svy design object
                      na.rm=T)%>%
    mutate(`Women in DHS clusters` = round(num_f, 0))
}) 

head(s129_num_clu)

#this is for 2017 IR BF MIS 
system.time({
  ml101_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                       by=~v001, # by which variable
                       FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                       design=ml101.svydesign, # my svy design object
                       na.rm=T) %>% 
    mutate(`Women in DHS clusters` = round(num_f, 0))
  
}) 

head(ml101_num_clu)


## Proportion of women who slept under a bednet by cluster


#This computes the proportion of women that slept under a bednet by surveyed cluster for 2014 IR BF MIS 
system.time({
  Bednet_clu_s129<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the HT estimate and variance)
                         design=s129.svydesign, # my svy design object
                         na.rm=T)
  
}) 

head(Bednet_clu_s129)

#this is for 2017 IR BF MIS 
system.time({
  Bednet_clu_ml101<-svyby(formula=~LLIN_use_a,# which variable do we want estimate
                          by=~v001, # by which variable
                          FUN=svymean, # what is the function (svymean gives the HT estimate and variance)
                          design=ml101.svydesign, # my svy design object
                          na.rm=T)
  
})

head(Bednet_clu_ml101)


## Dataset clean-up for merging and mapping adult women's LLIN use


#renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(Bednet_clu_s129)[3]<- "standard_error" #2014 IR BF MIS 
colnames(Bednet_clu_ml101)[3]<- "standard_error" #2017 IR BF MIS  


#Merging the DS-level estimates of women who slept under a bednet with the full list of health districts
dim(DS_merge)
DS_merge_s129<-DS_merge%>%left_join(Bednet_s129) #2014 IR BF MIS 
DS_merge_ml101<-DS_merge%>%left_join(Bednet_ml101) #2017 IR BF MIS 


#Merging the # of women in each cluster with the # of women that slept under a bednet by surveyed cluster
pts_est_s129<-s129_num_clu%>%left_join(Bednet_clu_s129)%>% #2014 IR BF MIS 
  rename(DHSCLUST = v001)

pts_est_ml101<-ml101_num_clu%>%left_join(Bednet_clu_ml101)%>% #2017 IR BF MIS 
  rename(DHSCLUST = v001)


####creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_s129,file="outputs/LLIN_adult_use_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_ml101,file="outputs/LLIN_adult_use_BF17DS.csv") #2017 IR BF MIS


## Adult women's bednet use maps 


#merging the adult women's LLIN use stats to the sf DS shape file created earlier 
DS_shape_s129 <- DSshape_sf%>%left_join(DS_merge_s129) #2014 IR BF MIS 
DS_shape_ml101 <- DSshape_sf%>%left_join(DS_merge_ml101) #2017 IR BF MIS  


#merging the adult women's LLIN use cluster-level stats to the points sf file created earlier 
pt_s129 <- pt_sf_14%>%left_join(pts_est_s129)%>% #2014 IR BF MIS 
  rename(`Adult women LLIN use prevalence by cluster`=LLIN_use_a)

pt_ml101 <- pt_sf_17%>%left_join(pts_est_ml101)%>% #2017 IR BF MIS 
  rename(`Adult women LLIN use prevalence by cluster`=LLIN_use_a)

#this creates a shapefile with cluster-level estimates of the % of women that slept under a bednet 
st_write(pt_s129,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adult_use_2014/LLIN_adultuse14.csv")
st_write(pt_ml101,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adult_use_2017/LLIN_adultuse17.csv")



#2014 IR BF MIS 
LLIN_adult_use_2014_map <- tm_shape(DS_shape_s129) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use_a", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN use by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_s129)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#2017 IR BF MIS 
LLIN_adult_use_2017_map <- tm_shape(DS_shape_ml101) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "LLIN_use_a", textNA = "No data", 
              title = "LLIN use prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Adult women LLIN use by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_ml101)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "Adult women LLIN use prevalence by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")




#print map 
LLIN_adult_use_2014_map
LLIN_adult_use_2017_map

#save maps 
tmap_save(tm = LLIN_adult_use_2014_map, filename = "outputs/LLIN_adult_use_map2014.pdf",
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = LLIN_adult_use_2017_map, filename = "outputs/LLIN_adult_use_map2017.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)


# U5 fever reports

## setting up the survey related variables and survey design object 


#2014 IR BF MIS 
mis14_h22<-mis14_l[mis14_l$var == "h22_1"|mis14_l$var == "h22_2"|mis14_l$var == "h22_3"|mis14_l$var == "h22_4",]%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_kids=1)%>% #number of kids so that we can compute the totals by cluster  
  rename(fever = count) %>%
  filter(!is.na(wt))%>% 
  filter(!is.na(fever))


f14.svydesign <- svydesign(id= ~id,
                           strata=~strat,nest=T, 
                           weights= ~wt, data=mis14_h22)


#2017 IR BF MIS 
mis17_h22<-mis17_l[mis17_l$var == "h22_1"|mis17_l$var == "h22_2"|mis17_l$var == "h22_3"|mis17_l$var == "h22_4",]%>%
  mutate(wt=v005/1000000,strat=v022,
         id=v021, num_kids=1)%>% #number of kids so that we can compute the totals by cluster 
  rename(fever = count)%>%
  filter(!is.na(wt))%>% 
  filter(!is.na(fever))

f.svydesign <- svydesign(id= ~id,
                         strata=~strat,nest=T, 
                         weights= ~wt, data=mis17_h22)


## Proportion of U5 children with fever by health district


#2014 IR BF MIS
system.time({
  fever_14<-svyby(formula=~fever,# which variable do we want estimate
                  by=~NOMDEP, # by which variable
                  FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                  design=f14.svydesign, # my svy design object
                  na.rm=T)
})

head(fever_14)

#2017 IR BF MIS
system.time({
  fever<-svyby(formula=~fever,# which variable do we want estimate
               by=~NOMDEP, # by which variable
               FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
               design=f.svydesign, # my svy design object
               na.rm=T)
})

head(fever)


## Number of U5 by cluster


#2014 IR BF MIS 
system.time({
  f14_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                     by=~v001, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                     design=f14.svydesign, # my svy design object
                     na.rm=T)%>% 
    mutate(`U5 in DHS clusters` = round(num_kids, 0))
})

head(f14_num_clu)

#2017 IR BF MIS 
system.time({
  f_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance )
                   design=f.svydesign, # my svy design object
                   na.rm=T)%>%
    mutate(`U5 in DHS clusters` = round(num_kids, 0))
}) 

head(f_num_clu)


## Proportion of U5 children who had fever by cluster 


#2014 IR BF MIS 
system.time({
  fever14_clu<-svyby(formula=~fever,# which variable do we want estimate
                     by=~v001, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=f14.svydesign, # my svy design object
                     na.rm=T)
})

head(fever14_clu)

#2017 IR BF MIS 
system.time({
  fever_clu<-svyby(formula=~fever,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=f.svydesign, # my svy design object
                   na.rm=T)
})

head(fever_clu)


## Dataset clean-up for merging and mapping U5 fever reports 


#renaming 'se' colname in fever_clu to standard error in preparation for the joining procedure 
colnames(fever14_clu)[3]<- "standard_error" #2014 IR BF MIS 
colnames(fever_clu)[3]<- "standard_error" #2017 IR BF MIS 


#Merging DS-level estimates of the % U5 with fever with full list of health districts
dim(DS_merge)
DS_merge_f14<-DS_merge%>%left_join(fever_14) #2014 IR BF MIS 
DS_merge_f<-DS_merge%>%left_join(fever) #2017 IR BF MIS 


#Merging the # of U5 children in each cluster with the % of U5 children that had fever in each cluster
pts_est_f14<-f14_num_clu%>%left_join(fever14_clu)%>% #2014 IR BF MIS 
  rename(DHSCLUST = v001)

pts_est_f<-f_num_clu%>%left_join(fever_clu)%>% #2017 IR BF MIS 
  rename(DHSCLUST = v001)

#creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_f14,file="outputs/fever_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_f,file="outputs/fever_BF17DS.csv") #2017 IR BF MIS 


## U5 fever maps 


#merging the fever stats to the sf DS shape file
DS_shape_f14 <- DSshape_sf%>%left_join(DS_merge_f14) #2014 IR BF MIS
DS_shape_f <- DSshape_sf%>%left_join(DS_merge_f) #2017 IR BF MIS

#merging the full list of clusters with the cluster-level estimates and converting to sf object
pt_f14 <- pt_sf_14%>%left_join(pts_est_f14)%>%  #2014 IR BF MIS
                 rename(`Fever status by cluster`=fever)

pt_f <- pt_sf_17%>%left_join(pts_est_f)%>% #2017 IR BF MIS 
                rename(`Fever status by cluster`=fever)


#this creates a shapefile with cluster-level estimates of the proportion of children with fever 
st_write(pt_f14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Fever_2014/fever_2014_cluster.csv")
st_write(pt_f, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Fever_2017/fever_2017_cluster.csv")

#2014 IR BF MIS
fever_2014_map <- tm_shape(DS_shape_f14) + #this is the health district shapfile with LLIN info
  tm_polygons(col = "fever", textNA = "No data", 
              title = "Fever prevalence", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Fever status among U5 by Health Districts (2014)",
            main.title.position = c("center", "top"), aes.palette = list(seq = "-RdYlBu"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_f14)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Fever status by cluster", 
             border.col= "black", palette="seq",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_layout(aes.palette = list(seq = "-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#2017 IR BF MIS
fever_2017_map <- tm_shape(DS_shape_f) + #this is the health district shapfile with LLIN info
  tm_polygons(col = "fever", textNA = "No data", 
              title = "Fever prevalence", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                    0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Fever status among U5 by Health Districts (2017)",
            main.title.position = c("center", "top"), aes.palette = list(seq = "-RdYlBu"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_f)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Fever status by cluster", 
             border.col= "black", palette="seq",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_layout(aes.palette = list(seq = "-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")




#print map 
fever_2014_map
fever_2017_map


#saving map
tmap_save(tm = fever_2014_map, filename = "outputs/fever_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = fever_2017_map, filename = "outputs/fever_map_2017.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)


# U5 receipt of medical treatment for fever

## setting up the survey related variables and survey design object


#2014 IR BF MIS
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

#2017 IR BF MIS
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


## Proportion of U5 children with fever that received medical treatment by health district

#2014 IR BF MIS 
system.time({
  medical_14<-svyby(formula=~med_fever,# which variable do we want estimate
                    by=~NOMDEP, # by which variable
                    FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                    design=med14.svydesign, # my svy design object
                    na.rm=T)
})

head(medical_14)

#2017 IR BF MIS 
system.time({
  medical<-svyby(formula=~med_fever,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=med.svydesign, # my svy design object
                 na.rm=T)
})

head(medical)


## Number of U5 by cluster


#2014 IR BF MIS
system.time({
  m14_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                     by=~v001, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                     design=med14.svydesign, # my svy design object
                     na.rm=T)%>%
    mutate(`U5 in DHS clusters` = round(num_kids,0))
}) 

head(m14_num_clu)

#2017 IR BF MIS
system.time({
  m_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                   design=med.svydesign, # my svy design object
                   na.rm=T) %>%
    mutate(`U5 in DHS clusters` = round(num_kids,0))
}) 

head(m_num_clu)


## Proportion of U5 children that received medical treatment for fever by cluster


#2014 IR BF MIS 
system.time({
  med14_clu<-svyby(formula=~med_fever,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=med14.svydesign, # my svy design object
                   na.rm=T)
})

head(med14_clu)

#2017 IR BF MIS 
system.time({
  med_clu<-svyby(formula=~med_fever,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=med.svydesign, # my svy design object
                 na.rm=T)
})

head(med_clu)


## Dataset clean-up for merging and mapping medical treatment for fever


#renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(med14_clu)[3]<- "standard_error" #2014 IR BF MIS 
colnames(med_clu)[3]<- "standard_error" #2017 IR BF MIS 


#merging estimates of DS U5 medical treatment for fever with the full list of health districts

dim(DS_merge)
DS_merge_m14<-DS_merge%>%left_join(medical_14) #2014 IR BF MIS
DS_merge_m<-DS_merge%>%left_join(medical) #2017 IR BF MIS  


#merging the # of U5 children with the % of U5 children that received treatment for fever in each cluster

pts_est_m14<-m14_num_clu%>%left_join(med14_clu)%>%  #2014 IR BF MIS
  rename(DHSCLUST = v001)

pts_est_m<-m_num_clu%>%left_join(med_clu)%>% #2017 IR BF MIS  
  rename(DHSCLUST = v001)


#creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_m14,file="outputs/med_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_m,file="outputs/med_BF17DS.csv") #2017 IR BF MIS 


## Medical treatment for fever maps 


#merging the health district fever stats to the sf DS shape file
DS_shape_m14 <- DSshape_sf%>%left_join(DS_merge_m14) #2014 IR BF MIS
DS_shape_m <- DSshape_sf%>%left_join(DS_merge_m) #2017 IR BF MIS


#merging the full list of clusters to the cluster estimates and converting to sf objects
pt_m14 <- pt_sf_14%>%left_join(pts_est_m14)%>% #2014 IR BF MIS 
  rename(`Medical treatment for fever by cluster`=med_fever)
pt_m <- pt_sf_17%>%left_join(pts_est_m)%>% #2017 IR BF MIS
  rename(`Medical treatment for fever by cluster`= med_fever)

#this creates a shapefile that with cluster-level estimates of medical treatment for fever
st_write(pt_m14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2014/shape/medtreat_2014_cluster.csv")
st_write(pt_m, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2017/shape/medtreat_2017_cluster.csv")

#this is for 2014 IR BF MIS
med_2014_map <- tm_shape(DS_shape_m14) + #this is the health district shapfile with medical treatment info
  tm_polygons(col = "med_fever", textNA = "No data", 
              title = "Prevalence of medical treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                                0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="U5 treatment for fever by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_m14)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Medical treatment for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")

#this is for 2017 IR BF MIS
med_2017_map <- tm_shape(DS_shape_m) + #this is the health district shapfile with medical treatment info
  tm_polygons(col = "med_fever", textNA = "No data", 
              title = "Prevalence of medical treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                                0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="U5 treatment for fever by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_m)+ #this is the points shape file with fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "Medical treatment for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")




#print map 
med_2014_map
med_2017_map


#saving map
tmap_save(tm = med_2014_map, filename = "outputs/medfever_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)
tmap_save(tm = med_2017_map, filename = "outputs/medfever_map_2017.pdf",
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)


# U5 ACT taken for fever

## setting up the survey related variables and survey design object


#2014 IR BF MIS
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

#2017 IR BF MIS
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


## Proportion of U5 children that received act for fever by health district


#2014 IR BF MIS
system.time({
  act14<-svyby(formula=~act,# which variable do we want estimate
               by=~NOMDEP, # by which variable
               FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
               design=act14.svydesign, # my svy design object
               na.rm=T)
})

head(act14)

# 2017 IR BF MIS
system.time({
  act<-svyby(formula=~act,# which variable do we want estimate
             by=~NOMDEP, # by which variable
             FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
             design=act.svydesign, # my svy design object
             na.rm=T)
})

head(act)


## Number of U5 by cluster


# 2014 IR BF MIS 
system.time({
  act14_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                       by=~v001, # by which variable
                       FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                       design=act14.svydesign, # my svy design object
                       na.rm=T) %>% 
    mutate(`U5 in DHS clusters` = round(num_kids,0))
}) 

head(act14_num_clu)

#2017 IR BF MIS 
system.time({
  act_num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                     by=~v001, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                     design=act.svydesign, # my svy design object
                     na.rm=T)%>%
    mutate(`U5 in DHS clusters` = round(num_kids,0))
}) 

head(act_num_clu)


## Proportion of U5 children who received ACT for fever by cluster


#2014 IR BF MIS
system.time({
  act14_clu<-svyby(formula=~act,# which variable do we want estimate
                   by=~v001, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=act14.svydesign, # my svy design object
                   na.rm=T)
})

head(act14_clu)

#2017 IR BF MIS
system.time({
  act_clu<-svyby(formula=~act,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=act.svydesign, # my svy design object
                 na.rm=T)
})

head(act_clu)


## Dataset clean-up for merging and mapping U5 ACT for fever

#renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(act14_clu)[3]<- "standard_error" #2014 IR BF MIS
colnames(act_clu)[3]<- "standard_error" #2017 IR BF MIS


#merging the DS-level estimates  of the % of U5 that received ACT for fever with the full list of health districts
dim(DS_merge)
DS_merge_act14<-DS_merge%>%left_join(act14) #2014 IR BF MIS
DS_merge_act<-DS_merge%>%left_join(act) #2017 IR BF MIS


#merging the # of U5 children in each cluster with the % of U5 that had ACT for fever by cluster
pts_est_act14<-act14_num_clu%>%left_join(act14_clu)%>%  #2014 IR BF MIS
  rename(DHSCLUST = v001)   
pts_est_act<-act_num_clu%>%left_join(act_clu)%>% #2017 IR BF MIS
  rename(DHSCLUST = v001) 

####creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_act14,file="outputs/act_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_act,file="outputs/act_BF17DS.csv") #2017 IR BF MIS


## U5 ACT fever maps 


#merging the health district fever stats to the sf DS shape file 
DS_shape_act14 <- DSshape_sf%>%left_join(DS_merge_act14) #2014 IR BF MIS
DS_shape_act <- DSshape_sf%>%left_join(DS_merge_act) #2017 IR BF MIS


#convert the cluster-level estimates sf object
pt_act14 <- pt_sf_14%>%left_join(pts_est_act14)%>% #2014 IR BF MIS
              rename(`ACT for fever by cluster`=act)


pt_act <- pt_sf_17%>%left_join(pts_est_act)%>% #2017 IR BF MIS
                  rename(`ACT for fever by cluster`=act)

#this creates shapefiles with cluster-level estimates of the U5 that received ACT for fever 
st_write(pt_act, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/ACT_fever_2017/act_17.csv")
st_write(pt_act14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/ACT_fever_2014/act_14.csv")


#2014 IR BF MIS
ACT_2014_map <- tm_shape(DS_shape_act14) + #this is the health district shapfile with ACT info
  tm_polygons(col = "act", textNA = "No data", 
              title = "Prevalence of ACT treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                            0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="U5 ACT for fever by Health Districts (2014)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_act14)+ #this is the points shape file with ACT for fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "ACT for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")


#2017 IR BF MIS
ACT_2017_map <- tm_shape(DS_shape_act) + #this is the health district shapfile with ACT info
  tm_polygons(col = "act", textNA = "No data", 
              title = "Prevalence of ACT treatment for fever", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                            0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="U5 ACT for fever by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_act)+ #this is the points shape file with ACT for fever and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "ACT for fever by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")




#print map 
ACT_2014_map
ACT_2017_map


#saving map
tmap_save(tm = ACT_2014_map, filename = "outputs/ACTfever_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)
tmap_save(tm = ACT_2017_map, filename = "outputs/ACTfever_map_2017.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)


# Intermittent preventive treatment in pregnancy 

## setting up the survey related variables and survey design object


#sp/fansidar for IPTp 2014 IR BF MIS 
mis14_spf<-mis14_l[mis14_l$var == "s307a",]%>% mutate(wt=v005/1000000, strat=v022,
          id=v021, num_f=1)%>% #column for the number of women so that we can compute the totals by cluster  
  rename(sp_f = count)%>%   
  filter(!is.na(wt))%>% 
  filter(!is.na(sp_f))

spf.svydesign <- svydesign(id= ~id,
                           strata=~strat,nest=T, 
                           weights= ~wt, data=mis14_spf)


#sp/fansidar for IPTp 2017 IR BF MIS  
mis17_spf<-mis17_r%>% mutate(wt=v005/1000000, strat=v022,
          id=v021, num_f=1)%>% #column for the number of women so that we can compute the totals by cluster 
  rename(sp_f = m49a_1)%>%
  filter(!is.na(wt))%>% 
  filter(!is.na(sp_f))


spf17.svydesign <- svydesign(id= ~id,
                             strata=~strat,nest=T, 
                             weights= ~wt, data=mis17_spf)


## Proportion of women that received IPTp with sp/fanisdar for malaria by health district


#2014 IR BF MIS 
system.time({
  iptp<-svyby(formula=~sp_f,# which variable do we want estimate
              by=~NOMDEP, # by which variable
              FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
              design=spf.svydesign, # my svy design object
              na.rm=T)
})

head(iptp)

#2017 IR BF MIS 
system.time({
  iptp_17<-svyby(formula=~sp_f,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=spf17.svydesign, # my svy design object
                 na.rm=T)
})

head(iptp_17)


## Number of women that responded to the question on IPTp with sp/fansidar for fever by cluster


#2014 IR BF MIS 
system.time({
  iptp_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                      design=spf.svydesign, # my svy design object
                      na.rm=T) %>% 
    mutate(`Women in DHS clusters` = round(num_f,0))
}) 

head(iptp_num_clu)

#2017 IR BF MIS 
system.time({
  iptp17_num_clu<-svyby(formula=~num_f,# which variable do we want estimate
                        by=~v001, # by which variable
                        FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                        design=spf17.svydesign, # my svy design object
                        na.rm=T)%>% 
    mutate(`Women in DHS clusters` = round(num_f,0))
}) 

head(iptp17_num_clu)


## Proportion of women that received IPTp with sp/fansidar for malaria by cluster


#2014 IR BF MIS 
system.time({
  iptp_clu<-svyby(formula=~sp_f,# which variable do we want estimate
                  by=~v001, # by which variable
                  FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                  design=spf.svydesign, # my svy design object
                  na.rm=T)
})

head(iptp_clu)

#2017 IR BF MIS 
system.time({
  iptp17_clu<-svyby(formula=~sp_f,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                    design=spf17.svydesign, # my svy design object
                    na.rm=T)
})

head(iptp17_clu)

## Dataset clean-up for merging and mapping IPTP 


#renaming 'se' colname in bednet_clu to standard error in preparation for the joining procedure 
colnames(iptp_clu)[3]<- "standard_error" #2014 IR BF MIS
colnames(iptp17_clu)[3]<- "standard_error" #2017 IR BF MIS


#merging the list of all DS with DS level estimates of the proportion of women receiving IPTP
dim(DS_merge)
DS_merge_iptp17<-DS_merge%>%left_join(iptp_17) #2017 IR BF MIS
DS_merge_iptp<-DS_merge%>%left_join(iptp) #2014 IR BF MIS

#merging the # of women in each cluster with the # of women that had IPTp by cluster
pts_est_iptp<-iptp_num_clu%>%left_join(iptp_clu)%>% #2014 IR BF MIS
  rename(DHSCLUST = v001)
pts_est_iptp17<-iptp17_num_clu%>%left_join(iptp17_clu)%>% #2017 IR BF MIS
  rename(DHSCLUST = v001)


#creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_iptp,file="outputs/IPTp_spf_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_iptp17,file="outputs/IPTp_spf_BF17DS.csv") #2017 IR BF MIS


## IPTP maps 


#merging the health district fever stats to the sf DS shape file 
DS_shape_iptp <- DSshape_sf%>%left_join(DS_merge_iptp) #2014 IR BF MIS 
DS_shape_iptp17 <- DSshape_sf%>%left_join(DS_merge_iptp17) #2017 IR BF MIS 


#merging the points file to the points cluster values 
pt_iptp <- pt_sf_14%>%left_join(pts_est_iptp)%>% #2014 IR BF MIS 
               rename(`IPTp with sp/fansidar by cluster`=sp_f)

pt_iptp17 <- pt_sf_17%>%left_join(pts_est_iptp17)%>% #2017 IR BF MIS 
               rename(`IPTp with sp/fansidar by cluster`=sp_f)


#writing the cluster-level estimates to a shapefile 
st_write(pt_iptp17,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IPTP_spf_2017/iptp17.csv")
st_write(pt_iptp,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IPTP_spf_2014/iptp14.csv")

#2014 IR BF MIS 
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


#2017 IR BF MIS 
IPTP_2017_map <- tm_shape(DS_shape_iptp17) + #this is the health district shapfile with IPTp with sp/fansidar
  tm_polygons(col = "sp_f", textNA = "No data", 
              title = "IPTP with sp/fansidar prevalence", palette = "RdYlBu", breaks=c(0, 0.2, 0.3, 
                                                                                       0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="IPTp with sp/fansidar by Health Districts (2017)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_iptp17)+ #this is the points shape file with IPTp with sp/fansidar and number of women info by cluster 
  tm_bubbles(size="Women in DHS clusters", col = "IPTp with sp/fansidar by cluster", 
             border.col= "black", palette="RdYlBu",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_legend(legend.title.size = 0.8, legend.just="top")




#print map
IPTP_2014_map
IPTP_2017_map


#saving map
tmap_save(tm = IPTP_2014_map, filename = "outputs/IPTP_spf_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)
tmap_save(tm = IPTP_2017_map, filename = "outputs/IPTP_spf_map_2017.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)



#U5 parasite prevalence (microscopy) 

## setting up the survey related variables and survey design object for U5 microscopy positive proportion

#2010 PR BF DHS 
dhs10_hml32 <- prdhs10 %>%mutate(wt=hv005/1000000,strat=hv022,
            id=hv021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  rename(p_test = hml32)%>%
  filter(!is.na(wt))%>% 
  filter(!is.na(hc1)) #we want to restrict to children 


hml32.svydesign <- svydesign(id= ~id,
                             strata=~strat,nest=T, 
                             weights= ~wt, data=dhs10_hml32)



#2014 PR BF MIS 
mis14_hml32 <- prmis14 %>%mutate(wt=hv005/1000000,strat=hv022,
            id=hv021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
  rename(p_test = hml32)%>%
  filter(!is.na(wt))%>% 
  filter(!is.na(hc1)) #we want to restrict to children 


hml32.svyd14 <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis14_hml32)

## Proportion of U5 +ve test results (PfPr) by health district 

#2010 PR BF DHS 
system.time({
  ptest_10<-svyby(formula=~p_test,# which variable do we want to estimate
                      by=~NOMDEP, # by which variable
                      FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                      design=hml32.svydesign, # my svy design object
                      na.rm=T)
})

head(ptest_10)

#2014 PR BF MIS 
system.time({
  ptest_14<-svyby(formula=~p_test,# which variable do we want estimate
                      by=~NOMDEP, # by which variable
                      FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                      design=hml32.svyd14, # my svy design object
                      na.rm=T)
}) 

head(ptest_14)


## Proportion of U5 +ve test results (PfPr) by health district and timepoint (year, month and day)

#2010 PR BF DHS 
system.time({
  ptest_10_ymd<-svyby(formula=~p_test,# which variable do we want to estimate
                   by=~NOMDEP + timepoint, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=hml32.svydesign, # my svy design object
                   na.rm=T)%>% 
                arrange(NOMDEP, timepoint)
})

head(ptest_10_ymd)

#2014 PR BF MIS 
system.time({
  ptest_14_ymd<-svyby(formula=~p_test,# which variable do we want estimate
                   by=~NOMDEP + timepoint, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=hml32.svyd14, # my svy design object
                   na.rm=T)%>%
                  arrange(NOMDEP, timepoint)
}) 

head(ptest_14_ymd)


## Number of U5 children within each DS and by timepoint (year, month and day)

#2010 PR BF DHS 
system.time({
  ptest_num_10_ymd<-svyby(formula=~num_kids,# which variable do we want estimate
                   by=~NOMDEP + timepoint, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                   design=hml32.svydesign, # my svy design object
                   na.rm=T)%>% 
                 dplyr:: select(-se)%>%
                arrange(NOMDEP, timepoint)%>%
                 mutate(num_kids = round(num_kids, 0))
  
}) 

head(ptest_num_10_ymd)

#2014 PR BF MIS 
system.time({
  ptest_num_14_ymd<-svyby(formula=~num_kids,# which variable do we want estimate
                   by=~NOMDEP + timepoint, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                   design=hml32.svyd14, # my svy design object
                   na.rm=T)%>%
                  dplyr::select(-se)%>%
                   arrange(NOMDEP, timepoint)%>%
                   mutate(num_kids = round(num_kids, 0))
}) 

head(ptest_num_14_ymd)


## merging the number of kids by DS and timepoint (ymd) to the estimates of PfPr by DS and timepoint (ymd)

#2010 PR BF DHS
DS_merge_ymd<-ptest_10_ymd%>%left_join(ptest_num_10_ymd)%>%
  rename(DS = NOMDEP, `timepoint(mm/dd/yyyy)` = timepoint, PfPr = p_test,
         `Number of Kids` = num_kids)

head(DS_merge_ymd)

#2014 PR BF MIS
DS_merge_ymd_14<-ptest_14_ymd%>%left_join(ptest_num_14_ymd)  %>% 
                  rename(DS = NOMDEP, `timepoint(mm/dd/yyyy)` = timepoint, PfPr = p_test,
                                  `Number of Kids` = num_kids)

head(DS_merge_ymd_14)

#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) 
write.csv(DS_merge_ymd, file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/prevalence/pos_test_time_BF10DS.csv") #2010 PR BF DHS 
write.csv(DS_merge_ymd_14,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/prevalence/pos_test_time_BF14DS.csv")#2014 PR BF MIS



## Proportion of U5 +ve test results (PfPr) by health district and timepoint (year and month)

#2010 PR BF DHS 
system.time({
  ptest_10_ym<-svyby(formula=~p_test,# which variable do we want to estimate
                     by=~NOMDEP + time2, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=hml32.svydesign, # my svy design object
                     na.rm=T)%>% 
                      arrange(NOMDEP, time2)
})

head(ptest_10_ym)

#2014 PR BF MIS 
system.time({
  ptest_14_ym<-svyby(formula=~p_test,# which variable do we want estimate
                     by=~NOMDEP + time2, # by which variable
                     FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                     design=hml32.svyd14, # my svy design object
                     na.rm=T)%>%
    arrange(NOMDEP, time2)
}) 

head(ptest_14_ym)


#creates csv file of parasitemia prevalence by year and month 
write.csv(ptest_10_ym,file='outputs/parasitemia_10_ym.csv') #2010 PR BF DHS 
write.csv(ptest_14_ym,file='outputs/parasitemia_14_ym.csv') # #2014 PR BF MIS 


## Proportion of U5 children with +ve test results by cluster 

#2010 PR BF DHS 
system.time({
  ptest_clu_10<-svyby(formula=~p_test,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                         design=hml32.svydesign, # my svy design object
                         na.rm=T)%>% 
                          rename(DHSCLUST = v001, `positive malaria test results by cluster` = p_test)
}) 

head(ptest_clu_10)


#2014 PR BF MIS 
system.time({
  ptest_clu_14<-svyby(formula=~p_test,# which variable do we want estimate
                         by=~v001, # by which variable
                         FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                         design=hml32.svyd14, # my svy design object
                         na.rm=T)%>% 
                         rename(DHSCLUST = v001, `positive malaria test results by cluster` = p_test)
  
}) 

head(ptest_clu_14)


## Proportion of U5 children with +ve test results by cluster + timepoint (year and month)

#2010 PR BF DHS 
system.time({
  ptest_clu_10_ym<-svyby(formula=~p_test,# which variable do we want estimate
                      by=~v001 + time2, # by which variable
                      FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                      design=hml32.svydesign, # my svy design object
                      na.rm=T)%>% 
                    arrange(v001, time2)%>%
                      rename(DHSCLUST = v001, `positive malaria test results by cluster` = p_test)
                      
  
}) 

head(ptest_clu_10_ym)


#2014 PR BF MIS 
system.time({
  ptest_clu_14_ym<-svyby(formula=~p_test,# which variable do we want estimate
                      by=~v001 + time2, # by which variable
                      FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                      design=hml32.svyd14, # my svy design object
                      na.rm=T)%>% 
                        arrange(v001, time2)%>%
                          rename(DHSCLUST = v001, `positive malaria test results by cluster` = p_test)
  
}) 

head(ptest_clu_14_ym)


## Number of U5 children within each cluster

#2010 PR BF DHS 
system.time({
  numclu_10<-svyby(formula=~num_kids,# which variable do we want estimate
                      by=~v001, # by which variable
                      FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                      design=hml32.svydesign, # my svy design object
                      na.rm=T)%>%
    mutate(num_kids = round(num_kids, 0))%>% 
    rename(standard_error = se,  DHSCLUST = v001, `U5 in DHS clusters` = num_kids)
}) 

head(numclu_10)

#2014 PR BF MIS
system.time({
  numclut_14<-svyby(formula=~num_kids,# which variable do we want estimate
                       by=~v001 + time2, # by which variable
                       FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                       design=hml32.svyd14, # my svy design object
                       na.rm=T)%>%
    mutate(num_kids = round(num_kids, 0))%>% 
    rename(standard_error = se, DHSCLUST = v001, `U5 in DHS clusters` = num_kids)
}) 

head(numclut_14)



## Number of U5 children within each cluster and by timepoint (year and month)

#2010 PR BF DHS 
system.time({
  numclu_10_ym<-svyby(formula=~num_kids,# which variable do we want estimate
                     by=~v001 + time2, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                     design=hml32.svydesign, # my svy design object
                     na.rm=T)%>%
                      arrange(v001, time2)%>%
                     mutate(num_kids = round(num_kids, 0))%>% 
                    rename(standard_error = se,  DHSCLUST = v001, `U5 in DHS clusters` = num_kids)
}) 

head(numclu_10_ym)

#2014 PR BF MIS
system.time({
  numclut_14_ym<-svyby(formula=~num_kids,# which variable do we want estimate
                     by=~v001 + time2, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                     design=hml32.svyd14, # my svy design object
                     na.rm=T)%>%
                     arrange(v001, time2)%>%
                     mutate(num_kids = round(num_kids, 0))%>% 
                     rename(standard_error = se, DHSCLUST = v001, `U5 in DHS clusters` = num_kids)
}) 

head(numclut_14_ym)

#joining the pfpr by cluster dataset to that of number of children by cluster to create new table
ptest_clur_10<-ptest_clu_10%>%left_join(numclu_10) #2010 PR BF DHS 
ptest_clur_14<-ptest_clu_14%>%left_join(numclut_14) #2014 PR BF MIS


#joining the pfpr by cluster + month dataset to that of number of children by cluster + month to create new table
ptest_clu_all_10<-ptest_clu_10_ym%>%left_join(numclu_10_ym) #2010 PR BF DHS 
ptest_clu_all_14<-ptest_clu_14_ym%>%left_join(numclut_14_ym) #2014 PR BF MIS

head(ptest_clu_all_10)
head(ptest_clu_all_14)

#creates csv file of PfPr by year and month of survey with the number of U5 surveyed                                 
write.csv(ptest_clu_all_10,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_timepoint/pos_test_time_BF10CL.csv")
write.csv(ptest_clu_all_14,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_timepoint/pos_test_time_BF14CL.csv")


## Parasitemia maps 

#merging the DS shape file to the DS level parasitemia estimates 
DS_shape_10_ptest <- DSshape_sf%>%left_join(ptest_10) #2010 PR BF DHS 
DS_shape_14_ptest <- DSshape_sf%>%left_join(ptest_14)  #2014 PR BF MIS

#merging the cluster shape file to the cluster level parasitemia estimates
clu_shape_10_r <- pt_sf_10%>%left_join(ptest_clur_10)#2010 PR BF DHS 
clu_shape_14_r <- pt_sf_14%>%left_join(ptest_clur_14) #2014 PR BF MIS

#merging the cluster shape file to the cluster level parasitemia estimates by timepoint (year and month)
clu_shape_10_ym <- pt_sf_10%>%inner_join(ptest_clu_all_10)#2010 PR BF DHS 
clu_shape_14_ym <- pt_sf_10%>%inner_join(ptest_clu_all_14)#2014 PR BF MIS


#2010 PR BF DHS 
ptest_2010_map <- tm_shape(DS_shape_10_ptest) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                     0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="U5 malaria parasitemia by Health Districts (2010)",
            main.title.position = c("center", "top"), aes.palette = list(seq = "-RdYlBu"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(clu_shape_10_r)+ #this is the points shape file with test result and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "positive malaria test results by cluster", 
             border.col= "black", palette="seq",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_layout(aes.palette = list(seq = "-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")
  



#2014 PR BF DHS 
ptest_2014_map <- tm_shape(DS_shape_14_ptest) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                     0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="U5 malaria parasitemia by Health Districts (2014)",
            main.title.position = c("center", "top"), aes.palette = list(seq = "-RdYlBu"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(clu_shape_14_r)+ #this is the points shape file with test result and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "positive malaria test results by cluster", 
             border.col= "black", palette="seq",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_layout(aes.palette = list(seq = "-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")



#print map
ptest_2010_map
ptest_2014_map


#saving map
tmap_save(tm = ptest_2010_map, filename = "outputs/ptest_map_2010.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = ptest_2014_map, filename = "outputs/ptest_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)


## Subsetting the DS level parasitemia estimates by month of survey for 2010 PR BF DHS

month_ptest_10 <- split(ptest_10_ym, ptest_10_ym$time2)

for (I in 1:length(month_ptest_10)) { assign(unique(month_ptest_10[[I]]$time2), month_ptest_10[[I]]) }

## merging with the full list of health districts for the 2010 PR BF DHS 
DS_shape_10_jan<- DSshape_sf%>%left_join(`1-2010`) 
DS_shape_10_apr<- DSshape_sf%>%left_join(`4-2010`)
DS_shape_10_may<- DSshape_sf%>%left_join(`5-2010`)
DS_shape_10_jun<- DSshape_sf%>%left_join(`6-2010`)
DS_shape_10_jul<- DSshape_sf%>%left_join(`7-2010`)
DS_shape_10_aug<- DSshape_sf%>%left_join(`8-2010`)
DS_shape_10_sep<- DSshape_sf%>%left_join(`9-2010`)
DS_shape_10_oct<- DSshape_sf%>%left_join(`10-2010`)  
DS_shape_10_nov<- DSshape_sf%>%left_join(`11-2010`)
DS_shape_10_dec<- DSshape_sf%>%left_join(`12-2010`) 

## Plotting the 2010 PR BF DHS PfPr by month of survey 
#January 
ptest_2010_jan_map <- tm_shape(DS_shape_10_jan) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                             0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(aes.palette = list(seq = "-RdYlBu"), title="January 2010")


#April
ptest_2010_apr_map <- tm_shape(DS_shape_10_apr) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="April 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)
  

#May
ptest_2010_may_map <- tm_shape(DS_shape_10_may) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="May 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)


#June
ptest_2010_jun_map <- tm_shape(DS_shape_10_jun) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="June 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)
  
#July
ptest_2010_jul_map <- tm_shape(DS_shape_10_jul) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="July 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)
  

#Aug
ptest_2010_aug_map <- tm_shape(DS_shape_10_aug) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="Aug 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)
 

#Sep
ptest_2010_sep_map <- tm_shape(DS_shape_10_sep) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="Sep 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)


#October
ptest_2010_oct_map <- tm_shape(DS_shape_10_oct) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="October 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)



#Nov
ptest_2010_nov_map <- tm_shape(DS_shape_10_nov) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="November 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)


#Dec
ptest_2010_dec_map <- tm_shape(DS_shape_10_dec) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="December 2010", aes.palette = list(seq = "-RdYlBu"), legend.show=F)


ptest_month_map_10<- tmap_arrange(ptest_2010_jan_map, ptest_2010_apr_map, ptest_2010_may_map, ptest_2010_jun_map,
             ptest_2010_jul_map, ptest_2010_aug_map, ptest_2010_sep_map, ptest_2010_oct_map, ptest_2010_nov_map, 
             ptest_2010_dec_map)



## Subsetting the DS level parasitemia estimates by month of survey for 2014 PR BF DHS

month_ptest_14 <- split(ptest_14_ym, ptest_14_ym$time2)

for (I in 1:length(month_ptest_14)) { assign(unique(month_ptest_14[[I]]$time2), month_ptest_14[[I]]) }


## merging with the full list of health districts for the 2014 PR BF MIS 
DS_shape_14_sep<- DSshape_sf%>%left_join(`9-2014`)
DS_shape_14_oct<- DSshape_sf%>%left_join(`10-2014`)  
DS_shape_14_nov<- DSshape_sf%>%left_join(`11-2014`)
DS_shape_14_dec<- DSshape_sf%>%left_join(`12-2014`) 

## Plotting the 2010 PR BF DHS PfPr by month of survey 
#Sep
ptest_2014_sep_map <- tm_shape(DS_shape_14_sep) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="Sep 2014", aes.palette = list(seq = "-RdYlBu"))


#October
ptest_2014_oct_map <- tm_shape(DS_shape_14_oct) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="October 2014", aes.palette = list(seq = "-RdYlBu"), legend.show=F)



#Nov
ptest_2014_nov_map <- tm_shape(DS_shape_14_nov) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="November 2014", aes.palette = list(seq = "-RdYlBu"), legend.show=F)


#Dec
ptest_2014_dec_map <- tm_shape(DS_shape_14_dec) + #this is the health district shapefile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "U5 Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                                        0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(title="December 2014", aes.palette = list(seq = "-RdYlBu"), legend.show=F)


ptest_month_14_map<- tmap_arrange(ptest_2014_sep_map, ptest_2014_oct_map, ptest_2010_nov_map, 
                               ptest_2010_dec_map)

#print map 
ptest_month_10_map
ptest_month_14_map

#saving map
tmap_save(tm = ptest_month_10_map, filename = "outputs/ptest_month_map_2010.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = ptest_month_14_map, filename = "outputs/ptest_month_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)


# Intermittent Residual Spraying (IRS)

## Setting up the survey related variables and survey design object for IRS

#2010 HR BF DHS 
dhs10_hv253 <- hrdhs10 %>%mutate(wt=hv005/1000000,strat=hv022,
                                 id=hv021, num_HH=1)%>% #column for # of HH to compute the totals by DS and cluster 
  rename(IRS = hv253)%>%
  filter(!is.na(wt))


hv253.svydesign <- svydesign(id= ~id,
                             strata=~strat,nest=T, 
                             weights= ~wt, data=dhs10_hv253)


#2014 HR BF MIS 
mis14_hv253 <- hrmis14 %>%mutate(wt=hv005/1000000,strat=hv022,
                                 id=hv021, num_HH=1)%>% #column for # of HH to compute the totals by DS and cluster
  rename(IRS = hv253)%>%
  filter(!is.na(wt))



hv253.svyd14 <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=mis14_hv253)

## Proportion of households that got IRS by health district 

#2010 HR BF DHS 
system.time({
  IRS <-svyby(formula=~IRS,# which variable do we want estimate
              by=~NOMDEP, # by which variable
              FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
              design=hv253.svydesign, # my svy design object
              na.rm=T)
})

head(IRS)

#2014 HR BF MIS 
system.time({
  IRS_14<-svyby(formula=~IRS,# which variable do we want estimate
                by=~NOMDEP, # by which variable
                FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                design=hv253.svyd14, # my svy design object
                na.rm=T)
}) 

head(IRS_14)


## Number of HH within each health district
#2010 HR BF DHS 
system.time({
  num_DS_10<-svyby(formula=~num_HH,# which variable do we want estimate
                   by=~NOMDEP, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                   design=hv253.svydesign, # my svy design object
                   na.rm=T) %>% 
                  rename(standard_error = se) %>% 
                  mutate(num_HH = round(num_HH, 0))
                  
})

head(num_DS_10)

#2014 HR BF MIS
####This computes the total number of HH within each surveyed cluster for 2014 BF MIS 
system.time({
  num_DS_14<-svyby(formula=~num_HH,# which variable do we want estimate
                   by=~NOMDEP, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                   design=hv253.svyd14, # my svy design object
                   na.rm=T) %>% 
                 rename(standard_error = se) %>% 
                  mutate(num_HH = round(num_HH, 0))
}) 

head(num_DS_14)


## Number of HH within each cluster 
system.time({
  num_clu_10<-svyby(formula=~num_HH,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                    design=hv253.svydesign, # my svy design object
                    na.rm=T) %>% 
                    mutate(num_HH = round(num_HH, 0))
}) 

head(num_clu_10)

system.time({
  num_clu_14<-svyby(formula=~num_HH,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                    design=hv253.svyd14, # my svy design object
                    na.rm=T) %>% 
                    mutate(num_HH = round(num_HH, 0))
})

head(num_clu_14)


## Proportion of HH within each cluster
system.time({
  IRS_clu_10<-svyby(formula=~IRS,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                    design=hv253.svydesign, # my svy design object
                    na.rm=T)
}) 

head(IRS_clu_10)

system.time({
  IRS_clu_14<-svyby(formula=~IRS,# which variable do we want estimate
                    by=~v001, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance)
                    design=hv253.svyd14, # my svy design object
                    na.rm=T)
}) 

head(IRS_clu_14)


#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
#renaming 'se' colname in IRS_clu_10 to standard error in preparation for the joining procedure 
colnames(IRS_clu_10)[3]<- "standard_error" #for 2010 dataset 

#renaming 'se' colname in IRS_clu_14 to standard error in preparation for the joining procedure 
colnames(IRS_clu_14)[3]<- "standard_error" #for 2014 dataset 


#we want to merge with the full list of health districts

DS_IRS_10<-DSshape_sf%>%left_join(IRS)%>%left_join(num_DS_10) %>% #2010 BF DHS
                 rename(`Number of HH` = num_HH)                
DS_IRS_14 <- DSshape_sf%>%left_join(IRS_14)%>%left_join(num_DS_14) %>%  #next the 2014 BF MIS
                 rename(`Number of HH` = num_HH) 


#next we merge the number of HH within each surveyed cluster  
#with the proportion of HH that recieved IRS by surveyed cluster
#this is for 2010 
pts_est_IRS<- pt_sf_10 %>% left_join(IRS_clu_10)%>%left_join(num_clu_10) %>% 
                rename(`HH in DHS clusters` = num_HH)


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


#st_write(pt_10, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IRS_2010/IRS_2010_cluster.shp")
#st_write(pt_14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IRS_2014/IRS_2014_cluster.shp")

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


