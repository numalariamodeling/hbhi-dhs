
#############################################################################################
#### Scripts for exploration and descriptive analysis of malaria-related variables in the DHS/MIS Indivdual, household and person recode files in Burkina Faso
#### These scripts generates 20 maps and survey-adjusted descriptive statistics for the following malaria-related variables 
#### 1. U5 LLIN by health district for 2014 and 2017 
#### 2. Adult women's LLIN access by health district for 2014 and 2017 
#### 3. Adult women's LLIN use by health district for 2014 and 2017 
#### 4. U5 fever status by health district for 2014 and 2017 
#### 5. U5 receipt of medical treatment for fever by health district for 2014 and 2017
#### 6. U5 receipt of Arthemsinin Combination Therapy (ACT) for fever by health district for 2014 and 2017 
#### 7. Intermittent preventive treatment for pregnancy by health district for 2014 and 2017 
#### 8. U5 malaria parasitemia by health district for 2010 and 2014 
#### 9. Intermittent Residual Spraying of insecticides by health districts for 2010 and 2014 
#### 
#### Last Updated: October 8, 2019
####
#### Author:Ifeoma Ozodiegwu
##### 
##############################################################################################################



#############################################################################################################
## cleaning the global environment, setting the working environment, loading libraries and custoom functions 
#############################################################################################################

rm(list=ls())
#important to download the github version of tidyverse.Uncomment the script below to run
# install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("hadley/tidyverse")


# Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
      "lubridate", "RColorBrewer", "plotrix", "ggrepel", "sf", "shinyjs", "tmap", "knitr", "purrr")
lapply(x, library, character.only = TRUE) #applying the library function to packages
options(survey.lonely.psu="certainty") # this option allows admin units with only one cluster to be analyzed

#reads in custom functions 
source("BF DHS functions.R")




##############################################################################################################
## reading and cleaning the datasets 
##############################################################################################################


# Reading in all BF indvidual, household and person recode files 

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


# Renaming files 

# individual recode 
bf10dhs <- `BF_2010_DHS_06192019/BFIR62DT/BFIR62FL` 
bf14mis <- `BF_2014_MIS_06192019/BFIR71DT/BFIR71FL`
bf17mis <- `BF_2017-18_MIS_07252019_1531_86355/BFIR7ADT/BFIR7AFL`

# person recode 
bfprdhs10 <- `BF_2010_DHS_06192019/BFPR62DT/BFPR62FL`
bfprmis14 <- `BF_2014_MIS_06192019/BFPR71DT/BFPR71FL`
bfprmis18 <- `BF_2017-18_MIS_07252019_1531_86355/BFPR7ADT/BFPR7AFL`

# household recode 
bfhrdhs10 <- `BF_2010_DHS_06192019/BFHR62DT/BFHR62FL`
bfhrmis14 <- `BF_2014_MIS_06192019/BFHR71DT/BFHR71FL`
bfhrmis18 <- `BF_2017-18_MIS_07252019_1531_86355/BFHR7ADT/BFHR7AFL`





#################################################################################################################
## Reading in the shapefiles of GPS points and district boundaries for purposes of mapping and estimation 
#################################################################################################################

# GPS points
pts_10 <-readOGR("BF_2010_DHS_06192019/BFGE61FL", layer = "BFGE61FL")
pts_14 <- readOGR("BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL")
pts<- readOGR("BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL", layer = "BFGE7AFL")



# DS file
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

# converting the sp objects to sf object for easy plotting with tmap later 
pt_sf_10 <-st_as_sf(pts_10)
pt_sf_14 <- st_as_sf(pts_14)
pt_sf_17 <-st_as_sf(pts)
DSshape_sf<-st_as_sf(DS_shape)





############################################################################
## Quality checks and creating new files 
############################################################################

# DS is in the utm + zone 30 & points is in long + lat. lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,crs(pts)); 

# check map projections  
crs(DS_shape_W)
crs(pts)

# Check if the GPS points  are contained in the polygons 
raw.plot.fun(DS_shape_W, pts_10, 'Administrative boundary: Health Districts with DHS 2010 clusters')
raw.plot.fun(DS_shape_W, pts_14, 'Administrative boundary: Health Districts with MIS 2014 clusters')
raw.plot.fun(DS_shape_W, pts, 'Administrative boundary: Health Districts with DHS 2017 clusters')

#set up an empty dataframe with the DS names so DS with no estimates can be identified 
DS_merge<-data.frame(NOMDEP=DS_shape_W@data[,"NOMDEP"])





####################################################################################################
## Mapping points to inherit associated health district
###################################################################################################

# the dimensions of the pts data is 573 by 20 and of the DS is 70 by 10, key_10 will be 573 by 10 
# dhs 10
# dim(pts_10@data) #use the descriptive functions to check data below 
# dim(DS_shape_W@data)
key_10<-over(SpatialPoints(coordinates(pts_10),proj4string = pts_10@proj4string), DS_shape_W)
# dim(key_10)
# length(unique(key_10$NOMDEP))

# add in the cluster variable
key_10$v001<-pts_10@data[,"DHSCLUST"]

# the dimensions of the pts data is 252 by 20 and of the DS is 70 by 10, key_14 will be 252 by 10
#mis 14 
key_14<-over(SpatialPoints(coordinates(pts_14),proj4string = pts_14@proj4string), DS_shape_W)

# add in the cluster variable
key_14$v001<-pts_14@data[,"DHSCLUST"]

# dim pts@data is 245 by 20 and district is 70 by 10, then the corresponding data frame will be 245 by 10
#mis 17 
key<-over(SpatialPoints(coordinates(pts),proj4string = pts@proj4string), DS_shape_W)

# add in the cluster variable
key$v001<-pts@data[,"DHSCLUST"]





#########################################################################################################
## Creating and recoding variables 
#########################################################################################################

#applying function to create month and year of survey. Note this function only works for person recode files 

bfprdhs10 <- survey.month.fun(bfprdhs10)
bfprmis14 <- survey.month.fun(bfprmis14)
bfprmis18 <- survey.month.fun(bfprmis18)


#########################################################################
# recoding variables with custom functions 
#########################################################################


########################################################################
## IR datasets
########################################################################

# 2010 IR dataset 
bf10_dhsr<-apply.recoder(bf10dhs,'ml0') #recodes LLIN variables 


# 2014 IR dataset, h22 is fever variable 
mis14_r<-bf14mis%>%apply.recoder('ml0')%>%apply.recoder('h22')%>%
          apply.recoder('m49a_1')%>%apply.recoder('ml101') 


# 2017 IR dataset, m49a_1 is IPTp variable, ml101 is LLIN use variable for adult women 
mis17_r <- bf17mis%>%apply.recoder('ml0')%>%apply.recoder('h22')%>%
  apply.recoder('m49a_1')%>% apply.recoder('ml101') 
 

########################################################################
## HR datasets
########################################################################

# 2010 HR dataset 
hrdhs10<-apply.recoder(bfhrdhs10,'hv253')


# 2014 HR dataset 
hrmis14<-apply.recoder(bfhrmis14,'hv253')


########################################################################
## PR datasets
########################################################################

# 2010 PR dataset 
prdhs10<-apply.recoder(bfprdhs10,'hml35')



# 2014 PR dataset, hml32 is smear test results, hml35 is rapid test results 
prmis14<- bfprmis14%>%apply.recoder('hml32')%>%apply.recoder('hml35')

# 2017 PR dataset, sb215 is the smear test results
prmis18 <-apply.recoder2(bfprmis18,'sb215')





#######################################################################################
## Joining the DHS/MIS data to the key that contains the names of each health district
#######################################################################################



####################################################################################
##  IR datasets 
####################################################################################

# IR dhs 2010  
# dim(bf10_dhsr)# how many rows? #use these descriptive stats for checks 
# dim(key_10) #how many columns?
# length(unique(bf10_dhsr$v001)) # the lengths for the datasets and the key must match 
# length(unique(key_10$v001)) #
bf10dhs_work<-bf10_dhsr%>%left_join(key_10)

# should still have the same number of rows and a few new columns
dim(bf10dhs_work) #dataset to be used for subsequent 2010 computations that don't require long format  

# IR mis 2014 
mis14_r<-mis14_r%>%left_join(key_14)
dim(mis14_r) #dataset that will be used for subsequent 2014 computations that don't require long format  

# IR mis 2017 
mis17_r<-mis17_r%>%left_join(key)
dim(mis17_r) #dataset that will be used for subsequent 2017 computations that don't require long format


####################################################################################
##  HR datasets 
####################################################################################
#HR dhs 2010 
hrdhs10<-hrdhs10%>%rename(v001 = hv001)
hrdhs10<-hrdhs10%>%left_join(key_10)
dim(hrdhs10) #this is the dataset that will be used for subsequent 2010 computations 

# HR mis 2014
hrmis14<-hrmis14%>%rename(v001 = hv001)
hrmis14<-hrmis14%>%left_join(key_14)
dim(hrmis14) #this is the dataset that will be used for subsequent 2014 computations


####################################################################################
##  PR datasets 
####################################################################################

#finally the key datasets and dhs/mis datasets are joined for the PR datasets 
#dhs PR 2010 
prdhs10<-prdhs10%>%left_join(key_10)
dim(prdhs10) #this is the dataset to be used for subsequent 2010 computations that don't require long format 


#mis PR 2014 
prmis14<-prmis14%>%left_join(key_14)
dim(prmis14) #this is the dataset to be used for subsequent 2014 computations that don't require long format 

#checking the number of NAs and numbers in the age of child in months variable 
summary(is.na(prmis14$hc1))


#mis PR 2017
prmis18<-prmis18%>%left_join(key)
dim(prmis18) #this is the dataset to be used for subsequent 2014 computations that don't require long format 

#checking the number of NAs and numbers in the age of child in months variable 
summary(is.na(prmis18$hc1))





###############################################################################################
## Changing the IR dataset to long format to aggregate observations across children 
###############################################################################################

# U5 dhs 2010
bf10dhs_l <- short_to_long.fun(bf10dhs_work)


# U5 for mis 2014
mis14_l <- short_to_long.fun(mis14_r)


# U5 for mis 2017 
mis17_l <- short_to_long.fun(mis17_r)







################################################################################
## Analyzing the data
###############################################################################



###########################################################################
# U5 LLIN use
##########################################################################

# Setting up the survey-related variables and survey design object 

# 2010 IR BF DHS analysis dataset for U5 LLIN and design variables  
bf10dhs_ml0<-create.data(bf10dhs_l, 'ml0_1', 'ml0_2', 'ml0_3', 'ml0_4', 'count', 'LLIN_use')
svydesign_10 <-svydesign.fun(bf10dhs_ml0)


# 2014 IR BF MIS analysis dataset for U5 LLIN and design variables 
mis14_ml0<-create.data(mis14_l, 'ml0_1', 'ml0_2', 'ml0_3', 'ml0_4', 'count', 'LLIN_use' )
svydesign_14 <-svydesign.fun(mis14_ml0)

# 2017 IR BF MIS analysis dataset for U5 LLIN and design variables 
mis17_ml0<-create.data(mis17_l, 'ml0_1', 'ml0_2', 'ml0_3', 'ml0_4', 'count', 'LLIN_use')
my.svydesign <-svydesign.fun(mis17_ml0)


############################################################################################
# Proportion of U5 children who slept under a treated bednet by health district
############################################################################################
# 2010 IR BF DHS 
Bednet_10 <- svyby.fun('LLIN_use', 'NOMDEP', design = svydesign_10, svymean)
head(Bednet_10)

# 2014 IR BF MIS 
Bednet_14 <- svyby.fun('LLIN_use', 'NOMDEP', design = svydesign_14, svymean)
head(Bednet_14)

# 2017 IR BF MIS
Bednet<- svyby.fun('LLIN_use', 'NOMDEP', design = my.svydesign, svymean)
head(Bednet)


#########################################################################################
# Number of U5 children within each cluster 
#########################################################################################

# 2010 IR BF DHS 
num_clu_10 <- svyby.fun('num_kids', 'v001', design = svydesign_10, svytotal)
head(num_clu_10 )

# 2014 IR BF DHS
num_clu_14 <- svyby.fun('num_kids', 'v001', design = svydesign_14, svytotal)
head(num_clu_14)


# 2017 IR BF MIS 
num_clu <- svyby.fun('num_kids', 'v001', design = my.svydesign, svytotal)
head(num_clu)


#######################################################################################
# Proportion of U5 children who slept under a treated bednet by cluster
######################################################################################

# 2010 IR BF DHS
Bednet_clu_10 <- svyby.fun('LLIN_use', 'v001', design = svydesign_10, svymean)
colnames(Bednet_clu_10)[3]<- "standard_error"
head(Bednet_clu_10)


# 2014 IR BF MIS 
Bednet_clu_14 <- svyby.fun('LLIN_use', 'v001', design = svydesign_14, svymean)
colnames(Bednet_clu_14)[3]<- "standard_error"
head(Bednet_clu_14)


# 2017 IR BF MIS 
Bednet_clu <- svyby.fun('LLIN_use', 'v001', design = my.svydesign, svymean)
colnames(Bednet_clu)[3]<- "standard_error" 
head(Bednet_clu)


#######################################################################
## Dataset clean-up for merging and mapping LLIN variables 
#######################################################################


# we want to merge with the bednet DS estimates with full list of DS to identify DS with no estimates
dim(DS_merge)
DS_merge_10<-DS_merge%>%left_join(Bednet_10) #the 2010 IR BF MIS 
DS_merge_14<-DS_merge%>%left_join(Bednet_14) #2014 IR BF MIS
DS_merge_n<-DS_merge%>%left_join(Bednet) #the 2017 IR BF MIS 


# next we merge the # of U5 children in each cluster with the proportion of U5 children that slept under a bednet by cluster

# 2010 IR BF DHS 
pts_estimates_10 <- num_clu_10%>%left_join(Bednet_clu_10)
head(pts_estimates_10)



# 2014 IR BF MIS 
pts_estimates_14 <- num_clu_14%>%left_join(Bednet_clu_14)
head(pts_estimates_14)


# 2017 IR BF MIS
pts_estimates <- num_clu_14%>%left_join(Bednet_clu)
head(pts_estimates)


#creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_10,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/LLIN/LLIN_use_BF10DS.csv") #for the 2010 IR BF MIS
write.csv(DS_merge_14,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/LLIN/LLIN_use_BF14DS.csv") #for the 2014 IR BF MIS
write.csv(DS_merge_n,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/LLIN/LLIN_use_BF17DS.csv") #for the 2017 IR BF MIS


####################################################################################
## U5 LLIN maps 
###################################################################################

#merging the LLIN stats to the sf DS shape file created earlier 
DS_shape_10 <- DSshape_sf%>%left_join(DS_merge_10) #2010 BF DHS 
DS_shape_14 <- DSshape_sf%>%left_join(DS_merge_14) #2014 BF MIS 
DS_shape_n <- DSshape_sf%>%left_join(DS_merge_n) #2017 BF MIS  


#merging the LLIN stats to the points sf file created earlier 
#2010 BF DHS 
pts_n_10 <- merge.rename.fun(pts_estimates_10, pt_sf_10, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "LLIN_use", "LLIN use prevalence by cluster")

#2014 BF MIS 
pts_n_14 <- merge.rename.fun(pts_estimates_14, pt_sf_14, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "LLIN_use", "LLIN use prevalence by cluster")

#20147BF MIS
pts_n <- merge.rename.fun(pts_estimates, pt_sf_17, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "LLIN_use", "LLIN use prevalence by cluster")



#this creates shapefiles with cluster-level estimates
st_write(pt_n_10, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_2010/LLIN_2010_cluster.csv")
st_write(pt_n_14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_2014/LLIN_2014_cluster.csv")
st_write(pt_n, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_2017/LLIN_2017_cluster.csv")



#creatting maps using custom map functions 
# 2010 BF DHS 
LLIN_2010_map <- tmap.fun(DS_shape_10, DSmapvalue="LLIN_use", adminlegtitle=
                "LLIN use prevalence", main_title="LLIN use among U5 children by Health Districts (2010)", text_title = "NOMDEP", ptsfile=pt_n_10,
                "U5 in DHS clusters", "LLIN use prevalence by cluster") 
# 2014 BF MIS  
LLIN_2014_map <- tmap.fun(DS_shape_14, DSmapvalue="LLIN_use", adminlegtitle=
                      "LLIN use prevalence", main_title="LLIN use among U5 children by Health Districts (2014)", text_title = "NOMDEP", ptsfile=pt_n_14,
                      "U5 in DHS clusters", "LLIN use prevalence by cluster") 

# 2017 BF MIS  
LLIN_2017_map <- tmap.fun(DS_shape_n, DSmapvalue="LLIN_use", adminlegtitle=
                    "LLIN use prevalence", main_title="LLIN use among U5 children by Health Districts (2017)", text_title = "NOMDEP", ptsfile=pt_n,
                    "U5 in DHS clusters", "LLIN use prevalence by cluster")


#print map
LLIN_2010_map
LLIN_2014_map
LLIN_2017_map



#save maps 
tmap_save(tm = LLIN_2010_map, filename = "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/maps/burkina_dhs_mis_maps/LLIN/U5/LLIN_map_2010.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
tmap_save(tm = LLIN_2014_map, filename = "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/maps/burkina_dhs_mis_maps/LLIN/U5/LLIN_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
tmap_save(tm = LLIN_2017_map, filename = "outputs/LLIN_map_2017.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)




#######################################################
# Stratified LLIN maps by age and season 
#######################################################

#reading in adjusted ITN data as a list 
temp <- list.files(pattern = "*.csv", full.names = T)
ITNfiles <- sapply(temp, read.csv, simplify = F)

# month_llin_17 <- split(ITNfiles[[1]], ITNfile[[1]]$time2)

ITNList <- list() #create an empty list for holding the new list based on the joins 

#using a for loop to execute the left_join and enter the new objects in a list 
for (I in 1:length(ITNfiles)) {ITNname <-paste0("DS", names(ITNfiles)[I])
ITNList[[ITNname]] <- left_join(DSshape_sf, unique(ITNfiles[[I]])) 
}

#############################################################
# Jan maps
#############################################################

jan.df <- ITNfiles[[1]] %>% dplyr::select(NOMDEP, exp_January)
jan.df <- left_join(DSshape_sf, jan.df)
feb.df <- ITNfiles[[1]] %>% dplyr::select(NOMDEP, exp_february)
feb.df <- left_join(DSshape_sf, feb.df)
mar.df <- ITNfiles[[1]] %>% dplyr::select(NOMDEP, exp_march)
mar.df <- left_join(DSshape_sf, mar.df)
nov.df <- ITNfiles[[1]] %>% dplyr::select(NOMDEP, exp_november)
nov.df <- left_join(DSshape_sf, nov.df)
dec.df <- ITNfiles[[1]] %>% dplyr::select(NOMDEP, exp_december)
dec.df <- left_join(DSshape_sf, dec.df)

class(jan.df)

Jan.ITN <- tmap.fun3(jan.df, "January 2018", "exp_January")
Feb.ITN <- tmap.fun3(feb.df, "February 2018", "exp_february")
mar.ITN <- tmap.fun3(mar.df, "March 2018", "exp_march")
nov.ITN <- tmap.fun3(nov.df, "November 2017", "exp_november")
dec.ITN <- tmap.fun3(dec.df, "December 2017", "exp_december")

all_month_ITN <- tmap_arrange(Jan.ITN, Feb.ITN, mar.ITN, nov.ITN, dec.ITN)

tmap_save(tm = all_month_ITN, filename = "all_month_ITN.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)

#############################################################
# February maps
#############################################################

u5.ITN.2017<-ITNList[[6]]
u5.feb.2017 <- tmap.fun3(u5.ITN.2017, "February 2017", "U5 ITN coverage", "ITN_february")
A.ITN.2017<-ITNList[[5]]
A.feb.2017 <- tmap.fun3(A.ITN.2017, "February 2017", "ITN coverage among adult women", "ITN_february")


#############################################################
# March maps
#############################################################

u5.ITN.2017<-ITNList[[6]]
u5.mar.2017 <- tmap.fun3(u5.ITN.2017, "March 2017", "U5 ITN coverage", "ITN_march")
A.ITN.2017<-ITNList[[5]]
A.mar.2017 <- tmap.fun3(A.ITN.2017, "March 2017", "ITN coverage among adult women", "ITN_march")


#############################################################
# May maps
#############################################################

u5.ITN.2010<-ITNList[[2]]
u5.may.2010 <- tmap.fun3(u5.ITN.2010, "May 2010", "U5 ITN coverage", "ITN_may")
A.ITN.2010<-ITNList[[1]]
A.may.2010 <- tmap.fun3(A.ITN.2010, "May 2010", "ITN coverage among adult women", "ITN_may")


# elements of the function tmap.fun3 (note change column to be mapped in the function)
#list_may.ITN <- list("May 2010", "May 2010", "May 2014", "May 2014", "May 2017", "May 2017")
#list_lab.ITN <- list("ITN coverage among adult women", "U5 ITN coverage", "ITN coverage among adult women", "U5 ITN coverage", "ITN coverage among adult women",
                    # "U5 ITN coverage")

#mayITNmap.list <- list()
#mayITNmap.list <- pmap(list(ITNList, list_may.ITN, list_lab.ITN), tmap.fun3)#applying the user-defined tmap.fun2 to lists 



# ############################################################
# # June maps 
# ############################################################
# 
# u5.ITN.2010<-ITNList[[2]]
# u5.june.2010 <- tmap.fun3(u5.ITN.2010, "June 2010", "U5 ITN coverage", "ITN_june")
# A.ITN.2010<-ITNList[[1]]
# A.june.2010 <- tmap.fun3(A.ITN.2010, "June 2010", "ITN coverage among adult women", "ITN_june")
# 
# 
# ############################################################
# # July maps 
# ############################################################
# 
# u5.ITN.2010<-ITNList[[2]]
# u5.jul.2010 <- tmap.fun3(u5.ITN.2010, "July 2010", "U5 ITN coverage", "ITN_july")
# A.ITN.2010<-ITNList[[1]]
# A.jul.2010 <- tmap.fun3(A.ITN.2010, "July 2010", "ITN coverage among adult women", "ITN_july")
# 
# 
# ############################################################
# # August maps 
# ############################################################
# 
# u5.ITN.2010<-ITNList[[2]]
# u5.aug.2010 <- tmap.fun3(u5.ITN.2010, "August 2010", "U5 ITN coverage", "ITN_august")
# A.ITN.2010<-ITNList[[1]]
# A.aug.2010 <- tmap.fun3(A.ITN.2010, "August 2010", "ITN coverage among adult women", "ITN_august")
# 
# 
# ############################################################
# # October maps 
# ############################################################
# ITN.oct <- list(ITNList[[1]], ITNList[[2]], ITNList[[3]], ITNList[[4]])
# 
# # elements of the function tmap.fun3 
# list_oct.ITN <- list("October 2010", "October 2010", "October 2014", "October 2014")
# list_oct.lab <- list("ITN coverage among adult women", "U5 ITN coverage", "ITN coverage among adult women", "U5 ITN coverage")
# list_col.ITN <- list("ITN_october","ITN_october","ITN_october","ITN_october"  )
# 
# 
# octITNmap.list <- list()
# octITNmap.list <- pmap(list(ITN.oct, list_oct.ITN, list_oct.lab,list_col.ITN), tmap.fun3)#applying the user-defined tmap.fun2 to lists 
# 
# 
# ############################################################
# # november maps 
# ###########################################################
# # elements of the function tmap.fun3 
# list_nov.ITN <- list("November 2010", "November 2010", "November 2014", "November 2014","November 2017", "November 2017")
# list_nov.lab <- list("ITN coverage among adult women", "U5 ITN coverage", "ITN coverage among adult women", "U5 ITN coverage","ITN coverage among adult women", 
#                      "U5 ITN coverage" )
# list_col.novITN <- list("ITN_november","ITN_november","ITN_november","ITN_november","ITN_november","ITN_november")
# 
# 
# novITNmap.list <- list()
# novITNmap.list <- pmap(list(ITNList, list_nov.ITN, list_nov.lab,list_col.novITN), tmap.fun3)#applying the user-defined tmap.fun2 to lists 
# 
# 
# ############################################################
# # december maps 
# ###########################################################
# # elements of the function tmap.fun3 
# list_dec.ITN <- list("December 2010", "December 2010", "December 2014", "December 2014","December 2017", "December 2017")
# list_dec.lab <- list("ITN coverage among adult women", "U5 ITN coverage", "ITN coverage among adult women", "U5 ITN coverage","ITN coverage among adult women", 
#                      "U5 ITN coverage" )
# list_col.decITN <- list("ITN_december","ITN_december","ITN_december","ITN_december","ITN_december","ITN_december")
# 
# 
# decITNmap.list <- list()
# decITNmap.list <- pmap(list(ITNList, list_dec.ITN,list_dec.lab,list_col.novITN), tmap.fun3)#applying the user-defined tmap.fun2 to lists 
# 
# #################################################################
# # aggregate maps by age
# ################################################################
# 
# # u5
# u5.ITN.2010maps <- tmap_arrange(u5.may.2010, u5.june.2010, u5.jul.2010, u5.aug.2010, octITNmap.list[[2]], novITNmap.list[[2]],decITNmap.list[[2]]) 
# u5.ITN.2014maps <- tmap_arrange(octITNmap.list[[4]],novITNmap.list[[4]], decITNmap.list[[4]])
# u5.ITN.2017maps <- tmap_arrange(u5.feb.2017,u5.mar.2017, novITNmap.list[[5]], decITNmap.list[[5]])
# 
# # adults
# A.ITN.2010maps <- tmap_arrange(A.may.2010, A.june.2010, A.jul.2010, A.aug.2010, octITNmap.list[[1]], novITNmap.list[[1]],decITNmap.list[[1]]) 
# A.ITN.2014maps <- tmap_arrange(octITNmap.list[[3]],novITNmap.list[[3]], decITNmap.list[[3]])
# A.ITN.2017maps <- tmap_arrange(A.feb.2017,A.mar.2017, novITNmap.list[[6]], decITNmap.list[[6]])
# 
# tmap_save(tm = A.ITN.2017maps, filename = "2017_ITN_adjusted/Adult/Adult_2017adjustedITN.pdf", 
#           width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
# 
# 
# 
# #############################################################################################
# # smoothed maps 
# ############################################################################################
# 
# #reading in adjusted ITN data as a list 
# temp2 <- list.files(path="ITN_sm", pattern = "*.csv", full.names = T)
# ITN_sm_files <- sapply(temp2, read.csv, simplify = F)
# 
# ITN_sm_List <- list() #create an empty list for holding the new list based on the joins 
# 
# #using a for loop to execute the left_join and enter the new objects in a list 
# for (I in 1:length(ITN_sm_files)) {ITN_sm_name <-paste0("DS", names(ITN_sm_files)[I])
# ITN_sm_List[[ITN_sm_name]] <- left_join(DSshape_sf, unique(ITN_sm_files[[I]])) 
# }
# 
# # maps 
# # elements of the function tmap.fun3 
# list_sm.ITN <- list("2014", "2017", "2014", "2017")
# list_sm.lab <- list("ITN coverage among adult women (smoothed)", "ITN coverage among adult women (smoothed)", "U5 ITN coverage (smoothed)",
#                      "U5 ITN coverage (smoothed)")
# list_col.smITN <- list("smoothITN.est","smoothITN.est","smoothITN.est","smoothITN.est")
# 
# smITNmap.list <- list()
# smITNmap.list <- pmap(list(ITN_sm_List,list_sm.ITN, list_sm.lab,list_col.smITN), tmap.fun4)#applying the user-defined tmap.fun2 to lists 
# 
# smITNmap.list[[4]]
# 
# tmap_save(tm = smITNmap.list[[4]], filename = "ITN_sm/2017/U5_2017smoothedITN.pdf", 
#           width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)





##########################################################################################
##  Adult women's LLIN access
##########################################################################################

## Setting up the survey related variables and survey design object for women's LLIN access

#recoding and cleaning dataset adult LLIN access variables for analysis and other data processing

# 2014 IR BF MIS
mis14_s127 <- dataclean.fun(mis14_r, 's127', 'LLIN_adult', 1) 
s127.svydesign <- svydesign.fun(mis14_s127)


# 2017 IR BF MIS
mis17_v459 <- dataclean.fun(mis17_r, 'v459', 'LLIN_adult', 1) 
v459.svydesign <- svydesign.fun(mis17_v459)



#########################################################################################
## Proportion of women who own a bednet by health district
#########################################################################################

# 2014 IR BF MIS 
Bednet_s127<- svyby.fun('LLIN_adult', 'NOMDEP', design = s127.svydesign, namefun = svymean)
head(Bednet_s127)

# 2017 IR BF MIS 
Bednet_v459<- svyby.fun('LLIN_adult', 'NOMDEP', design = v459.svydesign, namefun = svymean)
head(Bednet_v459)



########################################################################################
## Number of women in each cluster
######################################################################################

# 2014 IR BF MIS 
s127_num_clu <- svyby.fun('num_p', 'v001', design =s127.svydesign, namefun = svytotal)
head(s127_num_clu)

# 2017 IR BF MIS 
v459_num_clu <- svyby.fun('num_p', 'v001', design =v459.svydesign, namefun = svytotal)
head(v459_num_clu)


#################################################################################
## Proportion of women who own a bednet by cluster
################################################################################

# 2014 IR BF MIS 
Bednet_clu_s127 <- svyby.fun('LLIN_adult', 'v001', design =s127.svydesign, namefun = svymean)
colnames(Bednet_clu_s127)[3]<- "standard_error"
head(Bednet_clu_s127)

# 2017 IR BF MIS
Bednet_clu_v459 <- svyby.fun('LLIN_adult', 'v001', design =v459.svydesign, namefun = svymean)
colnames(Bednet_clu_v459)[3]<- "standard_error" 
head( Bednet_clu_v459)


##############################################################################
## Dataset clean-up for merging and mapping adult women's LLIN access 
#############################################################################

#we want to merge with the full list of health districts

#we start with the proportion of children that slept under a bednet by health district for the 2017 BF MIS 
dim(DS_merge)
DS_merge_s127<-DS_merge%>%left_join(Bednet_s127) #2014 IR BF MIS 
DS_merge_v459<-DS_merge%>%left_join(Bednet_v459) #2017 IR BF MIS 



#next we merge the # of women in each cluster with the number women that slept under a bednet by cluster
pts_est_s127<-s127_num_clu%>%left_join(Bednet_clu_s127)#2014 IR BF MIS 
head(pts_est_s127)

                    
pts_est_v459<-v459_num_clu%>%left_join(Bednet_clu_v459)#2017 IR BF MIS 
head(pts_est_v459)


#creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_s127,file="outputs/LLIN_adultW_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_v459,file="outputs/LLIN_adultW_BF17DS.csv") #2017 IR BF MIS


#################################################################################
## Adult women's LLIN access maps 
#################################################################################

# merging the LLIN stats to the sf DS shape file created earlier 
DS_shape_s127 <- DSshape_sf%>%left_join(DS_merge_s127) #2014 IR BF MIS 
DS_shape_v459 <- DSshape_sf%>%left_join(DS_merge_v459) #2017 IR BF MIS  


# merging the LLIN stats to the points sf file created earlier and renaming variables with custom function  

# 2014 IR BF MIS 
pt_s127 <- merge.rename.fun(pts_est_s127, pt_sf_14, "v001", "DHSCLUST", "num_p", "Women in DHS clusters", "LLIN_adult", 
                            "Adult women LLIN access prevalence by cluster")


# 2017 IR BF MIS 
pt_v459 <- merge.rename.fun(pts_est_v459, pt_sf_17, "v001", "DHSCLUST", "num_p", "Women in DHS clusters", "LLIN_adult", 
                            "Adult women LLIN access prevalence by cluster")



# this creates shapefiles with cluster-level estimates of the % of women that slept under a bednet 
st_write(pt_v459,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adultW_2017/LLIN_adultW17.csv")
st_write(pt_s127,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adultW_2014/LLIN_adultW14.csv")


# 2014 IR BF MIS
LLIN_adultW_2014_map <- tmap.fun(DS_shape_s127, DSmapvalue="LLIN_adult", adminlegtitle="LLIN access prevalence", 
                                 main_title="Adult women LLIN access by Health Districts (2014)", text_title = "NOMDEP", 
                            ptsfile=pt_s127, "Women in DHS clusters", "Adult women LLIN access prevalence by cluster") 

# 2017 IR BF MIS 
LLIN_adultW_2017_map <- tmap.fun(DS_shape_v459, DSmapvalue="LLIN_adult", adminlegtitle="LLIN access prevalence", 
                                 main_title="Adult women LLIN access by Health Districts (2014)", text_title = "NOMDEP", 
                                 ptsfile=pt_v459, "Women in DHS clusters", "Adult women LLIN access prevalence by cluster") 


# print map 
LLIN_adultW_2014_map
LLIN_adultW_2017_map


#save maps 
tmap_save(tm = LLIN_adultW_2014_map, filename = "outputs/LLIN_adultW_map2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = LLIN_adultW_2017_map, filename = "outputs/LLIN_adultW_map2017.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)







###########################################################################################
## Adult women's LLIN use
###########################################################################################

## Setting up the survey related variables and survey design object for women's LLIN use

#recoding and cleaning adult LLIN use variables for analysis and other data processing

# 2014 IR BF MIS
mis14_s129 <- dataclean.fun(mis14_r, 's129', 'LLIN_use_a', 1) 
s129.svydesign <- svydesign.fun(mis14_s129)

# 2017 IR BF MIS
mis17_r <- mis17_r%>% mutate(MM = (v008 - ((v007 - 1900) * 12))) %>% #dhs pr 2010
  mutate(YYYY = (floor((v008 - 1)/12)+1900))%>%
  mutate (timepoint = str_c(MM, v016, YYYY, sep = '-'))%>%
  mutate(time2 = str_c(MM, YYYY, sep = '-'))


table(mis17_r$NOMDEP)
mis17_ml101 <- dataclean(mis17_r, ml101, v005, 'ml101', 'LLIN_use_a') 
 
mis17_ml101$LLIN_use_a

ml101.svydesign<- svydesign.fun(mis17_ml101)


#########################################################################################
## Proportion of women who use a bednet by health district
#########################################################################################

# 2014 IR BF MIS 
Bednet_s129<- svyby.fun('LLIN_use_a', 'NOMDEP', design = s129.svydesign, namefun = svymean)
head(Bednet_s129)

library(rlang)



# 2017 IR BF MIS 
Bednet_ml101<- result.fun('LLIN_use_a', 'NOMDEP', 'num_p', design = ml101.svydesign)
head(Bednet_ml101)

DS_file <- DSshape_sf %>%  left_join(Bednet_ml101)

map <- tmap.fun4(DS_file, "maintitle", "legtitle", "LLIN_use_a")


Bednet_ml101<- result.fun.para('LLIN_use_a', 'NOMDEP + time2', 'num_p', 'NOMDEP', design = ml101.svydesign)
head(Bednet_ml101)

month_llin_17 <- split(Bednet_ml101, Bednet_ml101$time2) #split the dataframe into a list 

dfList <- list() #create an empty list for holding the new list based on the joins 

#using a for loop to execute the left_join and enter the new objects in a list 
for (I in 1:length(month_llin_17)) {dfname <-paste0("DS", names(month_llin_17)[I])
dfList[[dfname]] <- left_join(DSshape_sf, unique(month_llin_17[[I]])) 
}


## saving the 2010 PR BF DHS PfPr by month of survey maps in a list
list_title<- list("January 2018", "November 2017", "December 2017", "February 2018", "March 2018")
mapbymonth.list <- list()
mapbymonth.list <-map2(dfList, list_title, tmap.fun3)
#plotting and printing can be accomplished by indexing the list 
mapbymonth.list[[1]]
mapbymonth.list[[2]]
mapbymonth.list[[3]]
mapbymonth.list[[4]]
mapbymonth.list[[5]]

all_adult_ITN <- tmap_arrange(mapbymonth.list[[1]],mapbymonth.list[[2]], mapbymonth.list[[3]],mapbymonth.list[[4]],mapbymonth.list[[5]])

tmap_save(tm = all_adult_ITN, filename = "all_adult_ITN.pdf",
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)

##########################################################################################
## Number of women in each cluster
##########################################################################################

# 2014 IR BF MIS 
s129_num_clu <- svyby.fun('num_p', 'v001', design =s129.svydesign, namefun = svytotal)
head(s127_num_clu)

# 2017 IR BF MIS 
ml101_num_clu <- svyby.fun('num_p', 'v001', design =ml101.svydesign, namefun = svytotal)
head(ml101_num_clu)


########################################################################################
## Proportion of women who slept under a bednet by cluster
########################################################################################

# 2014 IR BF MIS 
Bednet_clu_s129 <- svyby.fun('LLIN_use_a', 'v001', design =s129.svydesign, namefun = svymean)
colnames(Bednet_clu_s129)[3]<- "standard_error"
head(Bednet_clu_s129)

# 2017 IR BF MIS 
Bednet_clu_ml101 <- svyby.fun('LLIN_use_a', 'v001', design =ml101.svydesign, namefun = svymean)
colnames(Bednet_clu_ml101)[3]<- "standard_error"
head(Bednet_clu_ml101)



#########################################################################################
## Dataset clean-up for merging and mapping adult women's LLIN use
#########################################################################################

# Merging the DS-level estimates of women who slept under a bednet with the full list of health districts
dim(DS_merge)
DS_merge_s129<-DS_merge%>%left_join(Bednet_s129) #2014 IR BF MIS 
DS_merge_ml101<-DS_merge%>%left_join(Bednet_ml101) #2017 IR BF MIS 


# Merging the # of women in each cluster with the # of women that slept under a bednet by surveyed cluster

pts_est_s129<-s129_num_clu%>%left_join(Bednet_clu_s129) #2014 IR BF MIS 
head(pts_est_s129)

pts_est_ml101<-ml101_num_clu%>%left_join(Bednet_clu_ml101)#2017 IR BF MIS 
head(pts_est_ml101)


####creates a csv file of the proportion of children that slept under a bednet by health district 
write.csv(DS_merge_s129,file="LLIN_adult_use_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_ml101,file="LLIN_adult_use_BF17DS.csv") #2017 IR BF MIS

#########################################################################################
## Adult women's bednet use maps
#########################################################################################

# merging the adult women's LLIN use stats to the sf DS shape file created earlier 
DS_shape_s129 <- DSshape_sf%>%left_join(DS_merge_s129) #2014 IR BF MIS 
DS_shape_ml101 <- DSshape_sf%>%left_join(DS_merge_ml101) #2017 IR BF MIS  


# merging the adult women's LLIN use cluster-level stats to the points sf file created earlier 

# 2014 IR BF MIS 
pt_s129 <- merge.rename.fun(pts_est_s129, pt_sf_14, "v001", "DHSCLUST", "num_p", "Women in DHS clusters", "LLIN_use_a", 
                            "Adult women LLIN use prevalence by cluster")

# 2017 IR BF MIS
pt_ml101 <- merge.rename.fun(pts_est_ml101, pt_sf_17, "v001", "DHSCLUST", "num_p", "Women in DHS clusters", "LLIN_use_a", 
                            "Adult women LLIN use prevalence by cluster")


#this creates a shapefile with cluster-level estimates of the % of women that slept under a bednet 
st_write(pt_s129,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adult_use_2014/LLIN_adultuse14.csv")
st_write(pt_ml101,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/LLIN_adult_use_2017/LLIN_adultuse17.csv")



#2014 IR BF MIS 
LLIN_adult_use_2014_map <- tmap.fun(DS_shape_s129, DSmapvalue="LLIN_use_a", adminlegtitle="LLIN use prevalence", 
                                 main_title="Adult women LLIN use by Health Districts (2014)", text_title = "NOMDEP", 
                                 ptsfile=pt_s129, "Women in DHS clusters", "Adult women LLIN use prevalence by cluster") 

LLIN_adult_use_2014_nu <- tmap.fun3(DS_shape_s129, maintitle="Adult women LLIN use by Health Districts (2014)", 
                                    legtitle ="Adult women LLIN use prevalence", colname="LLIN_use_a")
                              


#2017 IR BF MIS 
LLIN_adult_use_2017_map <- tmap.fun(DS_shape_ml101, DSmapvalue="LLIN_use_a", adminlegtitle="LLIN use prevalence", 
                                    main_title="Adult women LLIN use by Health Districts (2017)", text_title = "NOMDEP", 
                                    ptsfile=pt_ml101, "Women in DHS clusters", "Adult women LLIN use prevalence by cluster") 

LLIN_adult_use_2017_nu <- tmap.fun3(DS_shape_ml101, maintitle="Adult women LLIN use by Health Districts (2017)", 
                                    legtitle ="Adult women LLIN use prevalence", colname="LLIN_use_a")




#print map 
LLIN_adult_use_2014_map
LLIN_adult_use_2017_map

#save maps 
tmap_save(tm = LLIN_adult_use_2014_map, filename = "outputs/LLIN_adult_use_map2014.pdf",
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = LLIN_adult_use_2017_map, filename = "LLIN_adult_use_map2017.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)

tmap_save(tm = LLIN_adult_use_2017_nu, filename = "LLIN_adult_use_map2017_nu.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)






######################################################################################################
# U5 fever reports
#####################################################################################################

## setting up the survey related variables and survey design object 


# 2014 IR BF MIS 
mis14_h22<-create.data(mis14_l, 'h22_1', 'h22_2', 'h22_3', 'h22_4', 'count', 'fever')
f14.svydesign <-svydesign.fun(mis14_h22)


# 2017 IR BF MIS 
mis17_h22<<-create.data(mis17_l, 'h22_1', 'h22_2', 'h22_3', 'h22_4', 'count', 'fever')
f.svydesign  <-svydesign.fun(mis17_h22)



#################################################################################################
## Proportion of U5 children with fever by health district
#################################################################################################

# 2014 IR BF MIS 
fever_14 <- svyby.fun('fever', 'NOMDEP', design = f14.svydesign, namefun = svymean)
head(fever_14)


#2017 IR BF MIS
fever <- svyby.fun('fever', 'NOMDEP', design = f.svydesign, namefun = svymean)
head(fever)


################################################################################################
## Number of U5 by cluster
################################################################################################

#2014 IR BF MIS
f14_num_clu <- svyby.fun('num_kids', 'v001', design =f14.svydesign, namefun = svytotal)
head(f14_num_clu)

#2017 IR BF MIS
f_num_clu <- svyby.fun('num_kids', 'v001', design =f.svydesign, namefun = svytotal)
head(f_num_clu)


##############################################################################################
## Proportion of U5 children who had fever by cluster 
###############################################################################################

#2014 IR BF MIS 
fever14_clu <- svyby.fun('fever', 'v001', design =f14.svydesign, namefun = svymean)
colnames(fever14_clu)[3]<- "standard_error"
head(fever14_clu)


#2017 IR BF MIS 
fever_clu <- svyby.fun('fever', 'v001', design =f.svydesign, namefun = svymean)
colnames(fever_clu)[3]<- "standard_error"
head(fever_clu)




############################################################################################
## Dataset clean-up for merging and mapping U5 fever reports 
###########################################################################################

#Merging DS-level estimates of the % U5 with fever with full list of health districts
dim(DS_merge)
DS_merge_f14<-DS_merge%>%left_join(fever_14) #2014 IR BF MIS 
DS_merge_f<-DS_merge%>%left_join(fever) #2017 IR BF MIS 


#Merging the # of U5 children in each cluster with the % of U5 children that had fever in each cluster
pts_est_f14<-f14_num_clu%>%left_join(fever14_clu) #2014 IR BF MIS 

pts_est_f<-f_num_clu%>%left_join(fever_clu) #2017 IR BF MIS 
  

#creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_f14,file="outputs/fever_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_f,file="outputs/fever_BF17DS.csv") #2017 IR BF MIS 


##############################################################################################
## U5 fever maps 
##############################################################################################

#merging the fever stats to the sf DS shape file
DS_shape_f14 <- DSshape_sf%>%left_join(DS_merge_f14) #2014 IR BF MIS
DS_shape_f <- DSshape_sf%>%left_join(DS_merge_f) #2017 IR BF MIS


# merging the full list of clusters with the cluster-level estimates and converting to sf object

# 2014 IR BF MIS
pt_f14 <- merge.rename.fun(pts_est_f14, pt_sf_14, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "fever", 
                            "Fever status by cluster")

# 2017 IR BF MIS 
pt_f <- merge.rename.fun(pts_est_f, pt_sf_17, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "fever", 
                           "Fever status by cluster")


#this creates a shapefile with cluster-level estimates of the proportion of children with fever 
st_write(pt_f14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Fever_2014/fever_2014_cluster.csv")
st_write(pt_f, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Fever_2017/fever_2017_cluster.csv")


# 2014 IR BF MIS
fever_2014_map <- tmap.fun(DS_shape_f14, DSmapvalue="fever", adminlegtitle="Fever prevalence", 
                                 main_title="Fever status among U5 by Health Districts (2014)", text_title = "NOMDEP", 
                                 ptsfile=pt_f14, "U5 in DHS clusters", "Fever status by cluster") 



#2017 IR BF MIS
fever_2017_map <- tmap.fun(DS_shape_f, DSmapvalue="fever", adminlegtitle="Fever prevalence", 
                           main_title="Fever status among U5 by Health Districts (2017)", text_title = "NOMDEP", 
                           ptsfile=pt_f, "U5 in DHS clusters", "Fever status by cluster") 


#print map 
fever_2014_map
fever_2017_map


#saving map
tmap_save(tm = fever_2014_map, filename = "outputs/fever_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = fever_2017_map, filename = "outputs/fever_map_2017.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)





####################################################################################################
# U5 receipt of medical treatment for fever
####################################################################################################

## setting up the survey related variables and survey design object


# 2014 IR BF MIS 
mis14_h32z<-create.data(mis14_l, 'h32z_1', 'h32z_2', 'h32z_3', 'h32z_4', 'count', 'med_fever')
med14.svydesign <-svydesign.fun(mis14_h32z)


#2017 IR BF MIS
mis17_h32z <-create.data(mis17_l, 'h32z_1', 'h32z_2', 'h32z_3', 'h32z_4', 'count', 'med_fever')
med.svydesign <-svydesign.fun(mis17_h32z)


#################################################################################################
## Proportion of U5 children with fever that received medical treatment by health district
#################################################################################################

#2014 IR BF MIS 
medical_14 <- svyby.fun('med_fever', 'NOMDEP', design = med14.svydesign, namefun = svymean)
head(medical_14)



#2017 IR BF MIS 
medical <- svyby.fun('med_fever', 'NOMDEP', design = med.svydesign, namefun = svymean)
head(medical)


#############################################################################################
## Number of U5 by cluster
###########################################################################################


# 2014 IR BF MIS
m14_num_clu <- svyby.fun('num_kids', 'v001', design = med14.svydesign, namefun = svytotal)
head(m14_num_clu)


# 2017 IR BF MIS
m_num_clu <- svyby.fun('num_kids', 'v001', design = med.svydesign, namefun = svytotal)
head(m_num_clu)


####################################################################################
## Proportion of U5 children that received medical treatment for fever by cluster
####################################################################################

# 2014 IR BF MIS 
med14_clu <- svyby.fun('med_fever', 'v001', design =med14.svydesign, namefun = svymean)
colnames(med14_clu)[3]<- "standard_error"
head(med14_clu)


#2017 IR BF MIS 
med_clu <- svyby.fun('med_fever', 'v001', design =med.svydesign, namefun = svymean)
colnames(med_clu)[3]<- "standard_error"
head(med_clu)



####################################################################################
## Dataset clean-up merging for mapping medical treatment for fever
####################################################################################

# merging estimates of DS U5 medical treatment for fever with the full list of health districts

dim(DS_merge)
DS_merge_m14<-DS_merge%>%left_join(medical_14) #2014 IR BF MIS
DS_merge_m<-DS_merge%>%left_join(medical) #2017 IR BF MIS  


#merging the # of U5 children with the % of U5 children that received treatment for fever in each cluster

pts_est_m14<-m14_num_clu%>%left_join(med14_clu)#2014 IR BF MIS
 

pts_est_m<-m_num_clu%>%left_join(med_clu) #2017 IR BF MIS  



#creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_m14,file="outputs/med_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_m,file="outputs/med_BF17DS.csv") #2017 IR BF MIS 



####################################################################################
## Medical treatment for fever maps 
####################################################################################

#merging the health district fever stats to the sf DS shape file
DS_shape_m14 <- DSshape_sf%>%left_join(DS_merge_m14) #2014 IR BF MIS
DS_shape_m <- DSshape_sf%>%left_join(DS_merge_m) #2017 IR BF MIS


#merging the full list of clusters to the cluster estimates and converting to sf objects

# 2014 IR BF MIS
pt_m14 <- merge.rename.fun(pts_est_m14, pt_sf_14, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "med_fever", 
                           "Medical treatment for fever by cluster")


# 2017 IR BF MIS
pt_m <- merge.rename.fun(pts_est_m, pt_sf_17, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "med_fever", 
                           "Medical treatment for fever by cluster")



#this creates a shapefile that with cluster-level estimates of medical treatment for fever
st_write(pt_m14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2014/shape/medtreat_2014_cluster.csv")
st_write(pt_m, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Medical_Treat_2017/shape/medtreat_2017_cluster.csv")



#2014 IR BF MIS
med_2014_map <- tmap.fun(DS_shape_m14, DSmapvalue="med_fever", adminlegtitle="Prevalence of medical treatment for fever", 
                           main_title="U5 treatment for fever by Health Districts (2014)", text_title = "NOMDEP", 
                           ptsfile=pt_m14, "U5 in DHS clusters", "Medical treatment for fever by cluster") 


#2017 IR BF MIS
med_2017_map <- tmap.fun(DS_shape_m, DSmapvalue="med_fever", adminlegtitle="Prevalence of medical treatment for fever", 
                         main_title="U5 treatment for fever by Health Districts (2017)", text_title = "NOMDEP", 
                         ptsfile=pt_m14, "U5 in DHS clusters", "Medical treatment for fever by cluster") 



#print map 
med_2014_map
med_2017_map


#saving map
tmap_save(tm = med_2014_map, filename = "outputs/medfever_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)
tmap_save(tm = med_2017_map, filename = "outputs/medfever_map_2017.pdf",
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)





#################################################################################################################
# U5 ACT taken for fever
################################################################################################################

## setting up the survey related variables and survey design object


#2014 IR BF MIS

# 2014 IR BF MIS 
mis14_act<-create.data(mis14_l, 'ml13e_1', 'ml13e_2', 'ml13e_3', 'ml13e_4', 'count', 'act')
act14.svydesign <-svydesign.fun(mis14_act)



# 2017 IR BF MIS
mis17_act<-create.data(mis17_l, 'ml13e_1', 'ml13e_2', 'ml13e_3', 'ml13e_4', 'count', 'act')
act.svydesign <-svydesign.fun(mis17_act)



######################################################################################################
## Proportion of U5 children that received act for fever by health district
######################################################################################################

#2014 IR BF MIS

# 2014 IR BF MIS 
act14 <- svyby.fun('act', 'NOMDEP', design = act14.svydesign, namefun = svymean)
head(act14)



# 2017 IR BF MIS
act <- svyby.fun('act', 'NOMDEP', design = act.svydesign, namefun = svymean)
head(act)



################################################################################################
## Number of U5 by cluster
################################################################################################

# 2014 IR BF MIS 
act14_num_clu <- svyby.fun('num_kids', 'v001', design =act14.svydesign, namefun = svytotal)
head(act14_num_clu)



#2017 IR BF MIS 
act_num_clu <- svyby.fun('num_kids', 'v001', design =act.svydesign, namefun = svytotal)
head(act_num_clu)



############################################################################################
## Proportion of U5 children who received ACT for fever by cluster
###########################################################################################


# 2014 IR BF MIS 
act14_clu <- svyby.fun('act', 'v001', design =act14.svydesign, namefun = svymean)
colnames(act14_clu)[3]<- "standard_error"
head(act14_clu)


#2017 IR BF MIS
act_clu <- svyby.fun('act', 'v001', design =act.svydesign, namefun = svymean)
colnames(act_clu)[3]<- "standard_error"
head(act_clu)



###########################################################################################
## Dataset clean-up for merging and mapping U5 ACT for fever
###########################################################################################


#merging the DS-level estimates  of the % of U5 that received ACT for fever with the full list of health districts
dim(DS_merge)
DS_merge_act14<-DS_merge%>%left_join(act14) #2014 IR BF MIS
DS_merge_act<-DS_merge%>%left_join(act) #2017 IR BF MIS


#merging the # of U5 children in each cluster with the % of U5 that had ACT for fever by cluster
pts_est_act14<-act14_num_clu%>%left_join(act14_clu) #2014 IR BF MIS
   
pts_est_act<-act_num_clu%>%left_join(act_clu) #2017 IR BF MIS


####creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_act14,file="outputs/act_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_act,file="outputs/act_BF17DS.csv") #2017 IR BF MIS


###############################################################################
## U5 ACT fever maps 
##############################################################################

#merging the health district fever stats to the sf DS shape file 
DS_shape_act14 <- DSshape_sf%>%left_join(DS_merge_act14) #2014 IR BF MIS
DS_shape_act <- DSshape_sf%>%left_join(DS_merge_act) #2017 IR BF MIS


#convert the cluster-level estimates to sf object

# 2014 IR BF MIS
pt_act14 <- merge.rename.fun(pts_est_act14, pt_sf_14, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "act", 
                           "ACT for fever by cluster")

# 2017 IR BF MIS
pt_act <- merge.rename.fun(pts_est_act, pt_sf_17, "v001", "DHSCLUST", "num_kids", "U5 in DHS clusters", "act", 
                             "ACT for fever by cluster")


#this creates shapefiles with cluster-level estimates of the U5 that received ACT for fever 
st_write(pt_act, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/ACT_fever_2017/act_17.csv")
st_write(pt_act14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/ACT_fever_2014/act_14.csv")



# 2014 IR BF MIS
ACT_2014_map <- tmap.fun(DS_shape_act14, DSmapvalue="act", adminlegtitle="Prevalence of ACT treatment for fever", 
                           main_title="U5 ACT for fever by Health Districts (2014)", text_title = "NOMDEP", 
                           ptsfile=pt_act14, "U5 in DHS clusters", "ACT for fever by cluster") 


# 2017 IR BF MIS
ACT_2017_map <- tmap.fun(DS_shape_act, DSmapvalue="act", adminlegtitle="Prevalence of ACT treatment for fever", 
                         main_title="U5 ACT for fever by Health Districts (2017)", text_title = "NOMDEP", 
                         ptsfile=pt_act, "U5 in DHS clusters", "ACT for fever by cluster") 



#print map 
ACT_2014_map
ACT_2017_map


#saving map
tmap_save(tm = ACT_2014_map, filename = "outputs/ACTfever_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)
tmap_save(tm = ACT_2017_map, filename = "outputs/ACTfever_map_2017.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)




#############################################################################################
# Intermittent preventive treatment in pregnancy (sp/fansidar)
#############################################################################################

## setting up the survey related variables and survey design object


# 2014 IR BF MIS
mis14_spf <- dataclean.fun(mis14_r, 'm49a_1', 'sp_f', 1) 
spf.svydesign <- svydesign.fun(mis14_spf)



#2017 IR BF MIS 
mis17_spf <- dataclean.fun(mis17_r, 'm49a_1', 'sp_f', 1) 
spf17.svydesign <- svydesign.fun(mis17_spf)


###########################################################################################
## Proportion of women that received IPTp with sp/fanisdar for malaria by health district
##########################################################################################

# 2014 IR BF MIS 
iptp<- svyby.fun('sp_f', 'NOMDEP', design = spf.svydesign, namefun = svymean)
head(iptp)


#2017 IR BF MIS 
iptp_17<- svyby.fun('sp_f', 'NOMDEP', design = spf17.svydesign, namefun = svymean)
head(iptp_17)



#################################################################################################
## Number of women that responded to the question on IPTp with sp/fansidar for fever by cluster
#################################################################################################

 
# 2014 IR BF MIS 
iptp_num_clu <- svyby.fun('num_p', 'v001', design =spf.svydesign, namefun = svytotal)
head(iptp_num_clu)


#2017 IR BF MIS 
iptp17_num_clu <- svyby.fun('num_p', 'v001', design =spf17.svydesign, namefun = svytotal)
head(iptp17_num_clu)



###########################################################################################
## Proportion of women that received IPTp with sp/fansidar for malaria by cluster
###########################################################################################


# 2014 IR BF MIS 
iptp_clu <- svyby.fun('sp_f', 'v001', design =spf.svydesign, namefun = svymean)
colnames(iptp_clu)[3]<- "standard_error"
head(iptp_clu)



#2017 IR BF MIS 
iptp17_clu <- svyby.fun('sp_f', 'v001', design =spf17.svydesign, namefun = svymean)
colnames(iptp17_clu)[3]<- "standard_error"
head(iptp17_clu)


#######################################################################################
## Dataset clean-up for merging and mapping IPTP 
#######################################################################################


#merging the list of all DS with DS level estimates of the proportion of women receiving IPTP
dim(DS_merge)
DS_merge_iptp17<-DS_merge%>%left_join(iptp_17) #2017 IR BF MIS
DS_merge_iptp<-DS_merge%>%left_join(iptp) #2014 IR BF MIS

#merging the # of women in each cluster with the # of women that had IPTp by cluster
pts_est_iptp<-iptp_num_clu%>%left_join(iptp_clu) #2014 IR BF MIS
 
pts_est_iptp17<-iptp17_num_clu%>%left_join(iptp17_clu) #2017 IR BF MIS



#creates a csv file of the proportion of children with fever by health district 
write.csv(DS_merge_iptp,file="outputs/IPTp_spf_BF14DS.csv") #2014 IR BF MIS
write.csv(DS_merge_iptp17,file="outputs/IPTp_spf_BF17DS.csv") #2017 IR BF MIS


#####################################################################################
## IPTP maps for the youngest child 
#####################################################################################

#merging the health district fever stats to the sf DS shape file 
DS_shape_iptp <- DSshape_sf%>%left_join(DS_merge_iptp) #2014 IR BF MIS 
DS_shape_iptp17 <- DSshape_sf%>%left_join(DS_merge_iptp17) #2017 IR BF MIS 


#merging the points file to the points cluster values 

# 2014 IR BF MIS 
pt_iptp <- merge.rename.fun(pts_est_iptp, pt_sf_14, "v001", "DHSCLUST", "num_p", "Women in DHS clusters", "sp_f", 
                            "IPTp with sp/fansidar by cluster")
# 2017 IR BF MIS 
pt_iptp17 <- merge.rename.fun(pts_est_iptp17, pt_sf_17, "v001", "DHSCLUST", "num_p", "Women in DHS clusters", "sp_f", 
                            "IPTp with sp/fansidar by cluster")



#writing the cluster-level estimates to a shapefile 
st_write(pt_iptp17,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IPTP_spf_2017/iptp17.csv")
st_write(pt_iptp,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IPTP_spf_2014/iptp14.csv")


#2014 IR BF MIS 
IPTP_2014_map <- tmap.fun(DS_shape_iptp, DSmapvalue="sp_f", adminlegtitle="IPTP with sp/fansidar prevalence", 
                          main_title="IPTp with sp/fansidar by Health Districts (2014)", text_title = "NOMDEP", 
                          ptsfile=pt_iptp, "Women in DHS clusters", "IPTp with sp/fansidar by cluster") 



#2017 IR BF MIS
IPTP_2017_map <- tmap.fun(DS_shape_iptp17, DSmapvalue="sp_f", adminlegtitle="IPTP with sp/fansidar prevalence", 
                          main_title="IPTp with sp/fansidar by Health Districts (2017)", text_title = "NOMDEP", 
                          ptsfile=pt_iptp17, "Women in DHS clusters", "IPTp with sp/fansidar by cluster") 



#print map
IPTP_2014_map
IPTP_2017_map


#saving map
tmap_save(tm = IPTP_2014_map, filename = "outputs/IPTP_spf_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)
tmap_save(tm = IPTP_2017_map, filename = "outputs/IPTP_spf_map_2017.pdf", 
          width=13, height=13, units ="in", asp=0, paper ="A4r", useDingbats=FALSE)




###########################################################################
#U5 parasite prevalence (microscopy) 
###########################################################################

## setting up the survey related variables and survey design object for U5 microscopy positive proportion
 
# 2010 PR BF DHS 
dhs10_hml32 <- dataclean.para(prdhs10, 'hml32', 'p_test', 1) 
hml32.svydesign <- svydesign.fun(dhs10_hml32)

# 2014 PR BF MIS
mis14_hml32<-dataclean.para(prmis14, 'hml32', 'p_test', 1)
hml32.svyd14<- svydesign.fun(mis14_hml32)



# 2017 PR BF MIS
mis17_hml32<-dataclean.para(prmis18, 'hml32', 'p_test', 1)
hml32.svyd17 <- svydesign.fun(mis17_hml32)


mis17_sb215<-dataclean.para(prmis18, 'sb215', 'p_testRDT', 1)
sb215.svyd17 <- svydesign.fun(mis17_sb215)

####################################################################################################
## Proportion of U5 +ve test results (PfPr) by health district and by cluster 
####################################################################################################
# 2010 PR BF MIS (microscopy)
ptest_10<-svyby(formula=~p_test, by=~NOMDEP, FUN=svymean, design=hml32.svydesign, na.rm=T)
head(ptest_10)

DS_shape_pfpr10 <- DSshape_sf%>%left_join(ptest_10) #2017 IR BF MIS (microscopy)
head(DS_shape_pfpr10)

BF2010para <- tmap.fun2(DSshape=DS_shape_pfpr10, "U5 malaria by Health Districts (2010)")



# 2014 PR BF MIS (microscopy)
ptest_14<-svyby(formula=~p_test, by=~NOMDEP, FUN=svymean, design=hml32.svyd14, na.rm=T)
head(ptest_10)

DS_shape_pfpr14 <- DSshape_sf%>%left_join(ptest_14) #2014 IR BF MIS (microscopy)
head(DS_shape_pfpr14)

BF2014para <- tmap.fun2(DSshape=DS_shape_pfpr14, "U5 malaria by Health Districts (2014)")



# 2017 PR BF MIS (microscopy)
ptest_17<-svyby(formula=~p_test, by=~NOMDEP, FUN=svymean, design=hml32.svyd17, na.rm=T)
head(ptest_17)

ptest_clu_17<-svyby(formula=~p_test, by=~v001, FUN=svymean, design=hml32.svyd17, na.rm=T)
head(ptest_clu_17)

DS_shape_pfpr17 <- DSshape_sf%>%left_join(ptest_17) #2017 IR BF MIS (microscopy)
head(DS_shape_pfpr17)

BF2017para <- tmap.fun2(DSshape=DS_shape_pfpr17, "U5 malaria by Health Districts (2017)")

all_para  <- tmap_arrange(BF2010para, BF2014para, BF2017para)

tmap_save(tm = all_para, filename = "all_para.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



# 2017 PR BF MIS (RDT)
ptest_17r<-svyby(formula=~p_testRDT, by=~NOMDEP, FUN=svymean, design=sb215.svyd17, na.rm=T)
head(ptest_17r)

ptest_clu_17r<-svyby(formula=~p_testRDT, by=~v001, FUN=svymean, design=sb215.svyd17, na.rm=T)
head(ptest_clu_17r)


##########################################################################################################
## Number of U5 children within each DS and cluster 
##########################################################################################################

# 2017 PR BF MIS (Microscopy)
ptest_num_17<-svyby(formula=~num_p, by=~NOMDEP, FUN=svytotal, design=hml32.svyd17, na.rm=T)%>% 
  dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_17)

ptest_num_clu_17<-svyby(formula=~num_p, by=~v001, FUN=svytotal, design=hml32.svyd17, na.rm=T)%>% 
  dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_clu_17)


# 2017 PR BF MIS (RDT)
ptest_num_17r<-svyby(formula=~num_p, by=~NOMDEP, FUN=svytotal, design=sb215.svyd17, na.rm=T)%>% 
  dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_17r)

ptest_num_clu_17r<-svyby(formula=~num_p, by=~v001, FUN=svytotal, design=sb215.svyd17, na.rm=T)%>% 
  dplyr:: select(-se)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_clu_17r)

##############################################################################################################
## merging the number of kids by DS to the estimates of PfPr by DS and cluster 
##############################################################################################################
# 2017 PR BF MIS (microscopy)
DS_merge_17<-ptest_17%>%left_join(ptest_num_17)%>%rename(PfPr = p_test,`Number of Kids` = num_p)
head(DS_merge_17)

clu_merge_17<-ptest_clu_17%>%left_join(ptest_num_clu_17)
head(clu_merge_17)

# 2017 PR BF MIS (RDT)
DS_merge_17r<-ptest_17r%>%left_join(ptest_num_17r)%>%rename(PfPr = p_testRDT,`Number of Kids` = num_p)
head(DS_merge_17r)

clu_merge_17r<-ptest_clu_17r%>%left_join(ptest_num_clu_17r)
head(clu_merge_17r)

#####################################################################################
## Parasitemia maps by DS independent of year 
#####################################################################################

#merging the health district parasitemia stats to the sf DS shape file 
DS_shape_pfpr17 <- DSshape_sf%>%left_join(DS_merge_17) #2017 IR BF MIS (microscopy)
head(DS_shape_pfpr17)

DS_shaper_pfpr17 <- DSshape_sf%>%left_join(DS_merge_17r) #2017 IR BF MIS (RDT)
head(DS_shaper_pfpr17)

st_write(DS_shape_pfpr17, "para2017_microscopy.csv")
st_write(DS_shaper_pfpr17, "para2017_RDT.csv")


#merging the points file to the points cluster values 

# 2017 IR BF MIS (microscopy)
pt_pfpr17 <- merge.rename.fun(clu_merge_17, pt_sf_17, "v001", "DHSCLUST", "num_p", "U5 in DHS clusters", "p_test", 
                              "PfPr by cluster")
head(pt_pfpr17)

# 2017 IR BF MIS (RDT)
pt_pfpr17r <- merge.rename.fun(clu_merge_17r, pt_sf_17, "v001", "DHSCLUST", "num_p", "U5 in DHS clusters", "p_testRDT", 
                              "PfPr by cluster")
head(pt_pfpr17r)


#2017 IR BF MIS 
para_2017_map <- tmap.fun1(DS_shape_pfpr17, DSmapvalue="PfPr", adminlegtitle="Malaria prevalence", 
                          main_title="Microscopy Malaria prevalence by Health Districts (2017)", text_title = "NOMDEP", 
                          ptsfile=pt_pfpr17, "U5 in DHS clusters", "PfPr by cluster") 


para_2017r_map <- tmap.fun1(DS_shaper_pfpr17, DSmapvalue="PfPr", adminlegtitle="Malaria prevalence", 
                           main_title="RDT Malaria prevalence by Health Districts (2017)", text_title = "NOMDEP", 
                           ptsfile=pt_pfpr17, "U5 in DHS clusters", "PfPr by cluster") 


tmap_save(tm = para_2017_map, filename = "para_map_2017.micro.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

tmap_save(tm = para_2017r_map, filename = "para_map_2017.RDT.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
####################################################################################################
## Proportion of U5 +ve test results (PfPr) by health district and timepoint (month and year)
####################################################################################################

# 2010 PR BF DHS 
ptest_10_ym<-svyby(formula=~p_test, by=~NOMDEP + time2, FUN=svymean, design=hml32.svydesign, na.rm=T)%>% 
                arrange(NOMDEP, time2)
head(ptest_10_ym)


# 2014 PR BF MIS 
ptest_14_ym<-svyby(formula=~p_test, by=~NOMDEP + time2, FUN=svymean, design=hml32.svyd14, na.rm=T)%>%
              arrange(NOMDEP, time2)
head(ptest_14_ym)


# 2017 PR BF MIS (microscopy)
ptest_17_ym<-svyby(formula=~p_test, by=~NOMDEP + time2, FUN=svymean, design=hml32.svyd17, na.rm=T)%>%
               arrange(NOMDEP, time2)
head(ptest_17_ym)


# 2017 PR BF MIS (RDT)
ptest_17_ymr<-svyby(formula=~p_testRDT, by=~NOMDEP + time2, FUN=svymean, design=sb215.svyd17, na.rm=T)%>%
               arrange(NOMDEP, time2)
head(ptest_17_ymr$time2)

##########################################################################################################
## Number of U5 children within each DS and by timepoint (month and year)
##########################################################################################################

# 2010 PR BF DHS 
ptest_num_10_ym<-svyby(formula=~num_p, by=~NOMDEP + time2, FUN=svytotal, design=hml32.svydesign, na.rm=T)%>% 
                 dplyr:: select(-se)%>%arrange(NOMDEP, time2)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_10_ym)


# 2014 PR BF MIS 
ptest_num_14_ym<-svyby(formula=~num_p, by=~NOMDEP + time2, FUN=svytotal, design=hml32.svyd14, na.rm=T)%>% 
                   dplyr:: select(-se)%>%arrange(NOMDEP, time2)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_14_ym)


# 2017 PR BF MIS (Microscopy)
ptest_num_17_ym<-svyby(formula=~num_p, by=~NOMDEP + time2, FUN=svytotal, design=hml32.svyd17, na.rm=T)%>% 
                  dplyr:: select(-se)%>%arrange(NOMDEP, time2)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_17_ym)


# 2017 PR BF MIS (RDT)
ptest_num_17_ymr<-svyby(formula=~num_p, by=~NOMDEP + time2, FUN=svytotal, design=sb215.svyd17, na.rm=T)%>% 
                   dplyr:: select(-se)%>%arrange(NOMDEP, time2)%>% mutate(num_p = round(num_p, 0))
head(ptest_num_17_ymr)



##############################################################################################################
## merging the number of kids by DS and timepoint (m&y) to the estimates of PfPr by DS and timepoint (m&y)
##############################################################################################################

# 2010 PR BF DHS
DS_merge_ym<-ptest_10_ym%>%left_join(ptest_num_10_ym)%>% rename(DS = NOMDEP, PfPr = p_test,`Number of Kids` = num_p)
head(DS_merge_ym)

# 2014 PR BF MIS
DS_merge_ym_14<-ptest_14_ym%>%left_join(ptest_num_14_ym)%>%rename(DS = NOMDEP, PfPr = p_test,`Number of Kids` = num_p)
head(DS_merge_ym_14)

# 2017 PR BF MIS (microscopy)
DS_merge_ym_17<-ptest_17_ym%>%left_join(ptest_num_17_ym)%>%rename(DS = NOMDEP, PfPr = p_test,`Number of Kids` = num_p)
head(DS_merge_ym_17)

# 2017 PR BF MIS (RDT)
DS_merge_ymr_17<-ptest_17_ymr%>%left_join(ptest_num_17_ymr)%>%rename(DS = NOMDEP, PfPr = p_testRDT,`Number of Kids` = num_p)
head(DS_merge_ym_17)


#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) 
write.csv(DS_merge_ym_17, file="pos_test_time_BF17DS_pfprmicro.csv") # 2017 PfPr PR micro BF DHS 
write.csv(DS_merge_ymr_17,file="pos_test_time_BF17DS_pfprRDT.csv") # 2017 PfPr RDT PR BF MIS




################################################################################################
## Treating the DHS as a random sample to estimate Pfpr for Banfora (substitute 2010, 2014 and 2017 data)

dhs <-  mis14_hml32%>% dplyr:: select(NOMDEP, time2, p_test, num_p)


dhs2 <-na.omit(dhs)%>%
  group_by(NOMDEP, time2)%>%
  summarise_each(funs(mean, sd, std.error, n()))

write.csv(dhs2, "parasitemia_SRS_14micro_ym.csv")
######################################################################################################


##########################################################################################
## Proportion of U5 children with +ve test results by cluster 
##########################################################################################

# 2010 PR BF DHS
ptest_clu_10_ym<-svyby(~p_test,~v001+time2, svymean, design=hml32.svydesign,na.rm=T)%>%rename(DHSCLUST = v001) 
head(ptest_clu_10_ym)

 
# 2014 PR BF MIS 
ptest_clu_14_ym<-svyby(~p_test,~v001+time2, svymean, design=hml32.svyd14,na.rm=T)%>%rename(DHSCLUST = v001) 
head(ptest_clu_14_ym)


# 2017 PR BF MIS (microscopy)
ptest_clu_17_ym<-svyby(~p_test,~v001+time2, svymean, design=hml32.svyd17,na.rm=T)%>%rename(DHSCLUST = v001) 
head(ptest_clu_17_ym)

# 2017 PR BF MIS (RDT)
ptest_clu_17_ymr<-svyby(~p_testRDT,~v001+time2, svymean, design=sb215.svyd17,na.rm=T)%>%rename(DHSCLUST = v001) 
head(ptest_clu_17_ymr)



########################################################################################
## Number of U5 children within each cluster and by timepoint (year and month)
########################################################################################

# 2010 PR BF DHS 
numclu_10_ym<-svyby(~num_p, ~v001 + time2, svytotal, design=hml32.svydesign, na.rm=T)%>% arrange(v001, time2)%>%
                      rename(standard_error = se,  DHSCLUST = v001, `U5 in DHS clusters` = num_p)
head(numclu_10_ym)


# 2014 PR BF MIS
numclut_14_ym<-svyby(~num_p,=~v001 + time2, svytotal, design=hml32.svyd14,na.rm=T)%>%arrange(v001, time2)%>%
                     rename(standard_error = se, DHSCLUST = v001, `U5 in DHS clusters` = num_p)
head(numclut_14_ym)


# 2017 PR BF MIS (microscopy)
numclut_17_ym<-svyby(~num_p,~v001 + time2, svytotal, design=hml32.svyd17,na.rm=T)%>%arrange(v001, time2)%>%
                  rename(standard_error = se, DHSCLUST = v001, `U5 in DHS clusters` = num_p)
head(numclut_17_ym)

# 2017 PR BF MIS (RDT)
numclut_17_ymr<-svyby(~num_p,~v001 + time2, svytotal, design=sb215.svyd17,na.rm=T)%>%arrange(v001, time2)%>%
  rename(standard_error = se, DHSCLUST = v001, `U5 in DHS clusters` = num_p)
head(numclut_17_ymr)


##################################################################################################################
#joining the pfpr by cluster + month dataset to that of number of children by cluster + month to create new table
##################################################################################################################
ptest_clu_all_10<-ptest_clu_10_ym%>%left_join(numclu_10_ym) #2010 PR BF DHS 
head(ptest_clu_all_10)

ptest_clu_all_14<-ptest_clu_14_ym%>%left_join(numclut_14_ym) #2014 PR BF MIS
head(ptest_clu_all_14)

ptest_clu_all_17<-ptest_clu_17_ym%>%left_join(numclut_17_ym) #2017 PR BF MIS (microscopy)
head(ptest_clu_all_17)

ptest_clu_all_17r<-ptest_clu_17_ymr%>%left_join(numclut_17_ymr) #2017 PR BF MIS (RDT)
head(ptest_clu_all_17r)


#creates csv file of PfPr by year and month of survey with the number of U5 surveyed                                 
write.csv(ptest_clu_all_10,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_timepoint/pos_test_time_BF10CL.csv")
write.csv(ptest_clu_all_14,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_timepoint/pos_test_time_BF14CL.csv")
write.csv(ptest_clu_all_17,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_timepoint/2017/pos_test_micro_BF17CL.csv")
write.csv(ptest_clu_all_17r,file="C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_timepoint/2017/pos_test_RDT_BF17CL.csv")



################################################
## Parasitemia maps 
#################################################

## Subsetting the DS level parasitemia estimates by month of survey for 2010 PR BF DHS

month_ptest_10 <- split(ptest_10_ym, ptest_10_ym$time2) #split the dataframe into a list 

dfList <- list() #create an empty list for holding the new list based on the joins 

#using a for loop to execute the left_join and enter the new objects in a list 
for (I in 1:length(month_ptest_10)) {dfname <-paste0("DS", names(month_ptest_10)[I])
   dfList[[dfname]] <- left_join(DSshape_sf, unique(month_ptest_10[[I]])) 
}

#releasing the newly created shape file by time into the environment 
list2env(dfList,.GlobalEnv)



## saving the 2010 PR BF DHS PfPr by month of survey maps in a list
list_DSshape<- list(`DS1-2010`, `DS4-2010`, `DS5-2010`, `DS6-2010`, `DS7-2010`,`DS8-2010`, `DS9-2010`, `DS10-2010`, `DS11-2010`, `DS12-2010`  )
list_title<- list("January 2010", "April 2010", "May 2010", "June 2010", "July 2010", "August 2010", "September 2010", "October 2010", "November 2010", 
                  "December 2010")

mapbymonth.list <- list()
mapbymonth.list <-map2(list_DSshape, list_title, tmap.fun2)
#plotting and printing can be accomplished by indexing the list 




## Subsetting the DS level parasitemia estimates by month of survey for 2014 PR BF DHS

month_ptest_14 <- split(ptest_14_ym, ptest_14_ym$time2)

dfList.2014 <- list() #create an empty list for holding the new list based on the joins 

#using a for loop to execute the left_join and enter the new objects in a list 
for (I in 1:length(month_ptest_14)) {dfname_14 <-paste0("DS", names(month_ptest_14)[I])
dfList.2014[[dfname_14]] <- left_join(DSshape_sf, unique(month_ptest_14[[I]])) 
}

list_title.14 <- list("October 2014", "November 2014", "December 2014", "September 2014")

mapbymonth14.list <- list()
mapbymonth14.list <- map2(dfList.2014, list_title.14, tmap.fun2)#applying the user-defined tmap.fun2 to lists 




## Subsetting the DS level parasitemia estimates by month of survey for 2017 (microscopy) PR BF DHS (microscopy and RDT maps are the same)

month_ptest_17 <- split(ptest_17_ym, ptest_17_ym$time2)

dfList.2017 <- list() #create an empty list for holding the new list based on the joins 

#using a for loop to execute the left_join and enter the new objects in a list 
for (I in 1:length(month_ptest_17)) {dfname_17 <-paste0("DS", names(month_ptest_17)[I])
dfList.2017[[dfname_17]] <- left_join(DSshape_sf, unique(month_ptest_17[[I]])) 
}

list_title.17 <- list("January 2018", "November 2017", "December 2017", "February 2018", "March 2018")

mapbymonth17.list <- list()
mapbymonth17.list <- map2(dfList.2017, list_title.17, tmap.fun2)#applying the user-defined tmap.fun2 to lists 




#saving map
tmap_save(tm = mapbymonth17.list[[5]], filename = "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/maps/burkina_dhs_mis_maps/pfpr/2017_2018/Mar_ptest_month_map_2017.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)

tmap_save(tm = ptest_month_14_map, filename = "outputs/ptest_month_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)


####################################################################
# Intermittent Residual Spraying (IRS)
####################################################################

## Setting up the survey related variables and survey design object for IRS

# 2010 HR BF DHS 
dhs10_hv253 <- hrdhs10 %>%mutate(wt=hv005/1000000,strat=hv022,id=hv021, num_HH=1)%>%rename(IRS = hv253)%>%filter(!is.na(wt))


hv253.svydesign <- svydesign(id= ~id, strata=~strat,nest=T, weights= ~wt, data=dhs10_hv253)


# 2014 HR BF MIS 
mis14_hv253 <- hrmis14 %>%mutate(wt=hv005/1000000,strat=hv022,id=hv021, num_HH=1)%>% rename(IRS = hv253)%>%filter(!is.na(wt))



hv253.svyd14 <- svydesign(id= ~id,strata=~strat,nest=T,weights= ~wt, data=mis14_hv253)


################################################################
## Proportion of households that got IRS by health district 
################################################################
# 2010 HR BF DHS 
IRS <-svyby(~IRS,~NOMDEP, svymean, design=hv253.svydesign, na.rm=T)
head(IRS)

# 2014 HR BF MIS 
IRS_14<-svyby(~IRS,~NOMDEP, svymean, design=hv253.svyd14, na.rm=T)
head(IRS_14)


##################################################
## Number of HH within each health district
##################################################

# 2010 HR BF DHS 
num_DS_10<-svyby(~num_HH,~NOMDEP,svytotal,design=hv253.svydesign, na.rm=T) %>% rename(standard_error = se) %>% mutate(num_HH = round(num_HH, 0))
head(num_DS_10)


# 2014 HR BF MIS
num_DS_14<-svyby(~num_HH,~NOMDEP,FUN=svytotal, design=hv253.svyd14, na.rm=T) %>% rename(standard_error = se) %>% mutate(num_HH = round(num_HH, 0))
head(num_DS_14)


##################################################
## Number of HH within each cluster 
#################################################

# 2010 HR BF DHS 
num_clu_10<-svyby(~num_HH,~v001,svytotal, design=hv253.svydesign,na.rm=T)%>% mutate(num_HH = round(num_HH, 0))%>%rename(DHSCLUST = v001)
head(num_clu_10)


# 2014 HR BF MIS
num_clu_14<-svyby(~num_HH,~v001, svytotal, design=hv253.svyd14,na.rm=T)%>% mutate(num_HH = round(num_HH, 0))%>%rename(DHSCLUST = v001)
head(num_clu_14)


## Proportion of HH within each cluster

# 2010 HR BF DHS
IRS_clu_10<-svyby(~IRS,~v001, svytotal, design=hv253.svydesign, na.rm=T)%>%rename(DHSCLUST = v001)
colnames(IRS_clu_10)[3]<- "standard_error"
head(IRS_clu_10)
 

# 2014 HR BF MIS
IRS_clu_14<-svyby(~IRS,~v001, svytotal, design=hv253.svyd14,na.rm=T)%>%rename(DHSCLUST = v001)
colnames(IRS_clu_14)[3]
head(IRS_clu_14)


#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################

# merging the DS level estimates with the full list of health districts
# 2010 HR BF DHS

DS_IRS_10<-DSshape_sf%>%left_join(IRS)%>%left_join(num_DS_10)%>% rename(`Number of HH` = num_HH)    
head(DS_IRS_10)

# 2014 HR BF MIS
DS_IRS_14 <- DSshape_sf%>%left_join(IRS_14)%>%left_join(num_DS_14)%>%rename(`Number of HH` = num_HH) 
head(DS_IRS_14)

# cluster estimates are merged with the cluster shapefile 
# 2010 HR BF DHS 
pts_est_IRS<- pt_sf_10%>%left_join(IRS_clu_10)%>%left_join(num_clu_10)%>%rename(`HH in DHS clusters`=num_HH) %>%rename(`HH sprayed with IRS by cluster`= IRS)
head(pts_est_IRS)

# 2014 HR BF MIS
pts_est_IRS_14<-pt_sf_14%>%left_join(IRS_clu_14)%>%left_join(num_clu_14)%>%rename(`HH in DHS clusters`=num_HH)%>%rename(`HH sprayed with IRS by cluster`= IRS)
head(pts_est_IRS_14)

#creates a csv file of the health district estimates 
st_write(DS_IRS_10,"outputs/IRS_BF10DS.csv") #2010 HR BF DHS
st_write(DS_IRS_14,"outputs/IRS_BF14DS.csv") #2014 HR BF MIS

#creates shp file of the cluster-level estimates 
st_write(pts_est_IRS, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IRS_2010/IRS_2010_cluster.shp")
st_write(pts_est_IRS_14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/IRS_2014/IRS_2014_cluster.shp")


## IRS maps 

#2010 HR BF DHS
IRS_2010_map <- tmap.fun4(DS_IRS_10,"Proportion of HH that received IRS by Health Districts (2010)","Proportion of HH that received IRS", "IRS")



#2014 HR BF MIS
IRS_2010_map <- tmap.fun4(DS_IRS_14,"Proportion of HH that received IRS by Health Districts (2014)","Proportion of HH that received IRS", "IRS")


#print maps 
IRS_2010_map
IRS_2014_map 

#saving map
tmap_save(tm = IRS_2010_map, filename = "outputs/IRS_map_2010.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)
tmap_save(tm = IRS_2014_map, filename = "outputs/IRS_map_2014.pdf", 
          width=13, height=13, units ="in", asp=0,paper ="A4r", useDingbats=FALSE)



# mutate(gateway, YearlyHit = case_when(Frequency == 'Year' ~ 1,
#                                       Frequency == 'Month' ~ 12,
#                                       Frequency == 'Week' ~ 48)
# )
