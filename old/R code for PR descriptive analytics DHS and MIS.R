#############################################################################################
####
#### Code for exploration and descriptive analysis of malaria-related variables in the DHS/MIS Household
#### recode files in Nigeria
#### 
#### Last Updated: August 28, 2019
####
#### Author:Ifeoma Ozodiegwu
##### Estimated variables: 

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
library(plyr);library(tidyverse);library(survey);library(haven);library(ggplot2); library(purrr);
library(summarytools); library(stringr); library(sp); library(rgdal); library(raster); 
library(lubridate);library(RColorBrewer); library(plotrix); library(ggrepel)
library(sf);library(shinyjs);library(tmap); library(knitr); library(scatterpie); library(maptools); 
options(survey.lonely.psu="adjust") # an important option for LGAs with only one cluster

####capabilities() #use this in mac to troubleshoot summarytools 

###removes the plyr package that was attached earlier.dplyr does not work well when plyr is attached.  
#detach("package:plyr", unload=TRUE)

###########################################################
#Reading all Burkina individual recode data DHS, MIS data#
###########################################################
####this reads all the dta files with a BFPR anywhere in their name in the main 
####and subdirectories in the working directorey
in_files <- list.files(pattern = ".*BFPR.*\\.DTA", recursive = TRUE)

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
bfprdhs93 <- `BF_1993_DHS_06192019/BFPR21DT/BFPR21FL`
bfprdhs98 <- `BF_1998-99_DHS_06192019/BFPR31DT/BFPR31FL`
bfprdhs03 <- `BF_2003_DHS_06192019/BFPR44DT/BFPR44FL`
bfprdhs10 <- `BF_2010_DHS_06192019/BFPR62DT/BFPR62FL`
bfprmis14 <- `BF_2014_MIS_06192019/BFPR70DT/BFPR70FL`
bfprmis17 <- `BF_2017-18_MIS_07252019_1531_86355/BFPR7ADT/BFPR7AFL`


###########################################################################################
#---We read in the GPS points and shapefiles that will used in subsequent analysis---#
###########################################################################################
####GPS points 
pts_10<- readOGR("BF_2010_DHS_06192019/BFGE61FL", layer = "BFGE61FL") #this is the 2010 BF 
pts_14 <- readOGR("BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL") #this is 2014 BF 

####shapefiles for the health districts 
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

####we convert the sp objects to sf object for easy plotting with tmap later 
pt_sf_10 <-st_as_sf(pts_10)
pt_sf_14 <-st_as_sf(pts_14)
DSshape_sf<-st_as_sf(DS_shape)


####Adding a row number to the DS_shape object and the pts data, will be handy for plotting later on
DS_shape@data$row_num<-1:nrow(DS_shape@data)
pts_10@data$row_num <- 1:nrow(pts_10@data)
pts_14@data$row_num <- 1:nrow(pts_14@data)


###################################################################
#---Quality checks and reading in new columns---#
##################################################################
#### DS is in the utm + zone 30 & points is in long + lat. lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,crs(pts_10))

####check map projections again 
crs(DS_shape_W)
crs(pts_10)
crs(pts_14)
####everything looks good now 

#### Check if the GPS points  are contained in the polygons 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2010 clusters')
plot(pts_10,add=T,col=4) #this dhs 2010 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with MIS 2014 clusters')
plot(pts_14,add=T,col=4) #this is mis 2014  
#### everything looks good 

# Now add name of the individual regions to the map to cross-check which admin areas have no clusters
text(DS_shape_W, DS_shape_W@data$NOMDEP, cex=0.75)

####set up an empty dataframe for the health district estimates. 
####this way there will be a row in the data for the health districts with no clusters
DS_merge<-data.frame(NOMDEP=DS_shape_W@data[,"NOMDEP"])

####this creates a dataframe with two cluster variables, one named v001 and another DHSCLUST for merging later 
pts_10_merge<-data.frame(v001=pts_10@data$DHSCLUST, DHSCLUST=pts_10@data$DHSCLUST) # this is dhs 2010 
pts_14_merge<-data.frame(v001=pts_14@data$DHSCLUST, DHSCLUST=pts_14@data$DHSCLUST) #this is dhs 2014 


###################################################################
#---mapping points to inherit associated health district---#
##################################################################
####if pts@data is 573 by 21 and district is 70 by 11, then the corresponding data frame will be 573 by 11
dim(pts_10@data)  # this is for dhs 2010  
dim(DS_shape_W@data)
key_10<-over(SpatialPoints(coordinates(pts_10),proj4string = pts_10@proj4string), DS_shape_W)
dim(key_10)
length(unique(key_10$NOMDEP))

# add in the cluster variable
key_10$v001<-pts_10@data[,"DHSCLUST"]

####the dimensions of the pts data is 252 by 21 and of the DS is 70 by 11, key_14 will be 252 by 11 
dim(pts_14@data) # this is for mis 14 
dim(DS_shape_W@data)
key_14<-over(SpatialPoints(coordinates(pts_14),proj4string = pts_14@proj4string), DS_shape_W)
dim(key_14)
length(unique(key_14$NOMDEP))

# add in the cluster variable
key_14$v001<-pts_14@data[,"DHSCLUST"]


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
bfprdhs10 <- bfprdhs10%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% 
              dplyr::rename(v001 = hv001) %>%
              mutate(YYYY = (((hv008 - 1)/12)+1900))%>% 
              mutate(YYYY = round(YYYY, 0))%>%
              mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
              mutate(time2 = str_c(MM, YYYY, sep = '-'))



bfprmis14 <- bfprmis14%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% 
              dplyr::rename(v001 = hv001)%>%
              mutate(YYYY = (((hv008 - 1)/12)+1900))%>% 
              mutate(YYYY = round(YYYY, 0))%>%
              mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
              mutate(time2 = str_c(MM, YYYY, sep = '-'))



####Applying the function to each dataset 
###We start with the 2010 and 2014 BF MIS dataset
prdhs10 <- fun1(bfprdhs10, 'hv227', 'hv253', 'hml', 'MM')
prmis14 <- fun1(bfprmis14, 'hv227', 'hv253', 'hml', 'sh125', 'sh13', 'sh14', 'sh22', 'sh21', 'MM')

###creating summary table with summary tools 
view(dfSummary(prdhs10))
view(dfSummary(prmis14))

###################################################################
#---Recoding variables---# 
###################################################################
####first we establish a recoder to recode variables using a function. We want to recode the smear test results
#### from anything other than 0 & 1 to NA. 3 = unreadable, 4 = not present, 5 = refused, 6 = others, 7 = test undetermined, 8 = sample not found in lab database     
#### for the rapid test, 3 = absent, 4 = refuse, 6 = others, 9 = missing 
recoder <- function(x){
  ifelse(x == 3| x == 4 | x == 5 | x == 6 | x == 7 | x == 8| x == 9, NA,ifelse(x == 0, 0, 1))
}

####this is for the 2010 dataset 
prdhs10 <- bfprdhs10%>%
  mutate_at(vars(contains('hml35')), recoder) #recodes for rapid test results 
  


####this is for the 2014 dataset 
prmis14 <- bfprmis14%>% 
  mutate_at(vars(contains('hml32')), recoder)%>% #recodes for smear test results  
mutate_at(vars(contains('hml35')), recoder) #recodes for rapid test results  
####this is the dataset for estimating prevalence of malaria parasitemia 


################################################################################################
# --Now let's join the DHS/MIS data to the key that contains the names of each health district-- #
################################################################################################
####a few descriptive stats for dhs 2010 
dim(prdhs10)# how many rows?
dim(key_10) #how many columns?
summary(prdhs10$v001)# the cluster variable
summary(key_10$v001)
length(unique(prdhs10$v001))
length(unique(key_10$v001)) # it looks like the cluster variables match!
prdhs10<-prdhs10%>%left_join(key_10)

summary(prdhs10$hc1)##checking age of child in months variable 

####should still have the same number of rows and a few new columns
dim(prdhs10) 
####this is the dataset that will be used for subsequent 2010 computations that don't require long format 


####a few descriptive stats for mis 2014 
dim(prmis14)# how many rows?
dim(key_14) #how many columns?
summary(prmis14$v001)# the cluster variable
summary(key_14$v001)
length(unique(prmis14$v001))
length(unique(key_14$v001)) # it looks like the cluster variables match!
prmis14<-prmis14%>%left_join(key_14)

####should still have the same number of rows and a few new columns
dim(prmis14) 
####this is the dataset that will be used for subsequent 2014 computations that don't require long format 

####checking the number of NAs and numbers in the variable 
summary(is.na(prmis14$hc1))




################################################################################################
#2. In this step we start using the created datasets in the computations and mapping 
################################################################################################


##############################################################################################################
# -- setting up the survey related variables and survey design object for U5 microscopy positive proportion -- #
###############################################################################################################
#### We start with the 2010 DHS PR dataset 
dhs10_hml32 <- prdhs10 %>%mutate(wt=hv005/1000000,strat=hv022,
         id=hv021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
        rename(p_test = hml32)%>%
        filter(!is.na(wt))%>% 
       filter(!is.na(hc1)) #we want to restrict to children 


hml32.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                          weights= ~wt, data=dhs10_hml32)


#### This is for 2014 MIS PR dataset 
mis14_hml32 <- prmis14 %>%mutate(wt=hv005/1000000,strat=hv022,
                                 id=hv021, num_kids=1)%>% #column for the number of kids so that we can compute the totals by cluster 
                                 rename(p_test = hml32)%>%
                                filter(!is.na(wt))%>% 
                                filter(!is.na(hc1)) #we want to restrict to children 


hml32.svyd14 <- svydesign(id= ~id,
                             strata=~strat,nest=T, 
                             weights= ~wt, data=mis14_hml32)


########################################################################################
# -- Proportion of U5 +ve test results by health district and timepoint-- #
########################################################################################
####This calculates the proportion of children with +ve test results by health district for 2010 BF DHS 
system.time({
  p_test_10<-svyby(formula=~p_test,# which variable do we want estimate
                by=~NOMDEP + timepoint, # by which variable
                FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                design=hml32.svydesign, # my svy design object
                na.rm=T)
})

####sorting the results 
p_test_10 <- arrange(p_test_10, NOMDEP, timepoint)


####This computes the proportion of children with +ve test results by health district and timepoint for 2014 BF MIS 
system.time({
  p_test_14<-svyby(formula=~p_test,# which variable do we want estimate
                   by=~NOMDEP + timepoint, # by which variable
                   FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                   design=hml32.svyd14, # my svy design object
                   na.rm=T)
}) # this one takes 25 seconds on my desktop


####sorting the results 
p_test_14 <- arrange(p_test_14, NOMDEP, timepoint)

#########################################################################
# -- Number of U5 children within each DS and by timepoint -- #
#########################################################################
####This computes the total number of under-five children within each surveyed cluster for 2010 BF DHS 
system.time({
  num_DS_10<-svyby(formula=~num_kids,# which variable do we want estimate
                 by=~NOMDEP + timepoint, # by which variable
                 FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                 design=hml32.svydesign, # my svy design object
                 na.rm=T)
}) # this one takes 73 seconds on my desktop

####data cleaning 
num_DS_10 <- num_DS_10%>% dplyr:: select(NOMDEP, timepoint, num_kids)%>%
                    arrange(NOMDEP, timepoint)%>%
                   mutate(num_kids = round(num_kids, 0))
         
####joining the number of kids by DS and timepoint to the estimates of PfPr by DS and timepoint
DS_merge_time<-p_test_10%>%left_join(num_DS_10)

DS_merge_time <- DS_merge_time %>% rename(DS = NOMDEP, `timepoint(mm/dd/yyyy)` = timepoint, PfPr = p_test,
                                          `Number of Kids` = num_kids)

####writing to my outputs                                   
write.csv(DS_merge_time,file="outputs/pos_test_time_BF10DS.csv")
  
####This computes the total number of U5 children within each surveyed cluster for 2014 BF MIS 
system.time({
  num_DS_14<-svyby(formula=~num_kids,# which variable do we want estimate
                    by=~NOMDEP + timepoint, # by which variable
                    FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                    design=hml32.svyd14, # my svy design object
                    na.rm=T)
}) # this one takes 73 seconds on my desktop

####data cleaning 
num_DS_14 <- num_DS_14%>% dplyr:: select(NOMDEP, timepoint, num_kids)%>%
  arrange(NOMDEP, timepoint)%>%
  mutate(num_kids = round(num_kids, 0))

####joining the number of kids by DS and timepoint to the estimates of PfPr by DS and timepoint
DS_merge_time_14<-p_test_14%>%left_join(num_DS_14)

DS_merge_time_14 <- DS_merge_time_14 %>% rename(DS = NOMDEP, `timepoint(mm/dd/yyyy)` = timepoint, PfPr = p_test,
                                          `Number of Kids` = num_kids)

####writing to my outputs                                   
write.csv(DS_merge_time_14,file="outputs/pos_test_time_BF14DS.csv")


#####################################################################################
# -- Proportion of U5 children with +ve test results by cluster + timepoint -- #
####################################################################################
####This computes the proportion of U5 children with positive test results by surveyed cluster for 2010 BF DHS 
system.time({
  ptest_clu_10<-svyby(formula=~p_test,# which variable do we want estimate
                    by=~v001 + time2, # by which variable
                    FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                    design=hml32.svydesign, # my svy design object
                    na.rm=T)
  
}) # this one takes 67 seconds on my desktop

ptest_clu_10 <- arrange(ptest_clu_10, v001, time2)




####This computes the proportion of U5 children with positive test results by surveyed cluster for 2014 BF MIS 
system.time({
  ptest_clu_14<-svyby(formula=~p_test,# which variable do we want estimate
                       by=~v001 + time2, # by which variable
                       FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                       design=hml32.svyd14, # my svy design object
                       na.rm=T)
  
}) # this one takes 67 seconds on my desktop

ptest_clu_14 <- arrange(ptest_clu_14, v001, time2)



#########################################################################
# -- Number of U5 children within each cluster and by timepoint -- #
#########################################################################
####This computes the total number of under-five children within each surveyed cluster by timepoint for 2010 BF DHS 
system.time({
  num_clut_10<-svyby(formula=~num_kids,# which variable do we want estimate
                   by=~v001 + time2, # by which variable
                   FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                   design=hml32.svydesign, # my svy design object
                   na.rm=T)
}) # this one takes 73 seconds on my desktop

num_clut_10<- num_clut_10%>%arrange(v001, time2)%>%
  mutate(num_kids = round(num_kids, 0))%>% 
  rename(standard_error = se)


####joining the cluster and month dataset by to that of timepoint to create new table
ptest_all_10<-ptest_clu_10%>%left_join(num_clut_10)


####writing to my outputs                                   
write.csv(ptest_all_10,file="outputs/pos_test_time_BF10CL.csv")


####This computes the total number of under-five children within each surveyed cluster by timepoint for 2010 BF DHS 
system.time({
  num_clut_14<-svyby(formula=~num_kids,# which variable do we want estimate
                     by=~v001 + time2, # by which variable
                     FUN=svytotal, # what is the function (svytotal estimates totals and variance from complex surveys)
                     design=hml32.svyd14, # my svy design object
                     na.rm=T)
}) # this one takes 73 seconds on my desktop

num_clut_14<- num_clut_14%>%arrange(v001, time2)%>%
  mutate(num_kids = round(num_kids, 0))%>% 
  rename(standard_error = se)


####joining the cluster and month dataset by to that of timepoint to create new table
ptest_all_14<-ptest_clu_14%>%left_join(num_clut_14)


####writing to my outputs                                   
write.csv(ptest_all_14,file="outputs/pos_test_time_BF14CL.csv")



#########################################################################
# -- A few more steps to clean up the datasets  -- #
#########################################################################
####renaming 'se' colname in ptest_clu_10 to standard error in preparation for the joining procedure 
colnames(ptest_clu_10)[3]<- "standard_error" #for 2010 dataset 

####renaming 'se' colname in ptest_clu_14 to standard error in preparation for the joining procedure 
colnames(ptest_clu_14)[3]<- "standard_error" #for 2014 dataset 


####we want to merge with the full list of health districts
#we start with the proportion of children that slept under a bednet by health district for the 2010 BF MIS 
dim(DS_merge)
DS_merge_10<-DS_merge%>%left_join(p_test_10)
DS_merge_14<-DS_merge%>%left_join(p_test_14) #next the 2014 BF MIS


####next we merge the number of under-five children within each surveyed cluster  
####with the number of under-five children that slept under a bednet by surveyed cluster
#this is for 2010 
pts_est_10<-num_clu_10%>%left_join(ptest_clu_10) 
pts_merge_10<-pts_10_merge%>%left_join(pts_est_10)%>%
  mutate(DHSCLUST = pts_10_merge$v001)%>%
  mutate(`U5 in DHS clusters` = round(num_kids, 0))


#this is for 2014
pts_est_14<-num_clu_14%>%left_join(ptest_clu_14) 
pts_merge_14<-pts_14_merge%>%left_join(pts_est_14)%>%
  mutate(DHSCLUST = pts_14_merge$v001)%>%
  mutate(`U5 in DHS clusters` = round(num_kids, 0))


####creates a csv file of the proportion of children that have a positive test result by health district 
write.csv(DS_merge_10,file="outputs/pos_test_BF10DS.csv") #for the 2010 BF DHS
write.csv(DS_merge_14,file="outputs/pos_test_BF14DS.csv") #for the 2014 BF MIS


#####################################
# -- Now let's map our findings -- #
#####################################
####merging the test result stats to the sf DS shape file created earlier 
DS_shape_10 <- merge(DSshape_sf, DS_merge_10, by = "NOMDEP") #this is for 2010 BF DHS 
DS_shape_14 <- merge(DSshape_sf, DS_merge_14, by = "NOMDEP") #this is for 2014 BF MIS 

####merging the tve test to the points sf file created earlier 
pt_10 <- merge(pt_sf_10,pts_merge_10, by = "DHSCLUST")%>% #for 2010 BF DHS 
  mutate(`positive malaria test results by cluster`=p_test)
pt_14 <- merge(pt_sf_14,pts_merge_14, by = "DHSCLUST")%>% #for 2014 BF MIS 
  mutate(`positive malaria test results by cluster`=p_test)


st_write(pt_10, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_10/pos_test_2010_cluster.shp")
st_write(pt_14, "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/cluster_values/Parasitemia_14/pos_test_2014_cluster.shp")



#this is for 2010 BF DHS 
ptest_2010_map <- tm_shape(DS_shape_10) + #this is the health district shapfile with test result info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Prevalence of malaria parasitemia among U5 by Health Districts (2010)",
            main.title.position = c("center", "top"), aes.palette = list(seq = "-RdYlBu"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_10)+ #this is the points shape file with test result and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "positive malaria test results by cluster", 
             border.col= "black", palette="seq",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_layout(aes.palette = list(seq = "-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")

#this is for 2014 BF 
ptest_2014_map <- tm_shape(DS_shape_14) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "p_test", textNA = "No data", 
              title = "Prevalence of malaria parasitemia", palette = "seq", breaks=c(0, 0.2, 0.3, 
                                                                          0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0))+
  tm_layout(main.title="Prevalence of malaria parasitemia among U5 by Health Districts (2014)",
            main.title.position = c("center", "top"), aes.palette = list(seq = "-RdYlBu"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_14)+ #this is the points shape file with test result and number of kids info by cluster 
  tm_bubbles(size="U5 in DHS clusters", col = "positive malaria test results by cluster", 
             border.col= "black", palette="seq",
             breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=F)+
  tm_layout(aes.palette = list(seq = "-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")


####print map 
ptest_2010_map
ptest_2014_map


####save maps 
tmap_save(tm = ptest_2010_map, filename = "outputs/ptest_map_2010.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")
tmap_save(tm = ptest_2014_map, filename = "outputs/ptest_map_2014.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r")


#####################################################################################################
#---Mapping the month of the survey---#
#########################################################################################################
####First we create column names and columns for cluster number and month of survey for the 2010 BF MIS  
oldnames=c('1', '4', '5', '6', '7', '8', '9', '10', '11', '12')
newnames=c('January', 'April', 'May', 'June','July', 'August', 'September', 'October', 'November', 'December')



dhs10_MM <- prdhs10 %>% dplyr:: select(v001, MM, hc1)%>%
  filter(!is.na(hc1))%>%
  dplyr:: select(v001, MM)   %>%
  group_by(v001, MM) %>%
  summarise(n=n())%>%
  spread(MM, n) %>%
  rename_at(vars(oldnames), ~newnames)%>%
  rename(DHSCLUST = v001)%>% 
  rowwise() %>% 
   mutate(total = sum(January, April, May, June, July, August, September, October,November, December, na.rm=T))


####Fill NA values with 0
dhs10_MM[is.na(dhs10_MM)] <- 0

####We merge the new columns of indicating the number of individuals surveyed by cluster and the cluster survey month
pt_sf_MM <- merge(pt_sf_10,dhs10_MM, by = "DHSCLUST") #this is for 2010 BF MIS 


####generating the values to increase the bounding box 
bbox_new <- st_bbox(DSshape_sf)
xrange <- bbox_new$xmax - bbox_new$xmin
yrange <- bbox_new$ymax - bbox_new$ymin
bbox_new[3] <- bbox_new[3] + (0.2 * xrange)
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

month_map <- tm_shape(DSshape_sf, bbox = bbox_new) +#this is the health district shapfile 
  tm_polygons()+
  tm_layout(main.title="Survey month and number of U5 in each surveyed cluster (BF 2010 DHS)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_sf_MM)+ #this is the points shape file with month of survey info 
  tm_bubbles(size="January", col = "blue")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "April", col = "red", sizes.legend =c(1, 2))+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "May", col = "yellow")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "June", col = "green")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "July", col = "orange")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "August", col = "purple")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "September", col = "pink")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "October", col = "khaki")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "November", col = "coral")+
  tm_shape(pt_sf_MM)+
  tm_bubbles(size = "December", col = "cyan")+
  tm_legend(legend.title.size = 0.7, legend.position= c("right", "top"))

  
####print map 
month_map


####save maps 
tmap_save(tm = month_map, filename = "outputs/survey_month_2010.pdf", width=15, height=13, units ="in", asp=0,
          paper ="A4r")


###############################################################################################################

####Next we create column names and columns for cluster number and month of survey for the 2014 BF MIS  
oldnames=c('9', '10', '11', '12')
newnames=c('September', 'October', 'November', 'December')

mis14_MM <- prmis14 %>% dplyr:: select(v001, MM, hc1)%>%
  filter(!is.na(hc1))%>%
  dplyr:: select(v001, MM)   %>%
  group_by(v001, MM) %>%
  summarise(n=n())%>%
  spread(MM, n) %>%
  rename_at(vars(oldnames), ~newnames)%>%
  rename(DHSCLUST = v001)%>% 
  rowwise() 


####Fill NA values with 0
mis14_MM[is.na(mis14_MM)] <- 0

####We merge the new columns of indicating the number of individuals surveyed by cluster and the cluster survey month
pt_sf_MM_14 <- merge(pt_sf_14,mis14_MM, by = "DHSCLUST") #this is for 2014 BF MIS 


####generating the values to increase the bounding box 
bbox_new <- st_bbox(DSshape_sf)
xrange <- bbox_new$xmax - bbox_new$xmin
yrange <- bbox_new$ymax - bbox_new$ymin
bbox_new[3] <- bbox_new[3] + (0.2 * xrange)
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

month_map_14 <- tm_shape(DSshape_sf, bbox = bbox_new) +#this is the health district shapfile
  tm_polygons()+
  tm_layout(main.title="Survey month and number of U5 in each surveyed cluster (BF 2014 MIS)",
            main.title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ #this is the points shape file with month of survey info
  tm_shape(pt_sf_MM_14)+
  tm_bubbles(size = "September", col = "pink")+
  tm_shape(pt_sf_MM_14)+
  tm_bubbles(size = "October", col = "khaki")+
  tm_shape(pt_sf_MM_14)+
  tm_bubbles(size = "November", col = "coral")+
  tm_shape(pt_sf_MM_14)+
  tm_bubbles(size = "December", col = "cyan", sizes.legend =c(1, 2))+
  tm_legend(legend.title.size = 0.7, legend.position= c("right", "top"))


####print map 
month_map_14


####save maps 
tmap_save(tm = month_map_14, filename = "outputs/survey_month_2014.pdf", width=15, height=13, units ="in", asp=0,
          paper ="A4r")









#