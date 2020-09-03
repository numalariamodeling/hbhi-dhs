#############################################################################################
#### Scripts for exploration and descriptive analysis of malaria-related variables in the DHS/MIS Indivdual, household and person recode files in Nigeria 
#### These scripts generates  maps and survey-adjusted descriptive statistics for the following malaria-related variables 
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
#### Last Updated: October 31, 2019
####
#### Author:Ifeoma Ozodiegwu
##### 
##############################################################################################################

#############################################################################################################
# ---- cleaning the global environment, setting the working environment and loading libraries --- #
#############################################################################################################
rm(list=ls())

setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs") 

#important to download the github version of tidyverse.Uncomment the script below to run
# install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("hadley/tidyverse")


## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer", "plotrix", "ggrepel", "sf", "shinyjs", "tmap", "knitr", "labelled", "rlang", "arules", "foreign")
lapply(x, library, character.only = TRUE) #applying the library function to packages
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

source("Nigeria functions.R")

##############################################################################################################
# --- reading and cleaning the datasets --- #
##############################################################################################################

# DHS datasets 
NGAfiles<-read.files(".*NGIR.*\\.DTA", ".*NGKR.*\\.DTA", ".*NGPR.*\\.DTA")
NGAfiles_2<-read.files(".*NGIR.*\\.DTA", ".*NGKR.*\\.DTA", ".*NGHR.*\\.DTA")

# cluster locations 
NGAshpfiles <- list.files(pattern="*FL.*\\.shp$", full.names=TRUE, recursive=T)
NGAshplist<-sapply(NGAshpfiles,shapefile, simplify = F)


# LGA shape file 
LGAshp <- readOGR("Nigeria LGAs shapefile 191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
LGAshp_sf <- st_as_sf(LGAshp)

#checking to see if cluster points into admin boundary 
raw.plot.fun(LGAshp, NGAshplist[[1]], "1990 Admin boundary")
raw.plot.fun(LGAshp, NGAshplist[[2]], "2003 Admin boundary")
raw.plot.fun(LGAshp, NGAshplist[[3]], "2008 Admin boundary")
raw.plot.fun(LGAshp, NGAshplist[[4]], "2010 Admin boundary")
raw.plot.fun(LGAshp, NGAshplist[[5]], "2013 Admin boundary")
raw.plot.fun(LGAshp, NGAshplist[[6]], "2015 Admin boundary")

# code to search through labels for specific names 
look_for(NGAfiles[[16]], "ml101")

# check <- NGAfiles[[18]] %>% dplyr::select(sh13, hv105, sh225, sh14, sh15)
# 
# tail(check)

table(NGAfiles[[18]][, "sh225"])

table(NGAfiles[[17]][, "ml13e"])
############################################################################################################
# Creating file list for analysis  
############################################################################################################
# creating list of files with Pfpr data for 2010 and 2015 
pfpr.list <- list(NGAfiles[[12]], NGAfiles[[18]])

#creating list of files for health seeking for 2010 and 2015 
hspr.list <- pfpr.list


# creating a variable that aggregates all U5 private health care seeking behavior 
match("h32j",names(NGAfiles[[17]])) # identifying column indexes 


NGAfiles[[2]]$pri_med <- rowSums(NGAfiles[[2]][, c(485:488, 490:493)], na.rm=T) # 1990 


NGAfiles[[5]]$pri_med <- rowSums(NGAfiles[[5]][, c(702:705, 707:710)], na.rm=T) #2003 
NGAfiles[[5]][,"pri_med"] <-recoder(NGAfiles[[5]][,"pri_med"]) 



NGAfiles[[8]]$pri_med <- rowSums(NGAfiles[[8]][, c(746:748, 752:753)], na.rm=T) #2008 
NGAfiles[[8]][,"pri_med"] <-recoder(NGAfiles[[8]][,"pri_med"]) 



NGAfiles[[11]]$pri_med <- rowSums(NGAfiles[[11]][, c(160:162, 164:168)], na.rm=T) #2010
NGAfiles[[11]][,"pri_med"] <-recoder(NGAfiles[[11]][,"pri_med"]) 



NGAfiles[[14]]$pri_med <- rowSums(NGAfiles[[14]][, c(758:760, 762, 764:766)], na.rm=T) #2013
NGAfiles[[14]][,"pri_med"] <-recoder(NGAfiles[[14]][,"pri_med"]) 
table(NGAfiles[[14]]$pri_med)


NGAfiles[[17]]$pri_med <- rowSums(NGAfiles[[17]][, c(572:574, 762, 576:580)], na.rm=T) #2015
NGAfiles[[17]][,"pri_med"] <-recoder(NGAfiles[[17]][,"pri_med"]) 
table(NGAfiles[[17]]$pri_med)



# creating a variable for medical treatment for fever 
match("h32a",names(NGAfiles[[17]]))

NGAfiles[[2]]$med_fever <- rowSums(NGAfiles[[2]][, c(476:493)], na.rm=T) # 1990 
table(NGAfiles[[2]]$med_fever)


NGAfiles[[5]]$med_fever <- rowSums(NGAfiles[[5]][, c(693:710)], na.rm=T) # 2003
NGAfiles[[5]][,"med_fever"] <-recoder(NGAfiles[[5]][,"med_fever"]) 
table(NGAfiles[[5]]$med_fever)


NGAfiles[[8]]$med_fever <- rowSums(NGAfiles[[8]][, c(737:750, 752:754)], na.rm=T) # 2008
NGAfiles[[8]][,"med_fever"] <-recoder(NGAfiles[[8]][,"med_fever"]) 
table(NGAfiles[[8]]$med_fever)


NGAfiles[[11]]$med_fever <- rowSums(NGAfiles[[11]][, c(151:162, 164:168)], na.rm=T) # 2010
NGAfiles[[11]][,"med_fever"] <-recoder(NGAfiles[[11]][,"med_fever"]) 
table(NGAfiles[[11]]$med_fever)


NGAfiles[[14]]$med_fever <- rowSums(NGAfiles[[14]][, c(749:760, 762:766)], na.rm=T) # 2013
NGAfiles[[14]][,"med_fever"] <-recoder(NGAfiles[[14]][,"med_fever"]) 
table(NGAfiles[[14]]$med_fever)


NGAfiles[[17]]$med_fever <- rowSums(NGAfiles[[17]][, c(563:574, 576:580)], na.rm=T) # 2015
NGAfiles[[17]][,"med_fever"] <-recoder(NGAfiles[[17]][,"med_fever"]) 
table(NGAfiles[[17]]$med_fever)



# recoding fever variables 

table(NGAfiles[[2]]$h22)
NGAfiles[[2]][,"h22"] <-recoder(NGAfiles[[2]][,"h22"]) 

table(NGAfiles[[5]]$h22)
NGAfiles[[5]][,"h22"] <-recoder(NGAfiles[[5]][,"h22"]) 

table(NGAfiles[[8]]$h22)
NGAfiles[[8]][,"h22"] <-recoder(NGAfiles[[8]][,"h22"]) 

table(NGAfiles[[11]]$h22)
NGAfiles[[11]][,"h22"] <-recoder(NGAfiles[[11]][,"h22"]) 

table(NGAfiles[[14]]$h22)
NGAfiles[[14]][,"h22"] <-recoder(NGAfiles[[14]][,"h22"]) 


table(NGAfiles[[17]]$h22)
NGAfiles[[17]][,"h22"] <-recoder(NGAfiles[[17]][,"h22"]) 


# U5 medical treatment for fever list for 1990, 2003, 2008, 2010, 2013, 2015
medfever.list <- list(NGAfiles[[2]], NGAfiles[[5]], NGAfiles[[8]], NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]])



# U5 ITN list for 2003, 2008, 2010, 2013, 2015
ITN.list <- list(NGAfiles[[5]], NGAfiles[[8]], NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]])

# Adult ITN list for 2003, 2008, 2010, 2013, 2015
A_ITN.list <- list(NGAfiles[[4]], NGAfiles[[7]], NGAfiles[[10]], NGAfiles[[13]], NGAfiles[[16]])
 
# Household bednet availability for 2003, 2008, 2010, 2013, 2015 
H_ITN.list <- list(NGAfiles[[6]], NGAfiles[[9]], NGAfiles[[12]], NGAfiles[[15]], NGAfiles[[18]])


# ACT taken for malaria - 2008, 2010, 2013, 2015 
ACT.list <- list(NGAfiles[[8]], NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]])


# IPTP list for 2003, 2008, 2010, 2013, 2015 
IPTP.list <- list(NGAfiles[[4]], NGAfiles[[7]], NGAfiles[[10]], NGAfiles[[13]], NGAfiles[[16]])

# IRS list for HR files 2010, 2013, and 2015 

IRS.list <- list(NGAfiles_2[[12]], NGAfiles_2[[15]], NGAfiles_2[[18]])

# list for Type of bednet used by respondent (ml101)

ml101.list <- A_ITN.list


# health seeking in PR files estimation 
# 2015 
table(hspr.list[[2]]$sh13)
hspr.list[[2]][,"sh13"] <-recoder(hspr.list[[2]][,"sh13"]) #fever in the last two weeks 

table(hspr.list[[2]]$sh14)
hspr.list[[2]][,"sh14"] <-recoder(hspr.list[[2]][,"sh14"]) #treatment for fever in the last two weeks 

table(hspr.list[[2]]$med_fever)
hspr.list[[2]][,"med_fever"] <-recoder3(hspr.list[[2]][,"sh15"]) #place treatment for fever was sought 


# 2010 

table(hspr.list[[1]]$sh11)
hspr.list[[1]][,"sh11"] <-recoder(hspr.list[[1]][,"sh11"]) #fever in the last two weeks 

table(hspr.list[[1]]$sh12)
hspr.list[[1]][,"sh12"] <-recoder(hspr.list[[1]][,"sh12"]) #treatment for fever in the last two weeks 

table(hspr.list[[1]]$med_fever)
hspr.list[[1]][,"med_fever"] <-recoder4(hspr.list[[1]][,"sh13"]) #place treatment for fever was sought 

#############################################################################################################
# Recoding analysis files 
############################################################################################################

# recoding pfpr (microscopy data)
# 2015 
pfpr.list[[2]][,"hml32"] <-recoder(pfpr.list[[2]][,"hml32"]) 
table(pfpr.list[[2]][,"hml32"])

# 2010
pfpr.list[[1]][,"hml32"] <-recoder(pfpr.list[[1]][,"hml32"])
table(pfpr.list[[1]][,"hml32"])
pfpr.list[[1]][,"hml35"] <-recoder(pfpr.list[[1]][,"hml35"])
table(pfpr.list[[1]][,"hml35"])



# recoding U5 medical treatment using h32z 
#2015 
medfever.list[[6]][,"h32z"]<-recoder(medfever.list[[6]][,"h32z"]) 


# 2013 
medfever.list[[5]][,"h32z"] <-recoder(medfever.list[[5]][,"h32z"]) 


# 2010
medfever.list[[4]][,"h32z"] <-recoder(medfever.list[[4]][,"h32z"]) 


# recoding U5 ITN use 

# 2003 
table(ITN.list[[1]][,"ml0"])
ITN.list[[1]][,"ml0"] <-recoder2(ITN.list[[1]][,"ml0"]) 


# 2008
table(ITN.list[[2]][,"ml0"])
ITN.list[[2]][,"ml0"] <-recoder2(ITN.list[[2]][,"ml0"]) 


# 2010
table(ITN.list[[3]][,"ml0"])
ITN.list[[3]][,"ml0"] <-recoder2(ITN.list[[3]][,"ml0"]) 


# 2013
table(ITN.list[[4]][,"ml0"])
ITN.list[[4]][,"ml0"] <-recoder2(ITN.list[[4]][,"ml0"]) 


# 2015
table(ITN.list[[5]][,"ml0"])
ITN.list[[5]][,"ml0"] <-recoder2(ITN.list[[5]][,"ml0"]) 


# recoding household ITN access 

# 2003 
table(H_ITN.list[[1]][,"hv227"])
H_ITN.list[[1]][,"hv227"] <-recoder(H_ITN.list[[1]][,"hv227"]) 


# 2010
table(H_ITN.list[[3]][,"hv227"])
H_ITN.list[[3]][,"hv227"] <-recoder(H_ITN.list[[3]][,"hv227"]) 


# 2013
table(H_ITN.list[[4]][,"hv227"])
H_ITN.list[[4]][,"hv227"] <-recoder(H_ITN.list[[4]][,"hv227"]) 


# recoding ACT taken for fever  

# 2010
table(ACT.list[[2]][,"ml13e"])
ACT.list[[2]][,"ml13e"] <-recoder(ACT.list[[2]][,"ml13e"]) 


# 2013
table(ACT.list[[3]][,"ml13e"])
ACT.list[[3]][,"ml13e"] <-recoder(ACT.list[[3]][,"ml13e"]) 


#recoding IPTP 

# 2003 
table(IPTP.list[[1]][,"m49a_1"])
IPTP.list[[1]][,"m49a_1"] <-recoder(IPTP.list[[1]][,"m49a_1"]) 


# 2008
table(IPTP.list[[2]][,"m49a_1"])
IPTP.list[[2]][,"m49a_1"] <-recoder(IPTP.list[[2]][,"m49a_1"])



# 2010
table(IPTP.list[[3]][,"m49a_1"])
IPTP.list[[3]][,"m49a_1"] <-recoder(IPTP.list[[3]][,"m49a_1"])


# 2013
table(IPTP.list[[4]][,"m49a_1"])
IPTP.list[[4]][,"m49a_1"] <-recoder(IPTP.list[[4]][,"m49a_1"])



# 2015
table(IPTP.list[[5]][,"m49a_1"])
IPTP.list[[5]][,"m49a_1"] <-recoder(IPTP.list[[5]][,"m49a_1"])


#recoding IRS

#2015
table(IRS.list[[3]][,"hv253"])
IRS.list[[3]][,"hv253"] <-recoder(IRS.list[[3]][,"hv253"])


#2013
table(IRS.list[[2]][,"hv253"])
IRS.list[[2]][,"hv253"] <-recoder(IRS.list[[2]][,"hv253"])


#2010
table(IRS.list[[1]][,"hv253"])
IRS.list[[1]][,"hv253"] <-recoder(IRS.list[[1]][,"hv253"])

# recoding ml101 ITN 

# 2003
table(ml101.list[[1]][,"ml101"])
ml101.list[[1]][,"ml101"] <-recoder2(ml101.list[[1]][,"ml101"])


# 2003
table(ml101.list[[1]][,"ml101"])
ml101.list[[1]][,"ml101"] <-recoder2(ml101.list[[1]][,"ml101"])


# 2008
table(ml101.list[[2]][,"ml101"])
ml101.list[[2]][,"ml101"] <-recoder2(ml101.list[[2]][,"ml101"])


# 2010
table(ml101.list[[3]][,"ml101"])
ml101.list[[3]][,"ml101"] <-recoder2(ml101.list[[3]][,"ml101"])


# 2015
table(ml101.list[[5]][,"ml101"])
ml101.list[[5]][,"ml101"] <-recoder2(ml101.list[[5]][,"ml101"])


#########################################################################################################
# Mapping points to inherit associated health district
#########################################################################################################


key_list <- map(NGAshplist, over.fun)

key_list[[1]]$v001<-NGAshplist[[1]]@data[,"DHSCLUST"] #1993 
key_list[[2]]$v001<-NGAshplist[[2]]@data[,"DHSCLUST"] #2003
key_list[[3]]$v001<-NGAshplist[[3]]@data[,"DHSCLUST"] #2008 
key_list[[4]]$v001<-NGAshplist[[4]]@data[,"DHSCLUST"] #2010 
key_list[[5]]$v001<-NGAshplist[[5]]@data[,"DHSCLUST"] #2013 
key_list[[6]]$v001<-NGAshplist[[6]]@data[,"DHSCLUST"] #2015 


# key list for pfpr 
keyspfpr <- list(key_list[[4]], key_list[[6]]) #changing to a list of keys 


# applying function to create month and year of survey. Note this function only works for person and household recode file
pfpr.list <- map(pfpr.list, survey.month.fun)
H_ITN.list <- map(H_ITN.list, survey.month.fun)
IRS.list <- map(IRS.list, survey.month.fun)
hspr.list <-map(hspr.list, survey.month.fun)

# key datasets and dhs/mis datasets are joined  
pfpr.list <- map2(pfpr.list, keyspfpr, left_join) #PR datasets
hspr.list <-map2(hspr.list, keyspfpr, left_join) #medical treatment for fever in PR files 
medfever.list <-map2(medfever.list,key_list, left_join) #medfever datasets 

#explore.dat <-medfever.list[[2]] %>% dplyr::select(LGA, h32z, pri_med) %>% filter(LGA == "Abeokuta South")

pri_medfever.list <- medfever.list #private health seeking beahvior 
fever.list <- medfever.list #fever symptoms 

# key list for ITN
keysITN <- list(key_list[[2]], key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]]) #changing to a list of keys 

ITN.list <-map2(ITN.list,keysITN, left_join) #ITN datasets 

A_ITN.list <- map2(A_ITN.list, keysITN, left_join) #adult ITN datasets 


# key list join with household ITN access data  

H_ITN.list <- map2(H_ITN.list, keysITN, left_join)


# key list join  for ACT 

key_ACT <- list(key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]])

ACT.list <- map2(ACT.list, key_ACT, left_join) #ACT datasets 


# key list join for IPTP 

IPTP.list <- map2(IPTP.list, keysITN, left_join)

# key list join for IRS 

key_IRS <- list(key_list[[4]], key_list[[5]], key_list[[6]])

IRS.list <- map2(IRS.list, key_IRS, left_join)

#key list for ml101 ITN variable 

ml101.list <-map2(ml101.list, keysITN, left_join)


####################################################################################################
## Pfpr analysis
####################################################################################################

# DS-level estimates 


# 2015 PR BF DHS 
pfpr.list[[2]] <- dataclean.para(pfpr.list[[2]], hv005, hc1, hml32, 'hml32', 'p_test') 
pfpr.list[[2]] <- dataclean.para(pfpr.list[[2]], hv005, hc1, hml35, 'hml35', 'p_testRDT')
hml32.svyd15 <- svydesign.fun(pfpr.list[[2]])



# 2015 
DS_merge_ym_15 <- result.fun.para('p_test', 'LGA + time2', 'num_p', 'LGA', design=hml32.svyd15) #microscopy 
head(DS_merge_ym_15)

DS_merge_ym_RDT15 <- result.fun.para('p_testRDT', 'LGA + time2', 'num_p', 'LGA', design=hml32.svyd15) #RDT 
head(DS_merge_ym_RDT15)


# recoding and renaming pfpr datasets 
# 2010 PR BF DHS 
pfpr.list[[1]] <- dataclean.para(pfpr.list[[1]], hv005, hc1, hml32, 'hml32', 'p_test') 
pfpr.list[[1]] <- dataclean.para(pfpr.list[[1]], hv005, hc1, hml35,'hml35', 'p_testRDT')
hml32.svydesign <- svydesign.fun(pfpr.list[[1]])



# 2010 PR BF DHS
DS_merge_ym <- result.fun.para('p_test', 'LGA + time2','num_p', 'LGA', design=hml32.svydesign) #microscopy 
head(DS_merge_ym)

DS_merge_ym_RDT <- result.fun.para('p_testRDT', 'LGA + time2', 'num_p', 'LGA', design=hml32.svydesign) #RDT 
head(DS_merge_ym_RDT)


#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) 
write.csv(DS_merge_ym_15,file="DS15_pfpr_ym_micro.csv") # 2015 PfPr micro BF DHS
write.csv(DS_merge_ym_RDT15, file="DS15_pfpr_ym_RDT15.csv") # 2015 PfPr PR RDT BF DHS 

#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) for the DSs 
write.csv(DS_merge_ym, file="DS10_pfpr_ym_micro.csv") # 2010 PfPr PR micro BF DHS 
write.csv(DS_merge_ym_RDT, file="DS10_pfpr_ym_RDT.csv") # 2010 PfPr PR RDT BF DHS 

 


# cluster-level estimates 

# 2015 PR BF DHS 

clu_ptest_num_15 <- result.clu.fun.para('p_test', 'v001 + time2','num_p', 'v001', design=hml32.svyd15, pfpr.list[[2]]) #microscopy 
head(clu_ptest_num_15)


clu_ptest_num_RDT15 <- result.clu.fun.para('p_testRDT', 'v001 + time2','num_p', 'v001', design=hml32.svyd15, pfpr.list[[2]]) #RDT 
head(clu_ptest_num_RDT15)


# 2010 PR BF DHS 
clu_ptest_num <- result.clu.fun.para('p_test', 'v001 + time2','num_p', 'v001', design=hml32.svydesign, pfpr.list[[1]]) #microscopy 
head(clu_ptest_num)

clu_ptest_num_RDT <- result.clu.fun.para('p_testRDT', 'v001 + time2','num_p', 'v001', design=hml32.svydesign, pfpr.list[[1]]) #RDT
head(clu_ptest_num_RDT)


#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) 
write.csv(clu_ptest_num, file="clu10_pfpr_ym_micro.csv") # 2010 PfPr PR micro BF DHS 
write.csv(clu_ptest_num_RDT, file="clu10_pfpr_ym_RDT.csv") # 2010 PfPr PR RDT BF DHS 


write.csv(clu_ptest_num_15,file="clu15_pfpr_ym_micro.csv") # 2015 PfPr micro BF DHS
write.csv(clu_ptest_num_RDT15, file="clu15_pfpr_ym_RDT15.csv") # 2015 PfPr PR RDT BF DHS  

#######################################################################################################
# merging to identify DSs with no estimates 
#######################################################################################################

# set up an empty dataframe with the LGA names so LGA with no estimates can be identified 
DS_merge<-data.frame(LGA=LGAshp@data[,"LGA"])

# read in the MAP shapefiles and convert to sf object to merge  
MAPshp <- readOGR("nigeria_MAP/District_Summary_Shapefiles", layer ="NGA_pfpr_maps_2-4", use_iconv=TRUE, encoding= "UTF-8")
MAPshp<-st_as_sf(MAPshp)


# # 2015 PR BF DHS
# DS_merge_noest15<-DS_merge%>%left_join(ptest_15_no_time)%>%filter(is.na(p_test))
# head(DS_merge_noest15)
# colnames(DS_merge_noest15)[1] <- "District"
# 
# DS_merge_noest15 <- DS_merge_noest15%>%left_join(MAPshp)%>%dplyr::select(District, Region, map3_2015, map4_2015, map4_LCI15, map4_UCI15)
# 
# 
# write.csv(DS_merge_noest15, file="DS15_noest.csv") # 2015 PfPr PR micro BF DHS 
# 
# 
# 
# # 2010 PR BF DHS
# DS_merge_noest<-DS_merge%>%left_join(ptest_10_no_time)%>%filter(is.na(p_test))
# head(DS_merge_noest)
# colnames(DS_merge_noest)[1] <- "District"
# 
# DS_merge_noest <- DS_merge_noest%>%left_join(MAPshp)%>%dplyr::select(District, Region, map3_2010, map4_2010, map4_LCI10, map4_UCI10)




########################################################################################################
# Treating the DHS as a random sample to estimate Pfpr for Banfora (substitute 2010 and 2015 data)
########################################################################################################
dhs <- pfpr.list[[1]]%>% dplyr:: select(LGA, p_test, time2) %>% filter(!is.na(p_test))



dhs2<-na.omit(dhs)%>%
  group_by(LGA, time2)%>%
  summarise_each(funs(mean, sd, std.error, n()))

write.csv(dhs2, "SRS10_ptest_micro.csv")
#####################################################################################################





#########################################################################################################################
# U5 Medical treatment for Fever analysis (data is cleaned in R and exported to SAS for analysis. Final cleaning in R)
#########################################################################################################################
# DS-level estimates 


# 2015 

medfever.list[[6]]<-dataclean(medfever.list[[6]], h32z, v005, 'med_fever', 'med_fever')  

write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")




# 2013 

medfever.list[[5]] <-dataclean(medfever.list[[5]], h32z, v005, 'med_fever', 'med_fever') 
write.foreign(medfever.list[[5]], "mydata_13.txt", "med_fever_13.sas",   package="SAS")





# 2010 

medfever.list[[4]] <-dataclean(medfever.list[[4]], h32z, v005, 'med_fever', 'med_fever') 
write.foreign(medfever.list[[4]], "mydata_10.txt", "med_fever_10.sas",   package="SAS")




# 2008

medfever.list[[3]] <-dataclean(medfever.list[[3]], h32z, v005, 'med_fever', 'med_fever') 

write.foreign(medfever.list[[3]], "mydata_08.txt", "med_fever_08.sas",   package="SAS")

#reading data back into R for final cleaning 

medfever_08 <- read.csv('C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/state DHS estimates/HSB/U5/med_fever/Sfever_med_08.csv')



medfever_08_ed <- medfever_08 %>% fill(sstate) %>% dplyr::filter(med_fever == 1)

iLabels <- val_labels(medfever.list[[3]]$sstate)

match.idx <- match(medfever_08$sstate, iLabels)
medfever_08$State <- ifelse(is.na(match.idx),
                              medfever_08$State,
                            names(iLabels)[match.idx])

medfever_08$State <- str_to_title(medfever_08$State)


rep_DS <- read.csv('C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/rep_DS_al.csv')

rep_DS_S <- rep_DS%>% left_join(LGAshp_sf)

rep_DS_S <- rep_DS_S %>% left_join(medfever_08)

write.csv(medfever_08_ed, "Sfever_med_08.csv")




# 2003

medfever.list[[2]] <-dataclean(medfever.list[[2]], h32z, v005, 'med_fever', 'med_fever') 
write.foreign(medfever.list[[2]], "mydata_03.txt", "med_fever_03.sas",   package="SAS")


h32z.svyd03 <- svydesign.fun(medfever.list[[2]])


S_fever_03 <- result.fun('fever', 'sstate','num_p', design=h32z.svyd03)
head(S_fever_03)

write.csv(S_fever_03, "Sfever_med_03.csv")


# 1990

medfever.list[[1]] <-dataclean(medfever.list[[1]], h32z, v005, 'med_fever', 'med_fever') 
write.foreign(medfever.list[[1]], "mydata_90.txt", "med_fever_90.sas",   package="SAS")


h32z.svyd90 <- svydesign.fun(medfever.list[[1]])

med_fever_90 <- medfever.list[[1]] %>% dplyr::select(sstate, v001, fever, v005, num_p) %>% filter(sstate=='1')
write.csv(med_fever_90, "med_fever_90.csv")

S_fever_90 <- result.fun('fever', 'sstate','num_p', design=h32z.svyd90)
head(S_fever_90)

write.csv(S_fever_90, "Sfever_med_90.csv")



# cluster-level estimates 

# 2015
clu_fever_15 <- result.clu.fun('fever', 'v001','num_p', design=h32z.svyd15, medfever.list[[6]])
head(clu_fever_15)

write.csv(clu_fever_15, "clu_fever_15.csv")


# 2013
clu_fever_13 <- result.clu.fun('fever', 'v001','num_p', design=h32z.svyd13, medfever.list[[5]])
head(clu_fever_13)

write.csv(clu_fever_13, "clu_fever_13.csv")


# 2010
clu_fever_10 <- result.clu.fun('fever', 'v001','num_p', design=h32z.svyd10, medfever.list[[4]])
head(clu_fever_10)

write.csv(clu_fever_10, "clu_fever_10.csv")


# 2008
clu_fever_08 <- result.clu.fun('fever', 'v001','num_p', design=h32z.svyd08, medfever.list[[3]])
head(clu_fever_08)

write.csv(clu_fever_08, "clu_fever_08.csv")


# 2003
clu_fever_03 <- result.clu.fun('fever', 'v001','num_p', design=h32z.svyd03, medfever.list[[2]])
head(clu_fever_03)

write.csv(clu_fever_03, "clu_fever_03.csv")



# 1990
clu_fever_90 <- result.clu.fun('fever', 'v001','num_p', design=h32z.svyd90, medfever.list[[1]])
head(clu_fever_90)

write.csv(clu_fever_90, "clu_fever_90.csv")



#####################################################################################################
# U5 private health seeking for fever 
####################################################################################################
# DS-level estimates 

# 2015

pri_medfever.list[[6]] <-dataclean(pri_medfever.list[[6]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd15 <- svydesign.fun(pri_medfever.list[[6]])


DS_primed_15 <- result.fun('pri_med', 'sstate','num_p', design=primed.svyd15)
head(DS_primed_15)

write.csv(DS_primed_15, "DSfever_primed_15.csv")


# 2013 
pri_medfever.list[[5]] <-dataclean(pri_medfever.list[[5]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd13 <- svydesign.fun(pri_medfever.list[[5]])

DS_primed_13 <- result.fun('pri_med', 'sstate','num_p', design=primed.svyd13)
head(DS_primed_13)

write.csv(DS_primed_13, "DSfever_primed_13.csv")


# 2010 
pri_medfever.list[[4]] <-dataclean(pri_medfever.list[[4]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd10 <- svydesign.fun(pri_medfever.list[[4]])

DS_primed_10 <- result.fun('pri_med', 'sstate','num_p', design=primed.svyd10)
head(DS_primed_10)

write.csv(DS_primed_10, "DSfever_primed_10.csv")


# 2008
pri_medfever.list[[3]] <-dataclean(pri_medfever.list[[3]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd08 <- svydesign.fun(pri_medfever.list[[3]])

DS_primed_08 <- result.fun('pri_med', 'sstate','num_p', design=primed.svyd08)
head(DS_primed_08)

write.csv(DS_primed_08, "DSfever_primed_08.csv")


# 2003
pri_medfever.list[[2]] <-dataclean(pri_medfever.list[[2]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd03 <- svydesign.fun(pri_medfever.list[[2]])

DS_primed_03 <- result.fun('pri_med', 'sstate','num_p', design=primed.svyd03)
head(DS_primed_03)

write.csv(DS_primed_03, "DSfever_primed_03.csv")


# 1990
pri_medfever.list[[1]] <-dataclean(pri_medfever.list[[1]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd90 <- svydesign.fun(pri_medfever.list[[1]])

DS_primed_90 <- result.fun('pri_med', 'sstate','num_p', design=primed.svyd90)
head(DS_primed_90)

write.csv(DS_primed_90, "DSfever_primed_90.csv")



pri_med_DS <- list(DS_primed_90,DS_primed_03,DS_primed_08, DS_primed_10,DS_primed_13,DS_primed_15)

LGAshp_sf_ls <- list(LGAshp_sf, LGAshp_sf, LGAshp_sf, LGAshp_sf, LGAshp_sf, LGAshp_sf)

pri_med_sf <- map2(LGAshp_sf_ls, pri_med_DS, left_join)




# cluster-level estimates 

# 2015
clu_primed_15 <- result.clu.fun('pri_med', 'v001','num_p', design=primed.svyd15, pri_medfever.list[[6]])
head(clu_primed_15)

write.csv(clu_primed_15, "clu_primed_15.csv")


# 2013
clu_primed_13 <- result.clu.fun('pri_med', 'v001','num_p', design=primed.svyd13, pri_medfever.list[[5]])
head(clu_primed_13)

write.csv(clu_primed_13, "clu_primed_13.csv")


# 2010
clu_primed_10 <- result.clu.fun('pri_med', 'v001','num_p', design=primed.svyd10, pri_medfever.list[[4]])
head(clu_primed_10)

write.csv(clu_primed_10, "clu_primed_10.csv")


# 2008
clu_primed_08 <- result.clu.fun('pri_med', 'v001','num_p', design=primed.svyd08, pri_medfever.list[[3]])
head(clu_primed_08)

write.csv(clu_primed_08, "clu_primed_08.csv")


# 2003
clu_primed_03 <- result.clu.fun('pri_med', 'v001','num_p', design=primed.svyd03, pri_medfever.list[[2]])
head(clu_primed_03)

write.csv(clu_primed_03, "clu_primed_03.csv")


# 1990
clu_primed_90 <- result.clu.fun('pri_med', 'v001','num_p', design=primed.svyd90, pri_medfever.list[[1]])
head(clu_primed_90)

write.csv(clu_primed_90, "clu_primed_90.csv")

clu_primed_ls <- list(clu_primed_90,clu_primed_03,clu_primed_08,clu_primed_10,clu_primed_13,clu_primed_15)

clu_shp_primed <- map2(NGAshplist, clu_primed_ls, left_join)

pri_med_90 <-tmap.fun2(pri_med_sf[[1]], "pri_med", "Private HSB", 
            "1990")

pri_med_03 <-tmap.fun2(pri_med_sf[[2]], "pri_med", "Private HSB", "2003")

pri_med_08 <-tmap.fun2(pri_med_sf[[3]], "pri_med", "Private HSB", "2008")

pri_med_10 <-tmap.fun2(pri_med_sf[[4]], "pri_med", "Private HSB", "2010")

pri_med_13 <-tmap.fun2(pri_med_sf[[5]], "pri_med", "Private HSB", "2013")

pri_med_15 <-tmap.fun2(pri_med_sf[[6]], "pri_med", "Private HSB", "2015")

all_private <- tmap_arrange(pri_med_90, pri_med_03, pri_med_08,pri_med_10, pri_med_13,pri_med_15)

tmap_save(tm = all_private , filename = "private hsb.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
#####################################################################################################
# U5 fever prevalence 
####################################################################################################
# DS-level estimates 

# 2015
fever.list[[6]] <-dataclean(fever.list[[6]], h22, v005,'h22', 'fever_prev')  
h22.svyd15 <- svydesign.fun(fever.list[[6]])


DS_fev_pre_15 <- result.fun('fever_prev', 'LGA','num_p', design=h22.svyd15)
head(DS_fev_pre_15)

write.csv(DS_fev_pre_15, "DS_fev_pre_15.csv")


# 2013 

fever.list[[5]] <-dataclean(fever.list[[5]], h22, v005,'h22', 'fever_prev')  
h22.svyd13 <- svydesign.fun(fever.list[[5]])


DS_fev_pre_13 <- result.fun('fever_prev', 'LGA','num_p', design=h22.svyd13)
head(DS_fev_pre_13)

write.csv(DS_fev_pre_13, "DS_fev_pre_13.csv")


# 2010 

fever.list[[4]] <-dataclean(fever.list[[4]], h22, v005,'h22', 'fever_prev')  
h22.svyd10 <- svydesign.fun(fever.list[[4]])

DS_fev_pre_10 <- result.fun('fever_prev', 'LGA','num_p', design=h22.svyd10)
head(DS_fev_pre_10)

write.csv(DS_fev_pre_10, "DS_fev_pre_10.csv")


# 2008

fever.list[[3]] <-dataclean(fever.list[[3]], h22, v005,'h22', 'fever_prev')  
h22.svyd08 <- svydesign.fun(fever.list[[3]])

DS_fev_pre_08 <- result.fun('fever_prev', 'LGA','num_p', design=h22.svyd08)
head(DS_fev_pre_08)

write.csv(DS_fev_pre_08, "DS_fev_pre_08.csv")


# 2003

fever.list[[2]] <-dataclean(fever.list[[2]], h22, v005,'h22', 'fever_prev')  
h22.svyd03 <- svydesign.fun(fever.list[[2]])

DS_fev_pre_03 <- result.fun('fever_prev', 'LGA','num_p', design=h22.svyd03)
head(DS_fev_pre_03)

write.csv(DS_fev_pre_03, "DS_fev_pre_03.csv")


# 1990
fever.list[[1]] <-dataclean(fever.list[[1]], h22, v005,'h22', 'fever_prev')  
h22.svyd90 <- svydesign.fun(fever.list[[1]])

DS_fev_pre_90 <- result.fun('fever_prev', 'LGA','num_p', design=h22.svyd90)
head(DS_fev_pre_90)

write.csv(DS_fev_pre_90, "DS_fev_pre_90.csv")



# cluster-level estimates 

# 2015

clu_fev_pre_15 <- result.clu.fun('fever_prev', 'v001','num_p', design=h22.svyd15, fever.list[[6]])
head(clu_fev_pre_15)

write.csv(clu_fev_pre_15, "clu_fev_pre_15.csv")


# 2013
clu_fev_pre_13 <- result.clu.fun('fever_prev', 'v001','num_p', design=h22.svyd13, fever.list[[5]])
head(clu_fev_pre_13)

write.csv(clu_fev_pre_13, "clu_fev_pre_13.csv")


# 2010
clu_fev_pre_10 <- result.clu.fun('fever_prev', 'v001','num_p', design=h22.svyd10, fever.list[[4]])
head(clu_fev_pre_10)

write.csv(clu_fev_pre_10, "clu_fev_pre_10.csv")


# 2008
clu_fev_pre_08 <- result.clu.fun('fever_prev', 'v001','num_p', design=h22.svyd08, fever.list[[3]])
head(clu_fev_pre_08)

write.csv(clu_fev_pre_08, "clu_fev_pre_08.csv")


# 2003
clu_fev_pre_03 <- result.clu.fun('fever_prev', 'v001','num_p', design=h22.svyd03, fever.list[[2]])
head(clu_fev_pre_03)

write.csv(clu_fev_pre_03, "clu_fev_pre_03.csv")


# 1990
clu_fev_pre_90 <- result.clu.fun('fever_prev', 'v001','num_p', design=h22.svyd90, fever.list[[1]]) 
head(clu_fev_pre_90)

write.csv(clu_fev_pre_90, "clu_fev_pre_90.csv")



#####################################################################################################
# U5 ITN use prevalence 
####################################################################################################
# DS-level estimates 

look_for(ITN.list[[5]], "state")

# 2015
ITN.list[[5]] <-dataclean(ITN.list[[5]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd15 <- svydesign.fun(ITN.list[[5]])


DS_ITN_S_15 <- result.fun('U5_ITN_use', 'sstate','num_p', design=ml0.svyd15)
head(DS_ITN_S_15)

write.csv(DS_ITN_pre_15, "DS_ITN_pre_15.csv")


# 2013
ITN.list[[4]] <-dataclean(ITN.list[[4]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd13 <- svydesign.fun(ITN.list[[4]])


DS_ITN_pre_13 <- result.fun('U5_ITN_use', 'LGA','num_p', design=ml0.svyd13)
head(DS_ITN_pre_13)

write.csv(DS_ITN_pre_13, "DS_ITN_pre_13.csv")



# 2010
ITN.list[[3]] <-dataclean(ITN.list[[3]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd10 <- svydesign.fun(ITN.list[[3]])


S_ITN_pre_10 <- result.fun('U5_ITN_use', 'sstate','num_p', design=ml0.svyd10)
head(S_ITN_pre_10)

write.csv(S_ITN_pre_10, "S_ITN_pre_10.csv")



# 2008
ITN.list[[2]] <-dataclean(ITN.list[[2]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd08 <- svydesign.fun(ITN.list[[2]])


S_ITN_pre_08 <- result.fun('U5_ITN_use', 'sstate','num_p', design=ml0.svyd08)
head(S_ITN_pre_08)

write.csv(S_ITN_pre_08, "S_ITN_pre_08.csv")


# 2003
ITN.list[[1]] <-dataclean(ITN.list[[1]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd03 <- svydesign.fun(ITN.list[[1]])


DS_ITN_pre_03 <- result.fun('U5_ITN_use', 'LGA','num_p', design=ml0.svyd03)
head(DS_ITN_pre_03)

write.csv(DS_ITN_pre_03, "DS_ITN_pre_03.csv")



# cluster-level estimates 


# 2015

clu_ITN_pre_15 <- result.clu.fun('U5_ITN_use', 'v001','num_p', design=ml0.svyd15,ITN.list[[5]])
head(clu_ITN_pre_15)

write.csv(clu_ITN_pre_15, "clu_ITN_pre_15.csv")


# 2013

clu_ITN_pre_13 <- result.clu.fun('U5_ITN_use', 'v001','num_p', design=ml0.svyd13,ITN.list[[4]])
head(clu_ITN_pre_13)

write.csv(clu_ITN_pre_13, "clu_ITN_pre_13.csv")


# 2010

clu_ITN_pre_10 <- result.clu.fun('U5_ITN_use', 'v001','num_p', design=ml0.svyd10,ITN.list[[3]])
head(clu_ITN_pre_10)

write.csv(clu_ITN_pre_10, "clu_ITN_pre_10.csv")


# 2008

clu_ITN_pre_08 <- result.clu.fun('U5_ITN_use', 'v001','num_p', design=ml0.svyd08,ITN.list[[2]])
head(clu_ITN_pre_08)

write.csv(clu_ITN_pre_08, "clu_ITN_pre_08.csv")


# 2003

clu_ITN_pre_03 <- result.clu.fun('U5_ITN_use', 'v001','num_p', design=ml0.svyd03,ITN.list[[1]])
head(clu_ITN_pre_03)

write.csv(clu_ITN_pre_03, "clu_ITN_pre_03.csv")


##############################################################################################
# Adult women ITN use 
#############################################################################################
# DS-level estimates 

# 2015
A_ITN.list[[5]] <-dataclean(A_ITN.list[[5]], v461, v005,'v461', 'A_ITN_use')  
v461.svyd15 <- svydesign.fun(A_ITN.list[[5]])

DS_ITN_A_15 <- result.fun('A_ITN_use', 'LGA','num_p', design=v461.svyd15)
head(DS_ITN_A_15)

write.csv(DS_ITN_A_15, "DS_ITN_A_15.csv")



# 2013
A_ITN.list[[4]] <-dataclean(A_ITN.list[[4]], v461, v005,'v461', 'A_ITN_use')  
v461.svyd13 <- svydesign.fun(A_ITN.list[[4]])

DS_ITN_A_13 <- result.fun('A_ITN_use', 'LGA','num_p', design=v461.svyd13)
head(DS_ITN_A_13)

write.csv(DS_ITN_A_13, "DS_ITN_A_13.csv")



# 2010
A_ITN.list[[3]] <-dataclean(A_ITN.list[[3]], v461, v005,'v461', 'A_ITN_use')  
v461.svyd10 <- svydesign.fun(A_ITN.list[[3]])

DS_ITN_A_10 <- result.fun('A_ITN_use', 'LGA','num_p', design=v461.svyd10)
head(DS_ITN_A_10)

write.csv(DS_ITN_A_10, "DS_ITN_A_10.csv")



# 2008
A_ITN.list[[2]] <-dataclean(A_ITN.list[[2]], v461, v005,'v461', 'A_ITN_use')  
v461.svyd08 <- svydesign.fun(A_ITN.list[[2]])

DS_ITN_A_08 <- result.fun('A_ITN_use', 'LGA','num_p', design=v461.svyd08)
head(DS_ITN_A_08)

write.csv(DS_ITN_A_08, "DS_ITN_A_08.csv")




# 2003
A_ITN.list[[1]] <-dataclean(A_ITN.list[[1]], v461, v005,'v461', 'A_ITN_use')  
v461.svyd03 <- svydesign.fun(A_ITN.list[[1]])

DS_ITN_A_03 <- result.fun('A_ITN_use', 'LGA','num_p', design=v461.svyd03)
head(DS_ITN_A_03)

write.csv(DS_ITN_A_03, "DS_ITN_A_03.csv")




# cluster-level estimates 


# 2015

clu_A_ITN_15 <- result.clu.fun('A_ITN_use', 'v001','num_p', design=v461.svyd15,A_ITN.list[[5]])
head(clu_A_ITN_15)

write.csv(clu_A_ITN_15, "clu_A_ITN_15.csv")


# 2013 

clu_A_ITN_13 <- result.clu.fun('A_ITN_use', 'v001','num_p', design=v461.svyd13,A_ITN.list[[4]])
head(clu_A_ITN_13)

write.csv(clu_A_ITN_13, "clu_A_ITN_13.csv")



# 2010

clu_A_ITN_10 <- result.clu.fun('A_ITN_use', 'v001','num_p', design=v461.svyd10,A_ITN.list[[3]])
head(clu_A_ITN_10)

write.csv(clu_A_ITN_10, "clu_A_ITN_10.csv")



# 2008

clu_A_ITN_08 <- result.clu.fun('A_ITN_use', 'v001','num_p', design=v461.svyd08,A_ITN.list[[2]])
head(clu_A_ITN_08)

write.csv(clu_A_ITN_08, "clu_A_ITN_08.csv")



# 2003

clu_A_ITN_03 <- result.clu.fun('A_ITN_use', 'v001','num_p', design=v461.svyd03,A_ITN.list[[2]])
head(clu_A_ITN_03)

write.csv(clu_A_ITN_08, "clu_A_ITN_03.csv")



##############################################################################################
# Household ITN access 
##############################################################################################


# DS-level estimates 

# 2015
H_ITN.list[[5]] <-dataclean.HH(H_ITN.list[[5]], hv227, hv005,'hv227', 'H_ITN_use')  
hv227.svyd15 <- svydesign.fun(H_ITN.list[[5]])

DS_ITN_H_15 <- result.fun('H_ITN_use', 'LGA','num_p', design=hv227.svyd15)
head(DS_ITN_H_15)

write.csv(DS_ITN_H_15, "DS_ITN_H_15.csv")



# 2013
H_ITN.list[[4]] <-dataclean.HH(H_ITN.list[[4]], hv227, hv005,'hv227', 'H_ITN_use')  
hv227.svyd13 <- svydesign.fun(H_ITN.list[[4]])

DS_ITN_H_13 <- result.fun('H_ITN_use', 'LGA','num_p', design=hv227.svyd13)
head(DS_ITN_H_13)

write.csv(DS_ITN_H_13, "DS_ITN_H_13.csv")



# 2010
H_ITN.list[[3]] <-dataclean.HH(H_ITN.list[[3]], hv227, hv005,'hv227', 'H_ITN_use')  
hv227.svyd10 <- svydesign.fun(H_ITN.list[[3]])

DS_ITN_H_10 <- result.fun('H_ITN_use', 'LGA','num_p', design=hv227.svyd10)
head(DS_ITN_H_10)

write.csv(DS_ITN_H_10, "DS_ITN_H_10.csv")



# 2008
H_ITN.list[[2]] <-dataclean.HH(H_ITN.list[[2]], hv227, hv005,'hv227', 'H_ITN_use')  
hv227.svyd08 <- svydesign.fun(H_ITN.list[[2]])

DS_ITN_H_08 <- result.fun('H_ITN_use', 'LGA','num_p', design=hv227.svyd08)
head(DS_ITN_H_08)

write.csv(DS_ITN_H_08, "DS_ITN_H_08.csv")



# 2003
H_ITN.list[[1]] <-dataclean.HH(H_ITN.list[[1]], hv227, hv005,'hv227', 'H_ITN_use')  
hv227.svyd03 <- svydesign.fun(H_ITN.list[[1]])

DS_ITN_H_03 <- result.fun('H_ITN_use', 'LGA','num_p', design=hv227.svyd03)
head(DS_ITN_H_03)

write.csv(DS_ITN_H_03, "DS_ITN_H_03.csv")



# cluster level estimates 

# 2015

clu_H_ITN_15 <- result.clu.fun('H_ITN_use', 'v001','num_p', design=hv227.svyd15,H_ITN.list[[5]])
head(clu_H_ITN_15)

write.csv(clu_H_ITN_15, "clu_H_ITN_15.csv")



# 2013

clu_H_ITN_13 <- result.clu.fun('H_ITN_use', 'v001','num_p', design=hv227.svyd13,H_ITN.list[[4]])
head(clu_H_ITN_13)

write.csv(clu_H_ITN_13, "clu_H_ITN_13.csv")



# 2010

clu_H_ITN_10 <- result.clu.fun('H_ITN_use', 'v001','num_p', design=hv227.svyd10,H_ITN.list[[3]])
head(clu_H_ITN_10)

write.csv(clu_H_ITN_10, "clu_H_ITN_10.csv")



# 2008

clu_H_ITN_08 <- result.clu.fun('H_ITN_use', 'v001','num_p', design=hv227.svyd08,H_ITN.list[[2]])
head(clu_H_ITN_08)

write.csv(clu_H_ITN_08, "clu_H_ITN_08.csv")



# 2003

clu_H_ITN_03 <- result.clu.fun('H_ITN_use', 'v001','num_p', design=hv227.svyd03,H_ITN.list[[1]])
head(clu_H_ITN_03)

write.csv(clu_H_ITN_03, "clu_H_ITN_03.csv")



##############################################################################################
# U5 ACT intake 
##############################################################################################


# DS-level estimates 

# 2015
ACT.list[[4]] <-dataclean(ACT.list[[4]], ml13e, v005,'ml13e', 'ACT_use')  
ml13e.svyd15 <- svydesign.fun(ACT.list[[4]])

DS_ACT_15 <- result.fun('ACT_use', 'LGA','num_p', design=ml13e.svyd15)
head(DS_ACT_15)

write.csv(DS_ACT_15, "DS_ACT_15.csv")


# 2013
ACT.list[[3]] <-dataclean(ACT.list[[3]], ml13e, v005,'ml13e', 'ACT_use')  
ml13e.svyd13 <- svydesign.fun(ACT.list[[3]])

DS_ACT_13 <- result.fun('ACT_use', 'LGA','num_p', design=ml13e.svyd13)
head(DS_ACT_13)

write.csv(DS_ACT_13, "DS_ACT_13.csv")


# 2010
ACT.list[[2]] <-dataclean(ACT.list[[2]], ml13e, v005,'ml13e', 'ACT_use')  
ml13e.svyd10 <- svydesign.fun(ACT.list[[2]])

DS_ACT_10 <- result.fun('ACT_use', 'LGA','num_p', design=ml13e.svyd10)
head(DS_ACT_10)

write.csv(DS_ACT_10, "DS_ACT_10.csv")


# 2008
ACT.list[[1]] <-dataclean(ACT.list[[1]], ml13e, v005,'ml13e', 'ACT_use')  
ml13e.svyd08 <- svydesign.fun(ACT.list[[1]])

DS_ACT_08 <- result.fun('ACT_use', 'LGA','num_p', design=ml13e.svyd08)
head(DS_ACT_08)

write.csv(DS_ACT_08, "DS_ACT_08.csv")




# cluster level estimates 

# 2015

clu_ACT_15 <- result.clu.fun('ACT_use', 'v001','num_p', design=ml13e.svyd15,ACT.list[[4]])
head(clu_ACT_15)

write.csv(clu_ACT_15, "clu_ACT_15.csv")


# 2013

clu_ACT_13 <- result.clu.fun('ACT_use', 'v001','num_p', design=ml13e.svyd13,ACT.list[[3]])
head(clu_ACT_13)

write.csv(clu_ACT_13, "clu_ACT_13.csv")


# 2010

clu_ACT_10 <- result.clu.fun('ACT_use', 'v001','num_p', design=ml13e.svyd10,ACT.list[[2]])
head(clu_ACT_10)

write.csv(clu_ACT_10, "clu_ACT_10.csv")


# 2008

clu_ACT_08 <- result.clu.fun('ACT_use', 'v001','num_p', design=ml13e.svyd10,ACT.list[[1]])
head(clu_ACT_08)

write.csv(clu_ACT_08, "clu_ACT_08.csv")



##############################################################################################
# IPTP 
##############################################################################################


# DS-level estimates 

# 2015
IPTP.list[[5]] <-dataclean(IPTP.list[[5]], m49a_1, v005,'m49a_1', 'IPTP')  
m49a_1.svyd15 <- svydesign.fun(IPTP.list[[5]])

DS_IPTP_15 <- result.fun('IPTP', 'sstate','num_p', design=m49a_1.svyd15)
head(DS_IPTP_15)

write.csv(DS_IPTP_15, "DS_IPTP_15.csv")



# 2013
IPTP.list[[4]] <-dataclean(IPTP.list[[4]], m49a_1, v005,'m49a_1', 'IPTP')  
m49a_1.svyd13 <- svydesign.fun(IPTP.list[[4]])

DS_IPTP_13 <- result.fun('IPTP', 'LGA','num_p', design=m49a_1.svyd13)
head(DS_IPTP_13)

write.csv(DS_IPTP_13, "DS_IPTP_13.csv")


# 2010 
IPTP.list[[3]] <-dataclean(IPTP.list[[3]], m49a_1, v005,'m49a_1', 'IPTP')  
m49a_1.svyd10 <- svydesign.fun(IPTP.list[[3]])

DS_IPTP_10 <- result.fun('IPTP', 'LGA','num_p', design=m49a_1.svyd10)
head(DS_IPTP_10)

write.csv(DS_IPTP_10, "DS_IPTP_10.csv")



# 2008
IPTP.list[[2]] <-dataclean(IPTP.list[[2]], m49a_1, v005,'m49a_1', 'IPTP')  
m49a_1.svyd08 <- svydesign.fun(IPTP.list[[2]])

DS_IPTP_08 <- result.fun('IPTP', 'LGA','num_p', design=m49a_1.svyd08)
head(DS_IPTP_08)

write.csv(DS_IPTP_08, "DS_IPTP_08.csv")



# 2003
IPTP.list[[1]] <-dataclean(IPTP.list[[1]], m49a_1, v005,'m49a_1', 'IPTP')  
m49a_1.svyd03 <- svydesign.fun(IPTP.list[[1]])

DS_IPTP_03 <- result.fun('IPTP', 'LGA','num_p', design=m49a_1.svyd03)
head(DS_IPTP_03)

write.csv(DS_IPTP_03, "DS_IPTP_03.csv")



# cluster level estimates 

# 2015

clu_IPTP_15 <- result.clu.fun('IPTP', 'v001','num_p', design=m49a_1.svyd15,IPTP.list[[5]])
head(clu_IPTP_15)

write.csv(clu_IPTP_15, "clu_IPTP_15.csv")



# 2013

clu_IPTP_13 <- result.clu.fun('IPTP', 'v001','num_p', design=m49a_1.svyd13,IPTP.list[[4]])
head(clu_IPTP_13)

write.csv(clu_IPTP_13, "clu_IPTP_13.csv")



# 2010

clu_IPTP_10 <- result.clu.fun('IPTP', 'v001','num_p', design=m49a_1.svyd10,IPTP.list[[3]])
head(clu_IPTP_10)

write.csv(clu_IPTP_10, "clu_IPTP_10.csv")


# 2008

clu_IPTP_08 <- result.clu.fun('IPTP', 'v001','num_p', design=m49a_1.svyd08,IPTP.list[[2]])
head(clu_IPTP_08)

write.csv(clu_IPTP_08, "clu_IPTP_08.csv")



# 2003

clu_IPTP_03 <- result.clu.fun('IPTP', 'v001','num_p', design=m49a_1.svyd03,IPTP.list[[1]])
head(clu_IPTP_03)

write.csv(clu_IPTP_03, "clu_IPTP_03.csv")


##############################################################################################
# IRS 
##############################################################################################


# DS-level estimates 

# 2015
IRS.list[[3]] <-dataclean.HH(IRS.list[[3]], hv253, hv005,'hv253', 'IRS')  
hv253.svyd15 <- svydesign.fun(IRS.list[[3]])

DS_IRS_15 <- result.fun('IRS', 'LGA','num_p', design=hv253.svyd15)
head(DS_IRS_15)

write.csv(DS_IRS_15, "DS_IRS_15.csv")


# 2013 
IRS.list[[2]] <-dataclean.HH(IRS.list[[2]], hv253, hv005,'hv253', 'IRS')  
hv253.svyd13 <- svydesign.fun(IRS.list[[2]])

DS_IRS_13 <- result.fun('IRS', 'LGA','num_p', design=hv253.svyd13)
head(DS_IRS_13)

write.csv(DS_IRS_13, "DS_IRS_13.csv")


# 2010
IRS.list[[1]] <-dataclean.HH(IRS.list[[1]], hv253, hv005,'hv253', 'IRS')  
hv253.svyd10 <- svydesign.fun(IRS.list[[1]])

DS_IRS_10 <- result.fun('IRS', 'LGA','num_p', design=hv253.svyd13)
head(DS_IRS_10)

write.csv(DS_IRS_10, "DS_IRS_10.csv")



# cluster level estimates 

# 2015

clu_IRS_15 <- result.clu.fun('IRS', 'v001','num_p', design=hv253.svyd15,IRS.list[[3]])
head(clu_IRS_15)

write.csv(clu_IRS_15, "clu_IRS_15.csv")


# 2013

clu_IRS_13 <- result.clu.fun('IRS', 'v001','num_p', design=hv253.svyd13,IRS.list[[2]])
head(clu_IRS_13)

write.csv(clu_IRS_13, "clu_IRS_13.csv")



# 2010

clu_IRS_10 <- result.clu.fun('IRS', 'v001','num_p', design=hv253.svyd10,IRS.list[[1]])
head(clu_IRS_10)

write.csv(clu_IRS_10, "clu_IRS_10.csv")


#######################################################################################
# ml101 adult ITN use 
######################################################################################


# DS-level estimates 

# 2015
ml101.list[[5]] <-dataclean(ml101.list[[5]], ml101, v005,'ml101', 'ITN_A_treated')  
ml101.svyd15 <- svydesign.fun(ml101.list[[5]])

DS_ITNT_15 <- result.fun('ITN_A_treated', 'LGA','num_p', design=ml101.svyd15)
head(DS_ITNT_15)

write.csv(DS_ITNT_15, "DS_ITNT_15.csv")



# 2013
ml101.list[[4]] <-dataclean(ml101.list[[4]], ml101, v005,'ml101', 'ITN_A_treated')  
ml101.svyd13 <- svydesign.fun(ml101.list[[4]])

DS_ITNT_13 <- result.fun('ITN_A_treated', 'LGA','num_p', design=ml101.svyd13)
head(DS_ITNT_13)

write.csv(DS_ITNT_13, "DS_ITNT_13.csv")



# 2010
ml101.list[[3]] <-dataclean(ml101.list[[3]], ml101, v005,'ml101', 'ITN_A_treated')  
ml101.svyd10 <- svydesign.fun(ml101.list[[3]])

DS_ITNT_10 <- result.fun('ITN_A_treated', 'LGA','num_p', design=ml101.svyd10)
head(DS_ITNT_10)

write.csv(DS_ITNT_10, "DS_ITNT_10.csv")


# 2008
ml101.list[[2]] <-dataclean(ml101.list[[2]], ml101, v005,'ml101', 'ITN_A_treated')  
ml101.svyd08 <- svydesign.fun(ml101.list[[2]])

DS_ITNT_08 <- result.fun('ITN_A_treated', 'LGA','num_p', design=ml101.svyd08)
head(DS_ITNT_08)

write.csv(DS_ITNT_08, "DS_ITNT_08.csv")


# 2003
ml101.list[[1]] <-dataclean(ml101.list[[1]], ml101, v005,'ml101', 'ITN_A_treated')  
ml101.svyd03 <- svydesign.fun(ml101.list[[1]])

DS_ITNT_03 <- result.fun('ITN_A_treated', 'LGA','num_p', design=ml101.svyd03)
head(DS_ITNT_03)

write.csv(DS_ITNT_03, "DS_ITNT_03.csv")


# cluster-level estimates 

# 2015

clu_ITNT_15 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=ml101.svyd15,ml101.list[[5]])
head(clu_ITNT_15)

write.csv(clu_ITNT_15, "clu_ITNT_15.csv")


# 2013

clu_ITNT_13 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=ml101.svyd13,ml101.list[[4]])
head(clu_ITNT_13)

write.csv(clu_ITNT_13, "clu_ITNT_13.csv")


# 2010

clu_ITNT_10 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=ml101.svyd10,ml101.list[[3]])
head(clu_ITNT_10)

write.csv(clu_ITNT_10, "clu_ITNT_10.csv")


# 2008

clu_ITNT_08 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=ml101.svyd08,ml101.list[[2]])
head(clu_ITNT_08)

write.csv(clu_ITNT_08, "clu_ITNT_08.csv")


# 2003

clu_ITNT_03 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=ml101.svyd03,ml101.list[[1]])
head(clu_ITNT_03)

write.csv(clu_ITNT_03, "clu_ITNT_03.csv")


###########################################################################################
#medical treatment for fever based on the PR files 
##########################################################################################


# DS-level estimates 

# 2015

hspr.list[[2]] <-dataclean.HH(hspr.list[[2]], med_fever, hv005,'med_fever', 'med_fever')  

over5_medf <- hspr.list[[2]] %>% filter(hv105 > 5) # over five 
u5_medf <- hspr.list[[2]] %>% filter(hv105 < 6)  # under five 

table(over5_medf$hv105)
table(u5_medf$hv105)

medf_pr.svyd15 <- svydesign.fun(over5_medf)
U_medf_pr.svyd15 <- svydesign.fun(u5_medf)

S_medpr_15 <- result.fun('med_fever', 'shstate','num_p', design=medf_pr.svyd15)
S_U_medpr_15 <- result.fun('med_fever', 'shstate','num_p', design=U_medf_pr.svyd15)

tail(S_medpr_15)
head(S_U_medpr_15)

write.csv(S_medpr_15, "State_over5.csv")
write.csv(S_U_medpr_15, "State_U5_2015.csv")




# 2010

hspr.list[[1]] <-dataclean.HH(hspr.list[[1]], med_fever, hv005,'med_fever', 'med_fever')  

over5_medf_10 <- hspr.list[[1]] %>% filter(hv105 > 5) # over five 
u5_medf_10 <- hspr.list[[1]] %>% filter(hv105 < 6)  # under five 

table(over5_medf_10$hv105)
table(u5_medf_10$hv105)

medf_pr.svyd10 <- svydesign.fun(over5_medf_10)
U_medf_pr.svyd10 <- svydesign.fun(u5_medf_10)

S_medpr_10 <- result.fun('med_fever', 'shstate','num_p', design=medf_pr.svyd10)
S_U_medpr_10 <- result.fun('med_fever', 'shstate','num_p', design=U_medf_pr.svyd10)

tail(S_medpr_10)
head(S_U_medpr_10)

write.csv(S_medpr_10, "State_over5_2010.csv")
write.csv(S_U_medpr_10, "State_U5_2010.csv")

