### ITN regression analysis
rm(list=ls())

## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer", "plotrix", "ggrepel", "sf", "shinyjs", "tmap", "knitr", "lme4")
lapply(x, library, character.only = TRUE) #applying the library function to packages

##################################################################################
# reading datasets #
##################################################################################

setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs")

BF_IR2010 <- read_dta("BF_2010_DHS_06192019/BFIR62DT/BFIR62FL.DTA")
BF_KR2010 <-  read_dta("BF_2010_DHS_06192019/BFKR62DT/BFKR62FL.DTA")
BF_IR2014 <- read_dta("BF_2014_MIS_06192019/BFIR71DT/BFIR71FL.DTA")
BF_KR2014 <- read_dta("BF_2014_MIS_06192019/BFKR71DT/BFKR71FL.DTA")
BF_IR2017 <- read_dta("BF_2017-18_MIS_07252019_1531_86355/BFIR7ADT/BFIR7AFL.DTA")
BF_KR2017 <- read_dta("BF_2017-18_MIS_07252019_1531_86355/BFKR7ADT/BFKR7AFL.DTA")

#GPS points
pts_10 <-readOGR("BF_2010_DHS_06192019/BFGE61FL", layer = "BFGE61FL")
pts_14 <-readOGR("BF_2014_MIS_06192019/BFGE71FL", layer = "BFGE71FL")
pts_17 <- readOGR("BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL", layer = "BFGE7AFL")

#DS file
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                   use_iconv=TRUE, encoding= "UTF-8")

#DS is in the utm + zone 30 & points is in long + lat. lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,crs(pts_10))


#check map projections again 
crs(DS_shape_W)
crs(pts_10)


#Check if the GPS points  are contained in the polygons 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2010 clusters')
plot(pts_10,add=T,col=4) #this is dhs 10

plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2014 clusters')
plot(pts_14,add=T,col=4) #this is dhs 14

plot(DS_shape_W,  main = 'Administrative boundary: Health Districts with DHS 2017 clusters')
plot(pts_17,add=T,col=4) #this is dhs 17

## Mapping points to inherit associated health district

#the dimensions of the pts_10 data is 573 by 20 and of the DS is 70 by 10, key_10 will be 573 by 10 
dim(pts_10@data) # this is for dhs 10
dim(DS_shape_W@data)
key_10<-over(SpatialPoints(coordinates(pts_10),proj4string = pts_10@proj4string), DS_shape_W)
dim(key_10)
length(unique(key_10$NOMDEP))

# add in the cluster variable
key_10$v001<-pts_10@data[,"DHSCLUST"]


#the dimensions of the pts_14 data is 252 by 20 and of the DS is 70 by 10, key_14 will be 252 by 10
dim(pts_14@data) # this is for mis 14 
dim(DS_shape_W@data)
key_14<-over(SpatialPoints(coordinates(pts_14),proj4string = pts_14@proj4string), DS_shape_W)
dim(key_14)
length(unique(key_14$NOMDEP))

#add in the cluster variable
key_14$v001<-pts_14@data[,"DHSCLUST"]


#the dimensions of the pts_17@data is 245 by 20 and district is 70 by 10, then the corresponding data frame will be 245 by 10
dim(pts_17@data)  # this is for mis 17 
dim(DS_shape_W@data)
key_17<-over(SpatialPoints(coordinates(pts_17),proj4string = pts_17@proj4string), DS_shape_W)
dim(key_17)
length(unique(key_17$NOMDEP))

#add in the cluster variable
key_17$v001<-pts_17@data[,"DHSCLUST"]

###################################################################################################
# creating IR datasets # 
##################################################################################################

#we start with the IR datasets 
#a few descriptive stats for IR dhs 2010  
dim(BF_IR2010)# how many rows?
dim(key_10) #how many columns?
length(unique(BF_IR2010$v001))
length(unique(key_10$v001)) #
BFIR2010_work<-BF_IR2010%>%left_join(key_10)
length(unique(BFIR2010_work$NOMDEP))

#calculating age and creating bednet variable for 2010 
BFIR2010_work$age <- BFIR2010_work$v008 - BFIR2010_work$v011
BFIR2010_work$bednet <- BFIR2010_work$ml101


#creating adult female dataset for 2010 
BFIR2010_A <- BFIR2010_work %>% dplyr::select(v001, v005, v006, v021, v022, NOMDEP, age, bednet) %>% 
  mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) %>% 
  mutate(zage = (age - mean(age)) / sd(age)) 


BFIR2010_A$month_A <- ifelse(BFIR2010_A$v006 ==5, 1, ifelse(BFIR2010_A$v006 ==6, 2, ifelse(BFIR2010_A$v006 == 7, 3, ifelse(BFIR2010_A$v006 == 8, 4, 
                ifelse(BFIR2010_A$v006 == 9, 5, ifelse(BFIR2010_A$v006 == 10, 6, ifelse(BFIR2010_A$v006 == 11, 7, ifelse(BFIR2010_A$v006 == 12, 8, NA))))))))


BFIR2010_A$month_A <- as.factor(BFIR2010_A$month_A)

class(BFIR2010_A$month_A)

#creating dataset to check trend in the r/ship of month and bednet coverage 
BFIR2010_check <- na.omit(BFIR2010_A)

BFIR2010_check <- BFIR2010_check%>% group_by(month_A) %>% 
  summarise(mean(bednet))
plot(BFIR2010_check$month_A, BFIR2010_check$`mean(bednet)`)


#a few descriptive stats for IR mis 2014 
dim(BF_IR2014)# how many rows?
dim(key_14) #how many columns?
length(unique(BF_IR2014$v001))
length(unique(key_14$v001)) #
BFIR2014_work<-BF_IR2014%>%left_join(key_14)

#calculating age and creating bednet variable for 2014 
BFIR2014_work$age <- BFIR2014_work$v008 - BFIR2014_work$v011
BFIR2014_work$bednet <- BFIR2014_work$ml101



#creating adult female dataset for 2014
BFIR2014_A <- BFIR2014_work %>% dplyr::select(v001, v005, v006, v021, v022, NOMDEP, age, bednet) %>% 
  mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) %>% 
  mutate(zage = (age - mean(age)) / sd(age)) 


#creating the month of survey variable 2014 
BFIR2014_A$month_A <- ifelse(BFIR2014_A$v006 ==9, 1, ifelse(BFIR2014_A$v006 ==10, 2, ifelse(BFIR2014_A$v006 == 11, 3, ifelse(BFIR2014_A$v006 == 12,4, 
                                                                                                                             NA))))
BFIR2014_A$month_A <- as.factor(BFIR2014_A$month_A)

class(BFIR2014_A$month_A)

#creating dataset to check trend in the r/ship of month and bednet coverage 2014 
BFIR2014_check <- na.omit(BFIR2014_A)

BFIR2014_check <- BFIR2014_check%>% group_by(month_A) %>% 
  summarise(mean(bednet))
plot(BFIR2014_check$month_A, BFIR2014_check$`mean(bednet)`)


#a few descriptive stats for IR mis 2017 
dim(BF_IR2017)# how many rows?
dim(key_17) #how many columns?
length(unique(BF_IR2017$v001))
length(unique(key_17$v001)) #
BFIR2017_work<-BF_IR2017%>%left_join(key_17)

#calculating age and creating bednet variable for 2017 
BFIR2017_work$age <- BFIR2017_work$v008 - BFIR2017_work$v011
BFIR2017_work$bednet <- BFIR2017_work$ml101



#creating adult female dataset for 2017
BFIR2017_A <- BFIR2017_work %>% dplyr::select(v001, v005, v006, v021, v022, NOMDEP, age, bednet) %>% 
  mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) %>% 
  mutate(zage = (age - mean(age)) / sd(age)) 

table(BFIR2017_A$v006)

#creating the month of survey variable 2017 
BFIR2017_A$month_A <- ifelse(BFIR2017_A$v006 ==1, 1, ifelse(BFIR2017_A$v006 ==2, 2, ifelse(BFIR2017_A$v006 == 3, 3, ifelse(BFIR2017_A$v006 == 11,4, 
                                                                                          ifelse(BFIR2017_A$v006 == 12, 5, NA)))))
BFIR2017_A$month_A <- as.factor(BFIR2017_A$month_A)

class(BFIR2017_A$month_A)

#creating dataset to check trend in the r/ship of month and bednet coverage 2017 
BFIR2017_check <- na.omit(BFIR2017_A)

BFIR2017_check <- BFIR2017_check%>% group_by(month_A) %>% 
  summarise(mean(bednet))
plot(BFIR2017_check$month_A, BFIR2017_check$`mean(bednet)`)


##############################################################################################################
# creating KR datasets # 
#############################################################################################################

#a few descriptive stats for KR dhs 2010  
dim(BF_KR2010)# how many rows?
dim(key_10) #how many columns?
length(unique(BF_KR2010$v001))
length(unique(key_10$v001)) #
BFKR2010_work<-BF_KR2010%>%left_join(key_10)

#calculating age and creating bednet variable 
BFKR2010_work$age <- BFKR2010_work$v008 - BFKR2010_work$b3
BFKR2010_work$bednet <- BFKR2010_work$ml0

#creating U5 dataset 
BFKR2010_fin <- BFKR2010_work %>% dplyr::select(v001, v005, v006, v021, v022, NOMDEP, age, bednet) %>% 
                               mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) %>% 
                               mutate(zage = (age - mean(age)) / sd(age)) 
                              
  
  
BFKR2010_fin$month_U5 <- ifelse(BFKR2010_fin$v006 ==5, 1, ifelse(BFKR2010_fin$v006 ==6, 2, ifelse(BFKR2010_fin$v006 == 7, 3, ifelse(BFKR2010_fin$v006 == 8, 4, 
                              ifelse(BFKR2010_fin$v006 == 9, 5, ifelse(BFKR2010_fin$v006 == 10, 6, ifelse(BFKR2010_fin$v006 == 11, 7, ifelse(BFKR2010_fin$v006 == 12, 8, NA))))))))

BFKR2010_fin$month_U5 <- as.factor(BFKR2010_fin$month_U5)

class(BFKR2010_fin$month_U5)

#creating dataset to check trend in the r/ship of month and bednet coverage 
BFKR2010_check <- na.omit(BFKR2010_fin)

BFKR2010_check <- BFKR2010_check%>% group_by(month_U5) %>% 
                    summarise(mean(bednet))
plot(BFKR2010_check$month_U5, BFKR2010_check$`mean(bednet)`)



#a few descriptive stats for KR dhs 2014  
dim(BF_KR2014)# how many rows?
dim(key_14) #how many columns?
length(unique(BF_KR2014$v001))
length(unique(key_14$v001)) #
BFKR2014_work<-BF_KR2014%>%left_join(key_14)

#calculating age and creating bednet variable 
BFKR2014_work$age <- BFKR2014_work$v008 - BFKR2014_work$b3
BFKR2014_work$bednet <- BFKR2014_work$ml0

#creating U5 dataset 
BFKR2014_fin <- BFKR2014_work %>% dplyr::select(v001, v005, v006, v021, v022, NOMDEP, age, bednet) %>% 
  mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) %>% 
  mutate(zage = (age - mean(age)) / sd(age)) 

table(BFKR2014_fin$v006)

BFKR2014_fin$month_U5 <- ifelse(BFKR2014_fin$v006 ==9, 1, ifelse(BFKR2014_fin$v006 ==10, 2, ifelse(BFKR2014_fin$v006 == 11, 3, ifelse(BFKR2014_fin$v006 == 12,4, 
                                                                                                                                NA))))

BFKR2014_fin$month_U5 <- as.factor(BFKR2014_fin$month_U5)

class(BFKR2014_fin$month_U5)

#creating dataset to check trend in the r/ship of month and bednet coverage 
BFKR2014_check <- na.omit(BFKR2014_fin)

BFKR2014_check <- BFKR2014_check%>% group_by(month_U5) %>% 
  summarise(mean(bednet))
plot(BFKR2014_check$month_U5, BFKR2014_check$`mean(bednet)`)


#a few descriptive stats for KR dhs 2017  
dim(BF_KR2017)# how many rows?
dim(key_17) #how many columns?
length(unique(BF_KR2017$v001))
length(unique(key_17$v001)) #
BFKR2017_work<-BF_KR2017%>%left_join(key_17)

#calculating age and creating bednet variable 
BFKR2017_work$age <- BFKR2017_work$v008 - BFKR2017_work$b3
BFKR2017_work$bednet <- BFKR2017_work$ml0

#creating U5 dataset 
BFKR2017_fin <- BFKR2017_work %>% dplyr::select(v001, v005, v006, v021, v022, NOMDEP, age, bednet) %>% 
  mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) %>% 
  mutate(zage = (age - mean(age)) / sd(age)) 

table(BFKR2017_fin$v006)

BFKR2017_fin$month_U5 <- ifelse(BFKR2017_fin$v006 ==1, 1, ifelse(BFKR2017_fin$v006 ==2, 2, ifelse(BFKR2017_fin$v006 == 3, 3, ifelse(BFKR2017_fin$v006 == 11,4, 
                                                                                          ifelse(BFKR2017_fin$v006 == 12, 5, NA)))))

BFKR2017_fin$month_U5 <- as.factor(BFKR2017_fin$month_U5)

class(BFKR2017_fin$month_U5)

#creating dataset to check trend in the r/ship of month and bednet coverage 
BFKR2017_check <- na.omit(BFKR2017_fin)

BFKR2017_check <- BFKR2017_check%>% group_by(month_U5) %>% 
  summarise(mean(bednet))
plot(BFKR2017_check$month_U5, BFKR2017_check$`mean(bednet)`)



##############################################################################################
# model fitting #
#############################################################################################

#fitting the model for under-fives. The reference group needs to be changed to the category with the highest # of observations for the model to converge
BFKR2010_fin <- within(BFKR2010_fin, month_U5 <- relevel(month_U5, ref = "5"))
cmod_lme4_L <- glmer(bednet~zage+month_U5+(1|v021)+(1|NOMDEP),data=BFKR2010_fin,
                     family=binomial)

cmod_lme4_L14 <- glmer(bednet~zage+month_U5+(1|v021)+(1|NOMDEP),data=BFKR2014_fin,
                     family=binomial)


cmod_lme4_L17 <- glmer(bednet~zage+month_U5+(1|v021)+(1|NOMDEP),data=BFKR2017_fin,
                       family=binomial)
#model for adult women
BFIR2010_A <- within(BFIR2010_A, month_A <- relevel(month_A, ref = "5")) #2010
cmod_lme4_A <- glmer(bednet~zage+month_A+(1|v021)+(1|NOMDEP),data=BFIR2010_A,
                     family=binomial)

cmod_lme4_A14 <- glmer(bednet~zage+month_A+(1|v021)+(1|NOMDEP),data=BFIR2014_A,#2014
                     family=binomial)

cmod_lme4_A17 <- glmer(bednet~zage+month_A+(1|v021)+(1|NOMDEP),data=BFIR2017_A,#2017
                       family=binomial)

#printing model results
print(summary(cmod_lme4_L),correlation=FALSE) #U5 2010 
print(summary(cmod_lme4_L14),correlation=FALSE) #U5 2014
print(summary(cmod_lme4_L17),correlation=FALSE) #U5 2017
print(summary(cmod_lme4_A),correlation=FALSE) #adults 2010 
print(summary(cmod_lme4_A14),correlation=FALSE) #adults 2014
print(summary(cmod_lme4_A17),correlation=FALSE) #adults 2017


#fixed effect coefficients 
#fixef(cmod_lme4_L)

#individual level predicted probabilities and mean overall mean proportion 
cpred1 <- predict(cmod_lme4_L, type="response")
mean(cpred1)
    
#putting the model random effects coefficient in a list and converting to dataframe 
y <-coef(cmod_lme4_L) #U5 2010
z <-coef(cmod_lme4_L14) #U5 2014 
u <- coef(cmod_lme4_L17) #U5 2017
A <-coef(cmod_lme4_A) #adults 2010
B <-coef(cmod_lme4_A14) #adults 2014
C <-coef(cmod_lme4_A17) #adults 2017

NOMDEP_U5 <-as.data.frame(y$NOMDEP)
NOMDEP_U514 <- as.data.frame(z$NOMDEP)
NOMDEP_U517 <- as.data.frame(u$NOMDEP)
NOMDEP_A <-as.data.frame(A$NOMDEP)
NOMDEP_A14 <- as.data.frame(B$NOMDEP)
NOMDEP_A17 <- as.data.frame(C$NOMDEP)

#function to convert logit to probability 
expit<-function(x){
  exp(x)/(1+exp(x))
}


###ITN coverage by month for U5 2010

NOMDEP_U5$exp_may <- expit(rowSums(NOMDEP_U5[, 1:3]))

NOMDEP_U5$exp_june <- expit(rowSums(NOMDEP_U5[, c(1,2,4)]))

NOMDEP_U5$exp_july <- expit(rowSums(NOMDEP_U5[, c(1,2,5)]))

NOMDEP_U5$exp_august <- expit(rowSums(NOMDEP_U5[, c(1,2,6)]))

NOMDEP_U5$exp_october <- expit(rowSums(NOMDEP_U5[, c(1,2,7)]))

NOMDEP_U5$exp_november <- expit(rowSums(NOMDEP_U5[, c(1,2,8)]))

NOMDEP_U5$exp_december <- expit(rowSums(NOMDEP_U5[, c(1,2,9)]))

write.csv(NOMDEP_U5, file = "ITN_U5_2010.csv")


###ITN coverage by month for U5 2014

NOMDEP_U514$exp_october <- expit(rowSums(NOMDEP_U514[, 1:3]))

NOMDEP_U514$exp_november <- expit(rowSums(NOMDEP_U514[, c(1,2,4)]))

NOMDEP_U514$exp_december <- expit(rowSums(NOMDEP_U514[, c(1,2,5)]))

write.csv(NOMDEP_U514, file = "ITN_U5_2014.csv")


###ITN coverage by month for U5 2017

NOMDEP_U517$exp_february <- expit(rowSums(NOMDEP_U517[, 1:3]))

NOMDEP_U517$exp_march <- expit(rowSums(NOMDEP_U517[, c(1,2,4)]))

NOMDEP_U517$exp_november <- expit(rowSums(NOMDEP_U517[, c(1,2,5)]))

NOMDEP_U517$exp_december <- expit(rowSums(NOMDEP_U517[, c(1,2,6)]))

write.csv(NOMDEP_U517, file = "ITN_U5_2017.csv")


###ITN coverage by month for adults 2010

NOMDEP_A$exp_may <- expit(rowSums(NOMDEP_A[, 1:3]))

NOMDEP_A$exp_june <- expit(rowSums(NOMDEP_A[, c(1,2,4)]))

NOMDEP_A$exp_july <- expit(rowSums(NOMDEP_A[, c(1,2,5)]))

NOMDEP_A$exp_august <- expit(rowSums(NOMDEP_A[, c(1,2,6)]))

NOMDEP_A$exp_october <- expit(rowSums(NOMDEP_A[, c(1,2,7)]))

NOMDEP_A$exp_november <- expit(rowSums(NOMDEP_A[, c(1,2,8)]))

NOMDEP_A$exp_december <- expit(rowSums(NOMDEP_A[, c(1,2,9)]))

write.csv(NOMDEP_A, file = "ITN_Adult_2010.csv")



###ITN coverage by month for adults 2014

NOMDEP_A14$exp_october <- expit(rowSums(NOMDEP_A14[, 1:3]))

NOMDEP_A14$exp_november <- expit(rowSums(NOMDEP_A14[, c(1,2,4)]))

NOMDEP_A14$exp_december <- expit(rowSums(NOMDEP_A14[, c(1,2,5)]))

write.csv(NOMDEP_A14, file = "ITN_Adult_2014.csv")


###ITN coverage by month for adults 2017

NOMDEP_A17$exp_february <- expit(rowSums(NOMDEP_A17[, 1:3]))

NOMDEP_A17$exp_march <- expit(rowSums(NOMDEP_A17[, c(1,2,4)]))

NOMDEP_A17$exp_november <- expit(rowSums(NOMDEP_A17[, c(1,2,5)]))

NOMDEP_A17$exp_december <- expit(rowSums(NOMDEP_A17[, c(1,2,6)]))

write.csv(NOMDEP_A17, file = "ITN_Adult_2017.csv")
