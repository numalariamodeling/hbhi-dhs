### ITN regression analysis
rm(list=ls())

## Reading in the necessary libraries 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer", "plotrix", "ggrepel", "sf", "shinyjs", "tmap", "knitr", "lme4")
lapply(x, library, character.only = TRUE) #applying the library function to packages

setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs")
BF_IR2010 <- read_dta("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs/BF_2010_DHS_06192019/BFIR62DT/BFIR62FL.DTA")
BF_KR2010 <-  read_dta("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs/BF_2010_DHS_06192019/BFKR62DT/BFKR62FL.DTA")


#GPS points
pts_10 <-readOGR("BF_2010_DHS_06192019/BFGE61FL", layer = "BFGE61FL")

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


#we start with the IR datasets 
#a few descriptive stats for IR dhs 2010  
dim(BF_IR2010)# how many rows?
dim(key_10) #how many columns?
length(unique(BF_IR2010$v001))
length(unique(key_10$v001)) #
BFIR2010_work<-BF_IR2010%>%left_join(key_10)

length(unique(BFIR2010_work$NOMDEP))

#calculating age and creating bednet variable 
BFIR2010_work$age <- BFIR2010_work$v008 - BFIR2010_work$v011
BFIR2010_work$bednet <- BFIR2010_work$ml101

#creating adult female dataset 
BFIR2010_fin <- BFIR2010_work %>% dplyr::select(v001, v005, v006, v021, v022, NOMDEP, age, bednet)
table(BFIR2010_work$age)


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
                               mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) 
table(BFKR2010_work$age)


#Combining the two data frames to create the analysis  data frame  
dhs_2010 <- rbind(BFIR2010_fin, BFKR2010_fin)

#recode variables 
dhs_2010<-dhs_2010%>%mutate(age_cat=ifelse(age <=59,1,0), #u5 is 1 and adult is 0
                  age_cat=ifelse(is.na(age),NA,age_cat)) %>%
                  mutate(bednet = ifelse(bednet == 3 | bednet == 0, 0, 1)) %>% 
                  mutate(season = ifelse(v006 > 5 & v006 <= 10, 1, 0)) %>% 
                  mutate(nat_at =ifelse(v006 == 5, 0.269108946, ifelse(v006 == 6,0.284976243, ifelse(v006 == 7, 0.306517415, 
                  ifelse(v006 == 8, 0.338060321, ifelse(v006 == 9, 0.370394898, ifelse(v006 == 10, 0.422254006, 
                  ifelse(v006 == 11,0.505268542, ifelse(v006 == 12, 0.589115732, NA))))))))) %>% 
                  mutate(zage = (age - mean(age)) / sd(age))



dhs_2010$month <- ifelse(dhs_2010$v006 == 5, 1, ifelse(dhs_2010$v006 == 6, 2, ifelse(dhs_2010$v006 == 7, 3, ifelse(dhs_2010$v006 == 8, 4, ifelse(dhs_2010$v006 == 9, 5,
                                                    ifelse(dhs_2010$v006 == 10, 6, ifelse(dhs_2010$v006 == 11, 7, ifelse(dhs_2010$v006 == 12, 8, NA))))))))

dhs_2010$month <- as.factor(dhs_2010$month)

#exploratory plots 
ggplot(dhs_2010,aes(x=month,y=bednet,colour=NOMDEP))+geom_point()

ggplot(dhs_2010,aes(x=age,y=bednet))+
  stat_summary(fun.data=mean_cl_boot,size=2)+
  ylim(c(0,1))

ggplot(dhs_2010,aes(x=age,y=bednet,colour=NOMDEP,group=NOMDEP))+
  stat_summary(fun.y=sum,geom="line",alpha=0.4)+
  stat_summary(fun.y=sum,geom="point",alpha=0.7,
               position=position_dodge(width=0.25))

nat_full <- dhs_2010$nat_at[dhs_2010$NOMDEP]

#fitting the model
cmod_lme4_L <- glmer(bednet~zage+month+(1|v021)+(1|NOMDEP),data=dhs_2010,
                     family=binomial)

#printing model results
print(summary(cmod_lme4_L),correlation=FALSE)

#fixed effect coefficients 
fixef(cmod_lme4_L)

#individual level predicted probabilities and mean overall mean proportion 
cpred1 <- predict(cmod_lme4_L, type="response")
mean(cpred1)
    
#putting the model random effects coefficient in a list and converting to dataframe 
y <-coef(cmod_lme4_L)

NOMDEP <-as.data.frame(y$NOMDEP)


#function to convert logit to probability 
expit<-function(x){
  exp(x)/(1+exp(x))
}





