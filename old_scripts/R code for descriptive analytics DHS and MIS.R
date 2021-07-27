#############################################################################################
####
#### Code for exploration and descriptive analysis of malaria-related variables in Nigeria
#### 
#### Last Updated: August 2, 2019
####
#### Author:Ifeoma Ozodiegwu

#################################
# -- clearing out the memory -- #
#################################
rm(list=ls())

#######################################
#Getting and setting working directory#
#######################################
getwd()
setwd("/Users/ifeomaozodiegwu/Box/NU-malaria-team/data/burkina_dhs")
setwd("C:/Users/ido0493/Box/NU-malaria-team/data/burkina_dhs")

############################################
# -- Reading in the Libraries -- #
############################################
library(survey);library(haven);library(tidyr);library(ggplot2); library(dplyr); library(purrr);
library(summarytools); library(stringr); library(sp); library(rgdal); library(raster); 
library(lubridate);library(RColorBrewer); library(plotrix); library(ggrepel)
library(sf)
####devtools::install_github("geocompr/geocompkg") #check the geocomputation manual for improving code 

####capabilities() #use this in mac to troubleshoot summarytools 

###removes the plyr package that was attached earlier.Dlyr does not work well when plyr is attached.  
detach("package:plyr", unload=TRUE)


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
bf17mis <- `BF_2017-18_MIS_07252019_1531_86355/BFIR7ADT/BFIR7AFL`
bf14mis <- `BF_2014_MIS_06192019/BFIR70DT/BFIR70FL`
bf10dhs <- `BF_2010_DHS_06192019/BFIR62DT/BFIR62FL`
bf98dhs <- `BF_1998-99_DHS_06192019/BFIR31DT/BFIR31FL`
bf93dhs <-`BF_1993_DHS_06192019/BFIR21DT/BFIR21FL`

###############################################################################################
#data exploration - Let's see what malaria-related variables are in the individual recode files
###############################################################################################

####This function provides a logical response to whether a column exists in a dataset 
fun0 <- function(df, ...) {
   c(...) %in% colnames(df) 
   }

####We start with the 2017 BF MIS and assess what malaria variables are within the dataset  
fun0(bf17mis, 'b2_01', 'ml0_1', 's405a', 's405b', 'h22_1', 'h32z_1', 'ml13e_1', 'v024')


####This function selects variables from a df that starts with specified names and creates a new dataframe 
fun1 <- function(df, ...){
  df2 <- df %>% 
    dplyr::select(map(c(...), 
               starts_with, 
               vars = colnames(.)) %>% 
           unlist()) 
}     
 
####Applying the function to each dataset 
###We start with the 2017 BF MIS dataset

bf17mis$MM = bf17mis$v008 - ((bf17mis$v007 - 1900) * 12)
table(bf17mis$MM)
mis17df <- fun1(bf17mis, 'b2', 'ml0', 'h22', 'h32z', 'ml13e', 'v024', 'MM', 'v007') 

####these are some of the important malaria variables 
####malaria_mis_var <- fun1(bmis_14, 'b2', 'ml0', 's405a', 's405b', 'h22', 'h32z', 'ml13e')

###creating summary table with summary tools 
view(dfSummary(mis17df))

###################################################################
#Lets's start the analysis at the subnational level 
###################################################################
####first we recode variables using function: 3 = only untreated nets, 0 = no nets, 1 = only treated nets 
#### in original dataset. Recode to 1 = only treated nets and 0 = no treated nets 
recoder <- function(x){
  if_else(x == 3 | x == 0, 0, 1)
}

####next we apply to dataset for ml0 variables 
mis17_work <- bf17mis %>%
  mutate_at(vars(contains('ml0')), recoder)

####checking to see how recoding performed 
table(mis17_work$ml0_3)
table(bf17mis$ml0_3)

###################################################################
#We read in the GPS points and shapefiles
###################################################################
####GPS points 
pts<- readOGR("BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL", layer = "BFGE7AFL")
####shapefiles for the admins (health districts is the priority) 
shape0<- shapefile("burkina_shapefiles/BFA_adm_shp/BFA_adm0")
shape1<- readOGR("burkina_shapefiles/BFA_adm_shp", layer = "BFA_adm1",  
                 use_iconv=TRUE, encoding= "UTF-8" )
shape2<- readOGR("burkina_shapefiles/BFA_adm_shp", layer ="BFA_adm2",
                 use_iconv=TRUE, encoding= "UTF-8")
DS_shape<- readOGR("burkina_shapefiles/Health Districts Burkina", layer ="70ds_",
                 use_iconv=TRUE, encoding= "UTF-8")


###Adding a row number to the DS_shape object, will be handy for plotting later on
DS_shape@data$row_num<-1:nrow(DS_shape@data)
pts@data$row_num <- 1:nrow(pts@data)
###################################################################
#Quality checks and reading in new columns 
##################################################################
####check the map projections to see if they are the same 
crs(DS_shape)
extent(DS_shape)
crs(pts)
extent(pts)

####shows different map projections. DS is in the utm + zone 30 & points is in long + lat 
####lets reproject the DS_shape to long & lat 
DS_shape_W <- spTransform(DS_shape,
                              crs(pts))
####check map projections again 
crs(DS_shape_W)
crs(pts)
####everything looks good now 

#### Check if the GPS points  are contained in the polygons 
plot(DS_shape_W,  main = 'Administrative boundary: Health Districts')
plot(pts,add=T,col=4)
#### everything looks good 
# Now add name of the individual regions to the map to cross-check which admin areas have no clusters
text(DS_shape_W, DS_shape_W@data$NOMDEP, cex=0.75)
# Toma has no clusters, some health districts have really few clusters   

####set up an empty dataframe for the health district estimates
DS_merge<-data.frame(NOMDEP=DS_shape_W@data[,"NOMDEP"])
pts_merge<-data.frame(v001=pts@data$DHSCLUST, DHSCLUST=pts@data$DHSCLUST)
# this way there will be a row in the data for the health districts with no clusters

#### This code associates each point to be associated with a health district polygon. So, if the 
####pts@data is 245 by 20 and district is 70 by 11, then the corresponding x data frame will be 245 by 11
dim(pts@data)
dim(DS_shape_W@data)
key<-over(SpatialPoints(coordinates(pts),proj4string = pts@proj4string), DS_shape_W)
dim(key)
unique(key$NOMDEP)
# add in the cluster variable
key$v001<-pts@data[,"DHSCLUST"]

################################################################################################
# --Now let's join the MIS data to the key that contains the names of each health district-- #
################################################################################################
# a few descriptive stats
dim(mis17_work)# how many rows?
dim(key) #how many columns?
summary(mis17_work$v001)# the cluster variable
summary(key$v001)
length(unique(mis17_work$v001))
length(unique(key$v001)) # it looks like the cluster variables match!
mis17_work<-mis17_work%>%left_join(key)
# should still have the same number of rows and a few new columns
dim(mis17_work)

#######################################################################################
##Changing data into long format to aggregate observations for LLINS across children 
#######################################################################################
#first we select relevant variables 
mis17<- dplyr::select(mis17_work, b2_01:b2_04, ml0_1:ml0_4, caseid, v001, v005, v021, v022, v024,NOMDEP)


mis17_long <- gather(mis17, key = Bednet_Status, value = LLIN_use, 
             ml0_1:ml0_4)

####Checking the validity of the aggregated variables
table(mis17_long$LLIN_use)
length(unique(mis17_long$caseid))
summary(mis17_long$LLIN_use)

##########################################################################
# -- setting up the survey related variables and survey design object -- #
##########################################################################
mis17_survey<-mis17_long %>%mutate(wt=v005/1000000,
                  strat=v022,
                  id=v021)%>%
                filter(!is.na(wt))%>% 
                filter(!is.na(LLIN_use))

my.svydesign <- svydesign(id= ~id,
                          strata=~strat,nest=T, 
                       weights= ~wt, data=mis17_survey)
#number of kids
mis17_survey$num_kids <- 1
table(mis17_survey$num_kids)
#####################################
# -- District level estimates -- #
#####################################

####This calculates the proportion of children that slept under a bednet by health district burkina faso 2017 MIS 
system.time({
Bednet<-svyby(formula=~LLIN_use,# which variable do we want estimate
                 by=~NOMDEP, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=my.svydesign, # my svy design object
                 na.rm=T)
}) # this one takes 25 seconds on my desktop


system.time({
num_clu<-svyby(formula=~num_kids,# which variable do we want estimate
                by=~v001, # by which variable
                FUN=svytotal, # what is the function (svymean gives the horvitz thompson estimate and variance)
                design=my.svydesign, # my svy design object
                na.rm=T)
}) # this one takes 25 seconds on my desktop

system.time({
 Bednet_clu<-svyby(formula=~LLIN_use,# which variable do we want estimate
                 by=~v001, # by which variable
                 FUN=svymean, # what is the function (svymean gives the horvitz thompson estimate and variance)
                 design=my.svydesign, # my svy design object
                 na.rm=T)
 

}) # this one takes 25 seconds on my desktop
colnames(Bednet_clu)[3]<- "standard_error"

###we want to merge with the full list of health districts
dim(DS_merge)
DS_merge_n<-DS_merge%>%left_join(Bednet)%>%
            mutate(LLIN_use_n = round(LLIN_use* 100,0))

pts_estimates<-num_clu%>%left_join(Bednet_clu) 
pts_merge_n<-pts_merge%>%left_join(pts_estimates)%>%
             mutate(DHSCLUST = pts_merge$v001)%>%
            mutate(`Number of kids` = round(num_kids, 0))

      

write.csv(DS_merge_n,file="outputs/LLIN_use_BF17DS.csv")

#####################################
# -- Now let's map our findings -- #
#####################################
#### using sf package to read in DS
DSshape_sf <- st_read("burkina_shapefiles/Health Districts Burkina")
pt_sf <- st_read("BF_2017-18_MIS_07252019_1531_86355/BFGE7AFL")


####merging the LLIN stats to with the sf DS shape file 
DS_shape_n <- merge(DSshape_sf, DS_merge_n, by = "NOMDEP")
pt_n <- merge(pt_sf,pts_merge_n, by = "DHSCLUST")
         
pt_n$`Number of kids`     

####making the map. I need to decide whether to use geom_sf_text or geom_text_repel. Both are not giving me 
####the results that I need. This section is a work in progress 
library(tmap)

qtm(DS_shape_n, fill = "LLIN_use", text = "NOMDEP", text.size = "AREA", root = 4, 
    fill.title = "LLIN use among children under age 5 by Health Districts (2017)", 
    fill.textNA = "Health districts with no information")

tm_shape(DS_shape_n) + 
  tm_polygons(col = "LLIN_use", textNA = "Health districts with no information", title = "LLIN use prevalence",
              palette = "RdYlBu")+
  tm_layout(main.title="LLIN use among children under age 5 by Health Districts (2017)",
            title.position = c("center", "top"))+
  tm_text("NOMDEP", size=0.5, root = 4)+ 
  tm_shape(pt_n)+
  tm_bubbles(size="Number of kids", col = "LLIN_use", border.col= "black", palette="RdYlBu")

tmap_tip(latest.version = FALSE)
tm_fill("LLIN_use", textNA = "Health districts with no information", title = "LLIN use prevalence")+
  
  library(shinyjs)
  tmaptools::palette_explorer()

tmap_format()
 
ggplot(DS_shape_n) +
  geom_sf(aes(fill = LLIN_use))+ 
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) + #this changes the color of the fill from the defaul
  geom_sf_text(aes(label = NOMDEP), colour = "black", size="Shape_Area") +
  labs(title = "LLIN use among children under age 5 by Health Districts (2017)")+ 
  theme(plot.title = element_text(hjust = 0.5))
 
 ###geom_sf_text(aes(label = NOMDEP), colour = "black", size=2.5)+ #this sets up, colors and sizes the labels +
  ### ggrepel::geom_text_repel(
    # data = DS_shape_n,
    # aes(label = NOMDEP, geometry = geometry), stat = "sf_coordinates",  min.segment.length = 2, size=2.5)

ggrepel::geom_text_repel(aes(label = NOMDEP, geometry = geometry), stat = "sf_coordinates", 
                         min.segment.length = 2, size=Shape_Area)+
  
##what I am trying to accomplish can be done by tm_text https://rdrr.io/cran/tmap/man/tm_text.html
##check tm_map package in geocomputation. see example: 
  ###https://mran.revolutionanalytics.com/snapshot/2015-07-13/web/packages/tmap/vignettes/tmap-nutshell.html
 
#we need to create code to repel the names that are too long and the district size is too small.
#first let's find out where the row numbers for the names 

#what row number is KV? Infact, let's create a function? 
DS_shape_n[which(DS_shape_n$NOMDEP == "KARANGASSO - VIGUE"),]
#checking to see if it is there 
DS_shape_n[30,]

DS_shape_n[which(DS_shape_n$NOMDEP == "SIG-NONGHIN"),]
DS_shape_n[57,]

ggplot(DS_shape_n) +
  geom_sf(aes(fill = LLIN_use)) +
  ggrepel::geom_text_repel(
    data = DS_shape_n,
    aes(label = NOMDEP, geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 1,
    colour = "magenta",
    segment.colour = "magenta" )


#function does not work. Let me get back to it
fun3 <- function(df, x, ...){
    df[which(x == ...),]
} 
 
y <- c("KARANGASSO - VIGUE", "DORI")
s <- fun3(DS_shape_n, DS_shape_n$NOMDEP, "KARANGASSO - VIGUE", "DORI")
 
####https://sarahleejane.github.io/learning/r/2014/09/21/plotting-data-points-on-maps-with-r.html 
#try this 

####https://yutani.rbind.io/post/geom-sf-text-and-geom-sf-label-are-coming/
####https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
####http://rstudio-pubs-static.s3.amazonaws.com/3355_d3f08cb2f71f44f2bbec8b52f0e5b5e7.html

###########################################################################################################
#---let's try this with ggplot ---# this did not work
###########################################################################################################
DSshape_sf <- st_read("burkina_shapefiles/Health Districts Burkina")%>% 
              mutate(
                CENTROID = map(geometry, st_centroid),
                COORDS = map(CENTROID, st_coordinates),
                COORDS_X = map_dbl(COORDS, 1),
                COORDS_Y = map_dbl(COORDS, 2)
                  )


DSshape_sf$nudge_x <- 0
DSshape_sf$nudge_y <- 0


x_range <- abs(Reduce("-", range(DSshape_sf$COORDS_X)))
y_range <- abs(Reduce("-", range(DSshape_sf$COORDS_Y)))

ix <- DSshape_sf$NOMDEP %in% c("NONGR-MASSOUM", "BASKUY")
DSshape_sf$nudge_x[ix] <- 1 * 0.15 * x_range
DSshape_sf$nudge_y[ix] <- -1 * 0.15 * y_range

ggplot(data = DSshape_sf) +
  geom_sf() +
  geom_text_repel(
    mapping = aes(
      x = COORDS_X,
      y = COORDS_Y,
      label = NOMDEP
    ),
    nudge_x = DSshape_sf$nudge_x,
    nudge_y = DSshape_sf$nudge_y,
    size = 3,
    min.segment.length = 0,
    point.padding = NA,
    segment.color = "blue"
  ) #+
  #coord_sf(crs = st_crs(DSshape_sf), datum = NA) +
  #theme_void() +
 # xlim(min(DSshape_sf$COORDS_X) * 1.1, max(DSshape_sf$COORDS_X) * 1.15)

length(DSshape_sf$COORDS_X)

length(DSshape_sf$COORDS_Y)

table(DSshape_sf$nudge_x)
length(DSshape_sf$nudge_y)