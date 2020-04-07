rm(list=ls())

setwd("~/Box/NU-malaria-team/data/nigeria_dhs") 
source("Nigeria functions.R")




x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer", "plotrix", "ggrepel", "sf", "shinyjs", "tmap", "knitr")
lapply(x, library, character.only = TRUE)#applying the library function to packages 
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed



## 1990 Treatment seeking for fever 
HSB_1990 <-read_csv("1990 HS.csv")
HSB90_long <- gather(HSB_1990, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)


new90.df <-HSB_1990[10:20,]

HSB90_long2 <- gather(new90.df, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)

HSB90.plot <-ggplot(data = HSB90_long2) + 
  geom_point(mapping = aes(x = factor(Number), y = measurement, color=HSB))

HSB90.plot 





## 2003 Treatment seeking for fever 

HSB_2003 <-read_csv("2003 HS.csv")
HSB03_long <- gather(HSB_2003, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)


new03.df <-HSB_2003[5:15,]

HSB03_long2 <- gather(new03.df, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)

HSB03.plot <-ggplot(data = HSB03_long2) + 
  geom_point(mapping = aes(x = factor(Number), y = measurement, color=HSB))

HSB03.plot



## 2008 Treatment seeking for fever 

HSB_2008 <-read_csv("2008 HS.csv")
HSB08_long <- gather(HSB_2008, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)


new08.df <-HSB_2008[20:30,]

HSB08_long2 <- gather(new08.df, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)

HSB08.plot <-ggplot(data = HSB08_long2) + 
  geom_point(mapping = aes(x = factor(Number), y = measurement, color=HSB))

HSB08.plot



## 2010 Treatment seeking for fever 

HSB_2010 <-read_csv("2010 HS.csv")
HSB10_long <- gather(HSB_2010, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)


new10.df <-HSB_2010[1:10,]

HSB10_long2 <- gather(new10.df, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)

HSB10.plot <-ggplot(data = HSB10_long2) + 
  geom_point(mapping = aes(x = factor(Number), y = measurement, color=HSB))

HSB10.plot


## 2013 Treatment seeking for fever 

HSB_2013 <-read_csv("2013 HS.csv")
HSB13_long <- gather(HSB_2013, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)


new13.df <-HSB_2013[1:10,]

HSB13_long2 <- gather(new13.df, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)

HSB13.plot <-ggplot(data = HSB13_long2) + 
  geom_point(mapping = aes(x = factor(Number), y = measurement, color=HSB))

HSB13.plot



## 2015 Treatment seeking for fever

HSB_2015 <-read_csv("2015 HS.csv")
HSB15_long <- gather(HSB_2015, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)


new15.df <-HSB_2015[1:10,]

HSB15_long2 <- gather(new15.df, HSB, measurement, `medical treatment for fever`:pri_med, factor_key=TRUE)

HSB15.plot <-ggplot(data = HSB15_long2) + 
  geom_point(mapping = aes(x = factor(Number), y = measurement, color=HSB))

HSB15.plot



## Mapping private health seeking 

# DHS datasets 
NGAfiles<-read.files(".*NGIR.*\\.DTA", ".*NGKR.*\\.DTA", ".*NGPR.*\\.DTA")

# cluster locations 
NGAshpfiles <- list.files(pattern="*FL.*\\.shp$", full.names=TRUE, recursive=T)
NGAshplist<-sapply(NGAshpfiles,shapefile, simplify = F)


# LGA shape file 
LGAshp <- readOGR("Nigeria LGAs shapefile 191016", layer ="NGA_LGAs", use_iconv=TRUE, encoding= "UTF-8")
LGAshp_sf <- st_as_sf(LGAshp)




# creating a variable that aggregates all U5 private health care seeking behavior 

NGAfiles[[2]]$pri_med <- rowSums(NGAfiles[[2]][,c('h32k', 'h32l', 'h32s', 'h32t', 'h32u', 'h32v', 'h32x')]) #1990 
table(NGAfiles[[2]]$pri_med)


NGAfiles[[5]]$pri_med <- rowSums(NGAfiles[[5]][, c('h32j', 'h32k', 'h32l', 'h32m', 'h32n', 'h32r', 'h32s', 'h32t', 'h32u', 'h32x')]) #2003 
NGAfiles[[5]][,"pri_med"] <-recoder(NGAfiles[[5]][,"pri_med"]) 
table(NGAfiles[[5]]$pri_med)


NGAfiles[[8]]$pri_med <- rowSums(NGAfiles[[8]][, c('h32j','h32k', 'h32l', 'h32m', 'h32n', 'h32o', 'h32r', 'h32s', 'h32t', 'h32x')]) #2008 
NGAfiles[[8]][,"pri_med"] <-recoder(NGAfiles[[8]][,"pri_med"]) 
table(NGAfiles[[8]]$pri_med)


NGAfiles[[11]]$pri_med <- rowSums(NGAfiles[[11]][, c('h32j', 'h32k', 'h32l', 'h32m', 'h32n', 'h32r', 'h32s', 'h32t', 'h32u','h32x')]) #2010 
NGAfiles[[11]][,"pri_med"] <-recoder(NGAfiles[[11]][,"pri_med"]) 
table(NGAfiles[[11]]$pri_med)


NGAfiles[[14]]$pri_med <- rowSums(NGAfiles[[14]][, c('h32j', 'h32k', 'h32l', 'h32m', 'h32n', 'h32r', 'h32s', 'h32v', 'h32w', 'h32x')]) #2013 
NGAfiles[[14]][,"pri_med"] <-recoder(NGAfiles[[14]][,"pri_med"]) 
table(NGAfiles[[14]]$pri_med)


NGAfiles[[17]]$pri_med <- rowSums(NGAfiles[[17]][, c('h32j', 'h32k', 'h32l', 'h32m', 'h32n', 'h32s', 'h32t', 'h32u', 'h32x')]) #2015 
NGAfiles[[17]][,"pri_med"] <-recoder(NGAfiles[[17]][,"pri_med"]) 
table(NGAfiles[[17]]$pri_med)




# U5 medical treatment for fever list for 1990, 2003, 2008, 2010, 2013, 2015
medfever.list <- list(NGAfiles[[2]], NGAfiles[[5]], NGAfiles[[8]], NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]])

# recoding U5 medical treatment  
# 2013 
medfever.list[[5]][,"h32z"] <-recoder(medfever.list[[5]][,"h32z"]) 


# 2010
medfever.list[[4]][,"h32z"] <-recoder(medfever.list[[4]][,"h32z"]) 






key_list <- purrr::map(NGAshplist, over.fun)

key_list[[1]]$v001<-NGAshplist[[1]]@data[,"DHSCLUST"] #1993 
key_list[[2]]$v001<-NGAshplist[[2]]@data[,"DHSCLUST"] #2003
key_list[[3]]$v001<-NGAshplist[[3]]@data[,"DHSCLUST"] #2008 
key_list[[4]]$v001<-NGAshplist[[4]]@data[,"DHSCLUST"] #2010 
key_list[[5]]$v001<-NGAshplist[[5]]@data[,"DHSCLUST"] #2013 
key_list[[6]]$v001<-NGAshplist[[6]]@data[,"DHSCLUST"] #2015 

medfever.list <-purrr::map2(medfever.list,key_list, left_join) #medfever datasets 
pri_medfever.list <- medfever.list



# DS-level estimates 

# 2015

pri_medfever.list[[6]] <-dataclean(pri_medfever.list[[6]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd15 <- svydesign.fun(pri_medfever.list[[6]])


DS_primed_15 <- result.fun('pri_med', 'LGA','num_p', design=primed.svyd15)
head(DS_primed_15)

write.csv(DS_primed_15, "DSfever_primed_15.csv")

# 2013 
pri_medfever.list[[5]] <-dataclean(pri_medfever.list[[5]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd13 <- svydesign.fun(pri_medfever.list[[5]])

DS_primed_13 <- result.fun('pri_med', 'LGA','num_p', design=primed.svyd13)
head(DS_primed_13)

write.csv(DS_primed_13, "DSfever_primed_13.csv")


# 2010 
pri_medfever.list[[4]] <-dataclean(pri_medfever.list[[4]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd10 <- svydesign.fun(pri_medfever.list[[4]])

DS_primed_10 <- result.fun('pri_med', 'LGA','num_p', design=primed.svyd10)
head(DS_primed_10)

write.csv(DS_primed_10, "DSfever_primed_10.csv")


# 2008
pri_medfever.list[[3]] <-dataclean(pri_medfever.list[[3]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd08 <- svydesign.fun(pri_medfever.list[[3]])

DS_primed_08 <- result.fun('pri_med', 'LGA','num_p', design=primed.svyd08)
head(DS_primed_08)

write.csv(DS_primed_08, "DSfever_primed_08.csv")


# 2003
pri_medfever.list[[2]] <-dataclean(pri_medfever.list[[2]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd03 <- svydesign.fun(pri_medfever.list[[2]])

DS_primed_03 <- result.fun('pri_med', 'LGA','num_p', design=primed.svyd03)
head(DS_primed_03)

write.csv(DS_primed_03, "DSfever_primed_03.csv")


# 1990
pri_medfever.list[[1]] <-dataclean(pri_medfever.list[[1]], pri_med, v005,'pri_med', 'pri_med')  
primed.svyd90 <- svydesign.fun(pri_medfever.list[[1]])

DS_primed_90 <- result.fun('pri_med', 'LGA','num_p', design=primed.svyd90)
head(DS_primed_90)

write.csv(DS_primed_90, "DSfever_primed_90.csv")



pri_med_DS <- list(DS_primed_90,DS_primed_03,DS_primed_08, DS_primed_10,DS_primed_13,DS_primed_15)
LGAshp_sf <- st_as_sf(LGAshp)
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

NGAshplist<-map(NGAshplist, st_as_sf)

clu_shp_primed <- map2(NGAshplist, clu_primed_ls, left_join)

pri_med_90 <-tmap.fun2(pri_med_sf[[1]], "pri_med", "Private HSB", 
                       "1990")

pri_med_03 <-tmap.fun2(pri_med_sf[[2]], "pri_med", "Private HSB", "2003")

pri_med_08 <-tmap.fun2(pri_med_sf[[3]], "pri_med", "Private HSB", "2008")

pri_med_10 <-tmap.fun2(pri_med_sf[[4]], "pri_med", "Private HSB", "2010")

pri_med_13 <-tmap.fun2(pri_med_sf[[5]], "pri_med", "Private HSB", "2013")

pri_med_15 <-tmap.fun2(pri_med_sf[[6]], "pri_med", "Private HSB", "2015")

all_private <- tmap_arrange(pri_med_90, pri_med_03, pri_med_08,pri_med_10, pri_med_13,pri_med_15)

all_private


