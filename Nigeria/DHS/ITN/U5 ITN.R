############################################################################################################
# Creating file list for analysis  
############################################################################################################

# U5 ITN list for 2003, 2008, 2010, 2013, 2015 and 2018 
ITN.list <- list(NGAfiles[[5]], NGAfiles[[8]], NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]], NGAfiles[[20]])



#############################################################################################################
# Recoding analysis files 
############################################################################################################
# recoding U5 ITN use

ITN.list <- map(ITN.list, recoder.ml0)


# key list for ITN
keysITN <- list(key_list[[2]], key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]], key_list[[7]]) #changing to a list of keys 

ITN.list <-map2(ITN.list,keysITN, left_join) #ITN datasets 




#####################################################################################################
# U5 ITN use prevalence 
####################################################################################################
# DS-level estimates 

look_for(ITN.list[[5]], "state")

# 2018
ITN.list[[6]] <-dataclean(ITN.list[[6]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd18 <- svydesign.fun(ITN.list[[6]])


DS_ITN_pre_18 <- result.fun('U5_ITN_use', 'LGA','num_p', design=ml0.svyd18)
head(DS_ITN_pre_18)

write.csv(DS_ITN_pre_18, "results/DS_ITN_pre_18.csv")



# 2015
ITN.list[[5]] <-dataclean(ITN.list[[5]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd15 <- svydesign.fun(ITN.list[[5]])


DS_ITN_pre_15 <- result.fun('U5_ITN_use', 'LGA','num_p', design=ml0.svyd15)
head(DS_ITN_pre_15)

write.csv(DS_ITN_pre_15, "DS_ITN_pre_15.csv")


# 2013
ITN.list[[4]] <-dataclean(ITN.list[[4]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd13 <- svydesign.fun(ITN.list[[4]])


DS_ITN_pre_13 <- result.fun('U5_ITN_use', 'LGA','num_p', design=ml0.svyd13)
head(DS_ITN_pre_13)

write.csv(DS_ITN_pre_13, "DS_ITN_pre_13.csv")



# 2010
ITN.list[[3]] <-dataclean(ITN.list[[3]], ml0, v005,'ml0', 'U5_ITN_use')  
ITN.list[[3]]  <- ITN.list[[3]]  %>% left_join(rep_DS)

ml0.svyd10 <- svydesign.fun(ITN.list[[3]])


DS_ITN_pre_10 <- result.fun('U5_ITN_use', 'repDS','num_p', design=ml0.svyd10)
head(DS_ITN_pre_10)

plot.file <- rep_DS%>%left_join(DS_ITN_pre_10)
plot.file


write.csv(DS_ITN_pre_10, "results/ITN_repDS/U5DS_ITN_pre_10.csv")



# 2008
ITN.list[[2]] <-dataclean(ITN.list[[2]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd08 <- svydesign.fun(ITN.list[[2]])


DS_ITN_pre_08 <- result.fun('U5_ITN_use', 'LGA','num_p', design=ml0.svyd08)
head(DS_ITN_pre_08)

write.csv(DS_ITN_pre_08, "DS_ITN_pre_08.csv")


# 2003
ITN.list[[1]] <-dataclean(ITN.list[[1]], ml0, v005,'ml0', 'U5_ITN_use')  
ml0.svyd03 <- svydesign.fun(ITN.list[[1]])


DS_ITN_pre_03 <- result.fun('U5_ITN_use', 'LGA','num_p', design=ml0.svyd03)
tail(DS_ITN_pre_03)

write.csv(DS_ITN_pre_03, "DS_ITN_pre_03.csv")



# cluster-level estimates

# 2018

clu_ITN_pre_18 <- result.clu.fun('U5_ITN_use', 'v001', design=ml0.svyd15,ITN.list[[6]])
head(clu_ITN_pre_18)

write.csv(clu_ITN_pre_18, "clu_ITN_pre_18.csv")


# 2015

clu_ITN_pre_15 <- result.clu.fun('U5_ITN_use', 'v001', design=ml0.svyd15,ITN.list[[5]])
head(clu_ITN_pre_15)

write.csv(clu_ITN_pre_15, "clu_ITN_pre_15.csv")


# 2013

clu_ITN_pre_13 <- result.clu.fun('U5_ITN_use', 'v001', design=ml0.svyd13,ITN.list[[4]])
head(clu_ITN_pre_13)

write.csv(clu_ITN_pre_13, "clu_ITN_pre_13.csv")


# 2010

clu_ITN_pre_10 <- result.clu.fun('U5_ITN_use', 'v001',design=ml0.svyd10,ITN.list[[3]])
head(clu_ITN_pre_10)

write.csv(clu_ITN_pre_10, "clu_ITN_pre_10.csv")


# 2008

clu_ITN_pre_08 <- result.clu.fun('U5_ITN_use', 'v001', design=ml0.svyd08,ITN.list[[2]])
head(clu_ITN_pre_08)

write.csv(clu_ITN_pre_08, "clu_ITN_pre_08.csv")


# 2003

clu_ITN_pre_03 <- result.clu.fun('U5_ITN_use', 'v001',design=ml0.svyd03,ITN.list[[1]])
head(clu_ITN_pre_03)

write.csv(clu_ITN_pre_03, "clu_ITN_pre_03.csv")



#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- LGAshp_sf %>% left_join(DS_ITN_pre_18)

pts_shp_18 <- st_as_sf(NGAshplist[[7]])
pts_file <- pts_shp_18 %>% left_join(clu_ITN_pre_18)

# 2015 transformations 
DS_file15 <- LGAshp_sf %>% left_join(DS_ITN_pre_15)

pts_shp_15 <- st_as_sf(NGAshplist[[6]])
pts_file_15 <- pts_shp_15 %>% left_join(clu_ITN_pre_15)


# 2013 transformations 
DS_file13 <- LGAshp_sf %>% left_join(DS_ITN_pre_13)

pts_shp_13 <- st_as_sf(NGAshplist[[5]])
pts_file_13 <- pts_shp_13 %>% left_join(clu_ITN_pre_13)


# 2010 transformations 
DS_file10 <- LGAshp_sf %>% left_join(DS_ITN_pre_10)

pts_shp_10 <- st_as_sf(NGAshplist[[4]])
pts_file_10 <- pts_shp_10 %>% left_join(clu_ITN_pre_10)


# 2008 transformations 
DS_file08 <- LGAshp_sf %>% left_join(DS_ITN_pre_08)

pts_shp_08 <- st_as_sf(NGAshplist[[3]])
pts_file_08 <- pts_shp_08 %>% left_join(clu_ITN_pre_08)


# 2003 transformations 
DS_file03 <- LGAshp_sf %>% left_join(DS_ITN_pre_03)

pts_shp_03 <- st_as_sf(NGAshplist[[2]])
pts_file_03 <- pts_shp_03 %>% left_join(clu_ITN_pre_03)

#rep DS transform 

repDS_file <- LGAshp_sf %>% left_join(plot.file)



# 2018 map 
nga_ITN18 <- tmap.fun1(DS_file, DSmapvalue="U5_ITN_use", adminlegtitle="ITN use", 
                        main_title="U5 ITN use prevalence by LGA (2018)", text_title = "LGA", 
                        ptsfile=pts_file, "Number of Participants", "U5_ITN_use") 

nga_ITN18 <- tmap.fun4(DS_file, "U5 ITN use prevalence by LGA (2018)", "ITN use",  
                       col="U5_ITN_use")

# 2015 map 
nga_ITN15 <- tmap.fun1(DS_file15, DSmapvalue="U5_ITN_use", adminlegtitle="ITN use", 
                       main_title="U5 ITN use prevalence by LGA (2015)", text_title = "LGA", 
                       ptsfile=pts_file_15, "Number of Participants", "U5_ITN_use") 

nga_ITN15 <- tmap.fun4(DS_file15, "U5 ITN use prevalence by LGA (2015)", "ITN use",  
                       col="U5_ITN_use")


# 2013 map 
nga_ITN13 <- tmap.fun3(DS_file13, "U5_ITN_use", "ITN use", "U5 ITN use prevalence by LGA (2013)", pts_file_13,  
                       "Number of Participants", "U5_ITN_use") 

nga_ITN13 <- tmap.fun4(DS_file13, "U5 ITN use prevalence by LGA (2013)", "ITN use",  
                       col="U5_ITN_use")

# 2010 map 
nga_ITN10 <- tmap.fun3(DS_file10, "U5_ITN_use", "ITN use", "U5 ITN use prevalence by LGA (2010)", pts_file_10,  
                       "Number of Participants", "U5_ITN_use") 

nga_ITN10 <- tmap.fun1(DS_file10, DSmapvalue="U5_ITN_use", adminlegtitle="ITN use", 
                       main_title="U5 ITN use prevalence by LGA (2010)", text_title = "LGA", 
                       ptsfile=pts_file_10, "Number of Participants", "U5_ITN_use") 


nga_ITN10_v <- tmap.fun4(DS_file10, "U5 ITN use prevalence by LGA (2010)", "ITN use",  
                       col="U5_ITN_use")

nga_ITN10_rep <- tmap.fun4(repDS_file, "U5 ITN use prevalence by LGA (2010)", "ITN use",  
                       col="U5_ITN_use")

tmap_save(tm = nga_ITN10_rep, filename = "results/archtype value.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


# 2008 map 
nga_ITN08 <- tmap.fun1(DS_file08, DSmapvalue="U5_ITN_use", adminlegtitle="ITN use", 
                       main_title="U5 ITN use prevalence by LGA (2008)", text_title = "LGA", 
                       ptsfile=pts_file_08, "Number of Participants", "U5_ITN_use") 

nga_ITN08 <- tmap.fun4(DS_file08, "U5 ITN use prevalence by LGA (2008)", "ITN use",  
                       col="U5_ITN_use")

# 2003 map 
nga_ITN03 <- tmap.fun1(DS_file03, DSmapvalue="U5_ITN_use", adminlegtitle="ITN use", 
                       main_title="U5 ITN use prevalence by LGA (2003)", text_title = "LGA", 
                       ptsfile=pts_file_03, "Number of Participants", "U5_ITN_use") 

nga_ITN03 <- tmap.fun4(DS_file03, "U5 ITN use prevalence by LGA (2003)", "ITN use",  
                       col="U5_ITN_use")

all_U5ITN <- tmap_arrange(nga_ITN03, nga_ITN08, nga_ITN10, nga_ITN13, nga_ITN15, nga_ITN18)

tmap_save(tm = nga_ITN10, filename = "results/U5ITN_10.pdf",width=13, height=13, units ="in", asp=0,
    paper ="A4r", useDingbats=FALSE)

