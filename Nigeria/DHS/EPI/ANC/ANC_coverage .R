#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with ANC data for 1993, 2003, 2008, 2013, 2018
ANC.list <- list(NGAfiles[[1]], NGAfiles[[4]], NGAfiles[[7]], NGAfiles[[13]], 
                 NGAfiles[[19]])

#recoding 2010 dataset seperately as it has a different variable name 
NGAfiles[[10]]$s226a <- recoder3(NGAfiles[[10]]$s226a)

# look_for(NGAfiles[[1]], "antenatal")
# table(NGAfiles[[10]]$s226a) #2010
# table(ANC.list[[5]]$m14_1)
# table(NGAfiles[[19]]$m14_1)


# recoding ANC 
ANC.list  <- map(ANC.list, recoder.ml4_1)

#appending the 2010 dataset to the li2t of dataframes 
ANC.list <- append(ANC.list,list(NGAfiles[[10]]), 3)


# key list for ANC (1993, 2003, 2008, 2010, 2013, 2018)
keys.ANC <- list(key_list[[1]], key_list[[2]], key_list[[3]], key_list[[4]], key_list[[5]], key_list[[7]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
ANC.list <- map2(ANC.list, keys.ANC, left_join) #PR datasets


#####################################################################################################
# ANC coverage 
####################################################################################################

# 2018
ANC.list[[6]] <-dataclean(ANC.list[[6]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd18 <- svydesign.fun(ANC.list[[6]])


DS_ANC_pre_18 <- result.fun('ANC', 'LGA','num_p', design=ANC.svyd18)
head(DS_ANC_pre_18)

write.csv(DS_ANC_pre_18, "results/ANC_Nigeria/DS_ANC_pre_18.csv")



# 2013
ANC.list[[5]] <-dataclean(ANC.list[[5]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd13 <- svydesign.fun(ANC.list[[5]])


DS_ANC_pre_13 <- result.fun('ANC', 'LGA','num_p', design=ANC.svyd13)
head(DS_ANC_pre_13)

write.csv(DS_ANC_pre_13, "results/ANC_Nigeria/DS_ANC_pre_13.csv")



# 2010
ANC.list[[4]] <-dataclean(ANC.list[[4]], s226a, v005,'s226a', 'ANC')  
ANC.svyd10 <- svydesign.fun(ANC.list[[4]])


DS_ANC_pre_10 <- result.fun('ANC', 'LGA','num_p', design=ANC.svyd10)
head(DS_ANC_pre_10)

write.csv(DS_ANC_pre_10, "results/ANC_Nigeria/DS_ANC_pre_10.csv")



# 2008
ANC.list[[3]] <-dataclean(ANC.list[[3]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd08 <- svydesign.fun(ANC.list[[3]])


DS_ANC_pre_08 <- result.fun('ANC', 'LGA','num_p', design=ANC.svyd08)
head(DS_ANC_pre_08)

write.csv(DS_ANC_pre_08, "results/ANC_Nigeria/DS_ANC_pre_08.csv")



# 2003
ANC.list[[2]] <-dataclean(ANC.list[[2]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd03 <- svydesign.fun(ANC.list[[2]])


DS_ANC_pre_03 <- result.fun('ANC', 'LGA','num_p', design=ANC.svyd03)
head(DS_ANC_pre_03)

write.csv(DS_ANC_pre_03, "results/ANC_Nigeria/DS_ANC_pre_03.csv")



# 1993
ANC.list[[1]] <-dataclean(ANC.list[[1]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd93 <- svydesign.fun(ANC.list[[1]])


DS_ANC_pre_93 <- result.fun('ANC', 'LGA','num_p', design=ANC.svyd93)
head(DS_ANC_pre_93)

write.csv(DS_ANC_pre_93, "results/ANC_Nigeria/DS_ANC_pre_93.csv")




# cluster-level estimates

# 2018

clu_ANC_pre_18 <- result.clu.fun('ANC', 'v001', design=ANC.svyd18,ANC.list[[6]])
head(clu_ANC_pre_18)

write.csv(clu_ANC_pre_18, "results/ANC_Nigeria/clu_ANC_pre_18.csv")



# 2013

clu_ANC_pre_13 <- result.clu.fun('ANC', 'v001', design=ANC.svyd13,ANC.list[[5]], "v007")
head(clu_ANC_pre_13)

write.csv(clu_ANC_pre_13, "results/ANC_Nigeria/clu_ANC_pre_13.csv")



# 2010

clu_ANC_pre_10 <- result.clu.fun('ANC', 'v001', design=ANC.svyd10,ANC.list[[4]])
head(clu_ANC_pre_10)

write.csv(clu_ANC_pre_10, "results/ANC_Nigeria/clu_ANC_pre_10.csv")



# 2008

clu_ANC_pre_08 <- result.clu.fun('ANC', 'v001', design=ANC.svyd08,ANC.list[[3]])
head(clu_ANC_pre_08)

write.csv(clu_ANC_pre_08, "results/ANC_Nigeria/clu_ANC_pre_08.csv")



# 2003

clu_ANC_pre_03 <- result.clu.fun('ANC', 'v001', design=ANC.svyd03,ANC.list[[2]])
head(clu_ANC_pre_03)

write.csv(clu_ANC_pre_03, "results/ANC_Nigeria/clu_ANC_pre_03.csv")



# 1993

clu_ANC_pre_93 <- result.clu.fun('ANC', 'v001', design=ANC.svyd93,ANC.list[[1]])
head(clu_ANC_pre_93)

write.csv(clu_ANC_pre_93, "results/ANC_Nigeria/clu_ANC_pre_93.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- LGAshp_sf%>% left_join(DS_ANC_pre_18)

pts_file <- NGAshplist_sf[[7]] %>% left_join(clu_ANC_pre_18)


# 2013 transformations 
DS_file_13 <- LGAshp_sf%>% left_join(DS_ANC_pre_13)

pts_file_13 <- NGAshplist_sf[[5]] %>% left_join(clu_ANC_pre_13)



# 2010 transformations 
DS_file_10 <- LGAshp_sf%>% left_join(DS_ANC_pre_10)

pts_file_10 <- NGAshplist_sf[[4]] %>% left_join(clu_ANC_pre_10)


# 2008 transformations 
DS_file_08 <- LGAshp_sf%>% left_join(DS_ANC_pre_08)

pts_file_08 <- NGAshplist_sf[[3]] %>% left_join(clu_ANC_pre_08)



# 2003 transformations 
DS_file_03 <- LGAshp_sf%>% left_join(DS_ANC_pre_03)

pts_file_03 <- NGAshplist_sf[[2]] %>% left_join(clu_ANC_pre_03)



# 1993 transformations 
DS_file_93 <- LGAshp_sf%>% left_join(DS_ANC_pre_93)

pts_file_93 <- NGAshplist_sf[[1]] %>% left_join(clu_ANC_pre_93)



# 2018 map 
NG_ANC18 <- tmap.fun3(DS_file, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by LGA (2018)", ptsfile=pts_file, "Number of Participants",
                      "ANC") 


# 2013 map 
NG_ANC13 <- tmap.fun3(DS_file_13, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by LGA (2013)", ptsfile=pts_file_13, "Number of Participants",
                      "ANC") 

# 2010 map 
NG_ANC10 <- tmap.fun3(DS_file_10, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by LGA (2010)", ptsfile=pts_file_10, "Number of Participants",
                      "ANC") 


# 2008 map 
NG_ANC08 <- tmap.fun3(DS_file_08, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by LGA (2008)", ptsfile=pts_file_08, "Number of Participants",
                      "ANC") 


# 2003 map 
NG_ANC03 <- tmap.fun3(DS_file_03, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by LGA (2003)", ptsfile=pts_file_03, "Number of Participants",
                      "ANC") 


# 1993 map 
NG_ANC93 <- tmap.fun3(DS_file_93, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by LGA (1993)", ptsfile=pts_file_03, "Number of Participants",
                      "ANC") 


all_ANC <- tmap_arrange(NG_ANC93, NG_ANC03, NG_ANC08, NG_ANC10, NG_ANC13,NG_ANC18)

tmap_save(tm = NG_ANC18, filename = "results/ANC_Nigeria/2018_ANC.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)








