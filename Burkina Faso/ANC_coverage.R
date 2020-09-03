#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with ANC data for 1993, 1998-99, 2003, 2010, 2014
ANC.list <- list(BFfiles[[1]], BFfiles[[4]], BFfiles[[7]], BFfiles[[10]], BFfiles[[13]])


# recoding ANC 


look_for(BFfiles[[16]], "antenatal")

ANC.list  <- map(ANC.list, recoder.ml4_1)


# key list for ANC
keys.ANC <- list(key_list[[1]], key_list[[2]], key_list[[3]],key_list[[4]], key_list[[5]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
ANC.list <- map2(ANC.list, keys.ANC, left_join) #PR datasets


#####################################################################################################
# ANC coverage 
####################################################################################################

# 2014
ANC.list[[5]] <-dataclean(ANC.list[[5]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd14 <- svydesign.fun(ANC.list[[5]])


DS_ANC_pre_14 <- result.fun('ANC', 'NOMDEP','num_p', design=ANC.svyd14)
head(DS_ANC_pre_14)

write.csv(DS_ANC_pre_14, "results/ANC/DS_ANC_pre_14.csv")


# 2010
ANC.list[[4]] <-dataclean(ANC.list[[4]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd10 <- svydesign.fun(ANC.list[[4]])


DS_ANC_pre_10 <- result.fun('ANC', 'NOMDEP','num_p', design=ANC.svyd10)
head(DS_ANC_pre_10)

write.csv(DS_ANC_pre_10, "results/ANC/DS_ANC_pre_10.csv")


# 2003
ANC.list[[3]] <-dataclean2(ANC.list[[3]], m14_1, v005,'m14_1', 'ANC', 'v023')  
ANC.svyd03 <- svydesign.fun(ANC.list[[3]])

DS_ANC_pre_03 <- result.fun('ANC', 'NOMDEP','num_p', design=ANC.svyd03)
head(DS_ANC_pre_03)

write.csv(DS_ANC_pre_03, "results/ANC/DS_ANC_pre_03.csv")



# 1998-99 
ANC.list[[2]] <-dataclean(ANC.list[[2]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd98 <- svydesign.fun(ANC.list[[2]])

DS_ANC_pre_98 <- result.fun('ANC', 'NOMDEP','num_p', design=ANC.svyd98)
head(DS_ANC_pre_98)

write.csv(DS_ANC_pre_98, "results/ANC/DS_ANC_pre_98.csv")



# 1993
ANC.list[[1]] <-dataclean(ANC.list[[1]], m14_1, v005,'m14_1', 'ANC')  
ANC.svyd93 <- svydesign.fun(ANC.list[[1]])

DS_ANC_pre_93 <- result.fun('ANC', 'NOMDEP','num_p', design=ANC.svyd93)
head(DS_ANC_pre_93)

write.csv(DS_ANC_pre_93, "results/ANC/DS_ANC_pre_93.csv")



# cluster-level estimates

# 2014

clu_ANC_pre_14 <- result.clu.fun('ANC', 'v001', design=ANC.svyd14,ANC.list[[5]])
head(clu_ANC_pre_14)

write.csv(clu_ANC_pre_14, "results/ANC/clu_ANC_pre_14.csv")



# 2010

clu_ANC_pre_10 <- result.clu.fun('ANC', 'v001', design=ANC.svyd10,ANC.list[[4]])
head(clu_ANC_pre_10)

write.csv(clu_ANC_pre_10, "results/ANC/clu_ANC_pre_10.csv")




# 2003

clu_ANC_pre_03 <- result.clu.fun('ANC', 'v001', design=ANC.svyd03,ANC.list[[3]])
head(clu_ANC_pre_03)

write.csv(clu_ANC_pre_03, "results/ANC/clu_ANC_pre_03.csv")



# 1998 - 99 

clu_ANC_pre_98 <- result.clu.fun('ANC', 'v001', design=ANC.svyd98,ANC.list[[2]])
head(clu_ANC_pre_98)

write.csv(clu_ANC_pre_98, "results/ANC/clu_ANC_pre_98.csv")



# 1993

clu_ANC_pre_93 <- result.clu.fun('ANC', 'v001', design=ANC.svyd93,ANC.list[[1]])
head(clu_ANC_pre_93)

write.csv(clu_ANC_pre_93, "results/ANC/clu_ANC_pre_93.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2014 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_ANC_pre_14)

pts_file <- BFshplist_sf[[5]] %>% left_join(clu_ANC_pre_14)



# 2010 transformations 
DS_file_10 <- DS_shape_sf %>% left_join(DS_ANC_pre_10)

pts_file_10 <- BFshplist_sf[[4]] %>% left_join(clu_ANC_pre_10)


# 2003 transformations 
DS_file_03 <- DS_shape_sf %>% left_join(DS_ANC_pre_03)

pts_file_03 <- BFshplist_sf[[3]] %>% left_join(clu_ANC_pre_03)


# 1998-99 transformations 
DS_file_98 <- DS_shape_sf %>% left_join(DS_ANC_pre_98)

pts_file_98 <- BFshplist_sf[[2]] %>% left_join(clu_ANC_pre_98)


# 1993 transformations 
DS_file_93 <- DS_shape_sf %>% left_join(DS_ANC_pre_93)

pts_file_93 <- BFshplist_sf[[1]] %>% left_join(clu_ANC_pre_93)


# 2014 map 
BF_ANC14 <- tmap.fun5(DS_file, colname="ANC", legtitle="ANC coverage (most recent child)", 
                       maintitle="ANC coverage by District (2014)", ptsfile=pts_file, "Number of Participants",
                      "ANC") 


# 2010 map 
BF_ANC10 <- tmap.fun5(DS_file_10, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by District (2010)", ptsfile=pts_file_10, "Number of Participants",
                      "ANC") 


# 2003 map 
BF_ANC03 <- tmap.fun5(DS_file_03, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by District (2003)", ptsfile=pts_file_03, "Number of Participants",
                      "ANC") 



# 1998 map 
BF_ANC98 <- tmap.fun5(DS_file_98, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by District (1998-99)", ptsfile=pts_file_98, "Number of Participants",
                      "ANC") 

# 1993 map 
BF_ANC93 <- tmap.fun5(DS_file_93, colname="ANC", legtitle="ANC coverage (most recent child)", 
                      maintitle="ANC coverage by District (1993)", ptsfile=pts_file_93, "Number of Participants",
                      "ANC") 


all_ANC <- tmap_arrange(BF_ANC93, BF_ANC98, BF_ANC03, BF_ANC10, BF_ANC14)

tmap_save(tm = BF_ANC14, filename = "results/ANC/2014_ANC_3.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
