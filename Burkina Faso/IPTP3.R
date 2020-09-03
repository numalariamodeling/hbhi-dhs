#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with IPTP3 data for 2010, 2014, 2017/18
IPTP3.list <- list(BFfiles[[10]], BFfiles[[13]], BFfiles[[16]])

table(BFfiles[[13]]$ml1_1)

# recoding IPTP3 

# var_label(IPTP3.list[[4]]$ml1_1)
#table(IPTP3.list[[2]]$IPTP3)
# table(BFfiles[[16]]$ml1_1)


IPTP3.list  <- map(IPTP3.list, recoder.ml1)


# key list for IPTP3
keys.IPTP3 <- list(key_list[[4]], key_list[[5]],key_list[[6]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
IPTP3.list <- map2(IPTP3.list, keys.IPTP3, left_join) #PR datasets



#####################################################################################################
# IPTP3 estimates 
####################################################################################################

# 2017/18
IPTP3.list[[3]] <-dataclean(IPTP3.list[[3]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd17_18 <- svydesign.fun(IPTP3.list[[3]])


DS_IPTP3_pre_18 <- result.fun('IPTP3', 'NOMDEP','num_p', design=IPTP3.svyd17_18)
head(DS_IPTP3_pre_18)

write.csv(DS_IPTP3_pre_18, "results/IPTP_3/DS_IPTP3_pre_18.csv")


# 2014
IPTP3.list[[2]] <-dataclean(IPTP3.list[[2]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd14 <- svydesign.fun(IPTP3.list[[2]])


DS_IPTP3_pre_14 <- result.fun('IPTP3', 'NOMDEP','num_p', design=IPTP3.svyd14)
head(DS_IPTP3_pre_14)

write.csv(DS_IPTP3_pre_14, "results/IPTP_3/DS_IPTP3_pre_14.csv")


# 2010
IPTP3.list[[1]] <-dataclean(IPTP3.list[[1]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd10 <- svydesign.fun(IPTP3.list[[1]])


DS_IPTP3_pre_10 <- result.fun('IPTP3', 'NOMDEP','num_p', design=IPTP3.svyd10)
head(DS_IPTP3_pre_10)

write.csv(DS_IPTP3_pre_10, "results/IPTP_3/DS_IPTP3_pre_10.csv")



# cluster-level estimates

# 2017/18

clu_IPTP3_pre_18 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd17_18,IPTP3.list[[3]])
head(clu_IPTP3_pre_18)

write.csv(clu_IPTP3_pre_18, "results/IPTP_3/clu_IPTP3_pre_18.csv")



# 2014

clu_IPTP3_pre_14 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd14,IPTP3.list[[2]])
head(clu_IPTP3_pre_14)

write.csv(clu_IPTP3_pre_14, "results/IPTP_3/clu_IPTP3_pre_14.csv")



# 2010

clu_IPTP3_pre_10 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd10,IPTP3.list[[1]])
head(clu_IPTP3_pre_10)

write.csv(clu_IPTP3_pre_10, "results/IPTP_3/clu_IPTP3_pre_10.csv")



#####################################################################################################
## Maps 
####################################################################################################

# 2017/18 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_IPTP3_pre_18)

pts_file <- BFshplist_sf[[6]] %>% left_join(clu_IPTP3_pre_18)


# 2014 transformations 
DS_file_14 <- DS_shape_sf %>% left_join(DS_IPTP3_pre_14)

pts_file_14 <- BFshplist_sf[[5]] %>% left_join(clu_IPTP3_pre_14)


# 2010 transformations 
DS_file_10 <- DS_shape_sf %>% left_join(DS_IPTP3_pre_10)

pts_file_10 <- BFshplist_sf[[4]] %>% left_join(clu_IPTP3_pre_10)


# 2018 map 
BF_IPTP18 <- tmap.fun5(DS_file, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                       maintitle="IPTP coverage by District (2017/18)", ptsfile=pts_file, "Number of Participants",
                       "IPTP3") 


# 2014 map 
BF_IPTP14 <- tmap.fun5(DS_file_14, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                       maintitle="IPTP coverage by District (2014)", ptsfile=pts_file_14, "Number of Participants",
                       "IPTP3") 

# 2010 map 
BF_IPTP10 <- tmap.fun5(DS_file_10, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                       maintitle="IPTP coverage by District (2014)", ptsfile=pts_file_10, "Number of Participants",
                       "IPTP3") 


all_IPTP <- tmap_arrange(BF_IPTP10, BF_IPTP14, BF_IPTP18)

tmap_save(tm =BF_IPTP18,filename = "results/IPTP_3/IPTP3_2018.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)




