#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with IPTP data for 2003, 2010, 2014, 2017/18
IPTP.list <- list(BFfiles[[7]], BFfiles[[10]], BFfiles[[13]], BFfiles[[16]])


# recoding IPTP 


table(BFfiles[[13]]$m49a_1)

IPTP.list  <- map(IPTP.list, recoder.m49a)


# key list for IPTP
keys.IPTP <- list(key_list[[3]],key_list[[4]], key_list[[5]],key_list[[6]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
IPTP.list <- map2(IPTP.list, keys.IPTP, left_join) #PR datasets



#####################################################################################################
# IPTP maps 
####################################################################################################

# 2017/18
IPTP.list[[4]] <-dataclean(IPTP.list[[4]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd17_18 <- svydesign.fun(IPTP.list[[4]])


DS_IPTP_pre_18 <- result.fun('IPTP', 'NOMDEP','num_p', design=IPTP.svyd17_18)
head(DS_IPTP_pre_18)

write.csv(DS_IPTP_pre_18, "results/IPTP/DS_IPTP_pre_18.csv")


# 2014
IPTP.list[[3]] <-dataclean(IPTP.list[[3]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd14 <- svydesign.fun(IPTP.list[[3]])


DS_IPTP_pre_14 <- result.fun('IPTP', 'NOMDEP','num_p', design=IPTP.svyd14)
head(DS_IPTP_pre_14)

write.csv(DS_IPTP_pre_14, "results/IPTP/DS_IPTP_pre_14.csv")


# 2010
IPTP.list[[2]] <-dataclean(IPTP.list[[2]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd10 <- svydesign.fun(IPTP.list[[2]])


DS_IPTP_pre_10 <- result.fun('IPTP', 'NOMDEP','num_p', design=IPTP.svyd10)
head(DS_IPTP_pre_10)

write.csv(DS_IPTP_pre_10, "results/IPTP/DS_IPTP_pre_10.csv")


# 2003
IPTP.list[[1]] <-dataclean2(IPTP.list[[1]], m49a_1, v005,'m49a_1', 'IPTP', 'v023')  
IPTP.svyd03 <- svydesign.fun(IPTP.list[[1]])


DS_IPTP_pre_03 <- result.fun('IPTP', 'NOMDEP','num_p', design=IPTP.svyd03)
head(DS_IPTP_pre_03)

write.csv(DS_IPTP_pre_03, "results/IPTP/DS_IPTP_pre_03.csv")



# cluster-level estimates

# 2017/18

clu_IPTP_pre_18 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd17_18,IPTP.list[[4]])
head(clu_IPTP_pre_18)

write.csv(clu_IPTP_pre_18, "results/IPTP/clu_IPTP_pre_18.csv")



# 2014

clu_IPTP_pre_14 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd14,IPTP.list[[3]])
head(clu_IPTP_pre_14)

write.csv(clu_IPTP_pre_14, "results/IPTP/clu_IPTP_pre_14.csv")



# 2010

clu_IPTP_pre_10 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd10,IPTP.list[[2]])
head(clu_IPTP_pre_10)

write.csv(clu_IPTP_pre_10, "results/IPTP/clu_IPTP_pre_10.csv")


# 2003 

clu_IPTP_pre_03 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd03,IPTP.list[[1]])
head(clu_IPTP_pre_03)

write.csv(clu_IPTP_pre_03, "results/IPTP/clu_IPTP_pre_03.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2017/18 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_IPTP_pre_18)

pts_file <- BFshplist_sf[[6]] %>% left_join(clu_IPTP_pre_18)


# 2014 transformations 
DS_file_14 <- DS_shape_sf %>% left_join(DS_IPTP_pre_14)

pts_file_14 <- BFshplist_sf[[5]] %>% left_join(clu_IPTP_pre_14)


# 2010 transformations 
DS_file_10 <- DS_shape_sf %>% left_join(DS_IPTP_pre_10)

pts_file_10 <- BFshplist_sf[[4]] %>% left_join(clu_IPTP_pre_10)


# 2003 transformations 
DS_file_03 <- DS_shape_sf %>% left_join(DS_IPTP_pre_03)

pts_file_03 <- BFshplist_sf[[3]] %>% left_join(clu_IPTP_pre_03)


# 2018 map 
BF_IPTP18 <- tmap.fun5(DS_file, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                      maintitle="IPTP coverage by District (2017/18)", ptsfile=pts_file, "Number of Participants",
                      "IPTP") 

# 2014 map 
BF_IPTP14 <- tmap.fun5(DS_file_14, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                       maintitle="IPTP coverage by District (2014)", ptsfile=pts_file_14, "Number of Participants",
                       "IPTP") 

# 2010 map 
BF_IPTP10 <- tmap.fun5(DS_file_10, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                       maintitle="IPTP coverage by District (2010)", ptsfile=pts_file_10, "Number of Participants",
                       "IPTP") 

# 2003 map 
BF_IPTP03 <- tmap.fun5(DS_file_03, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                       maintitle="IPTP coverage by District (2003)", ptsfile=pts_file_03, "Number of Participants",
                       "IPTP") 

all_IPTP <- tmap_arrange(BF_IPTP03, BF_IPTP10, BF_IPTP14, BF_IPTP18)

tmap_save(tm = all_IPTP,filename = "results/IPTP/all_IPTP.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



