#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with IPTP3 data for 2003, 2008, 2010, 2013, 2015, 2018 
IPTP3.list <- list(NGAfiles[[4]], NGAfiles[[7]], NGAfiles[[10]], NGAfiles[[13]], NGAfiles[[16]], NGAfiles[[19]])



table(IPTP3.list[[2]]$m49a_1)
table(IPTP3.list[[2]]$ml1_1)

# recoding IPTP3 

# var_label(IPTP3.list[[4]]$ml1_1)
#table(IPTP3.list[[2]]$IPTP3)
# table(BFfiles[[16]]$ml1_1)


IPTP3.list  <- map(IPTP3.list, recoder.ml1)


# key list for IPTP3
keys.IPTP3 <- list(key_list[[2]], key_list[[3]],key_list[[4]],key_list[[5]], key_list[[6]], key_list[[7]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
IPTP3.list <- map2(IPTP3.list, keys.IPTP3, left_join) #PR datasets



#####################################################################################################
# IPTP3 estimates
####################################################################################################

# 2018
IPTP3.list[[6]] <-dataclean(IPTP3.list[[6]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd18 <- svydesign.fun(IPTP3.list[[6]])


DS_IPTP3_pre_18 <- result.fun('IPTP3', 'LGA','num_p', design=IPTP3.svyd18)
head(DS_IPTP3_pre_18)

write.csv(DS_IPTP3_pre_18, "results/IPTP_3/DS_IPTP3_pre_18.csv")


# 2015
IPTP3.list[[5]] <-dataclean(IPTP3.list[[5]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd15 <- svydesign.fun(IPTP3.list[[5]])


DS_IPTP3_pre_15 <- result.fun('IPTP3', 'LGA','num_p', design=IPTP3.svyd15)
head(DS_IPTP3_pre_15)

write.csv(DS_IPTP3_pre_15, "results/IPTP_3/DS_IPTP3_pre_15.csv")



# 2013
IPTP3.list[[4]] <-dataclean(IPTP3.list[[4]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd13 <- svydesign.fun(IPTP3.list[[4]])


DS_IPTP3_pre_13 <- result.fun('IPTP3', 'LGA','num_p', design=IPTP3.svyd13)
head(DS_IPTP3_pre_13)

write.csv(DS_IPTP3_pre_13, "results/IPTP_3/DS_IPTP3_pre_13.csv")


# 2010
IPTP3.list[[3]] <-dataclean(IPTP3.list[[3]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd10 <- svydesign.fun(IPTP3.list[[3]])


DS_IPTP3_pre_10 <- result.fun('IPTP3', 'LGA','num_p', design=IPTP3.svyd10)
head(DS_IPTP3_pre_10)

write.csv(DS_IPTP3_pre_10, "results/IPTP_3/DS_IPTP3_pre_10.csv")


# 2008
IPTP3.list[[2]] <-dataclean(IPTP3.list[[2]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd08 <- svydesign.fun(IPTP3.list[[2]])


DS_IPTP3_pre_08 <- result.fun('IPTP3', 'LGA','num_p', design=IPTP3.svyd08)
head(DS_IPTP3_pre_08)

write.csv(DS_IPTP3_pre_08, "results/IPTP_3/DS_IPTP3_pre_08.csv")


# 2003
IPTP3.list[[1]] <-dataclean(IPTP3.list[[1]], ml1_1, v005,'ml1_1', 'IPTP3')  
IPTP3.svyd03 <- svydesign.fun(IPTP3.list[[1]])


DS_IPTP3_pre_03 <- result.fun('IPTP3', 'LGA','num_p', design=IPTP3.svyd03)
head(DS_IPTP3_pre_03)

write.csv(DS_IPTP3_pre_03, "results/IPTP_3/DS_IPTP3_pre_03.csv")


# cluster-level estimates

# 2018

clu_IPTP3_pre_18 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd18,IPTP3.list[[6]])
head(clu_IPTP3_pre_18)

write.csv(clu_IPTP3_pre_18, "results/IPTP_3/clu_IPTP3_pre_18.csv")


# 2015

clu_IPTP3_pre_15 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd15,IPTP3.list[[5]])
head(clu_IPTP3_pre_15)

write.csv(clu_IPTP3_pre_15, "results/IPTP_3/clu_IPTP3_pre_15.csv")


# 2013

clu_IPTP3_pre_13 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd13,IPTP3.list[[4]])
head(clu_IPTP3_pre_13)

write.csv(clu_IPTP3_pre_13, "results/IPTP_3/clu_IPTP3_pre_13.csv")


# 2010
clu_IPTP3_pre_10 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd10,IPTP3.list[[3]])
head(clu_IPTP3_pre_10)

write.csv(clu_IPTP3_pre_10, "results/IPTP_3/clu_IPTP3_pre_10.csv")



# 2008

clu_IPTP3_pre_08 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd08,IPTP3.list[[2]])
head(clu_IPTP3_pre_08)

write.csv(clu_IPTP3_pre_08, "results/IPTP_3/clu_IPTP3_pre_08.csv")



# 2003

clu_IPTP3_pre_03 <- result.clu.fun('IPTP3', 'v001', design=IPTP3.svyd03,IPTP3.list[[1]])
head(clu_IPTP3_pre_03)

write.csv(clu_IPTP3_pre_03, "results/IPTP_3/clu_IPTP3_pre_03.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- LGAshp_sf %>% left_join(DS_IPTP3_pre_18)

pts_file <- NGAshplist_sf[[7]] %>% left_join(clu_IPTP3_pre_18)


# 2015 transformations 
DS_file_15 <- LGAshp_sf %>% left_join(DS_IPTP3_pre_15)

pts_file_15 <- NGAshplist_sf[[6]] %>% left_join(clu_IPTP3_pre_15)


# 2013 transformations 
DS_file_13 <- LGAshp_sf %>% left_join(DS_IPTP3_pre_13)

pts_file_13 <- NGAshplist_sf[[5]] %>% left_join(clu_IPTP3_pre_13)


# 2010 transformations 
DS_file_10 <- LGAshp_sf %>% left_join(DS_IPTP3_pre_10)

pts_file_10 <- NGAshplist_sf[[4]] %>% left_join(clu_IPTP3_pre_10)


# 2008 transformations 
DS_file_08 <- LGAshp_sf %>% left_join(DS_IPTP3_pre_08)

pts_file_08 <- NGAshplist_sf[[3]] %>% left_join(clu_IPTP3_pre_08)


# 2003 transformations 
DS_file_03 <- LGAshp_sf %>% left_join(DS_IPTP3_pre_03)

pts_file_03 <- NGAshplist_sf[[2]] %>% left_join(clu_IPTP3_pre_03)


# 2018 map 
NGA_IPTP18 <- tmap.fun3(DS_file, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                       maintitle="IPTP coverage by District (2018)", ptsfile=pts_file, "Number of Participants",
                       "IPTP3") 


# 2015 map 
NGA_IPTP15 <- tmap.fun3(DS_file_15, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                        maintitle="IPTP coverage by District (2015)", ptsfile=pts_file_15, "Number of Participants",
                        "IPTP3") 


# 2013 map 
NGA_IPTP13 <- tmap.fun3(DS_file_13, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                        maintitle="IPTP coverage by District (2013)", ptsfile=pts_file_13, "Number of Participants",
                        "IPTP3") 

# 2010 map 
NGA_IPTP10 <- tmap.fun3(DS_file_10, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                        maintitle="IPTP coverage by District (2010)", ptsfile=pts_file_10, "Number of Participants",
                        "IPTP3") 


# 2008 map 
NGA_IPTP08 <- tmap.fun3(DS_file_08, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                        maintitle="IPTP coverage by District (2008)", ptsfile=pts_file_08, "Number of Participants",
                        "IPTP3") 


# 2003 map 
NGA_IPTP03 <- tmap.fun3(DS_file_03, colname="IPTP3", legtitle="IPTP coverage (3 or more times for most recent child)", 
                        maintitle="IPTP coverage by District (2003)", ptsfile=pts_file_03, "Number of Participants",
                        "IPTP3") 


all_IPTP3 <- tmap_arrange(NGA_IPTP03, NGA_IPTP08, NGA_IPTP10,NGA_IPTP13,NGA_IPTP15,NGA_IPTP18)

tmap_save(tm =NGA_IPTP18,filename = "results/IPTP_3/NGA_IPTP18.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



