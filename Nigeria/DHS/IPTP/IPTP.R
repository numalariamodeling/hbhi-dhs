#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files with IPTP data for 2003, 2008, 2010, 2013, 2015, 2018 
IPTP.list <- list(NGAfiles[[4]], NGAfiles[[7]], NGAfiles[[10]], NGAfiles[[13]], NGAfiles[[16]], NGAfiles[[19]])


# recoding IPTP 


table(IPTP.list[[6]]$m49a_1)

IPTP.list  <- map(IPTP.list, recoder.m49a)


# key list for IPTP
keys.IPTP <- list(key_list[[2]],key_list[[3]], key_list[[4]],key_list[[5]], key_list[[6]], key_list[[7]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
IPTP.list <- map2(IPTP.list, keys.IPTP, left_join) #PR datasets


#####################################################################################################
# IPTP estimates 
####################################################################################################

# 2018
IPTP.list[[6]] <-dataclean(IPTP.list[[6]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd18 <- svydesign.fun(IPTP.list[[6]])


DS_IPTP_pre_18 <- result.fun('IPTP', 'LGA','num_p', design=IPTP.svyd18)
head(DS_IPTP_pre_18)

write.csv(DS_IPTP_pre_18, "results/IPTP/DS_IPTP_pre_18.csv")


# 2015
IPTP.list[[5]] <-dataclean(IPTP.list[[5]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd15 <- svydesign.fun(IPTP.list[[5]])


DS_IPTP_pre_15 <- result.fun('IPTP', 'LGA','num_p', design=IPTP.svyd15)
head(DS_IPTP_pre_15)

write.csv(DS_IPTP_pre_15, "results/IPTP/DS_IPTP_pre_15.csv")


# 2013 
IPTP.list[[4]] <-dataclean(IPTP.list[[4]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd13 <- svydesign.fun(IPTP.list[[4]])


DS_IPTP_pre_13 <- result.fun('IPTP', 'LGA','num_p', design=IPTP.svyd13)
head(DS_IPTP_pre_13)

write.csv(DS_IPTP_pre_13, "results/IPTP/DS_IPTP_pre_13.csv")


# 2010
IPTP.list[[3]] <-dataclean(IPTP.list[[3]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd10 <- svydesign.fun(IPTP.list[[3]])


DS_IPTP_pre_10 <- result.fun('IPTP', 'LGA','num_p', design=IPTP.svyd10)
head(DS_IPTP_pre_10)

write.csv(DS_IPTP_pre_10, "results/IPTP/DS_IPTP_pre_10.csv")


# 2008
IPTP.list[[2]] <-dataclean(IPTP.list[[2]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd08 <- svydesign.fun(IPTP.list[[2]])


DS_IPTP_pre_08 <- result.fun('IPTP', 'LGA','num_p', design=IPTP.svyd08)
head(DS_IPTP_pre_08)

write.csv(DS_IPTP_pre_08, "results/IPTP/DS_IPTP_pre_08.csv")



# 2003
IPTP.list[[1]] <-dataclean(IPTP.list[[1]], m49a_1, v005,'m49a_1', 'IPTP')  
IPTP.svyd03 <- svydesign.fun(IPTP.list[[1]])


DS_IPTP_pre_03 <- result.fun('IPTP', 'LGA','num_p', design=IPTP.svyd03)
head(DS_IPTP_pre_03)

write.csv(DS_IPTP_pre_03, "results/IPTP/DS_IPTP_pre_03.csv")



# cluster-level estimates

# 2018

clu_IPTP_pre_18 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd18,IPTP.list[[6]])
head(clu_IPTP_pre_18)

write.csv(clu_IPTP_pre_18, "results/IPTP/clu_IPTP_pre_18.csv")



# 2015

clu_IPTP_pre_15 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd15,IPTP.list[[5]])
head(clu_IPTP_pre_15)

write.csv(clu_IPTP_pre_15, "results/IPTP/clu_IPTP_pre_15.csv")


# 2013

clu_IPTP_pre_13 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd13,IPTP.list[[4]])
head(clu_IPTP_pre_13)

write.csv(clu_IPTP_pre_13, "results/IPTP/clu_IPTP_pre_13.csv")



# 2010

clu_IPTP_pre_10 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd10,IPTP.list[[3]])
head(clu_IPTP_pre_10)

write.csv(clu_IPTP_pre_10, "results/IPTP/clu_IPTP_pre_10.csv")


# 2008

clu_IPTP_pre_08 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd08,IPTP.list[[2]])
head(clu_IPTP_pre_08)

write.csv(clu_IPTP_pre_08, "results/IPTP/clu_IPTP_pre_08.csv")


# 2003

clu_IPTP_pre_03 <- result.clu.fun('IPTP', 'v001', design=IPTP.svyd03,IPTP.list[[1]])
head(clu_IPTP_pre_03)

write.csv(clu_IPTP_pre_03, "results/IPTP/clu_IPTP_pre_03.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- LGAshp_sf %>% left_join(DS_IPTP_pre_18)

pts_file <- NGAshplist_sf[[7]] %>% left_join(clu_IPTP_pre_18)


# 2015 transformations 
DS_file_15 <- LGAshp_sf %>% left_join(DS_IPTP_pre_15)

pts_file_15 <- NGAshplist_sf[[6]] %>% left_join(clu_IPTP_pre_15)


# 2013 transformations 
DS_file_13 <- LGAshp_sf %>% left_join(DS_IPTP_pre_13)

pts_file_13 <- NGAshplist_sf[[5]] %>% left_join(clu_IPTP_pre_13)


# 2010 transformations 
DS_file_10 <- LGAshp_sf %>% left_join(DS_IPTP_pre_10)

pts_file_10 <- NGAshplist_sf[[4]] %>% left_join(clu_IPTP_pre_10)


# 2008 transformations 
DS_file_08 <- LGAshp_sf %>% left_join(DS_IPTP_pre_08)

pts_file_08 <- NGAshplist_sf[[3]] %>% left_join(clu_IPTP_pre_08)



# 2003 transformations 
DS_file_03 <- LGAshp_sf %>% left_join(DS_IPTP_pre_03)

pts_file_03 <- NGAshplist_sf[[2]] %>% left_join(clu_IPTP_pre_03)

# 2018 map 
NGA_IPTP18 <- tmap.fun3(DS_file, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                       maintitle="IPTP coverage by District (2018)", ptsfile=pts_file, "Number of Participants",
                       "IPTP") 


# 2015 map 
NGA_IPTP15 <- tmap.fun3(DS_file_15, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (2015)", ptsfile=pts_file_15, "Number of Participants",
                        "IPTP") 


# 2013 map 
NGA_IPTP13 <- tmap.fun3(DS_file_13, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (2013)", ptsfile=pts_file_13, "Number of Participants",
                        "IPTP") 


# 2010 map 
NGA_IPTP10 <- tmap.fun3(DS_file_10, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (2010)", ptsfile=pts_file_10, "Number of Participants",
                        "IPTP") 


# 2008 map 
NGA_IPTP08 <- tmap.fun3(DS_file_08, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (2008)", ptsfile=pts_file_08, "Number of Participants",
                        "IPTP") 

# 2003 map 
NGA_IPTP03 <- tmap.fun3(DS_file_03, colname="IPTP", legtitle="IPTP coverage (most recent child)", 
                        maintitle="IPTP coverage by District (2003)", ptsfile=pts_file_03, "Number of Participants",
                        "IPTP") 

all_IPTP <- tmap_arrange(NGA_IPTP03, NGA_IPTP08, NGA_IPTP10, NGA_IPTP13, NGA_IPTP15, NGA_IPTP18)


tmap_save(tm = NGA_IPTP18,filename = "results/IPTP/NGA_IPTP18.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

