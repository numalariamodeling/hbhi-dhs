#U5 bed net list includes 2003, 2010, 2014, 2017/18

U5_ITN_access <- list(BFfiles[[9]],BFfiles[[12]], BFfiles[[15]], BFfiles[[18]])

#recoding variables 
U5_ITN_access  <- map(U5_ITN_access, recoder.hv227)

# table(U5_ITN_access[[1]]$hv227)
# var_label(U5_ITN_access[[4]]$hv228)


# key list for ANC
keys.U5_ITN_access <- list(key_list[[3]],key_list[[4]], key_list[[5]], key_list[[6]]) 
#changing to a list of keys 

U5_ITN_access <- map(U5_ITN_access, survey.month.fun)


# key datasets and dhs/mis datasets are joined  
U5_ITN_access <- map2(U5_ITN_access, keys.U5_ITN_access, left_join) #PR datasets

#####################################################################################################
# U5 ITN coverage 
####################################################################################################

# 2018
U5_ITN_access[[4]] <- U5_ITN_access[[4]] %>% filter(hv105<=5)

table(U5_ITN_access[[4]]$U5_ITN_access)

U5_ITN_access[[4]] <-dataclean.HH(U5_ITN_access[[4]], hv227, hv005,'hv227', 'U5_ITN_access') 

U5_ITN_access.svyd18 <- svydesign.fun(U5_ITN_access[[4]])


DS_U5a_pre_18 <- result.fun('U5_ITN_access', 'NOMDEP','num_p', design=U5_ITN_access.svyd18)
head(DS_U5a_pre_18)

write.csv(DS_U5a_pre_18, "results/U5_ITN_access/DS_U5a_pre_18.csv")


# 2014
U5_ITN_access[[3]] <- U5_ITN_access[[3]] %>% filter(hv105<=5)

U5_ITN_access[[3]] <-dataclean.HH(U5_ITN_access[[3]], hv227, hv005,'hv227', 'U5_ITN_access')  
U5_ITN_access.svyd14 <- svydesign.fun(U5_ITN_access[[3]])


DS_U5a_pre_14 <- result.fun('U5_ITN_access', 'NOMDEP','num_p', design=U5_ITN_access.svyd14)
head(DS_U5a_pre_14)

write.csv(DS_U5a_pre_14, "results/U5_ITN_access/DS_U5a_pre_14.csv")


# 2010
U5_ITN_access[[2]] <- U5_ITN_access[[2]] %>% filter(hv105<=5)

U5_ITN_access[[2]] <-dataclean.HH(U5_ITN_access[[2]], hv227, hv005,'hv227', 'U5_ITN_access')  
U5_ITN_access.svyd10 <- svydesign.fun(U5_ITN_access[[2]])


DS_U5a_pre_10 <- result.fun('U5_ITN_access', 'NOMDEP','num_p', design=U5_ITN_access.svyd10)
head(DS_U5a_pre_10)

write.csv(DS_U5a_pre_10, "results/U5_ITN_access/DS_U5a_pre_10.csv")


# 2003
U5_ITN_access[[1]] <- U5_ITN_access[[1]] %>% filter(hv105<=5)
U5_ITN_access[[1]] <-dataclean2.HH(U5_ITN_access[[1]], hv227, hv005,'hv227', 'U5_ITN_access', 'v023')  
U5_ITN_access.svyd03 <- svydesign.fun(U5_ITN_access[[1]])


DS_U5a_pre_03 <- result.fun('U5_ITN_access', 'NOMDEP','num_p', design=U5_ITN_access.svyd03)
head(DS_U5a_pre_03)

write.csv(DS_U5a_pre_03, "results/U5_ITN_access/DS_U5a_pre_03.csv")


#cluster-level estimates 

# 2018

clu_U5a_pre_18 <- result.clu.fun('U5_ITN_access', 'v001', design=U5_ITN_access.svyd18,
                                 U5_ITN_access[[4]])
head(clu_U5a_pre_18)

write.csv(clu_U5a_pre_18, "results/U5_ITN_access/clu_U5a_pre_18.csv")



# 2014

clu_U5a_pre_14 <- result.clu.fun('U5_ITN_access', 'v001', design=U5_ITN_access.svyd14,
                                 U5_ITN_access[[3]])
head(clu_U5a_pre_14)

write.csv(clu_U5a_pre_14, "results/U5_ITN_access/clu_U5a_pre_14.csv")



# 2010

clu_U5a_pre_10 <- result.clu.fun('U5_ITN_access', 'v001', design=U5_ITN_access.svyd10,
                                 U5_ITN_access[[2]])
head(clu_U5a_pre_10)

write.csv(clu_U5a_pre_10, "results/U5_ITN_access/clu_U5a_pre_10.csv")



# 2003

clu_U5a_pre_03 <- result.clu.fun('U5_ITN_access', 'v001', design=U5_ITN_access.svyd03,
                                 U5_ITN_access[[1]])
head(clu_U5a_pre_03)

write.csv(clu_U5a_pre_03, "results/U5_ITN_access/clu_U5a_pre_03.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_U5a_pre_18)

pts_file <- BFshplist_sf[[6]] %>% left_join(clu_U5a_pre_18)


# 2014 transformations 
DS_file_14 <- DS_shape_sf %>% left_join(DS_U5a_pre_14)

pts_file_14 <- BFshplist_sf[[5]] %>% left_join(clu_U5a_pre_14)


# 2010 transformations 
DS_file_10 <- DS_shape_sf %>% left_join(DS_U5a_pre_10)

pts_file_10 <- BFshplist_sf[[4]] %>% left_join(clu_U5a_pre_10)


# 2003 transformations 
DS_file_03 <- DS_shape_sf %>% left_join(DS_U5a_pre_03)

pts_file_03 <- BFshplist_sf[[3]] %>% left_join(clu_U5a_pre_03)


# 2018 map 
BF_U5a18 <- tmap.fun5(DS_file, colname="U5_ITN_access", legtitle="U5 ITN access", 
                      maintitle="Insecticide Treated Net Access among Under-fives by District (2018)", ptsfile=pts_file, "Number of Participants",
                      "U5_ITN_access") 


# 2014 map 
BF_U5a14 <- tmap.fun5(DS_file_14, colname="U5_ITN_access", legtitle="U5 ITN access", 
                      maintitle="Insecticide Treated Net Access among Under-fives by District (2014)", ptsfile=pts_file_14, "Number of Participants",
                      "U5_ITN_access") 

# 2010 map 
BF_U5a10 <- tmap.fun5(DS_file_10, colname="U5_ITN_access", legtitle="U5 ITN access", 
                      maintitle="Insecticide Treated Net Access among Under-fives by District (2010)", ptsfile=pts_file_10, "Number of Participants",
                      "U5_ITN_access") 

# 2003 map 
BF_U5a03 <- tmap.fun5(DS_file_03, colname="U5_ITN_access", legtitle="U5 ITN access", 
                      maintitle="Insecticide Treated Net Access among Under-fives by District (2003)", ptsfile=pts_file_03, "Number of Participants",
                      "U5_ITN_access") 


all_U5a <- tmap_arrange(BF_U5a03, BF_U5a10, BF_U5a14, BF_U5a18)

tmap_save(tm = BF_U5a18, filename = "results/U5_ITN_access/BF_U5a18.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)










