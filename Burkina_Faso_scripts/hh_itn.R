# creating list of files with PR data for 2003, 2010, 2014, 2017/2018
PR.list <- list(BFfiles[[9]], BFfiles[[12]], BFfiles[[15]], BFfiles[[18]])

PR.list <- lapply(PR.list, subset, hv103 == "1")

PR.list <- map(PR.list, recode_itn)

PR.list <- map(PR.list, survey.month.fun)

# key list for ITN (2003, 2010, 2014, 2017/2018)
keys.hh.itn <- list(key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
hh.itn.list <- map2(PR.list, keys.hh.itn, left_join) #PR datasets

table(PR.list[[1]]$time2)

#####################################################################################################
# ANC coverage 
####################################################################################################

# 2017/18
hh.itn.list[[4]] <-dataclean.HH(hh.itn.list[[4]], hml12, hv005,'hh.itn', 'hh.itn')  
hh.itn.svyd18 <- svydesign.fun(hh.itn.list[[4]])


DS_hh_itn_18 <- result.fun('hh.itn', 'NOMDEP','num_p', design=hh.itn.svyd18)
head(DS_hh_itn_18)

summary(DS_hh_itn_18$hh.itn)

write.csv(DS_hh_itn_18, "results/DHS_HH_ITN/DS_hh_itn_18.csv")



# 2014
hh.itn.list[[3]] <-dataclean.HH(hh.itn.list[[3]], hml12, hv005,'hh.itn', 'hh.itn')  
hh.itn.svyd14 <- svydesign.fun(hh.itn.list[[3]])


DS_hh_itn_14 <- result.fun('hh.itn', 'NOMDEP','num_p', design=hh.itn.svyd14)
head(DS_hh_itn_14)

summary(DS_hh_itn_14$hh.itn)

write.csv(DS_hh_itn_14, "results/DHS_HH_ITN/DS_hh_itn_14.csv")



# 2010
hh.itn.list[[2]] <-dataclean.HH(hh.itn.list[[2]], hml12, hv005,'hh.itn', 'hh.itn')  
hh.itn.svyd10 <- svydesign.fun(hh.itn.list[[2]])


DS_hh_itn_10 <- result.fun('hh.itn', 'NOMDEP','num_p', design=hh.itn.svyd10)
head(DS_hh_itn_10)

summary(DS_hh_itn_10$hh.itn)

write.csv(DS_hh_itn_10, "results/DHS_HH_ITN/DS_hh_itn_10.csv")



# 2003
hh.itn.list[[1]] <-dataclean2.HH(hh.itn.list[[1]], hml12, hv005,'hh.itn', 'hh.itn', 'hv023')  
hh.itn.svyd03 <- svydesign.fun(hh.itn.list[[1]])


DS_hh_itn_03 <- result.fun('hh.itn', 'NOMDEP','num_p', design=hh.itn.svyd03)
head(DS_hh_itn_03)

summary(DS_hh_itn_03$hh.itn)

write.csv(DS_hh_itn_03, "results/DHS_HH_ITN/DS_hh_itn_03.csv")




# cluster-level estimates

# 2018

clu_HH_ITN_18 <- result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd18,hh.itn.list[[4]])
head(clu_HH_ITN_18)

write.csv(clu_HH_ITN_18, "results/DHS_HH_ITN/clu_hh_itn_18.csv")


# 2014
clu_HH_ITN_14 <- result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd14,hh.itn.list[[3]])
head(clu_HH_ITN_14)

write.csv(clu_HH_ITN_14, "results/DHS_HH_ITN/clu_hh_itn_14.csv")


# 2010
clu_HH_ITN_10 <- result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd10,hh.itn.list[[2]])
head(clu_HH_ITN_10)

write.csv(clu_HH_ITN_10, "results/DHS_HH_ITN/clu_hh_itn_10.csv")


# 2003
clu_HH_ITN_03 <- result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd03,hh.itn.list[[1]])
head(clu_HH_ITN_03)

write.csv(clu_HH_ITN_03, "results/DHS_HH_ITN/clu_hh_itn_03.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_hh_itn_18)

pts_file <- BFshplist_sf[[6]] %>% left_join(clu_HH_ITN_18)


# 2014 transformations 
DS_file_14 <- DS_shape_sf %>% left_join(DS_hh_itn_14)

pts_file_14 <- BFshplist_sf[[5]] %>% left_join(clu_HH_ITN_14)


# 2010 transformations 
DS_file_10 <- DS_shape_sf %>% left_join(DS_hh_itn_10)

pts_file_10 <- BFshplist_sf[[4]] %>% left_join(clu_HH_ITN_10)



# 2003 transformations 
DS_file_03 <- DS_shape_sf %>% left_join(DS_hh_itn_03)

pts_file_03 <- BFshplist_sf[[3]] %>% left_join(clu_HH_ITN_03)

# 2018 map 
BF_HH_ITN18 <- tmap.fun5(DS_file, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                         maintitle="Household (HH) ITN use by District (2018)", ptsfile=pts_file, "Number of Participants",
                         "hh.itn")


# 2014 map 
BF_HH_ITN14 <- tmap.fun5(DS_file_14, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                         maintitle="Household (HH) ITN use by District (2014)", ptsfile=pts_file_14, "Number of Participants",
                         "hh.itn")


# 2010 map 
BF_HH_ITN10 <- tmap.fun5(DS_file_10, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                         maintitle="Household (HH) ITN use by District (2010)", ptsfile=pts_file_10, "Number of Participants",
                         "hh.itn")


# 2003 map 
BF_HH_ITN03 <- tmap.fun5(DS_file_03, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
                         maintitle="Household (HH) ITN use by District (2003)", ptsfile=pts_file_03, "Number of Participants",
                         "hh.itn")

all_hh.itn <- tmap_arrange(BF_HH_ITN03,BF_HH_ITN10, BF_HH_ITN14,  BF_HH_ITN18)


tmap_save(tm = BF_HH_ITN18, filename = "results/DHS_HH_ITN/BF_HH_ITN18.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



