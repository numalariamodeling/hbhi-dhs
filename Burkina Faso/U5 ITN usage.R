# creating list of files with ANC data for 2003, 2010, 2014, 2017/2018 
U5_ITN.list <- list(BFfiles[[8]], BFfiles[[11]], BFfiles[[14]], BFfiles[[17]])

val_labels(U5_ITN.list[[4]]$ml0)

U5_ITN.list  <- map(U5_ITN.list, recoder.ml0)


# key list for ANC
keys.U5_ITN <- list(key_list[[3]],key_list[[4]], key_list[[5]], key_list[[6]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
U5_ITN.list <- map2(U5_ITN.list, keys.U5_ITN, left_join) #PR datasets



#####################################################################################################
# U5  coverage 
####################################################################################################

# 2018
U5_ITN.list[[4]] <-dataclean(U5_ITN.list[[4]], ml0, v005,'ml0', 'ITN')  
U5_ITN.svyd18 <- svydesign.fun(U5_ITN.list[[4]])


DS_U5_ITN_18 <- result.fun('ITN', 'NOMDEP','num_p', design=U5_ITN.svyd18)
head(DS_U5_ITN_18)

write.csv(DS_U5_ITN_18, "results/U5_ITN/DS_U5_ITN_18.csv")


# 2014
U5_ITN.list[[3]] <-dataclean(U5_ITN.list[[3]], ml0, v005,'ml0', 'ITN')  
U5_ITN.svyd14 <- svydesign.fun(U5_ITN.list[[3]])


DS_U5_ITN_14 <- result.fun('ITN', 'NOMDEP','num_p', design=U5_ITN.svyd14)
head(DS_U5_ITN_14)

write.csv(DS_U5_ITN_14, "results/U5_ITN/DS_U5_ITN_14.csv")



# 2010
U5_ITN.list[[2]] <-dataclean(U5_ITN.list[[2]], ml0, v005,'ml0', 'ITN')  
U5_ITN.svyd10 <- svydesign.fun(U5_ITN.list[[2]])


DS_U5_ITN_10 <- result.fun('ITN', 'NOMDEP','num_p', design=U5_ITN.svyd10)
head(DS_U5_ITN_10)

write.csv(DS_U5_ITN_14, "results/U5_ITN/DS_U5_ITN_10.csv")


# 2003
U5_ITN.list[[1]] <-dataclean2(U5_ITN.list[[1]], ml0, v005,'ml0', 'ITN', 'v023')  
U5_ITN.svyd03 <- svydesign.fun(U5_ITN.list[[1]])



DS_U5_ITN_03 <- result.fun('ITN', 'NOMDEP','num_p', design=U5_ITN.svyd03)
head(DS_U5_ITN_03)

write.csv(DS_U5_ITN_03, "results/U5_ITN/DS_U5_ITN_03.csv")



# cluster-level estimates

# 2018

clu_U5_ITN_18 <- result.clu.fun('ITN', 'v001', design=U5_ITN.svyd18,U5_ITN.list[[4]])
head(clu_U5_ITN_18)

write.csv(clu_U5_ITN_18, "results/U5_ITN/clu_U5_ITN_18.csv")


# 2014

clu_U5_ITN_14 <- result.clu.fun('ITN', 'v001', design=U5_ITN.svyd14,U5_ITN.list[[3]])
head(clu_U5_ITN_14)

write.csv(clu_U5_ITN_14, "results/U5_ITN/clu_U5_ITN_14.csv")


# 2010

clu_U5_ITN_10 <- result.clu.fun('ITN', 'v001', design=U5_ITN.svyd10,U5_ITN.list[[2]])
head(clu_U5_ITN_10)

write.csv(clu_U5_ITN_10, "results/U5_ITN/clu_U5_ITN_10.csv")



# 2003

clu_U5_ITN_03 <- result.clu.fun('ITN', 'v001', design=U5_ITN.svyd03,U5_ITN.list[[1]])
head(clu_U5_ITN_03)

write.csv(clu_U5_ITN_03, "results/U5_ITN/clu_U5_ITN_03.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_U5_ITN_18)

pts_file <- BFshplist_sf[[6]] %>% left_join(clu_U5_ITN_18)


# 2014 transformations 
DS_file_14 <- DS_shape_sf %>% left_join(DS_U5_ITN_14)

pts_file_14 <- BFshplist_sf[[5]] %>% left_join(clu_U5_ITN_14)


# 2010 transformations 
DS_file_10 <- DS_shape_sf %>% left_join(DS_U5_ITN_10)

pts_file_10 <- BFshplist_sf[[4]] %>% left_join(clu_U5_ITN_10)



# 2003 transformations 
DS_file_03 <- DS_shape_sf %>% left_join(DS_U5_ITN_03)

pts_file_03 <- BFshplist_sf[[3]] %>% left_join(clu_U5_ITN_03)




# 2018 map 
BF_ITN18 <- tmap.fun5(DS_file, colname="ITN", legtitle="ITN use among under-fives", 
                      maintitle="ITN use among under-fives by District (2018)", ptsfile=pts_file, "Number of Participants",
                      "ITN") 


# 2014 map 
BF_ITN14 <- tmap.fun5(DS_file_14, colname="ITN", legtitle="ITN use among under-fives", 
                      maintitle="ITN use among under-fives by District (2014)", ptsfile=pts_file_14, "Number of Participants",
                      "ITN") 


# 2010 map 
BF_ITN10 <- tmap.fun5(DS_file_10, colname="ITN", legtitle="ITN use among under-fives", 
                      maintitle="ITN use among under-fives by District (2010)", ptsfile=pts_file_10, "Number of Participants",
                      "ITN") 


# 2003 map 
BF_ITN03 <- tmap.fun5(DS_file_03, colname="ITN", legtitle="ITN use among under-fives", 
                      maintitle="ITN use among under-fives by District (2003)", ptsfile=pts_file_03, "Number of Participants",
                      "ITN") 


all_U5_ITN <- tmap_arrange(BF_ITN03, BF_ITN10, BF_ITN14, BF_ITN18)


tmap_save(tm = BF_ITN03, filename = "results/U5_ITN/BF_ITN03.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
