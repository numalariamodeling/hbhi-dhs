# creating list of files with ANC data for 2003, 2010, 2014, 2017/2018 
HH_ITN.list <- list(BFfiles[[9]], BFfiles[[12]], BFfiles[[15]], BFfiles[[18]])

look(BFfiles[[18]])

# creates two new variables named hh_net (number of household members per net) and net_ratio 
#(categorical variable- if > 2 then access is 0, If 2 or less than access is 1)
HH_ITN.list  <- map(HH_ITN.list, recoder.nets)
table(HH_ITN.list[[4]]$net_ratio)

HH_ITN.list <- map(HH_ITN.list, survey.month.fun)#creates survey month and changes hv001 to v001 to enable left_join 

# key list for ANC
keys.HH_ITN <- list(key_list[[3]],key_list[[4]], key_list[[5]], key_list[[6]]) 
#changing to a list of keys 



# key datasets and dhs/mis datasets are joined  
HH_ITN.list <- map2(HH_ITN.list, keys.HH_ITN, left_join) #PR datasets


#####################################################################################################
# HH_ITN_access
####################################################################################################

# 2018
HH_ITN.list[[4]] <-dataclean.HH(HH_ITN.list[[4]], net_ratio, hv005,'net_ratio', 'net_ratio')  
HH_ITN.svyd18 <- svydesign.fun(HH_ITN.list[[4]])

table(HH_ITN.list[[4]]$net_ratio)

DS_HH_ITN_18 <- result.fun.HH('net_ratio', 'NOMDEP','num_p', design=HH_ITN.svyd18)
head(DS_HH_ITN_18)

write.csv(DS_HH_ITN_18, "results/HH_ITN/DS_HH_ITN_18.csv")


# 2014
HH_ITN.list[[3]] <-dataclean.HH(HH_ITN.list[[3]], net_ratio, hv005,'net_ratio', 'net_ratio')  
HH_ITN.svyd14 <- svydesign.fun(HH_ITN.list[[3]])


DS_HH_ITN_14 <- result.fun.HH('net_ratio', 'NOMDEP','num_p', design=HH_ITN.svyd14)
head(DS_HH_ITN_14)

write.csv(DS_HH_ITN_14, "results/HH_ITN/DS_HH_ITN_14.csv")


# 2010
HH_ITN.list[[2]] <-dataclean.HH(HH_ITN.list[[2]], net_ratio, hv005,'net_ratio', 'net_ratio')  
HH_ITN.svyd10 <- svydesign.fun(HH_ITN.list[[2]])


DS_HH_ITN_10 <- result.fun.HH('net_ratio', 'NOMDEP','num_p', design=HH_ITN.svyd10)
head(DS_HH_ITN_10)

write.csv(DS_HH_ITN_10, "results/HH_ITN/DS_HH_ITN_10.csv")


# 2003
HH_ITN.list[[1]] <-dataclean2.HH(HH_ITN.list[[1]], net_ratio, hv005,'net_ratio', 'net_ratio','hv023' )  
HH_ITN.svyd03 <- svydesign.fun(HH_ITN.list[[1]])


DS_HH_ITN_03 <- result.fun.HH('net_ratio', 'NOMDEP','num_p', design=HH_ITN.svyd03)
head(DS_HH_ITN_03)

write.csv(DS_HH_ITN_03, "results/HH_ITN/DS_HH_ITN_03.csv")



# cluster-level estimates

# 2018

clu_HH_ITN_18 <- result.clu.fun.HH('net_ratio', 'v001', design=HH_ITN.svyd18,HH_ITN.list[[4]])
head(clu_HH_ITN_18)

write.csv(clu_HH_ITN_18, "results/HH_ITN/clu_U5_ITN_18.csv")


# 2014

clu_HH_ITN_14 <- result.clu.fun.HH('net_ratio', 'v001', design=HH_ITN.svyd14,HH_ITN.list[[3]])
head(clu_HH_ITN_14)

write.csv(clu_HH_ITN_14, "results/HH_ITN/clu_U5_ITN_14.csv")


# 2010

clu_HH_ITN_10 <- result.clu.fun.HH('net_ratio', 'v001', design=HH_ITN.svyd10,HH_ITN.list[[2]])
head(clu_HH_ITN_10)

write.csv(clu_HH_ITN_10, "results/HH_ITN/clu_U5_ITN_10.csv")


# 2003

clu_HH_ITN_03 <- result.clu.fun.HH('net_ratio', 'v001', design=HH_ITN.svyd03,HH_ITN.list[[1]])
head(clu_HH_ITN_03)

write.csv(clu_HH_ITN_03, "results/HH_ITN/clu_U5_ITN_03.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_HH_ITN_18)

pts_file <- BFshplist_sf[[6]] %>% left_join(clu_HH_ITN_18)



# 2014 transformations 
DS_file_14 <- DS_shape_sf %>% left_join(DS_HH_ITN_14)

pts_file_14 <- BFshplist_sf[[5]] %>% left_join(clu_HH_ITN_14)


# 2010 transformations 
DS_file_10 <- DS_shape_sf %>% left_join(DS_HH_ITN_10)

pts_file_10 <- BFshplist_sf[[4]] %>% left_join(clu_HH_ITN_10)


# 2003 transformations 
DS_file_03 <- DS_shape_sf %>% left_join(DS_HH_ITN_03)

pts_file_03 <- BFshplist_sf[[3]] %>% left_join(clu_HH_ITN_03)



# 2018 map 
BF_HH_ITN18 <- tmap.fun5(DS_file, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                      maintitle="Household (HH) ITN access by District (2018)", ptsfile=pts_file, "Number of Households",
                      "net_ratio")


# 2014 map 
BF_HH_ITN14 <- tmap.fun5(DS_file_14, colname="net_ratio", legtitle="HH with 2 or fewer  persons per net", 
                         maintitle="Household (HH) ITN access by District (2014)", ptsfile=pts_file_14, "Number of Households",
                         "net_ratio") 

# 2010 map 
BF_HH_ITN10 <- tmap.fun5(DS_file_10, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                         maintitle="Household (HH) ITN access by District (2010)", ptsfile=pts_file_10, "Number of Households",
                         "net_ratio")

# 2003 map 
BF_HH_ITN03 <- tmap.fun5(DS_file_03, colname="net_ratio", legtitle="HH with 2 or fewer persons per net", 
                         maintitle="Household (HH) ITN access by District (2003)", ptsfile=pts_file_03, "Number of Households",
                         "net_ratio") 

all_HH_ITN <- tmap_arrange(BF_HH_ITN03,BF_HH_ITN10,BF_HH_ITN14,BF_HH_ITN18)


tmap_save(tm = BF_HH_ITN18, filename = "results/HH_ITN/BF18_HH_ITN.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)






