#analysis 2017 

#household dataset for U5
U5_HH_18 <- BFfiles[[18]]
U5_HH_18$age_months <- U5_HH_18$hv008 - U5_HH_18$hc32
U5_HH_18 <- U5_HH_18 %>% filter(age_months<=60) %>% dplyr::select(hvidx,hv001,hv002,hv007,hv008,hv016, 
                                                                  hv227,hv005,age_months,hv022,hv021)

table(U5_HH_18$age_months)
table(U5_HH_18$hv227)



#child dataset 
U5_child_18 <- BFfiles[[17]] %>% dplyr::select(hv001=v001, hv002=v002, ml0, hvidx=b16)


U5_child_18 <- recoder.ml0(U5_child_18)

#joining both datasets and restricting dataset to only those that responded that they have a net 
U5_ITN_access_use <- U5_HH_18 %>% left_join(U5_child_18) %>% filter(hv227 ==1)


table(U5_HH_18$hv227)
table(U5_ITN_access_use$hv227)

table(U5_child_18$ml0)
summary(U5_ITN_access_use$ml0)

 
#apply survey month function and join to key list 

U5_ITN_access_use<- survey.month.fun(U5_ITN_access_use)
U5_ITN_access_use <-U5_ITN_access_use %>% left_join(key_list[[6]])

var_label(U5_ITN_access_use$ml0)


#####################################################################################################
# U5  coverage 
####################################################################################################

# 2018
U5_ITN_access_use <-dataclean.HH(U5_ITN_access_use, ml0, hv005,'ml0', 'ITN_use')  
U5_ITN.svyd18 <- svydesign.fun(U5_ITN_access_use)


DS_U5_ITN_18 <- result.fun('ITN_use', 'NOMDEP','num_p', design=U5_ITN.svyd18)
head(DS_U5_ITN_18)

write.csv(DS_U5_ITN_18, "results/U5_ITN_use_onlyaccess/DS_U5_ITN_18.csv")


# cluster-level estimates

# 2018

clu_U5_ITN_18 <- result.clu.fun('ITN_use', 'v001', design=U5_ITN.svyd18,U5_ITN_access_use)
head(clu_U5_ITN_18)

write.csv(clu_U5_ITN_18, "results/U5_ITN_use_onlyaccess/clu_U5_ITN_18.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- DS_shape_sf %>% left_join(DS_U5_ITN_18)

pts_file <- BFshplist_sf[[6]] %>% left_join(clu_U5_ITN_18)



 
 

