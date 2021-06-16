
# creating list of files with PR data for 2008, 2010, 2013, 2015, 2018
PR.list <- list(NGAfiles[[9]], NGAfiles[[12]], 
                NGAfiles[[15]], NGAfiles[[18]], NGAfiles[[21]])


PR.list <- lapply(PR.list, subset, hv103 == "1")

PR.list <- map(PR.list, recode_itn)

PR.list <- map(PR.list, survey.month.fun)

# key list for ITN (2008, 2010, 2013, 2015, 2018)
keys.hh.itn <- list(key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]], key_list[[7]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
hh.itn.list <- map2(PR.list, keys.hh.itn, left_join) #PR datasets


#####################################################################################################
# ANC coverage 
####################################################################################################

# 2018
hh.itn.list[[5]] <-dataclean.HH(hh.itn.list[[5]], hml12, hv005,'hh_itn', 'hh_itn')  
hh.itn.svyd18 <- svydesign.fun(hh.itn.list[[5]])


DS_hh_itn_18 <- result.fun('hh_itn', 'LGA','num_p', design=hh.itn.svyd18)
head(DS_hh_itn_18)

summary(DS_hh_itn_18$hh_itn)

write.csv(DS_ANC_pre_18, "results/ANC_Nigeria/DS_ANC_pre_18.csv")






