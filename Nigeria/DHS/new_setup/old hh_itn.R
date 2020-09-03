######
look_for(itn.ls[[1]], "itn" )

PR.list <- list(NGAfiles[[6]], NGAfiles[[9]], NGAfiles[[12]], NGAfiles[[15]], NGAfiles[[18]], NGAfiles[[21]])

PR.list <- lapply(PR.list, subset, hv103 == "1")

PR.list <- map(PR.list, recode_itn)



PR.list <- map(PR.list, survey.month.fun)

# key list for ITN (2003, 2008, 2010, 2013, 2015, 2017/2018)
keys.hh.itn <- list(key_list[[2]], key_list[[3]], key_list[[4]], key_list[[5]],key_list[[6]], key_list[[7]]) 
#changing to a list of keys 


# key datasets and dhs/mis datasets are joined  
hh.itn.list <- map2(PR.list, keys.hh.itn, left_join) #PR datasets

rep_DS.ls <- list(rep_DS)

hh.itn.list <- map2(hh.itn.list, rep_DS.ls, left_join) #PR datasets
