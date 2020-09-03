# Adult ITN list for 2003, 2008, 2010, 2013, 2015
A_ITN.list <- list(NGAfiles[[4]], NGAfiles[[7]], NGAfiles[[10]], NGAfiles[[13]], NGAfiles[[16]], NGAfiles[[19]])


# recoding ml101 ITN 
A_ITN.list <- map(A_ITN.list, recoder.ml101)


# key list for ITN
keysITN <- list(key_list[[2]], key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]], key_list[[7]]) #changing to a list of keys 

A_ITN.list <-map2(A_ITN.list,keysITN, left_join) #ITN datasets 



#######################################################################################
# ml101 adult ITN use 
######################################################################################


# DS-level estimates 

# 2018
A_ITN.list[[6]] <-dataclean(A_ITN.list[[6]], ml101, v005,'ml101', 'ITN_A_treated')  
A_ITN.svyd18 <- svydesign.fun(A_ITN.list[[6]])

DS_ITNT_15 <- result.fun('ITN_A_treated', 'LGA','num_p', design=A_ITN.svyd18)
head(DS_ITNT_15)

write.csv(DS_ITNT_15, "DS_ITNT_15.csv")


# 2015
A_ITN.list[[5]] <-dataclean(A_ITN.list[[5]], ml101, v005,'ml101', 'ITN_A_treated')  
A_ITN.svyd15 <- svydesign.fun(A_ITN.list[[5]])

DS_ITNT_15 <- result.fun('ITN_A_treated', 'LGA','num_p', design=A_ITN.svyd15)
head(DS_ITNT_15)

write.csv(DS_ITNT_15, "DS_ITNT_15.csv")


# 2013
A_ITN.list[[4]] <-dataclean(A_ITN.list[[4]], ml101, v005,'ml101', 'ITN_A_treated')  
A_ITN.svyd13 <- svydesign.fun(A_ITN.list[[4]])

DS_ITNT_13 <- result.fun('ITN_A_treated', 'LGA','num_p', design=A_ITN.svyd13)
head(DS_ITNT_13)

write.csv(DS_ITNT_13, "DS_ITNT_13.csv")



# 2010
A_ITN.list[[3]] <-dataclean(A_ITN.list[[3]], ml101, v005,'ml101', 'ITN_A_treated')  
A_ITN.list[[3]]  <- A_ITN.list[[3]]  %>% left_join(rep_DS)

A_ITN.svyd10 <- svydesign.fun(A_ITN.list[[3]])

DS_ITNT_10 <- result.fun('ITN_A_treated', 'repDS','num_p', design=A_ITN.svyd10)
head(DS_ITNT_10)

write.csv(DS_ITNT_10, "results/ITN_repDS/Adults_DS_ITNT_10.csv")


# 2008
A_ITN.list[[2]] <-dataclean(A_ITN.list[[2]], ml101, v005,'ml101', 'ITN_A_treated')  
A_ITN.svyd08 <- svydesign.fun(A_ITN.list[[2]])

DS_ITNT_08 <- result.fun('ITN_A_treated', 'LGA','num_p', design=A_ITN.svyd08)
head(DS_ITNT_08)

write.csv(DS_ITNT_08, "DS_ITNT_08.csv")


# 2003
A_ITN.list[[1]] <-dataclean(A_ITN.list[[1]], ml101, v005,'ml101', 'ITN_A_treated')  
A_ITN.svyd03 <- svydesign.fun(A_ITN.list[[1]])

DS_ITNT_03 <- result.fun('ITN_A_treated', 'LGA','num_p', design=A_ITN.svyd03)
head(DS_ITNT_03)

write.csv(DS_ITNT_03, "DS_ITNT_03.csv")


# cluster-level estimates 

# 2015

clu_ITNT_15 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=A_ITN.svyd15,A_ITN.list[[5]])
head(clu_ITNT_15)

write.csv(clu_ITNT_15, "clu_ITNT_15.csv")


# 2013

clu_ITNT_13 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=A_ITN.svyd13,A_ITN.list[[4]])
head(clu_ITNT_13)

write.csv(clu_ITNT_13, "clu_ITNT_13.csv")


# 2010

clu_ITNT_10 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=A_ITN.svyd10,A_ITN.list[[3]])
head(clu_ITNT_10)

write.csv(clu_ITNT_10, "clu_ITNT_10.csv")


# 2008

clu_ITNT_08 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=A_ITN.svyd08,A_ITN.list[[2]])
head(clu_ITNT_08)

write.csv(clu_ITNT_08, "clu_ITNT_08.csv")


# 2003

clu_ITNT_03 <- result.clu.fun('ITN_A_treated', 'v001','num_p', design=A_ITN.svyd03,A_ITN.list[[1]])
head(clu_ITNT_03)

write.csv(clu_ITNT_03, "clu_ITNT_03.csv")
