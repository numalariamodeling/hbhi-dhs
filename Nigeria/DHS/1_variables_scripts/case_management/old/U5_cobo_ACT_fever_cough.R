# identifying the combo with ACT variable 
look_for(NGAfiles[[8]], "combination")

# recoding variables 
NGAfiles[[11]][,"ml13e"] <-recoder(NGAfiles[[11]][,"ml13e"]) 
table(NGAfiles[[11]]$ml13e)

NGAfiles[[14]][,"ml13e"] <-recoder(NGAfiles[[14]][,"ml13e"]) 
table(NGAfiles[[14]]$ml13e)

# U5 medical treatment for fever list for 2008, 2010, 2013, 2015, 2018
comboACT.list <- list(NGAfiles[[8]], NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]], NGAfiles[[20]])
key.comboACT<-list(key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]], key_list[[7]])


# key datasets and dhs/mis datasets are joined  
comboACT.list <-map2(comboACT.list,key.comboACT, left_join) #medfever datasets 



# state level estimates 

# 2018

comboACT.list[[5]]<-dataclean(comboACT.list[[5]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


comboACT.svyd18 <- svydesign.fun(comboACT.list[[5]])


comboACT_pre_18 <- result.fun('comboACT', 'sstate','num_p', design=comboACT.svyd18)
head(comboACT_pre_18)

iLabels_18 <- val_labels(comboACT.list[[5]]$sstate)

match.idx_18 <- match(comboACT_pre_18$sstate, iLabels_18)

comboACT_pre_18$ADM1_NAME <- ifelse(is.na(match.idx_18),
                                    comboACT_pre_18$ADM1_NAME,
                                  names(iLabels_18)[match.idx_18])

comboACT_pre_18$ADM1_NAME <- str_to_title(comboACT_pre_18$ADM1_NAME)
head(comboACT_pre_18)

write.csv(comboACT_pre_18, "results/comboACT_pre_18.csv")



# 2015

comboACT.list[[4]]<-dataclean(comboACT.list[[4]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


comboACT.svyd15 <- svydesign.fun(comboACT.list[[4]])


comboACT_pre_15 <- result.fun('comboACT', 'sstate','num_p', design=comboACT.svyd15)
head(comboACT_pre_15)

iLabels_15 <- val_labels(comboACT.list[[4]]$sstate)

match.idx_15 <- match(comboACT_pre_15$sstate, iLabels_15)

comboACT_pre_15$ADM1_NAME <- ifelse(is.na(match.idx_15),
                                    comboACT_pre_15$ADM1_NAME,
                                    names(iLabels_15)[match.idx_15])

comboACT_pre_15$ADM1_NAME <- str_to_title(comboACT_pre_15$ADM1_NAME)
head(comboACT_pre_15)

write.csv(comboACT_pre_15, "results/comboACT_pre_15.csv")



# 2013

comboACT.list[[3]]<-dataclean(comboACT.list[[3]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


comboACT.svyd13 <- svydesign.fun(comboACT.list[[3]])


comboACT_pre_13 <- result.fun('comboACT', 'sstate','num_p', design=comboACT.svyd13)
head(comboACT_pre_13)

iLabels_13 <- val_labels(comboACT.list[[3]]$sstate)

match.idx_13 <- match(comboACT_pre_13$sstate, iLabels_13)

comboACT_pre_13$ADM1_NAME <- ifelse(is.na(match.idx_13),
                                    comboACT_pre_13$ADM1_NAME,
                                    names(iLabels_13)[match.idx_13])

comboACT_pre_13$ADM1_NAME <- str_to_title(comboACT_pre_13$ADM1_NAME)
head(comboACT_pre_13)

comboACT_pre_13 <- comboACT_pre_13%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja"))

write.csv(comboACT_pre_13, "results/comboACT_pre_13.csv")



# 2010

comboACT.list[[2]]<-dataclean(comboACT.list[[2]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


comboACT.svyd10 <- svydesign.fun(comboACT.list[[2]])


comboACT_pre_10 <- result.fun('comboACT', 'sstate','num_p', design=comboACT.svyd10)
head(comboACT_pre_10)

iLabels_10 <- val_labels(comboACT.list[[2]]$sstate)

match.idx_10 <- match(comboACT_pre_10$sstate, iLabels_10)

comboACT_pre_10$ADM1_NAME <- ifelse(is.na(match.idx_10),
                                    comboACT_pre_10$ADM1_NAME,
                                    names(iLabels_10)[match.idx_10])

comboACT_pre_10$ADM1_NAME <- str_to_title(comboACT_pre_10$ADM1_NAME)
head(comboACT_pre_10)

comboACT_pre_10<- comboACT_pre_10%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja"))

write.csv(comboACT_pre_10, "results/comboACT_pre_10.csv")




# 2008

comboACT.list[[1]]<-dataclean(comboACT.list[[1]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


comboACT.svyd08 <- svydesign.fun(comboACT.list[[1]])


comboACT_pre_08 <- result.fun('comboACT', 'sstate','num_p', design=comboACT.svyd08)
head(comboACT_pre_08)

iLabels_08 <- val_labels(comboACT.list[[1]]$sstate)

match.idx_08 <- match(comboACT_pre_08$sstate, iLabels_08)

comboACT_pre_08$ADM1_NAME <- ifelse(is.na(match.idx_08),
                                    comboACT_pre_08$ADM1_NAME,
                                    names(iLabels_08)[match.idx_08])

comboACT_pre_08$ADM1_NAME <- str_to_title(comboACT_pre_08$ADM1_NAME)
head(comboACT_pre_08)

comboACT_pre_08 <- comboACT_pre_08%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Abuja" = "Fct Abuja"))

write.csv(comboACT_pre_08, "results/comboACT_pre_08.csv")


# Maps 

# 2018 transformations 
S_file <- admin1shp_sf %>%left_join(comboACT_pre_18)

# 2015 transformations 
S_file_15 <- admin1shp_sf %>%left_join(comboACT_pre_15)

# 2013 transformations 
S_file_13 <- admin1shp_sf %>%left_join(comboACT_pre_13)

# 2010 transformations 
S_file_10 <- admin1shp_sf %>%left_join(comboACT_pre_10)

# 2008 transformations 
S_file_08 <- admin1shp_sf %>%left_join(comboACT_pre_08)


comboACT_18 <- tmap.fun4(S_file, "U5 ACT use in Nigerian States (2018)", "Prevalence", "comboACT")

comboACT_15 <- tmap.fun4(S_file_15, "U5 ACT use in Nigerian States (2015)", "Prevalence", "comboACT")

comboACT_13 <- tmap.fun4(S_file_13, "U5 ACT use in Nigerian States (2013)", "Prevalence", "comboACT")

comboACT_10 <- tmap.fun4(S_file_10, "U5 ACT use in Nigerian States (2010)", "Prevalence", "comboACT")

comboACT_08 <- tmap.fun4(S_file_08, "U5 ACT use in Nigerian States (2008)", "Prevalence", "comboACT")


all_comboACT <- tmap_arrange(comboACT_08,comboACT_10,comboACT_13,comboACT_15,comboACT_18)

tmap_save(tm = all_comboACT, filename = "results/U5_comboACT.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
