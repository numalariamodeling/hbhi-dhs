
# creating a variable for medical treatment for fever 
match("h32a",names(NGAfiles[[20]]))

NGAfiles[[2]]$med_fever <- rowSums(NGAfiles[[2]][, c(476:493)], na.rm=T) # 1990 
table(NGAfiles[[2]]$med_fever)


NGAfiles[[5]]$med_fever <- rowSums(NGAfiles[[5]][, c(693:710)], na.rm=T) # 2003
NGAfiles[[5]][,"med_fever"] <-recoder(NGAfiles[[5]][,"med_fever"]) 
table(NGAfiles[[5]]$med_fever)


NGAfiles[[8]]$med_fever <- rowSums(NGAfiles[[8]][, c(737:750, 752:754)], na.rm=T) # 2008
NGAfiles[[8]][,"med_fever"] <-recoder(NGAfiles[[8]][,"med_fever"]) 
table(NGAfiles[[8]]$med_fever)


NGAfiles[[11]]$med_fever <- rowSums(NGAfiles[[11]][, c(151:162, 164:168)], na.rm=T) # 2010
NGAfiles[[11]][,"med_fever"] <-recoder(NGAfiles[[11]][,"med_fever"]) 
table(NGAfiles[[11]]$med_fever)


NGAfiles[[14]]$med_fever <- rowSums(NGAfiles[[14]][, c(749:760, 762:766)], na.rm=T) # 2013
NGAfiles[[14]][,"med_fever"] <-recoder(NGAfiles[[14]][,"med_fever"]) 
table(NGAfiles[[14]]$med_fever)


NGAfiles[[17]]$med_fever <- rowSums(NGAfiles[[17]][, c(563:574, 576:580)], na.rm=T) # 2015
NGAfiles[[17]][,"med_fever"] <-recoder(NGAfiles[[17]][,"med_fever"]) 
table(NGAfiles[[17]]$med_fever)


NGAfiles[[20]]$med_fever <- rowSums(NGAfiles[[20]][, c(897:908, 910, 912:913)], na.rm=T) # 2018
NGAfiles[[20]][,"med_fever"] <-recoder(NGAfiles[[20]][,"med_fever"]) 
table(NGAfiles[[20]]$med_fever)

# U5 medical treatment for fever list for 1990, 2003, 2008, 2010, 2013, 2015, 2018
medfever.list <- list(NGAfiles[[2]], NGAfiles[[5]], NGAfiles[[8]], NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]], NGAfiles[[20]])
key.medfever<-list(key_list[[1]], key_list[[2]], key_list[[3]], key_list[[4]], key_list[[5]], key_list[[6]], key_list[[7]])

# key datasets and dhs/mis datasets are joined  

medfever.list <-map2(medfever.list,key.medfever, left_join) #medfever datasets 



#########################################################################################################################
# Cleaning and exporting to SAS 
#########################################################################################################################
# state level estimates 

# 2018

medfever.list[[7]]<-dataclean(medfever.list[[7]], med_fever, v005, 'med_fever', 'med_fever')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


medfev.svyd18 <- svydesign.fun(medfever.list[[7]])


medfev_pre_18 <- result.fun('med_fever', 'sstate','num_p', design=medfev.svyd18)
head(medfev_pre_18)

iLabels_18 <- val_labels(medfever.list[[7]]$sstate)

match.idx_18 <- match(medfev_pre_18$sstate, iLabels_18)

medfev_pre_18$ADM1_NAME <- ifelse(is.na(match.idx_18),
                                  medfev_pre_18$ADM1_NAME,
                               names(iLabels_18)[match.idx_18])

medfev_pre_18$ADM1_NAME <- str_to_title(medfev_pre_18$ADM1_NAME)

# write.csv(medfev_pre_18, "results/medfev_pre_18.csv")



# 2015 

medfever.list[[6]]<-dataclean(medfever.list[[6]], med_fever, v005, 'med_fever', 'med_fever')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


medfev.svyd15 <- svydesign.fun(medfever.list[[6]])


medfev_pre_15 <- result.fun('med_fever', 'sstate','num_p', design=medfev.svyd15)
head(medfev_pre_15)

iLabels_15 <- val_labels(medfever.list[[6]]$sstate)

match.idx_15 <- match(medfev_pre_15$sstate, iLabels_15)

medfev_pre_15$ADM1_NAME <- ifelse(is.na(match.idx_15),
                                  medfev_pre_15$ADM1_NAME,
                                  names(iLabels_15)[match.idx_15])

medfev_pre_15$ADM1_NAME <- str_to_title(medfev_pre_15$ADM1_NAME)
head(medfev_pre_15)

# write.csv(medfev_pre_15, "results/medfev_pre_15.csv")





# 2013 

medfever.list[[5]] <-dataclean(medfever.list[[5]], med_fever, v005, 'med_fever', 'med_fever') 
# write.foreign(medfever.list[[5]], "mydata_13.txt", "med_fever_13.sas",   package="SAS")

medfev.svyd13 <- svydesign.fun(medfever.list[[5]])


medfev_pre_13 <- result.fun('med_fever', 'sstate','num_p', design=medfev.svyd13)
head(medfev_pre_13)

iLabels_13 <- val_labels(medfever.list[[5]]$sstate)

match.idx_13 <- match(medfev_pre_13$sstate, iLabels_13)

medfev_pre_13$ADM1_NAME <- ifelse(is.na(match.idx_13),
                                  medfev_pre_13$ADM1_NAME,
                                  names(iLabels_13)[match.idx_13])

medfev_pre_13$ADM1_NAME <- str_to_title(medfev_pre_13$ADM1_NAME)

medfev_pre_13 <- medfev_pre_13%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja"))

head(medfev_pre_13)



# 2010 

medfever.list[[4]] <-dataclean(medfever.list[[4]], med_fever, v005, 'med_fever', 'med_fever') 
# write.foreign(medfever.list[[4]], "mydata_10.txt", "med_fever_10.sas",   package="SAS")

medfev.svyd10 <- svydesign.fun(medfever.list[[4]])


medfev_pre_10 <- result.fun('med_fever', 'sstate','num_p', design=medfev.svyd10)
head(medfev_pre_10)

iLabels_10 <- val_labels(medfever.list[[4]]$sstate)

match.idx_10 <- match(medfev_pre_10$sstate, iLabels_10)

medfev_pre_10$ADM1_NAME <- ifelse(is.na(match.idx_10),
                                  medfev_pre_10$ADM1_NAME,
                                  names(iLabels_10)[match.idx_10])

medfev_pre_10$ADM1_NAME <- str_to_title(medfev_pre_10$ADM1_NAME) 

medfev_pre_10 <- medfev_pre_10%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja"))
                                                                   
head(medfev_pre_10)



# 2008

medfever.list[[3]] <-dataclean(medfever.list[[3]], med_fever, v005, 'med_fever', 'med_fever') 

# write.foreign(medfever.list[[3]], "mydata_08.txt", "med_fever_08.sas",   package="SAS")

medfev.svyd08 <- svydesign.fun(medfever.list[[3]])


medfev_pre_08 <- result.fun('med_fever', 'sstate','num_p', design=medfev.svyd08)
head(medfev_pre_08)

iLabels_08 <- val_labels(medfever.list[[3]]$sstate)

match.idx_08 <- match(medfev_pre_08$sstate, iLabels_08)

medfev_pre_08$ADM1_NAME <- ifelse(is.na(match.idx_08),
                                  medfev_pre_08$ADM1_NAME,
                                  names(iLabels_08)[match.idx_08])

medfev_pre_08$ADM1_NAME <- str_to_title(medfev_pre_08$ADM1_NAME)

medfev_pre_08 <- medfev_pre_08%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Abuja" = "Fct Abuja"))

head(medfev_pre_08)



# 2003

medfever.list[[2]] <-dataclean(medfever.list[[2]], med_fever, v005, 'med_fever', 'med_fever') 
# write.foreign(medfever.list[[2]], "mydata_03.txt", "med_fever_03.sas",   package="SAS")

medfev.svyd03 <- svydesign.fun(medfever.list[[2]])


medfev_pre_03 <- result.fun('med_fever', 'sstate','num_p', design=medfev.svyd03)
head(medfev_pre_03)

iLabels_03 <- val_labels(medfever.list[[2]]$sstate)


match.idx_03 <- match(medfev_pre_03$sstate, iLabels_03)

medfev_pre_03$ADM1_NAME <- ifelse(is.na(match.idx_03),
                                  medfev_pre_08$ADM1_NAME,
                                  names(iLabels_03)[match.idx_03])

medfev_pre_03$ADM1_NAME <- str_to_title(medfev_pre_03$ADM1_NAME)

medfev_pre_03 <- medfev_pre_03%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Nassarawa" = "Nasarawa",
                  "Zamfora" = "Zamfara", "Abuja (Fct)" = "Fct Abuja"))

head(medfev_pre_03)


# 1990

medfever.list[[1]] <-dataclean(medfever.list[[1]], med_fever, v005, 'med_fever', 'med_fever') 
# write.foreign(medfever.list[[1]], "mydata_90.txt", "med_fever_90.sas",   package="SAS")

medfev.svyd90 <- svydesign.fun(medfever.list[[1]])


medfev_pre_90 <- result.fun('med_fever', 'sstate','num_p', design=medfev.svyd90)
head(medfev_pre_90)

iLabels_90 <- val_labels(medfever.list[[1]]$sstate)

match.idx_90 <- match(medfev_pre_90$sstate, iLabels_90)

medfev_pre_90$ADM1_NAME <- ifelse(is.na(match.idx_90),
                                  medfev_pre_90$ADM1_NAME,
                                  names(iLabels_90)[match.idx_90])

medfev_pre_90$ADM1_NAME <- str_to_title(medfev_pre_90$ADM1_NAME)
head(medfev_pre_90)



# # cluster-level estimates 

# 2018
clu_fever_18 <- result.clu.fun('med_fever', 'v001', design=medfev.svyd18, medfever.list[[7]])
head(clu_fever_18)
# 
# 2015
clu_fever_15 <- result.clu.fun('med_fever', 'v001', design=medfev.svyd15, medfever.list[[6]])
head(clu_fever_15)
# 
# write.csv(clu_fever_15, "clu_fever_15.csv")
# 
# 
# 2013
clu_fever_13 <- result.clu.fun('med_fever', 'v001', design=medfev.svyd13, medfever.list[[5]])
head(clu_fever_13)
# 
# write.csv(clu_fever_13, "clu_fever_13.csv")
# 
# 
# 2010
clu_fever_10 <- result.clu.fun('med_fever', 'v001', design=medfev.svyd10, medfever.list[[4]])
head(clu_fever_10)
# 
# write.csv(clu_fever_10, "clu_fever_10.csv")
# 
# 
# 2008
clu_fever_08 <- result.clu.fun('med_fever', 'v001', design=medfev.svyd08, medfever.list[[3]])
head(clu_fever_08)
# 
# write.csv(clu_fever_08, "clu_fever_08.csv")
# 
# 
# 2003
clu_fever_03 <- result.clu.fun('med_fever', 'v001', design=medfev.svyd03, medfever.list[[2]])
head(clu_fever_03)
# 
# write.csv(clu_fever_03, "clu_fever_03.csv")
# 
# 
# 
# 1990
clu_fever_90 <- result.clu.fun('med_fever', 'v001', design=medfev.svyd90, medfever.list[[1]])
head(clu_fever_90)
# 
# write.csv(clu_fever_90, "clu_fever_90.csv")
# 


# #####################################################################################################
# # cleaning SAS output to create form for EMOD 
# #####################################################################################################
# 
# 
# #2008 U5
# 
# medfever_08 <- read.csv('bin/med_fever_U5_2008.csv')
# 
# medfever_08_U5 <- medfever_08 %>% fill(sstate) %>% dplyr::filter(med_fever == 1)
# 
# iLabels <- val_labels(medfever.list[[3]]$sstate)
# 
# match.idx <- match(medfever_08_U5$sstate, iLabels)
# 
# medfever_08_U5$State <- ifelse(is.na(match.idx),
#                               medfever_08_U5$State,
#                               names(iLabels)[match.idx])
# 
# medfever_08_U5$State <- str_to_title(medfever_08_U5$State)
# 
# 
# rep_DS_S <- rep_DS%>%left_join(LGAshp_sf)
# 
# rep_DS_S <- rep_DS_S %>% left_join(medfever_08_U5)%>%dplyr::select(LGA, State, Weighted.Frequency, Row.Percent, LCL,UCL)
# 
# 
# write.csv(rep_DS_S, "results/arch_med_U5_08.csv")
# 
# 
# 
# 
# #2010 U5
# 
# medfever_10 <- read.csv('bin/med_fever_2010.csv')
# 
# medfever_10_E <- medfever_10 %>% fill(sstate) %>% dplyr::filter(med_fever == 1)
# 
# iLabels <- val_labels(medfever.list[[4]]$sstate)
# 
# match.idx <- match(medfever_10_E$sstate, iLabels)
# 
# medfever_10_E$State <- ifelse(is.na(match.idx),
#                             medfever_10_E$State,
#                             names(iLabels)[match.idx])
# 
# medfever_10_E$State <- str_to_title(medfever_10_E$State)
# 
# 
# rep_DS_S <- rep_DS%>%left_join(LGAshp_sf)
# 
# rep_DS_S <- rep_DS_S %>% left_join(medfever_10_E)%>%dplyr::select(LGA, State, Weighted.Frequency, Row.Percent, X80..LCL,X80..UCL)
# 
# 
# write.csv(rep_DS_S, "results/arch_med_10.csv")
# 
# 
# #2013 U5
# 
# medfever_13 <- read.csv('bin/med_fever_U5_2013.csv')
# 
# medfever_13_E <- medfever_13 %>% fill(sstate) %>% dplyr::filter(med_fever == 1)
# 
# iLabels <- val_labels(medfever.list[[5]]$sstate)
# 
# match.idx <- match(medfever_13_E$sstate, iLabels)
# 
# medfever_13_E$State <- ifelse(is.na(match.idx),
#                               medfever_13_E$State,
#                               names(iLabels)[match.idx])
# 
# medfever_13_E$State <- str_to_lower(medfever_13_E$State)
# 
# medfever_13_E$State <- str_to_title(medfever_13_E$State)
# 
# 
# rep_DS_S <- rep_DS%>%left_join(LGAshp_sf)
# 
# rep_DS_S <- rep_DS_S %>% left_join(medfever_13_E)%>%dplyr::select(LGA, State, Weighted.Frequency, Row.Percent, X80..LCL,X80..UCL)
# 
# 
# write.csv(rep_DS_S, "results/arch_med_U5_13.csv")
# 
# 
# 
# #2015 U5
# 
# medfever_15_U5 <- read.csv('bin/med_fever_U5_2015.csv', na.strings=c(""," ","NA"))
# 
# medfever_15_U5 <- medfever_15_U5 %>% fill(sstate) %>% dplyr::filter(med_fever == 1)
# 
# iLabels <- val_labels(medfever.list[[6]]$sstate)
# 
# match.idx <- match(medfever_15_U5$sstate, iLabels)
# 
# medfever_15_U5$State <- ifelse(is.na(match.idx),
#                               medfever_15_U5$State,
#                               names(iLabels)[match.idx])
# 
# medfever_15_U5$State <- str_to_title(medfever_15_U5$State)
# 
# 
# rep_DS_S <- rep_DS%>%left_join(LGAshp_sf)
# 
# rep_DS_S <- rep_DS_S %>% left_join(medfever_15_U5)%>%dplyr::select(LGA, State, Weighted.Frequency, Row.Percent, LCL,UCL)
# 
# 
# write.csv(rep_DS_S, "results/arch_med_15_U5.csv")


#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
S_file <- admin1shp_sf%>%left_join(medfev_pre_18)
          
pts_file <- st_as_sf(NGAshplist[[7]])   

pts_file <- pts_file %>% left_join(clu_fever_18)

# 2015 transformations 
S_file_15 <- admin1shp_sf %>% left_join(medfev_pre_15)

pts_file_15 <- st_as_sf(NGAshplist[[6]])  

pts_file_15 <- pts_file_15 %>% left_join(clu_fever_15)

# 2013 transformations 
S_file_13 <- admin1shp_sf %>% left_join(medfev_pre_13)

pts_file_13 <- st_as_sf(NGAshplist[[5]])  

pts_file_13 <- pts_file_13 %>% left_join(clu_fever_13)

# 2010 transformations 
S_file_10 <- admin1shp_sf %>% left_join(medfev_pre_10)

pts_file_10 <- st_as_sf(NGAshplist[[4]]) 

pts_file_10 <- pts_file_10 %>% left_join(clu_fever_10)


# 2008 transformations 
S_file_08 <- admin1shp_sf %>% left_join(medfev_pre_08)

pts_file_08 <- st_as_sf(NGAshplist[[3]]) 

pts_file_08 <- pts_file_08 %>% left_join(clu_fever_08)


# 2003 transformations 
S_file_03 <- admin1shp_sf %>% left_join(medfev_pre_03)

pts_file_03 <- st_as_sf(NGAshplist[[2]]) 

pts_file_03 <- pts_file_03 %>% left_join(clu_fever_03)


# 1990 transformations 
S_file_90 <- admin1shp_sf %>% left_join(medfev_pre_90)

pts_file_90 <- st_as_sf(NGAshplist[[1]]) 

pts_file_90 <- pts_file_90 %>% left_join(clu_fever_90)


# 2018 map 
nga_medfev18 <- tmap.fun3(S_file, "med_fever", "U5 prevalence",
              "Receipt of Medical Treatment for Fever in Nigerian States (2018)",pts_file, 
              "Number of Participants", "med_fever")

nga_medfev18_v1  <- tmap.fun4(S_file, "Receipt of Medical Treatment for Fever in Nigerian States (2018)", 
                              "Prevalence", "med_fever")

# 2015 map 
nga_medfev15 <- tmap.fun3(S_file_15, "med_fever", "U5 prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (2015)",pts_file_15, 
                          "Number of Participants", "med_fever")

# 2013 map 
nga_medfev13 <- tmap.fun3(S_file_13, "med_fever", "U5 prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (2013)",pts_file_13, 
                          "Number of Participants", "med_fever")


# 2010 map 
nga_medfev10 <- tmap.fun3(S_file_10, "med_fever", "U5 prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (2010)",pts_file_10, 
                          "Number of Participants", "med_fever")

# 2008 map 
nga_medfev08 <- tmap.fun3(S_file_08, "med_fever", "U5 prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (2008)",pts_file_08, 
                          "Number of Participants", "med_fever")

# 2003 map 
nga_medfev03 <- tmap.fun3(S_file_03, "med_fever", "U5 prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (2003)",pts_file_03, 
                          "Number of Participants", "med_fever")

# 1990 map 
nga_medfev90 <- tmap.fun3(S_file_90, "med_fever", "U5 prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (1990)",pts_file_90, 
                          "Number of Participants", "med_fever")

all_medfever <- tmap_arrange(nga_medfev03,nga_medfev08,nga_medfev10,nga_medfev13,nga_medfev15,nga_medfev18)

tmap_save(tm = nga_medfev18_v1, filename = "results/U5_medfever_state_18.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
