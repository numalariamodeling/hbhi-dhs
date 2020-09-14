cm.ls <-read.files( ".*NGKR.*\\.DTA", DataDir, read_dta)
cm.ls <- cm.ls[-c(1, 2)]


# identifying the combo with ACT variable 
look_for(cm.ls[[4]], "fever")

table(cm.ls[[1]]$ml13e)

val_labels(NGAfiles[[8]]$b5)

# recoding variables 
cm.ls[[3]][,"ml13e"] <-recoder(cm.ls[[3]][,"ml13e"]) 
table(cm.ls[[3]]$ml13e)

cm.ls[[2]][,"ml13e"] <-recoder(cm.ls[[2]][,"ml13e"]) 
table(cm.ls[[2]]$ml13e)

# key datasets and dhs/mis datasets are joined  
NGAshplist<- NGAshplist[-c(1, 2)]
NGA_ID <- lapply(NGAshplist, "[[", "DHSCLUST")
key_list <- key_list[-c(1, 2)]
key_list <- Map(cbind, key_list, v001 = NGA_ID)
comboACT.list <- map2(cm.ls, key_list, left_join)
comboACT.list <- lapply(comboACT.list, subset, b5 == 1 & h22 == 1)



# attach representative DS 
rep_DS.ls <- list(rep_DS)

comboACT.list  <- map2(comboACT.list, rep_DS.ls, left_join) #PR datasets


# LGA level estimates 
table(comboACT.list[[5]]$v007)
# 2018

comboACT.list[[5]]<-dataclean(comboACT.list[[5]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")

comboACT.svyd18 <- svydesign.fun(comboACT.list[[5]])

# svymean(comboACT.list[[5]]$comboACT, comboACT.svyd18)
# 
# table(comboACT.list[[5]]$b19)
head(rep_DS)
#generate LGA estimates 
comboACT_pre_18_LGA <- result.fun('comboACT', 'repDS',design=comboACT.svyd18, data =comboACT.list[[5]])
head(comboACT_pre_18_LGA)

# comboACT_pre_18_LGA_v2 <- comboACT_pre_18_LGA %>% 
#   dplyr::select(State, v025, comboACT) %>% 
#   pivot_wider(names_from = v025, values_from = comboACT) 
# head(comboACT_pre_18_LGA_v2)
# 
# write.csv(comboACT_pre_18_LGA_v2, "results/urbanvsrural/2018_ACTstate.csv")



LGA_sf <- st_as_sf(LGAshp) %>%  as_tibble() %>% dplyr::select(LGA, State)
head(LGA_sf)

repDS_LGA<- rep_DS %>% left_join(LGA_sf)
head(repDS_LGA)

ACT_18_repDS <- repDS_LGA %>%  left_join(comboACT_pre_18_LGA)
head(ACT_18_repDS)

# #generate repDS estimates and change variable names 
# comboACT_pre_18_repDS <- result.fun('comboACT', 'repDS','num_p', design=comboACT.svyd18)
# head(comboACT_pre_18_repDS)
# 
# comboACT_pre_18_repDS <- comboACT_pre_18_repDS %>% dplyr::select(repDS, comboACT_repDS = comboACT,
#                          se_repDS = se, Num_repDS = `Number of Participants`)
# 
# head(comboACT_pre_18_repDS)
# 
# #Now combine LGA files with LGA level estimates with repDS estimates 
# comboACT_18_LGA_repDS <- comboACT_pre_18_LGA %>% left_join(comboACT_pre_18_repDS) %>% 
#   mutate(comboACT = ifelse(comboACT =="0"| comboACT == "1" |is.na(comboACT), 
#                            comboACT_repDS, comboACT),
#          ci_l = comboACT - (1.96 * se_repDS), ci_u = comboACT + (1.96 * se_repDS)) %>%  
#   mutate(ci_l = ifelse(ci_l < 0, 0, ci_l))
# 
# head(comboACT_18_LGA_repDS)




# iLabels_18 <- val_labels(comboACT.list[[5]]$sstate)
# 
# match.idx_18 <- match(comboACT_pre_18$sstate, iLabels_18)
# 
# comboACT_pre_18$ADM1_NAME <- ifelse(is.na(match.idx_18),
#                                     comboACT_pre_18$ADM1_NAME,
#                                   names(iLabels_18)[match.idx_18])
# 
# comboACT_pre_18$ADM1_NAME <- str_to_title(comboACT_pre_18$ADM1_NAME)
# head(comboACT_pre_18)

write.csv(comboACT_18_LGA_repDS, "results/archetype_sim_input/Intervention_files_LGA/case_management/comboACT_18_LGA_repDS_v3.csv")



# 2015

comboACT.list[[4]]<-dataclean(comboACT.list[[4]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")

summary(comboACT.list[[4]]$LGA)

comboACT.svyd15 <- svydesign.fun(comboACT.list[[4]])

#generate LGA estimates 
comboACT_pre_15_LGA <- result.fun('comboACT', 'LGA','num_p', design=comboACT.svyd15)
head(comboACT_pre_15_LGA)

# comboACT_pre_15_LGA_v2 <- comboACT_pre_15_LGA %>% 
#   dplyr::select(State, v025, comboACT) %>% 
#   pivot_wider(names_from = v025, values_from = comboACT) 
# head(comboACT_pre_15_LGA_v2)
# 
# write.csv(comboACT_pre_15_LGA_v2, "results/urbanvsrural/2015_ACTstate.csv")


#next join each LGA to their repDS 

comboACT_pre_15_LGA <- rep_DS %>% left_join(comboACT_pre_15_LGA)
head(comboACT_pre_15_LGA)

#generate repDS estimates and change variable names 
comboACT_pre_15_repDS <- result.fun('comboACT', 'repDS','num_p', design=comboACT.svyd15)
head(comboACT_pre_15_repDS)

comboACT_pre_15_repDS <- comboACT_pre_15_repDS %>% dplyr::select(repDS, comboACT_repDS = comboACT,
                                                                 se_repDS = se, Num_repDS = `Number of Participants`)

head(comboACT_pre_15_repDS)

#Now combine LGA files with LGA level estimates with repDS estimates 
comboACT_15_LGA_repDS <- comboACT_pre_15_LGA %>% left_join(comboACT_pre_15_repDS) %>% 
  mutate(comboACT = ifelse(comboACT =="0"| comboACT == "1" |is.na(comboACT), 
                                  comboACT_repDS, comboACT),
                ci_l = comboACT - (1.96 * se_repDS), ci_u = comboACT + (1.96 * se_repDS)) %>%  
           mutate(ci_l = ifelse(ci_l < 0, 0, ci_l))

head(comboACT_15_LGA_repDS)




# iLabels_15 <- val_labels(comboACT.list[[4]]$sstate)
# 
# match.idx_15 <- match(comboACT_pre_15$sstate, iLabels_15)
# 
# comboACT_pre_15$ADM1_NAME <- ifelse(is.na(match.idx_15),
#                                     comboACT_pre_15$ADM1_NAME,
#                                     names(iLabels_15)[match.idx_15])
# 
# comboACT_pre_15$ADM1_NAME <- str_to_title(comboACT_pre_15$ADM1_NAME)
# head(comboACT_pre_15)

write.csv(comboACT_15_LGA_repDS, "results/archetype_sim_input/Intervention_files_LGA/case_management/comboACT_15_LGA_repDS_v2.csv")



# 2013

comboACT.list[[3]]<-dataclean(comboACT.list[[3]], ml13e, v005, 'ml13e', 'comboACT')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


comboACT.svyd13 <- svydesign.fun(comboACT.list[[3]])


#generate LGA estimates 
comboACT_pre_13_LGA <- result.fun('comboACT', 'LGA','num_p', design=comboACT.svyd13)
head(comboACT_pre_13_LGA)


# comboACT_pre_13_LGA_v2 <- comboACT_pre_13_LGA %>% 
#   dplyr::select(State, v025, comboACT) %>% 
#   pivot_wider(names_from = v025, values_from = comboACT) 
# head(comboACT_pre_13_LGA_v2)
# 
# write.csv(comboACT_pre_13_LGA_v2, "results/urbanvsrural/2013_ACTstate.csv")


#next join each LGA to their repDS 

comboACT_pre_13_LGA <- rep_DS %>% left_join(comboACT_pre_13_LGA)
head(comboACT_pre_13_LGA)

#generate repDS estimates and change variable names 
comboACT_pre_13_repDS <- result.fun('comboACT', 'repDS','num_p', design=comboACT.svyd13)
head(comboACT_pre_13_repDS)

comboACT_pre_13_repDS <- comboACT_pre_13_repDS %>% dplyr::select(repDS, comboACT_repDS = comboACT,
                                                                se_repDS = se, Num_repDS = `Number of Participants`)

head(comboACT_pre_13_repDS)

#Now combine LGA files with LGA level estimates with repDS estimates 
comboACT_13_LGA_repDS <- comboACT_pre_13_LGA %>% left_join(comboACT_pre_13_repDS) %>% 
  mutate(comboACT = ifelse(comboACT =="0"| comboACT == "1" |is.na(comboACT), 
                                                            comboACT_repDS, comboACT),
         ci_l = comboACT - (1.96 * se_repDS), ci_u = comboACT + (1.96 * se_repDS)) %>%  
        mutate(ci_l = ifelse(ci_l < 0, 0, ci_l))

head(comboACT_13_LGA_repDS)


# iLabels_13 <- val_labels(comboACT.list[[3]]$sstate)
# 
# match.idx_13 <- match(comboACT_pre_13$sstate, iLabels_13)
# 
# comboACT_pre_13$ADM1_NAME <- ifelse(is.na(match.idx_13),
#                                     comboACT_pre_13$ADM1_NAME,
#                                     names(iLabels_13)[match.idx_13])
# 
# comboACT_pre_13$ADM1_NAME <- str_to_title(comboACT_pre_13$ADM1_NAME)
# head(comboACT_pre_13)
# 
# comboACT_pre_13 <- comboACT_pre_13%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja"))

write.csv(comboACT_13_LGA_repDS, "results/archetype_sim_input/Intervention_files_LGA/case_management/comboACT_13_LGA_repDS_v2.csv")



# 2010

comboACT.list[[2]]<-dataclean(comboACT.list[[2]], ml13e, v005, 'ml13e', 'comboACT') 
# write.csv(comboACT.list[[2]], "bin/U5_ACT_use/2010_U5_ACT.csv")
# 
# table(comboACT.list[[2]]$comboACT)

comboACT.svyd10 <- svydesign.fun(comboACT.list[[2]])

#svymean(~comboACT,comboACT.svyd10)

comboACT_pre_10 <- result.fun('comboACT', 'LGA','num_p', design=comboACT.svyd10)
head(comboACT_pre_10)


# comboACT_pre_10_v2 <- comboACT_pre_10  %>% 
#   dplyr::select(State, v025, comboACT) %>% 
#   pivot_wider(names_from = v025, values_from = comboACT) 
# head(comboACT_pre_10_v2)
# 
# write.csv(comboACT_pre_10_v2, "results/urbanvsrural/2010_ACTstate.csv")


comboACT_pre_10 <- comboACT_pre_10 %>% left_join(rep_LGA_state)

#repDS estimates

comboACT_pre_10_repDS <- result.fun('comboACT', 'repDS','num_p', design=comboACT.svyd10)
head(comboACT_pre_10_repDS)

rep_DS_LGA_10 <- left_join(rep_DS, comboACT_pre_10_repDS)

#ACT_U5 <- read.csv('bin/ACT_fever_U5.csv', na.strings=c(""," ","NA"))
 
#ACT_U5<- ACT_U5 %>% fill(repDS) %>% dplyr::filter(comboACT == 1)

# rep_LGA_v2 <- rep_LGA %>%left_join(LGAshp_sf) %>% dplyr::select(LGA, State)
# head(rep_LGA_v2)
# 
# arch_med_fever_10 <- rep_LGA_v2 %>% left_join(ACT_U5)
# head(arch_med_fever_10)


write.csv(comboACT_pre_10, "results/U5_ACT_arch_med_fever_10_v2.csv")




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


#cluster 

clu_ACT_18 <- result.clu.fun('comboACT', 'v001', design=comboACT.svyd18,comboACT.list[[5]], "v007")
head(clu_ACT_18)


clu_ACT_15 <- result.clu.fun('comboACT', 'v001', design=comboACT.svyd15,comboACT.list[[4]], "v007")
head(clu_ACT_15)

clu_ACT_13 <- result.clu.fun('comboACT', 'v001', design=comboACT.svyd13,comboACT.list[[3]], "v007")
head(clu_ACT_13)

clu_ACT_10 <- result.clu.fun('comboACT', 'v001', design=comboACT.svyd10,comboACT.list[[2]], "v007")
head(clu_ACT_10)

# Maps 

# 2018 transformations 
colnames(admin1_sf)[3] <- "State" 
DS_file <- admin1_sf %>%left_join(comboACT_pre_18_LGA)

summary(comboACT_pre_18_LGA$comboACT)

pts_shp_18 <- st_as_sf(NGAshplist[[7]])


pts_file <- pts_shp_18 %>% left_join(clu_ACT_18) #%>% filter(URBAN_RURA =="U")
head(pts_file)
summary(is.na(pts_file$comboACT))
summary(pts_file$comboACT)
summary(pts_file$`Number of Participants`)
num <- na.omit(pts_file$`Number of Participants`)
sd(num)

# 2015 transformations 
DS_file_15 <- LGAshp_sf %>%left_join(comboACT_15_LGA_repDS)


pts_shp_15 <- st_as_sf(NGAshplist[[6]])

pts_file_15 <- pts_shp_15 %>% left_join(clu_ACT_15) #%>% filter(URBAN_RURA =="U")
head(pts_file_15)
summary(is.na(pts_file_15$comboACT))
summary(pts_file_15$comboACT)
summary(pts_file_15$`Number of Participants`)
num <- na.omit(pts_file_15$`Number of Participants`)
sd(num)

# 2013 transformations 
DS_file_13 <- LGAshp_sf %>%left_join(comboACT_13_LGA_repDS)

pts_shp_13 <- st_as_sf(NGAshplist[[5]])

pts_file_13 <- pts_shp_13 %>% left_join(clu_ACT_13) #%>% filter(URBAN_RURA =="U")
head(pts_file_15)
summary(is.na(pts_file_13$comboACT))
summary(pts_file_13$comboACT)
summary(pts_file_13$`Number of Participants`)
num <- na.omit(pts_file_13$`Number of Participants`)
sd(num)

# 2010 transformations 
DS_file_10 <- LGAshp_sf %>%left_join(rep_DS_LGA_10)

pts_shp_10 <- st_as_sf(NGAshplist[[4]])

pts_file_10 <- pts_shp_10 %>% left_join(clu_ACT_10) #%>% filter(URBAN_RURA =="U")
head(pts_file_10)
summary(is.na(pts_file_10$comboACT))
summary(pts_file_10$comboACT)
summary(pts_file_10$`Number of Participants`)
num <- na.omit(pts_file_10$`Number of Participants`)
sd(num)

# # 2008 transformations 
# S_file_08 <- admin1shp_sf %>%left_join(comboACT_pre_08)
# 
# 
comboACT_18 <- tmap.fun6(DS_file, "comboACT",  "Prevalence", "U5 ACT use in Nigerian States (2018)",
                         pts_file, "Number of Participants", "comboACT", "State")

comboACT_18 <- tmap.fun4(DS_file, "U5 ACT use in Nigerian States (2018)", "Prevalence", "comboACT")

# 
#comboACT_15 <- tmap.fun3(DS_file_15, "comboACT","Prevalence","U5 ACT use in Nigerian LGAs (2015)",pts_file_15,
                        # "Number of Participants", "comboACT")

comboACT_15 <- tmap.fun4(DS_file_15, "U5 ACT use in Nigerian States (2015)", "Prevalence", "comboACT")
# 
#comboACT_13 <- tmap.fun3(DS_file_13, "comboACT","Prevalence","U5 ACT use in Nigerian LGAs (2013)",pts_file_13,
                        # "Number of Participants", "comboACT")

comboACT_13 <- tmap.fun4(DS_file_13, "U5 ACT use in Nigerian States (2013)", "Prevalence", "comboACT")
# 
#comboACT_10 <- tmap.fun3(DS_file_10, "comboACT","Prevalence","U5 ACT use in Nigerian LGAs (2010)",pts_file_10,
                         #"Number of Participants", "comboACT")

comboACT_10 <- tmap.fun4(DS_file_10, "U5 ACT use in Nigerian States (2010)", "Prevalence", "comboACT")
# 
# comboACT_08 <- tmap.fun4(S_file_08, "U5 ACT use in Nigerian States (2008)", "Prevalence", "comboACT")
# 
# 
all_comboACT_LGA <- tmap_arrange(comboACT_10,comboACT_13,comboACT_15, comboACT_18)

tmap_save(tm = comboACT_18, filename = "results/State_maps/ACT_maps/comboACT_U5_maps.pdf",
           width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

#Maps 
u_clu_map <- tmap.clu(admin1_sf, ptsfile=pts_file, "Number of Participants", "comboACT", "2018 cluster ACT")

u_clu_map_15 <- tmap.clu(admin1_sf, ptsfile=pts_file_15, "Number of Participants", "comboACT", "2015 cluster ACT")

u_clu_map_13 <- tmap.clu(admin1_sf, ptsfile=pts_file_13, "Number of Participants", "comboACT", "2013 cluster ACT")

u_clu_map_10 <- tmap.clu(admin1_sf, ptsfile=pts_file_10, "Number of Participants", "comboACT", "2010 cluster ACT")


## creating a combined prevalence map for all years 
#row binding cluster points 
pts_merge <- rbind(pts_file,pts_file_15,pts_file_13,pts_file_10)

head(pts_merge)
summary(is.na(pts_merge$comboACT))
summary(pts_merge$comboACT)
summary(pts_merge$`Number of Participants`)
num <- na.omit(pts_merge$`Number of Participants`)
sd(num)



#aggregated plot on state shape file 
u_clu_map_bindrows <- tmap.clu(admin1_sf, ptsfile=pts_merge, "Number of Participants", "comboACT", "ACT 2010 - 2018")



#creates aggreagted map of pfpr for all years using mean values 

#calculate mean of points to pfpr estimate over time 
pts_merge_test <- pts_merge %>% drop_na(comboACT, `Number of Participants`)
summary(is.na(pts_merge_test$`Number of Participants`))

pts100 <- st_is_within_distance(pts_merge_test, dist = 2000)



res <- data.frame(id=1:length(pts100),x=NA, y=NA,mean_ACT=NA)
res$x <- sapply(pts100, function(p){mean(pts_merge_test$LONGNUM[p])})
res$y <- sapply(pts100, function(p){mean(pts_merge_test$LATNUM[p])})
res$mean_ACT<- sapply(pts100, function(p) #scaled by the proportion of participants in each nearby cluster 
{sum((pts_merge_test$`Number of Participants`[p]/sum(pts_merge_test$`Number of Participants`[p]) * pts_merge_test$comboACT[p]))})
res$`Number of Participants` <- sapply(pts100, function(p){mean(pts_merge_test$`Number of Participants`[p])}) 
res_2_ACT<- res %>% distinct(x, y, .keep_all = TRUE)

coordinates(res_2) <- c("x","y")

proj4string(res_2) <- CRS('+proj=longlat +datum=WGS84 +no_defs')


res_sf_ACT<- st_as_sf(res_2)
head(res_sf_ACT)


summary(is.na(res_sf_ACT$mean_ACT))
summary(res_sf_ACT$mean_ACT)
summary(res_sf_ACT$`Number of Participants`)
sd(res_sf_ACT$`Number of Participants`)


u_clu_map_aggregated <- tmap.clu4(admin1_sf, ptsfile=res_sf, "Number of Participants", "mean_ACT", "Aggregated ACT 2010 - 2018")

all_maps <- tmap_arrange(u_clu_map_10,u_clu_map_13,u_clu_map_15,u_clu_map,u_clu_map_bindrows,u_clu_map_aggregated)

tmap_save(tm = all_maps, filename = "results/urban_cluster/ACT/ACT_diff.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

