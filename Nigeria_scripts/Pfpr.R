#######################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################
#run seperately for RDT and micrscopy 
# creating list of files with Pfpr data for 2010, 2015 & 2018 
pfpr.list <- list(NGAfiles[[12]], NGAfiles[[18]], NGAfiles[[21]])


# recoding pfpr (microscopy data)

# 2018 
table(pfpr.list[[3]][,"hml32"])
table(pfpr.list[[3]][,"hml35"])

# 2015 
pfpr.list[[2]][,"hml32"] <-recoder.pfpr(pfpr.list[[2]][,"hml32"]) 
table(pfpr.list[[2]][,"hml32"])
table(pfpr.list[[2]][,"hml35"])

# 2010
pfpr.list[[1]][,"hml32"] <-recoder.pfpr(pfpr.list[[1]][,"hml32"])
table(pfpr.list[[1]][,"hml32"])
# pfpr.list[[1]][,"hml35"] <-recoder(pfpr.list[[1]][,"hml35"])
# table(pfpr.list[[1]][,"hml32"])
# dim(pfpr.list[[1]])
# val_labels(pfpr.list[[3]]$hml32)

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)
pfpr.list <- lapply(pfpr.list, subset, hv042 == "1" & hv103 == "1" & hml32 %in% c(0, 1) )

# key list for pfpr 
keyspfpr <- list(key_list[[4]], key_list[[6]], key_list[[7]]) #changing to a list of keys 


# applying function to create month and year of survey. Note this function only works for person and household recode file
pfpr.list <- map(pfpr.list, survey.month.fun)


# key datasets and dhs/mis datasets are joined  
pfpr.list <- map2(pfpr.list, keyspfpr, left_join) #PR datasets

rep_DS.ls <- list(rep_DS)

pfpr.list <- map2(pfpr.list, rep_DS.ls, left_join) #PR datasets


####################################################################################################
## Pfpr analysis
####################################################################################################

# DS-level estimates 

# recoding and renaming pfpr datasets 
# 2018 PR NGA DHS 

pfpr.list[[3]] <- dataclean.para(pfpr.list[[3]], hv005, hc1, hml32, 'hml32', 'p_test') 
pfpr.list[[3]] <- dataclean.para(pfpr.list[[3]], hv005, hc1, hml35, 'hml35', 'p_testRDT')

table(pfpr.list[[3]]$time2)

hml32.svyd18 <- svydesign.fun(pfpr.list[[3]])

#write.csv(pfpr.list[[3]], "bin/pfpr/pfpr2018.csv")
#write.csv(pfpr.list[[3]], "bin/pfpr/pfpr2018RDT.csv")


# 2018 estimation of pfpr by LGA replaced with state value 
#estimate state value 
S_merge_ym_18 <- result.fun('p_test', 'State + hv025', 'num_p', design=hml32.svyd18) #microscopy 
head(S_merge_ym_18)


S_merge_ym_18_v2 <- S_merge_ym_18  %>% 
  dplyr::select(State, hv025, p_test) %>% 
  pivot_wider(names_from = hv025, values_from = p_test) 
head(S_merge_ym_18_v2)

write.csv(S_merge_ym_18_v2, "results/urbanvsrural/2018_micropfpr_LGA.csv")

S_merge_ym_18 <- S_merge_ym_18 %>% dplyr::select(State, time2, PfPr_microscopy = PfPr, ci_l_micro = ci_l,
                                            ci_u_micro = ci_u, num_state_micro = `Number of Kids`)

head(S_merge_ym_18)
S_merge_ym_18$LGA <- gsub("\\/", "-", S_merge_ym_18$LGA)
write.csv(S_merge_ym_18, "results/pfpr_LGA/2018_micropfpr_LGA.csv")

#estimate LGA value 
DS_merge_ym_18 <- result.fun.para('p_test', 'LGA + time2', 'num_p', 'LGA', design=hml32.svyd18) #microscopy 
head(DS_merge_ym_18)

DS_merge_ym_18_RDT$LGA <- gsub("\\/", "-", DS_merge_ym_18_RDT$LGA)
write.csv(DS_merge_ym_18_RDT, "results/pfpr_LGA/2018_RDTpfpr_LGA.csv")


#join LGA estimates to LGA admin  and state estimates 
pfpr_LGA_state_val <- DS_merge_ym_18 %>% left_join(LGAshp_sf) %>% left_join(S_merge_ym_18) %>% 
                      dplyr::select(LGA, time2, PfPrRDT_state, ci_l_state, ci_u_state)

head(pfpr_LGA_state_val)

write.csv(pfpr_LGA_state_val, "results/pfpr_state/pfpr_RDT2018_LGA_state_val.csv")






# recoding and renaming pfpr datasets 
# 2015 NGA BF DHS 
pfpr.list[[2]] <- dataclean.para(pfpr.list[[2]], hv005, hc1, hml32, 'hml32', 'p_test') 
pfpr.list[[2]] <- dataclean.para(pfpr.list[[2]], hv005, hc1, hml35, 'hml35', 'p_testRDT')
hml32.svyd15 <- svydesign.fun(pfpr.list[[2]])

table(pfpr.list[[2]]$time2)

# 2015 

# 2015 estimation of pfpr by LGA replaced with state value 
# estimate state value 
S_merge_ym_15 <- result.fun('p_test', 'State + hv025', 'num_p', design=hml32.svyd15) #microscopy 
head(S_merge_ym_15)

S_merge_ym_15_v2 <- S_merge_ym_15  %>% 
  dplyr::select(State, hv025, p_test) %>% 
  pivot_wider(names_from = hv025, values_from = p_test) 
head(S_merge_ym_15_v2)

write.csv(S_merge_ym_15_v2, "results/urbanvsrural/2015_micropfpr_LGA.csv")



S_merge_ym_15 <- S_merge_ym_15 %>% dplyr::select(State, time2, PfPr_RDT = PfPr, ci_l_RDT = ci_l,
                                                 ci_u_RDT = ci_u, num_state_RDT = `Number of Kids`)

head(S_merge_ym_15)
write.csv(S_merge_ym_15, "results/pfpr_state/pfpr_state_v3/2015_RDTpfpr_state_time.csv")

# estimate LGA value
DS_merge_ym_15 <- result.fun.para('p_testRDT', 'LGA + time2', 'num_p', 'LGA', design=hml32.svyd15) #microscopy 
head(DS_merge_ym_15)

DS_merge_ym_15$LGA <- gsub("\\/", "-", DS_merge_ym_15$LGA)
write.csv(DS_merge_ym_15, "results/pfpr_LGA/2015_RDTpfpr_LGA.csv")


#join LGA estimates to LGA admin  and state estimates 
pfpr_LGA_state_val_15 <- DS_merge_ym_15 %>% left_join(LGAshp_sf) %>% left_join(S_merge_ym_15) %>% 
  dplyr::select(LGA, time2, PfPrRDT_state, ci_l_state, ci_u_state)

head(pfpr_LGA_state_val_15)

write.csv(pfpr_LGA_state_val_15, "results/pfpr_state/pfprRDT_2015_LGA_state_val.csv")


# recoding and renaming pfpr datasets 
# 2010 PR BF DHS 
pfpr.list[[1]] <- dataclean.para(pfpr.list[[1]], hv005, hc1, hml32, 'hml32', 'p_test') 
write.csv(pfpr.list[[1]], "bin/pfpr/pfpr2010.csv")


table(pfpr.list[[1]]$time2)

pfpr.list[[1]] <- dataclean.para(pfpr.list[[1]], hv005, hc1, hml35,'hml35', 'p_testRDT')

write.csv(pfpr.list[[1]], "bin/pfpr/pfpr2010RDT.csv")

hml32.svydesign <- svydesign.fun(pfpr.list[[1]])

# 2010 PR BF DHS
# 2010 estimation of pfpr by LGA replaced with state value 
# estimate state value 
S_merge_ym_15 <- result.fun('p_test', 'State + hv025', 'num_p', design=hml32.svyd15) #microscopy 
head(S_merge_ym_15)

S_merge_ym_15_v2 <- S_merge_ym_15  %>% 
  dplyr::select(State, hv025, p_test) %>% 
  pivot_wider(names_from = hv025, values_from = p_test) 
head(S_merge_ym_15_v2)

write.csv(S_merge_ym_15_v2, "results/urbanvsrural/2015_micropfpr_LGA.csv")



S_merge_ym_10 <- result.fun('p_test', 'State + hv025', 'num_p', design=hml32.svydesign) #microscopy 
head(S_merge_ym_10)


S_merge_ym_10_v2 <- S_merge_ym_10  %>% 
  dplyr::select(State, hv025, p_test) %>% 
  pivot_wider(names_from = hv025, values_from = p_test) 
head(S_merge_ym_10_v2)

write.csv(S_merge_ym_10_v2, "results/urbanvsrural/2010_micropfpr_LGA.csv")


S_merge_ym_10 <- S_merge_ym_10 %>% dplyr::select(State, time2, PfPr_RDT = PfPr, ci_l_RDT = ci_l,
                                                 ci_u_RDT = ci_u, num_state_RDT = `Number of Kids`)

head(S_merge_ym_10)
write.csv(S_merge_ym_10, "results/pfpr_state/pfpr_state_v3/2010_RDTpfpr_state_val_time.csv")

# estimate LGA value
DS_merge_ym_10 <- result.fun.para('p_testRDT', 'LGA + time2', 'num_p', 'LGA', design=hml32.svydesign) #microscopy 
head(DS_merge_ym_10)

DS_merge_ym_10$LGA <- gsub("\\/", "-", DS_merge_ym_10$LGA)
write.csv(DS_merge_ym_10, "results/pfpr_LGA/2010_RDTpfpr_LGA.csv")

#join LGA estimates to LGA admin  and state estimates 
pfpr_LGA_state_val_10 <- DS_merge_ym_10 %>% left_join(LGAshp_sf) %>% left_join(S_merge_ym_10) %>% 
  dplyr::select(LGA, time2, PfPr_state, ci_l_state, ci_u_state)

head(pfpr_LGA_state_val_10)

write.csv(pfpr_LGA_state_val_10, "results/pfpr_state/pfprRDT_2010_LGA_state_val.csv")





#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) 
write.csv(DS_merge_ym_18,file="results/DS18_pfpr_ym_micro.csv") # 2018 PfPr micro BF DHS
write.csv(DS_merge_ym_RDT18, file="results/DS18_pfpr_ym_RDT.csv") # 2018 PfPr PR RDT BF DHS 

#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) 
write.csv(DS_merge_ym_15,file="DS15_pfpr_ym_micro.csv") # 2015 PfPr micro BF DHS
write.csv(DS_merge_ym_RDT15, file="DS15_pfpr_ym_RDT.csv") # 2015 PfPr PR RDT BF DHS 

#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) for the DSs 
write.csv(DS_merge_ym, file="results/pfpr_repDS/DS10_pfpr_ym_micro.csv") # 2010 PfPr PR micro BF DHS 
write.csv(DS_merge_ym_RDT, file="results/pfpr_repDS/DS10_pfpr_ym_RDT.csv") # 2010 PfPr PR RDT BF DHS 




# cluster-level estimates

# 2018 PR BF DHS 

clu_ptest_num_18 <- result.clu.fun.para('p_test', 'v001+time2', 'num_p', 'v001',design=hml32.svyd18, pfpr.list[[3]]) #microscopy 
head(clu_ptest_num_18)

clu_ptest_num_18 <- result.clu.fun('p_test', 'v001', design=hml32.svyd18,pfpr.list[[3]], 'hv007')
head(clu_ptest_num_18)


clu_ptest_num_RDT18 <- result.clu.fun.para('p_testRDT', 'v001 + time2','num_p', 'v001', design=hml32.svyd18, pfpr.list[[3]]) #RDT 
head(clu_ptest_num_RDT18)




# 2015 PR BF DHS 

clu_ptest_num_15 <- result.clu.fun.para('p_test', 'v001+time2', 'num_p', 'v001', design=hml32.svyd15, pfpr.list[[2]]) #microscopy 
head(clu_ptest_num_15)

clu_ptest_num_15 <- result.clu.fun('p_test', 'v001', design=hml32.svyd15,pfpr.list[[2]], 'hv007')
head(clu_ptest_num_15)


clu_ptest_num_RDT15 <- result.clu.fun.para('p_testRDT', 'v001 + time2','num_p', 'v001', design=hml32.svyd15, pfpr.list[[2]]) #RDT 
head(clu_ptest_num_RDT15)


# 2010 PR BF DHS 
clu_ptest_num <- result.clu.fun('p_test', 'v001',design=hml32.svydesign, pfpr.list[[1]]) #microscopy 
head(clu_ptest_num)

clu_ptest_num_10 <- result.clu.fun('p_test', 'v001', design=hml32.svydesign,pfpr.list[[1]], 'hv007')
head(clu_ptest_num_10)


clu_ptest_num_RDT <- result.clu.fun.para('p_testRDT', 'v001 + time2','num_p', 'v001', design=hml32.svydesign, pfpr.list[[1]]) #RDT
head(clu_ptest_num_RDT)


#creating a csv file with both the PfPr and number of surveyed kids by timepoint (ymd) 
write.csv(clu_ptest_num, file="clu10_pfpr_ym_micro.csv") # 2010 PfPr PR micro BF DHS 
write.csv(clu_ptest_num_RDT, file="clu10_pfpr_ym_RDT.csv") # 2010 PfPr PR RDT BF DHS 


write.csv(clu_ptest_num_15,file="clu15_pfpr_ym_micro.csv") # 2015 PfPr micro BF DHS
write.csv(clu_ptest_num_RDT15, file="clu15_pfpr_ym_RDT15.csv") # 2015 PfPr PR RDT BF DHS  


write.csv(clu_ptest_num_18,file="results/clu15_pfpr_ym_micro.csv") # 2018 PfPr micro BF DHS
write.csv(clu_ptest_num_RDT18, file="results/clu15_pfpr_ym_RDT15.csv") # 2018 PfPr PR RDT BF DHS  

#######################################################################################################
# merging to identify DSs with no estimates 
#######################################################################################################

# set up an empty dataframe with the LGA names so LGA with no estimates can be identified 
DS_merge<-data.frame(LGA=LGAshp@data[,"LGA"])

# read in the MAP shapefiles and convert to sf object to merge  
MAPshp <- readOGR("bin/nigeria_MAP/District_Summary_Shapefiles", layer ="NGA_pfpr_maps_2-4", use_iconv=TRUE, encoding= "UTF-8")
MAPshp<-st_as_sf(MAPshp)

# 2018 PR BF DHS
DS_merge_noest18<-DS_merge%>%left_join(DS_merge_ym_18)%>%filter(is.na(p_test))
head(DS_merge_noest18)
colnames(DS_merge_noest18)[1] <- "District"

DS_merge_noest18 <- DS_merge_noest18%>%left_join(MAPshp)%>%dplyr::select(District, Region, map3_2018, map4_2018, map4_LCI18, map4_UCI18)


write.csv(DS_merge_noest18, file="results/DS18_noest.csv") # 2018 PfPr PR micro BF DHS
# 


# # 2015 PR BF DHS
# DS_merge_noest15<-DS_merge%>%left_join(ptest_15_no_time)%>%filter(is.na(p_test))
# head(DS_merge_noest15)
# colnames(DS_merge_noest15)[1] <- "District"
# 
# DS_merge_noest15 <- DS_merge_noest15%>%left_join(MAPshp)%>%dplyr::select(District, Region, map3_2015, map4_2015, map4_LCI15, map4_UCI15)
# 
# 
# write.csv(DS_merge_noest15, file="DS15_noest.csv") # 2015 PfPr PR micro BF DHS 
# 
# 
# 
# # 2010 PR BF DHS
# DS_merge_noest<-DS_merge%>%left_join(ptest_10_no_time)%>%filter(is.na(p_test))
# head(DS_merge_noest)
# colnames(DS_merge_noest)[1] <- "District"
# 
# DS_merge_noest <- DS_merge_noest%>%left_join(MAPshp)%>%dplyr::select(District, Region, map3_2010, map4_2010, map4_LCI10, map4_UCI10)




########################################################################################################
# Treating the DHS as a random sample to estimate Pfpr for Banfora (substitute 2010 and 2015 data)
########################################################################################################
dhs <- pfpr.list[[3]]%>% dplyr:: select(LGA, p_test, time2) %>% filter(!is.na(p_test))



dhs2<-na.omit(dhs)%>%
  group_by(LGA, time2)%>%
  summarise_each(funs(mean, sd, std.error, n()))

write.csv(dhs2, "results/SRS18_ptest_micro.csv")
#####################################################################################################



#####################################################################################################
## Maps 
####################################################################################################

 # 2018 transformations 
# DS_file <- LGAshp_sf %>% left_join(DS_merge_ym_18)

pts_shp_18 <- st_as_sf(NGAshplist[[7]])


pts_file <- pts_shp_18 %>% left_join(clu_ptest_num_18) %>% filter(URBAN_RURA =="U")
head(pts_file)

# num_part <- pts_file$`Number of Participants`
# num_part <- na.omit(num_part)
# sd(num_part)

# 2015 transformations 
# DS_file15 <- LGAshp_sf %>% left_join(DS_merge_ym_15)

pts_shp_15 <- st_as_sf(NGAshplist[[6]])
pts_file_15 <- pts_shp_15 %>% left_join(clu_ptest_num_15)%>%filter(URBAN_RURA =="U")
head(pts_file_15)

sd(pts_file_15$`Number of Participants`)

# 2010 transformations 
# DS_file10 <- LGAshp_sf %>% left_join(DS_merge_ym)
# DS_file10 <- LGAshp_sf %>% left_join(plot.file)

pts_shp_10 <- st_as_sf(NGAshplist[[4]])

pts_file_10 <- pts_shp_10 %>% left_join(clu_ptest_num_10)%>%filter(URBAN_RURA =="U")
head(pts_file_10)

sd(pts_file_10$`Number of Participants`)

# 2018 map 
nga_pfpr18 <- tmap.fun1(DS_file, DSmapvalue="p_test", adminlegtitle="Malaria prevalence", 
                        main_title="U5 Malaria prevalence by LGA (2018)", text_title = "LGA", 
                        ptsfile=pts_file, "Number of Participants", "p_test") 



u_clu_map <- tmap.clu3(admin1_sf, ptsfile=pts_file, "Number of Participants", "p_test", "2018 cluster PfPR Nigeria")

summary(pts_file$`Number of Participants`)

sum(!is.na(pts_file$p_test))
summary(pts_file$`Number of Participants`)
summary(pts_file$p_test)

#2015 map 
nga_pfpr15 <- tmap.fun1(DS_file15, DSmapvalue="p_test", adminlegtitle="Malaria prevalence", 
                        main_title="U5 Malaria prevalence by LGA (2015)", text_title = "LGA", 
                        ptsfile=pts_file_15, "Number of Participants", "p_test") 

u_clu_map_15 <- tmap.clu2(admin1_sf, ptsfile=pts_file_15, "Number of Participants", "p_test", "2015 cluster PfPR Nigeria")

sum(!is.na(pts_file_15$p_test))
summary(pts_file_15$`Number of Participants`)
summary(pts_file_15$p_test)

#2010 map 
nga_pfpr10 <- tmap.fun1(DS_file10, DSmapvalue="p_test", adminlegtitle="Malaria prevalence", 
                        main_title="U5 Malaria prevalence by LGA (2010)", text_title = "LGA", 
                        ptsfile=pts_file_10, "Number of Participants", "p_test") 

u_clu_map_10 <- tmap.clu2(admin1_sf, ptsfile=pts_file_10, "Number of Participants", "p_test", "2010 cluster PfPR Nigeria")

sum(!is.na(pts_file_10$p_test))
summary(pts_file_10$`Number of Participants`)
summary(pts_file_10$p_test)

# nga_PfPR10_rep <- tmap.fun2(DS_file10, "p_test", "PfPR", "U5 PfPR by LGA (2010)")



tmap_save(tm = nga_PfPR10_rep, filename = "results/para_2010_repDS.micro.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

## creating a combined prevalence map for all years 
#row binding cluster points 
pts_merge <- rbind(pts_file,pts_file_15, pts_file_10)


summary(!is.na(pts_merge_test$p_test))
summary(pts_merge$`Number of Participants`)
vec <- na.omit(pts_merge$`Number of Participants`)
sd(vec)

#now plot on state shape file 
u_clu_map_bindrows <- tmap.clu3(admin1_sf, ptsfile=pts_merge, "Number of Participants", "p_test", "PfPR 2010 - 2018 in Nigeria")

head(pts_merge)


#creates aggreagted map of pfpr for all years using mean values 

#calculate mean of points to pfpr estimate over time 
pts_merge_test <- pts_merge %>% drop_na(p_test)

pts100 <- st_is_within_distance(pts_merge_test, dist = 2000)
class(pts100)


res <- data.frame(id=1:length(pts100),x=NA, y=NA,mean_pfpr=NA)
res$x <- sapply(pts100, function(p){mean(pts_merge_test$LONGNUM[p])})
res$y <- sapply(pts100, function(p){mean(pts_merge_test$LATNUM[p])})
res$mean_pfpr<- sapply(pts100, function(p) #scaled by the proportion of participants in each nearby cluster 
  {sum((pts_merge_test$`Number of Participants`[p]/sum(pts_merge_test$`Number of Participants`[p]) * pts_merge_test$p_test[p]))})
res$`Number of Participants` <- sapply(pts100, function(p){mean(pts_merge_test$`Number of Participants`[p])}) 
res_2_pfpr<- res %>% distinct(x, y, .keep_all = TRUE) %>% rename(num_pfpr = `Number of Participants`)

coordinates(res_2) <- c("x","y")

proj4string(res_2) <- CRS('+proj=longlat +datum=WGS84 +no_defs')



res_sf_pfpr <- st_as_sf(res_2)
head(res_sf_pfpr)


summary(res_sf_pfpr$`Number of Participants`)
sum(!is.na(res_sf$mean_pfpr))

u_clu_map_aggregated <- tmap.clu2(admin1_sf, ptsfile=res_sf, "Number of Participants", "mean_pfpr", "Aggregated PfPR 2010 - 2018 in Nigeria")

all_maps <- tmap_arrange(u_clu_map_10,u_clu_map_15,u_clu_map,u_clu_map_bindrows,u_clu_map_aggregated)

tmap_save(tm = all_maps, filename = "results/urban_cluster/pfpr_diff.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

res_2_ACT <- res_2_ACT %>% rename(num_ACT = `Number of Participants`)
res_2_itn <- res_2_itn %>% rename(num_itn = `Number of Participants`)
res_2_wealth <- res_2_wealth %>% rename(num_wealth = `Number of Participants`)

res_ <- left_join(res_2_itn, res_2_wealth, by=c("x", "y"))
res_n <- left_join(res_, res_2_ACT, by=c("x", "y"))
res_fin <- left_join(res_n, res_2_pfpr, by=c("x", "y"))

write.csv(res_fin, "results/urban_cluster/ranking_df.csv")


summary(res_$mean_rich)
