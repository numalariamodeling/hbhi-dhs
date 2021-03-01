ifelse(!dir.exists(file.path(ResultDir, "ITN")), 
       dir.create(file.path(ResultDir, "ITN")), FALSE)
print_path <- file.path(ResultDir, "ITN")


#functions 
recode_itn <- function(data) {
  data %>% mutate(hh_itn = ifelse(hml12 == 9, NA,ifelse(hml12 ==1 | hml12 ==2, 1, 0)))
}

survey.month.fun <- function(data) {
  data%>% mutate(MM = (hv008 - ((hv007 - 1900) * 12))) %>% #dhs pr 2010
    dplyr::rename(v001 = hv001) %>%
    mutate(YYYY = (floor((hv008 - 1)/12)+1900))%>%
    mutate (timepoint = str_c(MM, hv016, YYYY, sep = '-'))%>%
    mutate(time2 = str_c(MM, YYYY, sep = '-'))
}

# ITN coverage for year 2003 - 2018 with mapping script 

if (Variable == "ITN"){
  itn.ls <-read.files( ".*NGPR.*\\.DTA", DataDir, read_dta)
  itn.ls[[1]] <- NULL 
  itn.ls <- map(itn.ls, survey.month.fun)
  itn.ls <- lapply(itn.ls, subset, hv103 == 1)
  itn.ls <- lapply(itn.ls, recode_itn)
  #itn.ls <- map(itn.ls, survey.month.fun)
  
  #monthly ITN use values 
  col_names = c('YYYY', 'MM', 'hh_itn', 'hv005', 'v001', 'hv008', 'hv007', 'hv016', 'hv022', 'hv021')
  itn_ls_m = lapply(itn.ls, "[", col_names)
  itn_df <- plyr::ldply(itn_ls_m, rbind)
  itn_df<-itn_df %>% mutate(wt=hv005/1000000,strat=hv022,
           id=hv021, num_p=1)
  table(itn_df$MM)
  table(itn_df$YYYY)
  svyd <- svydesign.fun(itn_df)
  itn_use_month<-svyby(formula=~hh_itn, by=~MM, FUN=svymean, svyd, na.rm=T)
  write.csv(itn_use_month, paste0(print_path, "/", "itn_monthly_values.csv"))
  
  #continue making yearly values
  key_list[[1]] <- NULL
  NGAshplist[[1]]<- NULL
  NGA_ID <- lapply(NGAshplist, "[[", "DHSCLUST")
  key_list <- Map(cbind, key_list, v001 = NGA_ID)
  itn.ls <- map2(itn.ls, key_list, left_join)
  rep_DS.ls <- list(rep_DS)
 


      if (age == "> 18"){
        itn.ls <- lapply(itn.ls, subset, hv105 > 18)
      }else if (age == "U5"){
        itn.ls <-lapply(itn.ls, subset, hv105 <= 5)
      }else if (age == "6-9"){
        itn.ls <-lapply(itn.ls, subset, hv105 > 5 & hv105 < 10)
      }else if (age == "10-18"){
        itn.ls <-lapply(itn.ls, subset, hv105 > 9 & hv105 < 19)
      }else {
        itn.ls <- itn.ls
      }
  
  
      if (grepl("LGA|State|repDS|region", subVariable)) {
        comboITN.list  <- map2(comboITN.list, rep_DS.ls, left_join) #PR datasets
        LGA_sf <- LGA_clean_names%>%  as_tibble() %>% dplyr::select(LGA, State)
        repDS_LGA<- rep_DS %>% left_join(LGA_sf)
        
        if(subVariable == "LGA"){
          print("computing raw ITN coverage estimates at the LGA-level for years 2008, 2010, 2013, 2015 & 2018")
          var <- list("LGA")
          ITN_LGA <- map2(comboITN.list, var, generate.ITN.state_LGA_repDS)
          fin_df <- plyr::ldply(ITN_LGA, rbind)

      }
  }
}

#####################################################################################################
#ITN coverage 
####################################################################################################


# 2018
hh.itn.list[[6]] <-dataclean.HH(hh.itn.list[[6]], hml12, hv005,'hh_itn', 'hh_itn')  
hh.itn.svyd18 <- svydesign.fun(hh.itn.list[[6]])


#LGA_level estimates 
DS_hh_itn_18 <- result.fun('hh_itn', 'LGA','num_p', design=hh.itn.svyd18)
head(DS_hh_itn_18)

# DS_hh_itn_18_v2 <- DS_hh_itn_18  %>% 
#   dplyr::select(State, hv025, hh_itn) %>% 
#   pivot_wider(names_from = hv025, values_from = hh_itn) 
# head(DS_hh_itn_18_v2)
# 
# write.csv(DS_hh_itn_18_v2, "results/urbanvsrural/2018_ITNstate.csv")


#next join each LGA to their repDS 

DS_hh_itn_18 <- rep_DS %>% left_join(DS_hh_itn_18)
head(DS_hh_itn_18)
summary(DS_hh_itn_18$hh_itn)


#repDS_level estimates and change variable names 
rep_DS_hh_itn_18 <- result.fun('hh_itn', 'repDS','num_p', design=hh.itn.svyd18)
head(rep_DS_hh_itn_18)

rep_DS_hh_itn_18 <- rep_DS_hh_itn_18  %>% dplyr::select(repDS, hh_itn_repDS = hh_itn,
                                                                 se_repDS = se,Num_repDS = `Number of Participants`)

head(rep_DS_hh_itn_18)


#Now combine LGA files with LGA level estimates with repDS estimates 
rep_DS_hh_itn_18 <- DS_hh_itn_18 %>% left_join(rep_DS_hh_itn_18) %>% 
  mutate(hh_itn = ifelse(hh_itn =="0"| hh_itn == "1" |is.na(hh_itn), 
                         hh_itn_repDS, hh_itn), 
         ci_l = hh_itn - (1.96 * se_repDS), ci_u = hh_itn + (1.96 * se_repDS)) %>%  
  mutate(ci_l = ifelse(ci_l < 0, 0, ci_l), ci_u =ifelse(ci_u > 1, 1, ci_u))


head(rep_DS_hh_itn_18)
summary(rep_DS_hh_itn_18$hh_itn)


write.csv(rep_DS_hh_itn_18 , "results/archetype_sim_input/Intervention_files_LGA/ITN/over_eighteen2018repDS_LGA.csv")


# 2015
hh.itn.list[[5]] <-dataclean.HH(hh.itn.list[[5]], hml12, hv005,'hh_itn', 'hh_itn')  
hh.itn.svyd15 <- svydesign.fun(hh.itn.list[[5]])


DS_hh_itn_15 <- result.fun('hh_itn', 'LGA','num_p', design=hh.itn.svyd15)
head(DS_hh_itn_15)

# DS_hh_itn_15_v2 <- DS_hh_itn_15  %>% 
#   dplyr::select(State, hv025, hh_itn) %>% 
#   pivot_wider(names_from = hv025, values_from = hh_itn) 
# head(DS_hh_itn_15_v2)
# 
# write.csv(DS_hh_itn_15_v2, "results/urbanvsrural/2015_ITNstate.csv")




#next join each LGA to their repDS 

DS_hh_itn_15 <- rep_DS %>% left_join(DS_hh_itn_15)
head(DS_hh_itn_15)
summary(DS_hh_itn_15$hh_itn)


#repDS_level estimates and change variable names 
rep_DS_hh_itn_15 <- result.fun('hh_itn', 'repDS','num_p', design=hh.itn.svyd15)
head(rep_DS_hh_itn_15)

rep_DS_hh_itn_15 <- rep_DS_hh_itn_15  %>% dplyr::select(repDS, hh_itn_repDS = hh_itn,
                                                       se_repDS = se, Num_repDS = `Number of Participants`)

head(rep_DS_hh_itn_15)


#Now combine LGA files with LGA level estimates with repDS estimates 
rep_DS_hh_itn_15 <- DS_hh_itn_15 %>% left_join(rep_DS_hh_itn_15) %>% 
  mutate(hh_itn = ifelse(hh_itn =="0"| hh_itn == "1" |is.na(hh_itn), 
                         hh_itn_repDS, hh_itn), 
         ci_l = hh_itn - (1.96 * se_repDS), ci_u = hh_itn + (1.96 * se_repDS)) %>%  
  mutate(ci_l = ifelse(ci_l < 0, 0, ci_l), ci_u =ifelse(ci_u > 1, 1, ci_u))

head(rep_DS_hh_itn_15)

write.csv(rep_DS_hh_itn_15, "results/archetype_sim_input/Intervention_files_LGA/ITN/over_eighteen2015repDS_LGA_v2.csv")




# 2013
hh.itn.list[[4]] <-dataclean.HH(hh.itn.list[[4]], hml12, hv005,'hh_itn', 'hh_itn')  
hh.itn.svyd13 <- svydesign.fun(hh.itn.list[[4]])

DS_hh_itn_13 <- result.fun('hh_itn', 'LGA','num_p', design=hh.itn.svyd13)
head(DS_hh_itn_13)


# DS_hh_itn_13_v2 <- DS_hh_itn_13 %>% 
#   dplyr::select(State, hv025, hh_itn) %>% 
#   pivot_wider(names_from = hv025, values_from = hh_itn) 
# head(DS_hh_itn_13_v2)
# 
# write.csv(DS_hh_itn_13_v2, "results/urbanvsrural/2013_ITNstate.csv")


#next join each LGA to their repDS 

DS_hh_itn_13 <- rep_DS %>% left_join(DS_hh_itn_13)
head(DS_hh_itn_13)
summary(DS_hh_itn_13$hh_itn)

#repDS_level estimates and change variable names 
rep_DS_hh_itn_13 <- result.fun('hh_itn', 'repDS','num_p', design=hh.itn.svyd13)
head(rep_DS_hh_itn_13)

rep_DS_hh_itn_13 <- rep_DS_hh_itn_13 %>% dplyr::select(repDS, hh_itn_repDS = hh_itn,
                                                        se_repDS = se, Num_repDS = `Number of Participants`)

head(rep_DS_hh_itn_13)

#Now combine LGA files with LGA level estimates with repDS estimates 
rep_DS_hh_itn_13 <- DS_hh_itn_13 %>% left_join(rep_DS_hh_itn_13) %>% 
  mutate(hh_itn = ifelse(hh_itn =="0"| hh_itn == "1" |is.na(hh_itn), 
                         hh_itn_repDS, hh_itn), 
         ci_l = hh_itn - (1.96 * se_repDS), ci_u = hh_itn + (1.96 * se_repDS)) %>%  
  mutate(ci_l = ifelse(ci_l < 0, 0, ci_l))


head(rep_DS_hh_itn_13)

write.csv(rep_DS_hh_itn_13, "results/archetype_sim_input/Intervention_files_LGA/ITN/U52013repDS_LGA_v2.csv")





# 2010
hh.itn.list[[3]] <-dataclean2.HH(hh.itn.list[[3]], hml12, hv005,'hh_itn', 'hh_itn', 'hv023')  
hh.itn.svyd10 <- svydesign.fun(hh.itn.list[[3]])


DS_hh_itn_10 <- result.fun('hh_itn', 'LGA','num_p', design=hh.itn.svyd10)
head(DS_hh_itn_10)


# DS_hh_itn_10_v2 <- DS_hh_itn_10 %>% 
#   dplyr::select(State, hv025, hh_itn) %>% 
#   pivot_wider(names_from = hv025, values_from = hh_itn) 
# head(DS_hh_itn_10_v2)
# 
# write.csv(DS_hh_itn_10_v2, "results/urbanvsrural/2010_ITNstate.csv")


#next join each LGA to their repDS 

DS_hh_itn_10 <- rep_DS %>% left_join(DS_hh_itn_10)
head(DS_hh_itn_10)
summary(DS_hh_itn_10$hh_itn)


#repDS_level estimates and change variable names 
rep_DS_hh_itn_10 <- result.fun('hh_itn', 'repDS','num_p', design=hh.itn.svyd10)
head(rep_DS_hh_itn_10)

rep_DS_LGA_10 <- left_join(rep_DS, rep_DS_hh_itn_10)
head(rep_DS_LGA_10)


rep_DS_hh_itn_10 <- rep_DS_hh_itn_10 %>% dplyr::select(repDS, hh_itn_repDS = hh_itn,
                                                       se_repDS = se, Num_repDS = `Number of Participants`)

head(rep_DS_hh_itn_10)

#Now combine LGA files with LGA level estimates with repDS estimates 
rep_DS_hh_itn_10 <- DS_hh_itn_10 %>% left_join(rep_DS_hh_itn_10) %>% 
  mutate(hh_itn = ifelse(hh_itn =="0"| hh_itn == "1" |is.na(hh_itn), 
                         hh_itn_repDS, hh_itn), 
         ci_l = hh_itn - (1.96 * se_repDS), ci_u = hh_itn + (1.96 * se_repDS)) %>%  
  mutate(ci_l = ifelse(ci_l < 0, 0, ci_l), ci_u =ifelse(ci_u > 1, 1, ci_u))






#Now combine LGA files with LGA level estimates with repDS estimates 
head(rep_DS_hh_itn_10)



write.csv(rep_DS_hh_itn_10, "results/archetype_sim_input/Intervention_files_LGA/ITN/v2/ten_eighteen2010repDS_LGA_v2.csv")






# cluster-level estimates

# 2018

clu_HH_ITN_18 <- result.clu.fun('hh_itn', 'v001', design=hh.itn.svyd18,hh.itn.list[[6]], "hv007")
head(clu_HH_ITN_18)

# write.csv(clu_HH_ITN_18, "results/DHS_HH_ITN/clu_hh_itn_18.csv")


# 2015
clu_HH_ITN_15 <- result.clu.fun('hh_itn', 'v001', design=hh.itn.svyd15,hh.itn.list[[5]], "hv007")
head(clu_HH_ITN_15)

# write.csv(clu_HH_ITN_14, "results/DHS_HH_ITN/clu_hh_itn_14.csv")

# 2013
clu_HH_ITN_13 <- result.clu.fun('hh_itn', 'v001', design=hh.itn.svyd13,hh.itn.list[[4]], "hv007")
head(clu_HH_ITN_13)


# 2010
clu_HH_ITN_10 <- result.clu.fun('hh_itn', 'v001', design=hh.itn.svyd10,hh.itn.list[[3]], "hv007")
head(clu_HH_ITN_10)

# write.csv(clu_HH_ITN_10, "results/DHS_HH_ITN/clu_hh_itn_10.csv")


# # 2003
# clu_HH_ITN_03 <- result.clu.fun('hh.itn', 'v001', design=hh.itn.svyd03,hh.itn.list[[1]])
# head(clu_HH_ITN_03)
# 
# write.csv(clu_HH_ITN_03, "results/DHS_HH_ITN/clu_hh_itn_03.csv")
# 

#####################################################################################################
## Maps 
####################################################################################################

# 2018 transformations 
DS_file <- LGAshp_sf %>% left_join(rep_DS_hh_itn_18)

pts_shp_18 <- st_as_sf(NGAshplist[[7]])

pts_file <- pts_shp_18 %>% left_join(clu_HH_ITN_18) #%>% filter(URBAN_RURA =="U")
head(pts_file)
summary(is.na(pts_file$hh_itn))
summary(pts_file$hh_itn)
sd(pts_file$`Number of Participants`)

# 2015 transformations 
DS_file_15 <- LGAshp_sf %>% left_join(rep_DS_hh_itn_15)

pts_shp_15 <- st_as_sf(NGAshplist[[6]])

pts_file_15 <- pts_shp_15 %>% left_join(clu_HH_ITN_15)  #%>% filter(URBAN_RURA =="U")
head(pts_file_15)
summary(is.na(pts_file_15$hh_itn))
summary(pts_file_15$hh_itn)
summary(pts_file_15$`Number of Participants`)
sd(pts_file_15$`Number of Participants`)

# 2013 transformations 

DS_file_13 <- LGAshp_sf %>% left_join(rep_DS_hh_itn_13)

pts_shp_13 <- st_as_sf(NGAshplist[[5]])

pts_file_13 <- pts_shp_13 %>% left_join(clu_HH_ITN_13) #%>%filter(URBAN_RURA =="U")
head(pts_file_13)
summary(is.na(pts_file_13$hh_itn))
summary(pts_file_13$hh_itn)
summary(pts_file_13$`Number of Participants`)
sd(pts_file_13$`Number of Participants`)


# 2010 transformations 

DS_file_10 <- LGAshp_sf %>% left_join(rep_DS_LGA_10)
head(DS_file_10)

pts_shp_10 <- st_as_sf(NGAshplist[[4]])

pts_file_10 <- pts_shp_10 %>% left_join() #%>%filter(URBAN_RURA =="U")

head(pts_file_10)

summary(is.na(pts_file_10$hh_itn))
summary(pts_file_10$hh_itn)
summary(pts_file_10$`Number of Participants`)
sd(pts_file_10$`Number of Participants`)




# # 2003 transformations 
# DS_file_03 <- DS_shape_sf %>% left_join(DS_hh_itn_03)
# 
# pts_file_03 <- BFshplist_sf[[3]] %>% left_join(clu_HH_ITN_03)

# 2018 map 
# BF_HH_ITN18 <- tmap.fun5(DS_file, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
#                          maintitle="Household (HH) ITN use by District (2018)", ptsfile=pts_file, "Number of Participants",
#                          "hh.itn")

#u_clu_map <- tmap.clu4(admin1_sf, ptsfile=pts_file, "Number of Participants", "hh_itn", "2018 cluster ITN Nigeria")

HH_ITN_18 <- tmap.fun3(DS_file, "hh_itn",  "Prevalence", "HH ITN 2018",
                             pts_file, "Number of Participants", "hh_itn")


HH_ITN_18 <- tmap.fun4(DS_file, "U5 HH ITN (2018)", "Prevalence", "hh_itn")


# 2015 map 
# BF_HH_ITN14 <- tmap.fun5(DS_file_14, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
#                          maintitle="Household (HH) ITN use by District (2014)", ptsfile=pts_file_14, "Number of Participants",
#                          "hh.itn")

u_clu_map_15 <- tmap.clu4(admin1_sf, ptsfile=pts_file_15, "Number of Participants", "hh_itn", "2015 cluster ITN Nigeria")

HH_ITN_15 <- tmap.fun3(DS_file_15, "hh_itn",  "Prevalence", "HH ITN 2015",
                       pts_file_15, "Number of Participants", "hh_itn")

HH_ITN_15 <- tmap.fun4(DS_file_15, "U5 HH ITN (2015)", "Prevalence", "hh_itn")

# 2013 map 

u_clu_map_13 <- tmap.clu4(admin1_sf, ptsfile=pts_file_13, "Number of Participants", "hh_itn", "2013 cluster ITN Nigeria")

HH_ITN_13 <- tmap.fun3(DS_file_13, "hh_itn",  "Prevalence", "HH ITN 2013",
                       pts_file_13, "Number of Participants", "hh_itn")

HH_ITN_13 <- tmap.fun4(DS_file_13, "U5 HH ITN (2013)", "Prevalence", "hh_itn")

# 2010 map 
# BF_HH_ITN10 <- tmap.fun5(DS_file_10, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
#                          maintitle="Household (HH) ITN use by District (2010)", ptsfile=pts_file_10, "Number of Participants",
#                          "hh.itn")

u_clu_map_10 <- tmap.clu4(admin1_sf, ptsfile=pts_file_15, "Number of Participants", "hh_itn", "2010 cluster ITN Nigeria")

HH_ITN_10 <- tmap.fun3(DS_file_10, "hh_itn",  "Prevalence", "HH ITN 2010",
                       pts_file_10, "Number of Participants", "hh_itn")

HH_ITN_10 <- tmap.fun4(DS_file_10, "U5 HH ITN (2010)", "Prevalence", "hh_itn")


# 2003 map 
# BF_HH_ITN03 <- tmap.fun5(DS_file_03, colname="hh.itn", legtitle="% de facto HH population who slept the night before the survey under any mosquito net", 
#                          maintitle="Household (HH) ITN use by District (2003)", ptsfile=pts_file_03, "Number of Participants",
#                          "hh.itn")

all_hh.itn <- tmap_arrange(HH_ITN_10,HH_ITN_13, HH_ITN_15,HH_ITN_18)

tmap_save(tm = all_hh.itn, filename = "results/archetype_sim_input/Intervention_files_LGA/ITN/hh_itn/hh_itn_repDS_v2.pdf", 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


cluster.itn <- tmap_arrange(u_clu_map_10,u_clu_map_13,u_clu_map_15,u_clu_map)


## creating a combined prevalence map for all years 
#row binding cluster points 
pts_merge <- rbind(pts_file,pts_file_15,pts_file_13,pts_file_10)

head(pts_merge)
summary(is.na(pts_merge$hh_itn))
summary(pts_merge$hh_itn)
summary(pts_merge$`Number of Participants`)
sd(pts_merge$`Number of Participants`)



#aggregated plot on state shape file 
u_clu_map_bindrows <- tmap.clu4(admin1_sf, ptsfile=pts_merge, "Number of Participants", "hh_itn", "ITN 2010 - 2018 in Nigeria")



#creates aggreagted map of pfpr for all years using mean values 

#calculate mean of points to pfpr estimate over time 
pts_merge_test <- pts_merge %>% drop_na(hh_itn)

pts100 <- st_is_within_distance(pts_merge_test, dist = 2000)
class(pts100)


res <- data.frame(id=1:length(pts100),x=NA, y=NA,mean_hh_itn=NA)
res$x <- sapply(pts100, function(p){mean(pts_merge_test$LONGNUM[p])})
res$y <- sapply(pts100, function(p){mean(pts_merge_test$LATNUM[p])})
res$mean_hh_itn<- sapply(pts100, function(p) #scaled by the proportion of participants in each nearby cluster 
{sum((pts_merge_test$`Number of Participants`[p]/sum(pts_merge_test$`Number of Participants`[p]) * pts_merge_test$hh_itn[p]))})
res$`Number of Participants` <- sapply(pts100, function(p){mean(pts_merge_test$`Number of Participants`[p])}) 
res_2_itn <- res %>% distinct(x, y, .keep_all = TRUE)

coordinates(res_2) <- c("x","y")

proj4string(res_2) <- CRS('+proj=longlat +datum=WGS84 +no_defs')



res_sf_itn <- st_as_sf(res_2)
head(res_sf_itn)


summary(is.na(res_sf_itn$mean_hh_itn))
summary(res_sf_itn$mean_hh_itn)
summary(res_sf_itn$`Number of Participants`)
sd(res_sf_itn$`Number of Participants`)


u_clu_map_aggregated <- tmap.clu4(admin1_sf, ptsfile=res_sf, "Number of Participants", "mean_hh_itn", "Aggregated ITN 2010 - 2018 in Nigeria")

all_maps <- tmap_arrange(u_clu_map_10,u_clu_map_13,u_clu_map_15,u_clu_map,u_clu_map_bindrows,u_clu_map_aggregated)

tmap_save(tm = all_maps, filename = "results/urban_cluster/ITN/itn_diff.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)









