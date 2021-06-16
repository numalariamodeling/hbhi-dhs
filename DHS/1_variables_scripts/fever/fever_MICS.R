########################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files for analysis of case amanegement data for 2007, 2011 and 2016-17 
case.lst <- list(ch_files[[1]], ch_files[[4]], ch_files[[7]])

#fever, sought care and ACT var 
#2016-17 
#recode (obtain PSU and stratum values from HH dataset)
case.lst[[3]]$CA6AA <- recoder5(case.lst[[3]]$CA6AA)#fever 

hh17.df <- ch_files[[8]] %>% dplyr::select(HH1,HH2,stratum, PSU)
case.lst[[3]] <- case.lst[[3]] %>% left_join(hh17.df)


#fever, sought care and ACT var 
#2011
#recode (PSU and strtaum values in dataset)
case.lst[[2]]$ML1 <- recoder5(case.lst[[2]]$ML1) #fever 



#fever, sought care (child seen at health facility during illness) and ACT var 
#2007

#recode (can't find stratum and PSU values)
case.lst[[1]]$ML1 <- recoder5(case.lst[[1]]$ML1) #fever 



#####################################################################################################
# fever 
####################################################################################################

# 2016 - 2017 
case.lst[[3]] <-dataclean.mics(case.lst[[3]], CA6AA, chweight,'CA6AA', 'fever')  
fever.svyd18 <- svydesign.fun(case.lst[[3]])


DS_fever_pre_18 <- result.fun('fever', 'HH7','num_p', design=fever.svyd18)

iLabels <- val_labels(DS_fever_pre_18$HH7)

match.idx <- match(DS_fever_pre_18$HH7, iLabels)

DS_fever_pre_18$State <- ifelse(is.na(match.idx),
                                DS_fever_pre_18$State,
                              names(iLabels)[match.idx])

head(DS_fever_pre_18)
write.csv(DS_fever_pre_18, "results/MICS/fever/DS_fever_pre_16-17.csv")




# 2011
case.lst[[2]] <-dataclean.mics(case.lst[[2]], ML1, chweight,'ML1', 'fever')  
fever.svyd11 <- svydesign.fun(case.lst[[2]])



DS_fever_pre_11 <- result.fun('fever', 'HH7','num_p', design=fever.svyd11)

iLabels <- val_labels(DS_fever_pre_11$HH7)

match.idx <- match(DS_fever_pre_11$HH7, iLabels)

DS_fever_pre_11$State <- ifelse(is.na(match.idx),
                                DS_fever_pre_11$State,
                                names(iLabels)[match.idx])

head(DS_fever_pre_11)
write.csv(DS_fever_pre_18, "results/MICS/fever/DS_fever_pre_11.csv")


#####################################################################################################
## Maps 
####################################################################################################

S_file_16 <- admin1shp_sf%>%left_join(DS_fever_pre_18)
head(S_file_16)

S_file_11 <- admin1shp_sf%>%left_join(DS_fever_pre_11)
head(S_file_11)


fever_mics_16 <- tmap.fun4(S_file_16, "Fever prevalence in Nigerian States among Under-fives (2016-17)", 
                       "U5 prevalence", "fever", "RdYlBu")

fever_mics_11 <- tmap.fun4(S_file_11, "Fever prevalence in Nigerian States among Under-fives (2011)", 
                          "U5 prevalence", "fever", "RdYlBu")

fever_mics <- tmap_arrange(fever_mics_11, fever_mics_16)

tmap_save(tm = fever_mics, filename = "results/MICS/fever/all_fever_mics.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


