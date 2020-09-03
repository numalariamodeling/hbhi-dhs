########################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files for analysis of case amanegement data for 2007, 2011 and 2016-17 
case.lst <- list(ch_files[[1]], ch_files[[4]], ch_files[[7]])

table(case.lst[[3]]$CA6AA)


#fever, sought care and ACT var 
#2016-17 
#recode (obtain PSU and stratum values from HH dataset)
case.lst[[3]]$CA10 <- recoder5(case.lst[[3]]$CA10) #sought care for fever 


hh17.df <- ch_files[[8]] %>% dplyr::select(HH1,HH2,stratum, PSU)
case.lst[[3]] <- case.lst[[3]] %>% left_join(hh17.df)


#fever, sought care and ACT var 
#2011
#recode (PSU and strtaum values in dataset)
case.lst[[2]]$ML3 <- recoder5(case.lst[[2]]$ML3) #sought care 


#####################################################################################################
# sought care for fever 
####################################################################################################

# 2016 - 2017 
case.lst[[3]] <-dataclean.mics(case.lst[[3]], CA10, chweight,'CA10', 'sought_care')  
fever.svyd16_17 <- svydesign.fun(case.lst[[3]])


DS_fever_pre_16 <- result.fun('sought_care', 'HH7','num_p', design=fever.svyd16_17)

iLabels <- val_labels(DS_fever_pre_16$HH7)

match.idx <- match(DS_fever_pre_16$HH7, iLabels)

DS_fever_pre_16$State <- ifelse(is.na(match.idx),
                                DS_fever_pre_16$State,
                                names(iLabels)[match.idx])

head(DS_fever_pre_16)
write.csv(DS_fever_pre_16, "results/MICS/sought_care/DS_sought_care_fever_pre_16_17.csv")



# 2011 
case.lst[[2]] <-dataclean.mics(case.lst[[2]], ML3, chweight,'ML3', 'sought_care')  
fever.svyd11 <- svydesign.fun(case.lst[[2]])


DS_fever_pre_11 <- result.fun('sought_care', 'HH7','num_p', design=fever.svyd11)

iLabels <- val_labels(DS_fever_pre_11$HH7)

match.idx <- match(DS_fever_pre_11$HH7, iLabels)

DS_fever_pre_11$State <- ifelse(is.na(match.idx),
                                DS_fever_pre_11$State,
                                names(iLabels)[match.idx])

head(DS_fever_pre_11)
write.csv(DS_fever_pre_11, "results/MICS/sought_care/DS_sought_care_fever_pre_11.csv")



#####################################################################################################
## Maps 
####################################################################################################

S_file_16 <- admin1shp_sf%>%left_join(DS_fever_pre_16)
head(S_file_16)

S_file_11 <- admin1shp_sf_%>%left_join(DS_fever_pre_11)
head(S_file_11)


care_mics_16 <- tmap.fun4(S_file_16, "Care-seeking in Nigerian States among Under-fives (2016-17)", 
                      "U5 prevalence", "sought_care", "RdYlBu")

care_mics_11 <- tmap.fun4(S_file_11, "Care-seeking in Nigerian States among Under-fives (2011)", 
                         "U5 prevalence", "sought_care", "RdYlBu")

care_mics <- tmap_arrange(care_mics_11, care_mics_16)

tmap_save(tm = care_mics, filename = "results/MICS/sought_care/all_care_mics.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

