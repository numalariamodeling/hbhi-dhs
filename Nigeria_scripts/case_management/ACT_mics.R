########################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files for analysis of case amanegement data for 2007, 2011 and 2016-17 
case.lst <- list(ch_files[[1]], ch_files[[4]], ch_files[[7]])



#fever, sought care and ACT var 
#2016-17 
#recode (obtain PSU and stratum values from HH dataset)
#sought care and received ACT 
case.lst[[3]]$ACT <- ifelse(case.lst[[3]]$CA10 == 1 & case.lst[[3]]$CA13E == 'E', 1, 
                            ifelse(case.lst[[3]]$CA10 == 1 & case.lst[[3]]$CA13E == '',0,
                                   ifelse(case.lst[[3]]$CA10 == 1 & case.lst[[3]]$CA13E == '?',0,NA)))

table(case.lst[[3]]$CA10)
table(case.lst[[3]]$ACT)


hh17.df <- ch_files[[8]] %>% dplyr::select(HH1,HH2,stratum, PSU)
case.lst[[3]] <- case.lst[[3]] %>% left_join(hh17.df)


#fever, sought care and ACT var 
#2011
#recode (PSU and strtaum values in dataset)
case.lst[[2]]$ACT <- ifelse(case.lst[[2]]$ML3 == 1 & case.lst[[2]]$ML6E == 'E', 1, 
                            ifelse(case.lst[[2]]$ML3 == 1 & case.lst[[2]]$ML6E == '',0,
                                   ifelse(case.lst[[2]]$ML3 == 1 & case.lst[[2]]$ML6E == '?',0,NA)))


#####################################################################################################
# received ACT
####################################################################################################

# 2016 - 2017 
case.lst[[3]] <-dataclean.mics(case.lst[[3]], ACT, chweight,'ACT', 'ACT')  
ACT.svyd16_17 <- svydesign.fun(case.lst[[3]])


DS_ACT_pre_16 <- result.fun('ACT', 'HH7','num_p', design=ACT.svyd16_17)

iLabels <- val_labels(DS_ACT_pre_16$HH7)

match.idx <- match(DS_ACT_pre_16$HH7, iLabels)

DS_ACT_pre_16$State <- ifelse(is.na(match.idx),
                                DS_ACT_pre_16$State,
                                names(iLabels)[match.idx])

head(DS_ACT_pre_16)
write.csv(DS_ACT_pre_16, "results/MICS/ACT/DS_ACT_fever_pre_16_17.csv")



# 2011 
case.lst[[2]] <-dataclean.mics(case.lst[[2]], ACT, chweight,'ACT', 'ACT')  
ACT.svyd11 <- svydesign.fun(case.lst[[2]])

#svymean(~ACT, ACT.svyd11)


DS_ACT_pre_11 <- result.fun('ACT', 'HH7','num_p', design=ACT.svyd11)

iLabels <- val_labels(DS_ACT_pre_11$HH7)

match.idx <- match(DS_ACT_pre_11$HH7, iLabels)

DS_ACT_pre_11$State <- ifelse(is.na(match.idx),
                                DS_ACT_pre_11$State,
                                names(iLabels)[match.idx])

head(DS_ACT_pre_11)
write.csv(DS_ACT_pre_11, "results/MICS/ACT/DS_ACT_fever_pre_11.csv")



#####################################################################################################
## Maps 
####################################################################################################

S_file_16 <- admin1shp_sf%>%left_join(DS_ACT_pre_16)
head(S_file_16)

S_file_11 <- admin1shp_sf_%>%left_join(DS_ACT_pre_11)
head(S_file_11)


ACT_mics <- tmap.fun4(S_file_16, "Receipt of ACT in Nigerian States among Under-fives (2016-17)", 
                      "U5 prevalence", "ACT", "RdYlBu")

ACT_mics_11 <- tmap.fun4(S_file_11, "Receipt of ACT in Nigerian States among Under-fives (2011)", 
                      "U5 prevalence", "ACT", "RdYlBu")

all_ACT <- tmap_arrange(ACT_mics_11, ACT_mics)

tmap_save(tm = all_ACT, filename = "results/MICS/ACT/all_ACT.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

