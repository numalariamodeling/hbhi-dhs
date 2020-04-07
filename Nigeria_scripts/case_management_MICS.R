########################################################################################################################
# --- Data preparation ---  # 
#######################################################################################################################

# creating list of files for analysis of case amanegement data for 2007, 2011 and 2016-17 
case.lst <- list(ch_files[[1]], ch_files[[4]], ch_files[[7]])

#fever, sought care and ACT var 
#2016-17 
#recode (obtain PSU and stratum values from HH dataset)
case.lst[[3]]$CA6AA <- recoder5(case.lst[[3]]$CA6AA)#fever 
case.lst[[3]]$CA10 <- recoder5(case.lst[[3]]$CA10) #sought care for fever 
#sought care and received ACT 
case.lst[[3]]$ACT <- ifelse(case.lst[[3]]$CA10 == 1 & case.lst[[3]]$CA13E == 'E', 1, 
                            ifelse(case.lst[[3]]$CA10 == 1 & case.lst[[3]]$CA13E == '',0,
                            ifelse(case.lst[[3]]$CA10 == 1 & case.lst[[3]]$CA13E == '?',0,NA)))


hh17.df <- ch_files[[8]] %>% dplyr::select(HH1,HH2,stratum, PSU)

case.lst[[3]] <- case.lst[[3]] %>% left_join(hh17.df)
summary(case.lst[[3]]$PSU)

#fever, sought care and ACT var 
#2011
#recode (PSU and strtaum values in dataset)
case.lst[[2]]$ML1 <- recoder5(case.lst[[2]]$ML1) #fever 
case.lst[[2]]$ML3 <- recoder5(case.lst[[2]]$ML3) #sought care 
case.lst[[2]]$ACT <- ifelse(case.lst[[2]]$ML3 == 1 & case.lst[[2]]$ML6E == 'E', 1, 
                            ifelse(case.lst[[2]]$ML3 == 1 & case.lst[[2]]$ML6E == '',0,
                                   ifelse(case.lst[[2]]$ML3 == 1 & case.lst[[2]]$ML6E == '?',0,NA)))


#fever, sought care (child seen at health facility during illness) and ACT var 
#2007

#recode (can't find stratum and PSU values)
case.lst[[1]]$ML1 <- recoder5(case.lst[[1]]$ML1) #fever 
case.lst[[1]]$ML2 <- recoder5(case.lst[[1]]$ML2) #sought care 
case.lst[[1]]$ACT <- ifelse(case.lst[[1]]$ML2 == 1 & case.lst[[1]]$ML4E == 'E', 1, 
                            ifelse(case.lst[[1]]$ML2 == 1 & case.lst[[1]]$ML4E == '',0,
                                   ifelse(case.lst[[1]]$ML2 == 1 & case.lst[[1]]$ML4E == '?',0,NA)))



#####################################################################################################
# fever 
####################################################################################################

# 2016 - 2017 
case.lst[[3]] <-dataclean.mics(case.lst[[3]], CA6AA, chweight,'CA6AA', 'fever')  
fever.svyd18 <- svydesign.fun(case.lst[[3]])

table(case.lst[[3]]$HH7)
DS_fever_pre_18 <- result.fun('fever', 'HH7','num_p', design=fever.svyd18)
head(DS_fever_pre_18)
write.csv(DS_ANC_pre_18, "results/ANC_Nigeria/DS_ANC_pre_18.csv")




