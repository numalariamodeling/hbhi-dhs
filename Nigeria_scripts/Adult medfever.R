hspr.list <- list(NGAfiles[[12]], NGAfiles[[18]])


# health seeking in PR files estimation 

# 2015 

table(hspr.list[[2]]$sh13)
hspr.list[[2]][,"sh13"] <-recoder(hspr.list[[2]][,"sh13"]) #fever in the last two weeks 

table(hspr.list[[2]]$sh14)
hspr.list[[2]][,"sh14"] <-recoder(hspr.list[[2]][,"sh14"]) #treatment for fever in the last two weeks 


hspr.list[[2]][,"med_fever"] <-recoder3(hspr.list[[2]][,"sh15"]) #place treatment for fever was sought 
table(hspr.list[[2]]$med_fever)




# 2010 


hspr.list[[1]][,"sh11"] <-recoder(hspr.list[[1]][,"sh11"]) #fever in the last two weeks 
table(hspr.list[[1]]$sh11)

hspr.list[[1]][,"sh12"] <-recoder(hspr.list[[1]][,"sh12"]) #treatment for fever in the last two weeks 
table(hspr.list[[1]]$sh12)

table(hspr.list[[1]]$sh13)
hspr.list[[1]][,"med_fever"] <-recoder4(hspr.list[[1]][,"sh13"]) #place treatment for fever was sought 
table(hspr.list[[1]]$med_fever)


# applying function to create month and year of survey. Note this function only works for person and household recode file
hspr.list <-map(hspr.list, survey.month.fun)




# key list for hspr, 2010 and 2015 
key_hspr <- list(key_list[[4]], key_list[[6]]) #changing to a list of keys 

hspr.list <-map2(hspr.list, key_hspr , left_join) #medical treatment for fever in PR files 



#########################################################################################################################
# Cleaning and exporting to SAS 
#########################################################################################################################

# DS-level estimates 

# 2010

hspr.list[[1]] <-dataclean.HH(hspr.list[[1]], med_fever, hv005,'med_fever', 'med_fever')  

over5_medf <- hspr.list[[1]] %>% filter(hv105 > 5 | is.na(med_fever)) # over five 
table(over5_medf$med_fever)

medf_pr.svyd10 <- svydesign.fun(over5_medf)
S_medpr_10 <- result.fun('med_fever', 'shstate','num_p', design=medf_pr.svyd10)


tail(S_medpr_10)
head(S_medpr_10)

# 
# write.foreign(over5_medf, "data/med_A_10_dat.txt", "src/med_fever_A_10.sas",   package="SAS")



# 2015

hspr.list[[2]] <-dataclean.HH(hspr.list[[2]], med_fever, hv005,'med_fever', 'med_fever')  

over5_medf_15 <- hspr.list[[2]] %>% filter(hv105 > 5) # over five 
table(over5_medf_15$med_fever)

medf_pr.svyd15 <- svydesign.fun(over5_medf_15)
S_medpr_15 <- result.fun('med_fever', 'shstate','num_p', design=medf_pr.svyd15)



head(S_medpr_15)


# write.foreign(over5_medf, "data/med_A_10_dat.txt", "src/med_fever_A_10.sas",   package="SAS")


# cluster-level estimates 

# 2010
clu_fever_10 <- result.clu.fun('med_fever', 'v001', design=medf_pr.svyd10 , hspr.list[[1]])
head(clu_fever_10)


# 2015
clu_fever_15 <- result.clu.fun('med_fever', 'v001', design=medf_pr.svyd15 , hspr.list[[2]])
head(clu_fever_15)


#####################################################################################################
# Changing the state codes to state names 
#####################################################################################################
look_for(hspr.list[[1]], "state")

#2010 
iLabels <- val_labels(hspr.list[[1]]$shstate)

match.idx <- match(S_medpr_10$shstate, iLabels)

S_medpr_10$ADM1_NAME <- ifelse(is.na(match.idx),
                           S_medpr_10$shstate,
                              names(iLabels)[match.idx])

S_medpr_10$ADM1_NAME <- str_to_title(S_medpr_10$ADM1_NAME)

S_medpr_10 <- S_medpr_10%>%mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja")) 

# write.csv(rep_DS_S, "results/arch_med_10_A.csv")



#2015 

iLabels_15 <- val_labels(hspr.list[[2]]$shstate)

match.idx <- match(S_medpr_15$shstate, iLabels)

S_medpr_15$ADM1_NAME <- ifelse(is.na(match.idx),
                           S_medpr_15$shstate,
                              names(iLabels)[match.idx])

S_medpr_15$ADM1_NAME <- str_to_title(S_medpr_15$ADM1_NAME)

S_medpr_15 <- S_medpr_15%>%mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja")) 

# write.csv(rep_DS_S, "results/arch_med_15_A.csv")


#####################################################################################################
# Maps
#####################################################################################################
# 2010 transformations 
S_file <- admin1shp_sf %>%left_join(S_medpr_10)

pts_file <- st_as_sf(NGAshplist[[4]])   

pts_file <- pts_file %>% left_join(clu_fever_10)


# 2015 transformations 
S_file_15 <- admin1shp_sf %>%left_join(S_medpr_15)

pts_file_15 <- st_as_sf(NGAshplist[[6]])   

pts_file_15 <- pts_file_15 %>% left_join(clu_fever_15)



# 2010 map 
nga_medfev10 <- tmap.fun3(S_file, "med_fever", "Adult prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (2010)",pts_file, 
                          "Number of Participants", "med_fever")


# 2015 map 
nga_medfev15 <- tmap.fun3(S_file_15, "med_fever", "Adult prevalence",
                          "Receipt of Medical Treatment for Fever in Nigerian States (2015)",pts_file, 
                          "Number of Participants", "med_fever")


all_medfever_AS <- tmap_arrange(nga_medfev10,nga_medfev15)

tmap_save(tm = all_medfever_AS, filename = "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/maps/DHS maps/Adult_med_fever/State/allmedfever_AS.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
