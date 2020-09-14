
look_for(NGAfiles[[11]], "fever")

table(NGAfiles[[2]]$h32z) #1990
table(NGAfiles[[5]]$h32z) #2003
table(NGAfiles[[8]]$h32z) #2008
table(NGAfiles[[11]]$h32z)#2010
table(NGAfiles[[14]]$h32z)#2013
table(NGAfiles[[17]]$h32z)#2015
table(NGAfiles[[20]]$h32z)#2018

# list for 1990, 2003, 2008, 2010, 2013, 2015, 2018 
h32z.list <- list(NGAfiles[[2]],NGAfiles[[5]],NGAfiles[[8]],NGAfiles[[11]],NGAfiles[[14]],NGAfiles[[17]],
                  NGAfiles[[20]])

# recoding U5 medical treatment using h32z 
# 2013 
h32z.list[[5]][,"h32z"] <-recoder(h32z.list[[5]][,"h32z"]) 
table(h32z.list[[5]][,"h32z"])


# 2010
h32z.list[[4]][,"h32z"] <-recoder(h32z.list[[4]][,"h32z"]) 
table(h32z.list[[4]][,"h32z"])


# key datasets and dhs/mis datasets are joined  

h32z.list <-map2(h32z.list,key_list, left_join) #medfever datasets 



#data cleaning 


# 2018

h32z.list[[7]]<-dataclean(h32z.list[[7]], h32z, v005, 'h32z', 'med_fever_raw')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


h32z.svyd18 <- svydesign.fun(h32z.list[[7]])


h32z_pre_18 <- result.fun('med_fever_raw', 'sstate','num_p', design=h32z.svyd18)
head(h32z_pre_18)

iLabels_18 <- val_labels(h32z.list[[7]]$sstate)

match.idx_18 <- match(h32z_pre_18$sstate, iLabels_18)

h32z_pre_18$ADM1_NAME <- ifelse(is.na(match.idx_18),
                                h32z_pre_18$ADM1_NAME,
                                  names(iLabels_18)[match.idx_18])

h32z_pre_18$ADM1_NAME <- str_to_title(h32z_pre_18$ADM1_NAME)

# write.csv(medfev_pre_18, "results/medfev_pre_18.csv")


# 2015

h32z.list[[6]]<-dataclean(h32z.list[[6]], h32z, v005, 'h32z', 'med_fever_raw')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


h32z.svyd15 <- svydesign.fun(h32z.list[[6]])


h32z_pre_15 <- result.fun('med_fever_raw', 'sstate','num_p', design=h32z.svyd15)
head(h32z_pre_15)

iLabels_15 <- val_labels(h32z.list[[6]]$sstate)

match.idx_15 <- match(h32z_pre_15$sstate, iLabels_15)

h32z_pre_15$ADM1_NAME <- ifelse(is.na(match.idx_15),
                                h32z_pre_15$ADM1_NAME,
                                names(iLabels_15)[match.idx_15])

h32z_pre_15$ADM1_NAME <- str_to_title(h32z_pre_15$ADM1_NAME)

# write.csv(medfev_pre_18, "results/medfev_pre_18.csv")


# 2013

h32z.list[[5]]<-dataclean(h32z.list[[5]], h32z, v005, 'h32z', 'med_fever_raw')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


h32z.svyd13 <- svydesign.fun(h32z.list[[5]])


h32z_pre_13 <- result.fun('med_fever_raw', 'sstate','num_p', design=h32z.svyd13)
head(h32z_pre_13)

iLabels_13 <- val_labels(h32z.list[[5]]$sstate)

match.idx_13 <- match(h32z_pre_13$sstate, iLabels_13)

h32z_pre_13$ADM1_NAME <- ifelse(is.na(match.idx_13),
                                h32z_pre_13$ADM1_NAME,
                                names(iLabels_13)[match.idx_13])

h32z_pre_13$ADM1_NAME <- str_to_title(h32z_pre_13$ADM1_NAME)


h32z_pre_13 <- h32z_pre_13%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja"))

# write.csv(medfev_pre_18, "results/medfev_pre_18.csv")


# 2010

h32z.list[[4]]<-dataclean(h32z.list[[4]], h32z, v005, 'h32z', 'med_fever_raw')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


h32z.svyd10 <- svydesign.fun(h32z.list[[4]])


h32z_pre_10 <- result.fun('med_fever_raw', 'sstate','num_p', design=h32z.svyd10)
head(h32z_pre_10)

iLabels_10 <- val_labels(h32z.list[[4]]$sstate)

match.idx_10 <- match(h32z_pre_10$sstate, iLabels_10)

h32z_pre_10$ADM1_NAME <- ifelse(is.na(match.idx_10),
                                h32z_pre_10$ADM1_NAME,
                                names(iLabels_10)[match.idx_10])

h32z_pre_10$ADM1_NAME <- str_to_title(h32z_pre_10$ADM1_NAME)


h32z_pre_10 <- h32z_pre_10%>% mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Fct-Abuja" = "Fct Abuja"))


# write.csv(medfev_pre_18, "results/medfev_pre_18.csv")


# 2008

h32z.list[[3]]<-dataclean(h32z.list[[3]], h32z, v005, 'h32z', 'med_fever_raw')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


h32z.svyd08 <- svydesign.fun(h32z.list[[3]])


h32z_pre_08 <- result.fun('med_fever_raw', 'sstate','num_p', design=h32z.svyd08)
head(h32z_pre_08)

iLabels_08 <- val_labels(h32z.list[[3]]$sstate)

match.idx_08 <- match(h32z_pre_08$sstate, iLabels_08)

h32z_pre_08$ADM1_NAME <- ifelse(is.na(match.idx_08),
                                h32z_pre_08$ADM1_NAME,
                                names(iLabels_08)[match.idx_08])

h32z_pre_08$ADM1_NAME <- str_to_title(h32z_pre_08$ADM1_NAME)


h32z_pre_08 <- h32z_pre_08%>%  mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Abuja" = "Fct Abuja"))


# write.csv(medfev_pre_18, "results/medfev_pre_18.csv")


# 2003

h32z.list[[2]]<-dataclean(h32z.list[[2]], h32z, v005, 'h32z', 'med_fever_raw')  

# write.foreign(medfever.list[[6]], "mydata.txt", "med_fever.sas",   package="SAS")


h32z.svyd03 <- svydesign.fun(h32z.list[[2]])


h32z_pre_03 <- result.fun('med_fever_raw', 'sstate','num_p', design=h32z.svyd03)
head(h32z_pre_03)

iLabels_03 <- val_labels(h32z.list[[2]]$sstate)

match.idx_03 <- match(h32z_pre_03$sstate, iLabels_03)

h32z_pre_03$ADM1_NAME <- ifelse(is.na(match.idx_03),
                                h32z_pre_03$ADM1_NAME,
                                names(iLabels_03)[match.idx_03])

h32z_pre_03$ADM1_NAME <- str_to_title(h32z_pre_03$ADM1_NAME)


h32z_pre_03 <- h32z_pre_03%>%mutate(ADM1_NAME = dplyr::recode(ADM1_NAME,"Nassarawa" = "Nasarawa",
                                                              "Zamfora" = "Zamfara", "Abuja (Fct)" = "Fct Abuja"))


# write.csv(medfev_pre_18, "results/medfev_pre_18.csv")



#Maps 

# 2018 transformations 
S_file <- admin1shp_sf%>%left_join(h32z_pre_18)

nga_h32z18  <- tmap.fun4(S_file, "Receipt of Medical Treatment for Fever in Nigerian States_h32z (2018)", 
                              "Prevalence", "med_fever_raw")

# 2015 transformations 
S_file_15 <- admin1shp_sf%>%left_join(h32z_pre_15)

nga_h32z15  <- tmap.fun4(S_file_15, "Receipt of Medical Treatment for Fever in Nigerian States_h32z (2015)", 
                         "Prevalence", "med_fever_raw")

# 2013 transformations 
S_file_13 <- admin1shp_sf%>%left_join(h32z_pre_13)

nga_h32z13  <- tmap.fun4(S_file_13, "Receipt of Medical Treatment for Fever in Nigerian States_h32z (2013)", 
                         "Prevalence", "med_fever_raw")

# 2010 transformations 
S_file_10 <- admin1shp_sf%>%left_join(h32z_pre_10)

nga_h32z10  <- tmap.fun4(S_file_10, "Receipt of Medical Treatment for Fever in Nigerian States_h32z (2010)", 
                         "Prevalence", "med_fever_raw")

# 2008 transformations 
S_file_08 <- admin1shp_sf%>%left_join(h32z_pre_08)

nga_h32z08  <- tmap.fun4(S_file_08, "Receipt of Medical Treatment for Fever in Nigerian States_h32z (2008)", 
                         "Prevalence", "med_fever_raw")

# 2003 transformations 
S_file_03 <- admin1shp_sf%>%left_join(h32z_pre_03)

nga_h32z03 <- tmap.fun4(S_file_03, "Receipt of Medical Treatment for Fever in Nigerian States_h32z (2003)", 
                         "Prevalence", "med_fever_raw")


all_h32z <- tmap_arrange(nga_h32z03,nga_h32z08,nga_h32z10,nga_h32z13,nga_h32z15,nga_h32z18)

tmap_save(tm = all_h32z, filename = "results/all_U5_h32z_medfever.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

