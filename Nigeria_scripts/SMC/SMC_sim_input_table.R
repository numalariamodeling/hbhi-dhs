#read in data and change working directory 

setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

rm(list=ls())

x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix","rlang", "arules", "foreign", "fuzzyjoin")

lapply(x, library, character.only = TRUE) #applying the library function to packages


#population and archetype data 
rep_DS <- read.csv("bin/rep_DS/representative_DS_orig60clusters.csv")
head(rep_DS)
nigeria_LGA_pop <- read.csv("bin/smc_table/nigeria_LGA_pop.csv")
head(nigeria_LGA_pop)
nigeria_LGA_pop_2 <- nigeria_LGA_pop%>%left_join(rep_DS)
write.csv(nigeria_LGA_pop, "results/nigeria_LGA_pop.csv")


#smc data 

smc_df <- read.csv("bin/smc_table/smc_distribution_by_LGA.csv", header = TRUE, 
                   colClasses = c("smc1_month" ="character", "smc2_month" = "character", "smc3_month"
                                  ="character", "smc4_month" = "character", "year" = "character"))


#The smc rounds of data for Adamawa and Sokoto are split across LGAs that recieved it in 2018 weighted by 
#the population size in each LGA 

#Adamawa 

Adam.df <- nigeria_LGA_pop%>%filter(LGA == "Michika"| LGA =="Mubi North"| LGA == "Mubi South") %>% 
            mutate(prop = geopode.pop.0.4/sum(geopode.pop.0.4)) %>% dplyr::select(adm2=LGA, adm1=State, prop)

smc_df2 <- smc_df%>%left_join(Adam.df)


smc_df2_ <- smc_df2 %>%mutate(smc1_num = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc1_num[[116]]*prop, smc1_num), 
                             smc2_num = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc2_num[[116]]*prop, smc2_num),
                             smc3_num = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc3_num[[116]]*prop, smc3_num),
                             smc4_num = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc4_num[[116]]*prop, smc4_num),
                             smc1_month = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc1_month[[116]], smc1_month),
                             smc2_month = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc2_month[[116]], smc2_month),
                             smc3_month = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc3_month[[116]], smc3_month),
                             smc4_month = ifelse(adm1 =="Adamawa" & year=="2018" & adm2!="", smc4_month[[116]], smc4_month))

#Sokoto 

sokoto.df <- nigeria_LGA_pop %>%dplyr::filter(State == "Sokoto") %>% 
                mutate(prop = geopode.pop.0.4/sum(geopode.pop.0.4), year = "2018") %>% 
                dplyr::select(adm2=LGA, adm1=State, prop, year)

smc_df2_so <- smc_df2_%>%left_join(sokoto.df, by = c("adm1", "adm2", "year"))


smc_df2_fin <- smc_df2_so %>%mutate(smc1_num = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc1_num[[135]]*prop.y, smc1_num), 
                                  smc2_num = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc2_num[[135]]*prop.y, smc2_num),
                                  smc3_num = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc3_num[[135]]*prop.y, smc3_num),
                                  smc4_num = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc4_num[[135]]*prop.y, smc4_num),
                                  smc1_month = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc1_month[[135]], smc1_month),
                                  smc2_month = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc2_month[[135]], smc2_month),
                                  smc3_month = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc3_month[[135]], smc3_month),
                                  smc4_month = ifelse(adm1 =="Sokoto" & year=="2018" & adm2!="", smc4_month[[135]], smc4_month)) %>% 
                                  dplyr::select(-prop.y, -prop.x) %>% 
                                  filter(adm2!="")

smc_df2_fin <- smc_df2_fin %>% mutate(smc1_date =ifelse(smc1_month =="August", as.Date(paste(as.numeric(smc1_month), "01", year, sep="-"), format = "%m-%d-%Y"),smc1_month)) 

write.csv(smc_df2_fin, "smc_df2_fin.csv")

# merge smc file with population data 
#first rename columns in LGA population to match smc file 

LGA_pop <- nigeria_LGA_pop %>% dplyr::select(adm2=LGA, adm1=State, geopode.pop.0.4,repDS)

smc_df2_fin2 <- smc_df2_fin %>% left_join(LGA_pop) %>% mutate(smc1_pop = smc1_num/geopode.pop.0.4, 
                                                smc2_pop = smc2_num/geopode.pop.0.4)

summary()



