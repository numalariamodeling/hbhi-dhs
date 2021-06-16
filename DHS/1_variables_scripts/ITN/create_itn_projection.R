library(data.table)
library(splitstackshape)

itn_dis <- read.csv("bin/ITN_distribution/itn_distr_by_LGA_and_month.csv") %>% 
              dplyr::select(LGA, year) %>% mutate(value = 1)
head(itn_dis)

itn_dis_1 <- itn_dis %>% 
  pivot_wider(names_from = year, values_from = value) %>% 
  dplyr::select(LGA, year_10 = `2010`, year_13 = `2013`, year_14 = `2014`,
         year_15 =`2015`, year_16 = `2016`, year_17 = `2017`,
         year_18 = `2018`, year_19 = `2019`) 

rep_DS_dist <- rep_DS %>% left_join(itn_dis_1) %>%  mutate(year = ifelse(is.na(year_19)& is.na(year_18)& is.na(year_17),2021,
                                                    ifelse(year_17 == 1 & is.na(year_18)& is.na(year_19), 2021, 
                                                   ifelse(year_18 == 1 & is.na(year_19), 2021,ifelse(year_19 == 1, 2022,NA)))))

#write.csv(rep_DS_dist, 'bin/ITN_distribution/itn_distr_by_LGA_and_monthv2.csv')

#itn_dis_2 <- read.csv("bin/ITN_distribution/itn_distr_by_LGA_and_monthv2.csv") %>% 
                      dplyr::select(-X)

#itn_dis_2$LGA <- gsub("\\/", "-", itn_dis_2$LGA)

itn_cov <- read.csv("bin/ITN_distribution/ITN_by_LGA_current_coverage.csv") %>%  
                      mutate(LGA = ifelse(LGA == "kiyawa", "Kiyawa", ifelse(LGA == "kaita", "Kaita", as.character(LGA))))

itn_dis_2 <- rep_DS_dist %>% left_join(itn_cov)

itn_dis_2 <- expandRows(itn_dis_2, count = 2,count.is.col=FALSE, 
                        drop = FALSE) 


itn_dis_2_v1<- data.frame(row.names(itn_dis_2), 
                          itn_dis_2, row.names = NULL)

itn_dis_2_v1 <- itn_dis_2_v1 %>% 
  group_by(LGA) %>% mutate(round = rep(c(1,2), times = 774))

itn_dis_2_v1 <- itn_dis_2_v1 %>% 
                 mutate(year = ifelse(round == 2, year + 3,year))

head(itn_dis_2_v1)

itn_dis_2_v1 <- itn_dis_2_v1 %>% 
      mutate(peak = ifelse(repDS == 'Ahoada West', 'may',
    if_else(repDS == 'Akinyele', 'may',if_else(repDS == 'Alkaleri', 
     'july',if_else(repDS == 'Babura','may',if_else(repDS =='Birnin Kebbi',
      'august', if_else(repDS =='Darazo', 'july', 
    if_else(repDS == 'Gawabawa','august', if_else(repDS ==
    'Giwa', 'august', if_else(repDS =='Gwarzo', 'august',
    if_else(repDS == 'Isoko South', 'june', if_else(repDS =='Keana',
    'august', if_else(repDS =='Ngor-Okpala', 'june', 
  if_else(repDS =='Nsit Ubium','june',
  if_else(repDS=='Obubra','june', if_else(repDS == 'Oke-Ero','july',
  if_else(repDS =='Oru West', 'june', if_else(repDS =='Owan East',
   'march', if_else(repDS == 'Rimi','march',
    if_else(repDS == 'Soba', 'july', if_else(repDS == 'Takali',
    'september', if_else(repDS =='Taura', 'july', if_else(repDS ==
     'Uzo-Uwani', 'june', '')))))))))))))))))))))))

table(itn_dis_2_v1$peak)

itn_dis_2_v1 <- itn_dis_2_v1 %>% mutate(simday = 
          if_else(peak == 'july',186,if_else(peak == 'may',131,
        if_else(peak == 'august', 223, if_else(peak == 'june',
        162, if_else(peak =='march', 131, if_else(peak =='september',
      250, NA_real_)))))))

table(itn_dis_2_v1$simday)

itn_dis_2_v2 <- itn_dis_2_v1 %>%mutate(simday = ifelse(year == 2021, simday + 365,
                            ifelse(year == 2022, simday + (365*2),
                            ifelse(year == 2023, simday + (365*3),
                            ifelse(year == 2024, simday + (365*4),
                            ifelse(year == 2025, simday + (365*5), simday))))))
head(itn_dis_2_v2)         

write.csv(itn_dis_2_v2, 'results/archetype_sim_input/Intervention_files_LGA/ITN/itn_scenario1_v3_2021.csv')