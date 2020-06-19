library(data.table)
library(splitstackshape)
MapA_smc <- read.csv("bin/projection/MapA_smc.csv") %>%
              dplyr::select(adm1, LGA = adm2, smc_map_A)
head(MapA_smc)

MapA_smc_repDS <- MapA_smc %>% left_join(rep_DS)
head(MapA_smc_repDS)
length(MapA_smc_repDS$repDS)

MapA_smc_repDS <- MapA_smc_repDS %>% 
   mutate(peak = if_else(repDS == 'Akinyele', 'may', 
   if_else(repDS == 'Alkaleri', 'july',if_else(repDS == 'Babura','may', 
    if_else(repDS =='Birnin Kebbi','august', if_else(repDS =='Darazo',
    'july', if_else(repDS == 'Gawabawa','august', if_else(repDS ==
      'Giwa', 'august', if_else(repDS =='Gwarzo', 'august',
      if_else(repDS == 'Isoko South', 'june', if_else(repDS ==
       'Keana', 'august', if_else(repDS =='Ngor-Okpala', 'june',
        if_else(repDS=='Obubra','june', if_else(repDS == 'Oke-Ero','july',
        if_else(repDS =='Oru West', 'june', if_else(repDS ==
        'Owan East','march', if_else(repDS == 'Rimi','march',
        if_else(repDS == 'Soba', 'july', if_else(repDS == 'Takali',
        'september', if_else(repDS =='Taura', 'july', if_else(repDS ==
          'Uzo-Uwani', 'june', '')))))))))))))))))))))
head(MapA_smc_repDS)

MapA_smc_2020 <- MapA_smc_repDS %>% mutate(simday = 
            if_else(peak == 'july',186,if_else(peak == 'may',131,
          if_else(peak == 'august', 223, if_else(peak == 'june',
          162, if_else(peak =='march', 70, if_else(peak =='september',
           250, NA_real_)))))), year = 2020)

tail(MapA_smc_2020)

MapA_smc_2020_v2 <- expandRows(MapA_smc_2020, count = 4,count.is.col=FALSE, 
                               drop = FALSE) 

MapA_smc_2020_v3<- data.frame(row.names(MapA_smc_2020_v2), 
                              MapA_smc_2020_v2, row.names = NULL)

MapA_smc_2020_v3 <- MapA_smc_2020_v3 %>% 
                    group_by(LGA) %>% mutate(round = 
                                  rleid(row.names.MapA_smc_2020_v2.))

MapA_smc_2020_v4 <- MapA_smc_2020_v3 %>% 
                  mutate(simday = if_else(round == 2, simday + 30,
                    if_else(round == 3, simday + 60, 
                    if_else(round ==4, simday + 90, simday))))

head(MapA_smc_2020_v4)

#2021
MapA_smc_2021 <- MapA_smc_2020_v4 %>% mutate(simday = 
          if_else(peak == 'july',550,if_else(peak == 'may',488,
           if_else(peak == 'august', 579, if_else(peak == 'june',
            518, if_else(peak =='march', 426, if_else(peak =='september',
              610, NA_real_)))))), year = 2021)

head(MapA_smc_2021)

MapA_smc_2021 <- MapA_smc_2021 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                  if_else(round == 3, simday + 60, 
                  if_else(round ==4, simday + 90, simday))))

head(MapA_smc_2021)


#2022
MapA_smc_2022 <- MapA_smc_2020_v4 %>% mutate(simday = 
               if_else(peak == 'july',915,if_else(peak == 'may',852,
            if_else(peak == 'august', 950, if_else(peak == 'june',
        887, if_else(peak =='march', 796, if_else(peak =='september',
     978, NA_real_)))))), year = 2022)

head(MapA_smc_2022)

MapA_smc_2022 <- MapA_smc_2022 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                  if_else(round == 3, simday + 60, 
                      if_else(round ==4, simday + 90, simday))))

head(MapA_smc_2022)

#2023 
MapA_smc_2023 <- MapA_smc_2020_v4 %>% mutate(simday = 
  if_else(peak == 'july',1286,if_else(peak == 'may',1223,
    if_else(peak == 'august', 1314, if_else(peak == 'june',
    1251, if_else(peak =='march', 1160, if_else(peak =='september',
                    1342, NA_real_)))))), year = 2023)                                               

head(MapA_smc_2023)

MapA_smc_2023 <- MapA_smc_2023 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
  if_else(round == 3, simday + 60, 
  if_else(round ==4, simday + 90, simday))))

head(MapA_smc_2023)

#2024 
MapA_smc_2024 <- MapA_smc_2020_v4 %>% mutate(simday = 
        if_else(peak == 'july',1646,if_else(peak == 'may',1587,
        if_else(peak == 'august', 1678, if_else(peak == 'june',
         1622, if_else(peak =='march', 1531, if_else(peak =='september',
            1713, NA_real_)))))), year = 2024)                                               

head(MapA_smc_2024)

MapA_smc_2024 <- MapA_smc_2024 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                  if_else(round == 3, simday + 60, 
                        if_else(round ==4, simday + 90, simday))))

head(MapA_smc_2024)

#2025 
MapA_smc_2025 <- MapA_smc_2020_v4 %>% mutate(simday = 
        if_else(peak == 'july',2014,if_else(peak == 'may',1951,
      if_else(peak == 'august', 2049, if_else(peak == 'june',
        1988, if_else(peak =='march', 1895, if_else(peak =='september',
               2079, NA_real_)))))), year = 2025)                                               

head(MapA_smc_2025)

MapA_smc_2025 <- MapA_smc_2025 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
              if_else(round == 3, simday + 60, 
                    if_else(round ==4, simday + 90, simday))))

head(MapA_smc_2025)


all_smc <-do.call("rbind", list(MapA_smc_2020_v4, MapA_smc_2021, 
                      MapA_smc_2022, MapA_smc_2023, MapA_smc_2024,
                      MapA_smc_2025))
summary(all_smc$year)
head(all_smc)

all_smc$LGA <- gsub("\\/", "-", all_smc$LGA)

all_smc<- all_smc %>%  mutate(duration = -1, coverage_high_access = 0.96,
                                     coverage_low_access = 0.24, max_age = 5)

write.csv(all_smc, 'results/archetype_sim_input/Intervention_files_LGA/all_smc_MapA 20 percent.csv')

