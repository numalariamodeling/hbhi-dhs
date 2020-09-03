library(data.table)
library(splitstackshape)
MapB_smc <- read.csv("bin/projection/MapB_smc.csv") %>%
  dplyr::select(adm1, LGA = adm2, smc_map_B)
head(MapB_smc)

MapB_smc_repDS <- MapB_smc %>% left_join(rep_DS)
head(MapB_smc_repDS)
summary(MapB_smc_repDS$repDS)

MapB_smc_repDS <- MapB_smc_repDS %>% 
  mutate(peak = if_else(repDS == 'Akinyele', 'may', 
                if_else(repDS == 'Alkaleri', 'july',
                if_else(repDS == 'Babura','may', 
                if_else(repDS =='Birnin Kebbi','august', 
                if_else(repDS =='Darazo','july', if_else(repDS == 'Gawabawa',
                  'august', if_else(repDS =='Giwa', 'august', if_else(repDS =='Gwarzo', 'august',
                   if_else(repDS == 'Isoko South', 'june', if_else(repDS ==
                    'Keana', 'august', if_else(repDS =='Ngor-Okpala', 'june',
                   if_else(repDS=='Obubra','june', if_else(repDS == 'Oke-Ero','july',
                    if_else(repDS =='Oru West', 'june', if_else(repDS ==
              'Owan East','march', if_else(repDS == 'Rimi','march',
               if_else(repDS == 'Soba', 'july',if_else(repDS == 'Takali',
               'september', if_else(repDS =='Taura', 'july', if_else(repDS ==
              'Uzo-Uwani', 'june', '')))))))))))))))))))))

table(MapB_smc_repDS$peak)

MapB_smc_2020 <- MapB_smc_repDS %>% mutate(simday = 
        if_else(peak == 'july',186,if_else(peak == 'may',131,
        if_else(peak == 'august', 223, if_else(peak == 'june',
        162, if_else(peak =='march', 70, if_else(peak =='september',
                    250, NA_real_)))))), year = 2020)

tail(MapB_smc_2020)

MapB_smc_2020_v2 <- expandRows(MapB_smc_2020, count = 4,count.is.col=FALSE, 
                               drop = FALSE) 

MapB_smc_2020_v3<- data.frame(row.names(MapB_smc_2020_v2), 
                              MapB_smc_2020_v2, row.names = NULL)

MapB_smc_2020_v3 <- MapB_smc_2020_v3 %>% 
  group_by(LGA) %>% mutate(round = 
                             rleid(row.names.MapB_smc_2020_v2.))

MapB_smc_2020_v4 <- MapB_smc_2020_v3 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(MapB_smc_2020_v4)

#2021
MapB_smc_2021 <- MapB_smc_2020_v4 %>% mutate(simday = 
                          if_else(peak == 'july',550,if_else(peak == 'may',488,
                          if_else(peak == 'august', 579, if_else(peak == 'june',
                           518, if_else(peak =='march', 426, if_else(peak =='september',
                                      610, NA_real_)))))), year = 2021)

head(MapB_smc_2021)

MapB_smc_2021 <- MapB_smc_2021 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(MapB_smc_2021)


#2022
MapB_smc_2022 <- MapB_smc_2020_v4 %>% mutate(simday = 
                if_else(peak == 'july',915,if_else(peak == 'may',852,
                if_else(peak == 'august', 950, if_else(peak == 'june',
                887, if_else(peak =='march', 796, if_else(peak =='september',
                       978, NA_real_)))))), year = 2022)

head(MapB_smc_2022)

MapB_smc_2022 <- MapB_smc_2022 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(MapB_smc_2022)

#2023 
MapB_smc_2023 <- MapB_smc_2020_v4 %>% mutate(simday = 
          if_else(peak == 'july',1286,if_else(peak == 'may',1223,
           if_else(peak == 'august', 1314, if_else(peak == 'june',
         1251, if_else(peak =='march', 1160, if_else(peak =='september',
            1342, NA_real_)))))), year = 2023)                                               

head(MapB_smc_2023)

MapB_smc_2023 <- MapB_smc_2023 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(MapB_smc_2023)

#2024 
MapB_smc_2024 <- MapB_smc_2020_v4 %>% mutate(simday = 
                  if_else(peak == 'july',1646,if_else(peak == 'may',1587,
                 if_else(peak == 'august', 1678, if_else(peak == 'june',
                 1622, if_else(peak =='march', 1531, if_else(peak =='september',
                 1713, NA_real_)))))), year = 2024)                                               

head(MapB_smc_2024)

MapB_smc_2024 <- MapB_smc_2024 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(MapB_smc_2024)

#2025 
MapB_smc_2025 <- MapB_smc_2020_v4 %>% mutate(simday = 
              if_else(peak == 'july',2014,if_else(peak == 'may',1951,
              if_else(peak == 'august', 2049, if_else(peak == 'june',
               1988, if_else(peak =='march', 1895, if_else(peak =='september',
               2079, NA_real_)))))), year = 2025)                                               

head(MapB_smc_2025)

MapB_smc_2025 <- MapB_smc_2025 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(MapB_smc_2025)


all_smc_B <-do.call("rbind", list(MapB_smc_2020_v4, MapB_smc_2021, 
                                MapB_smc_2022, MapB_smc_2023, MapB_smc_2024,
                                MapB_smc_2025))
summary(all_smc_B$year)
head(all_smc_B)

all_smc_B<- all_smc_B %>%  mutate(duration = -1, coverage_high_access = 0.96,
                              coverage_low_access = 0.24, max_age = 5)

all_smc_B$LGA <- gsub("\\/", "-", all_smc_B$LGA)

write.csv(all_smc_B, 'results/archetype_sim_input/Intervention_files_LGA/all_smc_mapB.csv')

