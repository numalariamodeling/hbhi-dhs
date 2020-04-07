smc_cc <- read.csv('bin/projection/s2_v2/smc_cc_v3.csv')
head(smc_cc)

smc_scen_2 <- read.csv('bin/projection/s2_v2/smc_scenario2.csv') %>% dplyr::rename(LGA = adm2)
smc_scen_2$LGA <- gsub("\\/", "-", smc_scen_2$LGA)

head(smc_scen_2)

rep_DS$LGA <- gsub("\\/", "-", rep_DS$LGA)

rep_DS <- rep_DS %>% mutate(LGA = str_to_title(LGA))

smc_2 <- smc_scen_2 %>%mutate(LGA = str_to_title(LGA)) %>% left_join(rep_DS) 
                    
head(smc_2)

table(smc_2$repDS)

repDS <- unique(as.character(smc_2$repDS))
repDS


peak <- c('july', 'july', 'august', 'august', 'july', 'september', 'august', 'july', 'march', 'may',
          'august', 'june', 'july', 'june', 'march', 'august', 'june')

df <- tibble(repDS, peak)

smc_2 <- smc_2 %>% left_join(df)
head(smc_2)

#giving march smc and may smc the same timing
smc_2020 <- smc_2 %>% mutate(simday = if_else(peak == 'july',186,if_else(peak == 'may',131,
                                            if_else(peak == 'august', 223, if_else(peak == 'june',162, 
                                             if_else(peak =='march', 131, if_else(peak =='september',
                                                               250, NA_real_)))))), year = 2020)

tail(smc_2020)

smc_2020 <- expandRows(smc_2020, count = 4,count.is.col=FALSE, 
                               drop = FALSE) 

smc_2020 <- smc_2020 %>% 
  group_by(LGA) %>% mutate(round = rep(c(1,2,3,4), times = 395))

head(smc_2020, 20)

smc_2020 <- smc_2020 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(smc_2020)

#2021
smc_2021 <- smc_2020 %>% mutate(simday = if_else(peak == 'july',550,if_else(peak == 'may',488,
                                                if_else(peak == 'august', 579, if_else(peak == 'june',518, 
                                                  if_else(peak =='march', 426, if_else(peak =='september',
                                                      610, NA_real_)))))), year = 2021)


head(smc_2021)

smc_2021 <- smc_2021 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(smc_2021)


#2022
smc_2022 <- smc_2020 %>% mutate(simday = if_else(peak == 'july',915,if_else(peak == 'may',852,
                                            if_else(peak == 'august', 950, if_else(peak == 'june',
                                                887, if_else(peak =='march', 796, if_else(peak =='september',
                                                      978, NA_real_)))))), year = 2022)

head(smc_2022)

smc_2022 <- smc_2022 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(smc_2022)

#2023 
smc_2023 <- smc_2020 %>% mutate(simday = if_else(peak == 'july',1286,if_else(peak == 'may',1223,
                                    if_else(peak == 'august', 1314, if_else(peak == 'june',
                                        1251, if_else(peak =='march', 1160, if_else(peak =='september',
                                                        1342, NA_real_)))))), year = 2023)                                               

head(smc_2023)

smc_2023 <- smc_2023 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(smc_2023)

#2024 
smc_2024 <- smc_2020 %>% mutate(simday = if_else(peak == 'july',1646,if_else(peak == 'may',1587,
                                             if_else(peak == 'august', 1678, if_else(peak == 'june',
                                              1622, if_else(peak =='march', 1531, if_else(peak =='september',
                                                  1713, NA_real_)))))), year = 2024)                                               

head(smc_2024)

smc_2024 <- smc_2024 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(smc_2024)

#2025 
smc_2025 <- smc_2020 %>% mutate(simday = if_else(peak == 'july',2014,if_else(peak == 'may',1951,
                                      if_else(peak == 'august', 2049, if_else(peak == 'june',
                                        1988, if_else(peak =='march', 1895, if_else(peak =='september',
                                                2079, NA_real_)))))), year = 2025)                                               

head(smc_2025)

smc_2025 <- smc_2025 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(smc_2025)


all_smc <-do.call("rbind", list(smc_2020, smc_2021, 
                                smc_2022, smc_2023, smc_2024,
                                smc_2025))
summary(all_smc$year)
head(all_smc)

all_smc$LGA <- gsub("\\/", "-", all_smc$LGA)

all_smc<- all_smc %>%  mutate(duration = -1, coverage_high_access = 1,
                              coverage_low_access = 0.80, max_age = 5)

write.csv(all_smc, 'results/archetype_sim_input/Intervention_files_LGA/smc_scen2.csv')


