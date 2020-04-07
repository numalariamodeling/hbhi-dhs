library(data.table)
library(splitstackshape)
cc_smc <- read.csv("bin/projection/s1/smc_LGA.csv")
table(cc_smc$State)

cc_smc_repDS <- cc_smc %>% left_join(rep_DS)
table(cc_smc_repDS$repDS)
summary(rep_DS$LGA)

cc_smc_repDS <- cc_smc_repDS %>% 
  mutate(peak = if_else(repDS == 'Akinyele', 'may', 
    if_else(repDS == 'Alkaleri', 'july',
    if_else(repDS == 'Babura','may', 
    if_else(repDS =='Birnin Kebbi','august', 
    if_else(repDS =='Darazo','july', 
    if_else(repDS == 'Gawabawa', 'august', 
    if_else(repDS =='Giwa', 'august', 
    if_else(repDS =='Gwarzo', 'august',
    if_else(repDS == 'Isoko South', 'june', 
    if_else(repDS =='Keana', 'august', 
    if_else(repDS =='Ngor-Okpala', 'june',
    if_else(repDS =='Nsit Ubium', 'june',
    if_else(repDS=='Obubra','june', 
    if_else(repDS == 'Oke-Ero','july',
    if_else(repDS =='Oru West', 'june', 
    if_else(repDS =='Owan East','march', 
    if_else(repDS == 'Rimi','march',
    if_else(repDS == 'Soba', 'july',
    if_else(repDS == 'Takali','september', 
    if_else(repDS =='Taura', 'july', 
    if_else(repDS =='Uzo-Uwani', 'june', ''))))))))))))))))))))))                                                                                                                                                                                                                                                                                                                    

table(cc_smc_repDS$peak) 

cc_smc_2020 <- cc_smc_repDS %>% 
mutate(simday = if_else(peak == 'july',186,
                if_else(peak == 'may',131,
                if_else(peak == 'august', 223, 
                if_else(peak == 'june', 162, 
                if_else(peak =='march', 70, 
                if_else(peak =='september',250, NA_real_)))))), 
                 year = 2020)

tail(cc_smc_2020)

cc_smc_2020_v2 <- expandRows(cc_smc_2020, count = 4,count.is.col=FALSE, 
                    drop = FALSE)

cc_smc_2020_v3<- data.frame(row.names(cc_smc_2020_v2), 
                            cc_smc_2020_v2, row.names = NULL)


cc_smc_2020_v3 <- cc_smc_2020_v3 %>% 
  group_by(LGA) %>% mutate(round = 
                             rleid(row.names.cc_smc_2020_v2.))

cc_smc_2020_v4 <- cc_smc_2020_v3 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                  if_else(round == 3, simday + 60, 
                  if_else(round ==4, simday + 90, simday))))

head(cc_smc_2020_v4)

#2021
cc_smc_2021 <- cc_smc_2020_v4 %>% mutate(simday = 
              if_else(peak == 'july',550,if_else(peak == 'may',488,
              if_else(peak == 'august', 579, if_else(peak == 'june',
               518, if_else(peak =='march', 426, if_else(peak =='september',
                  610, NA_real_)))))), year = 2021)

head(cc_smc_2021)

cc_smc_2021 <- cc_smc_2021 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
        if_else(round == 3, simday + 60, 
                if_else(round ==4, simday +90, simday))))

head(cc_smc_2021)


#2022
cc_smc_2022 <- cc_smc_2020_v4 %>% mutate(simday = 
              if_else(peak == 'july',915,if_else(peak == 'may',852,
               if_else(peak == 'august', 950, if_else(peak == 'june',
                 887, if_else(peak =='march', 796, if_else(peak =='september',
                 978, NA_real_)))))), year = 2022)

head(cc_smc_2022)

cc_smc_2022 <- cc_smc_2022 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
        if_else(round == 3, simday + 60, 
        if_else(round ==4, simday + 90, simday))))

head(cc_smc_2022)

#2023 
cc_smc_2023 <- cc_smc_2020_v4 %>% mutate(simday = 
            if_else(peak == 'july',1286,if_else(peak == 'may',1223,
            if_else(peak == 'august', 1314, if_else(peak == 'june',
             1251, if_else(peak =='march', 1160, if_else(peak =='september',
               1342, NA_real_)))))), year = 2023)                                               

head(cc_smc_2023)

cc_smc_2023 <- cc_smc_2023 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                    if_else(round == 3, simday + 60, 
                        if_else(round ==4, simday + 90, simday))))

head(cc_smc_2023)

#2024 
cc_smc_2024 <- cc_smc_2020_v4 %>% mutate(simday = 
              if_else(peak == 'july',1646,if_else(peak == 'may',1587,
              if_else(peak == 'august', 1678, if_else(peak == 'june',
               1622, if_else(peak =='march', 1531, if_else(peak =='september',
                               1713, NA_real_)))))), year = 2024)                                               

head(cc_smc_2024)

cc_smc_2024 <- cc_smc_2024 %>% 
  mutate(simday = if_else(round == 2, simday +30,
            if_else(round == 3, simday + 60, 
                    if_else(round ==4, simday + 90, simday))))

head(cc_smc_2024)

#2025 
cc_smc_2025 <- cc_smc_2020_v4 %>% mutate(simday = 
            if_else(peak == 'july',2014,if_else(peak == 'may',1951,
            if_else(peak == 'august', 2049, if_else(peak == 'june',
                    1988, if_else(peak =='march', 1895, if_else(peak =='september',
                            2079, NA_real_)))))), year = 2025)                                               

head(cc_smc_2025)

cc_smc_2025 <- cc_smc_2025 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(cc_smc_2025)


all_smc_cc <-do.call("rbind", list(cc_smc_2020_v4, cc_smc_2021, 
                      cc_smc_2022, cc_smc_2023, cc_smc_2024,
                                  cc_smc_2025))
summary(all_smc_cc$year)
head(all_smc_cc)

all_smc_cc$LGA <- gsub("\\/", "-", all_smc_cc$LGA)

all_smc_cc <- all_smc_cc %>%  mutate(duration = -1, coverage_high_access = 0.8,
                                     coverage_low_access = 0.2, max_age = 5  )

write.csv(all_smc_cc, 'results/archetype_sim_input/Intervention_files_LGA/smc_cc_v3.csv')

