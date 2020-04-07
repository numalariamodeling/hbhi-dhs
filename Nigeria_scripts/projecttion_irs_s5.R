library(data.table)
library(splitstackshape)
s7_irs <- read.csv("bin/projection/s7/irs_s7.csv") 

head(s7_irs)

s7_irs_repDS <- s7_irs%>% left_join(rep_DS)
tail(s7_irs_repDS)
summary(s7_irs_repDS$repDS)

s7_irs_repDS <- s7_irs_repDS %>% 
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

table(s7_irs_repDS$peak)

s7_irs_2020 <-  s7_irs_repDS %>% 
  mutate(simday = if_else(peak == 'july',186,
                  if_else(peak == 'may',131,
                  if_else(peak == 'august', 223, 
                  if_else(peak == 'june', 162, 
                  if_else(peak =='march', 70, 
                  if_else(peak =='september',250, NA_real_)))))), 
         year = 2020, coverage = 0.6, initial_killing = 0.7,
         box_duration = 180, decay_time_constant = 90)

tail(s7_irs_2020)
summary(s7_irs_2020$simday)

#2021
s7_irs_2021 <- s7_irs_2020 %>% mutate(simday = 
               if_else(peak == 'july',550,
               if_else(peak == 'may',488,
               if_else(peak == 'august', 579, 
               if_else(peak == 'june',518, 
               if_else(peak =='march', 426, 
               if_else(peak =='september',610, NA_real_)))))), 
               year = 2021)

head(s7_irs_2021)


#2022
s7_irs_2022 <- s7_irs_2020 %>% mutate(simday = 
              if_else(peak == 'july',915,if_else(peak == 'may',852,
              if_else(peak == 'august', 950, if_else(peak == 'june',887, 
              if_else(peak =='march', 796, if_else(peak =='september',
              978, NA_real_)))))), year = 2022)

head(s7_irs_2022)


#2023 
s7_irs_2023 <- s7_irs_2020 %>% mutate(simday = 
              if_else(peak == 'july',1286,
              if_else(peak == 'may',1223,
              if_else(peak == 'august', 950, 
              if_else(peak == 'june',1314, 
              if_else(peak =='march', 1160, 
              if_else(peak =='september', 1342, NA_real_)))))), 
              year = 2023)                                               

head(s7_irs_2023)



#2024 
s7_irs_2024 <- s7_irs_2020 %>% mutate(simday = 
              if_else(peak == 'july',1646,
              if_else(peak == 'may',1587,
              if_else(peak == 'august', 1678, 
              if_else(peak == 'june',1622, 
              if_else(peak =='march', 1531, 
              if_else(peak =='september',1713, NA_real_)))))), 
              year = 2024)                                               

head(s7_irs_2024)



#2025 
s7_irs_2025 <- s7_irs_2020 %>% mutate(simday = 
              if_else(peak == 'july',2014,
              if_else(peak == 'may',1951,
              if_else(peak == 'august', 2049, 
              if_else(peak == 'june',1988, 
              if_else(peak =='march', 1895, 
              if_else(peak =='september',2079, NA_real_)))))), 
              year = 2025)                                               

head(s7_irs_2025)


all_irs <-do.call("rbind", list(s7_irs_2020, s7_irs_2021, 
                                s7_irs_2022, s7_irs_2023, s7_irs_2024,
                                s7_irs_2025))
summary(all_irs$year)
head(all_irs)

all_irs$LGA <- gsub("\\/", "-", all_irs$LGA)

write.csv(all_irs, 'results/archetype_sim_input/Intervention_files_LGA/irs_s7.csv')





