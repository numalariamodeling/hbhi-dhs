library(data.table)
library(splitstackshape)
cc_smc <- read.csv("bin/projection/s1/smc_LGA.csv")



cc_smc_2020 <- cc_smc %>% 
mutate(simday = if_else(month_new == 7 & round == 1,186,
                if_else(month_new == 8 & round == 1, 223, NA_real_)), 
                 year = 2020)

while(length(ind <- which(is.na(cc_smc_2020$simday))) > 0){
  cc_smc_2020$simday[ind] <- cc_smc_2020$simday[ind -1]
}

cc_smc_2020_v2 <- cc_smc_2020 %>% 
  mutate(simday = if_else(round == 2, simday + 28,
                  if_else(round == 3, simday + 56, 
                  if_else(round ==4, simday + 84, simday))))



#2021
cc_smc_2021 <-  cc_smc %>% mutate(simday = 
              if_else(month_new == 7 & round == 1, 550,
                      if_else(month_new == 8 & round == 1,488,NA_real_)), year = 2021)



while(length(ind <- which(is.na(cc_smc_2021$simday))) > 0){
  cc_smc_2021$simday[ind] <- cc_smc_2021$simday[ind -1]
}

cc_smc_2021 <- cc_smc_2021 %>% 
  mutate(simday = if_else(round == 2, simday + 28,
        if_else(round == 3, simday + 56, 
                if_else(round ==4, simday +84, simday))))




#2022
cc_smc_2022 <- cc_smc  %>% mutate(simday = 
                                    if_else(month_new == 7 & round == 1, 915,
                                            if_else(month_new == 8 & round == 1,950,NA_real_)), year = 2022)

while(length(ind <- which(is.na(cc_smc_2022$simday))) > 0){
  cc_smc_2022$simday[ind] <- cc_smc_2022$simday[ind -1]
}

cc_smc_2022 <- cc_smc_2022 %>% 
  mutate(simday = if_else(round == 2, simday + 28,
        if_else(round == 3, simday + 56, 
        if_else(round ==4, simday + 84, simday))))

#2023 
cc_smc_2023 <- cc_smc %>% mutate(simday = 
                                   if_else(month_new == 7 & round == 1, 1286,
                                           if_else(month_new == 8 & round == 1,1314,NA_real_)), year = 2023)    

while(length(ind <- which(is.na(cc_smc_2023$simday))) > 0){
  cc_smc_2023$simday[ind] <- cc_smc_2023$simday[ind -1]
}                                

cc_smc_2023 <- cc_smc_2023 %>% 
  mutate(simday = if_else(round == 2, simday + 28,
                    if_else(round == 3, simday + 56, 
                        if_else(round ==4, simday + 84, simday))))

#2024 
cc_smc_2024 <- cc_smc %>% mutate(simday = 
                                           if_else(month_new == 7 & round == 1, 1646,
                                                   if_else(month_new == 8 & round == 1,1678,NA_real_)), year = 2024)     

while(length(ind <- which(is.na(cc_smc_2024$simday))) > 0){
  cc_smc_2024$simday[ind] <- cc_smc_2024$simday[ind -1]
}   

cc_smc_2024 <- cc_smc_2024 %>% 
  mutate(simday = if_else(round == 2, simday +28,
            if_else(round == 3, simday + 56, 
                    if_else(round ==4, simday + 84, simday))))


#2025 
cc_smc_2025 <- cc_smc %>% mutate(simday = 
                                           if_else(month_new == 7 & round == 1, 2014,
                                                   if_else(month_new == 8 & round == 1,2049,NA_real_)), year = 2025)   

while(length(ind <- which(is.na(cc_smc_2025$simday))) > 0){
  cc_smc_2025$simday[ind] <- cc_smc_2025$simday[ind -1]
} 
                                           
cc_smc_2025 <- cc_smc_2025 %>% 
  mutate(simday = if_else(round == 2, simday + 28,
                          if_else(round == 3, simday + 56, 
                                  if_else(round ==4, simday + 84, simday))))




all_smc_cc <-do.call("rbind", list(cc_smc_2020_v2, cc_smc_2021, 
                      cc_smc_2022, cc_smc_2023, cc_smc_2024,
                                  cc_smc_2025))
summary(all_smc_cc$year)
head(all_smc_cc)

# all_smc_cc$LGA <- gsub("\\/", "-", all_smc_cc$LGA)
# 
# all_smc_cc <- all_smc_cc %>%  mutate(duration = -1, coverage_high_access = 0.8,
#                                      coverage_low_access = 0.2, max_age = 5  )

write.csv(all_smc_cc, 'results/archetype_sim_input/Intervention_files_LGA/smc_cc_v4.csv')

