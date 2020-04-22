setwd('C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2025_v6')
burden <- read.csv('NGA projection scenario 7/malariaBurden_withAdjustments.csv') %>%  filter(year <2026) %>%  
                    dplyr::select(year, LGA, Run_Number,Statistical_Population, New_Clinical_Cases, PfPR_MiP_adjusted,
                           mLBW_births, MiP_stillbirths, total_mortality_1, total_mortality_2,
                           Pop_U5, PfPR_U5, New_clinical_cases_U5, total_mortality_U5_1, total_mortality_U5_2)
head(burden)
summary(burden$year)
summary(is.na(burden$Received_Severe_Treatment))


burden_v2 <- burden %>%group_by(year, LGA, Run_Number) %>% 
                        summarise(mean(Statistical_Population),
                                  sum(New_Clinical_Cases), mean(PfPR_MiP_adjusted), sum(mLBW_births),
                                  sum(MiP_stillbirths), sum(total_mortality_1), sum(total_mortality_2),
                                  mean(Pop_U5), mean(PfPR_U5), sum(New_clinical_cases_U5), sum(total_mortality_U5_1),
                                  sum(total_mortality_U5_2)) %>% ungroup()
head(burden_v2)

burden_v3 <- burden_v2 %>% group_by(year, LGA) %>%  
                  summarise_all(mean) %>% ungroup()  

head(burden_v3)

adamawa_lga <- read.csv('Adamawa_LGA.csv')

adamawa <- left_join(adamawa_lga, burden_v3, by = 'LGA')
head(adamawa)

colnames(adamawa)<- c('State', 'LGA','year', 'Run_Number', 'Statistical_Population', 'New_Clinical_Cases', 'PfPR_MiP_adjusted', 'mLBW_births', 'MiP_stillbirths',
                      'total_mortality_1', 'total_mortality_2', 'Pop_U5', 'PfPR_U5', 'New_clinical_cases_U5', 'total_mortality_U5_1',
                      'total_mortality_U5_2')

pop <- read.csv('nigeria_LGA_pop.csv')
head(pop)

df <- left_join(adamawa, pop, by = "LGA")

head(df)



df$cases <- df$New_Clinical_Cases *df$geopode.pop/df$Statistical_Population
df$deaths_1 <- df$total_mortality_1* df$geopode.pop/df$Statistical_Population
df$deaths_2 <- df$total_mortality_2*df$geopode.pop/df$Statistical_Population
df$geopode.pop_U5 <- df$geopode.pop * df$Pop_U5/df$Statistical_Population
df$cases_U5 <- df$New_clinical_cases_U5 *df$geopode.pop_U5/df$Pop_U5
df$deaths_U5_1 <- df$total_mortality_U5_1*df$geopode.pop_U5/df$Pop_U5
df$deaths_U5_2 <- df$total_mortality_U5_2*df$geopode.pop_U5/df$Pop_U5



head(LGA_results)

LGA_results <- df %>% dplyr::select(LGA, cases, deaths_1, deaths_2, cases_U5, deaths_U5_1, deaths_U5_2)%>% 
                  group_by(LGA) %>% summarise_all(sum)%>%  mutate(mean_death = (deaths_1 + deaths_2)/2,
                                                              mean_death_U5 = (deaths_U5_1 + deaths_U5_2)/2) %>% 
                dplyr::select(LGA, cases, deaths_1, deaths_2, mean_death, cases_U5, 
                              deaths_U5_1, deaths_U5_2, mean_death_U5)

state_results <- df %>%  dplyr::select(cases, deaths_1, deaths_2, cases_U5, deaths_U5_1, deaths_U5_2)  %>% 
                         summarise_all(sum) %>%  mutate(mean_death = (deaths_1 + deaths_2)/2,
                        mean_death_U5 = (deaths_U5_1 + deaths_U5_2)/2) %>% 
                       dplyr::select(cases, mean_death, cases_U5, mean_death_U5)





write.csv(state_results, "Adamawa_scenario 7.csv")
