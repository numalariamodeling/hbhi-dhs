setwd('C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2025_v6/NGA projection scenario 6')

burden <- read.csv('malariaBurden_withAdjustments.csv') %>%  filter(year <2026) %>%  
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

colnames(adamawa)<- c('State', 'LGA', 'year', 'Run_Number', 'Statistical_Population', 'New_Clinical_Cases', 'PfPR_MiP_adjusted', 'mLBW_births', 'MiP_stillbirths',
                      'total_mortality_1', 'total_mortality_2', 'Pop_U5', 'PfPR_U5', 'New_clinical_cases_U5', 'total_mortality_U5_1',
                      'total_mortality_U5_2')

pop <- read.csv('nigeria_LGA_pop.csv')
head(pop)

adamawa_2 <- anti_join(adamawa, pop, by = "LGA")

head(adamawa_2)


adamawa_3 <- adamawa_2 