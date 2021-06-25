##----------------------------------
## old code for cm file 
##----------------------------------

# cm_2 <- read.csv(paste0(SimInDir, "/HS_placeholder.csv"))
# 
# summary(cm_2$U5_coverage)
# summary(cm_2$adult_coverage)
# 
# cm_3 <- expandRows(cm_2, count = 13,count.is.col=FALSE, 
#                         drop = FALSE) 
# head(cm_3)
# 
# lookup_key <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
# 
# cm_3 <- cm_3 %>% mutate(round = 
#                              rep(seq(lookup_key), times =774), severe_cases = 0.49, start_day_override = 1) 
# head(cm_3)
# 
# duration <- c(196, 154, 212, 154,202, 159, 219, 136,218, 151, 185, 183,-1)
# year_sim <- c(2020, 2020, 2021,2021,2022,2022,2023,2023,2024,2024,2025,2025,2025)
# sim_day <- c(0, 196, 350, 562, 716, 918, 1077, 1296, 1432, 1650,1801, 1986,0)
# 
# scale_med <- c(1, 2, 5, 15, 20, 40, 45, 50, 55, 60, 65, 65,65)
# 
# # scale factors 
# scale_med_2 <- c(1, 2.05, 2.06, 3.07, 3.08, 3.09, 4.1, 4.12, 4.5, 5,5.5, 6, 6)
# 
# 
# scale_med_3 <- c(1, 1.05, 1.06, 1.07, 1.08, 1.09, 1.1, 1.12, 1.5, 2,2.5, 3, 3)
# 
# scale_severe <- c(1, 1.1, 1.2, 1.3, 1.4, 1.5, 1.6, 1.7, 1.8, 1.9, 2, 2.1,2.1) #need to fix the scaling and reduce to 12 so it stops giving clunky results> 13 number was 2.2
# 
# df <- tibble(lookup_key,sim_day,year_sim,duration)
# head(df, 13)
# 
# 
# cm_3$duration[cm_3$round %in% df$lookup_key == TRUE] <- 
#   duration[df$lookup_key %in% cm_3$round == TRUE]
# 
# 
# cm_3$year[cm_3$round %in% df$lookup_key == TRUE] <- 
#   year_sim[df$lookup_key %in% cm_3$round == TRUE]
# 
# cm_3$simday[cm_3$round %in% df$lookup_key == TRUE] <- 
#  sim_day[df$lookup_key %in% cm_3$round == TRUE]
# 
# #creates max multiplication values 
# 
# max_v <- 0.80
# 
# # scaling (assumption is that case management scaling is focused on the worst areas)
# cm_3 <- cm_3 %>% mutate(U5_coverage = ifelse(U5_coverage >=0.01398 & U5_coverage <0.15493
#                                              & round %in% df$lookup_key, pmin(U5_coverage * scale_med, 
#                                                                               max_v), U5_coverage))
#  
# cm_4 <- cm_3 %>% mutate(U5_coverage = ifelse(U5_coverage >= 0.15493 & U5_coverage <0.27773
#                                              & round %in% df$lookup_key, pmin(U5_coverage * scale_med_2, 
#                                                                               max_v), U5_coverage))
# 
# cm_5 <- cm_4 %>% mutate(U5_coverage = ifelse(U5_coverage >= 0.27773 & U5_coverage <0.80
#                                              & round %in% df$lookup_key, 
#                                              pmin(U5_coverage * scale_med_3, max_v), U5_coverage), 
#                         adult_coverage = U5_coverage)
# 
# cm_5 <- cm_5 %>% mutate(severe_cases = pmin(severe_cases * scale_severe, max_v))
# 
# head(cm_5, 15)
# 
# write.csv(cm_5, paste0(path,'/HS_placeholder_scen2_80_v4.csv'))
# 
# 
# # increase cm coverage by 10% 
# cm_2 <- read.csv('bin/projection/s2_v2/HS_placeholder_v2.csv')
# head(cm_2)
# 
# cm_2 <- cm_2 %>% mutate(U5_coverage = ifelse(U5_coverage < 0.80, pmin(U5_coverage + (U5_coverage *0.10), max_v),U5_coverage),
#                         adult_coverage = U5_coverage) 
# head(cm_2)
# 
# summary(cm_2$U5_coverage)
# 
# 
# write.csv(cm_2, 'results/archetype_sim_input/Intervention_files_LGA/case_management/cm_scen2_10.csv')
# 
# 
# # increase cm coverage by 20% 
# cm_2 <- read.csv('bin/projection/s2_v2/HS_placeholder_v2.csv')
# head(cm_2)
# summary(cm_2$U5_coverage)
# 
# cm_2 <- cm_2 %>% mutate(U5_coverage = ifelse(U5_coverage <0.80, pmin(U5_coverage + (U5_coverage *0.20), max_v), U5_coverage),
#                         adult_coverage = U5_coverage)
# head(cm_2)
# 
# summary(cm_2$U5_coverage)
# 
# write.csv(cm_2, 'results/archetype_sim_input/Intervention_files_LGA/case_management/cm_scen2_20.csv')
# 
# 
# 
# # increase cm coverage by 30% 
# cm_2 <- read.csv('bin/projection/s2_v2/HS_placeholder_v2.csv')
# head(cm_2)
# 
# cm_2 <- cm_2 %>% mutate(U5_coverage = ifelse(U5_coverage < 0.80, pmin(U5_coverage + (U5_coverage *0.30), max_v),U5_coverage),
#                         adult_coverage = U5_coverage)
# head(cm_2)
# 
# summary(cm_2$U5_coverage)
# 
# write.csv(cm_2, 'results/archetype_sim_input/Intervention_files_LGA/case_management/cm_scen2_30.csv')

