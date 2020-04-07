s2_itn <- read.csv('bin/projection/s2_v2/itn_scen2_80.csv') %>% dplyr::rename(LGA = adm2)
head(s2_itn)
       
head(s2_itn) # all LGAs seem to be getting LLINs here 

s2_itn$LGA <- gsub("\\/", "-", s2_itn$LGA)

# ento <- read.csv('C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/ento/insecticide_resistance_DS_means/Permethrin_mortality_DS_means.csv')
# colnames(ento)[2]<- "LGA"
# head(ento)

# s2_itn$LGA <- as.character(s2_itn$LGA)
# 
# df <- s2_itn %>%  mutate(LGA = ifelse(LGA == "Kaura-Namoda","Kaura Namoda", LGA))
# 
# itn_check <- df %>% left_join(ento)
# 
# head(itn_check)
# 
# write.csv(itn_check, 'bin/projection/s2_v2/itn_scenario2_aadrita_names.csv')



in_80_fun <- function(x){ifelse(x < 0.80, 0.80,x)} #function to increase by up to 80%  
in_10_fun <- function(x){pmin(x + (0.10*x), 1)} #function to increase by 10% 
in_20_fun <- function(x){pmin(x + (0.20*x), 1)} #function to increase by 20% 
in_30_fun <- function(x){pmin(x + (0.30*x), 1)} #function to increase by 30% 

s1_itn_80 <- read.csv('results/archetype_sim_input/Intervention_files_LGA/itn_scenario1_v3.csv') %>% 
              mutate_at(vars(matches("use")), in_80_fun)
head(s1_itn_80)


s1_itn_10 <- read.csv('results/archetype_sim_input/Intervention_files_LGA/itn_scenario1_v3.csv') %>% 
  mutate_at(vars(matches("use")), in_10_fun)

head(s1_itn_10)

summary(s1_itn_10$over_eighteen_ITN_use)

s1_itn_20 <- read.csv('results/archetype_sim_input/Intervention_files_LGA/itn_scenario1_v3.csv') %>% 
  mutate_at(vars(matches("use")), in_20_fun)

head(s1_itn_20)

summary(s1_itn_20$over_eighteen_ITN_use)


s1_itn_30 <- read.csv('results/archetype_sim_input/Intervention_files_LGA/itn_scenario1_v3.csv') %>% 
  mutate_at(vars(matches("use")), in_30_fun)

head(s1_itn_30)

summary(s1_itn_30$over_eighteen_ITN_use)




#final join 
kill_block_80 <- read.csv('bin/projection/s2_v2/itn_scenario2_block_kill.csv') %>% 
    dplyr::select(LGA, llins1, mortality, EMOD_kill_rate, new_block) %>% 
      mutate(LGA = ifelse(LGA == "Kaura Namoda","Kaura-Namoda", as.character(LGA)))
head(kill_block_80)

kill_block_80$LGA <- gsub("\\/", "-", kill_block_80$LGA)

colnames(kill_block_80)[4] <- "kill_rate_new"
colnames(kill_block_80)[5] <- "block_initial_new"

head(kill_block_80)

s2_itn_80 <- left_join(s1_itn_80, kill_block_80, by = "LGA") # don't think a join is warranted 
head(s2_itn_80)

s2_itn_10 <- left_join(s1_itn_10, kill_block_80, by = "LGA") # don't think a join is warranted 
head(s2_itn_10)

s2_itn_20 <- left_join(s1_itn_20, kill_block_80, by = "LGA") # don't think a join is warranted 
head(s2_itn_20)

s2_itn_30 <- left_join(s1_itn_30, kill_block_80, by = "LGA") # don't think a join is warranted 
head(s2_itn_30)

write.csv(s2_itn_80, 'results/archetype_sim_input/Intervention_files_LGA/ITN/v2/ITN_scen2_80.csv')
write.csv(s2_itn_10, 'results/archetype_sim_input/Intervention_files_LGA/ITN/v2/ITN_scen2_10.csv')
write.csv(s2_itn_20, 'results/archetype_sim_input/Intervention_files_LGA/ITN/v2/ITN_scen2_20.csv')
write.csv(s2_itn_30, 'results/archetype_sim_input/Intervention_files_LGA/ITN/v2/ITN_scen2_30.csv')

