# read ento file 

itn.scen3<- read.csv('bin/projection/s3_v3/itn_v2_block_kill.csv')%>% dplyr::rename(LGA = DS) %>% 
  dplyr::select(LGA, mortality, llins_plan, kill_rate, blocking_rate) 
head(itn.scen3)

itn.scen3$LGA<- gsub("\\/", "-", itn.scen3$LGA)


in_80_fun <- function(x){ifelse(x < 0.80, 0.80,x)} #function to increase by up to 80%

s1_itn_80 <- read.csv('results/archetype_sim_input/Intervention_files_LGA/ITN/itn_scenario1_v3.csv') %>% 
  mutate_at(vars(matches("use")), in_80_fun) %>% dplyr::select(-c(kill_rate, mortality_rate))

s3_itn_80 <- left_join(s1_itn_80,itn.scen3, by = "LGA") 


# #final join 
# kill_block_3 <- read.csv('bin/projection/s3_v3/itn_block_kill.csv') %>% 
#   dplyr::select(-c(X,adm1)) %>% 
#   mutate(adm2 = ifelse(adm2 == "Kaura Namoda","Kaura-Namoda", as.character(adm2)))
# head(kill_block_3)
# 
# colnames(kill_block_3)[1]<- "LGA"
# 
# 
# kill_block_3$LGA <- gsub("\\/", "-", kill_block_3$LGA)
# 
# colnames(kill_block_3)[4] <- "kill_rate_new"
# colnames(kill_block_3)[5] <- "block_initial_new"
# 
# 
# s3_itn_80 <- inner_join(s1_itn_80, kill_block_3, by = "LGA") 
# head(s3_itn_80)


write.csv(s3_itn_80, 'results/archetype_sim_input/Intervention_files_LGA/ITN/v2/ITN_scen3_80_v2.csv')
