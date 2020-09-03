s7_itn <- read.csv('bin/projection/s7/itn_MapB.csv')
head(s7_itn)

s7_itn$LGA <- gsub("\\/", "-", s7_itn$LGA)

s1_itn <- read.csv('bin/projection/s7/itn_scenario1.csv')
head(s1_itn)

s7_itn <- s7_itn %>% left_join(s1_itn)
head(s7_itn)

itn_s7 <- s1_itn %>% filter(!is.na(llin_MapB))
write.csv(s7_itn, 'results/archetype_sim_input/Intervention_files_LGA/itn_scenario7_v2.csv')
