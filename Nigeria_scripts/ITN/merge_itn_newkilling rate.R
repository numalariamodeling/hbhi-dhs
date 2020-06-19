itn_lga<- read.csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/DHS/LGA_intervention_files/ITN/ITN_by_LGA.csv")

itn_para <- read.csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/ITN_parameter/ITN_new_kill_and_block_2019_forward.csv")

itn_lga_list <- split(itn_lga, itn_lga$year)

head(itn_lga_list[[1]])

itn_para<- itn_para %>% rename(LGA_old = DS)

head(itn_para)

itn_para_list <- list(itn_para)

itn_lga_list2 <- map2(itn_lga_list, itn_para_list, left_join)

df <- bind_rows(itn_lga_list2)

write.csv(df,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/DHS/LGA_intervention_files/ITN/ITN_by_LGA_v3.csv")
