s4_mapA <- read.csv("bin/projection/s4/s4_mix.csv")
s1_itn <- read.csv("bin/projection/s4/itn_scenario1.csv")

#change symbols 
s4_mapA$LGA <- gsub("\\/", "-", s4_mapA$LGA)
#check row 
s4_mapA[s4_mapA$LGA =='Abua-Odual',]

s4_df <- s1_itn %>% left_join(s4_mapA) %>%  
          dplyr::mutate(kill_rate = 
            ifelse(llin_type == 'itn_pbo', 0.7188, kill_rate)) %>% 
                dplyr::select(-mortality_rate)


write.csv(s4_df, "results/archetype_sim_input/Intervention_files_LGA/ITN/itn_scenario4.csv")
