scen_1<- read.csv('bin/projection/LGA_scenarios/scenario7.csv')
head(scen_1)

head(LGAshp_sf)

LGA_state <- LGAshp_sf %>% dplyr::select(LGA, State) %>% 
  mutate(LGA = ifelse(LGA == "kaita","Kaita", ifelse(LGA == "kiyawa", "Kiyawa", as.character(LGA))))
head(LGA_state)

LGA_state$LGA <- gsub("\\/", "-", LGA_state$LGA)


scen_1_v2 <- scen_1 %>% left_join(LGA_state) %>% dplyr::select(-geometry)

head(scen_1_v2)

write.csv(scen_1_v2, "check.csv")

df <- scen_1_v2 %>% group_by(year, State) %>% 
  summarise(weighted_u5_pfpr = sum(U5_PfPR *(geopode.pop_U5/sum(geopode.pop_U5))),
            weighted_pfpr = sum(PfPR * (geopode.pop/sum(geopode.pop))),
            weighted_U5_incidence = sum(U5_incidence *(geopode.pop_U5/sum(geopode.pop_U5))),
            weighted_incidence = sum(incidence *(geopode.pop/sum(geopode.pop))),
            w_U5_death_rate_1 = sum(U5_death_rate_1 *(geopode.pop_U5/sum(geopode.pop_U5))),
            w_U5_death_rate_2 = sum(U5_death_rate_2 *(geopode.pop_U5/sum(geopode.pop_U5))), 
            w_U5_death_rate_mean =
              (sum(U5_death_rate_1 *(geopode.pop_U5/sum(geopode.pop_U5))) 
            + sum(U5_death_rate_2 *(geopode.pop_U5/sum(geopode.pop_U5))))/2,
            w_death_rate_1 = sum(death_rate_1 *(geopode.pop/sum(geopode.pop))),
            w_death_rate_2 = sum(death_rate_2 *(geopode.pop/sum(geopode.pop))),
            w_death_rate_mean =
              (sum(death_rate_1 *(geopode.pop/sum(geopode.pop))) 
               + sum(death_rate_2 *(geopode.pop/sum(geopode.pop))))/2,
            sum(num_mStillbirths),
            sum(num_mLBW))

colnames(df) <- c("Year", "State", "U5 PfPR", "PfPR", "U5 incidence", "incidence", "U5 Death rate 1",
                  "U5 Death rate 2", "U5 Mean death rate", "Death rate 1", "Death rate 2", "Mean death rate",
                  "Number of stillbirths (thousands)", "Number of LBWs (thousands)")
            
            
write.csv(df, "results/state_projection/scenario7.csv")
                                                                            