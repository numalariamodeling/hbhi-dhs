
#read in files 
filenames <- list.files("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/DHS/LGA_intervention_files/ACT", pattern="*.csv", full.names=TRUE)
ldf <- lapply(filenames, read.csv)

head(ldf[[1]])

year_2013 <- ldf[[1]] %>%  dplyr::select(-c(simday, duration))
head(year_2013)

year_2015 <- ldf[[2]] %>%  dplyr::select(-X)
head(year_2015)

year_2018 <- ldf[[3]] %>%  dplyr::select(-c(simday, duration, X.1, X))
head(year_2018)

all_df <- rbind(year_2013, year_2015, year_2018)
head(all_df)


year_2010 <- read.csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/DHS/arch_med_10_v2.csv")
head(year_2010)

head(rep_DS)


year_2010 <- left_join(rep_DS, year_2010, by = "repDS") %>% dplyr:: select(-c(X, State, adult_coverage, year, notes, simday, duration)) %>%
                              rename(Number.of.Participants = U5_num, comboACT_repDS = U5_coverage, ci_l_repDS=U5_80_LCL,ci_u_repDS=U5_80_UCL) %>% 
                        mutate(year = 2010, ci_l = ci_l_repDS, ci_u = ci_u_repDS, comboACT= comboACT_repDS, Num_repDS = NA)
head(year_2010)



all_df_2 <- rbind(all_df,year_2010) %>% dplyr::select(LGA,  repDS, comboACT, ci_l_repDS, ci_u_repDS, year) %>% rename(Rep_DS = repDS) %>% 
                                mutate(LGA = as.character(LGA), LGA = ifelse(LGA == "kaita", "Kaita", ifelse(LGA == "kiyawa", "Kiyawa", LGA)))
head(all_df_2)
class(all_df_2$LGA)

all_df_2$LGA <- gsub("\\/", "-", all_df_2$LGA)

summary(all_df_2$LGA)

all_old <- ldf[[4]] %>% dplyr::select(-c(X, X.1, U5_coverage, adult_coverage)) %>% mutate(LGA = as.character(LGA), LGA = ifelse(LGA == "kaita", "Kaita", ifelse(LGA == "kiyawa", "Kiyawa", LGA)))
head(all_old)

all_old$LGA <- gsub("\\/", "-", all_old$LGA)

df_fin <- all_old %>% left_join(all_df_2)%>%rename(U5_coverage = comboACT, U5_coverage_l = ci_l_repDS, U5_coverage_h = ci_u_repDS) %>% 
                          mutate(adult_coverage = U5_coverage, adult_coverage_l = U5_coverage_l, adult_coverage_h = U5_coverage_h) %>% 
                          mutate(U5_coverage_l = case_when(U5_coverage_l < 0 ~ 0,
                                                            TRUE ~ as.numeric(U5_coverage_l)))
head(df_fin)

write.csv(df_fin,"C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/DHS/LGA_intervention_files/ACT/HS_by_LGA_v2.csv")
