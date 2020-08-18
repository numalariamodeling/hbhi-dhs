#read itn files 

rm(list=ls())

path = "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/projection_csvs/projection_v2"


itn_cc <- read.csv(paste0(path, "/itn_scenario1.csv")) %>% dplyr:: select(LGA, simday, year, round)
itn_80_3 <- read.csv(paste0(path, "/ITN_scen3_80_v3.csv")) %>% left_join(itn_cc, by = c("LGA", "year", "round")) %>% 
  mutate(simday = simday.y) %>%  dplyr::select(-c(simday.x, simday.y))

head(itn_cc)
head(itn_10)
summary(itn_30$simday)
summary(itn_cc$simday)

write.csv(itn_80_3, paste0(path, '/ITN_scen3_80_v3.csv'))

smc_cc <- read.csv(paste0(path, "/smc_cc_v4.csv")) %>%
  mutate(overall_coverage_cc = coverage_high_access * 0.5 + coverage_low_access *0.5) %>% 
  dplyr:: select(LGA, simday, year, round, overall_coverage_cc, high_access_cc = coverage_high_access, low_access_cc = coverage_low_access)

smc_80 <- read.csv(paste0(path, "/smc_scen3_no_PAAR_v3.csv")) %>% mutate(overall_coverage = coverage_high_access * 0.5 + coverage_low_access *0.5) %>% 
  left_join(smc_cc, by = c("LGA", "year", "round")) %>% 
  mutate(coverage_high_access = ifelse(coverage_high_access  > high_access_cc, coverage_high_access, high_access_cc),
         coverage_low_access = ifelse(coverage_low_access > low_access_cc, coverage_low_access, low_access_cc),
         coverage_high_access = ifelse(is.na(coverage_high_access), 1, coverage_high_access),
         coverage_low_access = ifelse(is.na(coverage_low_access), 0.6, coverage_low_access)) %>% 
  mutate(simday = ifelse(is.na(simday.y),simday.x, simday.y)) %>%  dplyr::select(-c(simday.x, simday.y))

head(smc_cc)
tail(smc_80)
summary(smc_80$simday.y)

write.csv(smc_80, paste0(path, '/smc_scen3_no_PAAR_v4.csv'))
