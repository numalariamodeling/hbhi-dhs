
rep_DS <- read.csv("bin/rep_DS/representative_DS_orig60clusters.csv") %>% dplyr::select(-X)
head(rep_DS)

may_peak <- c("Ahoada West", "Akinyele", "Babura", "Gwarzo")

june_peak <- c("Isoko South","Ngor-Okpala", "Nsit Ubium", "Obubra", "Oru West", "Owan East", "Uzo-Uwani")

july_peak <- c("Alkaleri", "Darazo")

august_peak <- c("Birnin Kebbi", "Giwa")

sept_peak <- c("Gawabawa", "Keana")

oct_peak <- c("Oke-Ero", "Rimi", "Soba", "Takali")

nov_peak <- c("Taura")

LGA <- rep_DS %>% mutate(peak = ifelse(repDS %in% may_peak, "may", 
                                       ifelse(repDS %in% june_peak, "june", 
                                              ifelse(repDS %in% july_peak, "july",
                                                        ifelse(repDS %in% august_peak, "august",
                                                               ifelse(repDS %in% sept_peak, "september",
                                                                      ifelse(repDS %in% oct_peak, "october",
                                                                             ifelse(repDS %in% nov_peak, "november", "NA"))))))))

table(LGA$peak)

LGA_2020 <- LGA %>% mutate(simday = if_else(peak == 'july',186,if_else(peak == 'may',131,
                                                       ifelse(peak == 'august', 223, if_else(peak == 'june',162,
                                                           ifelse(peak =='september', 250, 
                                                               ifelse(peak == "october", 250,
                                                                      ifelse(peak == "november", 250, NA))))))), year = 2020)

summary(LGA_2020$simday)


LGA_2020 <- expandRows(LGA_2020, count = 4,count.is.col=FALSE, 
                       drop = FALSE) 

LGA_2020 <- LGA_2020 %>%  mutate(round = rep(c(1,2,3,4), times = 774))


LGA_2020 <- LGA_2020 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

tail(LGA_2020, 60)




#2021
LGA_2021 <- LGA_2020 %>% mutate(simday = if_else(peak == 'july',550,if_else(peak == 'may',488,
                                                if_else(peak == 'august', 579, if_else(peak == 'june',518, 
                                                   if_else(peak =='september',610, 
                                                      if_else(peak == "october", 610,
                                                              if_else(peak == "november", 610, NA_real_))))))), year = 2021)



LGA_2021 <- LGA_2021 %>% 
  mutate(simday = if_else(round == 2, simday + 30,
                          if_else(round == 3, simday + 60, 
                                  if_else(round ==4, simday + 90, simday))))

head(LGA_2021)


all_smc <-do.call("rbind", list(LGA_2020, 
                                LGA_2021))

all_smc$LGA <- gsub("\\/", "-", all_smc$LGA)

all_smc <- all_smc %>%  mutate(duration = -1, coverage_high_access = 1,
                              coverage_low_access = 0.60, max_age = 10)

write.csv(all_smc, 'results/archetype_sim_input/Intervention_files_LGA/smc/smc_covid_scen5.csv')

