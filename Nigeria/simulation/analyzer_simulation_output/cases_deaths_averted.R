rm(list=ls())

path = "C:/Users/ido0593/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2025_v6"
BAU <- read.csv("NGA projection scenario 1/annual_indicators.csv") %>% dplyr::select(year,  
                                                                                     cases, deaths_1, deaths_2,
                                                                                     cases_U5, deaths_U5_1, deaths_U5_2)
# file.names <- dir(path, pattern ="annual_indicators.csv", recursive = TRUE)
# for(i in 1:length(file.names)){
#   file <- read.csv(file.names[i],header=TRUE, sep=";", stringsAsFactors=FALSE) %>% 
#     dplyr::select(year,  
#                   paste0(file.names, "cases", sep = "")=cases, paste0(file.names, "deaths")=deaths_1, 
#                   paste0(file.names, "deaths_2", sep="")=deaths_2, paste0(file.names, "cases_U5")=cases_U5, 
#                   paste0(file.names, "deaths_U5_1", sep="")=deaths_U5_1, 
#                   paste0(file.names, "deaths_U5_2", sep="") =deaths_U5_2)
#   
#   out.file <- left_join(BAU, file, by ="year") %>%mutate(paste0("cases_averted", file.names, sep="") 
#                                                          = cases - paste0(file.names, "cases"),
#                                                          paste0("deaths_1_ averted", file.names)
#                                                          = deaths_1 - paste0(file.names, "death_1"),
#                                                          paste0("deaths_ 2_averted", file.names) 
#                                                          = deaths_2 - paste0(file.names, "death_2"),
#                                                         paste0("cases_U5_averted", file.names) = 
#                                                           cases_U5 - paste0(file.names, "cases_U5"),
#                                                         paste0("deaths_1_U5_averted", file.names)
#                                                         = deaths_U5_1 - paste0(file.names, "deaths_U5_1"),
#                                                         paste0("deaths_2_U5_averted", file.names)
#                                                         = deaths_U5_2 - paste0(file.names, "deaths_U5_2")) %>% 
#     summarise_all(sum)
#   write.csv(out.file, file = paste0(file.names, ".csv", sep=""))
# }
# 
# write.table(out.file, file = "cand_Brazil.txt",sep=";", 
#             row.names = FALSE, qmethod = "double",fileEncoding="windows-1252")




setwd("C:/Users/ido0593/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2025_v6")


BAU <- read.csv("NGA projection scenario 1/annual_indicators.csv") %>% dplyr::select(year = year,  
                                                                                     cases, deaths_1, deaths_2,
                                                                              cases_U5, deaths_U5_1, deaths_U5_2)
head(BAU)

scen <- read.csv("NGA projection scenario 6/annual_indicators.csv") %>% dplyr::select(year, scen6_cases = cases, 
                                                                              scen6_deaths_1 =deaths_1, scen6_deaths_2 =deaths_2,
                                                                           scen6_cases_U5= cases_U5, 
                                                                           scen6_deaths_U5_1=deaths_U5_1 ,
                                                                           scen6_deaths_U5_2=deaths_U5_2)
head(scen)

comp <- left_join(BAU, scen, by = "year") %>% dplyr::mutate(cases_averted_scenario_6 = cases - scen6_cases,
                                                      deaths_1_averted_scenario_6= deaths_1 - scen6_deaths_1,
                                                      deaths_2_averted_scenario_6 = deaths_2 - scen6_deaths_2,
                                                      cases_U5_averted_scenario_6 = cases_U5 - scen6_cases_U5,
                                                      deaths_1_U5_averted_scenario_6= deaths_U5_1 - scen6_deaths_U5_1,
                                                      deaths_2_U5_averted_scenario_6 = deaths_U5_2 - scen6_deaths_U5_2) %>% 
                                        summarise_all(sum)
  
  
head(comp)
write.csv(comp, "scenario 6_averted_sum.csv")
