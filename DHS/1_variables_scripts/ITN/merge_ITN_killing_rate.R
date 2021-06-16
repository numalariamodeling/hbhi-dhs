ITN_files <- list.files(path = "results/archetype_sim_input/Intervention_files_LGA/ITN", 
                          pattern="*.csv$", full.names=TRUE, recursive=F)

ITN_files.list <-  sapply(ITN_files, read.csv, simplify = F)

ITN_files.list <- map(ITN_files.list, ~ (.x %>% dplyr::select(-X)))

kill_rate <- read.csv("bin/kill_rate/Killing_rate_nigeria_2010.csv") %>% dplyr::select(-X, LGA=DS)

kill_rate.list <- list(kill_rate)

allfiles <- map2(ITN_files.list,kill_rate.list,left_join)

mapply(write.csv, allfiles, file=paste0(names(allfiles), '.csv'))


#map(myList, ~ (.x %>% select(-ID)))