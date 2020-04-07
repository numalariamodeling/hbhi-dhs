# creating list of files to identify areas that receieved LLIN for 2010 and 2013 
dist.ITN.list <- list(NGAfiles[[12]], NGAfiles[[15]])

look_for(dist.ITN.list[[2]], "net")

val_labels(dist.ITN.list[[1]]$shmalar)

key.ITN.dist <- list(key_list[[4]], key_list[[5]])

dist.ITN.list <- map(dist.ITN.list, survey.month.fun)

table(dist.ITN.list[[1]]$MM)

# key datasets and dhs/mis datasets are joined  
dist.ITN.list <- map2(dist.ITN.list, key.ITN.dist, left_join) #PR datasets

LLIN_distr_2010 <- dist.ITN.list[[1]] %>% filter(shmalar =="1" | shmalar =="2") %>% 
                      dplyr::select(State, LGA, shmalar) %>% group_by(LGA) %>% summarise(n()) %>% 
                        dplyr::select(LGA, number =`n()`)

head(LLIN_distr_2010)

distr_LGA <- LGAshp_sf %>% left_join(LLIN_distr_2010) %>% filter(!is.na(number)) %>%mutate(year = '2010')%>% 
               dplyr::select(State, LGA, year, number)

head(distr_LGA)

st_write(distr_LGA, "results/archetype_sim_input/2010ITN_distribution.csv")

