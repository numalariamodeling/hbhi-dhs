
setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/results/archetype_sim_input/Intervention_files_LGA/ITN/v2")

current_itn <- read.csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/DHS/LGA_intervention_files/ITN/ITN_by_LGA_v3.csv") %>% 
                  rename(LGA_new = LGA, LGA = LGA_old) %>% 
  mutate(LGA = as.character(LGA), LGA = ifelse(LGA == "kaita", "Kaita", ifelse(LGA == "kiyawa", "Kiyawa", LGA)))
head(current_itn)

##Get all different first five letter strings for all cvs files in directory "."                                                                                                                                                                                           
file.prefixes <- unique(sapply(list.files(path=".", pattern="*.csv"), substr, 1,5))


##Group all matching file names according to file.prefixes into a list                                                                                                                                                                                                     
file.list <- lapply(file.prefixes, function(x)list.files(pattern=paste("^",x,".*.csv",sep=""), path="."))
names(file.list) <- file.prefixes ##just for convenience     


##parse all csv files in file.list, create a list of lists containing all tables for each prefix                                                                                                                                                                           
tables <- lapply(file.list, function(filenames)lapply(filenames, function(file)read.csv(file, header=TRUE)))


##for each prefix, rbind the tables. Result is a list of length being length(file.prefixes)                                                                                                                                                                                
##  each containing a matrix with the combined data parsed from the files that match the prefix                                                                                                                                                                            
joined.tables <- lapply(tables, function(t)do.call(rbind, t))

all_df <- do.call(cbind, joined.tables)
head(all_df)


all_df <- all_df %>% dplyr::select(U5201.LGA, U5201.hh_itn, U5201.repDS, U5201.ci_l,U5201.ci_u, U5201.year,six_n.hh_itn,
                                   six_n.ci_l,six_n.ci_u, six_n.year, ten_e.hh_itn, ten_e.ci_l, ten_e.ci_u, ten_e.year, over_.hh_itn,
                                   over_.ci_l, over_.ci_u, over_.year) %>% rename(LGA = U5201.LGA,  year = U5201.year) %>% 
  mutate(LGA = as.character(LGA), LGA = ifelse(LGA == "kaita", "Kaita", ifelse(LGA == "kiyawa", "Kiyawa", LGA)))
head(all_df)

fin_df <- left_join(current_itn, all_df)

head(fin_df)

write.csv(fin_df, "ITN_by_LGA.csv")
