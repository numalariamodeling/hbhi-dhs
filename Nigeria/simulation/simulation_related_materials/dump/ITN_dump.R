# Read in ITN from Aadrita's work 

aa_ITN <- read.csv(file.path(ProjectDir, "ITN_parameter", "itn_scenario2_block_kill.csv"))  #%>%  filter(llins1 =="PBO") %>%  
#mutate(LGA = ifelse(grepl("Namoda$", LGA), "Kaura-Namoda", LGA))
summary(aa_ITN$new_block)

#check if Bea's scenario files and Aadrita's ITN files match 
check <- anti_join(PBO_scen, aa_ITN, by =c("adm2" = "LGA")) # matches 


#now we read in aadrita's dataset and bind to scenarios to change llins1, mortality_rate, EMOD_kill_rate and block_rate 
aa_ITN <- read.csv(file.path(ProjectDir, "ITN_parameter", "itn_scenario2_block_kill.csv")) %>%  
  mutate(LGA = ifelse(grepl("Namoda$", LGA), "Kaura-Namoda", LGA), new_block= ifelse(llins1== "Urban areas", 0.53, new_block),
         mortality_group = ifelse(mortality >=0.5, ">=0.5 mortality group", "<0.5 mortality group")) 



aa_ITN_ls <- list(aa_ITN)                   

update_ITN_fun<- function(df, df2){
  data <- left_join(df, df2, by =c("LGA_old" = "LGA")) %>% 
    mutate(mortality_rate = mortality, kill_rate=EMOD_kill_rate, block_initial = new_block) %>% 
    dplyr::select(-c(mortality, EMOD_kill_rate, new_block, X, Unnamed..0))
}

