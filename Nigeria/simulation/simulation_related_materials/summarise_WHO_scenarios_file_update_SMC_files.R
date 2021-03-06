# This script was used to update the nigeria scenario files from LLINS to PBO 
# Date: 02-25-2021



# file paths 

rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v3")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")
simInDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v2_old")
simInDir_new <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v4/SMC")
ScenDir <- file.path(ProjectDir, "scenarios")
DataDir<- file.path(TeamDir, "data", "nigeria_dhs", "data_analysis")
subDir <- file.path(DataDir, "results/LGA_maps/SMC")
dir.create(subDir, showWarnings = TRUE)
source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))


###############################################################################
# summarize scenarios file 
###############################################################################


nsp_plan <- read.csv(file.path(ProjectDir, "scenarios", "210623_NGA_NSP_GFfr.csv")) %>% group_by(caseman_nsp, mass_llins_nsp, iptp_nsp, smc_nsp, ipti_nsp) %>% tally() 

funded_plan<- read.csv(file.path(ProjectDir, "scenarios", "210623_NGA_NSP_GFfr.csv")) %>% group_by(caseman_fund, mass_llins_fund, iptp_fund, smc_fund_paar) %>% tally()






###############################################################################
# SMC scenario 2 - 5 file update 
###############################################################################

# select the old list of smc LGAs in scenarios 2-5

scen_2_5_old <- df[[1]] %>%  filter(year == 2020, round == 1) %>%  dplyr::select(State, LGA)

# Read in scenarios from Bea 

SMC_scen <- read.csv(file.path(ProjectDir, "scenarios", "210203_NGA_NSP-GFfr.csv")) %>%  dplyr::select(LGA=adm2, smc_nsp, State=adm1) %>%  filter(smc_nsp == "Yes") %>% 
  mutate(LGA = gsub("\\/","-", LGA),
         LGA = case_when(LGA == "Jama'Are" ~ "Jama'are",
                        LGA == "Jema'A"  ~ "Jema'a",
                        LGA == "Qua'An Pan" ~"Qua'an Pan",
                        TRUE ~ LGA))


#check what is new 
new_smc<- anti_join(SMC_scen, scen_2_5_old, by ="LGA") %>% dplyr::select(LGA, State) %>% 
  uncount(44) %>% 
  mutate(year=rep(rep(2020:2030, each = 4), 9), round=rep(1:4, 99), duration = -1, coverage_high_access = 1, coverage_low_access = 0.6, max_age = 5, peak = "may")

#get simulation times for those in may from df 
simday <- df[[1]] %>% filter(peak == "may", State =="Katsina", LGA == "Baure") %>%  dplyr::select(simday)


scen_2_5_newSMC <- cbind(new_smc, simday)

#get repDS 
new_smc_LGA<- anti_join(SMC_scen, scen_2_5_old) %>% dplyr::select(LGA)
rep_DS <-read.csv(file.path(DataDir, "bin", "rep_DS", "representative_DS_orig60clusters.csv")) %>% filter(LGA %in% new_smc_LGA$LGA) 

scen_2_5_newSMC <- left_join(scen_2_5_newSMC, rep_DS) 
colnames(scen_2_5_newSMC)[colnames(scen_2_5_newSMC) == 'X'] <- 'X1'

dim(df[[1]])
dim(scen_2_5_newSMC)# number of columns are the same 


smc_2_5_fin <- rbind(df[[1]],scen_2_5_newSMC)%>% mutate(scenario = 'scenario 2 - 5')
write.csv(smc_2_5_fin, file.path(simInDir_new, "smc_increase80_2020_2030.csv"))


###############################################################################
# SMC scenario 6 file update 
###############################################################################


# Read in SMC scenario 6 files in need of updates 
scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))


for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(simInDir_new), pattern = "*_noPAAR", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}

#remove LGAs in Kwara
scen_6_old <- df[[1]] %>%  filter(year == 2020, round == 1) %>%  dplyr::select(State=adm1, LGA) %>% filter(State != 'Kwara')


# Read in scenarios from Bea 

SMC_scen <- read.csv(file.path(ProjectDir, "scenarios", "210623_NGA_NSP_GFfr.csv")) %>%  dplyr::select(LGA=adm2, smc_fund_paar, State=adm1) %>%  filter(smc_fund_paar == "SMC") %>% 
  mutate(LGA = gsub("\\/","-", LGA),
         LGA = case_when(LGA == "Jama'Are" ~ "Jama'are",
                         LGA == "Jema'A"  ~ "Jema'a",
                         LGA == "Qua'An Pan" ~"Qua'an Pan",
                         LGA == 'kiyawa' ~ 'Kiyawa',
                         LGA == 'kaita' ~ 'Kaita',
                         TRUE ~ LGA))


#check what is new (19 LGAs found)
new_smc<- anti_join(SMC_scen,scen_6_old, by ="LGA") %>% dplyr::select(LGA, State) %>% 
  uncount(44) %>% 
 mutate(year=rep(rep(2020:2030, each = 4), 19),  round=rep(1:4, 209), duration = -1, coverage_high_access = 1, coverage_low_access = 0.6, max_age = 5, peak = "may")




#get simulation times for those in may from df 
simday <- df[[1]] %>% filter(peak == "may", adm1  =="Katsina", LGA == "Baure") %>%  dplyr::select(simday) 


scen_6_newSMC <- cbind(new_smc, simday)


#get repDS 
new_smc_LGA<- anti_join(SMC_scen, scen_6_old) %>% dplyr::select(LGA)
rep_DS <-read.csv(file.path(DataDir, "bin", "rep_DS", "representative_DS_orig60clusters.csv")) %>% filter(LGA %in% new_smc_LGA$LGA) 

scen_6_newSMC <- left_join(scen_6_newSMC, rep_DS) %>%mutate(smc_plan1 ="SMC")
colnames(scen_6_newSMC)[colnames(scen_6_newSMC) == 'State'] <- 'adm1'

all_SMC <- df[[1]]  %>%  filter(  adm1 != 'Kwara')
scen_6_newSMC <- scen_6_newSMC %>%  dplyr::select(-c(X))
scen_6_newSMC$scenario = 'scenario 6'

head(df[[1]])
head(scen_6_newSMC)# number of columns are the same 


smc_6_fin <- rbind(all_SMC,scen_6_newSMC)
write.csv(smc_6_fin, file.path(simInDir_new, "smc_scenario6_noPAAR_2020_2030.csv"))


###############################################################################
# SMC scenario 7 file update 
###############################################################################


# Read in SMC scenario 6 files in need of updates 
scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))


for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(simInDir_new), pattern = "*_PAAR", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}

scen_7_old <- df[[1]] %>%  filter(year == 2020, round == 1) %>%  dplyr::select(State=adm1, LGA)

# Read in scenarios from Bea 

SMC_scen <- read.csv(file.path(ProjectDir, "scenarios", "210623_NGA_NSP_GFfr.csv")) %>%  dplyr::select(LGA=adm2, smc_fund_paar, State=adm1) %>%  filter(smc_fund_paar != "No SMC") %>% 
  mutate(LGA = gsub("\\/","-", LGA),
         LGA = case_when(LGA == "Jama'Are" ~ "Jama'are",
                         LGA == "Jema'A"  ~ "Jema'a",
                         LGA == "Qua'An Pan" ~"Qua'an Pan",
                         LGA == 'kiyawa' ~ 'Kiyawa',
                         LGA == 'kaita' ~ 'Kaita',
                         TRUE ~ LGA))


#check what is new (No new LGAs)
new_smc<- anti_join(scen_7_old, SMC_scen,by ="LGA") %>% dplyr::select(LGA, State)




# smc_7_fin <- rbind(df[[2]],scen_6_newSMC)%>% mutate(scenario = 'scenario 7')

#write.csv(smc_7_fin, file.path(simInDir_new, "smc_noPAAR_2020_2030.csv"))







###############################################################################
# Maps 
###############################################################################

#turn smc files to list 
smc_df <- list(smc_2_5_fin, smc_6_fin, smc_7_fin)



#map of kill rates 

LGA <- clean_LGA(file.path(DataDir,"data/Nigeria_LGAs_shapefile_191016"), file.path(DataDir, "bin/names/LGA_shp_pop_names.csv"))
LGA_list<-list(LGA)


#SMC csvs for maps 

SMC_map_cv <- map(smc_df, ~filter(.x, year == 2020, round ==1))



#create shp file for mapping 

LGA_shp <-map2(LGA_list, SMC_map_cv, left_join, by ="LGA")


# map function

map_fun <- function(shpfile, map_val, var) {
  scenario <- unique(na.omit(shpfile$scenario))
  tm_shape(shpfile) + #this is the health district shapfile with LLIn info
    tm_polygons(col = map_val, textNA = "No data", 
                title = "", palette = "seq", breaks=c(0, 0.1, 0.2, 
                                                      0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
    tm_layout(title =paste0(scenario,var,Sys.Date()),
              aes.palette = list(seq="RdYlBu")) 
}



# map

map_val <- list("coverage_high_access")
var <- list("_smc_coverage-")
maps <- pmap(list(LGA_shp, map_val, var), map_fun)
arrange_maps <- do.call(tmap_arrange, maps)
arrange_maps

tmap_save(tm =arrange_maps, filename = paste0(subDir,"/", Sys.Date(), "_SMC_maps_scenario_2-7.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
