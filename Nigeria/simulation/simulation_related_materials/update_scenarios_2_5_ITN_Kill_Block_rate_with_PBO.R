# This script was used to update the nigeria scenario files from LLINS to PBO 




# paths 

rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")
simInDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v2_old")
simInDir_new <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v3")
ScenDir <- file.path(ProjectDir, "scenarios")
DataDir<- file.path(TeamDir, "data", "nigeria_dhs", "data_analysis")
subDir <- file.path(DataDir, "results/LGA_maps/ITN_updated_ITN_efficacy_scenario_2_5")
dir.create(subDir, showWarnings = TRUE)
source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))


###############################################################################
# Data processing 
###############################################################################


# Read in ITN scenario files in need of updates 
scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(simInDir), pattern = "*itn_increase", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}

# Read in scenarios from Bea 

PBO_scen <- read.csv(file.path(ScenDir, "NGA_NSP_GFfr_v3.csv")) %>%  dplyr::select(adm2, llins1) %>%  filter(llins1 == "PBO")


# Read in ITN from Aadrita's work 

aa_ITN <- read.csv(file.path(ProjectDir, "ITN_parameter", "itn_scenario2_block_kill.csv")) %>%  filter(llins1 =="PBO") %>%  
  mutate(LGA = ifelse(grepl("Namoda$", LGA), "Kaura-Namoda", LGA))
summary(aa_ITN$new_block)

#check if Bea's scenario files and Aadrita's ITN files match 
check <- anti_join(PBO_scen, aa_ITN, by =c("adm2" = "LGA")) # matches 


#now we read in aadrita's dataset and bind to scenarios to change llins1, mortality_rate, EMOD_kill_rate and block_rate 
aa_ITN <- read.csv(file.path(ProjectDir, "ITN_parameter", "itn_scenario2_block_kill.csv")) %>%  
mutate(LGA = ifelse(grepl("Namoda$", LGA), "Kaura-Namoda", LGA), new_block= ifelse(llins1== "Urban areas", 0.53, new_block))



aa_ITN_ls <- list(aa_ITN)                   
          
update_ITN_fun<- function(df, df2){
  data <- left_join(df, df2, by =c("LGA_old" = "LGA")) %>% 
    mutate(mortality_rate = mortality, kill_rate=EMOD_kill_rate, block_initial = new_block) %>% 
    dplyr::select(-c(mortality, EMOD_kill_rate, new_block, X, Unnamed..0))
}


df_updated_ITN <- map2(df, aa_ITN_ls, update_ITN_fun)

name_1 <- str_sub(str_split(names(df_updated_ITN[1]), "/", simplify = TRUE)[, 11], end=-5)
name_2 <- str_sub(str_split(names(df_updated_ITN[2]), "/", simplify = TRUE)[, 11], end=-5)
name_3 <- str_sub(str_split(names(df_updated_ITN[3]), "/", simplify = TRUE)[, 11], end=-5)
name_4 <- str_sub(str_split(names(df_updated_ITN[4]), "/", simplify = TRUE)[, 11], end=-5)

names_ls <- list(name_1, name_2, name_3, name_4)

for (i in 1:length(df_updated_ITN)) {
  write.csv(df_updated_ITN[[i]], paste0(simInDir_new, "/", names_ls[[i]], ".csv" ))
}

#write summary stats to text 
sink(file.path(subDir, "summary_stat_ITN_kill_bloc_rates_scenario_2-5.txt"))
cat("ITN kill rates")
cat("\n")
print(summary(df_updated_ITN[[1]]$kill_rate))
cat("\n")
cat("ITN block rates")
cat("\n")
print(summary(df_updated_ITN[[1]]$block_initial))
cat("\n")
cat("ITN kill rates by group")
cat("\n")
tapply(df_updated_ITN[[1]]$kill_rate, df_updated_ITN[[1]]$llins1, summary)
cat("\n")
cat("ITN block rates by group")
cat("\n")
tapply(df_updated_ITN[[1]]$block_initial, df_updated_ITN[[1]]$llins1, summary)
sink()










###############################################################################
# Maps 
###############################################################################


#map of kill rates 

LGA <- clean_LGA(file.path(DataDir,"data/Nigeria_LGAs_shapefile_191016"), file.path(DataDir, "bin/names/LGA_shp_pop_names.csv"))
LGA_list<-list(LGA)


#ITN csvs for maps 

ITN_map_cv <- map(df_updated_ITN, ~filter(.x, year == 2020| year == 2021 | year == 2022))

name_1 <- str_sub(str_split(names(ITN_map_cv[1]), "/", simplify = TRUE)[, 11], end=-5)
name_2 <- str_sub(str_split(names(ITN_map_cv[2]), "/", simplify = TRUE)[, 11], end=-5)
name_3 <- str_sub(str_split(names(ITN_map_cv[3]), "/", simplify = TRUE)[, 11], end=-5)
name_4 <- str_sub(str_split(names(ITN_map_cv[4]), "/", simplify = TRUE)[, 11], end=-5)

scen_names <-c(name_1, name_2, name_3, name_4)

ITN_map_cv<-Map(cbind, ITN_map_cv, scenario=scen_names)


#create shp file for mapping 

LGA_shp <-map2(LGA_list, ITN_map_cv, left_join)


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

map_val <- list("block_initial")
var <- list("-")
maps <- pmap(list(LGA_shp, map_val, var), map_fun)
arrange_maps <- do.call(tmap_arrange, maps)
arrange_maps

tmap_save(tm =arrange_maps, filename = paste0(subDir,"/", Sys.Date(), "_PBO_Urban_LLINs_block_initial.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



map_val <- list("kill_rate")
var <- list("-")
maps <- pmap(list(LGA_shp, map_val, var), map_fun)
arrange_maps <- do.call(tmap_arrange, maps)
arrange_maps

tmap_save(tm =arrange_maps, filename = paste0(subDir,"/", Sys.Date(), "_PBO_Urban_LLINs_kill_rate.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)