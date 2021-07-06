# This script was used to update the nigeria scenario files from LLINS to PBO 
# Date: 06-25-2021



# file paths 

rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")
simInDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v3")
simInDir_new <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v4/ITN")
ScenDir <- file.path(ProjectDir, "scenarios")
DataDir<- file.path(TeamDir, "data", "nigeria_dhs", "data_analysis")
subDir <- file.path(DataDir, "results/LGA_maps/ITN_updated_ITN_efficacy_scenario_2_5")
dir.create(subDir, showWarnings = TRUE)
source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))


###############################################################################
# Data processing 
###############################################################################


# Read in scenarios from Bea to check 

df <- read.csv(file.path(ScenDir, "210623_NGA_NSP_GFfr.csv")) %>%  dplyr::select(adm2, mass_llins_nsp)
table(df$mass_llins_nsp)

# Read in ITN scenario files in need of updates (focus on scenario 2 - 5)
for (i in 1:7){
  files <- list.files(path = file.path(simInDir), pattern = "*itn_scenario", full.names = TRUE)
  files<- files[(grep('_increase', files))]
  df <- sapply(files, read_csv, simplify = F)
}



# Net parameters from churcher et al 
beta1 = 3.41
beta2 = 5.88
beta3 = 0.78
tau = 0.5
alpha1 = 0.63
alpha2 = 4

#new kill and block rates 
df <- df %>% map(~mutate(., PBO_mortality = expit(beta1 + ((beta2 *(mortality_rate - tau))/(1 + beta3*(mortality_rate- tau))))))
df <- df %>%  map(~mutate(.,  kill_rate_PBO = expit(alpha1 + alpha2 * (PBO_mortality - tau))))
df <- df  %>%  map(~mutate(., EMOD_kill_rate =kill_rate_PBO * 0.807703))
df <- df %>%  map(~mutate(., diff_kill_rate = kill_rate - EMOD_kill_rate))                         
                
df <- df %>% map(~mutate(., new_block_rate = ifelse(kill_rate < 0.5, 0.65, mortality_rate * 1.28))) #checking how a scale factor of 1.28 works
df <- df %>% map(~mutate(., diff_block_rate = block_initial - new_block_rate))
df <- df %>% map(~mutate(., block_initial = ifelse(llins1 == 'Urban areas', new_block_rate, block_initial)))#using the newly block rates only for urban areas
df <- df %>% map(~mutate(., kill_rate = ifelse(llins1 == 'Urban areas', EMOD_kill_rate, kill_rate)))
df <- df %>% map(~dplyr::select(., -c(X1, X1_1, PBO_mortality, kill_rate_PBO, EMOD_kill_rate, diff_kill_rate, new_block_rate, diff_block_rate)))




#saving files 
name_1 <- str_sub(str_split(names(df[1]), "/", simplify = TRUE)[, 11], end=-5)
name_2 <- str_sub(str_split(names(df[2]), "/", simplify = TRUE)[, 11], end=-5)
name_3 <- str_sub(str_split(names(df[3]), "/", simplify = TRUE)[, 11], end=-5)
name_4 <- str_sub(str_split(names(df[4]), "/", simplify = TRUE)[, 11], end=-5)

names_ls <- list(name_1, name_2, name_3, name_4)

for (i in 1:length(df)) {
  write.csv(df[[i]], paste0(simInDir_new, "/", names_ls[[i]], ".csv" ))
}

#write summary stats to text 
sink(file.path(subDir, "summary_stat_ITN_kill_bloc_rates_scenario_2-5.txt"))
cat("ITN kill rates")
cat("\n")
print(summary(df[[1]]$kill_rate))
cat("\n")
cat("ITN block rates")
cat("\n")
print(summary(df[[1]]$block_initial))
cat("\n")
cat("ITN kill rates by group")
cat("\n")
tapply(df[[1]]$kill_rate, df[[1]]$llins1, summary)
cat("\n")
cat("ITN block rates by group")
cat("\n")
tapply(df[[1]]$block_initial, df[[1]]$llins1, summary)
cat("\n")
cat("ITN block rates by mortality group")
cat("\n")
tapply(df[[1]]$block_initial, df[[1]]$mortality_rate, summary)
sink()










###############################################################################
# Maps 
###############################################################################


#map of kill rates 

LGA <- clean_LGA(file.path(DataDir,"data/shapefiles/Nigeria_LGAs_shapefile_191016"), file.path(DataDir, "bin/names/LGA_shp_pop_names.csv"))
LGA_list<-list(LGA)


#ITN csvs for maps 

ITN_map_cv <- map(df, ~filter(.x, year == 2020| year == 2021 | year == 2022))

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

tmap_save(tm =arrange_maps, filename = paste0(subDir,"/", Sys.Date(), "_PBO_block_initial.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



map_val <- list("kill_rate")
var <- list("-")
maps <- pmap(list(LGA_shp, map_val, var), map_fun)
arrange_maps <- do.call(tmap_arrange, maps)
arrange_maps

tmap_save(tm =arrange_maps, filename = paste0(subDir,"/", Sys.Date(), "_PBO_kill_rate.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



