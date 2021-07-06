# This script was used to update the nigeria scenario files 6 and 7 
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
subDir <- file.path(DataDir, "results/LGA_maps/ITN_updated_ITN_efficacy_scenario_6_7")
dir.create(subDir, showWarnings = TRUE)
source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))


###############################################################################
# Data processing 
###############################################################################

# Read in ITN scenario files in need of updates (focus on scenario 2 - 5)
for (i in 1:7){
  files <- list.files(path = file.path(simInDir), pattern = "*itn_scenario", full.names = TRUE)
  files<- files[(grep('_funded', files))]
  df <- sapply(files, read_csv, simplify = F)
}

# Read in scenarios from Bea to check 

dfB <- read.csv(file.path(ScenDir, "210623_NGA_NSP_GFfr.csv")) %>%  dplyr::select(adm2, mass_llins_fund) %>% 
  mutate(adm2 = ifelse(adm2== "kaita", "Kaita", ifelse(adm2 == "kiyawa", "Kiyawa", adm2)))
table(dfB$mass_llins_fund)

#merge new scenario file and old ITN file 
df_ <- left_join(df[[1]], dfB, by =c("LGA_old"= "adm2")) #%>%  filter(year == 2020| year == 2021 | year == 2022, mass_llins_fund == 'PBO (2021)' )#use filter to make df == 774


# Net parameters from churcher et al 
beta1 = 3.41
beta2 = 5.88
beta3 = 0.78
tau = 0.5
alpha1 = 0.63
alpha2 = 4

df_ <- df_ %>%  mutate(PBO_mortality = expit(beta1 + ((beta2 *(mortality - tau))/(1 + beta3*(mortality - tau)))))
df_ <- df_ %>%  mutate(kill_rate_PBO = expit(alpha1 + alpha2 * (PBO_mortality - tau)))
df_ <- df_  %>%  mutate(EMOD_kill_rate =kill_rate_PBO * 0.807703)


df_ <- df_ %>%  mutate(kill_rate = ifelse(mass_llins_fund == 'PBO (2021)', EMOD_kill_rate, kill_rate),
                       kill_rate =ifelse(mass_llins_fund == 'IG2 & PBO (2020)', 0.9, kill_rate))


df_ <- df_ %>%  mutate(new_block_rate= ifelse(mortality < 0.5, 0.65, block_initial), 
                       block_initial = ifelse(mass_llins_fund == 'PBO (2021)', new_block_rate, block_initial),
                      block_initial = ifelse(mass_llins_fund == 'IG2 & PBO (2020)', 0.94, block_initial))
df_<-df_ %>%  dplyr::select(-c(llins_plan, PBO_mortality, kill_rate_PBO, EMOD_kill_rate, new_block_rate))

write.csv(df_, file.path(simInDir_new,  'itn_scenario6_7_funded_2020_2030.csv'))
###############################################################################
# Maps 
###############################################################################

map_fun <- function(shpfile, map_val, var) {
  scenario <- unique(na.omit(shpfile$scenario))
  tm_shape(shpfile) + #this is the health district shapfile with LLIn info
    tm_polygons(col = map_val, textNA = "No data", 
                title = "", palette = "seq", breaks=c(0, 0.1, 0.2, 
                                                      0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
    tm_layout(title =paste0(map_val,'_scenario_67', var,Sys.Date()),
              aes.palette = list(seq="RdYlBu")) 
}


#map of kill rates 

LGA <- clean_LGA(file.path(DataDir,"data/shapefiles/Nigeria_LGAs_shapefile_191016"), file.path(DataDir, "bin/names/LGA_shp_pop_names.csv"))



#ITN csvs for maps 

ITN_map_cv <- df_ %>% filter(year == 2020| year == 2021 | year == 2022)


LGA_shp <- left_join(LGA, ITN_map_cv)



map_k <- map_fun(LGA_shp, 'kill_rate', "-")
map_b <- map_fun(LGA_shp, 'block_initial', "-")

tmap_save(tm =map_k, filename = paste0(subDir,"/", Sys.Date(), "_kill_rate.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

tmap_save(tm =map_b, filename = paste0(subDir,"/", Sys.Date(), "_block_rate.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)
