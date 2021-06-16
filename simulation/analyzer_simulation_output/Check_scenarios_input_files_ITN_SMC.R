# file paths 

rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
ProjectDir<- file.path(TeamDir, "projects/hbhi_nigeria")
WorkDir <- file.path(ProjectDir, "simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v3")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")
simInDir <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v2_old")
simInDir_new <- file.path(ProjectDir, "simulation_inputs/projection_csvs/projection_v3")
ScenDir <- file.path(ProjectDir, "scenarios")
DataDir<- file.path(TeamDir, "data", "nigeria_dhs", "data_analysis")
subDir <- file.path(DataDir, "results/LGA_maps/ITN_updated_ITN_efficacy_scenario_2_5")
dir.create(subDir, showWarnings = TRUE)
source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))


###############################################################################
# Data processing  to check ITN block and kill rates 
###############################################################################


# Read in ITN scenario files in need of updates 
scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(simInDir), pattern = "*itn_increase", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}


for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(simInDir_new), pattern = "*itn_increase", full.names = TRUE)
  df_new <- sapply(files, read_csv, simplify = F)
}


state_df <- read.csv(file.path(ProjectDir, 'simulation_output', '2020_to_2025_v6', 'LGA_state.csv'))
state_ls <- list(state_df)
key_list <- map2(df_new, state_ls, left_join)


check_na<-key_list[[1]] %>%
  dplyr::select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))


key_list_cv <- map(key_list, ~filter(.x, State == "Abia", round ==1))

cols<- c("kill_rate", "block_initial", "simday", "U5_ITN_use")

Abia_ITN_parms <- lapply(key_list_cv, "[", cols)



# old data 

key_list <- map2(df, state_ls, left_join)


check_na<-key_list[[1]] %>%
  dplyr::select(everything()) %>%  # replace to your needs
  summarise_all(funs(sum(is.na(.))))


key_list_cv <- map(key_list, ~filter(.x, State == "Abia", round ==1))

cols<- c("kill_rate", "block_initial", "simday", "U5_ITN_use")

Abia_ITN_parms_old <- lapply(key_list_cv, "[", cols)



check <- cbind(Abia_ITN_parms_old[[1]], Abia_ITN_parms[[1]])

names(check)<- make.unique(names(check))


diff_fun <- function(df1, df2){
  df <- cbind(df1,df2) 
    names(df)<- make.unique(names(df))
    df_fin<- df %>% mutate(kill_rate_check = kill_rate - kill_rate.1, 
                     block_rate_check = block_initial - block_initial.1,
                     simday_check = simday - simday.1,
                     U5_ITN_use_check = U5_ITN_use - U5_ITN_use.1)
}


diff_Abia_old_new<- map2(Abia_ITN_parms, Abia_ITN_parms_old, diff_fun)


p_10_itn<- ggplot(data = diff_Abia_old_new[[1]], aes(x = block_initial, y = block_initial.1))+
  geom_point(shape=21,  size=6, fill="#EC0B88")+
  xlab("Block Rate in New Simulation")+
  ylab("Block Rate in Old Simulation")+
  ggtitle("ITN increase 10 percent")



p_20_itn<- ggplot(data = diff_Abia_old_new[[2]], aes(x = block_initial, y = block_initial.1))+
  geom_point(shape=21,  size=6, fill="#EC0B88")+
  xlab("Block Rate in New Simulation")+
  ylab("Block Rate in Old Simulation")+
  ggtitle("ITN increase 20 percent")


p_30_itn<- ggplot(data = diff_Abia_old_new[[3]], aes(x = block_initial, y = block_initial.1))+
  geom_point(shape=21,  size=6, fill="#EC0B88")+
  xlab("Block Rate in New Simulation")+
  ylab("Block Rate in Old Simulation")+
  ggtitle("ITN increase 30 percent")




p_80_itn<- ggplot(data = diff_Abia_old_new[[4]], aes(x = block_initial, y = block_initial.1))+
  geom_point(shape=21,  size=6, fill="#EC0B88")+
  xlab("Block Rate in New Simulation")+
  ylab("Block Rate in Old Simulation")+
  ggtitle("ITN increase 80 percent")


all_itn<- ggarrange(p_10_itn, p_20_itn, p_30_itn, p_80_itn)

pdf(file=paste0(simInDir_new, "/", Sys.Date(), "_Abia_block_rates_previoussimvs_newsimInDir.pdf"))
plot(all_itn)
dev.off()


###############################################################################
# Data processing  to check malariAdjustment 
###############################################################################

