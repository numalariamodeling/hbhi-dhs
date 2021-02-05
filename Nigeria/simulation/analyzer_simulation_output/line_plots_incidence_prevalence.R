# Plots of projected incidence, prevalence, and mortality 

# create baseline file 
rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
WorkDir <- file.path(TeamDir, "projects/hbhi_nigeria/simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")

source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))
library(ggpubr)

ProcessDir <- file.path(WorkDir, "2020_to_2030_v2")
box_filepath = "C:/Users/ido0493/Box/NU-malaria-team/projects"
box_hbhi_filepath = paste0(box_filepath, '/hbhi_nigeria')
print_path <- file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/figurespng")


# read in data 

scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_2020_2030.csv", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}

#read in 2019 annual indicators

df_2019 <- read_csv(file.path(WorkDir, "2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc 1", "annual_indicators_2011_2020.csv")) %>% 
rename(PfPR = PfPR_all_ages, U5_PfPR = PfPR_U5, incidence = incidence_all_ages, U5_incidence = incidence_U5)

df[['C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2030_v2/NGA projection scenario 0/annual_indicators_2020_2030.csv']] <- df_2019

df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR, U5_PfPR,  incidence, U5_incidence, death_rate_mean, U5_death_rate_mean) %>% 
  mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) 

df_2019_val <- df[df$year ==2019,] %>%  uncount(7) %>%  mutate(number = as.character(c(1:7)), scenario = str_sub(scenario, end = -2),
                                                               scenario = paste0(scenario,"", number)) %>%  dplyr::select(-number)

df <- rbind(df, df_2019_val) 

line_plot <- function(y, ylab) {
  p<-ggplot(df, aes(x = year, y = y, group = scenario, color = scenario)) + 
  geom_line()+ 
  theme_minimal()+
  #theme(legend.position = "none")+
    scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) +
    ylab(ylab)
}


pfpr <- line_plot(df$PfPR, "all age PfPR")

u5_pfpr <- line_plot(df$U5_PfPR, "U5 PfPR")

incidence <- line_plot(df$incidence, "all age incidence")

u5_incidence <- line_plot(df$U5_incidence, "U5 incidence")

death <- line_plot(df$death_rate_mean, "all age death rate")

death_U5 <- line_plot(df$U5_death_rate_mean, "U5 death rate")



all_indicators <- ggarrange(pfpr, incidence,death,u5_pfpr, u5_incidence,death_U5, ncol =3, nrow = 3, common.legend = TRUE, legend = "bottom")
ggsave(paste0(print_path, '/', 'pfpr_incidence_mortality_line_plot.pdf'), all_indicators, width=13, height=13)

