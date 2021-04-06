# Plots of projected incidence, prevalence, and mortality 

# create baseline file 
rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
WorkDir <- file.path(TeamDir, "projects/hbhi_nigeria/simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v3")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")

source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))
library(ggpubr)


box_filepath = "C:/Users/ido0493/Box/NU-malaria-team/projects"
box_hbhi_filepath = paste0(box_filepath, '/hbhi_nigeria')
print_path <- file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/figures")


# read in data for just scenario 2 - 5 

scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = "*annual_indicators_2020_2030.csv", full.names = TRUE)
  df <- sapply(files, read_csv, simplify = F)
}


#read in 2019 annual indicators and scenarios and bind with all scenarios 

df_2019 <- read_csv(file.path(WorkDir, "2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc", "annual_indicators_2011_2020.csv")) %>% 
  rename(death_rate_mean_all_ages =death_rate_mean,   death_rate_mean_U5=U5_death_rate_mean)

df[['C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/2020_to_2030_v3/NGA projection scenario 0/annual_indicators_2020_2030.csv']] <- df_2019

df <- plyr::ldply(df, rbind)%>%  dplyr::select(.id,year, PfPR_all_ages, PfPR_U5,  incidence_all_ages, incidence_U5, 
                                                   death_rate_mean_all_ages, death_rate_mean_U5) %>% 
  mutate(scenario = str_split(.id, "/", simplify = T)[, 10]) 


df_2019_val <- df[df$year ==2019,] %>%  uncount(7) %>%  mutate(number = as.character(c(1:7)), scenario = str_sub(scenario, end = -2),
                                                               scenario = paste0(scenario,"", number)) %>%  dplyr::select(-number)

df <- rbind(df, df_2019_val) 

# 
# GTS_incidence_all_ages_2020 <- df[df$year == "2015", "incidence_all_ages"] * 0.6
# 
# GTS_incidence_all_ages_2025 <- df[df$year == "2015", "incidence_all_ages"] * 0.25
# 
# GTS_incidence_all_ages_2030 <- df[df$year == "2015", "incidence_all_ages"] * 0.10
# 
# 
# GTS_deaths_all_ages_2020 <- df[df$year == "2015", "death_rate_mean_all_ages"] * 0.6
# 
# GTS_deaths_all_ages_2025 <- df[df$year == "2015", "death_rate_mean_all_ages"] * 0.25
# 
# GTS_deaths_all_ages_2030 <- df[df$year == "2015", "death_rate_mean_all_ages"] * 0.10
# 
# 
# 
# GTS_df <- data.frame(.id = "GTS", year=c("2020", "2025", "2030"),  PfPR_all_ages = NA , PfPR_U5= NA,
#                      incidence_all_ages = c(GTS_incidence_all_ages_2020, GTS_incidence_all_ages_2025, GTS_incidence_all_ages_2030),
#                      incidence_U5=NA, death_rate_mean_all_ages = c(GTS_deaths_all_ages_2020, GTS_deaths_all_ages_2025, GTS_deaths_all_ages_2030),
#                      death_rate_mean_U5 = NA, scenario = "GTS")
# 
# df <- rbind(df, GTS_df)

line_plot <- function(y, ylab) {
  p<-ggplot(df, aes(x = year, y = y, group = scenario, color = scenario)) + 
  geom_line()+
  theme_minimal()+
  #theme(legend.position = "none")+
    scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) +
    ylab(ylab)
}


pfpr <- line_plot(df$PfPR_all_ages, "all age PfPR")

u5_pfpr <- line_plot(df$PfPR_U5, "U5 PfPR")

incidence <- line_plot(df$incidence_all_ages, "all age incidence")

u5_incidence <- line_plot(df$incidence_U5, "U5 incidence")

death <- line_plot(df$death_rate_mean_all_ages, "all age death rate")

death_U5 <- line_plot(df$death_rate_mean_U5, "U5 death rate")



all_indicators <- ggarrange(pfpr, incidence,death,u5_pfpr, u5_incidence,death_U5, ncol =3, nrow = 3, common.legend = TRUE, legend = "bottom")
ggsave(paste0(print_path, '/', Sys.Date(),  '_u5_incidence_line_plot_GTS_target.pdf'), u5_incidence, width=13, height=13)

