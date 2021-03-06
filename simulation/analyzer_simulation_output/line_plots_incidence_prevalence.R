# Plots of projected incidence, prevalence, and mortality 

# create baseline file 
rm(list = ls())
TeamDir <-"C:/Users/ido0493/Box/NU-malaria-team"
WorkDir <- file.path(TeamDir, "projects/hbhi_nigeria/simulation_output")
ProcessDir <- file.path(WorkDir, "2020_to_2030_v4")
ScriptDir <- file.path(TeamDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")

source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))
library(ggpubr)
library(cowplot)


box_filepath = "C:/Users/ido0493/Box/NU-malaria-team/projects"
box_hbhi_filepath = paste0(box_filepath, '/hbhi_nigeria')
print_path <- file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/figures")


# read in data for all scenarios

all_df  = list()

names = c("mean", "0", "1", "2", "3", "4")


for (i in 1:length(names)){
scen_dat <- read.csv(file.path(ProcessDir, "scenario_adjustment_info.csv"))

  for (row in 1:nrow(scen_dat)){
  files <- list.files(path = file.path(ProcessDir, scen_dat[, "ScenarioName"]), pattern = paste0("*annual_indicators_2020_2030_", names[i], ".csv"), full.names = TRUE)
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
df$run_number <- paste0('run number', " ", names[i]) 

all_df[[i]] <- df

}


df_com = plyr::ldply(all_df, rbind) %>% filter(run_number != "run number mean")  %>% 
group_by(scenario, year) %>%  summarise_if(is.numeric, list(min = min, max = max), na.rm = TRUE)


df_mean = plyr::ldply(all_df, rbind) %>% filter(run_number == "run number mean")  %>% 
  dplyr::select(-c(.id, run_number))

df = left_join(df_com, df_mean, by =c('scenario', 'year')) %>%  dplyr::select(scenario, year, sort(names(.)))
  
#GTS targets 
GTS_incidence_all_ages_2020 <- df[df$year == "2015", "incidence_all_ages"] * 0.6
GTS_incidence_all_ages_2025 <- df[df$year == "2015", "incidence_all_ages"] * 0.25
GTS_incidence_all_ages_2030 <- df[df$year == "2015", "incidence_all_ages"] * 0.10

GTS_deaths_all_ages_2020 <- df[df$year == "2015", "death_rate_mean_all_ages"] * 0.6
GTS_deaths_all_ages_2025 <- df[df$year == "2015", "death_rate_mean_all_ages"] * 0.25
GTS_deaths_all_ages_2030 <- df[df$year == "2015", "death_rate_mean_all_ages"] * 0.10



GTS_df <- data.frame(year=c(2020, 2025, 2030),
                     gts_incidence_all_ages = c(GTS_incidence_all_ages_2020[1,1], GTS_incidence_all_ages_2025[1,1], GTS_incidence_all_ages_2030[1,1]),
                      gts_death_rate_mean_all_ages = c(GTS_deaths_all_ages_2020[1,1], GTS_deaths_all_ages_2025[1, 1], GTS_deaths_all_ages_2030[1, 1]))

df <- left_join(df, GTS_df, by =c('year'))

line_plot_gts <- function(y, gts, ylab, ymin, ymax, title) {
  p<-ggplot(df, aes(x = year, group = scenario, color =scenario)) + 
  geom_line(aes(y = y), size =0.3)+
    geom_ribbon(aes(ymin =ymin, ymax =ymax, fill =NA), linetype=2, alpha=0.1, size =0.3)+
    geom_point(aes(y =gts), color ='black', size =2)+
    scale_color_manual(values = c("#5a5757", '#913058', "#F6851F", "#00A08A", "#D61B5A", "#5393C3", "#98B548", "#8971B3"))+
    scale_fill_manual(values = c( "#5a5757", '#913058', "#F6851F", "#00A08A", "#D61B5A", "#5393C3", "#98B548", "#8971B3"))+
  theme_minimal()+
    theme(legend.direction = "vertical", legend.title = element_blank(),
          plot.title=element_text(size=, color = "black", face = "bold", hjust=0.5),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.x = element_text(size = 7.5, colour = "black", face = "bold"))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) +
    labs(x = '', y = ylab, col= "INTERVENTION SCENARIOS", title =title)
}



line_plot <- function(y,ylab, ymin, ymax, title) {
  p<-ggplot(df, aes(x = year, group = scenario, color =scenario)) + 
    geom_ribbon(aes(ymin =ymin, ymax =ymax, fill =NA), linetype=2, alpha=0.1, size =0.3)+
    geom_line(aes(y = y), size =0.3)+
    scale_color_manual(labels= c('', 'Business as usual (Scenario 1)', 'NMSP with ramping up to 80% coverage (Scenario 2)',
                       'NMSP with instantaneous 10% improved coverage (Scenario 3)',
                       'NMSP with instantaneous 20% improved coverage (Scenario 4)', 
                       'NMSP with instantaneous 30% improved coverage (Scenario 5)',
                       'Budget-prioritized plan with coverage increases at historical rate and SMC in 221 LGAs (Scenario 6)',
                       'Budget-prioritized plan with coverage increases at historical rate and SMC in 310 LGAs (Scenario 7)'), 
                       values = c( "#5a5757", '#913058', "#F6851F", "#00A08A", "#D61B5A", "#5393C3", "#98B548", "#8971B3"))+
    scale_fill_manual(values = c( "#5a5757", '#913058', "#F6851F", "#00A08A", "#D61B5A", "#5393C3", "#98B548", "#8971B3"))+
    theme_minimal()+
    theme(legend.direction = "vertical", 
          legend.box.background = element_blank(),
          plot.title=element_text(size=, color = "black", face = "bold", hjust=0.5),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          #axis.ticks.x = element_line(size = 0.5, colour = "black"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          strip.text.x = element_text(size = 7.5, colour = "black", face = "bold"))+
    scale_y_continuous(expand = c(0, 0), limits = c(0,NA)) +
    labs(x = '', y = ylab, col= "INTERVENTION SCENARIOS", title =title)
}

all_ages_title <- expression(paste(atop(textstyle(bold("all ages")), 
                                        atop(textstyle("all age PfPR by microscopy,"),
                                             textstyle("annual average")))))

u5_title <- expression(paste(atop(textstyle(bold("children under the age of five")), 
                               atop(textstyle("U5 PfPR by microscopy,"),
                                    textstyle("annual average")))))

#plots 
pfpr <- line_plot(df$PfPR_all_ages, all_ages_title, df$PfPR_all_ages_min, df$PfPR_all_ages_max, 'Parasite Prevalence')

u5_pfpr <- line_plot(df$PfPR_U5, u5_title, df$PfPR_U5_min, df$PfPR_U5_max, '')

incidence <- line_plot_gts(df$incidence_all_ages, df$gts_incidence_all_ages, "all age annual incidence per 1000", df$incidence_all_ages_min, df$incidence_all_ages_max, 'Uncomplicated malaria incidence')

u5_incidence <- line_plot(df$incidence_U5,  "U5 annual incidence per 1000", df$incidence_U5_min, df$incidence_U5_max, '')

death <- line_plot_gts(df$death_rate_mean_all_ages, df$gts_death_rate_mean_all_ages,'all age annual death per 1000',  df$death_rate_mean_all_ages_min, df$death_rate_mean_all_ages_max, "Malaria mortality")

death_U5 <- line_plot(df$death_rate_mean_U5, "U5 annual death per 1000", df$death_rate_mean_U5_min, df$death_rate_mean_U5_max, '')

legend <- get_legend(
  pfpr + 
    guides(color = guide_legend(override.aes=list(fill=NA), ncol = 2)) +
    theme(legend.position = "bottom", legend.background = element_blank(), legend.box.background = element_rect(colour = "black"))
)


#top_row <- plot_grid(pfpr+theme(legend.position="none"), incidence+ theme(legend.position="none"), death+theme(legend.position="none"), labels = c('A', 'B', 'C'), nrow =1)

first_col <- plot_grid(pfpr+theme(legend.position="none"), u5_pfpr+theme(legend.position="none"), labels = c('A'), nrow =2)

second_col <- plot_grid(incidence+theme(legend.position="none"), u5_incidence+theme(legend.position="none"), labels = c('B'),nrow =2)

third_col <- plot_grid(death+theme(legend.position="none"), death_U5+theme(legend.position="none"), labels = c('C'),nrow =2)

#bottom_row <- plot_grid(u5_pfpr+theme(legend.position="none"), u5_incidence+ theme(legend.position="none"), death_U5+theme(legend.position="none"), nrow =1)

indicator_plot <-  plot_grid(first_col, second_col, third_col, nrow =1)

final_plot <- plot_grid(indicator_plot, legend, nrow =2, rel_heights = c(1, 0.3))
final_plot

#all_indicators <- ggarrange(pfpr, incidence,death,u5_pfpr, u5_incidence,death_U5, ncol =3, nrow = 3, common.legend = TRUE, legend = "bottom")
ggsave(paste0(print_path, '/', Sys.Date(),  '_hbhi_nigeria_burden_projections.pdf'), final_plot)

