library(awtools)
library(ggplot2)
library(tidyr)
library(cowplot)

path <- "C:/Users/ido0493/Box/NU-malaria-team/presentations/Ifeoma archive/210301_Team_meeting"
df <- read.csv(file.path(path, "malaria_cases_deaths_nigeria.csv"))

###############################################################################
# cases
###############################################################################

cases<-df %>% dplyr::select(Year, cases_per_1000, GTS.target_cases) %>% 
  pivot_longer(!Year, names_to="cases", values_to ="count")

#sppalette
#ppalette
cases_plot<-ggplot(data = cases, aes(x = Year, y = count, fill=cases))+
  geom_line(color="#9F248F") +
  geom_point(shape=21,  size=6) +
  scale_fill_manual(values = c("#9F248F", "#FFCE4E"), labels = c("Malaria incidence", "GTS Targets"))+
  expand_limits(y=0)+
  theme_half_open()+
  background_grid()+
  theme(legend.title = element_blank())+
  ylab("Malaria cases per 1000 population at risk")
cases_plot


pdf(file=paste0(path, "/", "malaria_cases_GTS_targets_WMR2020.pdf"))
plot(cases_plot)
dev.off()



###############################################################################
# deaths
###############################################################################

deaths<-df %>% dplyr::select(Year, deaths_per_100000, GTS.target_deaths) %>% 
  pivot_longer(!Year, names_to="deaths", values_to ="count") 

deaths_plot<-ggplot(data = deaths, aes(x = Year, y = count, fill=deaths))+
  geom_line(color="#EC0B88") +
  geom_point(shape=21,  size=6) +
  scale_fill_manual(values = c("#EC0B88", "#FFCE4E"), labels = c("Malaria deaths", "GTS Targets"))+
  expand_limits(y=0)+
  theme_half_open()+
  background_grid()+
  theme(legend.title = element_blank())+
  ylab("Malaria deaths per 100 000 population at risk")
deaths_plot

pdf(file=paste0(path, "/", "malaria_deaths_GTS_targets_WMR2020.pdf"))
plot(deaths_plot)
dev.off()

