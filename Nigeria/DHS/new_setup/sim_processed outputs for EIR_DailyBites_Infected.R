library(ggpubr)
# reading the scenarios 

rm(list=ls())

file <- read.files("\\.csv$", WorkDir, read.csv) 



scen2 <- read.csv(paste0(WorkDir, "/NGA projection scenario 2/All_Age_Monthly_Cases.csv"),colClasses="character",na.strings="?" )
head(scen2)
scen2$date <- as.Date(scen2$date,format="%Y-%m-%d")

past <- read.csv(paste0(WorkDir, "/NGA projection scenario 2010_2020/All_Age_Monthly_Cases.csv"),colClasses="character",na.strings="?" )
head(past)
past$date <- as.Date(past$date,format="%Y-%m-%d")

LGA_pop <- read.csv(paste0(ProjectDir, "/nigeria_LGA_pop.csv")) %>% dplyr::select(State, LGA, geopode.pop) %>%  filter(State == "Gombe")
head(LGA_pop)


#present
# month_df <- function(data){}
#  data %>% dplyr::select(date, Infected, Daily.Bites.per.Human,Daily.EIR, Run_Number, LGA) %>% 
#    mutate_all(as.character()) %>% 
#    mutate(date = as.Date(date, f))
#   filter(date <="2030-12-31") %>% 
#   group_by(date) %>%
#   summarise(mean_bites = mean(as.numeric(Daily.Bites.per.Human)), mean_EIR= mean(as.numeric(Daily.EIR)), mean_Infected =mean(as.numeric(Infected)))
# }

df_2 <- scen2 %>% dplyr::select(date, Infected, Daily.Bites.per.Human,Daily.EIR, Run_Number, LGA) %>% 
  filter(date <="2030-12-31") %>% 
  group_by(date) %>%
  summarise(mean_bites = mean(as.numeric(Daily.Bites.per.Human)), mean_EIR= mean(as.numeric(Daily.EIR)), mean_Infected =mean(as.numeric(Infected)))





df_2_year <- scen2 %>% dplyr::select(date, Infected, Daily.Bites.per.Human,Daily.EIR, Run_Number, LGA) %>% 
  separate(date, c("Year", "Month", "Day"), sep = "-") %>% 
  filter(Year != 2031) %>% 
  group_by(Year) %>% 
  summarise(mean_bites = mean(as.numeric(Daily.Bites.per.Human)), mean_EIR= mean(as.numeric(Daily.EIR)), mean_Infected =mean(as.numeric(Infected))) %>% 
  ungroup() %>% 
mutate(Year = as.numeric(Year))

head(df_2_year)



df_2_PfPR <- scen2 %>% dplyr::select(date, LGA, Statistical.Population, PfHRP2.Prevalence,Run_Number) %>% 
  separate(date, c("Year", "Month", "Day"), sep = "-") %>% 
  filter(Year != 2031) %>% 
  group_by(Year, LGA, Run_Number) %>%
  summarise(PfHRP2.Prevalence = mean(as.numeric(PfHRP2.Prevalence)), Statistical.Population = mean(as.numeric(Statistical.Population))) %>% 
  group_by(Year, LGA) %>% 
  summarise(mean_prevalence = mean(PfHRP2.Prevalence), mean_stat_pop = mean(Statistical.Population)) %>% 
  left_join(LGA_pop) %>% 
  mutate(positives = mean_prevalence * geopode.pop) %>% 
  group_by(Year) %>% 
  summarise(positives = sum(positives), population= sum(geopode.pop)) %>% 
  mutate(PfPR = positives/population) %>% 
  mutate(Year = as.numeric(Year))
  
  

head(df_2_PfPR)


#past 
df_past<- past %>% dplyr::select(date, Infected, Daily.Bites.per.Human,Daily.EIR, Run_Number, LGA) %>% 
  filter(date <="2019-12-01") %>% 
  group_by(date) %>%
  summarise(mean_bites = mean(as.numeric(Daily.Bites.per.Human)), mean_EIR= mean(as.numeric(Daily.EIR)), mean_Infected =mean(as.numeric(Infected)))

head(df_past)

df_past_year <- past %>% dplyr::select(date, Infected, Daily.Bites.per.Human,Daily.EIR, Run_Number, LGA) %>% 
  separate(date, c("Year", "Month", "Day"), sep = "-") %>% 
  filter(Year != 2020) %>% 
group_by(Year) %>% 
  summarise(mean_bites = mean(as.numeric(Daily.Bites.per.Human)), mean_EIR= mean(as.numeric(Daily.EIR)), mean_Infected =mean(as.numeric(Infected))) %>% 
  ungroup() %>% 
  mutate(Year = as.numeric(Year))



head(df_past_year)


df_past_PfPR <- past %>% dplyr::select(date, LGA, Statistical.Population, PfHRP2.Prevalence,Run_Number) %>% 
  separate(date, c("Year", "Month", "Day"), sep = "-") %>% 
  filter(Year != 2020) %>% 
  group_by(Year, LGA, Run_Number) %>%
  summarise(PfHRP2.Prevalence = mean(as.numeric(PfHRP2.Prevalence)), Statistical.Population = mean(as.numeric(Statistical.Population))) %>% 
  group_by(Year, LGA) %>%
  summarise(mean_prevalence = mean(PfHRP2.Prevalence), mean_stat_pop = mean(Statistical.Population)) %>%
  left_join(LGA_pop) %>% 
  mutate(positives = mean_prevalence * geopode.pop) %>% 
  group_by(Year) %>% 
  summarise(positives = sum(positives), population= sum(geopode.pop)) %>% 
  mutate(PfPR = positives/population) %>% 
  mutate(Year = as.numeric(Year))
head(df_past_PfPR)


#join 

past_scen_2 <- rbind(df_past, df_2)
head(past_scen_2)


past_scen_2_year <- rbind(df_past_year, df_2_year)
head(past_scen_2_year)

past_scen_2_PfPR_year <- rbind(df_past_PfPR ,df_2_PfPR)
head(past_scen_2_PfPR_year)



#plots


infected_month <- ggplot(past_scen_2, aes(x = date, y = mean_Infected)) + 
  geom_line(color = "blue") +
  xlab('Time') +
  ylab('Infected (aggregated by month)')


infected_year<- ggplot(past_scen_2_year, aes(x = Year, y = mean_Infected,  group =1)) + 
  geom_line(color = "blue") +
  geom_point()+
  scale_x_continuous(breaks = seq(2010, 2030, by = 5))+
  xlab('Time') +
  ylab('Infected (aggregated by year)')

bites_month <- ggplot(past_scen_2, aes(x = date, y = mean_bites)) + 
  geom_line(color = "blue") +
  xlab('Time') +
  ylab('Daily Bites (aggregated by month)')


bites_year<- ggplot(past_scen_2_year, aes(x = Year, y = mean_bites,  group =1)) + 
  geom_line(color = "blue") +
  geom_point()+
  scale_x_continuous(breaks = seq(2010, 2030, by = 5))+
  xlab('Time') +
  ylab('Daily Bites (aggregated by year)')



EIR_month <- ggplot(past_scen_2, aes(x = date, y = mean_EIR)) + 
  geom_line(color = "blue") +
  xlab('Time') +
  ylab('EIR(aggregated by month)')


EIR_year<- ggplot(past_scen_2_year, aes(x = Year, y = mean_EIR,  group =1)) + 
  geom_line(color = "blue") +
  geom_point()+
  scale_x_continuous(breaks = seq(2010, 2030, by = 5))+
  xlab('Time') +
  ylab('EIR(aggregated by year)')



PfPR_year<- ggplot(past_scen_2_PfPR_year, aes(x = Year, y = PfPR,  group =1)) + 
  geom_line(color = "blue") +
  geom_point()+
  scale_x_continuous(breaks = seq(2010, 2030, by = 5))+
  xlab('Time') +
  ylab('PfPR(aggregated by year)')


pdf(file=paste0(WorkDir, "/plots/Mean_infected_month_year.pdf"))
ggarrange(infected_month, infected_year, PfPR_year,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
dev.off()


pdf(file=paste0(WorkDir, "/plots/Mean_Daily _Bites_month_year.pdf"))
ggarrange(bites_month, bites_year, PfPR_year,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
dev.off()


pdf(file=paste0(WorkDir, "/plots/Mean_EIR_month_year.pdf"))
ggarrange(EIR_month, EIR_year, PfPR_year,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
dev.off()