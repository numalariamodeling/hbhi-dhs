rm(list = ls())
user = Sys.getenv("USERNAME")
user_path = file.path("C:/Users",user)
box_hbhi_filepath = paste(user_path, '/Box/NU-malaria-team/projects/hbhi_nigeria', sep='')
sim_hbhi_filepath = paste(user_path, '/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_output/', sep='')
sim_output_new = paste(sim_hbhi_filepath, '/2010_to_2020_v11/NGA 2010-20 burnin_hs+itn+smc', sep='')
sim_output_old = paste(sim_hbhi_filepath, '/2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc', sep='')

#check prevalence 
all_age_cases = read.csv(file.path(sim_output_old, 'All_Age_Monthly_Cases.csv')) %>%  
  mutate(year = year(date)) %>%  group_by(year, LGA, Run_Number) %>%  summarise(pfpr = mean(PfHRP2.Prevalence))

prevalence = all_age_cases %>%  group_by(year, LGA) %>%  summarise(pfpr = mean(pfpr))

all_age_cases_new = read.csv(file.path(sim_output_new, 'All_Age_Monthly_Cases.csv'))%>%  
  mutate(year = year(date)) %>%  group_by(year, LGA, Run_Number) %>%  summarise(pfpr = mean(PfHRP2.Prevalence))

prevalence_new = all_age_cases %>%  group_by(year, LGA) %>%  summarise(pfpr = mean(pfpr))

all = left_join(prevalence, prevalence_new, by =c('year','LGA'))
all$diff <- all$pfpr.x - all$pfpr.y

#show one lGA
pfpr_biu = filter(prevalence, LGA == 'Biu')
pfpr_biu$category = 'old'
pfpr_biu_new = filter(prevalence_new, LGA == 'Biu')
pfpr_biu_new$category = 'new'

pfpr = rbind(pfpr_biu, pfpr_biu_new)

pfpr_plot<- ggplot(data=pfpr, aes(year, pfpr,color = category))+
  geom_line(position=position_jitter(w=0.02, h=0))+
  theme_minimal()
  


# check new clinical cases 
all_age_cases = read.csv(file.path(sim_output_old, 'All_Age_Monthly_Cases.csv')) %>%  
  mutate(year = year(date), month = month(date)) %>%  group_by(month, year, LGA, Run_Number) %>%  summarise(new_cases = mean(New.Clinical.Cases))

new_clinical_cases = all_age_cases %>%  group_by(year, LGA, Run_Number) %>%  summarise(new_cases = sum(new_cases))

new_clinical_cases = all_age_cases %>%  group_by(year, LGA) %>%  summarise(new_cases = mean(new_cases))


all_age_cases_new = read.csv(file.path(sim_output_new, 'All_Age_Monthly_Cases.csv')) %>%  
  mutate(year = year(date), month = month(date)) %>%  group_by(month, year, LGA, Run_Number) %>%  summarise(new_cases = mean(New.Clinical.Cases))

new_clinical_cases_new = all_age_cases %>%  group_by(year, LGA, Run_Number) %>%  summarise(new_cases = sum(new_cases))

new_clinical_cases_new = all_age_cases %>%  group_by(year, LGA) %>%  summarise(new_cases = mean(new_cases))

all = left_join(new_clinical_cases, new_clinical_cases_new, by =c('year','LGA'))
all$diff <- all$new_cases.x  - all$new_cases.y
summary(all$diff)

#show one lGA
cases_biu = filter(new_clinical_cases, LGA == 'Biu')
cases_biu$category = 'old'
cases_biu_new = filter(new_clinical_cases_new, LGA == 'Biu')
cases_biu_new$category = 'new'

cases = rbind(cases_biu, cases_biu_new)

cases_plot = ggplot(data=cases, aes(year, new_cases,color = category))+
  geom_line(position=position_jitter(w=0.02, h=0))+
  theme_minimal()

ggsave(paste0(sim_hbhi_filepath, '/', 'v10_v11_burnin_comparisons/',  Sys.Date(),  '_comparison_burnin_2010_2020_pfpr_biu_LGA.pdf'), pfpr_plot)
ggsave(paste0(sim_hbhi_filepath, '/', 'v10_v11_burnin_comparisons/', Sys.Date(),  '_comparison_burnin_2010_2020_new_clinical_cases_biu_LGA.pdf'), cases_plot)
