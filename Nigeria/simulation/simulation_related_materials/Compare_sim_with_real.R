
# set working directory 
rm(list=ls())

# setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

setwd("~/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")

x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer", "plotrix", "ggrepel", "plotrix","sf", "shinyjs", "tmap", "knitr", "labelled", "rlang", "arules", "foreign", "glue", "fuzzyjoin")
lapply(x, library, character.only = TRUE) #applying the library function to packages

require(dplyr)
require(readr)
require(fuzzyjoin)

source("src/Nigeria functions.R")




# Getting the sim files ready for plotting 

sim.dat <- read.csv("bin/All_Age_Monthly_Cases_sim.csv", colClasses = "character")

sim.dat$date2 <- as.Date(sim.dat$date, format = "%Y-%m-%d")

sim.dat$Received_NMF_Treatment <- as.numeric(sim.dat$Received_NMF_Treatment)

sim.dat$Received_Treatment <- as.numeric(sim.dat$Received_Treatment)

match("Received_Treatment",names(sim.dat))

sim.dat$cases <- rowSums(sim.dat[, c(6, 10)], na.rm=T)

sim.dat$abbr_month <- as.character(format(sim.dat$date2, "%m-%y"))


#selecting only needed variables 
sim_wider <- sim.dat%>% dplyr::select(repDS = LGA, cases, abbr_month)%>%arrange(abbr_month)

rep_DS_sim <- sim_wider%>%group_by(abbr_month, repDS)%>%
  summarise(sum_cases = sum(cases), n =n()) %>% mutate(month = substr(abbr_month, 1, 2))

rep_DS_sim <- rep_DS_sim %>% group_by(repDS, month) %>% summarise(sum_cases = sum(sum_cases))

case_list_sim <- split(rep_DS_sim, rep_DS_sim$repDS)          

fin_sim.df <- plyr::ldply(case_list_sim, max.fun)%>%dplyr::select(repDS, month,
                                                                  simulation = norm_cases)


# creating subset of files 
# sim.dat.05 <- sim.dat%>%filter(year == 2005 )%>%dplyr::select(LGA, month, simulation)
# 
# sim.dat.05$month <- as.numeric(sim.dat.05$month)
# 
# # sim.dat.06 <- sim.dat%>%filter(year == 2006)
# # 
# # sim.dat.07 <- sim.dat%>%filter(year == 2007) 
# 
# sim.dat.08 <- sim.dat%>%filter(year == 2008) 
# 
 # sim.dat.09 <- sim.dat%>%filter(year == 2009)%>%dplyr::select(LGA, month, simulation)
 # sim.dat.09$month <- as.numeric(sim.dat.09$month)
 # 





#real data 
read <- read_dta("bin/monthly_lga_14-18.dta")
names(read)
head(read)

#correcting the names in the data 
read <- read %>% mutate(lga = dplyr::recode(lga,"Gwadabawa" = "Gawabawa", "Yewa North" = "Egbado North",
                                            "Yewa South" = "Egbado South")) 

# read_2 <- read %>% dplyr::select(LGA = lga,state)%>%filter(LGA == "Zangon Kataf")
# head(read_2)

#computing the case counts by aggregating u5 and over5 
read$cases <- rowSums(read[, c("conf_u5", "conf_a5")], na.rm=T)
summary(read$cases)

#creating time related variables 
read <- read %>%mutate(year =as.character(year), month = as.character(read$month), 
                       time = as.Date(paste(year,month,1, sep = "-")), 
                       abbr_month = as.character(format(time, "%m-%y")))

#selecting only needed variables 
read_wider <- read%>% dplyr::select(LGA = lga, cases, abbr_month)%>%arrange(abbr_month)

#reading in the representative DS 
rep_DS_v2 <- read_csv("bin/repDS_v2.csv")

rep_DS_all <- rep_DS_v2 %>% left_join(read_wider) %>% filter(cases < 408949)%>%
  group_by(abbr_month, repDS)%>%
  summarise(sum_cases = sum(cases), n =n()) %>% mutate(month = substr(abbr_month, 1, 2))


rep_DS_all_v2 <- rep_DS_all %>% group_by(repDS, month) %>% summarise(sum_cases = sum(sum_cases))

write_csv(rep_DS_all_v2, "results/archetype_incidence.csv")

case_list <- split(rep_DS_all_v2, rep_DS_all_v2$repDS)          

fin.df <- plyr::ldply(case_list, max.fun)%>%dplyr::select(repDS, month,
                                                          DHIS2 =norm_cases)

#combine both data and sim points 

all.df <- fin.df %>% left_join(fin_sim.df)

# plotting 


# dat <- left_join(sim.dat.09,read.dat.14)


df2 <- pivot_longer(all.df, cols = c(DHIS2, simulation), names_to = "variable",values_to = "count")


g <- ggplot(data = df2, aes(x = month, y = count, color = variable, group=variable))+ 
geom_line()+ 
  geom_point()+
  facet_wrap(~ repDS, nrow = 5)+
  ggtitle("Simulation vs Data") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("results/sim_data.pdf", g, width = 13, height = 13, useDingbats=FALSE)



# plotting just real data 



g <- ggplot(data = fin.df , mapping = aes(x = month, y = variable, group =1)) + 
  geom_point() + 
  geom_line()+
  facet_wrap(~ repDS, nrow = 5)+
  ggtitle("Data") + 
  theme(plot.title = element_text(hjust = 0.5))


ggsave("results/real_data.pdf", g)



#computing the population by archetype 

pop <- read.csv("bin/nigeria_LGA_pop.csv")


#reading in the representative DS 
rep_DS <- read.csv("bin/representative_DS_orig60clusters.csv")



rep_DS_pop <- rep_DS %>%left_join(pop)%>%
              group_by(repDS) %>% dplyr::summarise(population = sum(geopode.pop))

pop_all <- left_join(rep_DS_all_v2 , rep_DS_pop) %>% mutate(incidence = (sum_cases/population)*1000)

write.csv(pop_all, "results/archetype_incidence_fin.csv")


# warnings()
# warpy <- as_tibble(read_wider)
# 
# warpy_id <- warpy %>%
#   group_by(LGA, abb_month) %>%
#   mutate(id = row_number()) %>%
#   ungroup()


# warpy_id_2 <- warpy_id %>%
#   pivot_wider(names_from = c(abb_month), values_from = cases)
# 
# tail(warpy_id_2)
# 
# rep_DS_all <- read_csv("bin/representative_DS_orig60clusters.csv")
# 
# rep_DS_all <- rep_DS_all %>% left_join(LGAshp_sf) %>% dplyr::select(LGA, repDS, State)
# 
# cases_warpy_id <- warpy_id_2 %>%stringdist_inner_join(rep_DS_all, max_dist = 0.03, ignore_case=T, method = "jw")
# 
# 
# 
# check <-cases_warpy_id%>% dplyr::select(LGA.x,LGA.y, repDS, State)
# 
# check <- cases_warpy_id %>% filter(is.na(repDS)) %>% dplyr::select(LGA.x, repDS)
# 
# write.csv(check, "state_repDS.csv")
# 
# read$norm_cases <- (read$cases - mean(read$cases)) / sd(read$cases)
# 
# 
# 
# 
# read_dat <- read %>% filter(lga == "Alkaleri" | lga == "Aniocha South" | lga == "Bakura" | lga == "Dandume" | lga == "Degema" | lga == "Emuoha"
#                             | lga == "Geidam"| lga == "Gujba" | lga == "Ikom"| lga == "Katagum"| lga == "Kogi"| lga == "Konshisha"
#                             | lga == "Maiduguri" | lga == "Ngor-Okpala"| lga == "Owo"| lga == "Rimi"| lga == "Sabon Gari"| lga == "Tafa"
#                             | lga == "Taura"| lga == "Uruan")
# 
# 
# 
# read.dat.14 <- read_dat%>%filter(year == 2014) %>% dplyr::rename(LGA = lga, data = norm_cases) %>% 
#                             dplyr::select(LGA, month, data)


















# read.dat.15 <- read_dat%>%filter(year == 2015)
# read.dat.16 <- read_dat%>%filter(year == 2016)
# read.dat.17 <- read_dat%>%filter(year == 2017)


# plot(read.dat.14$month, read.dat.14$norm_cases, xlab="month", ylab="cases", type = "p", col = "red", main= "Nigeria 2014")
# 
# plot(read.dat.15$month, read.dat.15$cases, xlab="month", ylab="cases", type = "p", col = "red", main= "Nigeria 2015")
# 
# plot(read.dat.16$month, read.dat.16$cases, xlab="month", ylab="cases", type = "p", col = "red", main= "Nigeria 2016")
# 
# plot(read.dat.17$month, read.dat.17$cases, xlab="month", ylab="cases", type = "p", col = "red", main= "Nigeria 2017")
# 
# 
# 
# plot(sim.dat.05$month, sim.dat.05$norm_cases, xlab="month", ylab="cases", type = "p", col = "red", main= "Nigeria 2005")
# 
# plot(sim.dat.06$month, sim.dat.06$cases, xlab="month", ylab="cases", type = "p", col = "green", main="Nigeria 2006")
# 
# plot(sim.dat.07$month, sim.dat.07$cases, xlab="month", ylab="cases", type = "p", col = "blue", main="Nigeria 2007")
# 
# plot(sim.dat.08$month, sim.dat.08$cases, xlab="month", ylab="cases", type = "p", col = "orange", main= "Nigeria 2008")
# 
# plot(sim.dat.09$month, sim.dat.09$norm_cases, xlab="month", ylab="cases", type = "p", col = "black", main = "Nigeria 2009")
# 


