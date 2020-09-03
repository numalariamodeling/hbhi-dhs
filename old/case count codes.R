rm(list=ls())

library(lubridate)


nameFile <- "C:/Users/ido0493/Box/NU-malaria-team/data/burkina_cases/casecounts.csv"

# read the data
dfIn <- read.csv(nameFile, header = T, sep = ",")
length(unique(dfIn$District.Admin1))
length(unique(dfIn$Année))
length(unique(dfIn$Mois))

dfIn$date <- paste(dfIn$Mois, dfIn$Année, sep = " ")

class(dfIn$date)
  
dfIn$date_eng <- readr::parse_date(dfIn$date,"%B %Y",locale=locale("fr"))

dfIn$date_eng <- readr::parse_date(readr::parse_character(dfIn$date, locale = readr::locale(encoding = "latin1")), "%B %Y", locale = readr::locale("fr"))

dfIn$MM <- as.numeric(format(dfIn$date_eng, format = "%m"))



####use to subset when subset or other methods don't work
dfIn_Sp <- dfIn[grep(Boulmiougou|Baskuy|Dori|Kongoussi|Koudougou|Do|Dano|Diebougou,dfIn$District.Admin2),]####does not work

####subsetting is a pain!

dfIn_new <- dfIn %>%
  rename(cases_c_p = Cas.de.paludisme.ambulatoires..confirmés...présumés., num_ACT = Nombre.de.cas.traité.ACT)%>%
     dplyr:: select(District.Admin2, MM, cases_c_p)%>%
      group_by(District.Admin2, MM)%>%
     summarise(mean = mean(cases_c_p))
     

write.csv(dfIn_new,file="outputs/HF_month_DHIS_avgcasecounts.csv")


  