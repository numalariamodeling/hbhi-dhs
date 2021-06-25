
#important to download the github version of tidyverse.Uncomment the script below to run
#install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("r-pkgs/usethis")

#devtools::install_github("hadley/tidyverse")


# Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/pc/Documents/NU - Malaria Modeling/GIS DataBase")

# reads in functions so we can alias it using funenv$
funEnv <- new.env()
sys.source(file = file.path("C:/Users/pc/Documents/NU - Malaria Modeling/Non Linear", "Nigeria functions.R"), 
           envir = funEnv, toplevel.env = funEnv)


options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed

#reading files
temp_vap_18 <- read.csv("mis_10_temp_vapour.csv")
dhs <-  read.csv("dhsclusters_20201214.csv")


#Calculating humidity indices

temp_vap_18_humid <- temp_vap_18 %>% 
  mutate(humidindex = (tmp_mean + 0.5555*
           (6.11*exp(5417.753*((1/273.16) -(1/(vap_mean+273.15))))-10) ))

#merging humidity indices and pre clustered data. 
# bind the datasets 
colnames(temp_vap_18_humid)[colnames(temp_vap_18_humid) == 'DHSCLUST'] <- 'hv001'
humid_df <- temp_vap_18_humid[,c("hv001","humidindex")]

joiner <- left_join(Joiner8, humid_df, by = "hv001") 
final2018 <- as.data.frame(joiner)

write.csv(final2018, "mis2010clusters_final_20210109.csv")



mis15 <- read.csv("mis2015clusters_final_20201214.csv")

mis10 <- read.csv("mis2010clusters_final_20201214.csv")

#binding datasets by same columns


bind_df <- data.frame(mapply(c,final2018, mis15, mis10))

#writing final dataset to file
write.csv(bind_df, "101518allcluster_housing_humid.csv")
