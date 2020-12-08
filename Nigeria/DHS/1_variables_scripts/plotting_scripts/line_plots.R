
wd <- file.path(DataDir,"urbanization")

setwd(wd)

read.csv("urban-and-rural-population.csv") %>%  filter(Code == "NGA")
