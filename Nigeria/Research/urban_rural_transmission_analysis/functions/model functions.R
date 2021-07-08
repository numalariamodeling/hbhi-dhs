# # Reading in the necessary packages 
list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
                      "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
                      "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "sjlabelled", "raster", "rlist", 'rgeos', 'INLA', 'ggpubr',
                      'cowplot', 'gridExtra', 'lme4')
lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages
