
list.of.packages <- c("caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
                      "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
                      "scales", "sjPlot", "sjlabelled", "sjmisc", "mapview", "geosphere", "GGally")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


x <- c("tidyverse", "ggplot2", "purrr", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", 
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "geosphere","GGally")

lapply(x, library, character.only = TRUE) #applying the library function to packages

theme_fun <- function() {
    theme_bw()+
    theme(panel.border = element_blank())+
    theme(legend.position = "none")
}

histofun <- function(df){
  df %>%
    ggplot( aes(x=value, color=text, fill=text)) +
    geom_histogram()+
    facet_wrap(~text, scales ="free", labeller = labeller(text = new_labels))+ 
    theme_fun()
}

