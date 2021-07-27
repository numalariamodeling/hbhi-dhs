Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DHSDir <- file.path(NGDir, "data","DHS", "Computed_cluster_information", "cluster_data_all_years")
DataDir <- file.path(NGDir, "data","DHS", "Computed_cluster_information", "urban_malaria_covariates")
ResearchDir <- file.path(NGDir, 'src', 'Research', 'urban_rural_transmission_analysis')
ResultDir <-file.path(ResearchDir, "manuscript_scripts", "plots")
Rdata <- file.path(ResultDir)


# # Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "readtext", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt", "reshape", "ggpubr" )
# 
lapply(x, library, character.only = TRUE) #applying the library function to packages


###################################################################################
####Loading data
###################################################################################
# Load pre-extracted cluster data:

file.reader <- function(filename){
  read.csv(file.path(DataDir, filename),
           header = T, sep = ',')
}

housing_0m <- file.reader("housing_0m_DHS_10_15_18.csv")
housing_1000m <- file.reader("housing_1000m_DHS_10_15_18.csv")
housing_2000m <- file.reader("housing_2000m_DHS_10_15_18.csv")
housing_3000m <- file.reader("housing_3000m_DHS_10_15_18.csv")

sum(is.na(housing_0m$Nature_Africa_Housing_2015_NGA))
sum(is.na(housing_1000m$Nature_Africa_Housing_2015_NGA))
sum(is.na(housing_2000m$Nature_Africa_Housing_2015_NGA))
sum(is.na(housing_3000m$Nature_Africa_Housing_2015_NGA))

#Seperated plots

density_fun<- function(variable, label){ggplot(housing_0m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}


elev_0m_plot <- density_fun(housing_0m$Nature_Africa_Housing_2015_NGA, "housing 0m buffer")
elev_0m_plot


density_fun<- function(variable, label){ggplot(housing_1000m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}

elev_1000m_plot <- density_fun(housing_1000m$Nature_Africa_Housing_2015_NGA, "housing 1000m buffer")
elev_1000m_plot

density_fun<- function(variable, label){ggplot(housing_2000m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}
elev_2000m_plot <- density_fun(housing_2000m$Nature_Africa_Housing_2015_NGA, "housing 2000m buffer")
elev_2000m_plot

density_fun<- function(variable, label){ggplot(housing_3000m, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}
elev_3000m_plot <- density_fun(housing_3000m$Nature_Africa_Housing_2015_NGA, "housing 3000m buffer")
elev_3000m_plot



plot_list <- list(elev_0m_plot, elev_1000m_plot, elev_2000m_plot, elev_3000m_plot) 

variables <- ggarrange(plotlist=plot_list, nrow =1, ncol=4)
ggsave(paste0(ResultDir, '/', Sys.Date(),  'housing_hist_urban.pdf'), variables, width=13, height=7.5)
variables

