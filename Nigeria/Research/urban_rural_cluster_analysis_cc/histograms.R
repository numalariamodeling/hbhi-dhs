rm(list = ls())

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", " randomGLM", "ROCR", "AER", "caretEnsemble", "klaR", "naniar")

lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)
## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("mambrose" %in% user) {
  user_path <- file.path("C:/Users", user)
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "NU-malaria-team")
  NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  SrcDir <- file.path(NGDir, "src", "DHS")
  BinDir <- file.path(NGDir, "bin")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Box", "NU-malaria-team")
  NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  PlotDir <-file.path(ResultDir, "research_plots")
  BinDir <- file.path(NGDir, "bin")
  SrcDir <- file.path(NGDir, "src", "Research", "urban_rural_cluster_analysis_cc")
  ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
}

#Read in functions 

source(file.path(SrcDir, "functions", "log_reg_functions.R"))
# Read in data file 
histogram <- T 
type <- "urban/rural"

if(histogram == T) {
  merged_df <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), header= TRUE)
  
  merged_df <-  merged_df %>% mutate(log_sex_f = log(merged_df$sex_f))
  
  
  df <- merged_df[ ,colnames(merged_df) 
                   %in% c("hv001", "l_pop_den", "sex_f", "wealth_2", "edu_a", "hh_size", "ACT_use_u5", 
                          "hh_members_age", "humidindex", "annual_precipitation", "net_use", "build_count", "Rural_urban")]
  
  
  if(type!="combined"){
    df_split <- split(df, df$Rural_urban)
    df_split_long <- lapply(df_split, function(x) gather(x, key = "text", value = "value"))
    new_labels <- c("Log Population density", "% of famale", "% of wealth","% of education", "Household size", 
                    "% of U5 ACT use", "Household average age", "Humidity index", "Annual precipitation",
                    "% of net use", "Building count", "Residence") 
    
    names(new_labels)<-c("l_pop_den", "sex_f", "wealth_2", "edu_a", "hh_size", "ACT_use_u5", 
                         "hh_members_age", "humidindex", "annual_precipitation", "net_use", "build_count", "Rural_urban")
    
    histo_list <- lapply(df_split_long, histofun)
    ggsave("lurban_histograms_2.pdf", plot =histo_list[[1]], path=file.path(PlotDir, "histograms"))
    ggsave("rural_histograms_2.pdf", plot =histo_list[[2]], path=file.path(PlotDir, "histograms"))
  }else if(type == "combined"){
    df_combo <- df %>%  gather(key = "text", value = "value")
    new_labels <-c("Log Population density", "% of famale", "% of wealth","% of education", "Household size", 
                   "% of U5 ACT use", "Household average age", "Humidity index", "Annual precipitation",
                   "% of net use", "Building count", "Residence")
    
    names(new_labels)<-c("l_pop_den", "sex_f", "wealth_2", "edu_a", "hh_size", "ACT_use_u5", 
                         "hh_members_age", "humidindex", "annual_precipitation", "net_use", "build_count", "Rural_urban")
    histo<- histofun(df_combo)
    ggsave("combined_histograms_2.pdf", plot =histo, path=file.path(PlotDir, "histograms"))
  }else{
    print("dataset created but no plots")
  }
  
  
}else{
  print("histograms will not be made")
}


library(ggplot2)
# Basic density
melted_data <- melt(df[,c("Freq")], id.vars = "Var1")
ggplot(melted_data, aes(x= value, fill = variable, color = variable)) +
  geom_density(alpha = 0.1) +
  theme(panel.background = element_rect(fill = "white", colour = "#BFD5E3", linetype = "solid"), 
panel.grid.major = element_line(size = 0.05, linetype = 'solid', colour = "azure2")) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

melted_data <- melt(df[,c("hv001", "net_use")], id.vars = "hv001")
ggplot(melted_data, aes(x= value, fill = variable, color = variable)) +
  geom_density(alpha = 0.1) +
  theme(panel.background = element_rect(fill = "white", colour = "#BFD5E3", linetype = "solid"), 
        panel.grid.major = element_line(size = 0.05, linetype = 'solid', colour = "azure2")) +
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))

densityPlot(~ edu_a, show.bw=TRUE, method="kernel", data=df)
