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

elevation_0m <- file.reader("elevation_0m_buffer_DHS_10_15_18.csv")
elevation_1000m <- file.reader("elevation_1000m_buffer_DHS_10_15_18.csv")
elevation_2000m <- file.reader("elevation_2000m_buffer_DHS_10_15_18.csv")
elevation_3000m <- file.reader("elevation_3000m_buffer_DHS_10_15_18.csv")
elevation_dhs <- read.csv(file.path(DHSDir, "Nigeria_2010_2018_clustered_final_dataset.csv"),
                            header = T, sep = ',') 

#######
#figuring out the number of clusters per year
table(elevation_0m$.id)


#functions 

#fxn for the range for the daata points for 2010 clusters

extr2010.fun <- function(filename){
  subset(filename, X %in% c(1:239))
}


#we add 326 to 239 range because 2015 has 326 clustrs
extr2015.fun <- function(filename){
  subset(filename, X %in% c(240:(239+326)))
}

#we add 1 the 2015 range for us to find 
#a starting point for 2020 cluster and we add 1389 becuase 2018 (2020) has 1389 cluster for us to get the range/. 

extr2020.fun <- function(filename){
  subset(filename, X %in% c((239+326+1):(239+326+1+1389))) 
}


# Proccessing the 0km data
elevation_0m_2010 <- extr2010.fun(elevation_0m) 
colnames(elevation_0m_2010)[3]<- "elev_0m"
elevation_0m_2015 <- extr2015.fun(elevation_0m)
colnames(elevation_0m_2015)[3]<- "elev_0m"
elevation_0m_2020 <- extr2020.fun(elevation_0m)
colnames(elevation_0m_2020)[3]<- "elev_0m"
elevation_0m_2010_2015_2020 <- 
  dplyr::bind_rows(elevation_0m_2010, elevation_0m_2015, 
                                                  elevation_0m_2020)


# Proccessing the 1000km data
elevation_1000m_2010 <- extr2010.fun(elevation_1000m)
colnames(elevation_1000m_2010)[3]<- "elev_1000m"
elevation_1000m_2015 <- extr2015.fun(elevation_1000m)
colnames(elevation_1000m_2015)[3]<- "elev_1000m"
elevation_1000m_2020 <- extr2020.fun(elevation_1000m)
colnames(elevation_1000m_2020)[3]<- "elev_1000m"
elevation_1000m_2010_2015_2020 <- 
  dplyr::bind_rows(elevation_1000m_2010, elevation_1000m_2015, 
                                                  elevation_1000m_2020)

# Proccessing the 2000km data
elevation_2000m_2010 <- extr2010.fun(elevation_2000m)
colnames(elevation_2000m_2010)[3]<- "elev_2000m"
elevation_2000m_2015 <- extr2015.fun(elevation_2000m)
colnames(elevation_2000m_2015)[3]<- "elev_2000m"
elevation_2000m_2020 <- extr2020.fun(elevation_2000m)
colnames(elevation_2000m_2020)[3]<- "elev_2000m"
elevation_2000m_2010_2015_2020 <- 
  dplyr::bind_rows(elevation_2000m_2010, elevation_2000m_2015, 
                                                     elevation_2000m_2020)

# Proccessing the 3000km data
elevation_3000m_2010 <- extr2010.fun(elevation_3000m)
colnames(elevation_3000m_2010)[3]<- "elev_3000m"
elevation_3000m_2015 <- extr2015.fun(elevation_3000m)
colnames(elevation_3000m_2015)[3]<- "elev_3000m"
elevation_3000m_2020 <- extr2020.fun(elevation_3000m)
colnames(elevation_3000m_2020)[3]<- "elev_3000m"


# Proccessing the dhs data
elevation_dhs_2010 <- elevation_dhs%>% filter(data_source == "mis2010")
elevation_dhs_2015 <- elevation_dhs%>% filter(data_source == "mis2015")
elevation_dhs_2018 <- elevation_dhs%>% filter(data_source == "dhs2018")

#adding dhs data with fb 3000m

elev_3000m_2020_dhs2010 <- left_join(elevation_3000m_2010, 
                                                 elevation_dhs_2010, by = c("ID" ="hv001"))
elev_3000m_2015_dhs2015 <- left_join(elevation_3000m_2015, 
                                                 elevation_dhs_2015, by = c("ID" ="hv001"))
elev_3000m_2020_dhs2018 <- left_join(elevation_3000m_2020, 
                                                 elevation_dhs_2018, by = c("ID" ="hv001"))


elevation_3000m_2010_2015_2020_dhs <- 
  dplyr::bind_rows(elev_3000m_2020_dhs2010, elev_3000m_2015_dhs2015, 
                   elev_3000m_2020_dhs2018)


####################
#####master pop density dataframe ###################
colnames(elevation_3000m_2010_2015_2020_dhs)[1]<- "X"

elevation_df <- left_join(elevation_0m_2010_2015_2020, 
                            elevation_1000m_2010_2015_2020, by = "X") %>% 
  left_join(., elevation_2000m_2010_2015_2020, by = "X") %>% 
  left_join(., elevation_3000m_2010_2015_2020_dhs, by = "X") 


elevation_df <- elevation_df[,c("ID.x", "data_source", "Rural_urban","elev_0m", "elev_1000m", 
                                    "elev_2000m", "elev_3000m", "build_count")]

elevation_df <- elevation_df %>% mutate(elev = na_if(elev, -9999))

elevation_df <- elevation_df %>% mutate(diff_0_1000m = (elev_1000m - elev_0m))
elevation_df <- elevation_df %>% mutate(diff_0_2000m = (elev_3000m - elev_0m))
elevation_df <- elevation_df %>% mutate(diff_0_3000m = (elev_3000m - elev_0m))
elevation_df <- elevation_df %>% mutate(diff_2000_3000m = (elev_3000m - elev_2000m))


elevation_df_urban <- elevation_df %>% filter(Rural_urban == 1)

############################################################################################
#################################### Counting NAs ####################################

na_df <- as.data.frame(colSums(is.na(elevation_df_urban)))
write.csv(na_df, "Elevation_na_count.csv")


###################################################################################
####urban histgram plots
###################################################################################


melted_data <- melt(elevation_df_urban[,c("ID.x", "elev_0m", "elev_1000m", 
                            "elev_2000m", "elev_3000m")], id.vars = "ID.x")

melted_data_plot <-  ggplot(melted_data, aes(x= value, fill = variable, color = variable)) +
  geom_freqpoly(size = 2) + ggtitle("Elevations for all buffers") + theme_classic()
melted_data_plot


diff_melt_0_3000m <- melt(elevation_df_urban[,c("ID.x",  "diff_0_1000m", "diff_0_3000m", "diff_2000_3000m")], id.vars = "ID.x")
diff_melt_plot_0_3000 <- ggplot(diff_melt_0_3000m, aes(x= value, fill = variable, color = variable)) +
  geom_freqpoly(size = 2) + ggtitle("Elevation differnces among differnt buffers sizes") + theme_classic()
diff_melt_plot_0_3000

plot_list_combined <- list(melted_data_plot, diff_melt_plot_0_3000)

variables_comb <- ggarrange(plotlist=plot_list_combined, nrow =1, ncol=2)
ggsave(paste0(ResultDir, '/', Sys.Date(),  'Elevationing_urban_combined.pdf'), variables_comb, width=13, height=7.5)
variables_comb



#Seperated plots

density_fun<- function(variable, label){ggplot(elevation_df_urban, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}


elev_0m_plot <- density_fun(elevation_df_urban$elev_0m, "Elevation 0m buffer")
elev_0m_plot
elev_1000m_plot <- density_fun(elevation_df_urban$elev_1000m, "Elevation 1000m buffer")
elev_1000m_plot
elev_2000m_plot <- density_fun(elevation_df_urban$elev_1000m, "Elevation 2000m buffer")
elev_2000m_plot
elev_3000m_plot <- density_fun(elevation_df_urban$elev_1000m, "Elevation 3000m buffer")
elev_3000m_plot



plot_list <- list(elev_0m_plot, elev_1000m_plot, elev_2000m_plot, elev_3000m_plot) 

variables <- ggarrange(plotlist=plot_list, nrow =1, ncol=4)
ggsave(paste0(ResultDir, '/', Sys.Date(),  'elev_hist_urban.pdf'), variables, width=13, height=7.5)
variables

