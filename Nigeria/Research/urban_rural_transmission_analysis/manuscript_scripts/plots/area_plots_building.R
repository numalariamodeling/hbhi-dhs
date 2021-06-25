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
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "nnt", "reshape" )
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

building_density_0m <- file.reader("building_density_0m_buffer_DHS_10_15_18.csv")
building_density_1000m <- file.reader("building_density_1000m_buffer_DHS_10_15_18.csv")
building_density_2000m <- file.reader("building_density_2000m_buffer_DHS_10_15_18.csv")
building_density_3000m <- file.reader("building_density_3000m_buffer_DHS_10_15_18.csv")
building_density_dhs <- read.csv(file.path(DHSDir, "Nigeria_2010_2018_clustered_final_dataset.csv"),
                            header = T, sep = ',') 

#######
#figuring out the number of clusters per year
table(building_density_0m$.id)


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
building_density_0m_2010 <- extr2010.fun(building_density_0m) 
colnames(building_density_0m_2010)[3]<- "build_den_0m"
building_density_0m_2015 <- extr2015.fun(building_density_0m)
colnames(building_density_0m_2015)[3]<- "build_den_0m"
building_density_0m_2020 <- extr2020.fun(building_density_0m)
colnames(building_density_0m_2020)[3]<- "build_den_0m"
building_density_0m_2010_2015_2020 <- 
  dplyr::bind_rows(building_density_0m_2010, building_density_0m_2015, 
                                                  building_density_0m_2020)


# Proccessing the 1000km data
building_density_1000m_2010 <- extr2010.fun(building_density_1000m)
colnames(building_density_1000m_2010)[3]<- "build_den_1000m"
building_density_1000m_2015 <- extr2015.fun(building_density_1000m)
colnames(building_density_1000m_2015)[3]<- "build_den_1000m"
building_density_1000m_2020 <- extr2020.fun(building_density_1000m)
colnames(building_density_1000m_2020)[3]<- "build_den_1000m"
building_density_1000m_2010_2015_2020 <- 
  dplyr::bind_rows(building_density_1000m_2010, building_density_1000m_2015, 
                                                  building_density_1000m_2020)

# Proccessing the 2000km data
building_density_2000m_2010 <- extr2010.fun(building_density_2000m)
colnames(building_density_2000m_2010)[3]<- "build_den_2000m"
building_density_2000m_2015 <- extr2015.fun(building_density_2000m)
colnames(building_density_2000m_2015)[3]<- "build_den_2000m"
building_density_2000m_2020 <- extr2020.fun(building_density_2000m)
colnames(building_density_2000m_2020)[3]<- "build_den_2000m"
building_density_2000m_2010_2015_2020 <- 
  dplyr::bind_rows(building_density_2000m_2010, building_density_2000m_2015, 
                                                     building_density_2000m_2020)

# Proccessing the 3000km data
building_density_3000m_2010 <- extr2010.fun(building_density_3000m)
colnames(building_density_3000m_2010)[3]<- "build_den_3000m"
building_density_3000m_2015 <- extr2015.fun(building_density_3000m)
colnames(building_density_3000m_2015)[3]<- "build_den_3000m"
building_density_3000m_2020 <- extr2020.fun(building_density_3000m)
colnames(building_density_3000m_2020)[3]<- "build_den_3000m"


# Proccessing the dhs data
building_density_dhs_2010 <- building_density_dhs%>% filter(data_source == "mis2010")
building_density_dhs_2015 <- building_density_dhs%>% filter(data_source == "mis2015")
building_density_dhs_2018 <- building_density_dhs%>% filter(data_source == "dhs2018")

#adding dhs data with fb 3000m

build_density_3000m_2020_dhs2010 <- left_join(building_density_3000m_2010, 
                                                 building_density_dhs_2010, by = c("ID" ="hv001"))
build_density_3000m_2015_dhs2015 <- left_join(building_density_3000m_2015, 
                                                 building_density_dhs_2015, by = c("ID" ="hv001"))
build_density_3000m_2020_dhs2018 <- left_join(building_density_3000m_2020, 
                                                 building_density_dhs_2018, by = c("ID" ="hv001"))


building_density_3000m_2010_2015_2020_dhs <- 
  dplyr::bind_rows(build_density_3000m_2020_dhs2010, build_density_3000m_2015_dhs2015, 
                   build_density_3000m_2020_dhs2018)


####################
#####master pop density dataframe ###################
colnames(building_density_3000m_2010_2015_2020_dhs)[1]<- "X"

building_density_df <- left_join(building_density_0m_2010_2015_2020, 
                            building_density_1000m_2010_2015_2020, by = "X") %>% 
  left_join(., building_density_2000m_2010_2015_2020, by = "X") %>% 
  left_join(., building_density_3000m_2010_2015_2020_dhs, by = "X") 


building_density_df <- building_density_df[,c("ID.x", "data_source", "Rural_urban","build_den_0m", "build_den_1000m", 
                                    "build_den_2000m", "build_den_3000m", "build_count")]

building_density_df <- building_density_df %>% mutate(build_den = na_if(build_den, -9999))

building_density_df <- building_density_df %>% mutate(diff_fb_0_1000m = (build_den_fb_1000m - build_den_fb_0m))
building_density_df <- building_density_df %>% mutate(diff_fb_0_3000m = (build_den_fb_3000m - build_den_fb_0m))


building_density_df_urban <- building_density_df %>% filter(Rural_urban == 1)

############################################################################################
#################################### Counting NAs ####################################

na_df <- as.data.frame(colSums(is.na(building_density_df_urban)))
write.csv(na_df, "na_count.csv")


###################################################################################
####urban histgram plots
###################################################################################


melted_data <- melt(building_density_df_urban[,c("ID.x", "build_den_0m", "build_den_1000m", 
                            "build_den_2000m", "build_den_3000m")], id.vars = "ID.x")

melted_data_plot <-  ggplot(melted_data, aes(x= value, fill = variable, color = variable)) +
  geom_freqpoly(size = 2) + ggtitle("Population densities for all buffers") + theme_classic()
melted_data_plot


melted_data_fb <- melt(building_density_df_urban[,c("ID.x",  "build_den_fb_0m",
                                         "build_den_fb_1000m", "build_den_fb_2000m", "build_den_fb_3000m")], id.vars = "ID.x")

melted_data_fb_plot <- ggplot(melted_data_fb, aes(x= value, fill = variable, color = variable)) +
  geom_freqpoly(size = 2)  + ggtitle("Facebook population densities for all buffers") + theme_classic()
melted_data_fb_plot

diff_fb_melt_0_3000m <- melt(building_density_df_urban[,c("ID.x",  "diff_fb_0_1000m", "diff_fb_0_3000m")], id.vars = "ID.x")
diff_fb_melt_plot_0_3000 <- ggplot(diff_fb_melt_0_3000m, aes(x= value, fill = variable, color = variable)) +
  geom_freqpoly(size = 2) + ggtitle("Facebook pop density differnces from 0m buffer") + theme_classic()
diff_fb_melt_plot_0_3000

plot_list_combined <- list(melted_data_plot, melted_data_fb_plot, diff_fb_melt_plot_0_3000)

variables_comb <- ggarrange(plotlist=plot_list_combined, nrow =1, ncol=3)
ggsave(paste0(ResultDir, '/', Sys.Date(),  'hist_urban_combined.pdf'), variables_comb, width=13, height=7.5)
variables_comb



#Seperated plots

density_fun<- function(variable, label){ggplot(building_density_df_urban, aes(x=variable)) + 
    geom_freqpoly(color="#FF847CFF", size = 1.5)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}


build_den_0m_plot <- density_fun(building_density_df_urban$build_den_0m, "Pop density 0m buffer")
build_den_0m_plot
build_den_1000m_plot <- density_fun(building_density_df_urban$build_den_1000m, "Pop density 1000m buffer")
build_den_1000m_plot
build_den_2000m_plot <- density_fun(building_density_df_urban$build_den_1000m, "Pop density 2000m buffer")
build_den_2000m_plot
build_den_3000m_plot <- density_fun(building_density_df_urban$build_den_1000m, "Pop density 3000m buffer")
build_den_3000m_plot
build_den_fb_0m_plot <- density_fun(building_density_df_urban$build_den_fb_0m, "FB Pop density 0m buffer")
build_den_fb_0m_plot
build_den_fb_1000m_plot <- density_fun(building_density_df_urban$build_den_fb_1000m, "FB Pop density 1000m buffer")
build_den_fb_1000m_plot
build_den_fb_2000m_plot <- density_fun(building_density_df_urban$build_den_fb_2000m, "FB Pop density 2000m buffer")
build_den_fb_2000m_plot
build_den_fb_3000m_plot <- density_fun(building_density_df_urban$build_den_fb_3000m, "FB Pop density 3000m buffer")
build_den_fb_3000m_plot
build_den_plot <- density_fun(building_density_df_urban$build_den, "DHS Pop density 0m buffer")
build_den_plot


plot_list <- list(build_den_0m_plot, build_den_1000m_plot, build_den_2000m_plot, build_den_3000m_plot, 
                  build_den_fb_0m_plot, build_den_fb_1000m_plot, build_den_fb_2000m_plot, build_den_fb_3000m_plot,
                  build_den_plot) 

variables <- ggarrange(plotlist=plot_list, nrow =3, ncol=4)
ggsave(paste0(ResultDir, '/', Sys.Date(),  'hist_urban.pdf'), variables, width=13, height=7.5)
variables

