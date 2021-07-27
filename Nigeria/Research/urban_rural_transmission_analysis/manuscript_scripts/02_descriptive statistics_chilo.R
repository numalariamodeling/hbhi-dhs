
x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "sp", "sf", "tmap", 
       'paletteer', 'cowplot', 'gridExtra', 'lme4', 'reshape2')




lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)


#_________________________________Directories

user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
DataDir <- file.path(ProjectDir, "data")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'DHS_survey_extract')
Rdata <- file.path(DataDir, 'DHS', 'Subset_data', "urban_malaria_rdata")
ResultDir <-file.path(ProjectDir, "results", "research_plots")
HisDir <-file.path(ResultDir, "histograms")
BinDir <- file.path(ProjectDir, "bin")
SrcDir <- file.path(ProjectDir, 'src', 'Research', 'urban_rural_transmission_analysis')
RastDir <- file.path(DataDir, "Raster_files")
CsvDir <- file.path(DHSData, "Computed_cluster_information", 'urban_malaria_covariates', 'cleaned_cluster_covariates_all')


#_______________________________ Load pre-clustered data:
clu_df_10_18 <- read.csv(file.path(CsvDir, "all_cluster_variables_urban_malaria.csv"), 
                               header = T, sep = ',') 

# Binarize response:
clu_df_10_18$y <- ifelse(clu_df_10_18$p_test < 0.1, 0,1)

#na count
missing_values <- sapply(clu_df_10_18, function(x) sum(is.na(x)))


#__________________________________maps

# urban cluster points

dhs18_sf <- st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
mis15_sf <- st_read(file.path(DHSData, "Downloads", "NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),)
mis10_sf <- st_read(file.path(DHSData, "Downloads", "NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),)
sf_10_18 <- rbind(dhs18_sf, mis15_sf, mis10_sf)


# join dhs variables to cluster points by year 
df_10_18_fin <- left_join(sf_10_18, clu_df_10_18, by = 
                                c("DHSCLUST" = "v001", "DHSYEAR" = "dhs_year"))%>% filter(URBAN_RURA == "U")



#make an urban map of all cluster values 

u_df_18_fin <- df_10_18_fin %>% filter(DHSYEAR == 2018) 
u_df_15_fin <- df_10_18_fin %>% filter(DHSYEAR == 2015) 
u_df_10_fin <- df_10_18_fin %>% filter(DHSYEAR == 2010)

#read in state shape file 
stateshp <- readOGR(file.path(DataDir, "shapefiles","gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)

#make cluster maps 

clustermap<-function(cluster_shp, title){
  tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(cluster_shp)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size =0.2, col = "p_test", 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 20, 30, 40, 50, 60, 70, 80, 90, 100), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}

map_18<-clustermap(u_df_18_fin, "2018 malaria prevalence by cluster (DHS)")
map_15 <-clustermap(u_df_15_fin, "2015 malaria prevalence by cluster (DHS)")
map_10 <-clustermap(u_df_10_fin, "2010 malaria prevalence by cluster (DHS)")


urban_map<-tmap_arrange(map_18, map_15, map_10)


tmap_save(tm =urban_map, filename = file.path(ResultDir, "maps", "urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


###################################################################################
##_______________________________________urban barplots ___________________________
###################################################################################

paletteer::paletteer_d("awtools::a_palette")

# Binarize response:
clu_df_10_18$y <- ifelse(clu_df_10_18$p_test < 0.1, "less than 10%", "greater than 10%") 


u_bar <- ggplot(clu_df_10_18, aes(x=as.factor(y), fill=as.factor(y))) + 
  geom_bar()+ 
  scale_fill_paletteer_d("awtools::spalette")+
  #geom_text(stat='count', aes(label=..count..), vjust=-1)+
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust =-1)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab("Malaria prevalence")+
  ylab("Number of clusters")

u_bar

ggsave(paste0(HisDir, '/', Sys.Date(),  'combined_urban_malaria_clusters_percent.pdf'), u_bar, width=13, height=13)



#________________________________all coveriates histogram plots_________________________________


clu_df_cont <- clu_df_10_18[ , -which(names(clu_df_10_18) %in% c("shstate", "region", "X"))]
clu_df_cont$y <- ifelse(clu_df_cont$p_test < 0.1, 0,1) %>% (as.numeric)

var_list = c(3:19)

plot_list = list()

for (i in 1:16) { 
  p<- ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_density(fill = "#FF847CFF")+
      theme_minimal()+
      theme(text = element_text(size=16))+
      labs (title = names(clu_df_cont)[var_list[[i]]], x = "values") +
      xlab("")
  plot_list[[i]]<-p
  variables <- ggarrange(plotlist=plot_list, nrow =4, ncol=4)
  ggsave(paste0(HisDir, '/', Sys.Date(),  'density.pdf'), variables, width=13, height=13)
}


#______________________________ Stacked hist__________________________________________
clu_df_cont <- na.omit(clu_df_cont)

for (i in 1:16) { 
  p<- ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_histogram(aes(fill=y), position = "fill", show.legend = FALSE)+
    theme_minimal()+
    theme(text = element_text(size=16))+
    labs (title = names(clu_df_cont)[var_list[[i]]], x = "values") +
    xlab("")
  plot_list[[i]]<-p
  variables <- ggarrange(plotlist=plot_list, nrow =4, ncol=4)
  ggsave(paste0(HisDir, '/', Sys.Date(),  'stacked_full_histograms.pdf'), variables, width=13, height=13)
}


#______________________________ Stacked hist__________________________________________

for (i in 1:16) { 
  p<- ggplot(clu_df_cont, aes_string(x=names(clu_df_cont)[var_list[[i]]])) + 
    geom_histogram(show.legend = FALSE, fill = "#FF847CFF")+
    geom_freqpoly(aes(x=p_test), color = "orange4")+
    theme_minimal()+
    theme(text = element_text(size=16))+
    labs (title = names(clu_df_cont)[var_list[[i]]], x = "values") +
    xlab("")
  plot_list[[i]]<-p
  variables <- ggarrange(plotlist=plot_list, nrow =4, ncol=4)
  ggsave(paste0(HisDir, '/', Sys.Date(),  'stacked_lined_histograms.pdf'), variables, width=13, height=13)
}

#_______________________combined 



social <- melt(clu_df_cont[,c("v001", "edu_a", "housing_q", "all_female_sex", "wealth",
                           "p_test")], id.vars = "v001")

climate <- melt(clu_df_cont[,c("v001", "temp_all_yrs_2000m", "dominant_vector_2000m", "secondary_vector_2000m", 
                               "p_test")], id.vars = "v001")

travel <- melt(clu_df_cont[,c("v001", "motorized_travel_healthcare_2019_2000m", "minutes_walking_healthcare_2000m", "minutes_to_city_2000m", 
                               "p_test")], id.vars = "v001")

behavior <- melt(clu_df_cont[,c("v001", "net_use_child", "med_treat_fever", "ACT_use_U5", 
                              "p_test")], id.vars = "v001")

terain <- melt(clu_df_cont[,c("v001", "elev_2000m", "minutes_travel_metre_2015_2000m", "minutes_travel_metre_2019_2000m", "minutes_walking_metre_2000m", 
                              "p_test")], id.vars = "v001")

var_list = c(1:5)
melted_data <- c(social, climate, terain, travel, behavior)

for (i in 1:5) { 
p <- ggplot(melted_data[var_list[[i]]], aes(x= value, fill = variable, color = variable)) +
  geom_density(alpha = 0.1) +
  theme(text = element_text(size=16))+
  #labs (title = quote(melted_data[var_list[[i]]]))
plot_list[[i]]<-p
variables <- ggarrange(plotlist=plot_list, nrow =3, ncol=3)
ggsave(paste0(HisDir, '/', Sys.Date(),  'stacked_lined_histograms.pdf'), variables, width=13, height=13)
}

p <- ggplot(social, aes(x= value, fill = variable, color = variable)) +
  geom_density(alpha = 0.1) +
  theme(text = element_text(size=16))
  #labs (title = quote(melted_data[var_list[[i]]]))
  plot_list[[i]]<-p
variables <- ggarrange(plotlist=plot_list, nrow =3, ncol=3)
ggsave(paste0(HisDir, '/', Sys.Date(),  'combined_density_histograms.pdf'), variables, width=13, height=13)


###################################################################################
####clustering
###################################################################################
#set.seed(786)
urbandataset <- clu_df_cont

standardize <- function(x){(x-min(x))/(max(x)-min(x))}

urbandataset$norm_building <- standardize(urbandataset$build_count)
urbandataset$norm_popden <- standardize(urbandataset$pop_density_2000msity_2000m)
urbandataset$norm_housing <- standardize(urbandataset$housing_q)

cluster_df <- data.frame(urbandataset$norm_building, urbandataset$norm_popden, urbandataset$norm_housing)

dist_mat <- dist(cluster_df, method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'centroid')
plot(hclust_avg)
############################################################################################
######################Elbow Method for finding the optimal number of clusters
set.seed(123)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 15
data <- cluster_df 
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss

plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

####### using the number of optimal clusters

cut_avg <- cutree(hclust_avg, k = 3)

urban_clustering <- mutate(urbandataset, cluster = cut_avg)
count_urban<-count(urban_clustering,cluster)

write_csv(count_urban, file.path(ResultDir, 'count_urban_analysis.csv'))

urban_clu_sum <- urban_clustering %>%  group_by(cluster) 
  summarise(med_pop_density_2000m = median(pop_density_2000m), med_housing=mean(housing_q),  med_build=median(build_count))

pop_density_2000msity_clustering<- ggplot(urban_clustering, aes(x=as.factor(cluster), y=pop_density_2000m, fill = as.factor(cluster))) + 
  geom_boxplot()+ 
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_paletteer_d("awtools::spalette")+
  theme_minimal()+
  xlab('Archetype number')+
  ylab('Population density')+ 
  theme(legend.position = 'none')


house_qua_clustering<- ggplot(urban_clustering, aes(x=as.factor(cluster), y=housing_q, fill = as.factor(cluster))) + 
  geom_boxplot()+ 
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_paletteer_d("awtools::spalette")+
  theme_minimal()+
  xlab('Archetype number')+
  ylab('Housing quality')+ 
  theme(legend.position = 'none')


build_den_clustering<-ggplot(urban_clustering, aes(x=as.factor(cluster), y=build_count, fill = as.factor(cluster))) + 
  geom_boxplot()+ 
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_paletteer_d("awtools::spalette")+
  theme_minimal()+
  xlab('Archetype number')+
  ylab('Building density')+ 
  theme(legend.position = 'none')

ggsave(paste0(Man_Dir, '/', Sys.Date(),  'pop_density_2000msity_urban.pdf'), pop_density_2000msity_clustering, width=13, height=13)
ggsave(paste0(Man_Dir, '/', Sys.Date(),  'housing_qlity_urban.pdf'), house_qua_clustering, width=13, height=13)
ggsave(paste0(Man_Dir, '/', Sys.Date(),  'building_den_urban.pdf'),build_den_clustering, width=13, height=13)


write_csv(urban_clu_sum, file.path(Man_Dir, 'summary_stat_urban_clustering analysis.csv'))

u_df_18 <- urban_clustering %>% filter(data_source == "dhs2018") 
u_df_18_fin <- st_join(u_dhs18_sf , u_df_18, by = c("DHSCLUST" ="hv001"))
u_df_15 <- urban_clustering %>% filter(data_source == "mis2015") 
u_df_15_fin <- st_join(u_mis15_sf, u_df_15, by = c("DHSCLUST" ="hv001"))
u_df_10 <- urban_clustering %>% filter(data_source == "mis2010") 
u_df_10_fin <- st_join(u_mis10_sf,u_df_10, by = c("DHSCLUST" = "hv001"))

clustering_map<-tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
  tm_polygons()+
  tm_shape(u_df_18_fin)+
  tm_bubbles(size =1, col = "cluster", 
             border.col= "black", palette="seq",textNA = "Missing",
             breaks=c(1, 2, 3, 4), legend.col.show=T)+
  tm_shape(u_df_15_fin)+
  tm_bubbles(size =0.2, col = "cluster", 
             border.col= "black", palette="seq",textNA = "Missing",
             breaks=c(1, 2, 3, 4), legend.col.show=T)+
  tm_shape(u_df_10_fin)+
  tm_bubbles(size =0.2, col = "cluster", 
             border.col= "black", palette="seq",textNA = "Missing",
             breaks=c(1, 2, 3, 4), legend.col.show=T)+
  tm_layout(aes.palette = list(seq ="-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")

clustering_map

tmap_save(tm =clustering_map, filename = file.path(Man_Dir, "clustering_urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



###################################################################################
####sampling issues 
###################################################################################


#how many areas were sampled vs areas that have malaria 
malaria_areas =tm_shape(state_sf) + 
  tm_polygons()+
  tm_text('NAME_1')+
  tm_shape(df_18_fin)+ 
  tm_bubbles(size =0.2, col = "prev_cat", 
             border.col= "black", palette="seq",textNA = "Missing",
             legend.col.show=T)+
  tm_shape(df_15_fin)+ 
  tm_bubbles(size =0.2, col = "prev_cat", 
             border.col= "black", palette="seq",textNA = "Missing", legend.col.show=F)+
  tm_shape(df_10_fin)+ 
  tm_bubbles(size =0.2, col = "prev_cat", 
             border.col= "black", palette="seq",textNA = "Missing", legend.col.show=F)+
  tm_layout(aes.palette = list(seq ="-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")

areas_sampled = tm_shape(state_sf) + 
  tm_polygons()+
  tm_text('NAME_1')+
  tm_shape(dhs18_sf)+
  tm_bubbles(size =0.2, col="green")+
  tm_shape(mis15_sf)+
  tm_bubbles(size =0.2, col="green")+
  tm_shape(mis10_sf)+
  tm_bubbles(size =0.2, col="green")

map_state_samples =tmap_arrange(areas_sampled, malaria_areas)
map_state_samples

#look at edo 

LGAshp_sf = st_as_sf(LGAshp) %>%  filter(State == 'Edo')

edo_dhs_18 = dhs_18 %>%  filter(ADM1NAME  == 'EDO')
edo_dhs_10 = dhs_10 %>%  filter(ADM1NAME  == 'EDO')

edo_malaria =tm_shape(LGAshp_sf) + 
  tm_polygons()+
  tm_text('LGA')+
  tm_shape(edo_dhs_18)+ 
  tm_bubbles(size =0.2, col = "prev_cat", 
             border.col= "black", palette="seq",textNA = "Missing", legend.col.show=F)+
  tm_shape(edo_dhs_10)+ 
  tm_bubbles(size =0.2, col = "prev_cat", 
             border.col= "black", palette="seq",textNA = "Missing", legend.col.show=F)+ 
  tm_layout(aes.palette = list(seq ="-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")



#look at lagos  

LGAshp_sf = st_as_sf(LGAshp) %>%  filter(State == 'Lagos')

lag_dhs_18 = dhs_18 %>%  filter(ADM1NAME  == 'LAGOS')
lag_dhs_10 = dhs_10 %>%  filter(ADM1NAME  == 'LAGOS')


#google key for extracting locations 
register_google(key = '')
getOption("ggmap")

locations <- c('Banana Island, Lagos', 'Makoko, Lagos', 'Ajegunle, Lagos') %>%
  geocode()


loc_sf <- st_as_sf(locations, coords = c("lon", "lat"), crs = 4326)
class(loc_sf)

loc_sf$place_name = c('Banana Island', 'Makoko', 'Ajengunle')



# tm_shape(LGAshp_sf) + 
#   tm_polygons()+
#   tm_text('LGA', size = 0.8)+
#   tm_shape(df_18_fin)+ 
#   tm_bubbles(size =0.2)+
#   tm_shape(loc_sf)+
#   tm_bubbles(size =0.5, col = 'red')

#missing malaria prevalence dataframe in lagos 

miss_prev_cat = lag_dhs_18 %>%  filter(prev_cat == 'missing')


miss_lag_malaria=tm_shape(LGAshp_sf) + 
  tm_polygons()+
  tm_text('LGA')+
  tm_shape(miss_prev_cat)+ 
  tm_bubbles(size =0.2, col = 'grey', 
             border.col= "black", palette="seq", legend.col.show=F)+ 
  tm_shape(loc_sf)+
  tm_bubbles(size =0.5, col = 'red')+
  tm_text('place_name')+
  tm_layout(title = 'areas with  missing data')


geocode_lag = tm_shape(LGAshp_sf) +
  tm_polygons()+
  tm_shape(loc_sf)+
  tm_bubbles(size =0.5, col = 'red')+
  tm_text('place_name')+
  tm_layout(title = 'geocoded areas (makoko, banana island, ajegunle')

# areas with data in lagos 

dat_prev_cat = lag_dhs_18 %>%  filter(prev_cat == 'data available')
dat_lag_malaria =
  tm_shape(LGAshp_sf) + 
  tm_polygons()+
  tm_text('LGA')+
  tm_shape(dat_prev_cat)+ 
  tm_bubbles(size =0.2, col = "prev_cat", 
             border.col= "black", palette="seq",textNA = "Missing", legend.col.show=F)+ 
  tm_layout(aes.palette = list(seq ="-RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")+
  tm_shape(loc_sf)+
  tm_bubbles(size =0.5, col = 'red')+
  tm_text('place_name')+
  tm_layout(title = 'areas with data')

map_state_samples =tmap_arrange(miss_lag_malaria, dat_lag_malaria, geocode_lag, nrow = 2)
map_state_samples

tmap_save(tm = map_state_samples, filename =paste0(PrintDir, '/', Sys.Date(),  '_sample_leakage_2018_DHS_lagos.pdf')
          , width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


tmap_save(tm = edo_malaria, filename =paste0(PrintDir, '/', Sys.Date(),  '_sample_leakage_2018_lagos.pdf')
          , width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

#sampling bias assessment 
#read in key data 

#2018 
lga_dhsid_2018 <- read.csv(file.path(DataDir, 'DHS_ID_and_LGAs', '2018_DHS_ID_and_LGA.csv')) %>% 
  rename(hv001 = v001)

df_18 <- clu_df_10_18 %>% filter(data_source == "dhs2018", Rural_urban == 1) 

dhs_18 <- left_join(df_18, lga_dhsid_2018, by = "hv001")



#2015 
lga_dhsid_2015 <- read.csv(file.path(DataDir, 'DHS_ID_and_LGAs', '2015_DHS_ID_and_LGA.csv')) %>% 
  rename(hv001 = v001)


df_15 <- clu_df_10_18 %>% filter(data_source == "mis2015", Rural_urban == 1) 


dhs_15 <- left_join(df_15, lga_dhsid_2015, by = "hv001")




#2010 
lga_dhsid_2010 <- read.csv(file.path(DataDir, 'DHS_ID_and_LGAs', '2010_DHS_ID_and_LGA.csv')) %>% 
  rename(hv001 = v001)


df_10 <- clu_df_10_18 %>% filter(data_source == "mis2010") 


dhs_10 <- left_join(df_10, lga_dhsid_2010, by = "hv001")


# #which LGA do we see the most DHS surveys 
# 
# sum_18 = dhs_18 %>% group_by(State, LGA) %>%  summarise(num_clusters = n())
# sum_15 = dhs_15 %>% group_by(State, LGA) %>%  summarise(num_clusters = n())
# sum_10 = dhs_10 %>% group_by(State, LGA) %>%  summarise(num_clusters = n())
# 
# #Let's look at Lagos in 2018 
# 
# lag_sum_18 = sum_18 %>%  filter(State == 'Lagos')
# lag_sum_15 = sum_15 %>%  filter(State == 'Lagos')
# lag_sum_10 = sum_10 %>%  filter(State == 'Lagos')


# let's make a map of DHS urban malaria samples 

 ###END 
