
x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "sp", "sf", "tmap", 'paletteer', 'cowplot', 'gridExtra', 'lme4')




lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)

###################################################################################
####Directories
###################################################################################

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src", "DHS")
VarDir <- file.path(SrcDir, "1_variables_scripts")
ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
PresentDir<-file.path(NuDir, "presentations")
Personal_P_Dir <- file.path(NuDir, "presentations", "team member archive_Ifeoma", "210409_EPI_seminar", "pictures")
Sem_Dir <- file.path(NuDir, "presentations", "team member archive_Ifeoma", "210409_EPI_seminar")
Man_Dir <- file.path(ProjectDir, "project_notes", "publication", "Urban-rural determinants of malaria infection in Nigeria", "Illustrations")


###################################################################################
####Loading data
###################################################################################
# Load pre-clustered data:


# Load pre-clustered data:
clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), 
                               header = T, sep = ',')


#Loading cluster points

dhs18_sf <- st_read(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) 
mis15_sf <- st_read(file.path(DataDir,"NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),)
mis10_sf <- st_read(file.path(DataDir,"NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),)

# join dhs variables to cluster points by year 

df_18 <- clu_variales_10_18 %>% filter(data_source == "dhs2018") 
df_18_fin <- left_join(dhs18_sf, df_18, by = c("DHSCLUST" ="hv001"))
df_15 <- clu_variales_10_18 %>% filter(data_source == "mis2015") 
df_15_fin <- left_join(mis15_sf, df_15, by = c("DHSCLUST" ="hv001"))
df_10 <- clu_variales_10_18 %>% filter(data_source == "mis2010") 
df_10_fin <- left_join(mis10_sf, df_10, by = c("DHSCLUST" = "hv001"))
clu_variales_10_18 <- rbind(df_18_fin, df_15_fin, df_10_fin)


#cleaning precip data
clu_variales_10_18 <- clu_variales_10_18%>% mutate(annual_precipitation = scale(clu_variales_10_18$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

clu_variales_10_18 <- clu_variales_10_18 %>% mutate(pop_den = na_if(pop_den, -9999))
clu_variales_10_18$annual_precipitation <- replace(clu_variales_10_18$annual_precipitation, which(clu_variales_10_18$annual_precipitation < 0), NA)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(log_pop_den = log(pop_den))


#filtering by residence type
urbandataset <- clu_variales_10_18 %>% filter(Rural_urban == 1) %>%  
  dplyr::select(p_test, wealth_2, u5_prop, preg,edu_a, hh_size, ACT_use_u5,pop_den,
                hh_members_age, sex_f, data_source, humidindex, Rural_urban, annual_precipitation,housing_qua, net_use, build_count, state, region, pop_count, housing_qua) %>% 
  na.omit()

table(urbandataset$data_source)

# Binarize response:
urbandataset$y <- ifelse(urbandataset$p_test < 0.1, 0,1)


#data cleaning

missing_values <- sapply(urbandataset, function(x) sum(is.na(x)))
summary(urbandataset$pop_count)

urbandataset <- urbandataset %>% filter(annual_precipitation>=0, pop_den != -9999)
table(urbandataset$data_source)


###################################################################################
####urban maps
###################################################################################


# urban cluster points

u_dhs18_sf <- dhs18_sf %>% filter(URBAN_RURA =='U')
u_mis15_sf <- mis15_sf  %>% filter(URBAN_RURA =='U')
u_mis10_sf <- mis10_sf %>% filter(URBAN_RURA =='U')



#make an urban map of all cluster values 

u_df_18 <- urbandataset %>% filter(data_source == "dhs2018") 
u_df_18_fin <- st_join(u_dhs18_sf , u_df_18, by = c("DHSCLUST" ="hv001"))
u_df_15 <- urbandataset %>% filter(data_source == "mis2015") 
u_df_15_fin <- st_join(u_mis15_sf, u_df_15, by = c("DHSCLUST" ="hv001"))
u_df_10 <- urbandataset %>% filter(data_source == "mis2010") 
u_df_10_fin <- st_join(u_mis10_sf,u_df_10, by = c("DHSCLUST" = "hv001"))


#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)

#make cluster maps 

clustermap<-function(cluster_shp, title){
  tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(cluster_shp)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size =0.2, col = "p_test", 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}

map_18<-clustermap(u_df_18_fin, "2018 malaria prevalence by cluster (DHS)")
map_15 <-clustermap(u_df_15_fin, "2015 malaria prevalence by cluster (DHS)")
map_10 <-clustermap(u_df_10_fin, "2010 malaria prevalence by cluster (DHS)")


urban_map<-tmap_arrange(map_18, map_15, map_10)


tmap_save(tm =urban_map, filename = file.path(Man_Dir, "urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


###################################################################################
####urban barplots
###################################################################################

paletteer::paletteer_d("awtools::a_palette")

# Binarize response:
urbandataset$y <- ifelse(urbandataset$p_test < 0.1, "less than 10%", "greater than 10%") 


u_bar <- ggplot(urbandataset, aes(x=as.factor(y), fill=as.factor(y))) + 
  geom_bar()+ 
  scale_fill_paletteer_d("awtools::spalette")+
  #geom_text(stat='count', aes(label=..count..), vjust=-1)+
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust =-1)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab("Malaria prevalence")+
  ylab("Number of clusters")

u_bar



ggsave(paste0(Man_Dir, '/', Sys.Date(),  'combined_urban_malaria_clusters_percent.pdf'), u_bar, width=13, height=13)


###################################################################################
####urban density plots
###################################################################################

density_fun<- function(variable, label){ggplot(urbandataset, aes(x=variable)) + 
    geom_density(fill ="#FF847CFF", color="#FF847CFF", alpha = 0.8)+
    theme_minimal()+
    theme(legend.position = "none", plot.title = element_text(hjust = 0.5))+
    ggtitle(label)+
    xlab("")
}

wealth_plot<-density_fun(urbandataset$wealth_2, "Wealth")
wealth_plot
edu_plot<-density_fun(urbandataset$edu_a, "Education")
edu_plot
fem_plot<-density_fun(urbandataset$sex_f, "Female proportion")
fem_plot



ACT_plot<-density_fun(urbandataset$ACT_use_u5, "Antimalarial use")
ACT_plot


hh_size_plot<-density_fun(urbandataset$hh_size, "Household size")
hh_size_plot


pop_den_plot<-density_fun(urbandataset$pop_count, "Population density")
pop_den_plot

hh_member_age<-density_fun(urbandataset$hh_members_age, "Household member age")
hh_member_age

Net_use<-density_fun(urbandataset$net_use, "Household net use")
Net_use

Humidity_index<-density_fun(urbandataset$humidindex, "Humidity index")
Humidity_index


precipitation<-density_fun(urbandataset$annual_precipitation, "Precipitation")
precipitation

housing_qua<-density_fun(urbandataset$housing_qua, "Housing quality")
housing_qua



building_density<-density_fun(urbandataset$build_count, "building count")
building_density
summary(building_density$data)

plot_list <- list(wealth_plot, edu_plot,fem_plot, ACT_plot, hh_size_plot, pop_den_plot, hh_member_age, Net_use,Humidity_index,precipitation, 
                  building_density, housing_qua)

variables <- ggarrange(plotlist=plot_list, nrow =4, ncol=3)
ggsave(paste0(Man_Dir, '/', Sys.Date(),  'independent_variables_urban.pdf'), variables, width=13, height=13)



###################################################################################
####clustering
###################################################################################
#set.seed(786)

standardize <- function(x){(x-min(x))/(max(x)-min(x))}

urbandataset$norm_building <- standardize(urbandataset$build_count)
urbandataset$norm_popden <- standardize(urbandataset$pop_den)
urbandataset$norm_housing <- standardize(urbandataset$housing_qua)

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

write_csv(count_urban, file.path(Man_Dir, 'count_urban_analysis.csv'))

urban_clu_sum <- urban_clustering %>%  group_by(cluster) %>% 
  summarise(med_pop_den = median(pop_den), med_housing=mean(housing_qua),  med_build=median(build_count))

pop_density_clustering<- ggplot(urban_clustering, aes(x=as.factor(cluster), y=pop_den, fill = as.factor(cluster))) + 
  geom_boxplot()+ 
  #scale_fill_viridis(discrete = TRUE) +
  scale_fill_paletteer_d("awtools::spalette")+
  theme_minimal()+
  xlab('Archetype number')+
  ylab('Population density')+ 
  theme(legend.position = 'none')


house_qua_clustering<- ggplot(urban_clustering, aes(x=as.factor(cluster), y=housing_qua, fill = as.factor(cluster))) + 
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

ggsave(paste0(Man_Dir, '/', Sys.Date(),  'pop_density_urban.pdf'), pop_density_clustering, width=13, height=13)
ggsave(paste0(Man_Dir, '/', Sys.Date(),  'housing_quality_urban.pdf'), house_qua_clustering, width=13, height=13)
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

df_18 <- clu_variales_10_18 %>% filter(data_source == "dhs2018", Rural_urban == 1) 

dhs_18 <- left_join(df_18, lga_dhsid_2018, by = "hv001")



#2015 
lga_dhsid_2015 <- read.csv(file.path(DataDir, 'DHS_ID_and_LGAs', '2015_DHS_ID_and_LGA.csv')) %>% 
  rename(hv001 = v001)


df_15 <- clu_variales_10_18 %>% filter(data_source == "mis2015", Rural_urban == 1) 


dhs_15 <- left_join(df_15, lga_dhsid_2015, by = "hv001")




#2010 
lga_dhsid_2010 <- read.csv(file.path(DataDir, 'DHS_ID_and_LGAs', '2010_DHS_ID_and_LGA.csv')) %>% 
  rename(hv001 = v001)


df_10 <- clu_variales_10_18 %>% filter(data_source == "mis2010") 


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
