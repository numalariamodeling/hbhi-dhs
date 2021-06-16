rm(list=ls())

x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "sp", "sf", "tmap", 'paletteer', 'cowplot', 'gridExtra')



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
PubDir <- file.path(ProjectDir, 'project_notes/publication/Urban-rural determinants of malaria infection in Nigeria')
PrintDir <- file.path(PubDir, 'Illustrations')



####################################################################################
####functions 
###################################################################################
clustermap<-function(map_sf_file, cluster_shp, title){
  tm_shape(map_sf_file) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(cluster_shp)+ #this is the points shape file with LLIN and number of kids info by cluster 
    tm_bubbles(size =0.2, col = "p_test", 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(0, 0.2, 0.3,0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"), title = title)+
    tm_legend(legend.title.size = 0.8, legend.just="top")
}




###################################################################################
####Loading data
###################################################################################
# Load pre-clustered data:
clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"),
                               header = T, sep = ',')
summary(clu_variales_10_18$interview_month)#remember to adjust for the survey month in the model 

####################################################################################
####Analysis data 
###################################################################################
#filtering by residence type
urbandataset <- clu_variales_10_18 %>% filter(Rural_urban == 1) %>%  
  dplyr::select(hv001,p_test, wealth_2, u5_prop, preg,edu_a, hh_size, ACT_use_u5,pop_den,
                hh_members_age, sex_f, data_source, humidindex, Rural_urban,
                annual_precipitation,housing_qua, net_use, build_count, state, region, pop_count, housing_qua, interview_month) %>% 
  na.omit()



####################################################################################
####Urban malaria state maps 
###################################################################################
#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)


#Loading urban cluster points

dhs18_sf <- st_read(file.path(DataDir,"NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),) %>%   filter(URBAN_RURA == 'U')
mis15_sf <- st_read(file.path(DataDir,"NG_2015_MIS_06192019/NGGE71FL/NGGE71FL.shp"),) %>%  filter(URBAN_RURA == 'U')
mis10_sf <- st_read(file.path(DataDir,"NG_2010_MIS_06192019/NGGE61FL/NGGE61FL.shp"),)%>%  filter(URBAN_RURA == 'U')


#join clusters to variables
df_18 <- urbandataset %>% filter(data_source == "dhs2018") %>% dplyr::select(hv001, p_test, state)
df_18$prev_cat <- ifelse(is.na(df_18$p_test),  "missing", "data available") 
dhs_18 <- left_join(dhs18_sf, df_18, by = c("DHSCLUST" ="hv001")) #447 clusters had prevalence and covariate data of the 576 areas that were sampled in urban areas 


df_15 <- urbandataset %>% filter(data_source == "mis2015", Rural_urban == 1) %>% dplyr::select(hv001, p_test, state)
df_15$prev_cat <- ifelse(is.na(df_15$p_test),  "missing", "data available") 
dhs_15 <- left_join(mis15_sf, df_15, by = c("DHSCLUST" ="hv001"))



df_10 <- urbandataset %>% filter(data_source == "mis2010") %>% dplyr::select(hv001, p_test, state)
df_10$prev_cat <- ifelse(is.na(df_10$p_test),  "missing", "data available") 
dhs_10 <- left_join(mis10_sf, df_10, by = c("DHSCLUST" ="hv001"))


map_18<-clustermap(state_sf, dhs_18, "2018 malaria prevalence by cluster (DHS)")
map_15 <-clustermap(state_sf, dhs_15, "2015 malaria prevalence by cluster (DHS)")
map_10 <-clustermap(state_sf, dhs_10, "2010 malaria prevalence by cluster (DHS)")


urban_map<-tmap_arrange(map_18, map_15, map_10)
urban_map
tmap_save(tm =urban_map, filename = file.path(PrintDir, "urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



###################################################################################
####Barplot of urban malaria prevalence across all clusters 
###################################################################################

paletteer::paletteer_d("awtools::a_palette")

# Binarize response:
urbandataset$y <- ifelse(urbandataset$p_test < 0.1, "less than 10%", "greater than 10%") 


u_bar <- ggplot(urbandataset, aes(x=as.factor(y), fill=as.factor(y))) + 
  geom_bar()+ 
  scale_fill_paletteer_d("awtools::spalette")+
  geom_text(aes(label = scales::percent(..prop..), group = 1), stat= "count", vjust =-1)+
  theme_minimal()+
  theme(legend.position = "none")+
  xlab("Malaria prevalence")+
  ylab("Number of clusters")

u_bar


ggsave(paste0(PrintDir , '/', Sys.Date(),  'combined_urban_malaria_clusters_percent.pdf'), u_bar, width=13, height=13)




###################################################################################
####glm
###################################################################################

urbandataset$y <- ifelse(urbandataset$p_test < 0.1, 0, 1) 

w_glm <- glm(y~ 1+ wealth_2,
             data = urbandataset, family = "binomial")

summary(w_glm)


e_glm <- glm(y~ 1+ edu_a,
             data = urbandataset, family = "binomial")

summary(e_glm)



sex_fglm <- glm(y~ 1+ sex_f,
                data = urbandataset, family = "binomial")

summary(sex_fglm)


sex_fglm <- glm(y~ 1+ ACT_use_u5,
                data = urbandataset, family = "binomial")

summary(sex_fglm)


u_glm <- glm(y~ 1+ wealth_2+ edu_a + sex_f+  ACT_use_u5 +  hh_size
             pop_count + hh_members_age + net_use  + humidindex+ annual_precipitation,
             data = urbandataset, family = "binomial")

