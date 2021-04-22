rm(list=ls())

x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "sp", "sf", "tmap", 'paletteer')



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

#setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data")



###################################################################################
####Loading data
###################################################################################
# Load pre-clustered data:
clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), 
                 header = T, sep = ',')



#filtering by residence type
urbandataset <- clu_variales_10_18 %>% filter(Rural_urban == 1) %>%  
  dplyr::select(hv001,p_test, wealth_2, u5_prop, preg,edu_a, hh_size, ACT_use_u5,pop_den,
                hh_members_age, sex_f, data_source, humidindex, Rural_urban, annual_precipitation,housing_qua, net_use, build_count, state, pop_count, housing_qua) %>% 
  na.omit()

table(urbandataset$data_source)

# urbandataset$y <- ifelse(urbandataset$p_test < 0.1, 0, 1) 
# ifelse(!dir.create(file.path(BinDir, 'urban_dataset_DHS')), dir.create(file.path(BinDir, 'urban_dataset_DHS')), FALSE)
# 
write_csv(urbandataset, paste0(BinDir, '/urban_dataset_DHS/urbandataset.csv'))

missing_values <- sapply(urbandataset, function(x) sum(is.na(x)))
summary(urbandataset$pop_count)

urbandataset <- urbandataset %>% filter(annual_precipitation>=0, pop_den != -9999)
ruraldataset <-clu_variales_10_18 %>% filter(Rural_urban == 2)
comineddataset <- clu_variales_10_18

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




###################################################################################
####urban maps
###################################################################################


# urban cluster points

u_dhs18_sf <- dhs18_sf %>% filter(URBAN_RURA =='U')
u_mis15_sf <- mis15_sf  %>% filter(URBAN_RURA =='U')
u_mis10_sf <- mis10_sf %>% filter(URBAN_RURA =='U')



#make an urban map of all cluster values 

u_df_18 <- urbandataset %>% filter(data_source == "dhs2018") 
u_df_18_fin <- left_join(u_dhs18_sf , u_df_18, by = c("DHSCLUST" ="hv001"))
u_df_15 <- urbandataset %>% filter(data_source == "mis2015") 
u_df_15_fin <- left_join(u_mis15_sf, u_df_15, by = c("DHSCLUST" ="hv001"))
u_df_10 <- urbandataset %>% filter(data_source == "mis2010") 
u_df_10_fin <- left_join(u_mis10_sf,u_df_10, by = c("DHSCLUST" = "hv001"))


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


tmap_save(tm =urban_map, filename = file.path(Personal_P_Dir, "urban_malaria_maps.pdf"), 
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



ggsave(paste0(Personal_P_Dir, '/', Sys.Date(),  'combined_urban_malaria_clusters_percent.pdf'), u_bar, width=13, height=13)


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
ggsave(paste0(Personal_P_Dir, '/', Sys.Date(),  'independent_variables_urban.pdf'), variables, width=13, height=13)



###################################################################################
####clustering
###################################################################################
set.seed(786)
standardize <- function(x){(x-min(x))/(max(x)-min(x))}

urbandataset$norm_building <- standardize(urbandataset$build_count)
urbandataset$norm_popden <- standardize(urbandataset$pop_den)
urbandataset$norm_housing <- standardize(urbandataset$housing_qua)

cluster_df <- data.frame(urbandataset$norm_building, urbandataset$norm_popden, urbandataset$norm_housing)

dist_mat <- dist(cluster_df, method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'centroid')
plot(hclust_avg)

cut_avg <- cutree(hclust_avg, k = 5)


plot(hclust_avg)
rect.hclust(hclust_avg , k = 5, border = 2:6)
abline(h = 3, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, k = 5)
plot(avg_col_dend)


urban_clustering <- mutate(urbandataset, cluster = cut_avg)
count_urban<-count(urban_clustering,cluster)

write_csv(count_urban, file.path(Sem_Dir, 'count_urban_analysis.csv'))

#urban_clustering %>%  filter(cluster == 10)

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

ggsave(paste0(Personal_P_Dir, '/', Sys.Date(),  'pop_density_urban.pdf'), pop_density_clustering, width=13, height=13)
ggsave(paste0(Personal_P_Dir, '/', Sys.Date(),  'housing_quality_urban.pdf'), house_qua_clustering, width=13, height=13)
ggsave(paste0(Personal_P_Dir, '/', Sys.Date(),  'building_den_urban.pdf'),build_den_clustering, width=13, height=13)
  

write_csv(urban_clu_sum, file.path(Sem_Dir, 'summary_stat_urban_clustering analysis.csv'))

u_df_18 <- urban_clustering %>% filter(data_source == "dhs2018") 
u_df_18_fin <- left_join(u_dhs18_sf , u_df_18, by = c("DHSCLUST" ="hv001"))
u_df_15 <- urban_clustering %>% filter(data_source == "mis2015") 
u_df_15_fin <- left_join(u_mis15_sf, u_df_15, by = c("DHSCLUST" ="hv001"))
u_df_10 <- urban_clustering %>% filter(data_source == "mis2010") 
u_df_10_fin <- left_join(u_mis10_sf,u_df_10, by = c("DHSCLUST" = "hv001"))

clustering_map<-tm_shape(state_sf) + #this is the health district shapfile with DS estimates info
    tm_polygons()+
    tm_shape(u_df_18_fin)+
  tm_bubbles(size =1, col = "cluster", 
             border.col= "black", palette="seq",textNA = "Missing",
             breaks=c(1, 2, 3, 4, 5,6), legend.col.show=T)+
    tm_shape(u_df_15_fin)+
  tm_bubbles(size =0.2, col = "cluster", 
             border.col= "black", palette="seq",textNA = "Missing",
             breaks=c(1, 2, 3, 4, 5,6), legend.col.show=T)+
    tm_shape(u_df_10_fin)+
    tm_bubbles(size =0.2, col = "cluster", 
               border.col= "black", palette="seq",textNA = "Missing",
               breaks=c(1, 2, 3, 4, 5,6), legend.col.show=T)+
    tm_layout(aes.palette = list(seq ="-RdYlBu"))+
    tm_legend(legend.title.size = 0.8, legend.just="top")

clustering_map

tmap_save(tm =clustering_map, filename = file.path(Personal_P_Dir, "clustering_urban_malaria_maps.pdf"), 
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

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

summary(ACT_use_u5)


u_glm <- glm(y~ 1+ wealth_2+ edu_a + sex_f+  ACT_use_u5 +  hh_size
                pop_count + hh_members_age + net_use  + humidindex+ annual_precipitation,
              data = urbandataset, family = "binomial")
summary(u_glm)
summary(urbandataset$pop_count)
###################################################################################
####data analysis  
###################################################################################





# Binarize response:
clu_variales_10_18$y <- ifelse(clu_variales_10_18$p_test < 0.1, 0,1)

#cleaning precip data
clu_variales_10_18 <- clu_variales_10_18%>% mutate(annual_precipitation = scale(clu_variales_10_18$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

clu_variales_10_18 <- clu_variales_10_18 %>% mutate(pop_den = na_if(pop_den, -9999))
clu_variales_10_18$annual_precipitation <- replace(clu_variales_10_18$annual_precipitation, which(clu_variales_10_18$annual_precipitation < 0), NA)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(log_pop_den = log(pop_den))







#sub setting variables of interest 
clu_variales_10_18 <- as.data.frame(clu_variales_10_18)
clu_variales_10_18 <- clu_variales_10_18[,c("y", "wealth_2", "edu_a", "hh_size",
                                            "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
                                            "annual_precipitation", "net_use", "Rural_urban", "data_source", "state", "build_count", "region", "LONGNUM", "LATNUM", "geometry")]


# Create reduced dataset for model 2:
# (filter out rows with NA values)






###################################################################################
####rural dataset
###################################################################################
  


#regular rural model without random effect 
r_rmod <- inla(y ~ 1 + wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
               data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE)) 

summary(r_rmod)



#model with random intercept in state and random slope in education
ruraldataset$state_2 <- ruraldataset$state
r_iid_s <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
              + humidindex +f(state, model = "iid") + f(state_2, net_use, model = "iid"), family = 'binomial',
              data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
              control.compute = list(cpo=TRUE, dic = TRUE))

summary(r_iid_s) 




#model with random intercept in state 
r_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
              + humidindex +f(state, model = "iid"), family = 'binomial',
              data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
              control.compute = list(cpo=TRUE, dic = TRUE))

summary(r_iid)


###################################################################################
####final rural model
###################################################################################

#model with random intercept in state and region
r_iid2 <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
               + humidindex +f(state, model = "iid") + f(region, model = "iid") +  f(state_2, net_use, model = "iid"), family = 'binomial',
               data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE))

summary(r_iid2) # model with the lowest dic and marginal loglikelihood

#C##################onverting between  Log-odds to Probability#################

#extraction and converting results to dataframe
r_iid2_fixed <- as.data.frame(r_iid2$summary.fixed)

#making column names a column
r_iid2_fixed <- cbind(coefs = rownames(r_iid2_fixed), r_iid2_fixed)
rownames(r_iid2_fixed) <- 1:nrow(r_iid2_fixed)

#changing row names
names(r_iid2_fixed)[names(r_iid2_fixed) == "0.975quant"] <- "logodds975"
names(r_iid2_fixed)[names(r_iid2_fixed) == "0.5quant"] <- "logodds5"
names(r_iid2_fixed)[names(r_iid2_fixed) == "0.025quant"] <- "logodds25"

#
r_iid2_prob <- r_iid2_fixed %>% mutate(probs_0.975 = plogis(logodds975))
r_iid2_prob <- r_iid2_prob %>% mutate(probs_0.5 = plogis(logodds5))
r_iid2_prob <- r_iid2_prob %>% mutate(probs_0.025 = plogis(logodds25))
r_iid2_prob <- r_iid2_prob[,c("coefs", "probs_0.025", "probs_0.5", "probs_0.975")]

#We can calculate the probabilites that malaria prevalence rates are higher for all hospitals
#These exceedance probabilities indicate that the probability that higher malaria prevalence 
#exceeds 0.1 is highest for observation number 1138 (probability equal to 1)

#pros <- sapply(r_iid2$marginals.fitted.values,
#               FUN = function(marg){1-inla.pmarginal(q = 0.1, marginal = marg)})

#probs_df <- as.data.frame(pros)

#plots of the posterior for the betas 

plot_fun<- function(data, x_label){
  ggplot(data.frame(inla.smarginal(data)), aes(x, y)) +
    geom_line(color = "green") +
    theme_bw()+
    geom_vline(xintercept=0, linetype="dashed", color = "red")+
    xlab(x_label)+
    ylab("")
}


data <- list(r_iid2$marginals.fixed$`(Intercept)`,r_iid2$marginals.fixed$wealth_2, r_iid2$marginals.fixed$edu_a, r_iid2$marginals.fixed$net_use, 
             r_iid2$marginals.fixed$hh_size, r_iid2$marginals.fixed$ACT_use_u5, r_iid2$marginals.fixed$hh_members_age,
             r_iid2$marginals.fixed$sex_f, r_iid2$marginals.fixed$`log(annual_precipitation)`, r_iid2$marginals.fixed$`log(build_count)`,
             r_iid2$marginals.fixed$humidindex)

labels_data <- list("intercept", "Highest wealth quintile", "Education", "Bednet Use", "Average Household size", "ACT_use",
                    "Average Household age", "Proportion of females", "log(annual precipitation)", 
                    "log(build_count)", "humidity index")

plots<-map2(data, labels_data, plot_fun)

figure<-ggarrange(plotlist = plots, nrow =4, ncol=3)
figure<-annotate_figure(figure, left = "Density")
figure


#extracting state random intercept 
r_random_effects_ <- r_iid2$summary.random[[1]]

#extracting factors 
r_random_effects_$ID <-str_to_title(r_random_effects_$ID)
r_random_effects_$ID <- factor(r_random_effects_$ID, levels=rev(r_random_effects_$ID))
r_random_effects_$ID<- trimws(r_random_effects_$ID)


#quick plot of state_random intercept 

r_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(r_fp)# no significant state variations 


#map 






state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)
))

r_map_df <- left_join(state_sf, r_random_effects_, by =c("NAME_1" = "ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
#setting cluster points 
cluster_points <- st_as_sf(ruraldataset)# %>% 
  #st_as_sf('LONGNUN', 'LATNUN')

r_map <- tm_shape(r_map_df)+
  tm_polygons(col = "mean", midpoint =NA, palette = "-RdYlGn")+
  tm_text("NAME_1") +
  tm_shape(cluster_points, col = "mean", midpoint =NA, palette = "-RdYlGn") +
  tm_symbols(col = "azure4", scale = .3)
 

r_map


#extracting region random intercept 
r_random_effects_ <- r_iid2$summary.random[[2]]
re_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(re_fp)#significant variation in the south west increasing likelihood of transmission and decreased likelihood in the north 


#map 



#############################################################################################
#########regional map
#read in state shape file 
regionshp <-  readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
region_sf <- st_as_sf(regionshp)



region_sf <- region_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                         NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                         TRUE ~ as.character(NAME_1)
))


                     
regions <- clu_variales_10_18[,c("state", "region")]


region_sf <- region_sf %>% mutate(NAME_1 = tolower(NAME_1))


region_sf <- left_join(region_sf, regions, by =c("NAME_1" = "state"))

#We have missing data in three states below, we assign the region mannually
region_sf$region[which(region_sf$NAME_1 =="kwara")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="plateau")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="zamfara")]<-"north west"

#grouping states into regions
region_sf <- region_sf %>% group_by(NAME_1) %>% slice(1L)
region_sf_na<-subset(region_sf,region_sf$region=="NA")

r_map_df <- left_join(region_sf, r_random_effects_, by =c("region" = "ID"))
#r_map_df  <- r_map_df %>% group_by(region) %>% slice(1L)
r_map_df <- st_as_sf(r_map_df)

r_map_df%>% group_by(region) %>% 
  summarise(geometry = sf::st_union(geometry)) %>% ungroup()

# we see spatial clustering of state-level random effects, although state effects are not statistically significant
r_map <- tm_shape(r_map_df)+
  tm_fill(col = "mean", midpoint =NA, palette = "-RdYlGn", breaks = c(-1, -0.5, -0.16, 0, 0.05, 1, 1.5)) +
  tm_borders() +
  #tm_text("NAME_1", size = 0.8, col = "snow4") +
  tm_shape(cluster_points) +
  tm_symbols(col = "black", scale = .3)


r_map



###################################################################################
####urban dataset
###################################################################################
#regular urban model without random effect 
u_rmod <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
               hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
             data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE)) 

summary(u_rmod)

#model with random effects in state
u_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
              hh_members_age + sex_f + log(annual_precipitation) +log(build_count)+  humidindex +f(state, model = "iid"), family = 'binomial',
            data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE))

summary(u_iid)


library(lme4)
#st_geometry(urbandataset) <- NULL
# 
#urbandataset$log_annual_precipitation<-log(urbandataset$annual_precipitation)
#urbandataset$log_build_count<-log(urbandataset$build_count)
#urbandataset$annual_precipitation<-NULL
# 
#urbandataset$build_count<-NULL
# u_lm <- glmer(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
#                 hh_members_age + sex_f + log_annual_precipitation +log_build_count+(1|state),family = binomial(link = "logit"),
#               data = urbandataset)
# 
# 
# 
num_cols<-c("wealth_2", "edu_a", "net_use", "hh_size", "ACT_use_u5",
             "hh_members_age", "sex_f", "log_annual_precipitation" , "log_build_count")
# 
# 
urbandataset_scaled  <- urbandataset
urbandataset_scaled [,num_cols] <- scale(urbandataset_scaled [,num_cols])
# m1_sc <- update(u_lm,data=urbandataset_scaled )
# summary(m1_sc)




u_lm <- glmer(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log_annual_precipitation +log_build_count+(1|state),family = binomial(link = "logit"),
              data = urbandataset_scaled)

summary(u_lm)

#extracting state random effects 
u_random_effects <- u_iid$summary.random

#changing to dataframe
u_random_effects_<- do.call(cbind, u_random_effects)

#extracting factos 
u_random_effects_$state.ID <-str_to_title(u_random_effects_$state.ID)
u_random_effects_$state.ID <- factor(u_random_effects_$state.ID, levels=rev(u_random_effects_$state.ID))
u_random_effects_$state.ID<- trimws(u_random_effects_$state.ID)

#quick plot of state random effect. South-West state have statistically significant effects
library(ggplot2)
u_fp <- ggplot(data=u_random_effects_, aes(x=state.ID, y=state.mean, ymin=state.0.025quant, ymax=state.0.975quant)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(u_fp)


#map
u_map_df <- left_join(state_sf, u_random_effects_, by =c("NAME_1" = "state.ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
u_map <- tm_shape(u_map_df)+
  tm_polygons(col = "state.mean", midpoint =NA, palette = "-RdYlGn", breaks =c(-1.5, -1.0, -0.5, 0, 0.2, 1.0, 1.5, 2.0))+
  tm_text("NAME_1")

u_map


#regular urban inlamodel without random effect 
u_rmod <- inla(y ~ 1 + wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
               data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE)) 

summary(u_rmod)



#model with random intercept in state and random slope in education
urbandataset$state_2 <- urbandataset$state
u_iid_s <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                  hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
                + humidindex +f(state, model = "iid") + f(state_2, net_use, model = "iid"), family = 'binomial',
                data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
                control.compute = list(cpo=TRUE, dic = TRUE))

summary(u_iid_s) 




#model with random intercept in state 
u_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
              + humidindex +f(state, model = "iid"), family = 'binomial',
              data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
              control.compute = list(cpo=TRUE, dic = TRUE))

summary(u_iid)


###################################################################################
####final urban model
###################################################################################

#model with random intercept in state and region
urbandataset$state_2 <- urbandataset$state
u_iid2 <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
               + humidindex +f(state, model = "iid") + f(region, model = "iid") +  f(state_2, net_use, model = "iid"), family = 'binomial',
               data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE))

summary(u_iid2) # model with the lowest dic and marginal loglikelihood


#extraction and converting results to dataframe
u_iid2_fixed <- as.data.frame(u_iid2$summary.fixed)

#making column names a column
u_iid2_fixed <- cbind(coefs = rownames(u_iid2_fixed), u_iid2_fixed)
rownames(u_iid2_fixed) <- 1:nrow(u_iid2_fixed)

#changing row names
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.975quant"] <- "logodds975"
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.5quant"] <- "logodds5"
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.025quant"] <- "logodds25"

#
u_iid2_prob <- u_iid2_fixed %>% mutate(probs_0.975 = plogis(logodds975))
u_iid2_prob <- u_iid2_prob %>% mutate(probs_0.5 = plogis(logodds5))
u_iid2_prob <- u_iid2_prob %>% mutate(probs_0.025 = plogis(logodds25))
u_iid2_prob <- u_iid2_prob[,c("coefs", "probs_0.025", "probs_0.5", "probs_0.975")]
u_iid2_prob

#pros <- sapply(u_iid2$marginals.fitted.values,
#               FUN = function(marg){1-inla.pmarginal(q = 0.1, marginal = marg)})

#probs_df <- as.data.frame(pros)

#plots of the posterior for the betas 

plot_fun<- function(data, x_label){
  ggplot(data.frame(inla.smarginal(data)), aes(x, y)) +
    geom_line(color = "green") +
    theme_bw()+
    geom_vline(xintercept=0, linetype="dashed", color = "red")+
    xlab(x_label)+
    ylab("")
}


data <- list(u_iid2$marginals.fixed$`(Intercept)`,u_iid2$marginals.fixed$wealth_2, u_iid2$marginals.fixed$edu_a, u_iid2$marginals.fixed$net_use, 
             u_iid2$marginals.fixed$hh_size, u_iid2$marginals.fixed$ACT_use_u5, u_iid2$marginals.fixed$hh_members_age,
             u_iid2$marginals.fixed$sex_f, u_iid2$marginals.fixed$`log(annual_precipitation)`, u_iid2$marginals.fixed$`log(build_count)`,
             u_iid2$marginals.fixed$humidindex)

labels_data <- list("intercept", "Highest wealth quintile", "Education", "Bednet Use", "Average Household size", "ACT_use",
                    "Average Household age", "Proportion of females", "log(annual precipitation)", 
                    "log(build_count)", "humidity index")

plots<-map2(data, labels_data, plot_fun)

figure<-ggarrange(plotlist = plots, nrow =4, ncol=3)
figure<-annotate_figure(figure, left = "Density")
figure


#extracting state random intercept 
r_random_effects_ <- u_iid2$summary.random[[1]]

#extracting factors 
r_random_effects_$ID <-str_to_title(r_random_effects_$ID)
r_random_effects_$ID <- factor(r_random_effects_$ID, levels=rev(r_random_effects_$ID))
r_random_effects_$ID<- trimws(r_random_effects_$ID)


#quick plot of state_random intercept 

r_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(r_fp)# no significant state variations 


#map 

#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)




state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)
))

r_map_df <- left_join(state_sf, r_random_effects_, by =c("NAME_1" = "ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
r_map <- tm_shape(r_map_df)+
  tm_polygons(col = "mean", midpoint =NA, palette = "-RdYlGn")+
  tm_text("NAME_1")

r_map


#extracting region random intercept 
r_random_effects_ <- u_iid2$summary.random[[2]]
re_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(re_fp)#significant variation in the south west increasing likelihood of transmission and decreased likelihood in the north 


#map 



#############################################################################################
#########regional map
#read in state shape file 
regionshp <-  readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
region_sf <- st_as_sf(regionshp)



region_sf <- region_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                     NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                     TRUE ~ as.character(NAME_1)
))



regions <- clu_variales_10_18[,c("state", "region")]


region_sf <- region_sf %>% mutate(NAME_1 = tolower(NAME_1))


region_sf <- left_join(region_sf, regions, by =c("NAME_1" = "state"))

#We have missing data in three states below, we assign the region mannually
region_sf$region[which(region_sf$NAME_1 =="kwara")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="plateau")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="zamfara")]<-"north west"

#grouping states into regions
region_sf <- region_sf %>% group_by(NAME_1) %>% slice(1L)
region_sf_na<-subset(region_sf,region_sf$region=="NA")

r_map_df <- left_join(region_sf, r_random_effects_, by =c("region" = "ID"))
#r_map_df  <- r_map_df %>% group_by(region) %>% slice(1L)
r_map_df <- st_as_sf(r_map_df)

r_map_df%>% group_by(region) %>% 
  summarise(geometry = sf::st_union(geometry)) %>% ungroup()

# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
r_map <- tm_shape(r_map_df)+
  tm_fill(col = "mean", midpoint =NA, palette = "-RdYlGn") +
  tm_borders() +
  tm_text("NAME_1", size = 0.8, col = "snow4")

r_map
