### MCDA analysis 

library(MCDA)

all_cluster <- read.csv("results/urban_cluster/ranking_df.csv") 
head(all_cluster)
fever_cluster <- read.csv("results/urban_cluster/fever/fever.csv")
head(fever_cluster)

all_cluster <- left_join(all_cluster, fever_cluster, by=c("x", "y"))
head(all_cluster)

#lets visualize the data 

itn_rich <- all_cluster %>% dplyr::select(id.x, mean_hh_itn, mean_rich, mean_pfpr, mean_fever, mean_ACT, x, y) %>%
                mutate(pfpr_cat = ifelse(mean_pfpr < 0.05, "<5%", ifelse(mean_pfpr >=0.05 & mean_pfpr <=0.15, "5-15%",
                              ifelse(mean_pfpr >0.15, ">15%", NA)))) 
              
head(itn_rich)

g <- ggplot(data = itn_rich, mapping = aes(x =mean_rich ,y = mean_ACT)) + 
  geom_point(color = "red")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlab("Proportion of individuals in the richest wealth quintile")+
  ylab("ACT")
  

ggsave("results/urban_cluster/scatterrichvs.png")
# matrix.please<-function(x) {
#   m<-as.matrix(x[,-1])
#   rownames(m)<-x[,1]
#   m
# }


itn_rich_3 <- all_cluster %>% dplyr::select(id.x, mean_hh_itn, mean_rich, mean_pfpr, mean_fever, mean_ACT, x, y) %>%
  mutate(`Risk level`= ifelse(mean_rich >= 0.75 & mean_fever <=0.10 , "Low risk", 
                       ifelse(is.na(mean_rich)|is.na(mean_fever), NA, "High risk"))) %>% drop_na(`Risk level`)


coordinates(itn_rich_3) <- c("x","y")

proj4string(itn_rich_3) <- CRS('+proj=longlat +datum=WGS84 +no_defs')

itn_rich_3 <- st_as_sf(itn_rich_3)
head(itn_rich_3) 
table(itn_rich_3$`Risk level`)

summary(is.na(itn_rich_3$mean_rich))



risk_map <-tm_shape(admin1_sf) + #this is the health district shapfile with DS estimates info
  tm_borders()+tm_shape(itn_rich_3)+ #this is the points shape file with LLIN and number of kids info by cluster 
  tm_dots(size = 0.5, col = "Risk level", palette = "seq")+
  tm_layout(aes.palette = list(seq ="RdYlBu"))+
  tm_legend(legend.title.size = 0.8, legend.just="top")+ 
  tm_shape(admin1_sf) +
  tm_borders()+
  tm_text('ADM1_NAME')
  # tm_basemap(server="OpenStreetMap",alpha=0.5)

tmap_mode("plot")

tmap_save(tm = risk_map, filename = "results/urban_cluster/maps/seventyfive10richfever.pdf", width=13, height=13,  useDingbats=FALSE)
