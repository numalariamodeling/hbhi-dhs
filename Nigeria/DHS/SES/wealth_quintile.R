# wealth quintile 

look_for(NGAfiles[[21]], "year")

table(NGAfiles[[21]]$hv007)

# creating list of files with PR data for  2010, 2013, 2015, 2017/2018

wealth_list <- list(NGAfiles[[12]], NGAfiles[[15]], NGAfiles[[18]], NGAfiles[[21]])

cols_list = list(quo(hv270)) #makes list of columns

wealth_list <- map2(wealth_list, cols_list, recoder.wealth) #recodes variables 


wealth_list <- map(wealth_list, survey.month.fun) #computes survey month

keys.wealth  <- list(key_list[[4]], key_list[[5]],key_list[[6]], key_list[[7]]) #key list 

# key datasets and dhs/mis datasets are joined  
wealth_list<- map2(wealth_list , keys.wealth, left_join) #PR datasets

  
cols_1 = list(quo(hv005)) #creates list of cols that are recycled 
col_2 = list(quo(hv022))
col_3 = list(quo(hv021))

wealth_list <- pmap(list(wealth_list, cols_1, col_2,col_3), mutate_f) # more recoding of variables 

wealth_svydesign <- map(wealth_list, svydesign.fun) #list of survey designs 

var_list <- list("hv270_new") # makes list of column names 

var1_list <- list("LGA")

year <- list("hv007")

#LGA-level estimate
wealth_result <- pmap(list(var_list, var1_list, wealth_svydesign, wealth_list, year), result.fun)

wealth_2018 <- wealth_result[[4]]

LGA_plot <- left_join(LGAshp_sf,wealth_2018)
summary(LGA_plot$hv270_new)

LGA_wealth<- tm_shape(LGA_plot) + #this is the health district shapfile with LLIn info
  tm_polygons(col = "hv270_new", textNA = "No data", 
              title = "Proportion in the richest quintile", palette = "seq", breaks=c(0, 0.1, 
                                                                                            0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 1.0))+
  tm_layout(title = "", aes.palette = list(seq="RdYlBu"))

tmap_save(tm = LGA_wealth, filename = "results/malaria_DHS_paper/nightlights/LGA_wealth_aggregate.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


#how to change column names in a list of data frames 

colnames <- c("Cluster_id", "Proportion of individuals in the rich wealth quintile", "se", "Number of Participants", "Year")

wealth_result_2 <- lapply(wealth_result, setNames, colnames) #changes column names 


#merging point shapefiles to LGA shapefiles - 2010, 2013, 2015, 2017/2018

NGAshp <- list(NGAshplist_sf[[4]], NGAshplist_sf[[5]], NGAshplist_sf[[6]], NGAshplist_sf[[7]])

pts_shapes <- map2(NGAshp, wealth_result, left_join)

pts_filtered <- map(pts_shapes, ~filter(.x, URBAN_RURA =="U"))
table(pts_filtered[[2]]$URBAN_RURA)

head(pts_filtered[[1]])
summary(is.na(pts_filtered[[1]]$hv270_new))
summary(pts_filtered[[1]]$hv270_new)
sd(pts_filtered[[1]]$`Number of Participants`)



#making the maps 
# 
# admin1_sf <- list(admin1_sf)
# num_col <- list("Number of Participants")
# 
# hh_col <- list("hv270_new")
# title <- list("2010 cluster richest", "2013 cluster richest", "2015 cluster richest", "2018 cluster richest")
# 
# 
# map.list <- pmap(list(admin1_sf, pts_filtered, num_col, hh_col, title), tmap.clu4)
# 
# map.list[[1]]


# all_map <- map(map.list, tmap_arrange)

u_clu_map <- tmap.clu4(admin1_sf, ptsfile=pts_filtered[[4]], "Number of Participants", "hv270_new", "2018 cluster")

u_clu_map_15 <- tmap.clu4(admin1_sf, ptsfile=pts_filtered[[3]], "Number of Participants", "hv270_new", "2015 cluster")

u_clu_map_13 <- tmap.clu4(admin1_sf, ptsfile=pts_filtered[[2]], "Number of Participants", "hv270_new", "2013 cluster")

u_clu_map_10 <- tmap.clu4(admin1_sf, ptsfile=pts_filtered[[1]], "Number of Participants", "hv270_new", "2010 cluster")


cluster.itn <- tmap_arrange(u_clu_map_10,u_clu_map_13,u_clu_map_15,u_clu_map)

# # tmap_save(tm = u_clu_map , filename = "results/urban_cluster/ITN/wealth_2018.pdf", width=13, height=13, units ="in", asp=0,
#           paper ="A4r", useDingbats=FALSE)
# 



# creating a combined prevalence map for all years 
#row binding cluster points 
pts_merge <- rbind(pts_filtered[[1]], pts_filtered[[2]], pts_filtered[[3]], pts_filtered[[4]])

head(pts_merge)
summary(is.na(pts_merge$hv270_new))
summary(pts_merge$hv270_new)
summary(pts_merge$`Number of Participants`)
sd(pts_merge$`Number of Participants`)



#aggregated plot on state shape file 
u_clu_map_bindrows <- tmap.clu4(admin1_sf, ptsfile=pts_merge, "Number of Participants", "hv270_new", "2010 - 2018 proportion of richest cluster")



#creates aggreagted map of pfpr for all years using mean values 

#calculate mean of points to pfpr estimate over time 
pts_merge_test <- pts_merge %>% drop_na(hv270_new)

pts100 <- st_is_within_distance(pts_merge_test, dist = 2000)
class(pts100)


res <- data.frame(id=1:length(pts100),x=NA, y=NA,mean_rich=NA)
res$x <- sapply(pts100, function(p){mean(pts_merge_test$LONGNUM[p])})
res$y <- sapply(pts100, function(p){mean(pts_merge_test$LATNUM[p])})
res$mean_rich<- sapply(pts100, function(p) #scaled by the proportion of participants in each nearby cluster 
{sum((pts_merge_test$`Number of Participants`[p]/sum(pts_merge_test$`Number of Participants`[p]) * pts_merge_test$hv270_new[p]))})
res$`Number of Participants` <- sapply(pts100, function(p){mean(pts_merge_test$`Number of Participants`[p])}) 
res_2_wealth <- res %>% distinct(x, y, .keep_all = TRUE)

coordinates(res_2) <- c("x","y")

proj4string(res_2) <- CRS('+proj=longlat +datum=WGS84 +no_defs')



res_sf_wealth <- st_as_sf(res_2)
head(res_sf_wealth)


summary(is.na(res_sf$mean_rich))
summary(res_sf$mean_rich)
summary(res_sf$`Number of Participants`)
sd(res_sf$`Number of Participants`)


u_clu_map_aggregated <- tmap.clu4(a, ptsfile=res_sf, "Number of Participants", "mean_rich", "Aggregated Rich 2010 - 2018 in Nigeria")

all_maps <- tmap_arrange(u_clu_map_10,u_clu_map_13,u_clu_map_15,u_clu_map,u_clu_map_bindrows,u_clu_map_aggregated)

tmap_save(tm = all_maps, filename = "results/urban_cluster/SES/SES_diff.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


together <- as_Spatial(res_sf_ACT)



