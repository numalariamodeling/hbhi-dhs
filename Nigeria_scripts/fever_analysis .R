#fever analysis
NGAfiles[[20]]
match("data/NG_2018_DHS_11072019_1720_86355/NGKR7ADT/NGKR7AFL.DTA",names(NGAfiles))
look_for(NGAfiles[[20]], "fever")

table(NGAfiles[[17]]$b19)

# creating list of files with KR data for  2010, 2013, 2015, 2017/2018

fever_list <- list(NGAfiles[[11]], NGAfiles[[14]], NGAfiles[[17]], NGAfiles[[20]])


cols_list = list(quo(h22)) #makes list of columns

fever_list <- map2(fever_list, cols_list, recoder.gen) #recodes variables 

# key list 
keys.fever  <- list(key_list[[4]], key_list[[5]],key_list[[6]], key_list[[7]]) #key list 


# key datasets and dhs/mis datasets are joined  
fever_list<- map2(fever_list , keys.fever, left_join) #PR datasets

#subsetting items in list 

fever_list <- map(fever_list, ~filter(.x, b5 == 1))

# table(fever_list[[1]]$b5)

cols_1 = list(quo(v005)) #creates list of cols that are recycled 
col_2 = list(quo(v022))
col_3 = list(quo(v021))

fever_list <- pmap(list(fever_list, cols_1, col_2,col_3), mutate_f) # more recoding of variables 

fever_svydesign <- map(fever_list, svydesign.fun) #list of survey designs 

var_list <- list("h22_new") # makes list of column names 

var1_list <- list("v001")

year <- list("v007")


#cluster-level estimate
fever_result <- pmap(list(var_list, var1_list, fever_svydesign, fever_list, year), result.clu.fun)

fever_result

#merging point shapefiles to LGA shapefiles - 2010, 2013, 2015, 2017/2018

NGAshp <- list(NGAshplist_sf[[4]], NGAshplist_sf[[5]], NGAshplist_sf[[6]], NGAshplist_sf[[7]])

pts_shapes <- map2(NGAshp, fever_result, left_join)

pts_filtered <- map(pts_shapes, ~filter(.x, URBAN_RURA =="U"))
table(pts_filtered[[2]]$URBAN_RURA)

head(pts_filtered[[1]])
summary(is.na(pts_filtered[[1]]$h22_new))
summary(pts_filtered[[1]]$h22_new)
sd(pts_filtered[[1]]$`Number of Participants`)



#making the maps 
# 
# admin1_sf <- list(admin1_sf)
# num_col <- list("Number of Participants")
# 
# hh_col <- list("h22_new")
# title <- list("2010 cluster feverest", "2013 cluster feverest", "2015 cluster feverest", "2018 cluster feverest")
# 
# 
# map.list <- pmap(list(admin1_sf, pts_filtered, num_col, hh_col, title), tmap.clu4)
# 
# map.list[[1]]


# all_map <- map(map.list, tmap_arrange)

u_clu_map <- tmap.clu2(admin1_sf, ptsfile=pts_filtered[[4]], "Number of Participants", "h22_new", "2018 cluster")

u_clu_map_15 <- tmap.clu2(admin1_sf, ptsfile=pts_filtered[[3]], "Number of Participants", "h22_new", "2015 cluster")

u_clu_map_13 <- tmap.clu2(admin1_sf, ptsfile=pts_filtered[[2]], "Number of Participants", "h22_new", "2013 cluster")

u_clu_map_10 <- tmap.clu2(admin1_sf, ptsfile=pts_filtered[[1]], "Number of Participants", "h22_new", "2010 cluster")


cluster.itn <- tmap_arrange(u_clu_map_10,u_clu_map_13,u_clu_map_15,u_clu_map)

# # tmap_save(tm = u_clu_map , filename = "results/urban_cluster/ITN/fever_2018.pdf", width=13, height=13, units ="in", asp=0,
#           paper ="A4r", useDingbats=FALSE)
# 



# creating a combined prevalence map for all years 
#row binding cluster points 
pts_merge <- rbind(pts_filtered[[1]], pts_filtered[[2]], pts_filtered[[3]], pts_filtered[[4]])

head(pts_merge)
summary(is.na(pts_merge$h22_new))
summary(pts_merge$h22_new)
summary(pts_merge$`Number of Participants`)
sd(pts_merge$`Number of Participants`)



#aggregated plot on state shape file 
u_clu_map_bindrows <- tmap.clu2(admin1_sf, ptsfile=pts_merge, "Number of Participants", "h22_new", "2010 - 2018 fever clusters")



#creates aggreagted map of pfpr for all years using mean values 

#calculate mean of points to pfpr estimate over time 
pts_merge_test <- pts_merge %>% drop_na(h22_new)

pts100 <- st_is_within_distance(pts_merge_test, dist = 2000)
class(pts100)


res <- data.frame(id=1:length(pts100),x=NA, y=NA,mean_fever=NA)
res$x <- sapply(pts100, function(p){mean(pts_merge_test$LONGNUM[p])})
res$y <- sapply(pts100, function(p){mean(pts_merge_test$LATNUM[p])})
res$mean_fever<- sapply(pts100, function(p) #scaled by the proportion of participants in each nearby cluster 
{sum((pts_merge_test$`Number of Participants`[p]/sum(pts_merge_test$`Number of Participants`[p]) * pts_merge_test$h22_new[p]))})
res$`Number of Participants` <- sapply(pts100, function(p){mean(pts_merge_test$`Number of Participants`[p])}) 
res_2 <- res %>% distinct(x, y, .keep_all = TRUE)

write.csv(res_2, "results/urban_cluster/fever/fever.csv")

coordinates(res_2) <- c("x","y")

proj4string(res_2) <- CRS('+proj=longlat +datum=WGS84 +no_defs')



res_sf_fever <- st_as_sf(res_2)
head(res_sf_fever)


summary(is.na(res_2_fever$mean_fever))
summary(res_2_fever$mean_fever)
summary(res_2_fever$`Number of Participants`)
sd(res_2_fever$`Number of Participants`)


u_clu_map_aggregated <- tmap.clu2(admin1_sf, ptsfile=res_sf_fever, "Number of Participants", "mean_fever", "Mean fever 2010 - 2018 in Nigeria")

all_maps <- tmap_arrange(u_clu_map_10,u_clu_map_13,u_clu_map_15,u_clu_map,u_clu_map_bindrows,u_clu_map_aggregated)

tmap_save(tm = all_maps, filename = "results/urban_cluster/fever/fever_diff.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


together <- as_Spatial(res_sf_ACT)




