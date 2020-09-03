# This script generates CM estimates from 4 DHS surveys at the LGA level, State, representative DHS level and at the geopolitical level 

dir.create(file.path(ResultDir, "case_management"), showWarnings = TRUE)
print_path <- file.path(ResultDir, "case_management")


if (Variable == "CM"){
  cm.ls <-read.files( ".*NGKR.*\\.DTA", DataDir, read_dta)
  cm.ls <- cm.ls[-c(1, 2)]
  cm.ls[[3]][,"ml13e"] <-recoder(cm.ls[[3]][,"ml13e"]) 
  table(cm.ls[[3]]$ml13e) #clean up to create better recoder function 
  cm.ls[[2]][,"ml13e"] <-recoder(cm.ls[[2]][,"ml13e"]) 
  NGAshplist<- NGAshplist[-c(1, 2)]  # key datasets and dhs/mis datasets are joined  
  NGA_ID <- lapply(NGAshplist, "[[", "DHSCLUST")#QA this 
  key_list <- key_list[-c(1, 2)]
  key_list <- Map(cbind, key_list, v001 = NGA_ID)
  comboACT.list <- map2(cm.ls, key_list, left_join)
  comboACT.list <- lapply(comboACT.list, subset, b5 == 1 & h22 == 1)             
  rep_DS.ls <- list(rep_DS) # attach representative DS 
  
  if (grepl("LGA|State|repDS|region", subVariable)) {
  comboACT.list  <- map2(comboACT.list, rep_DS.ls, left_join) #PR datasets
  LGA_sf <- LGA_clean_names%>%  as_tibble() %>% dplyr::select(LGA, State)
  repDS_LGA<- rep_DS %>% left_join(LGA_sf)
  
    if(subVariable == "LGA"){
      print("computing raw case management coverage estimates at the LGA-level for years 2008, 2010, 2013, 2015 & 2018")
      var <- list("LGA")
      ACT_LGA <- map2(comboACT.list, var, generate.ACT.state_LGA_repDS)
      fin_df_LGA <- plyr::ldply(ACT_LGA, rbind)
      
   }else if(subVariable == "State"){
      print("computing raw case management coverage estimates at the state-level for years 2008, 2010, 2013, 2015 & 2018")
      var <- list("State")
      ACT_State <- map2(comboACT.list,var, generate.ACT.state_LGA_repDS)
      fin_df_state <- plyr::ldply(ACT_State, rbind)
      state_sum <- fin_df_state %>%  distinct(State, comboACT, se, year)
      state_ls <- list(CM_State_DHS_2008_10 = state_sum, CM_State_with_LGA_DHS_2008_10 = fin_df_state)
      
    }else if(subVariable == "repDS"){
      print("computing raw case management coverage estimates at the representative DS-level for years 2008, 2010, 2013, 2015 & 2018")
      var <- list("repDS")
      ACT_rep <- map2(comboACT.list, var, generate.ACT.state_LGA_repDS)
      fin_df <- plyr::ldply(ACT_rep, rbind)
      
    }else if (grepl("region", subVariable)) {
      print("computing raw case management coverage estimates at the geopolitical-level for years 2008, 2010, 2013, 2015 & 2018")
      region_state<- data.frame(comboACT.list[[4]]$State, comboACT.list[[4]]$v024) %>%  distinct() %>% drop_na()
      colnames(region_state)<- c("State", "v024")
      repDS_region<- repDS_LGA %>% left_join(region_state)
      var <- list("v024")
      ACT_region <- map(comboACT.list,ACT.fun_region)
      fin_df_region <- plyr::ldply(ACT_region, rbind)
    }else {
      print("no subVariable to analyze")
    }
  }
} else {
  print("no variable to analyze")
}





if (smoothing == TRUE & subVariable == "LGA") {
  #smoothing 
  # adding a row number to the shape2 object, will be handy for plotting later on
  # smoothing, printing, plotting, mapping, saving 
  library(INLA); library(spdep)
  LGAshp@data$row_num<-1:nrow(LGAshp@data)
  key<-LGAshp@data[,c("State","LGA","row_num")]
  key_list <- list(key)
  
  ACT_logit <- lapply(ACT_LGA, generate_logit)
  
  ACT_logit<-map2(ACT_logit, key_list,left_join) # merge in key and put data in right order
  
  # setting up the outcome #
  ACT_logit <- lapply(ACT_logit, function(x) cbind(x, outcome = x[["logit_ACT"]], prec =1/ x[["var_logit_ACT"]]))
  
  # this takes the shape file and figures out which areas are neighbors
  nb.r <- poly2nb(LGAshp, queen=F)
  mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix
  
  # to double check that they are finding the neighbors, I like to plot a 
  # random LGA in blue and then the neighbors in red.
  row = 500
  indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
  indx
  
  # Plot the LGA and highlight the neighbors
  plot(LGAshp)
  plot(LGAshp[row,], col='blue', add=T)
  plot(LGAshp[indx,], col='red', add=T)
  # it works!
  
  
  #setting up the spatial model priors
  a<-1
  b<-5e-05
  if (smoothing_type == "space"){
    print("generating case management estimates for spatial model for years 2008, 2010, 2013, 2015 & 2018")
    # model formulation #
    smoothing.model.2 <- outcome ~ f(row_num, model="bym",graph=mat, param=c(a,b)) 
    df <- lapply(ACT_logit, generate_smooth_values)
    #creating data frame of all estimates for model 2
    
    values <- lapply(df, function(x){cbind(x[[49]]$.parent.frame$mod2$summary.fitted.values, year=x[[101]])})
    
    values <- lapply(values, function(x){cbind (x, saep.est= expit(x[["0.5quant"]]), saep.up= expit(x[["0.975quant"]]), saep.low= expit(x[["0.025quant"]]))})
    
    ACT_logit <- lapply(ACT_logit, function(x) x[!(names(x) %in% "year")])
    
    fin_df <- Map(cbind, ACT_logit, values)
    
    # combine list 
    fin_df_smooth_LGA <- plyr::ldply(fin_df, rbind)
    
  }else if (smoothing_type == "space-time"){
    print("generating case management estimates for space-time model for years 2008, 2010, 2013, 2015 & 2018")
    ACT_logit_combined <- plyr::ldply(ACT_logit, rbind)
    
    prec.prior <- list(prec = list(param = c(a, b)))
    
    ACT_space_time_mod <- inla(outcome ~ 1 + f(year, model = "rw1",
                                               hyper = prec.prior) + 
                                 f(as.numeric(row_num), model = "besag", graph = mat,
                                   hyper = prec.prior),
                               data =ACT_logit_combined, family = "gaussian",
                               control.predictor = list(compute = TRUE),
                               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
                               control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
                               scale=prec)
    
    
    df <- ACT_space_time_mod$summary.fitted.values
    df$saep.est <- expit(df$`0.5quant`)
    ACT_raw <- fin_df_LGA %>%  dplyr::select(LGA, repDS, State,year, comboACT)
    df_space_time <- cbind(ACT_raw, df) 
  }else {
    print("case management estimates will not be smoothed. Indicate type")
  }
}else {
  print("raw survey-adjusted case management estimates have been generated")
}



#plots

if (plot == TRUE & subVariable == "LGA" & smoothing == FALSE){
  LGA_plot<- ggplot(df_space_time, aes(x = year, y = comboACT, group = LGA)) + 
    geom_line(color = "blue") +
    xlab('Time') +
    ylab('ACT coverage by LGA')+
    labs(caption = "No smoothing, 92 missing values")
  
}else if (plot == TRUE & subVariable == "LGA" & smoothing == TRUE & smoothing_type == "space"){
  LGA_smooth_space<- ggplot(fin_df_smooth_LGA, aes(x = year, y =  saep.est, group = LGA)) + 
    # geom_point() +
    geom_line(color = "blue")+
    xlab('Time') +
    ylab('ACT coverage by LGA')+ 
    labs(caption = "smoothed across space")
  
}else if (plot == TRUE & subVariable == "LGA" & smoothing == TRUE & smoothing_type == "space-time"){
  LGA_smooth_space_time<- ggplot(df_space_time, aes(x = year, y = saep.est , group = LGA)) + 
    # geom_point() +
    geom_line(color = "blue")+
    xlab('Time') +
    ylab('ACT coverage by LGA')+
    labs(caption = "smoothed across space and time")
  
}else if (plot == TRUE & subVariable == "State"){
  State_plot<- ggplot(state_sum, aes(x = year, y = comboACT, group = State)) + 
    geom_line(color = "blue") +
    xlab('Time') +
    ylab('ACT coverage by State')
  
}else if (plot == TRUE & subVariable == "repDS"){
  rep_DS_plot<- ggplot(fin_df, aes(x = year, y = comboACT, group = LGA)) + 
    geom_line(color = "blue") +
    xlab('Time') +
    ylab('ACT coverage averaged by repDS')
  
}else if (plot == TRUE & subVariable == "region"){
  region_plot<- ggplot(fin_df_region, aes(x = year, y = comboACT, group = LGA)) + 
    geom_line(color = "blue") +
    xlab('Time') +
    ylab('ACT coverage averaged by geopolitical zones')
  
}else {
  print("values will not be plotted")
}





# map 

#state 
if (plot == TRUE & subVariable == "State"){
  state_sum <- state_sum %>%  mutate(State = case_when(grepl("Akwa", State) ~"Akwa Ibom", TRUE ~ State))
  state_split <- split(state_sum, state_sum$year)
  state_sf <- state_sf %>% 
    mutate(State = case_when(State == "Nassarawa" ~ "Nasarawa", 
                             grepl("Akwa", State) ~ "Akwa Ibom", 
                             TRUE ~ State))
  state_map_ls <- list(state_sf)
  join_state <- Map(function(x, y) left_join(x, y, by = "State"), state_map_ls, state_split)
  
  map_val <- list("comboACT")
  var<-list("ACT averaged by State")
  maps <- pmap(list(join_state, map_val, var), map_fun)
  arrange_maps <- do.call(tmap_arrange, maps)
  
}else if {
  
}



GPZ_no <- c(1, 2, 3, 4, 5, 6)
GPZ_value <- c("North Central", "North East", "North West", "South East", "South South", "South West")


df_2 <- df %>% mutate(GPZ = case_when(v024 == 1 ~ "North Central",
                                      v024== 2 ~"North East", 
                                      v024== 3 ~"North West", 
                                      v024== 4 ~"South East", 
                                      v024== 5 ~"South South", 
                                      v024== 6 ~"South West", 
                                      TRUE ~ GPZ))
 

df_split  <- split(df_2, df$year) 

LGA_list <- list(LGAshp)


join <- Map(function(x, y) left_join(x, y, by = "LGA"), LGA_list, df_split)


map_val <- list("GPZ")
var<-list("ACT averaged by GPZ")
maps <- pmap(list(join, map_val, var), map_fun)
arrange_maps <- do.call(tmap_arrange, maps)

tmap_save(tm =arrange_maps, filename = file.path(ProjectDir, cm_hist_sim_path, "/ACT by geopolitical zones.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)




#maps for the naive, smoothed across space, and space and time values 

LGA_list <- list(LGA_clean_names)

raw_df <- split(df_space_time, df_space_time$year)

join <- Map(function(x, y) left_join(x, y, by = "LGA"), LGA_list, raw_df)

map_val <- list("comboACT")
var<-list("ACT averaged by LGA (Naive Estimates")
maps <- pmap(list(join, map_val, var), map_fun)
arrange_maps_raw <- do.call(tmap_arrange, maps)

#smooth map space 
smooth_df <- split(fin_df_smooth_LGA, fin_df_smooth_LGA$year)

join <- Map(function(x, y) left_join(x, y, by = "LGA"), LGA_list, smooth_df)

map_val <- list("saep.est")
var<-list("ACT averaged by LGA (Smoothed across space")
maps <- pmap(list(join, map_val, var), map_fun)
arrange_maps_smooth <- do.call(tmap_arrange, maps)


#smooth map space and time 
smooth_time_df <- split(df_space_time, df_space_time$year)

join <- Map(function(x, y) left_join(x, y, by = "LGA"), LGA_list, smooth_time_df)

map_val <- list("saep.est")
var<-list("ACT averaged by LGA (Smoothed across space and time")
maps <- pmap(list(join, map_val, var), map_fun)
arrange_maps_smooth_time <- do.call(tmap_arrange, maps)


tmap_save(tm =arrange_maps_smooth_time, filename = file.path(ProjectDir, cm_hist_sim_path, "/ACT by LGA_smoothed_space_time.pdf"), width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


# create final dataset 


cm_sim_file <- read.csv(file.path(ProjectDir,cm_hist_sim_path, "HS_by_LGA_v4.csv" )) %>% dplyr::select(LGA, simday, duration, severe_cases,year)
head(cm_sim_file)

df <- df_space_time %>%  filter(year != 2008)
head(df)

df_clean <- df %>% mutate(LGA = str_replace_all(LGA, "/", "-"))

df <- df_clean %>% left_join(cm_sim_file, by=c("LGA", "year")) %>%  
  dplyr::select(LGA, repDS, State, year, comboACT_raw=comboACT, comboACT = saep.est, simday, duration, severe_cases)
head(df)

write.csv(df,file.path(ProjectDir,cm_hist_sim_path, "HS_by_LGA_v5.csv"))


if (SAVE == TRUE & subVariable == "LGA") {
  write_csv()
  
}else if (SAVE == TRUE & subVariable == "State"){
  mapply(write_csv, state_ls, path=paste0(print_path,"/",names(state_ls), '.csv'))
  pdf(file=paste0(print_path, "/", "CM_state_plots_DHS_2008_2018.pdf"))
  plot(State_plot)
  dev.off()
  print("job saved in case management directory within results directory")
  
}else {
  print("job was not saved.check file paths are correct")
}

