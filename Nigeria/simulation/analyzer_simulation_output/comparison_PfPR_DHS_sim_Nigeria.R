#################################################################################################################################################
# comparison_PfPR_DHS_sim.R
# HBHI - Nigeria written by Monique Ambrose and repurposed by Ifeoma Ozodiegwu for Nigeria 
# contact: Ifeoma Ozodiegwu 
# December 2020

# currently, we claim that our calibration and parameterization was successful because we can see that simulated DS trajectories often visually 
#    agree fairly well with survey/surveillance data. To look at this a bit more closely and to see how DS-specific this match is, create a 
#    series of plots that compare simulation and data, either for the same (matched) DS or for mismatched DS.
# main types of comparisons are:
#    1) comparison of PfPR seasonality (plot the DHS PfPR according to month of survey, along with corresponding simulation values)
#    2) scatter plot comparisions of PfPR between observed DHS and corresponding simulation values
#    3) change between survey years (percent or absolute change within a DS or admin1 area in the aggregated PfPR in different survey years)
#    4) histograms of PfPR values, differences, and likelihoods

#################################################################################################################################################


###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                                setup
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################
rm(list = ls())
# library(ggplot2)
# library(ggpubr)
# library(gridExtra)
# library(data.table)
# library(RColorBrewer)
# library(tidyr)
# library(tibble)
# library(dplyr)
# library(reshape2)
# library(hablar)
# library(stringr)
# library(lubridate)

list.of.packages <- c("tidyverse", "ggplot2", "purrr",  "stringr", "sp", "rgdal", "raster", "hablar", 
                      "lubridate", "RColorBrewer", "ggpubr", "gridExtra", "data.table",  "nngeo", "reshape2")
lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages



###################################################################
#   read in and format data and simulation output
###################################################################


box_filepath = "C:/Users/ido0493/Box/NU-malaria-team/projects"
box_hbhi_filepath = paste0(box_filepath, '/hbhi_nigeria')
SrsDir = file.path(box_hbhi_filepath, 'DS DHS estimates', 'U5', 'pfpr', 'SRS') 
SvyDir = file.path(box_hbhi_filepath, 'DS DHS estimates', 'U5', 'pfpr', 'DHS.data_surveyd')
ifelse(!dir.exists(file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/validation/PfPR")), 
       dir.create(file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/validation/PfPR")), FALSE)
print_path <- file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/validation/PfPR")


# - - - - - - - - - - - - - - - - #
# DHS PfPR data
# - - - - - - - - - - - - - - - - #

#get svy adjusted estimates 
files <- list.files(path = SvyDir, pattern = "*_micro.csv", full.names = TRUE)
df <- sapply(files, read_csv, simplify = F)
colnames(df[[1]])[4:6] = c('p_test_mean',  'p_test_std.error', 'num_U5_sampled')
colnames(df[[2]])[4:6] = c('p_test_mean',  'p_test_std.error', 'num_U5_sampled')
colnames(df[[3]])[4:6] = c('p_test_mean',  'p_test_std.error', 'num_U5_sampled')
# combine dhs from multiple years and get U5 sampled with LGA 
df_Svyd <- plyr::ldply(df, rbind) %>%  dplyr::select(LGA, time2, num_U5_sampled) %>% 
  mutate(LGA = case_when(LGA == "kaita" ~ "Kaita",
                         LGA == "kiyawa" ~ "Kiyawa",
                         TRUE ~ as.character(LGA)))


#get SRS estimates 
files <- list.files(path = SrsDir, pattern = "*_micro.csv", full.names = TRUE)
df2 <- sapply(files, read.csv, simplify = F)
colnames(df2[[1]])[4:7] = c('p_test_mean',  'p_test_sd', 'p_test_std.error', 'num_U5_sampled')
colnames(df2[[2]])[4:7] = c('p_test_mean',  'p_test_sd', 'p_test_std.error', 'num_U5_sampled')
colnames(df2[[3]])[4:7] = c('p_test_mean', 'p_test_sd', 'p_test_std.error', 'num_U5_sampled')
#combine survey results 
df_SRS <- plyr::ldply(df, rbind) %>%  dplyr::select(-num_U5_sampled) %>%  
  mutate(LGA = case_when(LGA == "kaita" ~ "Kaita",
                         LGA == "kiyawa" ~ "Kiyawa",
                         TRUE ~ LGA)) %>%  
  left_join(df_Svyd) 

dhs_pfpr <- df_SRS %>%  drop_na() %>%  mutate(month = str_split(time2, "-", simplify = TRUE)[,1],
                                                year = str_split(time2, "-", simplify = TRUE)[,2],
                                                date = make_date(year, month))



# - - - - - - - - - - - - - - - - #
# simulation output
# - - - - - - - - - - - - - - - - #
sim_filepath_2010 = paste0(box_hbhi_filepath, '/simulation_output/2010_to_2020_v10/NGA 2010-20 burnin_hs+itn+smc 1')
pfpr_case_all_2010 = fread(paste0(sim_filepath_2010, '/All_Age_monthly_Cases.csv'))
pfpr_case_all_2010[,date:=as.Date(date)]
pfpr_case_all_2010$year = lubridate::year(pfpr_case_all_2010$date)
pfpr_case_all_2010$month = lubridate::month(pfpr_case_all_2010$date)
pfpr_case_u5_2010 = fread(paste0(sim_filepath_2010, '/U5_PfPR_ClinicalIncidence_severeTreatment.csv'))
pfpr_case_u5_2010[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
# mean values across runs
pfpr_case_all_runMeans  <- pfpr_case_all_2010 %>% group_by(date, LGA) %>%  
  summarise_all(mean) %>% ungroup() 
pfpr_case_u5_runMeans  <- pfpr_case_u5_2010 %>% group_by(date, LGA) %>%  
  summarise_all(mean) %>% ungroup() 
#pfpr_case_u5_runMeans$LGA = toupper(pfpr_case_u5_runMeans$LGA)


# - - - - - - - - - - - - - - - - - #
# archetype and admin 1 for each DS
# - - - - - - - - - - - - - - - - - #
# create data tables specifying which DS belong to which archetype (and the rep DS for that archetype) and admin1
DS_arch_filename = file.path(box_hbhi_filepath, 'nigeria_LGA_pop.csv')

# add column with admin1 of each DS
admin1DS = fread(DS_arch_filename)
admin1DS = admin1DS[,c('LGA', 'State', 'Archetype')]
#admin1DS$LGA =toupper(admin1DS$LGA)
pfpr_matched = left_join(pfpr_case_u5_runMeans, admin1DS, by=c('LGA'))

# # add columns with archetype of each DS
# archDS = fread(DS_arch_filename)
# archDS = archDS[,c('LGA','Archetype')]
# # get seasonality archetype identity for each DS
# pfpr_case_u5_runMeans$Archetype = NA
# season_arch = unique(archDS$seasonality_archetype)
# for (aa in 1:length(season_arch)){
#   ds_in_arch = unique(archDS$LGA[archDS$seasonality_archetype == season_arch[aa]])
#   pfpr_case_u5_runMeans$Archetype[pfpr_case_u5_runMeans$LGA %in% ds_in_arch] = aa
# }
# # get seasonality-transmission archetype identity for each DS
# pfpr_case_u5_runMeans$archID = NA
# season2_arch = c(unique(archDS$seasonality_archetype_2[archDS$seasonality_archetype == season_arch[1]])[c(3,1,2)],
#                  unique(archDS$seasonality_archetype_2[archDS$seasonality_archetype == season_arch[2]]),
#                  unique(archDS$seasonality_archetype_2[archDS$seasonality_archetype == season_arch[3]])[c(3,1,2)])
# for (aa in 1:length(season2_arch)){
#   ds_in_arch = unique(archDS$LGA[archDS$seasonality_archetype_2 == season2_arch[aa]])
#   pfpr_case_u5_runMeans$archID[pfpr_case_u5_runMeans$LGA %in% ds_in_arch] = aa
# }
# 

# need to correct name issue (kiyawa and kaita)


###################################################################
# merge DHS and corresponding simulation values
###################################################################

# PfPR: match date from sim and data but shuffle DS
pfpr_matched_2 = pfpr_matched
LGAs = unique(pfpr_matched_2$LGA)
LGAs_shuffled = LGAs[c(2:length(LGAs), 1)]
pfpr_case_u5_runMeans_mis = pfpr_matched_2 
pfpr_case_u5_runMeans_mis$LGA = sapply(pfpr_case_u5_runMeans_mis$LGA, function(x) LGAs_shuffled[which(LGAs == x)])
pfpr_mismatched = left_join(dhs_pfpr, pfpr_case_u5_runMeans_mis, by=c('LGA','date'))



# match the same DS from simulation and data
dhs_pfpr <- dhs_pfpr %>%  mutate(LGA =gsub("/", "-", .$LGA))
pfpr_matched = left_join(dhs_pfpr, pfpr_matched, by=c('LGA','date'))


# relative and absolute differences - matched
pfpr_matched$error = (pfpr_matched$`PfPR U5` - pfpr_matched$p_test_mean) * -1
pfpr_matched$abs_error = abs(pfpr_matched$`PfPR U5` - pfpr_matched$p_test_mean)
pfpr_matched$rel_error = abs(pfpr_matched$`PfPR U5` - sapply(pfpr_matched$p_test_mean, max,0.0001))/sapply(pfpr_matched$p_test_mean, max,0.0001)
# relative and absolute differences -  mismatched
pfpr_mismatched$error = (pfpr_mismatched$`PfPR U5` - pfpr_mismatched$p_test_mean) * -1
pfpr_mismatched$abs_error = abs(pfpr_mismatched$`PfPR U5` - pfpr_mismatched$p_test_mean)
pfpr_mismatched$rel_error = abs(pfpr_mismatched$`PfPR U5` - sapply(pfpr_mismatched$p_test_mean, max,0.0001))/sapply(pfpr_mismatched$p_test_mean, max,0.0001)




#######################################################################
# calculate aggregated PfPR (to year and to admin1 level)
#######################################################################
# get annual averages for each DS
# matched DS
pfpr_matched$prod_dhs_pfpr_ss = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
pfpr_matched$prod_sim_pfpr_ss = pfpr_matched$`PfPR U5` * pfpr_matched$num_U5_sampled
pfpr_matched_annual = pfpr_matched[,c('LGA','year.y', 'prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss', 'Archetype')]  %>% group_by(year.y, LGA, Archetype) %>%  
  summarise_all(sum) %>% ungroup()
pfpr_annual_split = split(pfpr_matched_annual, pfpr_matched_annual$year.y)
pfpr_matched_annual = plyr::ldply(pfpr_annual_split, rbind)
pfpr_matched_annual$pfpr_dhs_mean = pfpr_matched_annual$prod_dhs_pfpr_ss/pfpr_matched_annual$num_U5_sampled
pfpr_matched_annual$pfpr_sim_mean = pfpr_matched_annual$prod_sim_pfpr_ss/pfpr_matched_annual$num_U5_sampled
# mismatched DS
pfpr_mismatched$prod_dhs_pfpr_ss = pfpr_mismatched$p_test_mean * pfpr_mismatched$num_U5_sampled
pfpr_mismatched$prod_sim_pfpr_ss = pfpr_mismatched$`PfPR U5` * pfpr_mismatched$num_U5_sampled
pfpr_mismatched_annual = pfpr_mismatched[,c('LGA','year.y', 'prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss', 'Archetype')]  %>% group_by(year.y, LGA, Archetype) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_mismatched_annual$pfpr_dhs_mean = pfpr_mismatched_annual$prod_dhs_pfpr_ss/pfpr_mismatched_annual$num_U5_sampled
pfpr_mismatched_annual$pfpr_sim_mean = pfpr_mismatched_annual$prod_sim_pfpr_ss/pfpr_mismatched_annual$num_U5_sampled

# get annual average for each admin 1 (instead of admin2=health district) - because DHS not powered at admin2
pfpr_matched_annual_admin1 = pfpr_matched[,c('State','year.y', 'prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss')]  %>% group_by(year.y, State) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_annual_admin1$pfpr_dhs_mean = pfpr_matched_annual_admin1$prod_dhs_pfpr_ss/pfpr_matched_annual_admin1$num_U5_sampled
pfpr_matched_annual_admin1$pfpr_sim_mean = pfpr_matched_annual_admin1$prod_sim_pfpr_ss/pfpr_matched_annual_admin1$num_U5_sampled




###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                            plot comparisons
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################

##################################################################
# compare seasonality patterns (plots of PfPR by month)
##################################################################
# save the mean and median absolute and relative differences for each month across all years and DS
median_dif_pfpr_monthly = rep(NA,12)
mean_abs_dif_pfpr_monthly = rep(NA,12)
median_abs_dif_pfpr_monthly = rep(NA,12)
mean_rel_dif_pfpr_monthly = rep(NA,12)
median_rel_dif_pfpr_monthly = rep(NA,12)
median_pfpr_dhs_monthly = rep(NA, 12)
median_pfpr_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_dif_pfpr_monthly[mm] = median(pfpr_matched$error[pfpr_matched$month.x == mm], na.rm=TRUE)
  mean_abs_dif_pfpr_monthly[mm] = mean(pfpr_matched$abs_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_abs_dif_pfpr_monthly[mm] = median(pfpr_matched$abs_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  mean_rel_dif_pfpr_monthly[mm] = mean(pfpr_matched$rel_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_rel_dif_pfpr_monthly[mm] = median(pfpr_matched$rel_error[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_pfpr_dhs_monthly[mm] = median(pfpr_matched$p_test_mean[pfpr_matched$month.x == mm], na.rm=TRUE)
  median_pfpr_sim_monthly[mm] = median(pfpr_matched$`PfPR U5`[pfpr_matched$month.x == mm], na.rm=TRUE)
}

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# plot monthly values in DHS dataset and from corresponding times and locations in the simulation 
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##

pdf(file=paste0(print_path, "/", "monthly_PfPR_DHS_compared_to_monthly_simulation.pdf"))
par(mfrow=c(1,1), mar=c(5,4,4,2))
set.seed(1); jitter = runif(length(pfpr_matched$month.x), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,1.1), type='b', bty='L', ylab='U5 PfPR', xlab='month')
pfpr_matched$month.x <-as.numeric(pfpr_matched$month.x)
points(pfpr_matched$month.x+jitter, pfpr_matched$p_test_mean, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
points(pfpr_matched$month.x+jitter, pfpr_matched$`PfPR U5`, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
lines(1:12, median_pfpr_dhs_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_pfpr_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=0.99, c('DHS','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
text(1:12, rep(1.1, 12), round(median_abs_dif_pfpr_monthly,2), col='grey')
dev.off()


## - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# plot error in each month
## - - - - - - - - - - - - - - - - - - - - - - - - - - ##

pdf(file=paste0(print_path, "/", "monthly_PfPR_DHS_compared_to_monthly_simulation_with_error.pdf"))
par(mfrow=c(1,1), mar=c(5,4,4,2))
set.seed(1); jitter = runif(length(pfpr_matched$month.x), min=-0.15, max=0.15)
plot(1:12, median_abs_dif_pfpr_monthly, type='l', bty='L', ylab='U5 PfPR or difference in PfPR', xlab='month', ylim=c(0,1))
pfpr_matched$month.x <-as.numeric(pfpr_matched$month.x)
points(pfpr_matched$month.x+jitter, pfpr_matched$p_test_mean, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
points(pfpr_matched$month.x+jitter, pfpr_matched$`PfPR U5`, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
lines(1:12, median_pfpr_dhs_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_pfpr_sim_monthly, col=rgb(0.83,0,0.1))
lines(1:12, median_dif_pfpr_monthly, col='salmon')
legend(x=0.7,y=0.99, c('DHS','simulation', 'median difference', 'median absolute difference'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1), 'salmon', 'black'), bty='n')
dev.off()

# plot(1:12, median_rel_dif_pfpr_monthly, type='b', ylim=c(0,1), bty='L', ylab='median relative difference between DHS and simulation PfPR', xlab='month')

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# zoom in on october and plot with DS names, colored by archetype
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
october_pfpr_matched = pfpr_matched[which(pfpr_matched$month.x == 10),]
pdf(file=paste0(print_path, "/", "october_PfPR_DHS_compared_to_october_simulation.pdf"))
ggplot(october_pfpr_matched, aes(x = `PfPR U5`, y = p_test_mean,color = Archetype, label = LGA))+
  geom_point()+ 
  geom_abline(slope=1, intercept=c(0,0)) +
  geom_text(aes(label=LGA))+ 
  theme_classic()+ 
  xlab('simulation U5 PfPR')+ 
  ylab('DHS U5 PfPR')+ 
  ggtitle('Comparison of october simulation and DHS')+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
dev.off()







###################################################################
#   scatterplots of simulated and DHS PfPR for each DS
###################################################################

# set of scatterplots: four plots returned, all from the same scenario but from different years
#   include regression lines (lm weighted by survey size) and correlation values
create_PfPR_scatters = function(pfpr_df, x_col_name, x_lab, y_col_name, y_lab){
  # matched DS
  p_all = ggplot(pfpr_df, aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled, col=year.y), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  # look at each year separately
  p_2010 = ggplot(pfpr_df[pfpr_df$year.y==2010,], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  p_2015 = ggplot(pfpr_df[pfpr_df$year.y==2015,], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  p_2018_2018 = ggplot(pfpr_df[pfpr_df$year.y%in%c(2018,2018),], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(aes(size=num_U5_sampled), shape=20, alpha=0.5) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm, mapping = aes(weight = num_U5_sampled))+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  return(list(p_all, p_2010, p_2015, p_2018_2018))
}

create_PfPR_scatters_admin1 = function(pfpr_df, x_col_name, x_lab, y_col_name, y_lab){
  # matched DS
  p_all = ggplot(pfpr_df, aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(size=5, shape=20, color = "skyblue2") + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm)+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  # look at each year separately
  p_2010 = ggplot(pfpr_df[pfpr_df$year.y==2010,], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(size=5, shape=20, color = "skyblue2") + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm)+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  p_2015 = ggplot(pfpr_df[pfpr_df$year.y==2015,], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(size=5, shape=20,color = "skyblue2") + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm)+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  p_2018_2018 = ggplot(pfpr_df[pfpr_df$year.y%in%c(2018,2018),], aes_string(x=x_col_name, y=y_col_name)) +
    geom_point(size=5,shape=20,color = "skyblue2") + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm)+
    stat_cor(method = "pearson", col='darkred') +
    scale_size(limits = c(1,300), range = c(1, 8)) + 
    xlim(0, 1) + 
    ylim(0, 1) +
    xlab(x_lab) + 
    ylab(y_lab)+
    theme_classic() +
    theme(legend.position = "none")
  return(list(p_all, p_2010, p_2015, p_2018_2018))
}

# correlation confidence interval 

df_2010 <- pfpr_matched_annual_admin1
cor.test(df_2010$pfpr_sim_mean, df_2010$pfpr_dhs_mean, method = "pearson", conf.level = 0.95)
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# compare matched DS versus mismatched DS
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ##
# plot point for each DHS survey cluster (separate points in each month in a DS)
p_match_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="p_test_mean", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="DHS U5 PfPR")
p_mismatch_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_mismatched, y_col_name="p_test_mean", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="DHS U5 PfPR")
gg = grid.arrange(p_match_DHS_plots[[1]], p_match_DHS_plots[[2]], p_match_DHS_plots[[3]], p_match_DHS_plots[[4]],
                  p_mismatch_DHS_plots[[1]], p_mismatch_DHS_plots[[2]], p_mismatch_DHS_plots[[3]], p_mismatch_DHS_plots[[4]],
                  nrow = 2)
# ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_PfPR_sim_DHS.pdf'), gg, width=20, height=10)
ggsave(paste0(print_path, '/', 'match_PfPR_DHS_sim_mismatch_corr_plot.pdf'), gg, width=13, height=13)



# annual weighted average
#   (take weighted average of all DHS (and matching simulation) PfPR values within a year rather than one point per month sampled)
p_match_annual_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_dhs_mean", y_lab="DHS U5 PfPR")
p_mismatch_annual_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_mismatched_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_dhs_mean", y_lab="DHS U5 PfPR")
gg = grid.arrange(p_match_annual_DHS_plots[[1]], p_match_annual_DHS_plots[[2]], p_match_annual_DHS_plots[[3]], p_match_annual_DHS_plots[[4]],
                  p_mismatch_annual_DHS_plots[[1]], p_mismatch_annual_DHS_plots[[2]], p_mismatch_annual_DHS_plots[[3]], p_mismatch_annual_DHS_plots[[4]],
                  nrow = 2)
ggsave(paste0(print_path, '/', 'match_PfPR_annual_PfPR_sim_DHS.pdf'), gg, width=13, height=13)
#ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_annual_PfPR_sim_DHS.png'), gg, width=10, height=4.5)

# scatter plot of DHS survey annual admin 1 estimates and simulation pfpr
p_match_DHS_plots_admin1 = create_PfPR_scatters_admin1(pfpr_df=pfpr_matched_annual_admin1, y_col_name="pfpr_dhs_mean", x_col_name="`pfpr_sim_mean`", x_lab="simulation U5 PfPR", y_lab="DHS U5 PfPR")
gg = grid.arrange(p_match_DHS_plots_admin1[[1]], p_match_DHS_plots_admin1[[2]], p_match_DHS_plots_admin1[[3]], p_match_DHS_plots_admin1[[4]], nrow = 1)
ggsave(paste0(print_path, '/', 'match_PfPR_DHS_sim_annual_admin1_corr_plot.pdf'), gg, width=13, height=13)
ggsave(paste0(print_path, '/', 'match_PfPR_DHS_sim_annual_admin1_corr_plot.eps'), gg, device=cairo_ps,  fallback_resolution = 600)



# zoom in on 2018 and plot with DS names, colored by archetype
pfpr_matched_annual_2018 = pfpr_matched_annual[pfpr_matched_annual$year.y == 2018,]
pdf(file=paste0(print_path, "/", "2018_PfPR_DHS_compared_to_2015_simulation.pdf"))
ggplot(pfpr_matched_annual_2018, aes(x = pfpr_sim_mean, y = pfpr_dhs_mean,color = Archetype, label = LGA))+
  geom_point()+ 
  geom_abline(slope=1, intercept=c(0,0)) +
  geom_text(aes(label=LGA))+ 
  theme_classic()+ 
  xlab('simulation U5 PfPR')+ 
  ylab('DHS U5 PfPR')+ 
  ggtitle('Comparison of 2018 simulation and DHS (annual, weighted)')+
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")
dev.off()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# compare oberved DHS-simulation relationship with what would be expected if survey were done in simulated population
#   (assume local PfPR = DS simulated PfPR)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
pfpr_matched$sim_survey_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
pfpr_matched$sim_survey_U5_PfPR = pfpr_matched$sim_survey_U5_pos / pfpr_matched$num_U5_sampled



# plot point for each DHS survey cluster (separate points in each month in a DS)
p_match_sim_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="sim_survey_U5_PfPR", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="simulation survey U5 PfPR")
gg = grid.arrange(p_match_DHS_plots[[1]], p_match_DHS_plots[[2]], p_match_DHS_plots[[3]], p_match_DHS_plots[[4]],
                  p_match_sim_plots[[1]], p_match_sim_plots[[2]], p_match_sim_plots[[3]], p_match_sim_plots[[4]],
                  nrow = 2)
ggsave(paste0(print_path, '/', 'match_PfPR_binomial_draw_PfPR_sim_DHS.pdf'), gg, width=13, height=13)


# annual weighted average
pfpr_matched$sim_survey_prod_dhs_pfpr_ss = pfpr_matched$sim_survey_U5_PfPR * pfpr_matched$num_U5_sampled
pfpr_matched_sim_survey_annual = pfpr_matched[,c('LGA','year.y', 'sim_survey_prod_dhs_pfpr_ss','num_U5_sampled', 'prod_sim_pfpr_ss', 'Archetype')]  %>% group_by(year.y, LGA, Archetype) %>%  
  summarise_all(sum) %>% ungroup() 
pfpr_matched_sim_survey_annual$pfpr_sim_survey_dhs_mean = pfpr_matched_sim_survey_annual$sim_survey_prod_dhs_pfpr_ss / pfpr_matched_sim_survey_annual$num_U5_sampled
pfpr_matched_sim_survey_annual$pfpr_sim_mean = pfpr_matched_sim_survey_annual$prod_sim_pfpr_ss / pfpr_matched_sim_survey_annual$num_U5_sampled

p_annual_sim_survey_DHS_plots = create_PfPR_scatters(pfpr_df=pfpr_matched_sim_survey_annual, x_col_name="pfpr_sim_mean", x_lab="simulation U5 PfPR", y_col_name="pfpr_sim_survey_dhs_mean", y_lab="simulation survey U5 PfPR")
gg = grid.arrange(p_match_annual_DHS_plots[[1]], p_match_annual_DHS_plots[[2]], p_match_annual_DHS_plots[[3]], p_match_annual_DHS_plots[[4]],
                  p_annual_sim_survey_DHS_plots[[1]], p_annual_sim_survey_DHS_plots[[2]], p_annual_sim_survey_DHS_plots[[3]], p_annual_sim_survey_DHS_plots[[4]],
                  nrow = 2)
ggsave(paste0(print_path, '/', 'match_PfPR_binomial_draw_annual_PfPR_sim_DHS.pdf'), gg, width=13, height=13)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# compare oberved DHS-simulation relationship with what would be expected if survey were done in simulated population
#   (assume local PfPR is drawn from a distribution centered around the DS's simulated PfPR)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# functions to draw local PfPR from several different distribution options 
# Beta distribution with mean equal to DS's PfPR
get_local_PfPR = function(mean, variance=0.01){
  if(mean>0){
    if(variance >= (mean*(1-mean))){
      variance = mean * (1-mean) * 9/10
    }
    vv = mean * (1-mean) / variance
    alpha = mean * vv
    beta = (1-mean)*vv
    local_PfPR = rbeta(1, shape1=alpha, shape2=beta)
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# Normal distribution with mean equal to DS's PfPR
get_local_PfPR2 = function(mean, variance=0.01){
  if(mean>0){
    local_PfPR=-9
    while((local_PfPR<0) | (local_PfPR>1)){
      local_PfPR = rnorm(1, mean=mean, sd=variance^(1/2))
    }
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# Gamma distribution with mean equal to DS's PfPR
get_local_PfPR3 = function(mean, variance=0.01){
  scale = variance/mean
  shape = mean/scale
  if(mean>0){
    local_PfPR=-9
    while((local_PfPR<0) | (local_PfPR>1)){
      local_PfPR = rgamma(1, shape=shape, scale=scale)
    }
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# Normal distribution with mean equal to DS's PfPR and a variance increasing as mean increases
get_local_PfPR4 = function(mean, variance=0.01){
  mod_var = (1-abs(mean-0.5)/.6) * variance
  if(mean>0){
    local_PfPR=-9
    while((local_PfPR<0) | (local_PfPR>1)){
      local_PfPR = rnorm(1, mean=mean, sd=mod_var^(1/2))
    }
  } else{
    local_PfPR = 0
  }
  return(local_PfPR)
}
# # plot histogram of densities for different means and variances for a choice of sampling distribution
# get_local_PfPR_func = get_local_PfPR4
# mean_vals = c(0.001, 0.01, 0.075,0.2)
# var_vals = c(0.01, 0.075, 0.2)
# par(mfrow=c(length(var_vals), length(mean_vals)))
# for(vv in 1:length(var_vals)){
#   for(mm in 1:length(mean_vals)){
#     mean_val = mean_vals[mm]
#     var_cur = var_vals[vv]
#     hist(sapply(rep(mean_val,100000), get_local_PfPR_func, variance=var_cur), main=paste0('true mean:', mean_val, '; var: ', var_cur, '; mean:', round(mean(sapply(rep(mean_val,100000), get_local_PfPR_func, variance=var_cur)),2)), breaks = seq(0,1,0.05))
#   }
# }

# what does this look like if we sample pretend DHS observations from simulation PfPR where local PfPR is assumed to be drawn from Beta? essentially make up a dhs dataset from sampling the simulated population
variance = 0.125
dist = 'N_adjV'
if(dist == 'N'){
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR2, variance = variance)
} else if(dist == 'N_adjV'){
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR4, variance = variance)
} else{
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR, variance = variance)
}
pfpr_matched$sim_survey2_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$local_sim_PfPR)
pfpr_matched$sim_survey2_U5_PfPR = pfpr_matched$sim_survey2_U5_pos / pfpr_matched$num_U5_sampled

p_match_sim_plots = create_PfPR_scatters(pfpr_df=pfpr_matched, y_col_name="sim_survey2_U5_PfPR", x_col_name="`PfPR U5`", x_lab="simulation U5 PfPR", y_lab="simulation survey U5 PfPR")
gg = grid.arrange(p_match_DHS_plots[[1]], p_match_DHS_plots[[2]], p_match_DHS_plots[[3]], p_match_DHS_plots[[4]],
                  p_match_sim_plots[[1]], p_match_sim_plots[[2]], p_match_sim_plots[[3]], p_match_sim_plots[[4]],
                  nrow = 2)
ggsave(paste0(print_path, '/', 'match_PfPR_beta_draw_PfPR_sim_DHS.pdf'), gg, width=13, height=13)


###############################################################################################
#   compare percent change and absolute change in sample PfPR between 2010, 2015, and 2018
###############################################################################################
# function to create scatter plots comparisons of simulation versus DHS change in PfPR between 2010 and 2015, 2010 and 2018, and 2015 and 2018
create_scatter_change_comparison = function(pfpr_df, 
                                            sim_col_10_15, sim_col_10_18, sim_col_15_18,
                                            dhs_col_10_15, dhs_col_10_18, dhs_col_15_18,
                                            xlab, ylab,
                                            min_xylim, max_xylim,
                                            include_lm=TRUE,
                                            include_seasonArch = FALSE,
                                            arch_colors=cm.colors(22),
                                            admin_name='LGA'){
  pp1 = ggplot(pfpr_df, aes_string(x=sim_col_10_15, y=dhs_col_10_15)) +
    geom_point(aes_string(group=admin_name, size='min_ss_10_15'), shape=20) + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2010 and 2015') +
    theme_classic() +
    theme(legend.position = "none")
  
  pp2 = ggplot(pfpr_df, aes_string(x=sim_col_10_18, y=dhs_col_10_18)) +
    geom_point(aes_string(group=admin_name, size='min_ss_10_18'), shape=20) + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2010 and 2018') +
    theme_classic() +
    theme(legend.position = "none")
  pp3 = ggplot(pfpr_df, aes_string(x=sim_col_15_18, y=dhs_col_15_18)) +
    geom_point(aes_string(group=admin_name, size='min_ss_15_18',), shape=20) + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2015 and 2018') +
    theme_classic() +
    theme(legend.position = "none")
  if(include_seasonArch){
    pp1 = pp1 +
      geom_point(aes(size=min_ss_10_15, col=as.factor(Archetype)), shape=20) + 
      scale_color_manual(values=arch_colors)
    pp2 = pp2 +
      geom_point(aes(size=min_ss_10_18, col=as.factor(Archetype)), shape=20) + 
      scale_color_manual(values=arch_colors)
    pp3 = pp3 +
      geom_point(aes(size=min_ss_15_18, col=as.factor(Archetype)), shape=20) + 
      scale_color_manual(values=arch_colors)
  }
  if(include_lm){
    pp1 = pp1 + 
      geom_smooth(method=lm, mapping = aes(weight = min_ss_10_15))+
      stat_cor(method = "pearson", col='darkred')
    pp2 = pp2 + 
      geom_smooth(method=lm, mapping = aes(weight = min_ss_10_18))+
      stat_cor(method = "pearson", col='darkred')
    pp3 = pp3 + 
      geom_smooth(method=lm, mapping = aes(weight = min_ss_15_18))+
      stat_cor(method = "pearson", col='darkred')
  }
  gg = grid.arrange(pp1, pp2, pp3, nrow = 1)
  return(gg)
}



create_scatter_change_comparison_admin1 = function(pfpr_df, 
                                            sim_col_10_15, sim_col_10_18, sim_col_15_18,
                                            dhs_col_10_15, dhs_col_10_18, dhs_col_15_18,
                                            xlab, ylab,
                                            min_xylim, max_xylim,
                                            include_lm=TRUE,
                                            include_seasonArch = FALSE,
                                            arch_colors=cm.colors(22)){
  pp1 = ggplot(pfpr_df, aes_string(x=sim_col_10_15, y=dhs_col_10_15)) +
    geom_point(size=5, shape=20, color = "skyblue2") + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2010 and 2015') +
    theme_classic() +
    theme(legend.position = "none")
  pp2 = ggplot(pfpr_df, aes_string(x=sim_col_10_18, y=dhs_col_10_18)) +
    geom_point(size=5, shape=20, color = "skyblue2") + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2010 and 2018') +
    theme_classic() +
    theme(legend.position = "none")
  pp3 = ggplot(pfpr_df, aes_string(x=sim_col_15_18, y=dhs_col_15_18)) +
    geom_point(size=5, shape=20, color = "skyblue2") + 
    ylim(min_xylim, max_xylim) + 
    xlim(min_xylim, max_xylim) + 
    xlab(xlab) + 
    ylab(ylab)+
    ggtitle('between 2015 and 2018') +
    theme_classic() +
    theme(legend.position = "none")
  if(include_seasonArch){
    pp1 = pp1 +
      geom_point(aes(size=min_ss_10_15, col=as.factor(Archetype)), shape=20) + 
      scale_color_manual(values=arch_colors)
    pp2 = pp2 +
      geom_point(aes(size=min_ss_10_18, col=as.factor(Archetype)), shape=20) + 
      scale_color_manual(values=arch_colors)
    pp3 = pp3 +
      geom_point(aes(size=min_ss_15_18, col=as.factor(Archetype)), shape=20) + 
      scale_color_manual(values=arch_colors)
  }
  if(include_lm){
    pp1 = pp1 + 
      geom_smooth(method=lm)+
      geom_abline(slope=1, intercept=c(0,0))+
      stat_cor(method = "pearson", col='darkred')
    pp2 = pp2 + 
      geom_smooth(method=lm)+
      geom_abline(slope=1, intercept=c(0,0))+
      stat_cor(method = "pearson", col='darkred')
    pp3 = pp3 + 
      geom_smooth(method=lm)+
      geom_abline(slope=1, intercept=c(0,0))+
      stat_cor(method = "pearson", col='darkred')
  }
  gg = grid.arrange(pp1, pp2, pp3, nrow = 1)
  return(gg)
}



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# calculate changes between years for DHS and simulation
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# get percent change and raw change for dhs
pfpr_matched_annual_wide_dhs = dcast(pfpr_matched_annual, LGA + Archetype ~ year.y, value.var='pfpr_dhs_mean')



colnames(pfpr_matched_annual_wide_dhs) = c('LGA','Archetype','dhs_2010','dhs_2015','dhs_2018')
pfpr_matched_annual_wide_dhs$dhs_rel_2010_2015 = (pfpr_matched_annual_wide_dhs$dhs_2015-pfpr_matched_annual_wide_dhs$dhs_2010)/pfpr_matched_annual_wide_dhs$dhs_2010*100
pfpr_matched_annual_wide_dhs$dhs_rel_2010_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2010)/pfpr_matched_annual_wide_dhs$dhs_2010*100
pfpr_matched_annual_wide_dhs$dhs_rel_2015_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2015)/pfpr_matched_annual_wide_dhs$dhs_2015*100
pfpr_matched_annual_wide_dhs$dhs_abs_2010_2015 = (pfpr_matched_annual_wide_dhs$dhs_2015-pfpr_matched_annual_wide_dhs$dhs_2010)
pfpr_matched_annual_wide_dhs$dhs_abs_2010_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2010)
pfpr_matched_annual_wide_dhs$dhs_abs_2015_2018 = (pfpr_matched_annual_wide_dhs$dhs_2018-pfpr_matched_annual_wide_dhs$dhs_2015)

# get percent change and raw change for simulation
pfpr_matched_annual_wide_sim = dcast(pfpr_matched_annual, LGA + Archetype ~ year.y, value.var='pfpr_sim_mean')
# # move 2018 values to 2018 column
# pfpr_matched_annual_wide_sim$`2018`[is.na(pfpr_matched_annual_wide_sim$`2018`)] = pfpr_matched_annual_wide_sim$`2018`[is.na(pfpr_matched_annual_wide_sim$`2018`)]
# pfpr_matched_annual_wide_sim=pfpr_matched_annual_wide_sim[,-5]
colnames(pfpr_matched_annual_wide_sim) = c('LGA','Archetype','sim_2010','sim_2015','sim_2018')
pfpr_matched_annual_wide_sim$sim_rel_2010_2015 = (pfpr_matched_annual_wide_sim$sim_2015-pfpr_matched_annual_wide_sim$sim_2010)/pfpr_matched_annual_wide_sim$sim_2010*100
pfpr_matched_annual_wide_sim$sim_rel_2010_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2010)/pfpr_matched_annual_wide_sim$sim_2010*100
pfpr_matched_annual_wide_sim$sim_rel_2015_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2015)/pfpr_matched_annual_wide_sim$sim_2015*100
pfpr_matched_annual_wide_sim$sim_abs_2010_2015 = (pfpr_matched_annual_wide_sim$sim_2015-pfpr_matched_annual_wide_sim$sim_2010)
pfpr_matched_annual_wide_sim$sim_abs_2010_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2010)
pfpr_matched_annual_wide_sim$sim_abs_2015_2018 = (pfpr_matched_annual_wide_sim$sim_2018-pfpr_matched_annual_wide_sim$sim_2015)

# get DHS survey sizes (take minimum survey size between the two compared years)
pfpr_matched_annual_wide_size = dcast(pfpr_matched_annual, LGA + Archetype ~ year.y, value.var='num_U5_sampled')
# move 2018 values to 2018 column
# pfpr_matched_annual_wide_size$`2018`[is.na(pfpr_matched_annual_wide_size$`2018`)] = pfpr_matched_annual_wide_size$`2018`[is.na(pfpr_matched_annual_wide_size$`2018`)]
# pfpr_matched_annual_wide_size=pfpr_matched_annual_wide_size[,-5]
colnames(pfpr_matched_annual_wide_size) = c('LGA','Archetype','survey_size_2010','survey_size_2015','survey_size_2018')
pfpr_matched_annual_wide = merge(pfpr_matched_annual_wide_dhs, pfpr_matched_annual_wide_sim, by=c('LGA', 'Archetype'))
pfpr_matched_annual_wide = merge(pfpr_matched_annual_wide, pfpr_matched_annual_wide_size, by=c('LGA', 'Archetype'))
# get minimum survey size between the two years
pfpr_matched_annual_wide$min_ss_10_15 = pmin(pfpr_matched_annual_wide$survey_size_2010, pfpr_matched_annual_wide$survey_size_2015)
pfpr_matched_annual_wide$min_ss_10_18 = pmin(pfpr_matched_annual_wide$survey_size_2010, pfpr_matched_annual_wide$survey_size_2018)
pfpr_matched_annual_wide$min_ss_15_18 = pmin(pfpr_matched_annual_wide$survey_size_2015, pfpr_matched_annual_wide$survey_size_2018)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter plot of PfPR percent change for simulation versus dhs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
min_val = min(s(pfpr_matched_annual_wide[,c('dhs_rel_2010_2015', 'dhs_rel_2010_2018', 'dhs_rel_2015_2018', 'sim_rel_2010_2015', 'sim_rel_2010_2018', 'sim_rel_2015_2018')]), na.rm=TRUE)
max_val = 300
gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                      sim_col_10_15='sim_rel_2010_2015', sim_col_10_18='sim_rel_2010_2018', sim_col_15_18='sim_rel_2015_2018',
                                      dhs_col_10_15='dhs_rel_2010_2015', dhs_col_10_18='dhs_rel_2010_2018', dhs_col_15_18='dhs_rel_2015_2018',
                                      xlab='percent change in simulation U5 PfPR', ylab='percent change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=FALSE,
                                      include_seasonArch = FALSE)
ggsave(paste0(print_path, '/', 'percent_change_vs.PfPR_sim_DHS.pdf'), gg, width=13, height=13)

gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                 sim_col_10_15='sim_rel_2010_2015', sim_col_10_18='sim_rel_2010_2018', sim_col_15_18='sim_rel_2015_2018',
                                 dhs_col_10_15='dhs_rel_2010_2015', dhs_col_10_18='dhs_rel_2010_2018', dhs_col_15_18='dhs_rel_2015_2018',
                                 xlab='percent change in simulation U5 PfPR', ylab='percent change in DHS U5 PfPR',
                                 min_xylim=min_val, max_xylim=max_val,
                                 include_lm=TRUE,
                                 include_seasonArch = FALSE)
ggsave(paste0(print_path, '/', 'percent_change_vs.PfPR_sim_LM.pdf'), gg, width=13, height=13)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter plot of PfPR change for simulation versus dhs
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
min_val = -0.75
max_val = 0.5
arch_colors = c(rgb(60,87,180, maxColorValue=255),rgb(115, 193, 80, maxColorValue=255), rgb(242, 68,72, maxColorValue=255))
gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                      sim_col_10_15='sim_abs_2010_2015', sim_col_10_18='sim_abs_2010_2018', sim_col_15_18='sim_abs_2015_2018',
                                      dhs_col_10_15='dhs_abs_2010_2015', dhs_col_10_18='dhs_abs_2010_2018', dhs_col_15_18='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=FALSE,
                                      include_seasonArch = FALSE)
ggsave(paste0(print_path, '/', 'absolute_change_DHS_vs.PfPR_sim_.pdf'), gg, width=13, height=13)

gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
                                      sim_col_10_15='sim_abs_2010_2015', sim_col_10_18='sim_abs_2010_2018', sim_col_15_18='sim_abs_2015_2018',
                                      dhs_col_10_15='dhs_abs_2010_2015', dhs_col_10_18='dhs_abs_2010_2018', dhs_col_15_18='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=TRUE,
                                      include_seasonArch = FALSE)
ggsave(paste0(print_path, '/', 'absolute_change_DHS_vs.PfPR_sim_LM_.pdf'), gg, width=13, height=13)

# gg = create_scatter_change_comparison(pfpr_df=pfpr_matched_annual_wide, 
#                                       sim_col_10_15='sim_abs_2010_2015', sim_col_10_18='sim_abs_2010_2018', sim_col_15_18='sim_abs_2015_2018',
#                                       dhs_col_10_15='dhs_abs_2010_2015', dhs_col_10_18='dhs_abs_2010_2018', dhs_col_15_18='dhs_abs_2015_2018',
#                                       xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
#                                       min_xylim=min_val, max_xylim=max_val,
#                                       include_lm=TRUE,
#                                       include_seasonArch = TRUE,
#                                       arch_colors=arch_colors)
# ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_change_PfPR_sim_withLM_colorArch.png'), gg, width=12, height=4)


# plot DS names
par(mfrow=c(1,3))
plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2010 to 2015')
lines(c(-1,1), c(-1,1), col='grey')
text(x=pfpr_matched_annual_wide$sim_abs_2010_2015, y=pfpr_matched_annual_wide$dhs_abs_2010_2015, labels= pfpr_matched_annual_wide$LGA, col=c('red','green','blue')[pfpr_matched_annual_wide$Archetype], cex=0.8)
plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2010 to 2018')
lines(c(-1,1), c(-1,1), col='grey')
text(x=pfpr_matched_annual_wide$sim_abs_2010_2018, y=pfpr_matched_annual_wide$dhs_abs_2010_2018, labels= pfpr_matched_annual_wide$LGA, col=c('red','green','blue')[pfpr_matched_annual_wide$Archetype], cex=0.8)
plot(NA, ylim=c(min_val, max_val), xlim=c(min_val,max_val), bty='L', xlab='Simulation PfPR change', ylab='DHS PfPR change', main='Change from 2015 to 2018')
lines(c(-1,1), c(-1,1), col='grey')
text(x=pfpr_matched_annual_wide$sim_abs_2015_2018, y=pfpr_matched_annual_wide$dhs_abs_2015_2018, labels= pfpr_matched_annual_wide$LGA, col=c('red','green','blue')[pfpr_matched_annual_wide$Archetype], cex=0.8)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter plot of annual PfPR change for simulation versus DHS, aggregated to admin1 level
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# get later year pfpr relative to earlier year for dhs
pfpr_matched_annual_admin1_wide_dhs = dcast(pfpr_matched_annual_admin1, State ~ year.y, value.var='pfpr_dhs_mean')
# move 2018 values to 2018 column
# pfpr_matched_annual_admin1_wide_dhs$`2018`[is.na(pfpr_matched_annual_admin1_wide_dhs$`2018`)] = pfpr_matched_annual_admin1_wide_dhs$`2018`[is.na(pfpr_matched_annual_admin1_wide_dhs$`2018`)]
# pfpr_matched_annual_admin1_wide_dhs=pfpr_matched_annual_admin1_wide_dhs[,-5]
colnames(pfpr_matched_annual_admin1_wide_dhs) = c('admin1','dhs_2010','dhs_2015','dhs_2018')
pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2010_2015 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2015-pfpr_matched_annual_admin1_wide_dhs$dhs_2010)
pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2010_2018 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2018-pfpr_matched_annual_admin1_wide_dhs$dhs_2010)
pfpr_matched_annual_admin1_wide_dhs$dhs_abs_2015_2018 = (pfpr_matched_annual_admin1_wide_dhs$dhs_2018-pfpr_matched_annual_admin1_wide_dhs$dhs_2015)

# get later year pfpr relative to earlier year for simulation
pfpr_matched_annual_admin1_wide_sim = dcast(pfpr_matched_annual_admin1, State ~ year.y, value.var='pfpr_sim_mean')
# move 2018 values to 2018 column
# pfpr_matched_annual_admin1_wide_sim$`2018`[is.na(pfpr_matched_annual_admin1_wide_sim$`2018`)] = pfpr_matched_annual_admin1_wide_sim$`2018`[is.na(pfpr_matched_annual_admin1_wide_sim$`2018`)]
# pfpr_matched_annual_admin1_wide_sim=pfpr_matched_annual_admin1_wide_sim[,-5]
colnames(pfpr_matched_annual_admin1_wide_sim) = c('admin1','sim_2010','sim_2015','sim_2018')
pfpr_matched_annual_admin1_wide_sim$sim_abs_2010_2015 = (pfpr_matched_annual_admin1_wide_sim$sim_2015-pfpr_matched_annual_admin1_wide_sim$sim_2010)
pfpr_matched_annual_admin1_wide_sim$sim_abs_2010_2018 = (pfpr_matched_annual_admin1_wide_sim$sim_2018-pfpr_matched_annual_admin1_wide_sim$sim_2010)
pfpr_matched_annual_admin1_wide_sim$sim_abs_2015_2018 = (pfpr_matched_annual_admin1_wide_sim$sim_2018-pfpr_matched_annual_admin1_wide_sim$sim_2015)

# get DHS survey sizes
pfpr_matched_annual_admin1_wide_size = dcast(pfpr_matched_annual_admin1, State ~ year.y, value.var='num_U5_sampled')
# move 2018 values to 2018 column
# pfpr_matched_annual_admin1_wide_size$`2018`[is.na(pfpr_matched_annual_admin1_wide_size$`2018`)] = pfpr_matched_annual_admin1_wide_size$`2018`[is.na(pfpr_matched_annual_admin1_wide_size$`2018`)]
# pfpr_matched_annual_admin1_wide_size=pfpr_matched_annual_admin1_wide_size[,-5]
colnames(pfpr_matched_annual_admin1_wide_size) = c('admin1','survey_size_2010','survey_size_2015','survey_size_2018')
pfpr_matched_annual_admin1_wide = merge(pfpr_matched_annual_admin1_wide_dhs, pfpr_matched_annual_admin1_wide_sim, by=c('admin1'))
pfpr_matched_annual_admin1_wide = merge(pfpr_matched_annual_admin1_wide, pfpr_matched_annual_admin1_wide_size, by=c('admin1'))

# get minimum survey size between the two years
pfpr_matched_annual_admin1_wide$min_ss_10_15 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2010, pfpr_matched_annual_admin1_wide$survey_size_2015)
pfpr_matched_annual_admin1_wide$min_ss_10_18 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2010, pfpr_matched_annual_admin1_wide$survey_size_2018)
pfpr_matched_annual_admin1_wide$min_ss_15_18 = pmin(pfpr_matched_annual_admin1_wide$survey_size_2015, pfpr_matched_annual_admin1_wide$survey_size_2018)


# scatter plot of year-to-year absolute differences for simulation versus dhs
min_val = -0.6
max_val = 0.23
gg = create_scatter_change_comparison_admin1(pfpr_df=pfpr_matched_annual_admin1_wide, 
                                      sim_col_10_15='sim_abs_2010_2015', sim_col_10_18='sim_abs_2010_2018', sim_col_15_18='sim_abs_2015_2018',
                                      dhs_col_10_15='dhs_abs_2010_2015', dhs_col_10_18='dhs_abs_2010_2018', dhs_col_15_18='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=FALSE,
                                      include_seasonArch = FALSE)
ggsave(paste0(print_path, '/', 'absolute_change_admin1_DHS_vs.PfPR_sim.pdf'), gg, width=13, height=13, useDingbats=FALSE)
gg = create_scatter_change_comparison_admin1(pfpr_df=pfpr_matched_annual_admin1_wide, 
                                      sim_col_10_15='sim_abs_2010_2015', sim_col_10_18='sim_abs_2010_2018', sim_col_15_18='sim_abs_2015_2018',
                                      dhs_col_10_15='dhs_abs_2010_2015', dhs_col_10_18='dhs_abs_2010_2018', dhs_col_15_18='dhs_abs_2015_2018',
                                      xlab='change in simulation U5 PfPR', ylab='change in DHS U5 PfPR',
                                      min_xylim=min_val, max_xylim=max_val,
                                      include_lm=TRUE,
                                      include_seasonArch = FALSE)
ggsave(paste0(print_path, '/', 'absolute_change_admin1_DHS_vs.PfPR_sim_LM.pdf'), gg, width=13, height=13, useDingbats=FALSE)


ggsave(paste0(print_path, '/', 'absolute_change_admin1_DHS_vs.PfPR_sim_LM.eps'), gg, device=cairo_ps)


#################################################################
# histograms of PfPR differences and likelihoods
#################################################################

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#   histogram of PfPR differences between matched and mismatched DS from sim/data, across years
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
pdf(paste0(print_path, '/', 'hist_PfPR_abs_dif_sim_DHS_mismatchDS.pdf'), gg, width=13, height=13)
#  ----  all years  ----  #
# relative difference for matching DS
match_rel_dif = abs(pfpr_matched$p_test_mean - pfpr_matched$`PfPR U5`)

# mismatched DS (multiple mis-matches)
LGAs = unique(pfpr_case_u5_runMeans$LGA)
LGAs_shuffled = LGAs[c(2:length(LGAs), 1)]
mismatch_rel_dif = c()
for(ii in 1:20){
  pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
  pfpr_case_u5_runMeans_mis$LGA = sapply(pfpr_case_u5_runMeans$LGA, function(x) LGAs[c((ii+1):length(LGAs), 1:ii)][which(LGAs == x)])
  pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('LGA','date'), all.x=TRUE)
  mismatch_rel_dif = c(mismatch_rel_dif, abs(pfpr_mismatched$p_test_mean - pfpr_mismatched$`PfPR U5`))
}
# compare histograms
par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='',  xlab='difference')
mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
title(main='matched DS, all years', line=0.5)
hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
title(main='mismatched DS, all years', line=0.5)
mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)

#  ----  2010, 2015, 2018  ----  #
for(year in c(2010, 2015, 2018)){
  # relative difference for matching DS
  match_rel_dif = abs(pfpr_matched[pfpr_matched$year.y==year,]$p_test_mean - pfpr_matched[pfpr_matched$year.y==year,]$`PfPR U5`)
  
  # mismatched DS (multiple mis-matches)
  mismatch_rel_dif = c()
  for(ii in 1:20){
    pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
    pfpr_case_u5_runMeans_mis$LGA = sapply(pfpr_case_u5_runMeans$LGA, function(x) LGAs[c((ii+1):length(LGAs), 1:ii)][which(LGAs == x)])
    pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('LGA','date'), all.x=TRUE)
    mismatch_rel_dif = c(mismatch_rel_dif, abs(pfpr_mismatched[pfpr_mismatched$year.y==year,]$p_test_mean - pfpr_mismatched[pfpr_mismatched$year.y==year,]$`PfPR U5`))
  }
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('matched DS, ', year), line=0.5)
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('mismatched DS, ', year), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#   histogram of PfPR differences between matched and mismatched year from sim/data, across years
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
pdf(paste0(print_path, '/', 'hist_PfPR_abs_dif_sim_DHS_mismatchDS_compare.pdf'), gg, width=13, height=13)
year_true_vect = c(2010, 2010, 2015, 2015, 2018, 2018)
mismatch_year_vect = c(2015, 2018, 2010, 2018, 2010, 2015)
for(ii in 1:length(year_true_vect)){
  year_true = year_true_vect[ii]
  mismatch_year = mismatch_year_vect[ii]
  match_rel_dif = abs(pfpr_matched[pfpr_matched$year.y==year_true,]$p_test_mean - pfpr_matched[pfpr_matched$year.y==year_true,]$`PfPR U5`)
  
  # mismatched years
  pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
  pfpr_case_u5_runMeans_mis$year[pfpr_case_u5_runMeans_mis$year == year_true] = NA
  pfpr_case_u5_runMeans_mis$year[pfpr_case_u5_runMeans_mis$year == mismatch_year] = year_true
  pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('LGA','date'), all.x=TRUE)
  mismatch_rel_dif = abs(pfpr_mismatched[pfpr_mismatched$year.y==year_true,]$p_test_mean - pfpr_mismatched[pfpr_mismatched$year.y==year_true,]$`PfPR U5`)
  
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('matched: sim and data from ', year_true), line=0.5)
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  hist(mismatch_rel_dif, breaks=seq(0,1,0.05), freq=FALSE, ylim=c(0,6), main='', xlab='difference')
  title(main=paste0('mismatched: data from ', year_true, ', sim from ' , mismatch_year), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# how extreme is matched likelihood compared to when DS are scrambled at random?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #

# probability of drawing observed DHS positives if simulation PfPR is true probability
pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
match_loglik = sum(log(match_lik))
# mismatched DS (multiple mis-matches)
LGAs = unique(pfpr_case_u5_runMeans$LGA)
num_reps = 300

gg_list = list()
gg_index = 1
#  ----  2010, 2015, 2018  ----  #
for(year in list(c(2010), c(2015), c(2018))){
  print(paste('working on year', year))
  # likelihood of drawing observed DHS positives if simulation PfPR is true probability
  match_lik = dbinom(round(pfpr_matched[pfpr_matched$year.y %in% year,]$num_U5_pos), size=pfpr_matched[pfpr_matched$year.y %in% year,]$num_U5_sampled, prob=pfpr_matched[pfpr_matched$year.y %in% year,]$`PfPR U5`)
  match_loglik = sum(log(match_lik))
  
  # mismatched DS (multiple mis-matches)
  mismatch_loglik = rep(NA,num_reps)
  for(ii in 1:num_reps){
    pfpr_case_u5_runMeans_mis = pfpr_case_u5_runMeans
    LGAs_shuffled = sample(LGAs, size=length(LGAs), replace=FALSE)
    pfpr_case_u5_runMeans_mis$LGA = sapply(pfpr_case_u5_runMeans$LGA, function(x) LGAs_shuffled[which(LGAs == x)])
    pfpr_mismatched = merge(x=dhs_pfpr, y=pfpr_case_u5_runMeans_mis, by=c('LGA','date'), all.x=TRUE)
    pfpr_mismatched$num_U5_pos = pfpr_mismatched$p_test_mean * pfpr_mismatched$num_U5_sampled
    mismatch_lik_cur = dbinom(round(pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$num_U5_pos), size=pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$num_U5_sampled, prob=pfpr_mismatched[pfpr_mismatched$year.y %in% year,]$`PfPR U5`)
    mismatch_loglik[ii] = sum(log(mismatch_lik_cur))
  }
  # histogram
  gg_list[[gg_index]] = ggplot(as.data.frame(mismatch_loglik), aes(mismatch_loglik))+
    geom_histogram(aes(y=..density..), bins=40, fill='grey', color='darkgrey')+
    geom_vline(xintercept=match_loglik, color='red', size=2) +
    xlab('ln(probability of DHS observations)')+
    # xlim(min(mismatch_loglik, na.rm=TRUE), max(c(mismatch_loglik,match_loglik), na.rm=TRUE)) + 
    theme_classic()
  gg_index = gg_index+1
}
gg = grid.arrange(gg_list[[1]], gg_list[[2]], gg_list[[3]], nrow = 1)
# ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/hist_logprob_DHS_PfPR_match_mismatch.png'), gg, width=7.2, height=2)
ggsave(paste0(print_path, '/', 'hist_logprob_DHS_PfPR_match_mismatch_2010_15_18.pdf'), gg, width=13, height=13)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# how does matched likelihood compare to when we draw PfPR directly from simulation?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
pdf(paste0(print_path, '/', 'hist_logprob_DHS_PfPR_match_simsurvey_2010_15_18.pdf'), gg, width=13, height=13)

#  ----  all years  ----  #
# likelihood of drawing observed DHS positives if simulation PfPR is true probability
pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
match_loglik = sum(log(match_lik))
num_reps = 300
sim_survey_loglik = rep(NA,num_reps)
for(ii in 1:num_reps){
  # what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
  pfpr_matched$sim_survey_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
  # likelihood of drawing observed surveyed positives if simulation PfPR is true probability
  sim_survey_lik = dbinom(round(pfpr_matched$sim_survey_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
  sim_survey_loglik[ii] = sum(log(sim_survey_lik))
}
# histogram
par(mfrow=c(1,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(sim_survey_loglik, breaks=seq(min(c(sim_survey_loglik[is.finite(sim_survey_loglik)], match_loglik)), max(c(match_loglik, sim_survey_loglik)), length.out=100), freq=FALSE, main='', xlab='loglikelihood of sim survey PfPR observations')
# title(main='sim survey PfPR likelihoods, all years', line=0.5)
abline(v=match_loglik, col='red', lwd=3)
mtext('DHS survey', line=-0.85, col='red', side=2)
dev.off()

# draw from simulation where local PfPR is drawn from Beta with mean equal to DS PfPR
variance = 0.01
pdf(paste0(print_path, '/hist_loglik_simSurvey2_PfPR_var_2',round(variance*1000),'.pdf'), width=5, height=5.5)
# png(paste0(box_hbhi_filepath, '/project_notes/figures/hist_loglik_simSurvey2_PfPR_var',round(variance*1000),'.png'), width=720, height=480)

#  ----  all years  ----  #
# likelihood of drawing observed DHS positives if simulation PfPR is true probability
pfpr_matched$num_U5_pos = pfpr_matched$p_test_mean * pfpr_matched$num_U5_sampled
match_lik = dbinom(round(pfpr_matched$num_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
match_loglik = sum(log(match_lik))

num_reps = 300
sim_survey_loglik = rep(NA,num_reps)
for(ii in 1:num_reps){
  # what does this look like if we sample pretend DHS observations from simulation PfPR? essentially make up a dhs dataset from sampling the simulated population
  pfpr_matched$local_sim_PfPR = sapply(pfpr_matched$`PfPR U5`, get_local_PfPR, variance = variance)
  pfpr_matched$sim_survey2_U5_pos = rbinom(length(pfpr_matched$num_U5_sampled), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$local_sim_PfPR)
  
  # likelihood of drawing observed surveyed positives if simulation PfPR is true probability
  sim_survey_lik = dbinom(round(pfpr_matched$sim_survey2_U5_pos), size=pfpr_matched$num_U5_sampled, prob=pfpr_matched$`PfPR U5`)
  sim_survey_loglik[ii] = sum(log(sim_survey_lik))
}

# histogram
par(mfrow=c(1,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(sim_survey_loglik, breaks=seq(min(c(sim_survey_loglik[is.finite(sim_survey_loglik)], match_loglik)), max(c(match_loglik, sim_survey_loglik)), length.out=100), freq=FALSE, main='', xlab='loglikelihood of sim survey PfPR observations')
# title(main='sim survey PfPR likelihoods, all years', line=0.5)
abline(v=match_loglik, col='red', lwd=3)
# mtext('DHS survey', line=-0.85, col='red', side=2)
dev.off()


