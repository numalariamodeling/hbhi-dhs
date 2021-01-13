#################################################################################################################################################
# comparison_incidence_DHIS2_sim.R
# HBHI - Burkina Faso
# contact: Monique Ambrose
# October 2020

# currently, we claim that our calibration and parameterization was successful because we can see that simulated DS trajectories often visually 
#    agree fairly well with survey/surveillance data. To look at this a bit more closely and to see how DS-specific this match is, create a 
#    series of plots that compare simulation and data, either for the same (matched) DS or for mismatched DS.
# main types of comparisons are:
#    1) 






# TODO: Note: not done, just copied code over from plot_comparison_sim_data.R, but still need to reorganize and check









#################################################################################################################################################


###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                                setup
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################

library(ggplot2)
library(ggpubr)
library(gridExtra)
library(data.table)
library(RColorBrewer)
library(tidyr)
library(tibble)
library(dplyr)
library(reshape2)
library(hablar)

###################################################################
#   read in and format data and simulation output
###################################################################
box_filepath = 'C:/Users/mambrose/Box'
box_hbhi_filepath = paste0(box_filepath, '/hbhi_burkina')

# - - - - - - - - - - - - - - - - #
# incidence from dhis2
# - - - - - - - - - - - - - - - - #
use_Seb_flag= FALSE
if (use_Seb_flag){
  # version from Sebastian-cleaned DHIS2 file
  incidence_filename_seb = file.path(box_filepath, 'burkina_who','Routine','Routine_data_from_country','sebastian aggregates', 'BFA_Routine_case_data_DS_aggregate_Seb_sum_columns.csv')
  incidence_dhis2 = fread(incidence_filename_seb)
  # update DS Name to match with simulations
  ds_names = unique(pfpr_case_u5_2010$DS_Name)
  fix_misspell_list = list(c('bitou', 'Bittou'), c('fada', "Fada N'Gourma"), c('gorom', 'Gorom-Gorom'), c('karangasso vigue', 'Karangasso - Vigue'), c('manni', 'Mani'), c('nanoro', 'Nanoro'), c('ndorola', "N'Dorola"), c('nongr-massom', 'Nongr-Massoum'), c('sig-noghin', 'Sig-Nonghin'))
  for(ff in 1:length(fix_misspell_list)){
    incidence_dhis2$District[incidence_dhis2$District==fix_misspell_list[[ff]][1]] = fix_misspell_list[[ff]][2]
  }
  get_ds_index = function(pattern){which(toupper(ds_names) ==toupper(pattern))}
  incidence_dhis2$DS_Name = ds_names[unlist(sapply(incidence_dhis2$District, get_ds_index))]
  # use confirmed + presumed malaria for incidence
  incidence_dhis2$Incidence = incidence_dhis2$confpres / incidence_dhis2$District.Pop * 1000
  # remove outlier incidence
  incidence_dhis2$Incidence[incidence_dhis2$Incidence > as.numeric(quantile(incidence_dhis2$Incidence, probs=0.99999))] = NA
} else{
  incidence_filename = file.path(box_filepath, 'burkina_cases', 'Cases_from_WHO', 'Donnees_Paludisme_jgcleaned.csv')
  incidence_dhis2 = fread(incidence_filename)
  incidence_dhis2$year = incidence_dhis2$Année
  incidence_dhis2$month = incidence_dhis2$Mois
  incidence_dhis2$DS_Name = incidence_dhis2$`District/Admin2`
  # remove NA rows
  incidence_dhis2 = incidence_dhis2[(incidence_dhis2$DS_Name != ''),]
  # remove outlier incidence
  incidence_dhis2$Incidence[incidence_dhis2$Incidence > as.numeric(quantile(incidence_dhis2$Incidence, probs=0.99999))] = NA
  # hist(incidence_dhis2$Incidence, breaks=seq(0,230,10))
}


# - - - - - - - - - - - - - - - - #
# years and DS with SMC
# - - - - - - - - - - - - - - - - #
smc_filename = file.path(box_filepath, 'burkina_cases', 'SMC coverage 2014-2018.csv')
smc_dt = fread(smc_filename)
smc_dt$receivedSMC = smc_dt$`Number of children treated in 1st cycle` > 1
smc_dt$DS_Name = smc_dt$`Administrative levels 2`
smc_dt$year = smc_dt$Year


# - - - - - - - - - - - - - - - - #
# simulation output
# - - - - - - - - - - - - - - - - #
sim_filepath_2010 = paste0(box_hbhi_filepath, '/simulation_output/2010_to_2020/_v18/BF 2010_2020 allInterventions')
pfpr_case_all_2010 = fread(paste0(sim_filepath_2010, '/All_Age_monthly_Cases.csv'))
pfpr_case_all_2010[,date:=as.Date(date)]
pfpr_case_all_2010$year = lubridate::year(pfpr_case_all_2010$date)
pfpr_case_all_2010$month = lubridate::month(pfpr_case_all_2010$date)
pfpr_case_u5_2010 = fread(paste0(sim_filepath_2010, '/U5_PfPR_ClinicalIncidence_severeTreatment.csv'))
pfpr_case_u5_2010[,date:=as.Date(paste0(year,"-",month,"-01"),format="%Y-%m-%d")]
# mean values across runs
pfpr_case_all_runMeans  = pfpr_case_all_2010 %>% group_by(date, DS_Name) %>%  
  summarise_all(mean) %>% ungroup() 
pfpr_case_u5_runMeans  = pfpr_case_u5_2010 %>% group_by(date, DS_Name) %>%  
  summarise_all(mean) %>% ungroup() 
pfpr_case_u5_runMeans$NOMDEP = toupper(pfpr_case_u5_runMeans$DS_Name)



###################################################################
# merge DHS and corresponding simulation values
###################################################################
# match the same DS from simulation and data
incidence_matched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans, by=c('DS_Name', 'year','month'), all.x=TRUE)
incidence_matched$treatment_incidence_include_NMF = (incidence_matched$Received_Treatment + incidence_matched$Received_NMF_Treatment) / incidence_matched$`Statistical Population` * 1000
# incidence: match date from sim and data but shuffle DS
ds_names = unique(pfpr_case_all_runMeans$DS_Name)
ds_names_shuffled = ds_names[c(2:length(ds_names), 1)]
pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
pfpr_case_all_runMeans_mis$DS_Name = sapply(pfpr_case_all_runMeans$DS_Name, function(x) ds_names_shuffled[which(ds_names == x)])
incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('DS_Name', 'year','month'), all.x=TRUE)
incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000

# add smc info
incidence_matched = merge(incidence_matched, smc_dt[,c('year','DS_Name','receivedSMC')], by=c('year','DS_Name'), all.x=TRUE)
incidence_matched$receivedSMC[is.na(incidence_matched$receivedSMC)] = FALSE



###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                            plot comparisons
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################

#################################################################################################
# plot timeseries of DHIS2 incidence in each DS through time
#################################################################################################
png(paste0(box_hbhi_filepath, '/project_notes/figures/DHIS2_timeseries.png'), width=8, height=5, units='in', res=900)
par(mfrow=c(1,1), mar=c(4,4,2,2))
plot(incidence_matched[DS_Name == unique(incidence_matched$DS_Name)[1], date], (incidence_matched[DS_Name == unique(incidence_matched$DS_Name)[1], Incidence]), type='l', xlim=c(min(incidence_matched$date), max(incidence_matched$date)), ylim=c(0,170), xlab='date', ylab=c('DHIS2 incidence ','(confirmed + presumed malaria)/population*1000'), main=paste('DHIS2 timeseries'), bty='L', col='white')
for (i_ds in 1:length(unique(incidence_matched$DS_Name))){
  ds_df = incidence_matched[DS_Name == unique(incidence_matched$DS_Name)[i_ds],]
  ds_df = ds_df[order(ds_df$date),]
  # plot(ds_df$date, ds_df$maltreat_u5, type='b', pch=c(21,19)[ds_df$received_smc+1], main=paste('DHIS2: ', ds_names_plot[i_ds]))
  lines(ds_df$date, (ds_df$Incidence),  col='black')
}
dt_mean = incidence_matched %>% group_by(date) %>%  
  summarise_all(median) %>% ungroup() 
lines(dt_mean$date, (dt_mean$Incidence), lwd=3, col='blue')
dev.off()



#####################################################################################################
# timeseries comparison of average monthly incidence in simulation and DHIS2 in each DS
#####################################################################################################
# include both DS-years with and without SMC
png(paste0(box_hbhi_filepath, '/project_notes/figures/timeseries_comparison_incidence.png'), width=700, height=350)
par(mfrow=c(1,1))
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_matched$Incidence[incidence_ds_month_means_matched$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_matched$treatment_incidence_include_NMF[incidence_ds_month_means_matched$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_matched$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,max(c(incidence_ds_month_means_matched$Incidence, incidence_ds_month_means_matched$treatment_incidence_include_NMF), na.rm=TRUE)), type='b', bty='L', ylab='incidence', xlab='month')
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_matched$month+jitter, incidence_ds_month_means_matched$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=150, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()

# separate by DS-years with and DS-years without SMC
png(paste0(box_hbhi_filepath, '/project_notes/figures/timeseries_comparison_incidence_withWithoutSMC_allYears.png'), width=700, height=350)
par(mfrow=c(1,2))
# DS-years with SMC
incidence_ds_month_means_smc = incidence_matched[(incidence_matched$receivedSMC),c('DS_Name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, DS_Name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_smc$Incidence[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_smc$treatment_incidence_include_NMF[incidence_ds_month_means_smc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_smc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,200), type='b', bty='L', ylab='incidence', xlab='month')
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_smc$month+jitter, incidence_ds_month_means_smc$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=200, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')

# DS-years without SMC
incidence_ds_month_means_nosmc = incidence_matched[(!incidence_matched$receivedSMC),c('DS_Name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, DS_Name) %>%  
  summarise_all(mean) %>% ungroup() 
median_inc_dhis2_monthly = rep(NA, 12)
median_inc_sim_monthly = rep(NA,12)
for(mm in 1:12){
  median_inc_dhis2_monthly[mm] = median(incidence_ds_month_means_nosmc$Incidence[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
  median_inc_sim_monthly[mm] = median(incidence_ds_month_means_nosmc$treatment_incidence_include_NMF[incidence_ds_month_means_nosmc$month == mm], na.rm=TRUE)
}
# plot average montly PfPR for DHS samples and sims
jitter = runif(length(incidence_ds_month_means_nosmc$month), min=-0.15, max=0.15)
plot(NA, xlim=c(1,12), ylim=c(0,200), type='b', bty='L', ylab='incidence', xlab='month')
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$treatment_incidence_include_NMF, col=rgb(0.83,0,0.1), cex=0.5, pch=21)
points(incidence_ds_month_means_nosmc$month+jitter, incidence_ds_month_means_nosmc$Incidence, col=rgb(0.2,0.4,1), cex=0.5, pch=20)
lines(1:12, median_inc_dhis2_monthly, col=rgb(0.2,0.4,1))
lines(1:12, median_inc_sim_monthly, col=rgb(0.83,0,0.1))
legend(x=0.7,y=200, c('DHIS2','simulation'), lty=1, col=c(rgb(0.2,0.4,1), rgb(0.83,0,0.1)), bty='n')
dev.off()
par(mfrow=c(1,1))



#################################################################################################
#   scatterplots of simulated versus DHIS2 incidence for each DS (separate dots for each year)
#################################################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# scatter with all months and years, color by year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
linearMod = lm(Incidence ~ treatment_incidence_include_NMF, data=incidence_matched)  # build linear regression model on full data
# matched DS
p_match_all = ggplot(incidence_matched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=as.factor(year)), shape=20, alpha=1) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  # geom_abline(slope=summary(linearMod)$coefficients[2],intercept=c(0,summary(linearMod)$coefficients[1]))+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred') +
  scale_color_brewer(palette="Spectral")+ 
  ylab("monthly incidence from DHIS2") + 
  xlab("monthly incidence from simulation") +
  # ggtitle(paste0('all years: R^2=',round(summary(linearMod)$r.squared,2), ', slope=', round(summary(linearMod)$coefficients[2],2)))+ 
  theme_classic() 
# theme(legend.position = "none")
p_match_all
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_incidence_match_sim_DHIS2_allYear_allMonth.png'), p_match_all, width=6, height=4.5)

for (yy in seq(2013,2019,2)){
  p_match_yy = ggplot(incidence_matched[incidence_matched$year == yy,], aes(y=Incidence, x=treatment_incidence_include_NMF)) +
    geom_point(shape=20, alpha=1) + 
    geom_abline(slope=1, intercept=c(0,0)) +
    geom_smooth(method=lm)+
    ggtitle(yy)+ 
    theme_classic() 
  # theme(legend.position = "none")
  print(p_match_yy)
}
# mismatched DS
p_mismatch_all = ggplot(incidence_mismatched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=as.factor(year)), shape=20, alpha=1) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  # geom_abline(slope=summary(linearMod)$coefficients[2],intercept=c(0,summary(linearMod)$coefficients[1]))+
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred') +
  scale_color_brewer(palette="Spectral")+ 
  ylab("monthly incidence from DHIS2") + 
  xlab("monthly incidence from simulation") +
  # ggtitle(paste0('all years: R^2=',round(summary(linearMod)$r.squared,2), ', slope=', round(summary(linearMod)$coefficients[2],2)))+ 
  theme_classic() 
# theme(legend.position = "none")
p_mismatch_all
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_incidence_mismatch_sim_DHIS2_allYear_allMonth.png'), p_mismatch_all, width=6, height=4.5)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#   scatterplots of simulated and DHIS2 incidence for each DS, faceted by month or month-year
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
# matched DS
p_match_all = ggplot(incidence_matched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=year), shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  geom_smooth(method=lm)+
  ylab("incidence from DHIS2") + 
  xlab("incidence from simulation") +
  theme_classic() +
  theme(legend.position = "none")
# mismatched DS
p_mismatch_all = ggplot(incidence_mismatched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=year), shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  geom_smooth(method=lm)+
  ylab("incidence from DHIS2") + 
  xlab("incidence from simulation") +
  theme_classic() +
  theme(legend.position = "none")

# facet plot to break out by year and month
# matched DS
gg = p_match_all + facet_grid(year ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_incidence_match_sim_DHIS2.png'), gg, width=11, height=7.5)
# mismatched DS
gg = p_mismatch_all + facet_grid(year ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_incidence_mismatch_sim_DHIS2.png'), gg, width=11, height=7.5)

# facet plot broken out by month, color by year
# matched DS
gg = p_match_all + facet_grid(. ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_monthly_incidence_match_sim_DHIS2.png'), gg, width=12, height=3)
# mismatched DS
gg = p_mismatch_all + facet_grid(. ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_monthly_incidence_mismatch_sim_DHIS2.png'), gg, width=12, height=3)


# - - - - - - - - - - - - - - - #
# zoom into specific months
# - - - - - - - - - - - - - - - #
months = c(7,8)
p_match_all = ggplot(incidence_matched[incidence_matched$month %in% months,], aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=year), shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  geom_smooth(method=lm)+
  theme_classic() +
  theme(legend.position = "none")
gg = p_match_all + facet_grid(year ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_incidence_match_sim_DHIS2_months78.pdf'), gg, width=20, height=20)
# mismatched DS
p_mismatch_all = ggplot(incidence_mismatched[incidence_mismatched$month %in% months,], aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(aes(col=year), shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  geom_smooth(method=lm)+
  theme_classic() +
  theme(legend.position = "none")
gg = p_mismatch_all + facet_grid(year ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_incidence_mismatch_sim_DHIS2_months78.pdf'), gg, width=20, height=20)



#################################################################################################
#   scatterplots of simulated versus DHIS2 monthly incidence for each DS (average across year)
#################################################################################################
# get average for each DS-month across all years
incidence_ds_month_means_matched = incidence_matched[,c('DS_Name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, DS_Name) %>%  
  summarise_all(mean) %>% ungroup() 
incidence_ds_month_means_mismatched = incidence_mismatched[,c('DS_Name','year','month','Incidence','treatment_incidence_include_NMF')] %>%  group_by(month, DS_Name) %>%  
  summarise_all(mean) %>% ungroup() 

# facet plot broken out by month, showing average monthly values for each DS across all years
# matched DS
p_match_all = ggplot(incidence_ds_month_means_matched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  ylab('DHIS2 incidence') + 
  xlab('simulation incidence') +
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred', aes(label = ..r.label..)) +
  theme_classic() +
  theme(legend.position = "none")
gg = p_match_all + facet_grid(. ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_mean_monthly_incidence_match_sim_DHIS2.png'), gg, width=12, height=3)
# mismatched DS
p_mismatch_all = ggplot(incidence_ds_month_means_mismatched, aes(y=Incidence, x=treatment_incidence_include_NMF)) +
  geom_point(shape=20, alpha=0.5) + 
  geom_abline(slope=1, intercept=c(0,0)) +
  ylab('DHIS2 incidence') + 
  xlab('simulation incidence') +
  geom_smooth(method=lm)+
  stat_cor(method = "pearson", col='darkred', aes(label = ..r.label..)) +
  theme_classic() +
  theme(legend.position = "none")
gg = p_mismatch_all + facet_grid(. ~ month)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/scatter_mean_monthly_incidence_mismatch_sim_DHIS2.png'), gg, width=12, height=3)



##########################################################################################################
#   violinplots with residuals in each month, broken out by year
##########################################################################################################

# matched DS
incidence_matched$residual = incidence_matched$Incidence - incidence_matched$treatment_incidence_include_NMF
p_match_all = ggplot(incidence_matched[incidence_matched$year<2019,], aes(y=residual, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-160,160))+
  ylab('DHIS2 incidence - simulated')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_match_all + facet_grid(. ~ year)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/violin_incidence_match_sim_SebDHIS2.png'), gg, width=7.4, height=3)
# mismatched DS
incidence_mismatched$residual = incidence_mismatched$Incidence - incidence_mismatched$treatment_incidence_include_NMF
p_mismatch_all = ggplot(incidence_mismatched[incidence_mismatched$year<2019,], aes(y=residual, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-160,160))+
  ylab('DHIS2 incidence - simulated')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_mismatch_all + facet_grid(. ~ year)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/violin_incidence_mismatch_sim_SebDHIS2.png'), gg, width=7.4, height=3)

# mean absolute difference in each year
incidence_matched$abs_residual = abs(incidence_matched$residual)
mean_residuals_matched = incidence_matched[,c('year','abs_residual')] %>% group_by(year) %>%  
  summarise_all(mean, na.rm=TRUE) %>% ungroup() 
incidence_mismatched$abs_residual = abs(incidence_mismatched$residual)
mean_residuals_mismatched = incidence_mismatched[,c('year','abs_residual')] %>% group_by(year) %>%  
  summarise_all(mean, na.rm=TRUE) %>% ungroup() 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#   violinplots of residuals of simulated and DHIS2 incidence *relative to July* for each DS
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
month_value = 7
# subset to just July values
# matched
incidence_matched_july = incidence_matched[incidence_matched$month == month_value,c('DS_Name','year','month','Incidence','treatment_incidence_include_NMF')]
colnames(incidence_matched_july) = c('DS_Name','year','month','dhis2_july_incidence','sim_july_incidence')
incidence_matched_july = incidence_matched_july[,-c('month')]
incidence_matched = merge(incidence_matched, incidence_matched_july, by=c('DS_Name','year'), all.x=TRUE)
incidence_matched$dhis2_incidence_relative = incidence_matched$Incidence / incidence_matched$dhis2_july_incidence
incidence_matched$sim_incidence_relative = incidence_matched$treatment_incidence_include_NMF / incidence_matched$sim_july_incidence
incidence_matched$residual_relative = incidence_matched$dhis2_incidence_relative - incidence_matched$sim_incidence_relative
# mismatched
incidence_mismatched_july = incidence_mismatched[incidence_mismatched$month == month_value,c('DS_Name','year','month','Incidence','treatment_incidence_include_NMF')]
colnames(incidence_mismatched_july) = c('DS_Name','year','month','dhis2_july_incidence','sim_july_incidence')
incidence_mismatched_july = incidence_mismatched_july[,-c('month')]
incidence_mismatched = merge(incidence_mismatched, incidence_mismatched_july, by=c('DS_Name','year'), all.x=TRUE)
incidence_mismatched$dhis2_incidence_relative = incidence_mismatched$Incidence / incidence_mismatched$dhis2_july_incidence
incidence_mismatched$sim_incidence_relative = incidence_mismatched$treatment_incidence_include_NMF / incidence_mismatched$sim_july_incidence
incidence_mismatched$residual_relative = incidence_mismatched$dhis2_incidence_relative - incidence_mismatched$sim_incidence_relative

# yearly plot with violins of residuals for each month, relative to july
# matched DS
p_match_all = ggplot(incidence_matched[incidence_matched$year<2019,], aes(y=residual_relative, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-80,20))+
  ylab('DHIS2 incidence - simulated')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_match_all + facet_grid(. ~ year)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/violin_incidence_relativeJuly_match_sim_DHIS2.png'), gg, width=12, height=3)
# mismatched DS
p_mismatch_all = ggplot(incidence_mismatched[incidence_mismatched$year<2019,], aes(y=residual_relative, x=as.factor(month))) +
  geom_violin()+
  ylim(c(-80,20))+
  ylab('DHIS2 incidence - simulated')+
  xlab('month')+
  geom_abline(slope=0, intercept=c(0,0))+
  theme_classic() +
  theme(legend.position = "none")
gg = p_mismatch_all + facet_grid(. ~ year)
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/violin_incidence_relativeJuly_mismatch_sim_DHIS2.png'), gg, width=12, height=3)



################################################################################################
#   histogram of incidence differences between matched sim/data and mismatched sim/data
################################################################################################
####================================####
#  matched versus mismatched DS
####================================####
pdf(paste0(box_hbhi_filepath, '/project_notes/figures/hist_incidence_abs_dif_sim_DHIS2_mismatchDS.pdf'), width=6, height=6)
#  ----  all years  ----  #
# relative difference for matching DS
match_rel_dif = abs(incidence_matched$Incidence - incidence_matched$treatment_incidence_include_NMF)

# mismatched DS (multiple mis-matches)
ds_names = unique(pfpr_case_all_runMeans$DS_Name)
ds_names_shuffled = ds_names[c(2:length(ds_names), 1)]
mismatch_rel_dif = c()
for(ii in 1:20){
  pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
  pfpr_case_all_runMeans_mis$DS_Name = sapply(pfpr_case_all_runMeans$DS_Name, function(x) ds_names[c((ii+1):length(ds_names), 1:ii)][which(ds_names == x)])
  incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('DS_Name', 'year','month'), all.x=TRUE)
  incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
  mismatch_rel_dif = c(mismatch_rel_dif, abs(incidence_mismatched$Incidence - incidence_mismatched$treatment_incidence_include_NMF))
}
# compare histograms
par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
hist(match_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='',  xlab='difference')
mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
title(main='matched DS, all years', line=0.5)
hist(mismatch_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='', xlab='difference')
title(main='mismatched DS, all years', line=0.5)
mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)

#  ----  2010, 2014, 2017  ----  #
for(yy in 2013:2019){
  # relative difference for matching DS
  match_rel_dif = abs(incidence_matched[incidence_matched$year==yy,]$Incidence - incidence_matched[incidence_matched$year==yy,]$treatment_incidence_include_NMF)
  # mismatched DS (multiple mis-matches)
  ds_names = unique(pfpr_case_all_runMeans$DS_Name)
  ds_names_shuffled = ds_names[c(2:length(ds_names), 1)]
  mismatch_rel_dif = c()
  for(ii in 1:20){
    pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
    pfpr_case_all_runMeans_mis$DS_Name = sapply(pfpr_case_all_runMeans$DS_Name, function(x) ds_names[c((ii+1):length(ds_names), 1:ii)][which(ds_names == x)])
    incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('DS_Name', 'year','month'), all.x=TRUE)
    incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
    mismatch_rel_dif = c(mismatch_rel_dif, abs(incidence_mismatched[incidence_mismatched$year==yy,]$Incidence - incidence_mismatched[incidence_mismatched$year==yy,]$treatment_incidence_include_NMF))
  }
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='',  xlab='difference')
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  title(main=paste0('matched DS, ', yy), line=0.5)
  hist(mismatch_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='', xlab='difference')
  title(main=paste0('mismatched DS, ', yy), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))


####================================####
#  matched versus mismatched year
####================================####
pdf(paste0(box_hbhi_filepath, '/project_notes/figures/hist_incidence_abs_dif_sim_DHIS2_mismatchYears.pdf'), width=6, height=6)
year_true_vect = rep(c(2013, 2015, 2017, 2019), each=3)
mismatch_year_vect = c(2015,2017, 2019, 2013, 2017, 2019, 2013, 2015, 2019, 2013, 2015, 2017)
mismatch_rel_dif = c()
for(ii in 1:length(year_true_vect)){
  year_true = year_true_vect[ii]
  mismatch_year = mismatch_year_vect[ii]
  match_rel_dif = abs(incidence_matched[incidence_matched$year==year_true,]$Incidence - incidence_matched[incidence_matched$year==year_true,]$treatment_incidence_include_NMF)
  # mismatched years
  pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
  pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == year_true] = NA
  pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == mismatch_year] = year_true
  incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('DS_Name', 'year','month'), all.x=TRUE)
  incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
  mismatch_rel_dif = c(mismatch_rel_dif, abs(incidence_mismatched[incidence_mismatched$year==year_true,]$Incidence - incidence_mismatched[incidence_mismatched$year==year_true,]$treatment_incidence_include_NMF))
  # compare histograms
  par(mfrow=c(2,1), mgp=c(2, 0.5, 0), mar=c(4,4,3,1))
  hist(match_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='',  xlab=' difference')
  title(main=paste0('matched: sim and data from ', year_true), line=0.5)
  mtext(paste0('median difference=', round(median(match_rel_dif),2)), line=-2)
  hist(mismatch_rel_dif, breaks=seq(0,240,length.out=20), freq=FALSE, ylim=c(0,0.05), main='', xlab=' difference')
  title(main=paste0('mismatched: data from ', year_true, ', sim from ' , mismatch_year), line=0.5)
  mtext(paste0('median difference=', round(median(mismatch_rel_dif),2)), line=-2)
}
dev.off()
par(mfrow=c(1,1))



####################################################################################
# matrix of median differences for matched/mismatched years of sim and data
####################################################################################
years = 2013:2019
median_abs_dif = matrix(NA, nrow=length(years), ncol=length(years))
median_dif = matrix(NA, nrow=length(years), ncol=length(years))
# rows are sim, columns are data
for(rr in 1:length(years)){
  for(cc in 1:length(years)){
    year_true = years[cc]
    mismatch_year = years[rr]
    pfpr_case_all_runMeans_mis = pfpr_case_all_runMeans
    if (year_true != mismatch_year){
      pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == year_true] = NA
      pfpr_case_all_runMeans_mis$year[pfpr_case_all_runMeans_mis$year == mismatch_year] = year_true
    }
    incidence_mismatched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans_mis, by=c('DS_Name', 'year','month'), all.x=TRUE)
    incidence_mismatched$treatment_incidence_include_NMF = (incidence_mismatched$Received_Treatment + incidence_mismatched$Received_NMF_Treatment) / incidence_mismatched$`Statistical Population` * 1000
    mismatch_abs_dif = abs(incidence_mismatched[incidence_mismatched$year==year_true,]$Incidence - incidence_mismatched[incidence_mismatched$year==year_true,]$treatment_incidence_include_NMF)
    median_abs_dif[rr,cc] = median(mismatch_abs_dif)
    mismatch_dif = (incidence_mismatched[incidence_mismatched$year==year_true,]$Incidence - incidence_mismatched[incidence_mismatched$year==year_true,]$treatment_incidence_include_NMF)
    median_dif[rr,cc] = median(mismatch_dif)    
  }
}
# matrix showing median difference between simulated and DHIS2 incidence
colnames(median_dif) = years
rownames(median_dif) = years
median_dif_df = as.data.frame(median_dif)
median_dif_df$year_sim = rownames(median_dif_df)
median_dif_long = gather(median_dif_df, year_data, difference, '2013':'2019',factor_key=TRUE)
gg = ggplot(median_dif_long, aes(year_sim, forcats::fct_rev(year_data))) +
  geom_tile(aes(fill = difference)) + 
  geom_text(aes(label = round(difference, 1))) +
  scale_fill_gradient2(low = "blue", mid='white', midpoint=0, high = "red")+
  ggtitle('median(dhis2 incidence - sim incidence)')
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/heat_incidence_year_dif_sim_DHIS2.pdf'), gg, width=10, height=10)

# matrix showing median absolute difference between simulated and DHIS2 incidence
colnames(median_abs_dif) = years
rownames(median_abs_dif) = years
# heatmap(median_abs_dif, Rowv=NA, Colv=NA, revC=TRUE)
median_abs_dif_df = as.data.frame(median_abs_dif)
median_abs_dif_df$year_sim = rownames(median_abs_dif_df)
median_abs_dif_long = gather(median_abs_dif_df, year_data, difference, '2013':'2019',factor_key=TRUE)
gg = ggplot(median_abs_dif_long, aes(year_sim, forcats::fct_rev(year_data))) +
  geom_tile(aes(fill = difference)) + 
  geom_text(aes(label = round(difference, 1))) +
  scale_fill_gradient(low = "white", high = "forestgreen")+
  ggtitle('median(abs(dhis2 incidence - sim incidence))')
ggsave(paste0(box_hbhi_filepath, '/project_notes/figures/heat_incidence_year_abs_dif_sim_DHIS2.pdf'), gg, width=10, height=10)








