##----------------------------------
## file paths 
##----------------------------------

rm(list=ls())
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
oldSimInDir <- file.path(NuDir, "/projects/hbhi_nigeria/simulation_inputs/projection_csvs/projection_v3")
SimInDir <- file.path(NuDir, "/projects/hbhi_nigeria/simulation_inputs/projection_csvs/projection_v4/CM")
DatDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
ParamDir <- file.path(DatDir, "results/archetype_sim_input/Intervention_files_LGA/case_management/model_params")
ScriptDir <- file.path(NuDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")
print_path <- file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/figures")
source(file.path(ScriptDir, "generic_functions", "DHS_fun.R"))




##--------------------------------------
## files used by all scenarios 
##--------------------------------------

param <- read.csv(file.path(ParamDir, 'beta_model_params_archetype.csv'))
colnames(param)[2]<- 'Rep_DS'

##--------------------------------------
## new code for cm update - scenario 2
##--------------------------------------

library(plyr)

cm <- read.csv(paste0(SimInDir, "/cm_scenario1_BAU_2020_2030.csv"))
cm <- expandRows(cm, count = 11,count.is.col=FALSE, 
                   drop = FALSE) 
lookup_key <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
cm<- cm %>% mutate(round = 
                     rep(seq(lookup_key), times =774))


cm<- ddply(cm, .(LGA), mutate, year = c(2020:2030), simday = seq(0, 3650, by=365), duration = c(diff(seq(0, 3650, by=365)), -1))




cm_split<- ddply(cm, .(Rep_DS), summarize, mean=round(mean(U5_coverage), 2), max = round(max(U5_coverage), 2),
                 min = round(min(U5_coverage), 2))

cm_param <- left_join(cm_split, param)
cm_param$min_increase <- (((0.80 - cm_param$min)/cm_param$beta_model_params)/10) * cm_param$beta_model_params
summary(cm_param$min_increase)
cm_param <- cm_param %>%  dplyr::select(Rep_DS, min_increase)
  
#join both dataframes 

cm_fin <- cm %>%  left_join(cm_param)

max_v <- 0.80 #coverage cap

cm_fin <- cm_fin %>% mutate(scale_values = round*min_increase,
                        U5_coverage = pmin(U5_coverage + scale_values,max_v),
                        adult_coverage = U5_coverage, severe_cases = pmin(severe_cases + scale_values, max_v))


write.csv(cm_fin, paste0(SimInDir, "/cm_scenario2_increase80_2020_2030.csv"))


##--------------------------------------
## new code for cm update - scenario 3 -5
##--------------------------------------

#3
cm <- read.csv(paste0(SimInDir, "/cm_scenario1_BAU_2020_2030.csv"))
cm <- expandRows(cm, count = 11,count.is.col=FALSE, 
                 drop = FALSE) 
lookup_key <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 10)
cm<- cm %>% mutate(round = 
                     rep(lookup_key, times =774))


cm<- ddply(cm, .(LGA), mutate, year = c(2020:2030), simday = seq(0, 3650, by=365), duration = c(diff(seq(0, 3650, by=365)), -1))


cm_param <- left_join(cm_split, param)
cm_param$min_increase <-  (0.1)/10 #(arbitary decrease)
cm_param <- cm_param %>%  dplyr::select(Rep_DS, min_increase)

#join both dataframes 

cm_fin <- cm %>%  left_join(cm_param)


cm_fin <- cm_fin %>% mutate(scale_values = round*min_increase,
                            U5_coverage = pmin(U5_coverage + scale_values, max_v),
                            adult_coverage = U5_coverage, severe_cases = pmin(severe_cases + scale_values, max_v))


cm_10_coverage <- data.frame(coverage =cm_fin$U5_coverage, scale_values = cm_fin$scale_values)
cm_10_coverage$category <- '10 percent'
write.csv(cm_fin, paste0(SimInDir, "/cm_scenario3_increase10_2020_2030.csv"))


#4
cm_param <- left_join(cm_split, param)
cm_param$min_increase <-  (0.2)/10 #(arbitary decrease)
cm_param <- cm_param %>%  dplyr::select(Rep_DS, min_increase)


#join both dataframes 

cm_fin <- cm %>%  left_join(cm_param)


cm_fin <- cm_fin %>% mutate(scale_values = round*min_increase,
                            U5_coverage = pmin(U5_coverage + scale_values, max_v),
                            adult_coverage = U5_coverage, severe_cases = pmin(severe_cases + scale_values, max_v))

cm_20_coverage <- data.frame(coverage =cm_fin$U5_coverage, scale_values = cm_fin$scale_values)
cm_20_coverage$category <- '20 percent'

write.csv(cm_fin, paste0(SimInDir, "/cm_scenario4_increase20_2020_2030.csv"))



#5
cm_param <- left_join(cm_split, param)
cm_param$min_increase <-  (0.3)/10 #(arbitary decrease)
cm_param <- cm_param %>%  dplyr::select(Rep_DS, min_increase)


#join both dataframes 

cm_fin <- cm %>%  left_join(cm_param)


cm_fin <- cm_fin %>% mutate(scale_values = round*min_increase,
                            U5_coverage = pmin(U5_coverage + scale_values, max_v),
                            adult_coverage = U5_coverage, severe_cases = pmin(severe_cases + scale_values, max_v))

cm_30_coverage <- data.frame(coverage =cm_fin$U5_coverage, scale_values = cm_fin$scale_values)
cm_30_coverage$category <- '30 percent'

write.csv(cm_fin, paste0(SimInDir, "/cm_scenario5_increase30_2020_2030.csv"))



##--------------------------------------
## new code for cm update - scenario 6_7
##--------------------------------------

cm <- read.csv(paste0(SimInDir, "/cm_scenario1_BAU_2020_2030.csv"))
cm <- expandRows(cm, count = 11,count.is.col=FALSE, 
                 drop = FALSE) 
lookup_key <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
cm<- cm %>% mutate(round = 
                     rep(seq(lookup_key), times =774))


cm<- ddply(cm, .(LGA), mutate, year = c(2020:2030), simday = seq(0, 3650, by=365), duration = c(diff(seq(0, 3650, by=365)), -1))

cm_param <- left_join(cm_split, param)
cm_param <- cm_param %>%  dplyr::select(Rep_DS, beta_model_params)


#join both dataframes 

cm_fin <- cm %>%  left_join(cm_param)


cm_fin <- cm_fin %>% mutate(scale_values = round*beta_model_params,
                            U5_coverage = pmin(U5_coverage + scale_values, max_v),
                            adult_coverage = U5_coverage, severe_cases = pmin(severe_cases + scale_values, max_v))

cm_6_7_coverage <- data.frame(coverage =cm_fin$U5_coverage, scale_values = cm_fin$scale_values)
cm_6_7_coverage$category <- 'funded'

write.csv(cm_fin, paste0(SimInDir, "/cm_scenario6_7_funded_2020_2030.csv"))


##--------------------------------------
## plots 
##--------------------------------------
cm_coverages<- rbind(cm_10_coverage, cm_20_coverage, cm_30_coverage, cm_6_7_coverage)

coverages_density <- ggplot(cm_coverages, aes(coverage, fill = category)) + geom_density(alpha = 0.2)

coverages_histogram <- ggplot(cm_coverages, aes(coverage, fill = category)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')

scaling_density <- ggplot(cm_coverages, aes(scale_values, fill = category)) + 
  geom_histogram(alpha = 0.5, aes(y = ..density..), position = 'identity')


figure <- ggarrange(coverages_density , scaling_density,
                    labels = c("A", "B"),
                    nrow = 2)


ggsave(paste0(SimInDir, '/', Sys.Date(),  'CM_coverages_scaling.png'), figure)
