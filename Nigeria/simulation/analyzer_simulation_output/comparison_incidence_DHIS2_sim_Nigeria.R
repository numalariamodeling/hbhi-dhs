#################################################################################################################################################
# comparison_incidence_DHIS2_sim.R
# HBHI - Nigeria. Script was originall written 
# contact: Ifeoma Nigeria 
# October 2020

# currently, we claim that our calibration and parameterization was successful because we can see that simulated DS trajectories often visually 
#    agree fairly well with survey/surveillance data. To look at this a bit more closely and to see how DS-specific this match is, create a 
#    series of plots that compare simulation and data, either for the same (matched) DS or for mismatched DS.
# main types of comparisons are:
#

#################################################################################################################################################


###########################################################################
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                                setup
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
###########################################################################
rm(list = ls())
list.of.packages <- c("tidyverse", "ggplot2", "purrr",  "stringr", "sp", "rgdal", "raster", "hablar", 
                      "lubridate", "RColorBrewer", "ggpubr", "gridExtra", "data.table",  "nngeo", "reshape2", "foreign", "ggthemes", "viridis", "lemon",
                      "egg", "grid")
lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages

###################################################################
#   read in and format data and simulation output
###################################################################
box_filepath = "C:/Users/ido0493/Box/NU-malaria-team/projects"
box_hbhi_filepath = paste0(box_filepath, '/hbhi_nigeria')
ifelse(!dir.exists(file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/validation/incidence")), 
       dir.create(file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/validation/incidence")), FALSE)
print_path <- file.path(box_hbhi_filepath, "project_notes/publication/Hbhi modeling/validation/incidence")


# - - - - - - - - - - - - - - - - - - - - - - #
# incidence from dhis2 and population data
# - - - - - - - - - - - - - - - - - - - - - - #

incidence_dhis2 <- read.csv(file.path(box_hbhi_filepath, "incidence", "RIA_by_LGA_and_rep_DS.csv")) %>% 
  mutate(incidence = (AllagesOutpatientMalariaC/geopode.pop) * 1000, date = as.Date(date), year = lubridate::year(date)) %>% 
  #filter(incidence < 3) %>% 
  mutate(LGA =gsub("/", "-", .$LGA))
glimpse(incidence_dhis2)
  


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



###############################################################################
# merge DHIS and corresponding simulation values aand aggregate to state level
###############################################################################
# match the same DS from simulation and data
incidence_matched = merge(x=incidence_dhis2, y=pfpr_case_all_runMeans, by=c('LGA', 'year','month'), all.x=TRUE)

#incidence_matched$treatment_incidence_include_NMF = (incidence_matched$Received_Treatment + incidence_matched$Received_NMF_Treatment) / incidence_matched$`Statistical Population` * 1000


DS_arch_filename = file.path(box_hbhi_filepath, 'nigeria_LGA_pop.csv')

# add column with admin1 of each DS
admin1DS = fread(DS_arch_filename)
admin1DS = admin1DS[,c('LGA', 'State', 'Archetype')]


#aggregate and merge 
incidence_matched_state = left_join(incidence_matched, admin1DS, by=c('LGA')) %>% dplyr::select(-c(LGA, Archetype, Run_Number, month, year, date.y, repDS)) %>% 
  group_by(State, date.x) %>%  {left_join (summarise_at(., vars(-`Statistical Population`), sum),
                                           summarise_at(., vars(`Statistical Population`), mean)
  )}


###############################################################################
# Calculate incidence for data  and simulation and scale data 
###############################################################################

# incidence calculations 
incidence_matched_state$treatment_incidence_include_NMF_state = 
  (incidence_matched_state$Received_Treatment + incidence_matched_state$Received_NMF_Treatment) / incidence_matched_state$`Statistical Population` * 30/365* 1000
incidence_matched_state$incidence_state = 
  (incidence_matched_state$AllagesOutpatientMalariaC / incidence_matched_state$geopode.pop) * 1000




#calculate scaling params and add 
incidence_matched_df <- incidence_matched_state %>%  dplyr::select(State, date.x, treatment_incidence_include_NMF_state,incidence_state) %>%
  mutate(date = as.Date(date.x), year = lubridate::year(date.x), scaling = treatment_incidence_include_NMF_state/incidence_state,
         scaling = ifelse(!is.finite(scaling), 1, scaling)) %>%  
  dplyr::select(-date.x) 

mean_scaling <- incidence_matched_df %>%  group_by(State, year) %>%  summarise(mean_scale = mean(scaling), median_scale = median(scaling), 
                                                                              min_scale = min(scaling), max_scale = max(scaling)) 




###############################################################################
# Prepare data set to make time series plot and ccf plot 
###############################################################################

incidence_matched_df  <- left_join(incidence_matched_df, mean_scaling, by=c("State", "year")) %>%  
mutate(new_incidence_state = incidence_state *median_scale, State = ifelse(State == "Federal Capital Territory", "FCT", State)) 

incidence_matched_df_v2 <- incidence_matched_df %>% 
  pivot_longer(cols = c("treatment_incidence_include_NMF_state","new_incidence_state"), names_to = "type", values_to = "value") %>% 
  mutate(month = month(date))


incidence_matched_df_v2$State <- paste0(incidence_matched_df_v2$State, " ",  "(", as.character(incidence_matched_df_v2$year), ")")

incidence_matched_df_split = split(incidence_matched_df_v2, incidence_matched_df_v2$year)



###############################################################################
# Plotting functions 
###############################################################################

#incidence 

plot_ <-function(data){
  p<-ggplot(data, aes(x =month, y =value, group = type, color=type)) +
    geom_line() + 
    scale_color_viridis(discrete = TRUE) +
    facet_wrap(~State, scales = "free")+
    scale_color_manual(labels = c("health facility data (rescaled)", "simulation (includes RDT + non-malarial fevers)"), values = c("darkorchid4", "deepskyblue2")) +
    scale_x_continuous(labels = function(x) month.abb[x], breaks = c(1, 4, 7, 10))+
    theme_minimal() + 
    theme(legend.direction = "vertical", legend.title = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          axis.ticks.x = element_line(size = 0.5, colour = "black"),
          axis.ticks.y = element_line(size = 0.5, colour = "black"),
          strip.text.x = element_text(size = 7.5, colour = "black", face = "bold"))+ 
    ylab("monthly all age treated cases of uncomplicated malaria (per 1000)")+
    xlab(unique(data$year))

    
}
  
shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
 reposition_legend(p, 'center', panel=names)

}


#cross-correlation functions 

x <- ("new_incidence_state")
y <- list("treatment_incidence_include_NMF_state")


xcf_plot <- function(df){
  State <- unique(df$State)
  year <- unique(df$year)
  title<- paste0(State, " ", "(", year, ")")
  x <- df$new_incidence_state
  y<- df$treatment_incidence_include_NMF_state
  df_x <- eval(substitute(x),df)
  df_y <- eval(substitute(y),df)
  ccf.object <- ccf(df_x,df_y,plot=FALSE)
  output_table <-
    cbind(lag=ccf.object$lag, x.corr=ccf.object$acf) %>%
    as_tibble() %>%
    mutate(cat=ifelse(x.corr>0,"CCF > 0","CCF < 0"))
  output_table %>%
    ggplot(aes(x=lag,y=x.corr)) +
    geom_bar(stat="identity",aes(fill=cat))+
    scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
    ylab("")+
    xlab("lag (months)")+
    scale_y_continuous(limits=c(-1,1))+
    theme_minimal()+
    theme(legend.position= "none",plot.title=element_text(size=8, color = "black", face = "bold", hjust=0.5),
          panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          axis.ticks.x = element_line(size = 0.5, colour = "black"),
          axis.ticks.y = element_line(size = 0.5, colour = "black"))+
    labs(title = title)
}






###############################################################################
# make plots 
###############################################################################

#apply functions and save incidence plots for all data 
p <-list()

for(i in 1:length(incidence_matched_df_split)){
  p[[i]]<- plot_(incidence_matched_df_split[[i]])  
}



for (i in 1:length(p)) {
  ggsave(shift_legend2(p[[i]]), file=paste0(print_path, '/', unique(p[[i]]$data$year),'_incidence_sim_data_comparison.pdf'), onefile=FALSE)
}




# make dataset for ccf
df_split_ccf <- split(incidence_matched_df, incidence_matched_df[, c("State", "year")])



#plot for all the data 
cc_df <- map(df_split_ccf, xcf_plot)

first_37 <- cc_df[1:37]
n <- length(first_37)
p<-do.call("grid.arrange", c(first_37, ncol=7, left = "Cross-correlation function (CCF)"))

ggsave(paste0(print_path, '/', 'incidence_ccf_2014_nigeria.pdf'), p, width=13, height=13)



second_37 <- cc_df[38:74]
n <- length(second_37)
p<-do.call("grid.arrange", c(second_37, ncol=7, left = "Cross-correlation function (CCF)"))

ggsave(paste0(print_path, '/', 'incidence_ccf_2015_nigeria.pdf'),p, width=13, height=13)




third_37 <- cc_df[75:111]
n <- length(third_37)
p<-do.call("grid.arrange", c(third_37, ncol=7, left = "Cross-correlation function (CCF)"))

ggsave(paste0(print_path, '/', 'incidence_ccf_2016_nigeria.pdf'),p, width=13, height=13)


fourth_37 <- cc_df[112:148]
n <- length(fourth_37)
p<-do.call("grid.arrange", c(fourth_37, ncol=7, left = "Cross-correlation function (CCF)"))
ggsave(paste0(print_path, '/', 'incidence_ccf_2017_nigeria.pdf'),p, width=13, height=13)


fifth_37 <- cc_df[149:185]
n <- length(fifth_37)
p<-do.call("grid.arrange", c(fifth_37, ncol=7, left = "Cross-correlation function (CCF)"))
ggsave(paste0(print_path, '/', 'incidence_ccf_2018_nigeria.pdf'),p, width=13, height=13)



###############################################################################
# plot incidence and cross-correlation function for Abia and Adamawa 
###############################################################################

#incidence plot 
Abia_Adamawa <- incidence_matched_df_split[[5]] %>%  filter(str_detect(State, "Abia|Adamawa"))
gg_abia_adamawa <-plot_(Abia_Adamawa)
gg_abia_adamawa



#ccf plot 
abia_ada <- incidence_matched_df %>% filter(str_detect(State, "Abia|Adamawa"), year == 2018)
abia_ada_ccf <- split(abia_ada, abia_ada[, c("State")])
cc_df <- map(abia_ada_ccf, xcf_plot)
cc_df


cc_plot <- grid.arrange(cc_df[[1]],cc_df[[2]], ncol =2)


g1 <- ggplotGrob(gg_abia_adamawa)
g2 <- ggplotGrob(cc_df[[1]])
g3 <- ggplotGrob(cc_df[[2]])

g2$grobs

fg2 <- gtable_frame(g2, debug = TRUE)
fg3 <- gtable_frame(g3, debug = TRUE)
fg23 <-
  gtable_frame(gtable_cbind(fg2, fg3))

fg1 <-
  gtable_frame(g1, debug = TRUE)

pdf(paste0(print_path, '/', 'Abia_Adamawa_incidence_validation_210119.pdf'),width=13, height=13)
grid.newpage()
combined <- gtable_rbind(fg1, fg23)
grid.draw(combined)
dev.off()















  


  
