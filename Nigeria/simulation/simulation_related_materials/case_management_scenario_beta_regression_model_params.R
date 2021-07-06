rm(list=ls())

##-------------------------------------------
# File paths 
##-------------------------------------------
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
ProjectDir <- file.path(NuDir, 'data', 'nigeria_dhs' , 'data_analysis')
functionDir <- file.path(NuDir,"data/nigeria_dhs/data_analysis/src/DHS/1_variables_scripts")
Bin<- file.path(ProjectDir, 'bin')
ResultsDir <- file.path(ProjectDir, 'results')
ModelparamDir <- file.path(ResultsDir, "archetype_sim_input/Intervention_files_LGA/case_management/model_params")
  



##-------------------------------------------
# packages
##-------------------------------------------
source(file.path(functionDir, "generic_functions", "DHS_fun.R"))
library(plyr)
library(betareg)
library(gamlss)



##-------------------------------------------
# Model  
##-------------------------------------------

#case management data (2010 - 2018)
cm_3 <- read.csv(file.path(Bin, 'projection/s3_v3/cm_trend.csv')) %>%  dplyr::select(year, repDS, comboACT)
hist(cm_3$comboACT)


cm_list<- split(cm_3, cm_3$repDS)
 
#fitting a zero-inflated beta model to the data to get average yearly change in case management 
avg_change <- list()
for(i in 1:length(cm_list)){
  m <- gamlss(comboACT ~ year, data=cm_list[[i]], family = BEZI)
  df <- data.frame(year = c(2013,2014, 2015,2016, 2017, 2018))
  df$comboACT <- predict(m, newdata=df, type = "response")
  diff <- mean(diff(df$comboACT))
  avg_change <- append(avg_change, list(diff))
}

names(avg_change) <- names(cm_list)
avg_change<-ldply(avg_change, data.frame)
colnames(avg_change)[2] <- "beta_model_params"

print(avg_change)#we observe negative values in model params

#get the second smallest values and use to replace negative value
n <- length(avg_change$beta_model_params)
x <- avg_change$beta_model_params
val<-sort(x,partial=2)[2]

overall_change <- mean(avg_change$beta_model_params)
avg_change <- avg_change %>%  mutate(beta_model_params = ifelse(.id == 'Soba', val, beta_model_params))
write.csv(avg_change,file.path(wd, print_path, 'beta_model_params_archetype.csv'))

sink(file.path(ModelparamDir, "overall_beta_model_param.txt"))
cat("overall change in case management")
cat('/n')
cat(overall_change)
sink()


##-------------------------------------------
# plots  
##-------------------------------------------
#fitting a zero-inflated beta model to the data to get model params

model_list <- list()
for(i in 1:length(cm_list)){
  m <- gamlss(comboACT ~ year, data=cm_list[[i]], family = BEOI)
  model_list <- append(model_list, list(m))
}

lapply(model_list, summary)

fitted(model_list[[1]])

# lm_eqn = function(df){
#   m = gamlss(comboACT ~ factor(year), data = df, family = BEZI);
#   eq <- paste0("CM coverage =", " ",  round(coef(m)[1], 3)," + ", "\n", round(abs(coef(m)[2]), 3), " year ")
#   as.character(as.expression(eq));                 
# }
# 
# mymax = function(df){
#   max(df$year)
# }
# 
# regs <- ddply(cm_3, .(repDS), lm_eqn)
# regs
# regs.xpos <- ddply(cm_3, .(repDS), function(df) (min(df$year)+max(df$year))/2)
# regs.ypos <- ddply(cm_3, .(repDS), function(df) min(df$year) + 0.05*(max(df$year)-min(df$year)))
# 
# regs$y <- regs.ypos$V1
# regs$x <- regs.xpos$V1
# 
# 
# gp<-ggplot(data=cm_3, aes(year, comboACT)) +
#   geom_point(size = 1, alpha=0.75)+ geom_smooth(method="lm", se=FALSE, color="red")+
#   scale_x_continuous(breaks = c(2013, 2015, 2017))+
#   geom_text(data = regs, size = 3, aes(label =V1,y = 0.82, x = 2015.5)) +
#   facet_wrap(vars(repDS))+
#   ylab("Case Management Coverage")+
#   theme_minimal()+
#   theme(strip.text.x = element_text(size = 8.5, colour = "black", face = "bold"),
#         panel.border = element_rect(colour = "black", fill=NA, size=0.5),
#         axis.ticks.x = element_line(size = 0.5, colour = "black"),
#         axis.ticks.y = element_line(size = 0.5, colour = "black"))
# 
# 
# 
# 
# 
# ggsave(file=paste0("results/archetype_sim_input/Intervention_files_LGA/case_management/linear_model_fit_", Sys.Date(),"_", ".png"), gp, scale=1.5, width=11, height=8)
