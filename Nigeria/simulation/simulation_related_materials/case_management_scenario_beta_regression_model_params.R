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
# Data
##-------------------------------------------

#case management data (2010 - 2018)
cm_3 <- read.csv(file.path(Bin, 'projection/s3_v3/cm_trend.csv')) %>%  dplyr::select(year, repDS, comboACT)
hist(cm_3$comboACT)


cm_list<- split(cm_3, cm_3$repDS)

names <- names(cm_list)

##-------------------------------------------
# Model  
##-------------------------------------------
 
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
eqn_list <- list()
pred_list <- list()

for(i in 1:length(cm_list)){
  
  m <- gamlss(comboACT ~ year, data=cm_list[[i]], family = BEZI)
  model_list <- append(model_list, list(m))
  
  eq <- paste0('logit(E(cm coverage)) =', " ", round(coef(m)[1], 2), " + ", round(coef(m)[2], 2), ' year ', "\n",
               'log(sigma) =', " ", round(unname(m$sigma.coefficients), 2), "\n",
               'logit(nu) =', round(unname(m$nu.coefficients), 2))
  eqn_list <- append(eqn_list, list(as.character(as.expression(eq))))
  
  df <- data.frame(year = c(2013,2014, 2015,2016, 2017, 2018))
  df$comboACT <- predict(m, newdata=df, type = "response")
  pred_list <- append(pred_list, list(df))
}


lapply(model_list, summary)

names(pred_list) <- names(cm_list)
names(eqn_list) <- names(cm_list)

predict_cm <- ldply(pred_list)
colnames(predict_cm)[1] <- 'rep_DS'

eqn_cm <- ldply(eqn_list)
colnames(eqn_cm)[1] <- 'rep_DS'


gp<-ggplot(data=cm_3, aes(year, comboACT)) +
  geom_point(alpha=0.1, color ='black')+ facet_wrap(~rep_DS)+
  geom_line(data = predict_cm, aes(year, comboACT), color = 'red', size =1)+ 
  geom_text(data = eqn_cm, size = 3, aes(label =V1,y = 0.76, x = 2015.5)) +
  scale_x_continuous(breaks = c(2013, 2015, 2017))+
  ylab("Case Management Coverage")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 8.5, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.ticks.x = element_line(size = 0.5, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black"))



 
ggsave(file=paste0(ModelparamDir, '/',  'beta_model_fit_', Sys.Date(),"_", ".pdf"), gp, scale=1.5, width=11, height=8)
