#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", " randomGLM", "ROCR", "AER", "caretEnsemble", "klaR", "naniar", "corrplot", "lmtest")

lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)

# set document path to current script path 
setwd("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data")

# Load pre-clustered data:
dat0 <- read.csv("Nigeria_2010_2018_clustered_final_dataset.csv", 
                 header = T, sep = ',')

print(names(dat0))

urbandataset <- dat0 %>% filter(Rural_urban == 1)
ruraldataset <- dat0 %>% filter(Rural_urban == 2)
comineddataset <- dat0


#replace dat0 with either urbandataset, ruraldataset, or comineddataset

dat1 = ruraldataset[,c("p_test", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
                       "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
                       "rainfal", "annual_precipitation")]


# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)



dat2 = dat1[,c("y", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
               "rainfal", "annual_precipitation")]

dat2 <- dat2%>% mutate(annual_precipitation = scale(dat2$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

dat2 <- dat2 %>% mutate(pop_den = na_if(pop_den, -9999))
dat2 <- dat2 %>% mutate(log_pop_den = log(pop_den))
nearZeroVar(dat2)

# Create reduced dataset for model 2:
# (filter out rows with NA values)
dat2 <- na.omit(dat2)
table(dat2$p_level)

######################################################################
####Step one: univariable analysis#####
#wealth
univariable_wealth <- glm(y ~ wealth_2, data = dat2, family = binomial)
summary(univariable_wealth)

#education 
univariable_edu <- glm(y ~ edu_a, data = dat2, family = binomial)
summary(univariable_edu)

#net_use_u5
univariable_net_use_u5 <- glm(y ~ net_use_u5, data = dat2, family = binomial)
summary(univariable_net_use_u5)

#net_use_preg
univariable_net_use_preg <- glm(y ~ net_use_preg, data = dat2, family = binomial)
summary(univariable_net_use_preg)

#hh_size
univariable_hh_size <- glm(y ~ hh_size, data = dat2, family = binomial)
summary(univariable_hh_size)

#ACT_use_u5
univariable_ACT_use_u5 <- glm(y ~ ACT_use_u5, data = dat2, family = binomial)
summary(univariable_ACT_use_u5)

#pop_den
univariable_pop_den <- glm(y ~ log_pop_den, data = dat2, family = binomial)
summary(univariable_pop_den)

#hh_average_age
univariable_average_age <- glm(y ~ hh_members_age, data = dat2, family = binomial)
summary(univariable_average_age)

#sex_f 
univariable_sex_f <- glm(y ~ sex_f, data = dat2, family = binomial)
summary(univariable_sex_f)

#humidindex
univariable_humidindex <- glm(y ~ humidindex, data = dat2, family = binomial)
summary(univariable_humidindex)

#annual_precipitation
univariable_prec <- glm(y ~ annual_precipitation, data = dat2, family = binomial)
summary(univariable_prec)


# Comparing fits estimates:
export_summs(univariable_wealth, univariable_edu, univariable_net_use_u5, univariable_net_use_preg, univariable_hh_size, 
             univariable_ACT_use_u5, univariable_pop_den, univariable_average_age, univariable_sex_f, univariable_humidindex, univariable_prec, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("% wealth", "% education", "% u5 net use", "% preg net use", "household size", "% ACT use u5", 
                                         "population density", "average household age", "% female", "humidity index", "annual precipitation"))


#multivariable model comparisons
model1 <- glm(y ~ edu_a + sex_f + wealth_2 + hh_size + log_pop_den  + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)
summary(model1)

model2 <- glm(y ~ edu_a + sex_f, data = dat2, binomial) 


delta_coef <- abs((coef(model2)-coef(model1)[1:3])/
                    coef(model1)[1:3]) 

round(delta_coef, 3)


lrtest(model1, model2)


#multivariable model comparisons adding wealth

model3 <- glm(y ~ edu_a + sex_f + wealth_2 , data = dat2, binomial) 


delta_coef_3 <- abs((coef(model3)-coef(model1)[1:4])/
                    coef(model1)[1:4]) 

round(delta_coef_3, 3)


lrtest(model1, model3)

#multivariable model comparisons adding hh_size 
model1a <- glm(y ~ edu_a + sex_f + hh_size + wealth_2 + log_pop_den  + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)
summary(model1a)

model4 <- glm(y ~ edu_a + sex_f + hh_size , data = dat2, binomial) 
summary(model4)

delta_coef_4 <- abs((coef(model4)-coef(model1a)[1:4])/
                      coef(model1a)[1:4]) 

round(delta_coef_4, 3)


lrtest(model1a, model4)

#multivariable model comparisons adding log_pop_den 
model1b <- glm(y ~ edu_a + sex_f + log_pop_den + hh_size + wealth_2  + hh_members_age
               + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)
summary(model1a)

model5 <- glm(y ~ edu_a + sex_f + log_pop_den, data = dat2, binomial) 
summary(model5)

delta_coef_5 <- abs((coef(model5)-coef(model1b)[1:4])/
                      coef(model1b)[1:4]) 

round(delta_coef_5, 3)


lrtest(model1b, model5)



#multivariable model comparisons adding hh_members_age
model1c <- glm(y ~ edu_a + sex_f + hh_members_age + log_pop_den + hh_size + wealth_2
               + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)
summary(model1a)

model6 <- glm(y ~ edu_a + sex_f + hh_members_age, data = dat2, binomial) 
summary(model5)

delta_coef_6 <- abs((coef(model6)-coef(model1c)[1:4])/
                      coef(model1c)[1:4]) 

round(delta_coef_6, 3)


lrtest(model1c, model6)

#multivariable model comparisons adding net_use_u5 
model1d <- glm(y ~ edu_a + sex_f + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               +  net_use_preg + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)
summary(model1d)

model7 <- glm(y ~ edu_a + sex_f + net_use_u5, data = dat2, binomial) 
summary(model7)

delta_coef_7 <- abs((coef(model7)-coef(model1d)[1:4])/
                      coef(model1d)[1:4]) 

round(delta_coef_7, 3)


lrtest(model1d, model7)

#multivariable model comparisons adding net_use_preg  
model1e <- glm(y ~ edu_a + sex_f + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)
summary(model1e)

model8 <- glm(y ~ edu_a + sex_f + net_use_preg, data = dat2, binomial) 
summary(model8)

delta_coef_8 <- abs((coef(model8)-coef(model1e)[1:4])/
                      coef(model1e)[1:4]) 

round(delta_coef_8, 3)


lrtest(model1e, model8)


#multivariable model comparisons adding net_use_preg  
model1e <- glm(y ~ edu_a + sex_f + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)
summary(model1e)

model8 <- glm(y ~ edu_a + sex_f + net_use_preg, data = dat2, binomial) 
summary(model8)

delta_coef_8 <- abs((coef(model8)-coef(model1e)[1:4])/
                      coef(model1e)[1:4]) 

round(delta_coef_8, 3)


lrtest(model1e, model8)

#multivariable model comparisons adding ACT_use_u5  
model1f <- glm(y ~ edu_a + sex_f + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + humidindex + annual_precipitation, data = dat2, binomial)

model9 <- glm(y ~ edu_a + sex_f + ACT_use_u5, data = dat2, binomial) 
summary(model9)

delta_coef_9 <- abs((coef(model9)-coef(model1f)[1:4])/
                      coef(model1f)[1:4]) 

round(delta_coef_9, 3)


lrtest(model1f, model9)

#multivariable model comparisons adding  humidindex
model1g <- glm(y ~ edu_a + sex_f + humidindex+ net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + annual_precipitation, data = dat2, binomial)

model10 <- glm(y ~ edu_a + sex_f +  humidindex, data = dat2, binomial) 
summary(model10)

delta_coef_10 <- abs((coef(model9)-coef(model1g)[1:4])/
                      coef(model1g)[1:4]) 

round(delta_coef_10, 3)


lrtest(model1g, model10)

#multivariable model comparisons adding  humidindex
model1h <- glm(y ~ edu_a + sex_f + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + annual_precipitation, data = dat2, binomial)

model11 <- glm(y ~ edu_a + sex_f +  humidindex, data = dat2, binomial) 
summary(model11)

delta_coef_11 <- abs((coef(model11)-coef(model1h)[1:4])/
                       coef(model1h)[1:4]) 

round(delta_coef_11, 3)


lrtest(model1h, model11)

#multivariable model comparisons adding  humidindex
model1i <- glm(y ~ edu_a + sex_f + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)

model12 <- glm(y ~ edu_a + sex_f + annual_precipitation, data = dat2, binomial) 
summary(model12)

delta_coef_12 <- abs((coef(model12)-coef(model1i)[1:4])/
                       coef(model1i)[1:4]) 

round(delta_coef_12, 3)


lrtest(model1i, model12)

anova(model1i, model12, test = "Chisq")


####################################################################
#checking for linearity assumption
par(mfrow = c(2,2))
pr<-(1/(1+exp(-z)))
scatter.smooth(dat2$y~dat2$edu_a)
