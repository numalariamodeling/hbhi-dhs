#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", "randomGLM", "ROCR", "caretEnsemble", "klaR", "naniar", "corrplot", 
       "lmtest", "Deducer", "lattice", "ResourceSelection")

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
                       "rainfal", "annual_precipitation", "l_pop_den")]


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
univariable_wealth <- glm(y ~ wealth_2, data = dat1, family = binomial)
summary(univariable_wealth)

#education 
univariable_edu <- glm(y ~ edu_a, data = dat1, family = binomial)
summary(univariable_edu)

#net_use_u5
univariable_net_use_u5 <- glm(y ~ net_use_u5, data = dat1, family = binomial)
summary(univariable_net_use_u5)

#net_use_preg
univariable_net_use_preg <- glm(y ~ net_use_preg, data = dat1, family = binomial)
summary(univariable_net_use_preg)

#hh_size
univariable_hh_size <- glm(y ~ hh_size, data = dat1, family = binomial)
summary(univariable_hh_size)

#ACT_use_u5
univariable_ACT_use_u5 <- glm(y ~ ACT_use_u5, data = dat1, family = binomial)
summary(univariable_ACT_use_u5)

#pop_den
univariable_pop_den <- glm(y ~ l_pop_den, data = dat1, family = binomial)
summary(univariable_pop_den)

#hh_average_age
univariable_average_age <- glm(y ~ hh_members_age, data = dat1, family = binomial)
summary(univariable_average_age)

#sex_f 
univariable_sex_f <- glm(y ~ sex_f, data = dat1, family = binomial)
summary(univariable_sex_f)

#humidindex
univariable_humidindex <- glm(y ~ humidindex, data = dat1, family = binomial)
summary(univariable_humidindex)

#annual_precipitation
univariable_prec <- glm(y ~ annual_precipitation, data = dat1, family = binomial)
summary(univariable_prec)


# Comparing fits estimates:
export_summs(univariable_edu, univariable_wealth, univariable_net_use_u5, univariable_net_use_preg, univariable_hh_size, 
             univariable_ACT_use_u5, univariable_pop_den, univariable_average_age, univariable_sex_f, univariable_humidindex, univariable_prec, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("% education", "% wealth", "% u5 net use", "% preg net use", "household size", "% ACT use u5", 
                                         "population density", "average household age", "% female", "humidity index", "annual precipitation"))

# Compare asymptotic distributions of coefficients:
plot_summs(univariable_edu, univariable_wealth, univariable_net_use_u5, univariable_net_use_preg, univariable_hh_size, 
           univariable_ACT_use_u5, univariable_pop_den, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("% of high education", "% high wealth quantile", "% u5 net use", "% preg net use", "household size", "% ACT use u5", 
                                                 "population density"))


# Compare asymptotic distributions of coefficients:
plot_summs(univariable_average_age, univariable_sex_f, univariable_humidindex, univariable_prec,  scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("average household age", "% female", "humidity index", "annual precipitation"))

########################multivariable model comparisons####################################
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
model1g <- glm(y ~ edu_a + sex_f +  humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + annual_precipitation, data = dat2, binomial)

model10 <- glm(y ~ edu_a + sex_f +  humidindex, data = dat2, binomial) 
summary(model10)

delta_coef_10 <- abs((coef(model10)-coef(model1g)[1:4])/
                       coef(model1g)[1:4]) 

round(delta_coef_10, 3)


lrtest(model1g, model10)

#multivariable model comparisons adding  annual_precipitation
model1h <- glm(y ~ edu_a + sex_f + annual_precipitation +  humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)

model11 <- glm(y ~ edu_a + sex_f + annual_precipitation, data = dat2, binomial) 
summary(model11)

delta_coef_11 <- abs((coef(model11)-coef(model1h)[1:4])/
                       coef(model1h)[1:4]) 

round(delta_coef_11, 3)


lrtest(model1h, model11)

anova(model1h, model11, test = "Chisq")


#Comparing fits estimates:
export_summs(model2, model3, model4, model5, model6,model7, model8, model9, model10, model11, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("significant model", "% wealth", "household size", "population density", "average household age",
                                         "% u5 net use", "% preg net use",  "% ACT use u5", "humidity index", "annual precipitation"))

# Compare asymptotic distributions of coefficients:
plot_summs(model2, model3, model4, model5, model6,model7, model8, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("% of high education included", "% high wealth quantile included", "% u5 net use included", "% preg net use included", "household size included", "% ACT use u5 included", 
                                                 "population density included"))


# Compare asymptotic distributions of coefficients:
plot_summs(model9, model10, model11,  scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("% ACT use u5 included", "humidity index included", "annual precipitation included"))
########################### adding more variable to the model that had at least 2% change in coeficients###########
#multivariable model comparisons





#multivariable model comparisons adding wealth
#new base model
model2_b <- glm(y ~ edu_a + sex_f + humidindex, data = dat2, binomial) 

model1i_a <- glm(y ~ edu_a + sex_f + humidindex + wealth_2 + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size, data = dat2, binomial)

model3_a <- glm(y ~ edu_a + sex_f + humidindex + wealth_2, data = dat2, binomial) 


delta_coef_3_a <- abs((coef(model3_a)-coef(model1i_a)[1:5])/
                            coef(model1i_a)[1:5]) 

round(delta_coef_3, 3)


lrtest(model1i, model3_a)

#multivariable model comparisons adding hh_size 
model1a_a <- glm(y ~ edu_a + sex_f + humidindex + hh_size + wealth_2 + log_pop_den  + hh_members_age
               + net_use_u5 + net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1a_a)

model4_a <- glm(y ~ edu_a + sex_f + humidindex + hh_size , data = dat2, binomial) 
summary(model4_a)

delta_coef_4_a <- abs((coef(model4_a)-coef(model1a_a)[1:5])/
                            coef(model1a_a)[1:5]) 

round(delta_coef_4_a, 3)


lrtest(model1a_a, model4_a)

#multivariable model comparisons adding log_pop_den 
model1b_a <- glm(y ~ edu_a + sex_f + humidindex + log_pop_den + hh_size + wealth_2  + hh_members_age
               + net_use_u5 + net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1b_a)

model5_a <- glm(y ~ edu_a + sex_f + humidindex + log_pop_den, data = dat2, binomial) 
summary(model5_a)

delta_coef_5_a <- abs((coef(model5_a)-coef(model1b_a)[1:5])/
                            coef(model1b_a)[1:5]) 

round(delta_coef_5_a, 3)


lrtest(model1b_a, model5_a)



#multivariable model comparisons adding hh_members_age
model1c_a <- glm(y ~ edu_a + sex_f + humidindex + hh_members_age + log_pop_den + hh_size + wealth_2
               + net_use_u5 + net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1a_a)

model6_a <- glm(y ~ edu_a + sex_f + humidindex + hh_members_age, data = dat2, binomial) 
summary(model5_a)

delta_coef_6_a <- abs((coef(model6_a)-coef(model1c_a)[1:5])/
                            coef(model1c_a)[1:5]) 

round(delta_coef_6_a, 3)


lrtest(model1c_a, model6_a)

#multivariable model comparisons adding net_use_u5 
model1d_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               +  net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1d_a)

model7_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_u5, data = dat2, binomial) 
summary(model7_a)

delta_coef_7_a <- abs((coef(model7_a)-coef(model1d_a)[1:5])/
                            coef(model1d_a)[1:5]) 

round(delta_coef_7_a, 3)


lrtest(model1d_a, model7_a)

#multivariable model comparisons adding net_use_preg  
model1e_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1e_a)

model8_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg, data = dat2, binomial) 
summary(model8)

delta_coef_8_a <- abs((coef(model8_a)-coef(model1e_a)[1:5])/
                            coef(model1e_a)[1:5]) 

round(delta_coef_8_a, 3)


lrtest(model1e_a, model8_a)


#multivariable model comparisons adding net_use_preg  
model1e_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1e_a)

model8_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg, data = dat2, binomial) 
summary(model8_a)

delta_coef_8_a <- abs((coef(model8_a)-coef(model1e_a)[1:5])/
                            coef(model1e_a)[1:5]) 

round(delta_coef_8_a, 3)


lrtest(model1e_a, model8_a)

#multivariable model comparisons adding ACT_use_u5  
model1f_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + annual_precipitation, data = dat2, binomial)

model9_a <- glm(y ~ edu_a + sex_f + humidindex + ACT_use_u5, data = dat2, binomial) 
summary(model9_a)

delta_coef_9_a <- abs((coef(model9_a)-coef(model1f_a)[1:5])/
                            coef(model1f_a)[1:5]) 

round(delta_coef_9_a, 3)


lrtest(model1f_a, model9_a)



#multivariable model comparisons adding interaction
model1g_a <- glm(y ~ edu_a + sex_f + humidindex + edu_a*sex_f + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + annual_precipitation, data = dat2, binomial)

model10_a <- glm(y ~ edu_a + sex_f + humidindex +  edu_a*sex_f, data = dat2, binomial) 
summary(model10_a)

delta_coef_10_a <- abs((coef(model10_a)-coef(model1g_a)[1:5])/
                             coef(model1g_a)[1:5]) 

round(delta_coef_10_a, 3)


lrtest(model1g_a, model10_a)

#multivariable model comparisons adding  annual_precipitation
model1h_a <- glm(y ~ edu_a + sex_f + humidindex + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)

model11_a <- glm(y ~ edu_a + sex_f + humidindex + annual_precipitation, data = dat2, binomial) 
summary(model11_a)

delta_coef_11_a <- abs((coef(model11_a)-coef(model1h_a)[1:5])/
                             coef(model1h_a)[1:5]) 

round(delta_coef_11, 3)


lrtest(model1h_a, model11_a)

anova(model1h_a, model11_a, test = "Chisq")



#Comparing fits estimates:
export_summs(model2_b, model3_a, model4_a, model5_a, model6_a, 
             model7_a, model8, model9_a, model10_a, model11_a, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("significant model with humidity index", "% wealth", "household size", "population density", "average household age",
                                         "% u5 net use", "% preg net use",  "% ACT use u5", "Edu:sex interaction", "annual precipitation"))

# Compare asymptotic distributions of coefficients:
plot_summs(model2_b, model3_a, model4_a, model5_a, model6_a, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("significant model with humidity index", "% wealth", "household size", "population density", "average household age"))

# Compare asymptotic distributions of coefficients:
plot_summs(model7_a, model8, model9_a, model11_a, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("% u5 net use", "% preg net use",  "% ACT use u5", "annual precipitation"))


#final model
model13 <- glm(y ~ edu_a + wealth_2 + sex_f + humidindex + hh_members_age + net_use_u5 + log_pop_den + ACT_use_u5 + 
               hh_size + log_pop_den + hh_size, data = dat2, binomial) 
summary(model13)


####################################################################
#checking for linearity assumption
# Select only numeric predictors

dat3 = dat2[,c("wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex")]

probabilities <- predict(model13, type = "response")

mydata <- dat3 %>%
        dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
        mutate(logit = log(probabilities/(1-probabilities))) %>%
        gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(logit, predictor.value))+
        geom_point(size = 0.5, alpha = 0.5) +
        geom_smooth(method = "loess") + 
        theme_bw() + 
        facet_wrap(~predictors, scales = "free")


####################################################################
#########Step four: interactions among covariates#########
model1i <- glm(y ~ edu_a + sex_f + edu_a*sex_f  + wealth_2 + annual_precipitation +  humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size, data = dat2, binomial)
summary(model1j)



delta_coef_12 <- abs((coef(model12)-coef(model1i)[1:4])/
                             coef(model1i)[1:4]) 

round(delta_coef_12, 3)


lrtest(model1i, model12)


##final model

#model13 <- glm(y ~ edu_a + sex_f + edu_a*sex_f +  humidindex, data = dat2, binomial) 
#summary(model13)

#####################################################
#####Step five: Assessing fit of the model###

hoslem.test(model13$y,fitted(model13))

#The P value > 0.5, indicates that there is no significant difference between observed and predicted value

Predprob<-predict(model13,type="response")
plot(Predprob,jitter(as.numeric(dat2$y),0.5),cex=0.5,ylab="Jittered malaraia prevalence outcome")


#histogram
#Histogram of estimated probability of death, stratified by observed outcome.
dat3 <-dat2 %>% mutate(predprobs = predict(model1,type="response"))
pred_0 <- dat3 %>% filter(y == 0)
pred_1 <- dat3 %>% filter(y == 1)
par(
        mfrow=c(1,2),
        mar=c(4,4,1,0)
)

hist(pred_0$predprobs, breaks=30 , xlim=c(0,1) , col=rgb(1,0,0,0.5) , xlab="Low prevalence predicted probabilities", ylab="Frequency" , main="" )
hist(pred_1$predprobs, breaks=30 , xlim=c(0,1) , col=rgb(0,0,1,0.5) , xlab="High prevalence predicted probabilities" , ylab="Frequency", main="")


hist(dat3$predprobs)


#roc
prob2a=predict(model13,type=c("response")) 
prob2a <- as.data.frame(prob2a)
dat2a <- dat2[,c("y", "edu_a", "pop_den", "sex_f")]
pred2a <- prediction(prob2a, dat2a$y)    
perf2a <- performance(pred2a, measure = "tpr", x.measure = "fpr")     
plot(perf2a, col="red", main="ROC curve High malaria prevalence", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #adds a 45 degree line



############fitstat##################
fitstats2(model2)

###################Residuals and regression diagnostics########

residualPlots(model2)

#
marginalModelPlots(model2)

#Outliers
outlierTest(model2)

#Leverage
influenceIndexPlot(model2)

#influence 
influencePlot(model2, col = "red",id.n=3)

#We can examine the change of coefficient by removing these influential observations.
model2351 <- update(model2,subset=c(-351))
compareCoefs(model2,model2351)
