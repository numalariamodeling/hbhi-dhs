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

dat1 = ruraldataset[,c("data_source","p_test", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
                       "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
                       "rainfal", "annual_precipitation", "l_pop_den", "housing_qua", "net_access", "net_use_access")]


# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)
table(dat1$y)


dat2 = dat1[,c("y", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
               "rainfal", "annual_precipitation", "housing_qua", "net_access", "net_use_access")]

dat2 <- dat2%>% mutate(annual_precipitation = scale(dat2$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

dat2 <- dat2 %>% mutate(pop_den = na_if(pop_den, -9999))
dat2$annual_precipitation <- replace(dat2$annual_precipitation, which(dat2$annual_precipitation < 0), NA)
dat2 <- dat2 %>% mutate(log_pop_den = log(pop_den))
nearZeroVar(dat2)

# Create reduced dataset for model 2:
# (filter out rows with NA values)
dat2 <- na.omit(dat2)
table(dat2$y)

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
           univariable_ACT_use_u5, univariable_pop_den, univariable_average_age, univariable_sex_f, univariable_humidindex, univariable_prec, scale = TRUE, colors = "Rainbow", plot.distributions = F, 
           inner_ci_level = .95, model.names = c())


########################multivariable model comparisons####################################
model1 <- glm(y ~ edu_a + sex_f +  annual_precipitation + wealth_2 + hh_size + log_pop_den  + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)

model1 <- glm(y ~ edu_a + sex_f +  annual_precipitation + wealth_2 + hh_size + log_pop_den  + hh_members_age
              + net_use_access + ACT_use_u5 + humidindex, data = dat2, binomial)

model1 <- glm(y ~ edu_a + sex_f +  annual_precipitation + wealth_2 + hh_size + log_pop_den  + hh_members_age
              + net_access + ACT_use_u5 + humidindex, data = dat2, binomial)

summary(model1)

model2 <- glm(y ~ edu_a + sex_f + annual_precipitation, data = dat2, binomial) 


delta_coef_significant <- abs((coef(model2)-coef(model1)[1:4])/
                    coef(model1)[1:4]) 

delta_coef_significant <- as.data.frame(round(delta_coef_significant, 3))



lrtest1 <- as.data.frame(lrtest(model1, model2))
lrtest1$model <- "sig_model"

#multivariable model comparisons adding wealth

model3 <- glm(y ~ edu_a + sex_f + + annual_precipitation + wealth_2 , data = dat2, binomial) 


delta_coef_wealth <- abs((coef(model3)-coef(model1)[1:5])/
                    coef(model1)[1:5]) 

delta_coef_wealth <- as.data.frame(round(delta_coef_wealth, 3))

lrtest3 <- as.data.frame(lrtest(model2, model3))
lrtest3$model <- "wealth_model"


#multivariable model comparisons adding hh_size 
model1a <- glm(y ~ edu_a + sex_f  + annual_precipitation + hh_size + wealth_2 + log_pop_den  + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)

model4 <- glm(y ~ edu_a + sex_f  + annual_precipitation + hh_size , data = dat2, binomial) 
summary(model4)

delta_coef_hh_size <- abs((coef(model4)-coef(model1a)[1:5])/
                      coef(model1a)[1:5]) 

delta_coef_hh_size <- as.data.frame(round(delta_coef_hh_size, 3))


lrtest4 <- as.data.frame(lrtest(model2, model4))
lrtest4$model <- "hh_size_model"

#multivariable model comparisons adding log_pop_den 
model1b <- glm(y ~ edu_a + sex_f + annual_precipitation + log_pop_den + hh_size + wealth_2  + hh_members_age
               + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)

model5 <- glm(y ~ edu_a + sex_f + annual_precipitation + log_pop_den, data = dat2, binomial) 
summary(model5)

delta_coef_pop_den <- abs((coef(model5)-coef(model1b)[1:5])/
                      coef(model1b)[1:5]) 

delta_coef_pop_den <- as.data.frame(round(delta_coef_pop_den, 3))


lrtest5 <- as.data.frame(lrtest(model2, model5))
lrtest5$model <- "pop_den_model"


#multivariable model comparisons adding hh_members_age
model1c <- glm(y ~ edu_a + sex_f + annual_precipitation + hh_members_age + log_pop_den + hh_size + wealth_2
               + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)

model6 <- glm(y ~ edu_a + sex_f + annual_precipitation + hh_members_age, data = dat2, binomial) 
summary(model5)

delta_coef_hh_age <- abs((coef(model6)-coef(model1c)[1:5])/
                      coef(model1c)[1:5]) 

delta_coef_hh_age <- as.data.frame(round(delta_coef_hh_age, 3))


lrtest6 <- as.data.frame(lrtest(model2, model6))
lrtest6$model <- "hh_age_model"

#multivariable model comparisons adding net_use_u5 
model1d <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               +  net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1d)

model7 <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use_u5, data = dat2, binomial) 
summary(model7)

delta_coef_net_u5 <- abs((coef(model7)-coef(model1d)[1:5])/
                      coef(model1d)[1:5]) 

delta_coef_net_u5 <- as.data.frame(round(delta_coef_net_u5, 3))


lrtest7 <- as.data.frame(lrtest(model2, model7))
lrtest7$model <- "u5_net_model"

#multivariable model comparisons adding net_use_preg  
model1e <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1e)

model8 <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use_preg, data = dat2, binomial) 
summary(model8)

delta_coef_net_preg <- abs((coef(model8)-coef(model1e)[1:5])/
                      coef(model1e)[1:5]) 

delta_coef_net_preg <- as.data.frame(round(delta_coef_net_preg, 3))


lrtest8 <- as.data.frame(lrtest(model2, model8))
lrtest8$model <- "preg_net_gmodel"


#multivariable model comparisons adding ACT_use_u5  
model1f <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + humidindex, data = dat2, binomial)

model9 <- glm(y ~ edu_a + sex_f  + annual_precipitation + ACT_use_u5, data = dat2, binomial) 
summary(model9)

delta_coef_ACT <- abs((coef(model9)-coef(model1f)[1:5])/
                      coef(model1f)[1:5]) 

delta_coef_ACT <- as.data.frame(round(delta_coef_ACT, 3))


lrtest9 <- as.data.frame(lrtest(model2, model9))
lrtest9$model <- "ACT_model"


#multivariable model comparisons adding  humidindex
model1g <- glm(y ~ edu_a + sex_f + annual_precipitation +  humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)

model10 <- glm(y ~ edu_a + sex_f + annual_precipitation +  humidindex, data = dat2, binomial) 
summary(model10)

delta_coef_humidindex <- abs((coef(model10)-coef(model1g)[1:5])/
                       coef(model1g)[1:5]) 

delta_coef_humidindex <- as.data.frame(round(delta_coef_humidindex, 3))


lrtest10 <- as.data.frame(lrtest(model2, model10))
lrtest10$model <- "humidindex_model10"

#multivariable model comparisons adding  hhousing q
model1h <- glm(y ~ edu_a + sex_f + annual_precipitation +  housing_qua + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)

model11 <- glm(y ~ edu_a + sex_f + annual_precipitation +  housing_qua, data = dat2, binomial) 
summary(model11)

delta_coef_housing_qua <- abs((coef(model11)-coef(model1h)[1:5])/
                                     coef(model1h)[1:5]) 

delta_coef_housing_qua <- as.data.frame(round(delta_coef_housing_qua, 3))


lrtest11 <- as.data.frame(lrtest(model2, model10))
lrtest10$model <- "housing_model11"

#Comparing fits estimates:
export_summs(model2, model3, model4, model5, model6,model7, model8, model9, model10, model11, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("significant model", "% wealth", "household size", "population density", "average household age",
                                         "% u5 net use", "% preg net use",  "% ACT use u5", "humidity index", "annual precipitation"))

# Compare asymptotic distributions of coefficients:
plot_summs(model2, model3, model4, model5, model6,model7, model8, model9, model10, model11, scale = TRUE, colors = "Rainbow", plot.distributions = F, 
           inner_ci_level = .95, model.names = c("significant model", "% wealth", "household size", "population density", "average household age",
                                                 "% u5 net use", "% preg net use",  "% ACT use u5", "humidity index", "annual precipitation"))

# Compare asymptotic distributions of coefficients:
plot_summs(model1, scale = TRUE, colors = "Rainbow", plot.distributions = F, 
           inner_ci_level = .95, model.names = c( ))


export_summs(model1, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c())

#Creating dataframe for computed coeficient difference 
delta_coef_df <- merge(delta_coef_pop_den, delta_coef_wealth, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_size, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_ACT, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_age, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_humidindex, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_net_preg, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_net_u5, by="row.names", all=TRUE)
#delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
#delta_coef_df <- merge(delta_coef_df,delta_coef_housing_qua, by="row.names", all=TRUE)
#delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- na.omit(delta_coef_df)
delta_coef_df <- delta_coef_df[c(4), ]
#delta_coef_df <- delta_coef_df[-c(1), ]

#plot of delta_coef
df1 <- melt(delta_coef_df,"Row.names")

g1 <- ggplot(df1, aes(x = variable, y = value)) +
        geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
        theme_bw()+ 
        scale_y_continuous(breaks = seq(0, 0.14, by = 0.02), limits=c(0,0.13))+
        theme(axis.text.x = element_text(angle=-40, hjust=.1))


g1





#Creating dataframe for computed likelihod ratio
lrtest_df <- dplyr::bind_rows(lrtest1, lrtest3, lrtest4, lrtest5, lrtest6, lrtest7, lrtest8, lrtest9, lrtest10)
write.csv(lrtest_df, "lrtest.csv")
######################## adding more variable to the model that had at least 2% change in coeficients###########
#multivariable model comparisons


########parsimonious model
model1i_parsi <- glm(y ~ edu_a + sex_f + annual_precipitation + net_use_preg + 
                         net_use_u5 + hh_members_age + log_pop_den + hh_size, data = dat2, binomial)


mm <- glm(y ~edu_a + sex_f + annual_precipitation, data = dat2, binomial)
summary(mm)

cofff <- as.data.frame(coef(model1i_parsi))
coeff5 <- cofff + (0.05/cofff)
coeff1 <- cofff + (0.1/cofff) 
coeff15 <- cofff + (0.15/cofff)
coeff20 <- cofff + (0.20/cofff)
coeff25 <- cofff + (0.25/cofff)
coeff30 <- cofff + (0.3/cofff)


or <- as.data.frame(exp(coef(model1i_parsi)))
or5 <- exp(coeff5)
or1 <- exp(coeff1)
or15 <- exp(coeff15)
or20 <- exp(coeff20)
or25 <- exp(coeff25)
or30 <- exp(coeff30)

or_df <- merge(or, or5, by="row.names", all=TRUE)
or_df <- or_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
or_df <- merge(or_df, or1, by="row.names", all=TRUE)
or_df <- or_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
or_df <- merge(or_df, or15, by="row.names", all=TRUE)
or_df <- or_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
or_df <- merge(or_df, or20, by="row.names", all=TRUE)
or_df <- or_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
or_df <- merge(or_df, or25, by="row.names", all=TRUE)
or_df <- or_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
or_df <- merge(or_df, or30, by="row.names", all=TRUE)
or_df <- or_df %>% remove_rownames %>% column_to_rownames(var="Row.names")



summary(model1i_parsi)
pass_coefs <- as.data.frame(coef(model1i_parsi))
pass_coefs <- as.data.frame(SE(model1i_parsi))
pass_odds <- as.data.frame(exp(coef(model1i_parsi)))


#comparing model with all variables and parsimonious model 
lrtest(model1h, model1i_parsi)

ddd <- export_summs(model1a, model1i_parsi, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("Model with all variables", "Parsimonious model"))


write.csv((as.data.frame(ddd)),"comparing_parsimonious.csv")
#multivariable model comparisons adding wealth
#new base model
model2_b <- glm(y ~ edu_a + sex_f + humidindex, data = dat2, binomial) 

model1i_a <- glm(y ~ edu_a + sex_f + humidindex + wealth_2 + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size, data = dat2, binomial)

model3_a <- glm(y ~ edu_a + sex_f + humidindex + wealth_2, data = dat2, binomial) 


delta_coef_wealth_a <- abs((coef(model3_a)-coef(model1i_a)[1:5])/
                            coef(model1i_a)[1:5]) 

delta_coef_wealth_a <- as.data.frame(round(delta_coef_wealth_a, 3))


lrtest1_a <- as.data.frame(lrtest(model2_b, model3_a))
lrtest1_a$model <- "precipi_model"

#multivariable model comparisons adding hh_size 
model1a_a <- glm(y ~ edu_a + sex_f + humidindex + hh_size + wealth_2 + log_pop_den  + hh_members_age
               + net_use_u5 + net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1a_a)

model4_a <- glm(y ~ edu_a + sex_f + humidindex + hh_size , data = dat2, binomial) 
summary(model4_a)

delta_coef_hhsize_a <- abs((coef(model4_a)-coef(model1a_a)[1:5])/
                            coef(model1a_a)[1:5]) 

delta_coef_humid_a <- as.data.frame(round(delta_coef_hhsize_a, 3))


lrtest(model2_b, model4_a)


#multivariable model comparisons adding log_pop_den 
model1b_a <- glm(y ~ edu_a + sex_f + humidindex + log_pop_den + hh_size + wealth_2  + hh_members_age
               + net_use_u5 + net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1b_a)

model5_a <- glm(y ~ edu_a + sex_f + humidindex + log_pop_den, data = dat2, binomial) 
summary(model5_a)

delta_coef_popden_a <- abs((coef(model5_a)-coef(model1b_a)[1:5])/
                            coef(model1b_a)[1:5]) 

delta_coef_popden_a <- as.data.frame(round(delta_coef_popden_a, 3))


lrtest(model1b_a, model5_a)



#multivariable model comparisons adding hh_members_age
model1c_a <- glm(y ~ edu_a + sex_f + humidindex + hh_members_age + log_pop_den + hh_size + wealth_2
               + net_use_u5 + net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1a_a)

model6_a <- glm(y ~ edu_a + sex_f + humidindex + hh_members_age, data = dat2, binomial) 
summary(model5_a)

delta_coef_hhage_a <- abs((coef(model6_a)-coef(model1c_a)[1:5])/
                            coef(model1c_a)[1:5]) 

delta_coef_hhage_a<- as.data.frame(round(delta_coef_hhage_a, 3))


lrtest(model1c_a, model6_a)

#multivariable model comparisons adding net_use_u5 
model1d_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               +  net_use_preg + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1d_a)

model7_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_u5, data = dat2, binomial) 
summary(model7_a)

delta_coef_u5net_a <- abs((coef(model7_a)-coef(model1d_a)[1:5])/
                            coef(model1d_a)[1:5]) 

delta_coef_u5net_a <- as.data.frame(round(delta_coef_u5net_a, 3))


lrtest(model1d_a, model7_a)

#multivariable model comparisons adding net_use_preg  
model1e_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + annual_precipitation, data = dat2, binomial)
summary(model1e_a)

model8_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg, data = dat2, binomial) 
summary(model8)

delta_coef_pregnet_a <- abs((coef(model8_a)-coef(model1e_a)[1:5])/
                            coef(model1e_a)[1:5]) 

delta_coef_pregnet_a <- as.data.frame(round(delta_coef_pregnet_a, 3))


lrtest(model1e_a, model8_a)

#multivariable model comparisons adding ACT_use_u5  
model1f_a <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + annual_precipitation, data = dat2, binomial)

model9_a <- glm(y ~ edu_a + sex_f + humidindex + ACT_use_u5, data = dat2, binomial) 
summary(model9_a)

delta_coef_ACT_a <- abs((coef(model9_a)-coef(model1f_a)[1:5])/
                            coef(model1f_a)[1:5]) 

delta_coef_ACT_a <- as.data.frame(round(delta_coef_ACT_a, 3))


lrtest(model1f_a, model9_a)


#multivariable model comparisons adding  annual_precipitation
model1h_a <- glm(y ~ edu_a + sex_f + humidindex + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)

model10_a <- glm(y ~ edu_a + sex_f + humidindex + annual_precipitation, data = dat2, binomial) 
summary(model11_a)

delta_coef_precip_a <- abs((coef(model10_a)-coef(model1h_a)[1:5])/
                             coef(model1h_a)[1:5]) 

delta_coef_precip_a <- as.data.frame(round(delta_coef_precip_a, 3))


lrtest(model1h_a, model10_a)

anova(model1h_a, model11_a, test = "Chisq")

#multivariable model comparisons adding housing quality
model1i_a <- glm(y ~ edu_a + sex_f + humidindex + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)

model11_a <- glm(y ~ edu_a + sex_f + humidindex + housing_qua, data = dat2, binomial) 
summary(model11_a)

delta_coef_housq_a  <- abs((coef(model11_a)-coef(model1i_a)[1:5])/
                                   coef(model1i_a)[1:5]) 

delta_coef_housq_a <- as.data.frame(round(delta_coef_housq_a, 3))


lrtest(model1i_a, model10_a)

#Comparing fits estimates:
export_summs(model2_b, model3_a, model4_a, model5_a, model6_a, 
             model7_a, model8, model9_a, model10_a, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("significant model with humidity index", "% wealth", "household size", "population density", "average household age",
                                         "% u5 net use", "% preg net use",  "% ACT use u5", "annual precipitation"))

# Compare asymptotic distributions of coefficients:
plot_summs(model2_b, model3_a, model4_a, model5_a, model6_a, model7_a, model8, model9_a, model10_a, scale = TRUE, colors = "Rainbow", plot.distributions = F, 
           inner_ci_level = .95, model.names = c())


#Creating dataframe for computed coeficient difference 
delta_coef_df_a <- merge(delta_coef_u5net_a, delta_coef_wealth_a, by="row.names", all=TRUE)
delta_coef_df_a <- delta_coef_df_a %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df_a <- merge(delta_coef_df_a,delta_coef_ACT_a, by="row.names", all=TRUE)
delta_coef_df_a <- delta_coef_df_a %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df_a <- merge(delta_coef_df_a,delta_coef_hhage_a, by="row.names", all=TRUE)
delta_coef_df_a <- delta_coef_df_a %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df_a <- merge(delta_coef_df_a,delta_coef_humid_a, by="row.names", all=TRUE)
delta_coef_df_a <- delta_coef_df_a %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df_a <- merge(delta_coef_df_a,delta_coef_popden_a, by="row.names", all=TRUE)
delta_coef_df_a <- delta_coef_df_a %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df_a <- merge(delta_coef_df_a,delta_coef_pregnet_a, by="row.names", all=TRUE)
delta_coef_df_a <- delta_coef_df_a %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df_a <- merge(delta_coef_df_a,delta_coef_precip_a, by="row.names", all=TRUE)
#delta_coef_df_a <- delta_coef_df_a %>% remove_rownames %>% column_to_rownames(var="Row.names")


delta_coef_df_a <- na.omit(delta_coef_df_a)
delta_coef_df_a <- delta_coef_df_a[c(4), ]

#plot of delta_coef
df1 <- melt(delta_coef_df_a,"Row.names")

p1 <- ggplot(df1, aes(x = variable, y = value)) +
        geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
        theme_bw()+ 
        theme(axis.text.x = element_text(angle=-40, hjust=.1))


p1


#Creating dataframe for computed likelihod ratio
lrtest_df <- dplyr::bind_rows(lrtest1_a, lrtest3_a, lrtest4_a, lrtes5_a, lrtest6_a, lrtest7_a, lrtest8_a, lrtest9_a, lrtest10_a, lrtest11_a)
write.csv(lrtest_df, "lrtest.csv")


#final model
model13 <- glm(y ~ edu_a + sex_f + humidindex + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + annual_precipitation, data = dat2, binomial) 
summary(model13)


####################################################################
#checking for linearity assumption
# Select only numeric predictors

dat3 = dat2[,c("edu_a", "net_use_u5", "net_use_preg","log_pop_den", "hh_members_age", "sex_f", "annual_precipitation")]

probabilities <- predict(model1i_parsi, type = "response")

mydata <- dat3 %>%
        dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
mydata <- mydata %>%
        mutate(logit = log(probabilities/(1-probabilities))) %>%
        gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(predictor.value, logit))+
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
