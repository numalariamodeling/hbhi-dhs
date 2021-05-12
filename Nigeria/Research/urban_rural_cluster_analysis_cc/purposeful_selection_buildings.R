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

dat1 = urbandataset[,c("p_test", "build_count", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size", 
                       "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex", "rainfal", 
                       "annual_precipitation", "l_pop_den", "housing_qua", "net_access", "net_use_access", "net_use")]


# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)
table(dat1$y)




dat2 = dat1[,c("y", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size", "build_count",
               "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
               "rainfal", "annual_precipitation", "housing_qua", "net_access", "net_use_access", "net_use")]

dat2 <- dat2%>% mutate(annual_precipitation = scale(dat2$annual_precipitation, center = T))
dat2 <- dat2%>% mutate(build_count = scale(dat2$build_count, center = T))



dat2 <- dat2 %>% mutate(pop_den = na_if(pop_den, -9999))
dat2$annual_precipitation <- replace(dat2$annual_precipitation, which(dat2$annual_precipitation < 0), NA)
dat2 <- dat2 %>% mutate(log_pop_den = log(pop_den))
nearZeroVar(dat2)

# Create reduced dataset for model 2:
# (filter out rows with NA values)
dat2 <- na.omit(dat2)
table(dat2$y)


###correlation matrix
ggcorrplot(round(cor(dat2[ ,(colnames(dat2) 
                             %in% c("build_count", "pop_den", "wealth_2", "edu_a", "hh_size"))]), 1), 
           type = "lower", 
           lab = TRUE, 
           title = 'Correlation matrix between variables')

######################################################################
####Step one: univariable analysis#####
#wealth
univariable_wealth <- glm(y ~ wealth_2, data = dat1, family = binomial)
summary(univariable_wealth)

#education 
univariable_edu <- glm(y ~ edu_a, data = dat1, family = binomial)
summary(univariable_edu)

#net_use
univariable_net_use <- glm(y ~ net_use, data = dat1, family = binomial)
summary(univariable_net_use)

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


#build count
univariable_build <- glm(y ~ build_count, data = dat1, family = binomial)
summary(univariable_build)


# Compare asymptotic distributions of coefficients:
plot_summs(univariable_edu, univariable_wealth, univariable_net_use, univariable_hh_size, univariable_build,
           univariable_ACT_use_u5, univariable_pop_den, univariable_average_age, univariable_sex_f, univariable_humidindex, univariable_prec, scale = TRUE, colors = "Rainbow", plot.distributions = F, 
           inner_ci_level = .95, model.names = c())

########################multivariable model comparisons####################################


###############multivariable model with all variables ####################
model1 <- glm(y ~ edu_a + sex_f +  annual_precipitation +  net_use + wealth_2 + hh_size + log_pop_den  + hh_members_age
              + ACT_use_u5 + humidindex + build_count, data = dat2, binomial)

summary(model1)


##################significant model ##################333

model3 <- glm(y ~ edu_a + sex_f + annual_precipitation, data = dat2, binomial) 




#multivariable model comparisons adding net use

model1use <- glm(y ~ edu_a + sex_f + annual_precipitation + net_use, data = dat2, binomial) 

summary(model1use)


delta_coef_net_use <- abs((coef(model1use)-coef(model1)[1:5])/
                            coef(model1)[1:5]) 

delta_coef_net_use <- as.data.frame(round(delta_coef_net_use, 3))


#multivariable model comparisons adding wealth
model1w <- glm(y ~ edu_a + sex_f + annual_precipitation + wealth_2, data = dat2, binomial)


delta_coef_wealth <- abs((coef(model1w)-coef(model1)[1:5])/
                           coef(model1)[1:5]) 

delta_coef_wealth <- as.data.frame(round(delta_coef_wealth, 3))


#multivariable model comparisons adding hh_size 
model1hh_size <- glm(y ~ edu_a + sex_f + annual_precipitation + hh_size, data = dat2, binomial)


delta_coef_hh_size <- abs((coef(model1hh_size)-coef(model1)[1:5])/
                            coef(model1)[1:5]) 

delta_coef_hh_size <- as.data.frame(round(delta_coef_hh_size, 3))


#multivariable model comparisons adding log_pop_den 
model1pop <- glm(y ~ edu_a + sex_f + annual_precipitation + log_pop_den , data = dat2, binomial)



delta_coef_pop_den <- abs((coef(model1pop)-coef(model1)[1:5])/
                            coef(model1)[1:5]) 

delta_coef_pop_den <- as.data.frame(round(delta_coef_pop_den, 3))



#multivariable model comparisons adding hh_members_age
model1_hh_age <- glm(y ~ edu_a + sex_f + annual_precipitation + hh_members_age, data = dat2, binomial)


delta_coef_hh_age <- abs((coef(model1_hh_age)-coef(model1)[1:5])/
                           coef(model1)[1:5]) 

delta_coef_hh_age <- as.data.frame(round(delta_coef_hh_age, 3))


#multivariable model comparisons adding ACT_use_u5  
model1_act <- glm(y ~ edu_a + sex_f  + annual_precipitation + ACT_use_u5, data = dat2, binomial)


delta_coef_ACT <- abs((coef(model1_act)-coef(model1)[1:5])/
                        coef(model1)[1:5]) 

delta_coef_ACT <- as.data.frame(round(delta_coef_ACT, 3))


#multivariable model comparisons adding  humidindex
model1_humid <- glm(y ~ edu_a + sex_f + annual_precipitation +  humidindex, data = dat2, binomial)


delta_coef_humidindex <- abs((coef(model1_humid)-coef(model1)[1:5])/
                               coef(model1)[1:5]) 

delta_coef_humidindex <- as.data.frame(round(delta_coef_humidindex, 3))

#multivariable model comparisons adding build count
model1_build <- glm(y ~ edu_a + sex_f + annual_precipitation +  build_count, data = dat2, binomial)


delta_coef_build <- abs((coef(model1_build)-coef(model1)[1:5])/
                               coef(model1)[1:5]) 

delta_coef_build <- as.data.frame(round(delta_coef_build, 3))



#Creating dataframe for computed coeficient difference 
#Creating dataframe for computed coeficient difference 
delta_coef_df <- merge(delta_coef_pop_den, delta_coef_wealth, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_ACT, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_age, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_humidindex, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_net_use, by="row.names", all=TRUE)

delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_build, by="row.names", all=TRUE)


delta_coef_df <- na.omit(delta_coef_df)
delta_df <- delta_coef_df[c(2), ]
#delta_coef_df <- delta_coef_df[-c(1), ]

#plot of delta_coef
df1 <- melt(delta_df,"Row.names")

g1 <- ggplot(df1, aes(x = variable, y = value)) +
  geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  scale_y_continuous(breaks = seq(0, 0.55, by = 0.1), limits=c(0,0.55))+
  theme(axis.text.x = element_text(angle=-40, hjust=.1))


g1


############parsmonious model#######################
model_pars <- glm(y ~ edu_a + sex_f +  annual_precipitation +  net_use + wealth_2 + log_pop_den  + hh_members_age
                  + ACT_use_u5 + build_count, data = dat2, binomial)

summary(model_pars)


####################################################################
#checking for linearity assumption
# Select only numeric predictors

#replace model with model of interest
probabilities <- predict(model1, type = "response")

#dataframe for testing linearity 

dat3 <- dat2[,c("wealth_2", "edu_a", "build_count",
                "ACT_use_u5", "log_pop_den","hh_members_age", "sex_f", 
                "annual_precipitation", "net_use")]
  
  
#change data frame with respect to which model under test.
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



