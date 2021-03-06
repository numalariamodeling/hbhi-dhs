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
                       "rainfal", "annual_precipitation", "l_pop_den", "housing_qua", "net_access", "net_use_access", "net_use")]


# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)
table(dat1$y)




dat2 = dat1[,c("y", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
               "rainfal", "annual_precipitation", "housing_qua", "net_access", "net_use_access", "net_use")]

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


#net use 
univariable_net_use <- glm(y ~ net_use, data = dat1, family = binomial)
summary(univariable_net_access)


# Compare asymptotic distributions of coefficients:
plot_summs(univariable_edu, univariable_wealth, univariable_net_use, univariable_hh_size, 
           univariable_ACT_use_u5, univariable_pop_den, univariable_average_age, univariable_sex_f, univariable_humidindex, univariable_prec, scale = TRUE, colors = "Rainbow", plot.distributions = F, 
           inner_ci_level = .95, model.names = c())

########################multivariable model comparisons####################################


#multivariable model comparisons adding net access and use
model1 <- glm(y ~ edu_a + sex_f +  annual_precipitation +  net_use_access + wealth_2 + hh_size + log_pop_den  + hh_members_age
                + ACT_use_u5 + humidindex, data = dat2, binomial)

summary(model1)

model3 <- glm(y ~ edu_a + sex_f + annual_precipitation, data = dat2, binomial) 


delta_coef_net_acuse <- abs((coef(model3)-coef(model1)[1:5])/
                           coef(model1)[1:5]) 

delta_coef_net_acuse <- as.data.frame(round(delta_coef_net_acuse, 3))



#multivariable model comparisons adding net access 


model1na <- glm(y ~ edu_a + sex_f +  annual_precipitation +  net_access + wealth_2 + hh_size + log_pop_den  + hh_members_age
              + ACT_use_u5 + humidindex, data = dat2, binomial)

summary(modelna)


export_summs(model1, model1na, scale = F, error_format = "[{conf.low}, {conf.high}]", digits = 3, model.names = c("Model with net access/use", "Model with net access"))


delta_coef_net_access <- abs((coef(model3)-coef(model1na)[1:5])/
                           coef(model1na)[1:5]) 

delta_coef_net_access <- as.data.frame(round(delta_coef_net_access, 3))


#multivariable model comparisons adding net use


model1use <- glm(y ~ edu_a + sex_f +  annual_precipitation +  net_use + wealth_2 + hh_size + log_pop_den  + hh_members_age
                + ACT_use_u5 + humidindex, data = dat2, binomial)

summary(model1use)


delta_coef_use <- abs((coef(model3)-coef(model1use)[1:4])/
                               coef(model1use)[1:4]) 

delta_coef_use <- as.data.frame(round(delta_coef_use, 3))


###################################### adding 
#multivariable model comparisons adding wealth
model1w <- glm(y ~ edu_a + sex_f  + annual_precipitation + wealth_2 + hh_size + log_pop_den  + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)


delta_coef_wealth <- abs((coef(model3)-coef(model1w)[1:4])/
                           coef(model1w)[1:4]) 

delta_coef_wealth <- as.data.frame(round(delta_coef_wealth, 3))


#multivariable model comparisons adding hh_size 
model1a <- glm(y ~ edu_a + sex_f  + annual_precipitation + hh_size + wealth_2 + log_pop_den  + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)


delta_coef_hh_size <- abs((coef(model3)-coef(model1a)[1:4])/
                            coef(model1a)[1:4]) 

delta_coef_hh_size <- as.data.frame(round(delta_coef_hh_size, 3))


#multivariable model comparisons adding log_pop_den 
model1b <- glm(y ~ edu_a + sex_f + annual_precipitation + log_pop_den + hh_size + wealth_2  + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)


delta_coef_pop_den <- abs((coef(model3)-coef(model1b)[1:4])/
                            coef(model1b)[1:4]) 

delta_coef_pop_den <- as.data.frame(round(delta_coef_pop_den, 3))



#multivariable model comparisons adding hh_members_age
model1c <- glm(y ~ edu_a + sex_f + annual_precipitation + hh_members_age + log_pop_den + hh_size + wealth_2
               + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)


delta_coef_hh_age <- abs((coef(model3)-coef(model1c)[1:4])/
                           coef(model1c)[1:4]) 

delta_coef_hh_age <- as.data.frame(round(delta_coef_hh_age, 3))


#multivariable model comparisons adding net_use_u5 
model1d <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1d)

delta_coef_net_u5 <- abs((coef(model3)-coef(model1d)[1:4])/
                           coef(model1d)[1:4]) 

delta_coef_net_u5 <- as.data.frame(round(delta_coef_net_u5, 3))


#multivariable model comparisons adding net_use_preg  
model1e <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use + hh_members_age + log_pop_den + hh_size + wealth_2
               + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1e)


delta_coef_net_preg <- abs((coef(model3)-coef(model1e)[1:4])/
                             coef(model1e)[1:4]) 

delta_coef_net_preg <- as.data.frame(round(delta_coef_net_preg, 3))


#multivariable model comparisons adding ACT_use_u5  
model1f <- glm(y ~ edu_a + sex_f  + annual_precipitation + net_use + hh_members_age + log_pop_den + hh_size + wealth_2
               + humidindex, data = dat2, binomial)


delta_coef_ACT <- abs((coef(model3)-coef(model1f)[1:4])/
                        coef(model1f)[1:4]) 

delta_coef_ACT <- as.data.frame(round(delta_coef_ACT, 3))


#multivariable model comparisons adding  humidindex
model1g <- glm(y ~ edu_a + sex_f + annual_precipitation +  humidindex + 
                 net_use + hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)


delta_coef_humidindex <- abs((coef(model3)-coef(model1g)[1:4])/
                               coef(model1g)[1:4]) 

#multivariable model comparisons adding  net use
model1g <- glm(y ~ edu_a + sex_f + annual_precipitation +  net_use + humidindex + 
                 hh_members_age + log_pop_den + hh_size + wealth_2, data = dat2, binomial)


delta_coef_net_use <- abs((coef(model3)-coef(model1g)[1:4])/
                               coef(model1g)[1:4]) 


delta_coef_net_use <- as.data.frame(round(delta_coef_net_use, 3))


export_summs(model1, model1na, model1use, scale = F, error_format = "[{conf.low}, {conf.high}]", digits = 3, model.names = c("Model with net access/use", "Model with net access", "model with net use"))



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
delta_coef_df <- merge(delta_coef_df,delta_coef_net_use, by="row.names", all=TRUE)

delta_coef_df <- na.omit(delta_coef_df)
delta_df <- delta_coef_df[c(2), ]
#delta_coef_df <- delta_coef_df[-c(1), ]

#plot of delta_coef
df1 <- melt(delta_df,"Row.names")

g1 <- ggplot(df1, aes(x = variable, y = value)) +
  geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  scale_y_continuous(breaks = seq(0, 0.48, by = 0.1), limits=c(0,0.5))+
  theme(axis.text.x = element_text(angle=-40, hjust=.1))


g1





####################################################################
#checking for linearity assumption
# Select only numeric predictors
dat_acuse = dat2[,c("edu_a", "log_pop_den", "hh_members_age", "sex_f", 
                    "annual_precipitation", "net_use_access", "wealth_2", "ACT_use_u5", "humidindex")]

dat_use = dat2[,c("edu_a", "log_pop_den", "hh_members_age", "sex_f", 
                  "annual_precipitation", "net_use",  "wealth_2", "ACT_use_u5", "humidindex")]

dat_na = dat2[,c("edu_a", "log_pop_den", "hh_members_age", "sex_f",
                 "annual_precipitation", "net_access",  "wealth_2", "ACT_use_u5", "humidindex")]


#replace model with model of interest
probabilities <- predict(model1use, type = "response")


#change data frame with respect to which model under test.
mydata <- dat_use %>%
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

