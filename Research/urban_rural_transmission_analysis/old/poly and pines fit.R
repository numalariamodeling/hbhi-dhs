#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", "randomGLM", "ROCR", "caretEnsemble", "klaR", "naniar", "corrplot", 
       "lmtest", "Deducer", "lattice", "ResourceSelection", "splines", "mgcv", "rms)")

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
                       "rainfal", "annual_precipitation", "l_pop_den", "housing_qua")]


# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)



dat2 = dat1[,c("y", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
               "rainfal", "annual_precipitation", "housing_qua")]

dat2 <- dat2%>% mutate(annual_precipitation = scale(dat2$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

dat2 <- dat2 %>% mutate(pop_den = na_if(pop_den, -9999))
dat2 <- dat2 %>% mutate(log_pop_den = log(pop_den))
nearZeroVar(dat2)

dat2 <- na.omit(dat2)
#train and test data
training.samples <- dat2$y %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- dat2[training.samples, ]
test.data <- dat2[-training.samples, ]


#plas model
model13 <- glm(y ~ edu_a + wealth_2 + sex_f + humidindex + hh_members_age + net_use_u5 + log_pop_den + ACT_use_u5 + 
                 hh_size + log_pop_den + hh_size, data = dat2, binomial) 
summary(model13)



model_hq <- glm(y ~ edu_a + housing_qua + sex_f + humidindex + hh_members_age + net_use_u5 + log_pop_den + ACT_use_u5 + 
                 hh_size + log_pop_den + hh_size, data = dat2, binomial) 
summary(model_hq)


#poly models
model_p <- glm(y ~ edu_a + wealth_2 + poly(sex_f, 2, raw = TRUE) + humidindex + poly(hh_members_age, 3, raw = TRUE) + net_use_u5 + log_pop_den + ACT_use_u5 + 
                 log_pop_den + poly(hh_size, 2, raw = TRUE), data = train.data, binomial) 

summary(model_p)



model_p_hq <- glm(y ~ edu_a + wealth_2 + poly(sex_f, 2, raw = TRUE) + humidindex + poly(hh_members_age, 3, raw = TRUE) + net_use_u5 + log_pop_den + ACT_use_u5 + 
                 log_pop_den + poly(hh_size, 2, raw = TRUE), data = dat2, binomial) 

summary(model_p_hq)


# spines model
knots <- quantile(train.data$hh_members_age, p = c(0.25, 0.50))
model_s <- glm (y ~ bs(hh_members_age, knots = knots) + edu_a + sex_f + humidindex + wealth_2 + net_use_u5 + log_pop_den + ACT_use_u5 + 
                  hh_size + log_pop_den + hh_size, data = dat2, binomial)

summary(model_s)


knots <- quantile(train.data$hh_members_age, p = c(0.25, 0.50))
model_s_hq <- glm (y ~ bs(hh_members_age, knots = knots) + edu_a + sex_f + humidindex + housing_qua + net_use_u5 + log_pop_den + ACT_use_u5 + 
                  hh_size + log_pop_den + hh_size, data = dat2, binomial)

summary(model_s_hq)

#rcs model
model_rcs <-lrm(formula = y ~ edu_a + net_use_u5 + wealth_2 +  net_use_preg + 
                  ACT_use_u5 + sex_f + humidindex + rcs(log_pop_den) + rcs(hh_members_age) 
                + rcs(hh_size), data = dat2, x=TRUE, y=TRUE)

plot(calibrate(model_rcs, B=1000), main="model_rcs")


model_rcs <-glm(formula = y ~ edu_a + net_use_u5 + wealth_2 +  net_use_preg + ACT_use_u5 + 
                  sex_f + humidindex + rcs(log_pop_den) + rcs(hh_members_age) + rcs(hh_size),
                data = dat2, x=TRUE, y=TRUE)

summary(model_rcs)


model_rcs_hq <-lrm(formula = y ~ edu_a + net_use_u5 + housing_qua +  net_use_preg + ACT_use_u5 + 
                     sex_f + humidindex + rcs(log_pop_den) + rcs(hh_members_age) + rcs(hh_size),
                   data = dat2, x=TRUE, y=TRUE)


plot(calibrate(model_rcs_hq, B=1000), main="model_rcs_hq")


model_rcs_hq <-glm(formula = y ~ edu_a + net_use_u5 + housing_qua +  net_use_preg + ACT_use_u5 + 
                  sex_f + humidindex + rcs(log_pop_den) + rcs(hh_members_age) + rcs(hh_size),
                data = dat2, x=TRUE, y=TRUE)

summary(model_rcs_hq)


# Build the model
model_mgcv <- gam(y ~ edu_a + net_use_u5 + wealth_2 +  net_use_preg + ACT_use_u5 + 
                    sex_f + humidindex + s(log_pop_den) + s(hh_members_age) + s(hh_size), family=binomial, data = dat2)

summary(model_mgcv)

model_mgcv_hq <- gam(y ~ edu_a + net_use_u5 + housing_qua +  net_use_preg + ACT_use_u5 + 
                       sex_f + humidindex + s(log_pop_den) + s(hh_members_age) + s(hh_size), family=binomial, data = dat2)



rcspline.plot(dat2$hh_members_age,dat2$y,model="logistic")

summary(model_mgcv)


lrtest(model13, model_hq)
lrtest(model13, model_p)
lrtest(model13, model_p_hq)
lrtest(model13, model_rcs)
lrtest(model13, model_rcs_hq)
lrtest(model13, model_s)
lrtest(model13, model_s_hq)
lrtest(model13, model_mgcv)
lrtest(model13, model_mgcv_hq)
lrtest(model13, model_mgcv)

export_summs(model13, model_hq, model_p, model_p_hq, model_rcs, model_rcs_hq, model_s, model_s_hq, model_mgcv, model_mgcv_hq, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("% education", "% wealth", "% u5 net use", "% preg net use", "household size", "% ACT use u5", 
                                         "population density", "average household age", "% female", "humidity index"))

predictions <- model_p %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$y),
  R2 = R2(predictions, test.data$y)
)

dat3 = test.data[,c("wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "log_pop_den","hh_members_age", "sex_f", "humidindex", "housing_qua")]

predictions <- model_p %>% predict(dat2)

mydata <- dat2 %>%
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





dat3 = dat2[,c("wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "log_pop_den","hh_members_age", "sex_f", "humidindex", "housing_qua")]

probabilities <- predict(model_s, type = "response")

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



