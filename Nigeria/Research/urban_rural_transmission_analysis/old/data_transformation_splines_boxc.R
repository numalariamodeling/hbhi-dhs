#Reading in the necessary packages 
x <- c("tidyverse", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", "randomGLM", "ROCR", "caretEnsemble", "klaR", "naniar", "corrplot", 
       "lmtest", "Deducer", "lattice", "ResourceSelection", "splines", "mgcv", "rms", "drc")

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
                       "ACT_use_u5", "pop_den","l_pop_den","hh_members_age", "sex_f", "humidindex",
                       "rainfal", "annual_precipitation", "l_pop_den", "housing_qua")]


# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)



dat2 = dat1[,c("y", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "pop_den","l_pop_den","hh_members_age", "sex_f", "humidindex",
               "rainfal", "annual_precipitation", "housing_qua")]



dat2 <- dat2 %>% mutate(pop_den = na_if(pop_den, -9999))
dat2 <- dat2 %>% mutate(log_pop_den = log(pop_den))
nearZeroVar(dat2)

# Create reduced dataset for model 2:
# (filter out rows with NA values)
dat2 <- na.omit(dat2)
table(dat2$p_level)

########### The restricted cubic spline ###################################

rcspline.plot(dat2$hh_members_age,dat2$y,model="logistic")

knots <- quantile(dat2$hh_members_age, p = c(0.10, 0.23, 0.30, 0.47, 0.77))
model_s_n <- glm (y ~ bs(hh_members_age, knots = knots) + edu_a + wealth_2 + sex_f + humidindex + net_use_u5 + log_pop_den + ACT_use_u5 + 
                    hh_size + log_pop_den + hh_size, data = dat2, binomial)

probabilities <- predict(model_s_n, type = "response")

dat3 = dat2[("hh_members_age")]

mydata <- dat3  
predictors <- colnames(mydata)# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(predictor.value, logit))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free")


############# rcs########

dat3 = dat2[("hh_members_age")]

mydata <- dat3  
predictors <- colnames(mydata)# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(predictor.value, logit))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free")



idx_model <- glm(y ~ rcs(hh_members_age, 3) + ACT_use_u5 + edu_a + humidindex 
                 + net_use_u5 + net_use_preg, data=dat2, x=TRUE, y=TRUE)

idx_model <- glm(y ~ rcs(hh_members_age, 3) + ACT_use_u5 + edu_a + humidindex 
                 + net_use_u5 + net_use_preg, data=dat2, x=TRUE, y=TRUE)

anova(idx_model, ss=FALSE)



########## BoxCox ###########################################################
idx_model <- glm(y ~ hh_members_age, data=dat2, x=TRUE, y=TRUE)

probabilities <- predict(idx_model, type = "response")

logit = log(probabilities/(1-probabilities)) 

fullmodel <- lm((logit+3) ~ dat2$pop_den)
summary(fullmodel)
plot(fullmodel)

bc <- boxcox(fullmodel)

best.lam <- bc$x[which(bc$y == max(bc$y))]

fullmodel.inv <- lm((logit) ~  (pop_den)^2, data = dat2)
plot(fullmodel.inv)


model13 <- glm(y ~ edu_a + pop_den + wealth_2 + (sex_f)^(3) + humidindex + 
                 rcs(hh_members_age, 3) + net_use_u5 + ACT_use_u5 + hh_size  + 
                 hh_size, data = dat2, binomial) 

model13 <- glm(y ~ rcs(hh_members_age, 3),data = dat2, binomial)
summary(model13)
plot(model13)

probabilities <- predict(model13, type = "response")

dat3 = dat2[,c("wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
               "ACT_use_u5", "l_pop_den","hh_members_age", "sex_f", "humidindex")]

dat3$pop_den <- (dat3$l_pop_den)^-2
dat3$hh_members_age <- rcs(dat3$hh_members_age, 3)

mydata <- dat3 %>%
  dplyr::select_if(is.numeric)
predictors <- colnames(mydata)# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(predictor.value, logit))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free")


############################inverse transformation############################################
dat4 <- dat2
dat4$pop_den <- (dat4$l_pop_den)^2
dat4$sex_f <- (dat4$sex_f)^3

model13 <- glm(y ~ edu_a + pop_den + wealth_2 + sex_f + humidindex + 
                 rcs(hh_members_age, 3) + net_use_u5 + ACT_use_u5 + hh_size  + 
                 hh_size, data = dat4, binomial) 

dat5 <- dat4[,c("wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
                "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex")]


probabilities <- predict(model13, type = "response")

###### ploting logit of predicted probs ########
mydata <- dat5 %>%
  dplyr::select_if(is.numeric)
predictors <- colnames(mydata)# Bind the logit and tidying the data for plot
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(predictor.value, logit))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free")



skewness(dat2$pop_den, na.rm = TRUE)
dat2$pop_den <- log10(max(dat2$pop_denT+1) - dat2$pop_den)

dat4$pop_den <- 1/dat4$pop_den
