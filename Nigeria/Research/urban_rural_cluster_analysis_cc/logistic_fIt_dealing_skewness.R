#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", " randomGLM", "ROCR", "AER", "caretEnsemble", "klaR")

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

dat1 = ruraldataset[c("p_test", "wealth_2", "edu_a", "u5_prop", "preg", 
                      "net_use_u5", "net_use_preg", "hh_size", "ACT_use_u5", "pop_den", 
                      "hh_members_age", "sex_f", "humidindex", "Rural_urban")]

# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)





# Create reduced dataset for model 2:
# (filter out rows with NA values)
dat2 <- na.omit(dat1)
table(dat2$p_level)


dat2 = dat2[,c("y", "wealth_2", "edu_a", "preg", 
                      "net_use_u5", "net_use_preg", "hh_size", "ACT_use_u5", "pop_den", 
                      "hh_members_age", "sex_f", "humidindex")]

#imodel
gfit1i <- glm(y ~ edu_a + wealth_2 + preg +  hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat1, binomial)
summary(gfit1i)


dat2$y <- factor(dat2$y)
levels(dat2$y)=c("Yes","No")
 
my_ctrl <- trainControl(method = "cv", 
                        number = 5,
                        classProbs = TRUE,
                        savePredictions = "final",
                        index = 
                          createResample(dat2$y, 3),
                        sampling = "up",
                        allowParallel = TRUE)

model_list <- caretList(y ~ .,
                        data = dat2,
                        methodList = c("glm"),
                        metric = "Kappa",
                        tuneList = NULL,
                        continue_on_fail = FALSE,  
                        preProcess = c("BoxCox", "center", "scale"),
                        trControl = my_ctrl)

summary(model_list$glm)
summary(model_list$nb)

exp(confint(model_list$glm))
# model comparisons 

resamp<-resamples(list(logistic = model_list$glm, NB = model_list$nb))
summary(resamp)
?xyplot.resamples

ggplot(resamp, models = resamp$models[1], metric = resamp$metrics[1])
resamp$models
modelDifferences<- diff(resamp)
summary(modelDifferences) # p-value suggests a small difference in performance with logistic model doing better 
