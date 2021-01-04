#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", " randomGLM", "ROCR", "AER")

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
dat1 <- dat1 %>% filter(p_test != "NA")
levels <- c(-2, (median(dat1$p_test)), 1)
labels <- c(0, 1)
dat1 <- dat1 %>% mutate(y = cut(p_test, levels, labels = labels))
dat1 <- dat1[!is.na(dat1$y), ]
dat1$y = as.factor(dat1$y)


table(dat1$y)

# Plot missing values:
aggr(dat1, col = c('green','red'), numbers = TRUE, sortVars = TRUE, 
     labels = names(dat1), cex.axis = .5, gap = 2, 
     ylab = c("Proportion in variable","Proportion in dataset"))

#---------------------------------------------
# Model 0: Wealth + Sex 
#---------------------------------------------
# Train model:

gfit0 = glm(y~wealth_2 + sex_f, data = dat1, binomial)

# Summarize and inspect:
summ(gfit0, confint = TRUE, pvals = TRUE, digits = 3)

# Comparing current model to the null model (LTR):
pval_m0 = 1 - pchisq(gfit0$null.deviance - gfit0$deviance, gfit0$df.null - gfit0$df.residual)
round(pval_m0, 3)

# Comparing current model to the saturated model (Deviance test):
pval_ms = 1 - pchisq(gfit0$deviance, df = gfit0$df.residual)
round(pval_ms, 3)

# Asymptotic distribution of model coefficients (model 0)
plot_summs(gfit0, scale = TRUE, plot.distributions = TRUE)


# high malria prevalence risk as a function of wealth: 
x1 = data.frame(sex_f = 0:1, wealth_2 = 0:1)
plot(predict(gfit0, x1, type = "response"), 
     type = 'l', xlab = "wealth_2", ylab = "High malaria prevalence")
x2 = data.frame(sex_f = 0:1, wealth_2=0:1)
lines(predict(gfit0, x2, type = "response"), col="red")

# Train different models on imputed data:
#socioeconomic
gfit1a <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age,
             data = dat1, binomial)

#behavioral 
gfit1b <- glm(y ~ net_use_u5 + net_use_preg + ACT_use_u5, data = dat1, binomial)

#climatic and social 
gfit1c <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age +
                humidindex, data = dat1, binomial)

#behavioral and social 
gfit1d <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5, data = dat1, binomial)

#behavioral, climatic, and social 
gfit1e <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat1, binomial)

#interactions
gfit1i <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg*edu_a + ACT_use_u5 + humidindex, data = dat1, binomial)

# Compare fits:
export_summs(gfit1a, gfit1b, gfit1c, gfit1d, gfit1e, gfit1i, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("model 1a", "model 1b", "model 1c", "model 1d", "model 1e", "model 1i"))

plot_summs(gfit1a, gfit1b, gfit1c, gfit1d, gfit1e, gfit1i, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("model 1a", "model 1b", "model 1c", "model 1d", "model 1e", "model 1i"))


# Create reduced dataset for model 2:
# (filter out rows with NA values)
not_na_mask = !is.na(dat1$y) & !is.na(dat1$edu_a) & !is.na(dat1$wealth_2) & 
        !is.na(dat1$hh_size) & !is.na(dat1$pop_den) & !is.na(dat1$sex_f) & 
        !is.na(dat1$hh_members_age) & !is.na(dat1$net_use_u5) & 
        !is.na(dat1$net_use_preg) & !is.na(dat1$ACT_use_u5) & !is.na(dat1$humidindex)
dat2 = dat1[not_na_mask, ]
table(dat2$y)

#------------------------------------------------------------------
# Model 2: on reduced dataset 
#------------------------------------------------------------------
# Train models:

#socioeconomic
gfit2a <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age,
              data = dat2, binomial)

#behavioral 
gfit2b <- glm(y ~ net_use_u5 + net_use_preg + ACT_use_u5, data = dat2, binomial)

#climatic and social 
gfit2c <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age +
                      humidindex, data = dat2, binomial)

#behavioral and social 
gfit2d <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5, data = dat2, binomial)

#behavioral, climatic, and social 
gfit2e <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)

#interactions
gfit2i <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5*edu_a + net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)

# Merge model summaries:
export_summs(gfit2a, gfit2b, gfit2c, gfit2d, gfit2e, gfit2i, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3,  model.names = c("model 2a", "model 2b", "model 2c", "model 2d", "model 2e", "model 2i"))

# Compare asymptotic distributions of coefficients:
plot_summs(gfit2a, gfit2b, gfit2c, gfit2d, gfit2e, gfit2i, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("model 2a", "model 2b", "model 2c", "model 2d", "model 2e", "model 2i"))


#------------------------------------------------------------------
# Model interpretation
#------------------------------------------------------------------
#odds ratio confidence intervals
exp(confint(gfit2e))

#------------------------------------------------------------------
### Model diagnostics 
#------------------------------------------------------------------
#ROC
#ROC original datasets models


#socioeconomic
prob2a=predict(gfit2a,type=c("response")) 
prob2a <- as.data.frame(prob2a)
dat2a <- dat2[,c("y", "edu_a", "wealth_2", "hh_size", "pop_den", "sex_f", "hh_members_age")]
pred2a <- prediction(prob2a, dat2a$y)    
perf2a <- performance(pred2a, measure = "tpr", x.measure = "fpr")     
plot(perf2a, col="red", main="ROC curve High malaria prevalence", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #adds a 45 degree line

#behavioral
prob2b=predict(gfit2b,type=c("response")) 
prob2b <- as.data.frame(prob2b)
dat2b <- dat2[,c("y", 'net_use_u5', 'net_use_preg', "ACT_use_u5")]
pred2b <- prediction(prob2b, dat2b$y)    
perf2b <- performance(pred2b, measure = "tpr", x.measure = "fpr")     
plot(perf2b, col="black", main="ROC curve High malaria prevalence", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #adds a 45 degree line


#climatic and social 
prob2c=predict(gfit2c,type=c("response")) 
prob2c <- as.data.frame(prob2c)
dat2c <- dat2[,c("y", "edu_a", "wealth_2", "hh_size", "pop_den", "sex_f", 
                 "hh_members_age", "humidindex")]
pred2c <- prediction(prob2c, dat2c$y)    
perf2c <- performance(pred2c, measure = "tpr", x.measure = "fpr")     
plot(perf2c, col="blue", main="ROC curve High malaria prevalence", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #adds a 45 degree line


#behavioral and social 
prob2d=predict(gfit2d,type=c("response")) 
prob2d <- as.data.frame(prob2d)
dat2d <- dat2[,c("y", "edu_a", "wealth_2", "hh_size", "pop_den", "sex_f", 
                 "hh_members_age", 'net_use_u5', 'net_use_preg', "ACT_use_u5")]
pred2d <- prediction(prob2d, dat2d$y)    
perf2d <- performance(pred2d, measure = "tpr", x.measure = "fpr")     
plot(perf2d, col="green", main="ROC curve High malaria prevalence", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #adds a 45 degree line


#behavioral, climatic, and social 
prob2e=predict(gfit2e,type=c("response")) 
prob2e <- as.data.frame(prob2e)
dat2e <- dat2[,c("y", "edu_a", "wealth_2", "hh_size", "pop_den", "sex_f", 
                 "hh_members_age", 'net_use_u5', 'net_use_preg', "ACT_use_u5", "humidindex")]
pred2e <- prediction(prob2e, dat2e$y)    
perf2e <- performance(pred2e, measure = "tpr", x.measure = "fpr")     
plot(perf2e, col="yellow", main="ROC curve High malaria prevalence", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #adds a 45 degree line


plot( perf2a, col="red", main="ROC curve High malaria prevalence")
plot(perf2b, add = TRUE, col="deepskyblue")
plot(perf2c, add = TRUE, col="orange")
plot(perf2d, add = TRUE, col="green")
plot(perf2e, add = TRUE, col="blue")
abline(0, 1) #adds a 45 degree line

