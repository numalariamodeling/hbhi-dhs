#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", " randomGLM", "ROCR")

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

dat1 = urbandataset[c("p_test", "wealth_2", "edu_a", "u5_prop", "preg", 
              "net_use_u5", "net_use_preg", "hh_size", "ACT_use_u5", "pop_den", 
              "l_pop_den", "hh_members_age", "sex_f", "humidindex", "Rural_urban")]

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
gfit1a <- glm(y ~ edu_a + wealth_2 + hh_size + l_pop_den + sex_f + hh_members_age,
             data = dat1, binomial)

#behavioral 
gfit1b <- glm(y ~ net_use_u5 + net_use_preg + ACT_use_u5, data = dat1, binomial)

#climatic and social 
gfit1c <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age +
                humidindex, data = dat1, binomial)

#behavioral and social 
gfit1d <- glm(y ~ edu_a + wealth_2 + hh_size + l_pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5, data = dat1, binomial)

#behavioral, climatic, and social 
gfit1e <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat1, binomial)

# Compare fits:
export_summs(gfit1a, gfit1b, gfit1c, gfit1d, gfit1e, scale = F, error_format = "[{conf.low}, 
             {conf.high}]", digits = 3, model.names = c("model 1a", "model 1b", "model 1c", "model 1d", "model 1e"))

plot_summs(gfit1a, gfit1b, gfit1c, gfit1d, gfit1e, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("model 1a", "model 1b", "model 1c", "model 1d", "model 1e"))


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
gfit2a <- glm(y ~ edu_a + wealth_2 + hh_size + l_pop_den + sex_f + hh_members_age,
              data = dat2, binomial)

#behavioral 
gfit2b <- glm(y ~ net_use_u5 + net_use_preg + ACT_use_u5, data = dat2, binomial)

#climatic and social 
gfit2c <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age +
                      humidindex, data = dat2, binomial)

#behavioral and social 
gfit2d <- glm(y ~ edu_a + wealth_2 + hh_size + l_pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5, data = dat2, binomial)

#behavioral, climatic, and social 
gfit2e <- glm(y ~ edu_a + wealth_2 + hh_size + pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex, data = dat2, binomial)


# Merge model summaries:
export_summs(gfit2a, gfit2b, gfit2c, gfit2d, gfit2e, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3,  model.names = c("model 2a", "model 2b", "model 2c", "model 2d", "model 2e"))

# Compare asymptotic distributions of coefficients:
plot_summs(gfit2a, gfit2b, gfit2c, gfit2d, gfit2e, scale = TRUE, plot.distributions = F, 
           inner_ci_level = .95, model.names = c("model 2a", "model 2b", "model 2c", "model 2d", "model 2e"))


#------------------------------------------------------------------
# Model interpretation
#------------------------------------------------------------------
#odds ratio confidence intervals
exp(confint(gfit2e))

#------------------------------------------------------------------
### Model diagonistics 
#------------------------------------------------------------------
#ROC
prob=predict(gfit2e,type=c("response")) 
prob <- as.data.frame(prob)
pred <- prediction(prob, dat2$y)    
perf <- performance(pred, measure = "tpr", x.measure = "fpr")     
plot(perf, col=rainbow(7), main="ROC curve Admissions", xlab="Specificity", 
     ylab="Sensitivity")    
abline(0, 1) #add a 45 degree line

