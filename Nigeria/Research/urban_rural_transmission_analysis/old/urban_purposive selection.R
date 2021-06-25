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

dat1 = urbandataset[,c("p_test", "wealth_2", "edu_a", "net_use_u5", "net_use_preg", "hh_size",
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

########################multivariable model comparisons####################################
model1 <- glm(y ~ edu_a + wealth_2 +  annual_precipitation + sex_f + hh_size + log_pop_den  + hh_members_age
              + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)

summary(model1)

model2 <- glm(y ~ edu_a + wealth_2, data = dat2, binomial) 


delta_coef_significant <- abs((coef(model2)-coef(model1)[1:3])/
                                coef(model1)[1:3]) 

delta_coef_significant <- as.data.frame(round(delta_coef_significant, 3))



lrtest1 <- as.data.frame(lrtest(model1, model2))
lrtest1$model <- "sig_model"

#multivariable model comparisons adding  annual_precipitation

model3 <- glm(y ~ edu_a  + wealth_2 + annual_precipitation, data = dat2, binomial) 


delta_coef_precip <- abs((coef(model3)-coef(model1)[1:4])/
                           coef(model1)[1:4]) 

delta_coef_precip <- as.data.frame(round(delta_coef_precip, 3))

lrtest3 <- as.data.frame(lrtest(model2, model3))
lrtest3$model <- "wealth_model"


#multivariable model comparisons adding hh_size 
model1a <- glm(y ~ edu_a  + wealth_2 + hh_size + sex_f  + annual_precipitation  + log_pop_den  + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)

model4 <- glm(y ~ edu_a + wealth_2 + hh_size , data = dat2, binomial) 
summary(model4)

delta_coef_hh_size <- abs((coef(model4)-coef(model1a)[1:4])/
                            coef(model1a)[1:4]) 

delta_coef_hh_size <- as.data.frame(round(delta_coef_hh_size, 3))


lrtest4 <- as.data.frame(lrtest(model2, model4))
lrtest4$model <- "hh_size_model"

#multivariable model comparisons adding log_pop_den 
model1b <- glm(y ~ edu_a +  + wealth_2 + log_pop_den + sex_f + annual_precipitation + hh_size + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)

model5 <- glm(y ~ edu_a + wealth_2 + log_pop_den, data = dat2, binomial) 
summary(model5)

delta_coef_pop_den <- abs((coef(model5)-coef(model1b)[1:4])/
                            coef(model1b)[1:4]) 

delta_coef_pop_den <- as.data.frame(round(delta_coef_pop_den, 3))


lrtest5 <- as.data.frame(lrtest(model2, model5))
lrtest5$model <- "pop_den_model"


#multivariable model comparisons adding hh_members_age
model1c <- glm(y ~ edu_a + wealth_2 + hh_members_age + sex_f + annual_precipitation + log_pop_den + hh_size 
               + net_use + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1a)

model6 <- glm(y ~ edu_a + wealth_2 + hh_members_age, data = dat2, binomial) 
summary(model6)

delta_coef_hh_age <- abs((coef(model6)-coef(model1c)[1:4])/
                           coef(model1c)[1:4]) 

delta_coef_hh_age <- as.data.frame(round(delta_coef_hh_age, 3))


lrtest6 <- as.data.frame(lrtest(model2, model6))
lrtest6$model <- "hh_age_model"

#multivariable model comparisons adding net_use
model1d <- glm(y ~ edu_a + wealth_2 + net_use + sex_f + annual_precipitation  + hh_members_age + log_pop_den + hh_size
               + ACT_use_u5 + humidindex, data = dat2, binomial)
summary(model1d)

model7 <- glm(y ~ edu_a + wealth_2 + net_use, data = dat2, binomial) 
summary(model7)

delta_coef_net_u5 <- abs((coef(model7)-coef(model1d)[1:4])/
                           coef(model1d)[1:4]) 

delta_coef_net_u5 <- as.data.frame(round(delta_coef_net_u5, 3))


lrtest7 <- as.data.frame(lrtest(model2, model7))
lrtest7$model <- "u5_net_model"


#multivariable model comparisons adding ACT_use_u5  
model1f <- glm(y ~ edu_a  + wealth_2 + ACT_use_u5 + net_use_u5 + sex_f  + annual_precipitation + net_use_preg + hh_members_age + log_pop_den + hh_size
               + humidindex, data = dat2, binomial)

model9 <- glm(y ~ edu_a + wealth_2 + ACT_use_u5, data = dat2, binomial) 
summary(model9)

delta_coef_ACT <- abs((coef(model9)-coef(model1f)[1:4])/
                        coef(model1f)[1:4]) 

delta_coef_ACT <- as.data.frame(round(delta_coef_ACT, 3))


lrtest9 <- as.data.frame(lrtest(model2, model9))
lrtest9$model <- "ACT_model"


#multivariable model comparisons adding  humidindex
model1g <- glm(y ~ edu_a   + wealth_2 +  humidindex+ sex_f + annual_precipitation + net_use_preg + net_use_u5 + hh_members_age + log_pop_den + hh_size, data = dat2, binomial)

model10 <- glm(y ~ edu_a  + wealth_2 +  humidindex, data = dat2, binomial) 
summary(model10)

delta_coef_humidindex <- abs((coef(model10)-coef(model1g)[1:4])/
                               coef(model1g)[1:4]) 

delta_coef_humidindex <- as.data.frame(round(delta_coef_humidindex, 3))


lrtest10 <- as.data.frame(lrtest(model2, model10))
lrtest10$model <- "humidindex_model10"


#Creating dataframe for computed coeficient difference 
delta_coef_df <- merge(delta_coef_pop_den, delta_coef_precip, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_size, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_ACT, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_age, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_humidindex, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_net_u5, by="row.names", all=TRUE)
#delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
#delta_coef_df <- merge(delta_coef_df,delta_coef_housing_qua, by="row.names", all=TRUE)
#delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- na.omit(delta_coef_df)
delta_coef_df <- delta_coef_df[c(3), ]
#delta_coef_df <- delta_coef_df[-c(1), ]

#plot of delta_coef
df1 <- melt(delta_coef_df,"Row.names")

g1 <- ggplot(df1, aes(x = variable, y = value)) +
  geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  scale_y_continuous(breaks = seq(0, 0.21, by = 0.020), limits=c(0,0.21))+
  theme(axis.text.x = element_text(angle=-40, hjust=.1))


g1


#Creating dataframe for computed likelihod ratio
lrtest_df <- dplyr::bind_rows(lrtest1, lrtest3, lrtest4, lrtest5, lrtest6, lrtest7, lrtest8, lrtest9, lrtest10)
write.csv(lrtest_df, "lrtest.csv")
######################## adding more variable to the model that had at least 2% change in coeficients###########
#multivariable model comparisons


########parsimonious model
model1i_parsi <- glm(y ~ edu_a  + log_pop_den + wealth_2 + net_use_u5 + hh_members_age + net_use_preg, data = dat2, binomial)


summary(model1i_parsi)
pass_coefs <- as.data.frame(coef(model1i_parsi))
pass_coefs <- as.data.frame(SE(model1i_parsi))
pass_odds <- as.data.frame(exp(coef(model1i_parsi)))


#comparing model with all variables and parsimonious model 
lrtest(model1a, model1i_parsi)

ddd <- export_summs(model1a, model1i_parsi, scale = F, error_format = "[{conf.low}, {conf.high}]", 
                    digits = 3, model.names = c("Model with all variables", "Parsimonious model"))

ddd

write.csv((as.data.frame(ddd)),"urban_comparing_parsimonious.csv")
