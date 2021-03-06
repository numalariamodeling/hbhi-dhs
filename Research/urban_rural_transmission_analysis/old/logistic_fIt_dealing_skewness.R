#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "mi", "mice", "mitools", "VIM", "jtools", "huxtable", "jtools",
       "gridExtra", "broom.mixed", " randomGLM", "ROCR", "AER", "caretEnsemble", "klaR", "naniar", "corrplot")

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

dat1 = ruraldataset[,c("p_test", "wealth_2", "edu_a", "u5_prop", "preg", 
                      "net_use_u5", "net_use_preg", "hh_size", "ACT_use_u5", "pop_den", 
                      "hh_members_age", "sex_f", "humidindex", "Rural_urban", "rainfal", "annual_precipitation")]


# Binarize response:
dat1$y <- ifelse(dat1$p_test < 0.1, 0,1)





# Create reduced dataset for model 2:
# (filter out rows with NA values)
dat2 <- na.omit(dat1)
table(dat2$p_level)


dat2 = dat2[,c("y", "wealth_2", "edu_a", "preg", 
                      "net_use_u5", "net_use_preg", "hh_size", "ACT_use_u5", "pop_den", 
                      "hh_members_age", "sex_f", "humidindex", "rainfal", "annual_precipitation")]

dat2 <- dat2%>% mutate(annual_precipitation = scale(dat2$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

dat2 <- dat2 %>% mutate(pop_den = na_if(pop_den, -9999))
dat2 <- dat2 %>% mutate(log_pop_den = log(pop_den))
nearZeroVar(dat2)

#colmatrix
ggcorrplot(round(cor(dat2[ ,(colnames(dat2) 
                             %in% c("wealth_2", "edu_a", "preg", 
                                    "net_use_u5", "net_use_preg", "hh_size", "ACT_use_u5", "log_pop_den", 
                                    "hh_members_age", "sex_f", "humidindex", "rainfal", "annual_precipitation"))]), 1), 
           type = "lower", 
           lab = TRUE, 
           title = 'Correlation matrix between variables')

# Train different models on imputed data:
#social 
gfit1a <- glm(y ~ edu_a + wealth_2 + hh_size + sex_f + hh_members_age + log_pop_den,
              data = dat2, binomial)

#interventions
gfit1b <- glm(y ~ net_use_u5 + net_use_preg + ACT_use_u5, data = dat2, binomial)


#environment 
gfit1c <- glm(y ~ humidindex +  annual_precipitation, data = dat2, binomial)

#social and interventions 
gfit1d <- glm(y ~ edu_a + wealth_2 + hh_size + log_pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5, data = dat2, binomial)

# social and environment
gfit1e <- glm(y ~ edu_a + wealth_2 + hh_size + log_pop_den + sex_f + hh_members_age +
                  humidindex + annual_precipitation, data = dat2, binomial)

#environment and interventions
gfit1f <- glm(y ~ net_use_u5 + net_use_preg + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)

#Social, environment and interventions
gfit1g <- glm(y ~ edu_a + wealth_2 + hh_size + log_pop_den + sex_f + hh_members_age
              + net_use_u5 + net_use_preg + ACT_use_u5 + humidindex + annual_precipitation, data = dat2, binomial)



# Comparing fits estimates:
export_summs(gfit1a, gfit1b, gfit1c, gfit1d, gfit1e, gfit1f, gfit1g, scale = F, error_format = "[{conf.low}, {conf.high}]", 
             digits = 3, model.names = c("Social ", "Interventions", "Environment ", "Social and interventions", "Social and environment", "Environment and interventions", "Social, environment and interventions"))



#computing odds ratios
odd_a <- as.data.frame(exp(coef(gfit1a)))
odd_b  <- as.data.frame(exp(coef(gfit1b)))
odd_c  <- as.data.frame(exp(coef(gfit1c)))
odd_d  <- as.data.frame(exp(coef(gfit1d)))
odd_e  <- as.data.frame(exp(coef(gfit1e)))
odd_f  <- as.data.frame(exp(coef(gfit1f)))
odd_g  <- as.data.frame(exp(coef(gfit1g)))

#Creating dataframe forcomputed odd ratios
odds_df <- merge(odd_a,odd_b, by="row.names", all=TRUE)
odds_df <- odds_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
odds_df <- merge(odds_df,odd_c, by="row.names", all=TRUE)
odds_df <- odds_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
odds_df <- merge(odds_df,odd_d, by="row.names", all=TRUE)
odds_df <- odds_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
odds_df <- merge(odds_df,odd_e, by="row.names", all=TRUE)
odds_df <- odds_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
odds_df <- merge(odds_df,odd_f, by="row.names", all=TRUE)
odds_df <- odds_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
odds_df <- merge(odds_df,odd_g, by="row.names", all=TRUE)
odds_df <- odds_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
odds_df

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

#making results into a dataframe 

results = coefficients(summary(model_list$glm))
colnames(results)[2] = "SE"
results_df <- data.frame(results)%>% mutate(odds = (exp(Estimate))) %>% 
        mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
        mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_df["vars"] = c("(Intercept)", "wealth_2","edu_a","preg","net_use_u5",
                        "net_use_preg", "hh_size",  "ACT_use_u5", "pop_den", 
                        "hh_members_age", "sex_f", "humidindex" )

results2 = coefficients(summary(gfit1i))
colnames(results2)[2] = "SE"
results_df2 <- data.frame(results2)%>% mutate(odds = (exp(Estimate))) %>% 
        mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
        mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_df2["vars"] = c("(Intercept_c)", "wealth_2_c","edu_a_c","preg_c","net_use_u5_c",
                           "net_use_preg_c", "hh_size_c",  "ACT_use_u5_c", "pop_den_c", 
                           "hh_members_age_c", "sex_f_c", "humidindex_C" )



p <- ggplot(results_df2, aes(x = odds, y = vars)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                               .2, color = "gray50") +
        geom_point(size = 3.5, color = "orange")+
        theme_bw()+
        theme(panel.grid.minor = element_blank())+ 
        theme(panel.border = element_blank())+
        ylab("ff")+
        xlab("Odds Ratio")
p

d <- ggplot(results_df, aes(x = odds, y = vars)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                               .2, color = "blue") + 
        geom_point(size = 3.5, color = "red")+
        theme_bw()+
        theme(panel.grid.minor = element_blank())+ 
        theme(panel.border = element_blank())+
        ylab("ff")+
        xlab("Odds Ratio")
d 

e <- ggplot(results_df, aes(x = odds, y = vars)) +
        # blue plot
        geom_vline(data=results_df, aes(xintercept = 1), size = .25, linetype = "dashed") + 
        geom_errorbarh(data=results_df, aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                               .2, color = "blue") + geom_point(size = 2.5, color = "red")+
        # red plot
        geom_vline(data=results_df2, aes(xintercept = 1), size = .25, linetype = "dashed") + 
        geom_errorbarh(data=results_df2, aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                               .2, color = "gray50") + geom_point(size = 2.5, color = "orange")+
        theme_bw()+
        theme(panel.grid.minor = element_blank())+ 
        theme(panel.border = element_blank())+
        ylab("ff")+
        xlab("Odds Ratio")
e
        
results_df_12 <- rbind(results_df, results_df2)

c <- ggplot(results_df_12, aes(x = odds, y = vars)) + 
        geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
        geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                               .2, color = "blue") + 
        geom_point(size = 2.5, color = "red")+
        theme_bw()+
        theme(panel.grid.minor = element_blank())+ 
        theme(panel.border = element_blank())+
        ylab("ff")+
        xlab("Odds Ratio")
    
c
