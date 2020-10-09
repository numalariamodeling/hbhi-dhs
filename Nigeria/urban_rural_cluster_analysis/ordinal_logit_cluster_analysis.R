rm(list=ls())

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/Admin/Documents/NU - Malaria Modeling/Non Linear")

#loading dataset
dataset<- read.csv("ruralcluster.csv")

df <- dataset[ ,colnames(dataset) 
               %in% c("wealth_2", "u5_prop", "preg","edu_a",
                      "net_use_u5", "net_use_preg", "hh_size", 
                      "ACT_use_u5", "pop_den", "l_pop_den","p_test")]
#Exploratory data analysis (EDA)
summary(df)
#Plotting Histograms

df%>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins = 30,)

#Independent variable
#Boxplots
par(mfrow=c(2,3))
for (i in 1:(length(df)-1)){
  boxplot(x = df[i], 
          horizontal = TRUE, 
          main = sprintf('Boxplot of the variable: %s', 
                         colnames(df[i])),
          xlab = colnames(df[i]))
}

#Plotting scatters
plot.features <- melt(df, "p_test")
ggplot(plot.features, aes(value, p_test)) + 
  geom_jitter() + 
  facet_wrap(~variable, scales = "free")

#Exploring logistic regeressions
#creating prevelence classes

#levels <- c(-2, 0.009, 0.049, 0.09, 0.49, 0.749, 1)
levels <- c(-2, 0.009, 0.49, 1)
labels <- c("low stable EC", "hypoE12 or mesoendemic", "hyperendemic or holoendemic")
df <- df %>% mutate(p_level = cut(p_test, levels, labels = labels))

#plot prevelence

ggplot(df, aes(p_level, p_test)) +
  geom_jitter(size = 3) +
  coord_cartesian() +
  scale_color_gradient() +
  ggtitle("Prevalence Clustors") +
  xlab("Clustors") +
  ylab("Prevalence")+
  theme_bw()

#prevalence barplot
counts <- table(df$p_level)
barplot(counts, main="Prevelence Distribution",
        xlab="Clustors",
        col="cadetblue4",
        border="brown"
        )

# Boxplot of education by prevalence level
boxplot(edu_a~p_level,
        data=df,
        main="Education boxplots for each Prevalence Level",
        xlab="Prevslence Level",
        ylab="Education",
        col="cadetblue3",
        border="brown"
)

# Boxplot of education by prevalence level
boxplot(wealth_2~p_level,
        data=df,
        main="Wealth boxplots for each Prevalence Level",
        xlab="Prevslence Level",
        ylab="Wealth",
        col="cadetblue3",
        border="brown"
)
### Edu_a vs. wealth, by  color = prevelance level
ggplot(df, aes(edu_a, wealth_2, color = p_level)) + 
  geom_jitter() +
  ggtitle("Education vs. Wealth by Prevalence Level ") +
  xlab("Education") +
  ylab("Wealth")+
  theme_light()

### pHousehold Size vs. Net Use by Prevalence Level
ggplot(df, aes(hh_size, net_use_u5, color = p_level)) + 
  geom_jitter() +
  ggtitle("Household Size vs. Net Use by Prevalence Level ") +
  xlab("Household Size") +
  ylab("Net Use")+
  theme_light()

#Dividing data into training and test set
#Random sampling 
samplesize = 0.70*nrow(df)
set.seed(100)
index = sample(seq_len(nrow(df)), size = samplesize)
#Creating training and test set 
datatrain = df[index,]
datatest = df[-index,]


#ordinal Logistic regression fiting 

mod<-polr(p_level ~ ACT_use_u5 + edu_a + hh_size + l_pop_den + 
            net_use_preg + net_use_u5 + preg + u5_prop + wealth_2, 
          data=datatrain,Hess=T)

summary(mod)

coeffs <- coef(summary(mod))
p <- pnorm(abs(coeffs[, "t value"]), lower.tail = FALSE) * 2
cbind(coeffs, "p value" = round(p,3))


#odd ratios
odds <- exp(coef(mod))

#confidence intervals
(ci <- confint(mod))

#
#Tests for Ordinal regression assumptions
#No multi-collinearity.
# correlation plot
ggcorrplot(round(cor(df[ ,(colnames(df) 
                           %in% c("p_test", "wealth_2", "u5_prop", "preg","edu_a",
                                  "net_use_u5", "net_use_preg", "hh_size", 
                                  "ACT_use_u5", "pop_den", "l_pop_den"))]), 1), 
           type = "lower", 
           lab = TRUE, 
           title = 'Correlation matrix between variables')


#Variance Inflation Factor (VIF) test 
# check VIF
fit2 <- lm(scale(p_test) ~ wealth_2 + edu_a + net_use_u5 + net_use_preg + ACT_use_u5 + 
             u5_prop + preg + hh_size + pop_den, 
           data = df)
#The general rule of thumbs for VIF test is that if the VIF value is greater than 
#10, then there is multi-collinearity.
vif(fit2)

#Proportional Odds.. Brant Test i.e testing parallel regression assumption
#the relationship between each pair of outcome groups has to be the same

brant(mod)



#Plotting the effects 
plot(Effect(focal.predictors = "edu_a",mod))
plot(Effect(focal.predictors = "wealth_2",mod))
plot(Effect(focal.predictors = c("edu_a", "wealth_2"),mod))

#Model evaluation 
#Compute confusion table and misclassification error
predictprev = predict(mod,datatest)
table(datatest$p_level, predictprev)
mean(as.character(datatest$p_level) != as.character(predictprev))

#model Pseudo R^2
pR2(mod)


# Compute AUC for predicting Class with the variable p_level
f1 = roc(Class ~ p_level, data=datatest) 
plot(f1, col="red")

