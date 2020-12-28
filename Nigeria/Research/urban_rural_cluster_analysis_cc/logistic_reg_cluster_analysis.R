rm(list=ls())

#important to download the github version of tidyverse.Uncomment the script below to run
#install.packages("devtools") #download devtools if is not available in your packages 
#devtools::install_github("r-pkgs/usethis")

#devtools::install_github("hadley/tidyverse")


#Reading in the necessary packages 


list.of.packages <- c("caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
                      "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
                      "scales", "sjPlot", "sjlabelled", "sjmisc", "mapview", "geosphere")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales", "sjPlot", "sjlabelled", "sjmisc", "geosphere")

lapply(x, library, character.only = TRUE) #applying the library function to packages





# set document path to current script path 
setwd("C:/Users/pc/downloads")

#loading dataset
#dhs2019 <- read.csv("allcluster_revised_kap_housing_quality.csv", header= TRUE)
#mis2015 <- read.csv("mis_allcusters_housing_q.csv", header= TRUE)
#mis2010 <- read.csv("mis2010_allcusters_housing_q.csv", header= TRUE)
merged_df <- read.csv("Nigereia_2010_2018_clustered_final_dataset.csv", header= TRUE)

table(merged_df$Rural_urban)

#merged_df <- dplyr::bind_rows(dhs2019, mis2015, mis2010)


urbandataset <- merged_df %>% filter(Rural_urban == 1)
ruraldataset <- merged_df %>% filter(Rural_urban == 2)
comineddataset <- merged_df


#Exploratory data analysis (EDA)
summary(merged_df)

#selecting variables of interest. replace datafrom with cluster of interest
df <- urbandataset[ ,colnames(urbandataset) 
                    %in% c("wealth_2", "u5_prop", "net_access","preg","edu_a","net_use_u5", 
                           "net_use_preg", "hh_size", "house_floor", "house_wall", 
                           "house_roof", "housing_qua","ACT_use_u5", "pop_den",
                           "l_pop_den","p_test", "humidindex", "Rural_urban")]

colnames(df)
summary(df$humidindex)

df2 <- df
df2 <- df2 %>% mutate(scaled_hudix = scale(df2$humidindex, center = T))

#Plotting Histograms

ggfun <- function(dat, x.var, title){
  x.var <- enquo(x.var)
  title <- enquo(title) 
  ggp <- ggplot(data = dat,
                aes(x = !! x.var,)) +
    ggtitle(title,)+
    geom_histogram(bins = 30, color = "dimgray")
  
  return(ggp)
}

colnames(df2)

ggfun(dat = df2, x.var = p_test, title = "Malaria Transmission Intensisty")
ggfun(dat = df2, x.var = wealth_2, title = "Prop. of High Wealth Quantile")
ggfun(dat = df2, x.var = preg, title = "Prop. of Pregnant Women")
ggfun(dat = df2, x.var = edu_a, title = "Prop. of Higher Education Attainment")
ggfun(dat = df2, x.var = net_use_u5, title = "Prop. of U5 Net Use")
ggfun(dat = df2, x.var = net_use_preg, title = "Prop. of Pregnant Women Net Use")
ggfun(dat = df2, x.var = hh_size, title = "Household Size")
ggfun(dat = df2, x.var = ACT_use_u5, title = "Prop. of U5 ACT Use")
ggfun(dat = df2, x.var = l_pop_den, title = "population Density")
ggfun(dat = df2, x.var = ave_kap, title = "Prop. Of Positive Knowledge, Attitude & Practice")
ggfun(dat = df2, x.var = settlement_type, title = "Settlement Type")

#Independent variable
#Boxplots
par(mfrow=c(2,3))
for (i in 1:(length(df2)-1)){
  boxplot(x = df2[i], 
          horizontal = TRUE, 
          main = sprintf('Boxplot of the variable: %s', 
                         colnames(df2[i])),
          xlab = colnames(df2[i]))
  
  
}

#Plotting scatters
plot.features <- melt(df2, "p_test")
ggplot(plot.features, aes(value, p_test)) + 
  geom_jitter() + 
  facet_wrap(~variable, scales = "free")

#Exploring logistic regeressions
#creating prevelence classes

#levels <- c(-2, 0.009, 0.049, 0.09, 0.49, 0.749, 1)
df2 <- df2 %>% filter(p_test != "NA")
levels <- c(-2, (median(df2$p_test)), 1)
labels <- c("very low", "high")
df3 <- df2 %>% mutate(p_level = cut(p_test, levels, labels = labels))
df3 <- df3[!is.na(df3$p_level), ]
#plot prevelence

ggplot(df3, aes(p_level, p_test)) +
  geom_jitter(size = 3) +
  coord_cartesian() +
  scale_color_gradient() +
  ggtitle(" ") +
  xlab("Classes") +
  ylab("malaria transmission intensity ")+
  theme_bw()

#prevalence barplot
counts <- table(df3$p_level)
barplot(counts, main=" ",
        xlab="Classes",
        ylab = "Frequency",
        col="cadetblue4",
        border="brown"
)

# Stacked Bar Plot with Colors and Legend
counts <- table(df3$settlement_type, df3$p_level)
barplot(counts, main="Malaria Presence by Settlement Type",
        xlab="Test Results",
        ylab="Frequency", col=c("cadetblue4","gray87"),
        legend = rownames(counts))

# Boxplot of education by prevalence level
boxplot(edu_a~p_level,
        data=df3,
        main="Prop. of Higher Edu Attainment boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Prop. High Education Attainment",
        col="cadetblue3",
        border="brown"
)

# Boxplot of WEALTH by prevalence level
boxplot(wealth_2~p_level,
        data=df3,
        main="Prop. of High Wealth Quantile boxplots for each transmission intensity class",
        xlab="Transmission intensity class",
        ylab="Prop. of High Wealth Quantile",
        col="cadetblue3",
        border="brown"
)

#BOX plot of kap by transmission intersity 

boxplot(ave_kap~p_level,
        data=df3,
        main="KAP boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Average KAP",
        col="cadetblue3",
        border="brown"
)

#BOX plot of net use in under 5 by transmission intersity 

boxplot(net_use_u5~p_level,
        data=df3,
        main="Net use in U5 boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Net use U5",
        col="cadetblue3",
        border="brown"
)

#under 5 prop
boxplot(u5_prop~p_level,
        data=df3,
        main="Prop. U5 boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="U5 Prop.",
        col="cadetblue3",
        border="brown"
)

#preg
boxplot(preg~p_level,
        data=df3,
        main="Pregnant women proportion boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Pregnant women proportion",
        col="cadetblue3",
        border="brown"
)

#preg net use
boxplot(net_use_preg~p_level,
        data=df3,
        main="Prop. of Pregnant women net use boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Prop. of Pregnant women net use",
        col="cadetblue3",
        border="brown"
)

#ACT
boxplot(ACT_use_u5~p_level,
        data=df3,
        main="Prop. of U5 ACT use boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Prop. of U5 ACT use ",
        col="cadetblue3",
        border="brown"
)

# POP DEN
boxplot(l_pop_den~p_level,
        data=df3,
        main="Population density boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Population density",
        col="cadetblue3",
        border="brown"
)

# HH SIZE
boxplot(hh_size~p_level,
        data=df3,
        main="Household size boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Household size",
        col="cadetblue3",
        border="brown"
)


### Edu_a vs. wealth, by  color = transmission intersity
df_scatter <- df3 %>% filter(p_level == "positive")
ggplot(df_scatter, aes(edu_a, wealth_2, color = p_level)) + 
  geom_jitter(size = 4) +
  ggtitle("Education vs. Wealth by transmission intensity class ") +
  xlab("Education") +
  ylab("Wealth")+
  theme_grey()

### pHousehold Size vs. Net Use by Prevalence Level
ggplot(df3, aes(hh_size, net_use_u5, color = p_level)) + 
  geom_jitter(size = 3) +
  ggtitle("Household Size vs. Net Use by transmission intensity class") +
  xlab("Household Size") +
  ylab("Net Use U5")+
  theme_grey()

### pHousehold Size vs. Net Use by Prevalence Level
ggplot(df3, aes(ave_kap, net_use_u5, color = p_level)) + 
  geom_jitter(size = 3) +
  ggtitle("KAP vs. Net Use by transmission intensity class") +
  xlab("KAP") +
  ylab("Net Use U5")+
  theme_grey()

### pHousehold Size vs. Net Use by Prevalence Level
ggplot(df3, aes(ACT_use_u5, net_use_u5, color = p_level)) + 
  geom_jitter(size = 3) +
  ggtitle("KAP vs. ACT_use_u5 by transmission intensity class") +
  xlab("KAP") +
  ylab("ACT_use_u5")+
  theme_grey()

#Dividing data into training and test set
#Random sampling 
samplesize = 0.70*nrow(df)
set.seed(100)
index = sample(seq_len(nrow(df3)), size = samplesize)
#Creating training and test set 
train_data = df3[index,]
test_data = df3[-index,]


#fitting model for rural or urban

mmodel <- glm(p_level ~ edu_a + wealth_2 + net_access + net_use_u5 + net_use_preg + 
                ACT_use_u5 + hh_size + humidindex + l_pop_den, 
              data = df3, family = "binomial")

#fiting mode lfor all clusters
#creating dummy variable for cluster type. 1 = urban, 0 = rural
df3 <- mutate(df3, Rural_urban = ifelse(Rural_urban == 1, 1, 0))

mmodel <- glm(p_level ~ edu_a + wealth_2 + net_use_u5 + net_use_preg + 
                    ACT_use_u5 + hh_size + humidindex + l_pop_den + Rural_urban,
             data = df3, family = "binomial")


summary(mmodel)

#setting plotting theme

set_theme(base = theme_classic()) #To remove the background color and the grids

plot_model(mmodel, title = " ", line.size = 1, dot.size = 2) + ylim(0, 2.5)


## extract the coefficients from the model and exponentiate
exp(coef(mmodel))

#odds ratio confidence intervals
exp(confint(mmodel))

#calculatIN predicted probabilities for each of our outcome levels using the fitted function. 
head(pp <- fitted(mmodel))

#examine the changes in predicted probability associated with wealth while holding the other constant

#Plotting the effects 

plot(allEffects(mmodel))


plot(predictorEffects(mmodel, ~ edu_a), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ wealth_2), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ u5_prop), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ preg), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ net_use_u5), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ hh_size), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ ACT_use_u5), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ l_pop_den), axes=list(grid=TRUE))
plot(predictorEffects(mmodel, ~ ave_kap), axes=list(grid=TRUE))


edu_prob <-predictorEffects(mmodel, ~ edu_a)

expit<-function(x){
  exp(x)/(1+exp(x))
}

df_edu_rural_pred <- data.frame(edu_prob$edu_a$x,expit(edu_prob$edu_a$fit),expit(edu_prob$edu_a$lower), expit(edu_prob$edu_a$upper), cat ="rural")

#df_edu_urban_pred <- data.frame(edu_prob$edu_a$x,expit(edu_prob$edu_a$fit),expit(edu_prob$edu_a$lower), expit(edu_prob$edu_a$upper), cat= "urban")

df_edu_all <- rbind(df_edu_rural_pred, df_edu_urban_pred)

head(df_edu_all)

library(viridis)
df_edu_all%>%
  ggplot( aes(x=edu_a, y=expit.edu_prob.edu_a.fit., ymin=expit.edu_prob.edu_a.lower., ymax=expit.edu_prob.edu_a.upper., fill=str_to_title(cat), 
              linetype=str_to_title(cat))) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_color_viridis(discrete = TRUE) +
  ylab("Predicted probabilities of high parasite prevalence")+ 
  xlab("Proportion of individuals with secondary or higher educational attainment")+ 
  theme_bw()+
  theme(legend.title = element_blank())+ 
  theme(panel.border = element_blank())

  
                            

ggsave(
  "education_predicted_probabilities.pdf",
  path = "results/research_plots/pred_probabilities",
  dpi = 300,
  limitsize = TRUE,
)



ACT_prob <-predictorEffects(mmodel, ~ ACT_use_u5)

expit<-function(x){
  exp(x)/(1+exp(x))
}

df_ACT_rural_pred <- data.frame(ACT_prob$ACT_use_u5$x,expit(ACT_prob$ACT_use_u5$fit),expit(ACT_prob$ACT_use_u5$lower), expit(ACT_prob$ACT_use_u5$upper), cat ="rural")

df_ACT_urban_pred <- data.frame(ACT_prob$ACT_use_u5$x,expit(ACT_prob$ACT_use_u5$fit),expit(ACT_prob$ACT_use_u5$lower), expit(ACT_prob$ACT_use_u5$upper), cat= "urban")

df_ACT_use_u5ll <- rbind(df_ACT_rural_pred, df_ACT_urban_pred)

head(df_ACT_use_u5ll)

library(viridis)
df_ACT_use_u5ll%>%
  ggplot( aes(x=ACT_use_u5, y=expit.ACT_prob.ACT_use_u5.fit., ymin=expit.ACT_prob.ACT_use_u5.lower., ymax=expit.ACT_prob.ACT_use_u5.upper., fill=str_to_title(cat), 
              linetype=str_to_title(cat))) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_color_viridis(discrete = TRUE) +
  ylab("Predicted probabilities of high parasite prevalence")+ 
  xlab("Proportion of U5 that use ACT")+ 
  theme_bw()+
  theme(legend.title = element_blank())+ 
  theme(panel.border = element_blank())




ggsave(
  "ACT_Use_U5_predicted_probabilities.pdf",
  path = "results/research_plots/pred_probabilities",
  dpi = 300,
  limitsize = TRUE,
)

l_pop_den_prob <-predictorEffects(mmodel, ~ l_pop_den)

expit<-function(x){
  exp(x)/(1+exp(x))
}

df_l_pop_den_rural_pred <- data.frame(l_pop_den_prob$l_pop_den$x,expit(l_pop_den_prob$l_pop_den$fit),expit(l_pop_den_prob$l_pop_den$lower), expit(l_pop_den_prob$l_pop_den$upper), cat ="rural")

#df_l_pop_den_urban_pred <- data.frame(l_pop_den_prob$l_pop_den$x,expit(l_pop_den_prob$l_pop_den$fit),expit(l_pop_den_prob$l_pop_den$lower), expit(l_pop_den_prob$l_pop_den$upper), cat= "urban")

df_l_pop_denll <- rbind(df_l_pop_den_rural_pred, df_l_pop_den_urban_pred)

head(df_l_pop_denll)

library(viridis)
df_l_pop_denll%>%
  ggplot( aes(x=l_pop_den, y=expit.l_pop_den_prob.l_pop_den.fit., ymin=expit.l_pop_den_prob.l_pop_den.lower., ymax=expit.l_pop_den_prob.l_pop_den.upper., fill=str_to_title(cat), 
              linetype=str_to_title(cat))) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_color_viridis(discrete = TRUE) +
  ylab("Predicted probabilities of high parasite prevalence")+ 
  xlab("Population density")+ 
  theme_bw()+
  theme(legend.title = element_blank())+ 
  theme(panel.border = element_blank())




ggsave(
  "population_density_predicted_probabilities.pdf",
  path = "results/research_plots/pred_probabilities",
  dpi = 300,
  limitsize = TRUE,
)


edu_prob <-predictorEffects(mmodel, ~ edu_a)

expit<-function(x){
  exp(x)/(1+exp(x))
}

df_edu_rural_pred <- data.frame(edu_prob$edu_a$x,expit(edu_prob$edu_a$fit),expit(edu_prob$edu_a$lower), expit(edu_prob$edu_a$upper), cat ="rural")

#df_edu_urban_pred <- data.frame(edu_prob$edu_a$x,expit(edu_prob$edu_a$fit),expit(edu_prob$edu_a$lower), expit(edu_prob$edu_a$upper), cat= "urban")

df_edu_all <- rbind(df_edu_rural_pred, df_edu_urban_pred)

head(df_edu_all)

library(viridis)
df_edu_all%>%
  ggplot( aes(x=edu_a, y=expit.edu_prob.edu_a.fit., ymin=expit.edu_prob.edu_a.lower., ymax=expit.edu_prob.edu_a.upper., fill=str_to_title(cat), 
              linetype=str_to_title(cat))) +
  geom_line() +
  geom_ribbon(alpha=0.5) +
  scale_color_viridis(discrete = TRUE) +
  ylab("Predicted probabilities of high parasite prevalence")+ 
  xlab("Proportion of individuals with secondary or higher educational attainment")+ 
  theme_bw()+
  theme(legend.title = element_blank())+ 
  theme(panel.border = element_blank())




ggsave(
  "education_predicted_probabilities.pdf",
  path = "results/research_plots/pred_probabilities",
  dpi = 300,
  limitsize = TRUE,
)
#plotting forest plot using ggplot
rural_ci_df <- read.csv("data/ur_ci_rural.csv", header= TRUE)


p <- ggplot(rural_ci_df, aes(x = odds, y = vars)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = 1, height = .2, color = "gray50") +
  geom_point(size = 4, color = "orange")+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+ 
  theme(panel.border = element_blank())+
  ylab("")+
  xlab("Odds Ratio")
p 
 