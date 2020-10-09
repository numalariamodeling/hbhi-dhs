rm(list=ls())

#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS", "effects", "pscl", "pROC", "car", "nnet", "reshape2", "AER", "MNLpred",
       "scales")

lapply(x, library, character.only = TRUE) #applying the library function to packages


# set document path to current script path 
setwd("C:/Users/Admin/Documents/NU - Malaria Modeling/Non Linear")


#loading dataset
urbandataset<- read.csv("urbancluster_with_kaps.csv")
ruraldataset <- read.csv("ruralluster_with_kaps.csv")
comineddataset <- read.csv("allcluster_revised_kap.csv")

#specifying cluster type (rural or urban) with settlement_type variable
all_clusters <- mutate(comineddataset, settlement_type 
                       =(is.element(hv001, urbandataset$hv001)))

all_clusters$settlement_type[all_clusters$settlement_type == TRUE] <- "Urban"
all_clusters$settlement_type[all_clusters$settlement_type == FALSE] <- "Rural" 
 
#Exploratory data analysis (EDA)
summary(all_clusters)

#selecting variables of interest. replace datafrom with cluster of interest
df <- ruraldataset[ ,colnames(ruraldataset) 
               %in% c("wealth_2", "u5_prop", "preg","edu_a","net_use_u5", 
                      "net_use_preg", "hh_size", "ACT_use_u5", "pop_den", 
                      "l_pop_den","p_test","kap_cure_med", "kap_death", 
                      "kap_treat", "kap_know", "kap_weak", "settlement_type")]

#creating average KAP
kaps <- df[,c("kap_cure_med", "kap_death", "kap_treat", "kap_know", "kap_weak")]

df$ave_kap=rowMeans(df[,c("kap_cure_med", "kap_death", "kap_treat", "kap_know", "kap_weak")], na.rm=TRUE)


df <- df %>% mutate(ave_kap = apply(X=kaps, MARGIN=1, FUN=mean, na.rm=TRUE))

df2 <- df[ ,colnames(df) 
                %in% c("wealth_2", "u5_prop", "preg","edu_a","net_use_u5", 
                       "net_use_preg", "hh_size", "ACT_use_u5", "l_pop_den", 
                       "p_test","ave_kap", "settlement_type")]
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

levels <- c(-2, 0.049, 0.49, 1)
labels <- c("lse&hypo1", "hypo2&meso", "hyper&holo")
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
barplot(counts, main="Transmission Intensity by Settlement Type",
        xlab="Transmission intensity class",
        ylab="Frequency", col=c("cadetblue4","gray87"),
        legend = rownames(counts))

# Boxplot of education by prevalence level
boxplot(edu_a~p_level,
        data=df3,
        main="Education boxplots for each transmission intensity class",
        xlab="Transmission intensity classes",
        ylab="Education Attainment",
        col="cadetblue3",
        border="brown"
)

# Boxplot of WEALTH by prevalence level
boxplot(wealth_2~p_level,
        data=df3,
        main="Wealth boxplots for each transmission intensity class",
        xlab="Transmission intensity class",
        ylab="Wealth",
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

### Edu_a vs. wealth, by  color = transmission intersity
ggplot(df3, aes(edu_a, wealth_2, color = p_level)) + 
  geom_jitter(size = 3) +
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

#creating dummy variable for cluster type 
#Fitting Multinomial regression model
#choose the level of our outcome that we wish to use as our baseline
df3$p_level <- factor(df3$p_level)

df3$p_level2 <- relevel(df3$p_level, ref = "lse&hypo1")

#fitting model for rural or urban

mmodel <- multinom(p_level2 ~ wealth_2 + edu_a + net_use_u5 + net_use_preg + 
                     ACT_use_u5 + preg + hh_size + l_pop_den + 
                     ave_kap, data = df3, Hess = TRUE)

#fiting mode lfor all clusters
#creating dummy variable for cluster type
#df3 <- mutate(df3, settlement_type_dummy = ifelse(settlement_type == "Rural", 0, 1))

#mmodel <- multinom(p_level2 ~ wealth_2 + edu_a + net_use_u5 + net_use_preg + 
#                     ACT_use_u5 + preg + hh_size + l_pop_den + ave_kap 
#                  + settlement_type_dummy, data = df3, Hess = TRUE)


summary(mmodel)


z <- summary(mmodel)$coefficients/summary(mmodel)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p


## extract the coefficients from the model and exponentiate
exp(coef(mmodel))

#odds ratio confidence intervals
exp(confint(mmodel))

#calculatIN predicted probabilities for each of our outcome levels using the fitted function. 
head(pp <- fitted(mmodel))

#examine the changes in predicted probability associated with wealth while holding the other constant

#Plotting the effects 

# plotting predicted classes 
pred1 <- mnl_pred_ova(model = mmodel,
                      data = df3,
                      xvari = "edu_a",
                      by = 0.1,
                      seed = "random", # default
                      nsim = 100, # faster
                      probs = c(0.025, 0.975)) # default


pred1$plotdata %>% head()

#Plotting Wealth while Holding all variables constant
ggplot(data = pred1$plotdata, aes(x = edu_a, 
                                  y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) + # Confidence intervals
  geom_line(size = 1) + # Mean
  facet_wrap(.~ p_level2, scales = "free_y", ncol = 2) +
  #scale_x_continuous(labels = percent_format()) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Educational Attainment") # Always label your axes ;)

# plotting predicted classes wealth
pred1 <- mnl_pred_ova(model = mmodel,
                      data = df3,
                      xvari = "wealth_2",
                      by = 0.1,
                      seed = "random", # default
                      nsim = 100, # faster
                      probs = c(0.025, 0.975)) # default


pred1$plotdata %>% head()

#Plotting Wealth while Holding all variables constant
ggplot(data = pred1$plotdata, aes(x = wealth_2, 
                                  y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) + # Confidence intervals
  geom_line(size = 1) + # Mean
  facet_wrap(.~ p_level2, scales = "free_y", ncol = 2) +
  #scale_x_continuous(labels = percent_format()) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Social Economic Status") # Always label your axes ;)

# plotting predicted classes KAP
pred1 <- mnl_pred_ova(model = mmodel,
                      data = df3,
                      xvari = "ave_kap",
                      by = 0.1,
                      seed = "random", # default
                      nsim = 100, # faster
                      probs = c(0.025, 0.975)) # default


pred1$plotdata %>% head()

#Plotting Wealth while Holding all variables constant
ggplot(data = pred1$plotdata, aes(x = ave_kap, 
                                  y = mean,
                                  ymin = lower, ymax = upper)) +
  geom_ribbon(alpha = 0.1) + # Confidence intervals
  geom_line(size = 1) + # Mean
  facet_wrap(.~ p_level2, scales = "free_y", ncol = 2) +
  #scale_x_continuous(labels = percent_format()) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Knowledge, Attitude and Practices") # Always label your axes ;)


#If we want first differences between two scenarios 
#we want to know what difference it makes to position 
#oneself on the lowest or highest end of the SES scale
fdif1 <- mnl_fd2_ova(model = mmodel,
                     data = df3,
                     xvari = "wealth_2",
                     value1 = min(df3$wealth_2),
                     value2 = max(df3$wealth_2),
                     nsim = 100)
#depicting the difference in extremes

ggplot(fdif1$plotdata_fd, aes(x = categories, 
                              y = mean,
                              ymin = lower, ymax = upper)) +
  geom_pointrange() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = percent_format()) +
  theme_bw() +
  labs(y = "Predicted probabilities",
       x = "Transmission intensity")

#plotting the differences in categories
fdif2 <- mnl_fd_ova(model = mmodel,
                    data = df3,
                    xvari = "wealth_2",
                    by = 1,
                    scenname = "settlement_type_dummy",
                    scenvalues = c(0,1),
                    nsim = 100)

#
fdif2$plotdata_fd %>% head()


ggplot(data = fdif2$plotdata, aes(x = wealth_2, 
                                  y = mean,
                                  ymin = lower, ymax = upper,
                                  group = as.factor(settlement_type_dummy),
                                  linetype = as.factor(settlement_type_dummy))) +
  geom_ribbon(alpha = 0.1) +
  geom_line(size = 1) +
  facet_wrap(. ~ p_level2, scales = "free_y", ncol = 2) +
  scale_y_continuous(labels) + # % labels
  scale_x_continuous(labels) +
  scale_linetype_discrete(name = "Cluster",
                          breaks = c(0, 1),
                          labels = c("all", "rural")) +theme_bw() +
  labs(y = "Predicted probabilities", 
       x = "Socia Economic Status") # Always label your axes ;)


