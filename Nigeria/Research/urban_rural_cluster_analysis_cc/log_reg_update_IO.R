### ==========================================================================================
### Urban-rural analysis- Nigeria: Logistic regression model to understand the determinants of malaria prevalence in urban and rural areas respectively 
### This script is for descriptive analyses and generates odds ratios and predicted probabilities for urban and rural clusters from the DHS. 
### Check file paths and make sure they are correct. That is Box folders are correctly linked, as well as the data and script directory 
### September 2020, Ifeoma Doreen Ozodiegwu
### ==========================================================================================
rm(list = ls())


## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("mambrose" %in% user) {
  user_path <- file.path("C:/Users", user)
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "NU-malaria-team")
  NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  SrcDir <- file.path(NGDir, "src", "DHS")
  BinDir <- file.path(NGDir, "bin")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Box", "NU-malaria-team")
  NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
  DataDir <-file.path(NGDir, "data")
  ResultDir <-file.path(NGDir, "results")
  PlotDir <-file.path(ResultDir, "research_plots")
  BinDir <- file.path(NGDir, "bin")
  SrcDir <- file.path(NGDir, "src", "Research", "urban_rural_cluster_analysis_cc")
  ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
}

#Read in functions 

source(file.path(SrcDir, "functions", "log_reg_functions.R"))


histogram <- T 
type <- "urban/rural"


# Read in data file 

if(histogram == T) {
    merged_df <- read.csv(file.path(DataDir, "Nigereia_2010_2018_clustered_final_dataset.csv"), header= TRUE)


    df <- merged_df[ ,colnames(merged_df) 
                    %in% c("wealth_2", "preg","edu_a","net_use_u5", 
                           "net_use_preg", "hh_size", "ACT_use_u5",
                           "l_pop_den","p_test", "humidindex", "Rural_urban")]
    

    df <- df%>% mutate(scaled_hudix = scale(df$humidindex, center = T)) %>%  filter(ACT_use_u5 <= 1, preg <= 1) %>%  drop_na()
    if(type!="combined"){
      df_split <- split(df, df$Rural_urban)
      df_split_long <- lapply(df_split, function(x) gather(x, key = "text", value = "value"))
      new_labels <- c("% in higher wealth quintile", "% pregnant women", "% with higher education", "% U5 slept under a net",
                      "% preg slept under net", "Household size", "% U5 that use ACT", "Population density",
                      "Parasite prevalence", "Humidity index (HI)", "Residence", "Scaled HI") 
      names(new_labels)<-c("wealth_2", "preg","edu_a","net_use_u5", 
                           "net_use_preg", "hh_size", "ACT_use_u5",
                           "l_pop_den","p_test", "humidindex", "Rural_urban", "scaled_hudix")
      
      histo_list <- lapply(df_split_long, histofun)
      ggsave("urban_histograms.pdf", plot =histo_list[[1]], path=file.path(PlotDir, "histograms"))
      ggsave("rural_histograms.pdf", plot =histo_list[[2]], path=file.path(PlotDir, "histograms"))
      }else if(type == "combined"){
      df_combo <- df %>%  gather(key = "text", value = "value")
      new_labels <- c("% in higher wealth quintile", "% pregnant women", "% with higher education", "% U5 slept under a net",
                      "% preg slept under net", "Household size", "% U5 that use ACT", "Population density",
                      "Parasite prevalence", "Humidity index (HI)", "Residence", "Scaled HI") 
      names(new_labels)<-c("wealth_2", "preg","edu_a","net_use_u5", 
                           "net_use_preg", "hh_size", "ACT_use_u5",
                           "l_pop_den","p_test", "humidindex", "Rural_urban", "scaled_hudix")
      histo<- histofun(df_combo)
      ggsave("combined_histograms.pdf", plot =histo, path=file.path(PlotDir, "histograms"))
      }else{
      print("dataset created but no plots")
    }
    
    
}else{
  print("histograms will not be made")
}


  




#Independent variables

#Plotting scatters
if(scatter = T){
  df_list <-lapply(df_split,function(x) x[!(names(x) %in% c("scaled_hudix", "Rural_urban"))])
  corr_ls<- lapply(df_list, function(x) ggpairs(x, title=paste("correlogram", names(x))))
  ggsave("urban_correlation.pdf", plot =corr_ls[[1]], path=file.path(PlotDir, "correlations"))
  ggsave("rural_correlation.pdf", plot =corr_ls[[2]], path=file.path(PlotDir, "correlations"))
  cor_viz <-lapply(df_list, function(x) ggcorr(x, method = c("everything", "pearson")))
  ggsave("urban_correlation_blocks.pdf", plot =cor_viz[[1]], path=file.path(PlotDir, "correlations"))
  ggsave("rural_correlation_blocks.pdf", plot =cor_viz[[2]], path=file.path(PlotDir, "correlations"))
}



# this part is not updated 

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
                    ACT_use_u5 + hh_size + scaled_hudix + l_pop_den + Rural_urban,
             data = df3, family = "binomial")


summary(mmodel)

plot_model(mmodel, title = " ", line.size = 0.7, dot.size = 1.3) + ylim(0, 2)


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

df_edu_urban_pred <- data.frame(edu_prob$edu_a$x,expit(edu_prob$edu_a$fit),expit(edu_prob$edu_a$lower), expit(edu_prob$edu_a$upper), cat= "urban")

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
