
x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "sp", "sf", "tmap", 'paletteer', 'cowplot', 
       'gridExtra', 'lme4', 'reshape2', 'Greg')




lapply(x, library, character.only = TRUE) #applying the library function to packages

options(repr.plot.width = 14, repr.plot.height = 8)

###################################################################################
####Directories
###################################################################################

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
BinDir <- file.path(NGDir, "bin")
SrcDir <- file.path(NGDir, "src", "DHS")
VarDir <- file.path(SrcDir, "1_variables_scripts")
ProjectDir <- file.path(NuDir, "projects", "hbhi_nigeria")
PresentDir<-file.path(NuDir, "presentations")
Personal_P_Dir <- file.path(NuDir, "presentations", "team member archive_Ifeoma", "210409_EPI_seminar", "pictures")
Sem_Dir <- file.path(NuDir, "presentations", "team member archive_Ifeoma", "210409_EPI_seminar")
Man_Dir <- file.path(ProjectDir, "project_notes", "publication", "Urban-rural determinants of malaria infection in Nigeria", "Illustrations")


###################################################################################
####Loading data
###################################################################################
# Load pre-clustered data:


# Load pre-clustered data:
clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), 
                               header = T, sep = ',')
urbandatasetsss <- clu_variales_10_18 %>% filter(Rural_urban == 1)


#filtering by residence type
urbandataset <- clu_variales_10_18 %>% filter(Rural_urban == 1) %>%  
  dplyr::select(hv001,p_test, wealth_2, u5_prop, preg,edu_a, hh_size, ACT_use_u5,pop_den,
                hh_members_age, sex_f, data_source, humidindex, Rural_urban, annual_precipitation,housing_qua, net_use, build_count, state, pop_count, housing_qua) %>% 
  na.omit()

table(urbandataset$data_source)

#data cleaning

missing_values <- sapply(urbandataset, function(x) sum(is.na(x)))
summary(urbandataset$pop_count)

urbandataset <- urbandataset %>% filter(annual_precipitation>=0, pop_den != -9999)
table(urbandataset$data_source)


###################################################################################
####glm
###################################################################################

############################   Univariate Analysis ##################################

#binalize dependent variable
pfpr <- read.csv(file=file.path(DataDir, 'urban_malaria_cluster_est', 'pfpr_DHS_10_15_18.csv'))
pfpr$y <- ifelse(pfpr$hml32 *100 < 10, 0, 1) 
hist(pfpr$y)


edu <- read.csv(file=file.path(DataDir, 'urban_malaria_cluster_est', 'edu_a_DHS_10_15_18.csv'))
hist(edu$edu_a)

df <- pfpr %>%left_join(edu, by=c('.id', 'hv001'))

df$edu_a = df$edu_a*100
df

edu_glm<- glm(y~ 1+ edu_a, data = df,
              family = "binomial")

summary(edu_glm)
exp(coef(edu_glm))** 5 # we would need to convert all our proportions to percentages to make the interpretations less clunky and use exponents of 5 unit increases

w_glm <- glm(y~ 1+ wealth_2,
             data = urbandataset, family = "binomial")

summary(w_glm)
exp(coef(w_glm))

results_w_glm = coefficients(summary(w_glm))
colnames(results_w_glm)[2] = "SE"
results_w_glm_df <- data.frame(results_w_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_w_glm_df["vars"] = c("(Intercept_c)", "wealth_2_c")

#### education 

e_glm <- glm(y~ 1+ edu_a,
             data = urbandataset, family = "binomial")

summary(e_glm)
exp(coef(e_glm))

results_e_glm = coefficients(summary(e_glm))
colnames(results_e_glm)[2] = "SE"
results_e_glm_df <- data.frame(results_e_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_e_glm_df["vars"] = c("(Intercept_c)", "edu_a_c")

###sex
sex_fglm <- glm(y~ 1+ sex_f,
                data = urbandataset, family = "binomial")

summary(sex_fglm)
exp(coef(sex_fglm))

results_sex_fglm = coefficients(summary(sex_fglm))
colnames(results_sex_fglm)[2] = "SE"
results_sex_fglm_df <- data.frame(results_sex_fglm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_sex_fglm_df["vars"] = c("(Intercept_c)", "sex_f_c")

#ACT
ACT_use_u5_glm <- glm(y~ 1+ ACT_use_u5,
                      data = urbandataset, family = "binomial")

summary(ACT_use_u5_glm)
exp(coef(ACT_use_u5_glm))

results_ACT_use_u5_glm = coefficients(summary(ACT_use_u5_glm))
colnames(results_ACT_use_u5_glm)[2] = "SE"
results_ACT_use_u5_glm_df <- data.frame(results_ACT_use_u5_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_ACT_use_u5_glm_df["vars"] = c("(Intercept_c)", "ACT_use_u5_c")

#net use
net_use_glm <- glm(y~ 1+ net_use,
                   data = urbandataset, family = "binomial")

summary(net_use_glm)
exp(coef(net_use_glm))

results_net_use_glm = coefficients(summary(net_use_glm))
colnames(results_net_use_glm)[2] = "SE"
results_net_use_glm_df <- data.frame(results_net_use_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_net_use_glm_df["vars"] = c("(Intercept_c)", "net_use_c")

#humidity
humidindex_glm <- glm(y~ 1+ humidindex,
                      data = urbandataset, family = "binomial")

summary(humidindex_glm)
exp(coef(humidindex_glm))

results_humidindex_glm = coefficients(summary(humidindex_glm))
colnames(results_humidindex_glm)[2] = "SE"
results_humidindex_glm_df <- data.frame(results_humidindex_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_humidindex_glm_df["vars"] = c("(Intercept_c)", "humidindex_c")

#precipitation
annual_precipitation_glm <- glm(y~ 1+ annual_precipitation,
                                data = urbandataset, family = "binomial")

summary(annual_precipitation_glm)
exp(coef(annual_precipitation_glm))

results_annual_precipitation_glm = coefficients(summary(annual_precipitation_glm))
colnames(results_annual_precipitation_glm)[2] = "SE"
results_annual_precipitation_glm_df <- data.frame(results_annual_precipitation_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_annual_precipitation_glm_df["vars"] = c("(Intercept_c)", "annual_precipitation_c")

#hh size
hh_size_glm <- glm(y~ 1+ hh_size,
                   data = urbandataset, family = "binomial")

summary(hh_size_glm)
exp(coef(hh_size_glm))

results_hh_size_glm = coefficients(summary(hh_size_glm))
colnames(results_hh_size_glm)[2] = "SE"
results_hh_size_glm_df <- data.frame(results_humidindex_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_humidindex_glm_df["vars"] = c("(Intercept_c)", "hh_size_c")

#hh age
hh_members_age_glm <- glm(y~ 1+ hh_members_age,
                          data = urbandataset, family = "binomial")

summary(hh_members_age_glm)
exp(coef(hh_members_age_glm))

results_hh_members_age_glm = coefficients(summary(hh_members_age_glm))
colnames(results_hh_members_age_glm)[2] = "SE"
results_hh_members_age_glm_df <- data.frame(results_hh_members_age_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_hh_members_age_glm_df["vars"] = c("(Intercept_c)", "hh_members_age_c")

#pop dens
pop_count_glm <- glm(y~ 1+ pop_count,
                     data = urbandataset, family = "binomial")

summary(pop_count_glm)
exp(coef(pop_count_glm))

results_pop_count_glm = coefficients(summary(pop_count_glm))
colnames(results_pop_count_glm)[2] = "SE"
results_pop_count_glm_df <- data.frame(results_pop_count_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_pop_count_glm_df["vars"] = c("(Intercept_c)", "pop_count_c")

#building count
build_count_glm <- glm(y~ 1+ build_count,
                       data = urbandataset, family = "binomial")

summary(build_count_glm)
exp(coef(build_count_glm))

results_build_count_glm = coefficients(summary(build_count_glm))
colnames(results_build_count_glm)[2] = "SE"
results_build_count_glm_df <- data.frame(results_build_count_glm)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))

results_build_count_glm_df["vars"] = c("(Intercept_c)", "build_count_c")

#merging univeriate results
univ_merge <- dplyr::bind_rows(results_w_glm_df, results_e_glm_df, results_sex_fglm_df, results_net_use_glm_df, results_humidindex_glm_df, 
                               results_annual_precipitation_glm_df, results_humidindex_glm_df, results_hh_members_age_glm_df, results_pop_count_glm_df, 
                               results_ACT_use_u5_glm_df, results_build_count_glm_df)

univ_merge <- univ_merge[univ_merge$vars != "(Intercept_c)",]
write_csv(univ_merge, file.path(Man_Dir, 'univ_merged_odds.csv'))

#############################################################################################
########################multivariable model comparisons####################################


###############multivariable model with all variables ####################
model1 <- glm(y ~ edu_a + wealth_2 +  annual_precipitation +  net_use + sex_f + hh_size + pop_count  + hh_members_age
              + ACT_use_u5 + humidindex, data = urbandataset, binomial)

summary(model1)


##################significant model ##################333

model3 <- glm(y ~ edu_a + wealth_2, data = urbandataset, binomial) 




#multivariable model comparisons adding net use


model1use <- glm(y ~ edu_a + wealth_2 +  net_use +  annual_precipitation + sex_f + hh_size + pop_count  + hh_members_age
                 + ACT_use_u5 + humidindex, data = urbandataset, binomial)


delta_coef_use <- abs((coef(model3)-coef(model1use)[1:3])/
                        coef(model1use)[1:3]) 

delta_coef_use <- as.data.frame(round(delta_coef_use, 3))


#multivariable model comparisons adding annual_precipitation
model1w <- glm(y ~ edu_a + wealth_2  + annual_precipitation + sex_f + hh_size + pop_count  + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = urbandataset, binomial)


delta_coef_annual_precipitation <- abs((coef(model3)-coef(model1w)[1:3])/
                                         coef(model1w)[1:3]) 

delta_coef_annual_precipitation <- as.data.frame(round(delta_coef_annual_precipitation, 3))


#multivariable model comparisons adding hh_size 
model1a <- glm(y ~ edu_a + wealth_2 + hh_size + sex_f  + annual_precipitation + pop_count  + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = urbandataset, binomial)


delta_coef_hh_size <- abs((coef(model3)-coef(model1a)[1:3])/
                            coef(model1a)[1:3]) 

delta_coef_hh_size <- as.data.frame(round(delta_coef_hh_size, 3))


#multivariable model comparisons adding pop_count 
model1b <- glm(y ~ edu_a + wealth_2 + pop_count + hh_size + sex_f + annual_precipitation + hh_members_age
               + net_use + ACT_use_u5 + humidindex, data = urbandataset, binomial)


delta_coef_pop_den <- abs((coef(model3)-coef(model1b)[1:3])/
                            coef(model1b)[1:3]) 

delta_coef_pop_den <- as.data.frame(round(delta_coef_pop_den, 3))



#multivariable model comparisons adding hh_members_age
model1c <- glm(y ~ edu_a + wealth_2 + hh_members_age + pop_count + hh_size + sex_f + annual_precipitation
               + net_use + ACT_use_u5 + humidindex, data = urbandataset, binomial)


delta_coef_hh_age <- abs((coef(model3)-coef(model1c)[1:3])/
                           coef(model1c)[1:3]) 

delta_coef_hh_age <- as.data.frame(round(delta_coef_hh_age, 3))


#multivariable model comparisons adding ACT_use_u5  
model1f <- glm(y ~ edu_a  + wealth_2 + ACT_use_u5 + sex_f  + annual_precipitation + net_use + hh_members_age + pop_count + hh_size
               + humidindex, data = urbandataset, binomial)


delta_coef_ACT <- abs((coef(model3)-coef(model1f)[1:3])/
                        coef(model1f)[1:3]) 

delta_coef_ACT <- as.data.frame(round(delta_coef_ACT, 3))


#multivariable model comparisons adding  humidindex
model1g <- glm(y ~ edu_a  + wealth_2 + humidindex + ACT_use_u5 + sex_f  + annual_precipitation + net_use + 
                 hh_members_age + pop_count + hh_size, data = urbandataset, binomial)


delta_coef_humidindex <- abs((coef(model3)-coef(model1g)[1:3])/
                               coef(model1g)[1:3]) 

delta_coef_humidindex <- as.data.frame(round(delta_coef_humidindex, 3))
#multivariable model comparisons adding  net use
model1g <- glm(y ~ edu_a + wealth_2 +  net_use + sex_f + annual_precipitation + humidindex + 
                 hh_members_age + pop_count + hh_size + wealth_2, data = urbandataset, binomial)


delta_coef_net_use <- abs((coef(model3)-coef(model1g)[1:3])/
                            coef(model1g)[1:3]) 


delta_coef_net_use <- as.data.frame(round(delta_coef_net_use, 3))

#multivariable model comparisons adding  building count
#model1g <- glm(y ~ edu_a + sex_f + annual_precipitation +  net_use + humidindex + 
#                 hh_members_age + pop_count + hh_size + wealth_2, data = urbandataset, binomial)


summary(model1g)

delta_coef_build <- abs((coef(model3)-coef(model1g)[1:3])/
                          coef(model1g)[1:3]) 


delta_coef_build <- as.data.frame(round(delta_coef_build, 3))

#Creating dataframe for computed coeficient difference 
delta_coef_df <- merge(delta_coef_pop_den, delta_coef_annual_precipitation, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_size, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_ACT, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_hh_age, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_humidindex, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_net_use, by="row.names", all=TRUE)
delta_coef_df <- delta_coef_df %>% remove_rownames %>% column_to_rownames(var="Row.names")
delta_coef_df <- merge(delta_coef_df,delta_coef_build, by="row.names", all=TRUE)


delta_coef_df <- na.omit(delta_coef_df)

#Visualizing the percentage change that each  additioin variable makes to the education
#variable after being added to  the model. 
delta_df <- delta_coef_df[c(1), ]


#plot of delta_coef
df1 <- melt(delta_df,"Row.names")

g1 <- ggplot(df1, aes(x = variable, y = value)) +
  geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  scale_y_continuous(breaks = seq(0, 0.55, by = 0.1), limits=c(0,0.55))+
  theme(axis.text.x = element_text(angle=-40, hjust=.1))


g1

#Visualizing the percentage change that each  additioin variable makes to the Wealth
#variable after being added to  the model. 

delta_df <- delta_coef_df[c(2), ]

#plot of delta_coef
df1 <- melt(delta_df,"Row.names")

g1 <- ggplot(df1, aes(x = variable, y = value)) +
  geom_bar(aes(fill= variable),stat="identity", position ="dodge") + 
  theme_bw()+ 
  scale_y_continuous(breaks = seq(0, 0.55, by = 0.1), limits=c(0,0.55))+
  theme(axis.text.x = element_text(angle=-40, hjust=.1))


g1


#We see that Humidity index and household size did not change the significant variables'
#estimate by >10. 

#Therefore, our multivariable model of choice is below. 


u_glm <- glm(y~ 1+ wealth_2+ edu_a + sex_f+  ACT_use_u5 + build_count +
               pop_count + hh_members_age + net_use  + annual_precipitation,
             data = urbandataset, family = "binomial")
summary(u_glm)

printCrudeAndAdjustedModel(u_glm)[-1,]

