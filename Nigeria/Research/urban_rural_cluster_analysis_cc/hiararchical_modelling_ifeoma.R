rm(list=ls())

x <- c("tidyverse","INLA", "ggplot2", "ggpubr", "inlabru", "rgdal", "Sp", "sf", "tmap")



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


#setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data")



###################################################################################
####Loading data
###################################################################################
# Load pre-clustered data:
clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), 
                 header = T, sep = ',')

table(clu_variales_10_18$state)

# Binarize response:
clu_variales_10_18$y <- ifelse(clu_variales_10_18$p_test < 0.1, 0,1)

#cleaning precip data
clu_variales_10_18 <- clu_variales_10_18%>% mutate(annual_precipitation = scale(clu_variales_10_18$annual_precipitation, center = T))
#dat2[which(dat2$pop_den<0),]

clu_variales_10_18 <- clu_variales_10_18 %>% mutate(pop_den = na_if(pop_den, -9999))
clu_variales_10_18$annual_precipitation <- replace(clu_variales_10_18$annual_precipitation, which(clu_variales_10_18$annual_precipitation < 0), NA)
clu_variales_10_18 <- clu_variales_10_18 %>% mutate(log_pop_den = log(pop_den))







#sub setting variables of interest 
clu_variales_10_18 <- clu_variales_10_18[,c("hv001", "y", "wealth_2", "edu_a", "hh_size",
                                            "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex",
                                            "annual_precipitation", "net_use", "Rural_urban", "data_source", "state", "build_count", "region")]


# Create reduced dataset for model 2:
# (filter out rows with NA values)


#filtering by residence tyoe
urbandataset <- clu_variales_10_18 %>% filter(Rural_urban == 1)
ruraldataset <-clu_variales_10_18 %>% filter(Rural_urban == 2)
comineddataset <- clu_variales_10_18



###################################################################################
####rural dataset
###################################################################################
  


#regular rural model without random effect 
r_rmod <- inla(y ~ 1 + wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
               data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE)) 

summary(r_rmod)



#model with random intercept in state and random slope in education
ruraldataset$state_2 <- ruraldataset$state
r_iid_s <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
              + humidindex +f(state, model = "iid") + f(state_2, net_use, model = "iid"), family = 'binomial',
              data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
              control.compute = list(cpo=TRUE, dic = TRUE))

summary(r_iid_s) 




#model with random intercept in state 
r_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
              + humidindex +f(state, model = "iid"), family = 'binomial',
              data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
              control.compute = list(cpo=TRUE, dic = TRUE))

summary(r_iid)


###################################################################################
####final rural model
###################################################################################

#model with random intercept in state and region
r_iid2 <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
               + humidindex +f(state, model = "iid") + f(region, model = "iid") +  f(state_2, net_use, model = "iid"), family = 'binomial',
               data = ruraldataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE))

summary(r_iid2) # model with the lowest dic and marginal loglikelihood

#C##################onverting between  Log-odds to Probability#################

#extraction and converting results to dataframe
r_iid2_fixed <- as.data.frame(r_iid2$summary.fixed)

#making column names a column
r_iid2_fixed <- cbind(coefs = rownames(r_iid2_fixed), r_iid2_fixed)
rownames(r_iid2_fixed) <- 1:nrow(r_iid2_fixed)

#changing row names
names(r_iid2_fixed)[names(r_iid2_fixed) == "0.975quant"] <- "logodds975"
names(r_iid2_fixed)[names(r_iid2_fixed) == "0.5quant"] <- "logodds5"
names(r_iid2_fixed)[names(r_iid2_fixed) == "0.025quant"] <- "logodds25"

#
r_iid2_prob <- r_iid2_fixed %>% mutate(probs_0.975 = plogis(logodds975))
r_iid2_prob <- r_iid2_prob %>% mutate(probs_0.5 = plogis(logodds5))
r_iid2_prob <- r_iid2_prob %>% mutate(probs_0.025 = plogis(logodds25))
r_iid2_prob <- r_iid2_prob[,c("coefs", "probs_0.025", "probs_0.5", "probs_0.975")]

#We can calculate the probabilites that malaria prevalence rates are higher for all hospitals
#These exceedance probabilities indicate that the probability that higher malaria prevalence 
#exceeds 0.1 is highest for observation number 1138 (probability equal to 1)

#pros <- sapply(r_iid2$marginals.fitted.values,
#               FUN = function(marg){1-inla.pmarginal(q = 0.1, marginal = marg)})

#probs_df <- as.data.frame(pros)

#plots of the posterior for the betas 

plot_fun<- function(data, x_label){
  ggplot(data.frame(inla.smarginal(data)), aes(x, y)) +
    geom_line(color = "green") +
    theme_bw()+
    geom_vline(xintercept=0, linetype="dashed", color = "red")+
    xlab(x_label)+
    ylab("")
}


data <- list(r_iid2$marginals.fixed$`(Intercept)`,r_iid2$marginals.fixed$wealth_2, r_iid2$marginals.fixed$edu_a, r_iid2$marginals.fixed$net_use, 
             r_iid2$marginals.fixed$hh_size, r_iid2$marginals.fixed$ACT_use_u5, r_iid2$marginals.fixed$hh_members_age,
             r_iid2$marginals.fixed$sex_f, r_iid2$marginals.fixed$`log(annual_precipitation)`, r_iid2$marginals.fixed$`log(build_count)`,
             r_iid2$marginals.fixed$humidindex)

labels_data <- list("intercept", "Highest wealth quintile", "Education", "Bednet Use", "Average Household size", "ACT_use",
                    "Average Household age", "Proportion of females", "log(annual precipitation)", 
                    "log(build_count)", "humidity index")

plots<-map2(data, labels_data, plot_fun)

figure<-ggarrange(plotlist = plots, nrow =4, ncol=3)
figure<-annotate_figure(figure, left = "Density")
figure


#extracting state random intercept 
r_random_effects_ <- r_iid2$summary.random[[1]]

#extracting factors 
r_random_effects_$ID <-str_to_title(r_random_effects_$ID)
r_random_effects_$ID <- factor(r_random_effects_$ID, levels=rev(r_random_effects_$ID))
r_random_effects_$ID<- trimws(r_random_effects_$ID)


#quick plot of state_random intercept 

r_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(r_fp)# no significant state variations 


#map 

#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)




state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)
))

r_map_df <- left_join(state_sf, r_random_effects_, by =c("NAME_1" = "ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
r_map <- tm_shape(r_map_df)+
  tm_polygons(col = "mean", midpoint =NA, palette = "-RdYlGn")+
  tm_text("NAME_1")

r_map


#extracting region random intercept 
r_random_effects_ <- r_iid2$summary.random[[2]]
re_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(re_fp)#significant variation in the south west increasing likelihood of transmission and decreased likelihood in the north 


#map 



#############################################################################################
#########regional map
#read in state shape file 
regionshp <-  readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
region_sf <- st_as_sf(regionshp)



region_sf <- region_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                         NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                         TRUE ~ as.character(NAME_1)
))


                     
regions <- clu_variales_10_18[,c("state", "region")]


region_sf <- region_sf %>% mutate(NAME_1 = tolower(NAME_1))


region_sf <- left_join(region_sf, regions, by =c("NAME_1" = "state"))

#We have missing data in three states below, we assign the region mannually
region_sf$region[which(region_sf$NAME_1 =="kwara")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="plateau")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="zamfara")]<-"north west"

#grouping states into regions
region_sf <- region_sf %>% group_by(NAME_1) %>% slice(1L)
region_sf_na<-subset(region_sf,region_sf$region=="NA")

r_map_df <- left_join(region_sf, r_random_effects_, by =c("region" = "ID"))
#r_map_df  <- r_map_df %>% group_by(region) %>% slice(1L)
r_map_df <- st_as_sf(r_map_df)

r_map_df%>% group_by(region) %>% 
  summarise(geometry = sf::st_union(geometry)) %>% ungroup()

# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
r_map <- tm_shape(r_map_df)+
  tm_fill(col = "mean", midpoint =NA, palette = "-RdYlGn", breaks = c(-1, -0.5, -0.16, 0, 0.05, 1, 1.5)) +
  tm_borders() +
  tm_text("NAME_1", size = 0.8, col = "snow4")

r_map



###################################################################################
####urban dataset
###################################################################################
#regular urban model without random effect 
u_rmod <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
               hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
             data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE)) 

summary(u_rmod)

#model with random effects in state
u_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
              hh_members_age + sex_f + log(annual_precipitation) +log(build_count)+  humidindex +f(state, model = "iid"), family = 'binomial',
            data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE))

summary(u_iid)


library(lme4)
#st_geometry(urbandataset) <- NULL
# 
#urbandataset$log_annual_precipitation<-log(urbandataset$annual_precipitation)
#urbandataset$log_build_count<-log(urbandataset$build_count)
#urbandataset$annual_precipitation<-NULL
# 
#urbandataset$build_count<-NULL
# u_lm <- glmer(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
#                 hh_members_age + sex_f + log_annual_precipitation +log_build_count+(1|state),family = binomial(link = "logit"),
#               data = urbandataset)
# 
# 
# 
num_cols<-c("wealth_2", "edu_a", "net_use", "hh_size", "ACT_use_u5",
             "hh_members_age", "sex_f", "log_annual_precipitation" , "log_build_count")
# 
# 
urbandataset_scaled  <- urbandataset
urbandataset_scaled [,num_cols] <- scale(urbandataset_scaled [,num_cols])
# m1_sc <- update(u_lm,data=urbandataset_scaled )
# summary(m1_sc)




u_lm <- glmer(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log_annual_precipitation +log_build_count+(1|state),family = binomial(link = "logit"),
              data = urbandataset_scaled)

summary(u_lm)

#extracting state random effects 
u_random_effects <- u_iid$summary.random

#changing to dataframe
u_random_effects_<- do.call(cbind, u_random_effects)

#extracting factos 
u_random_effects_$state.ID <-str_to_title(u_random_effects_$state.ID)
u_random_effects_$state.ID <- factor(u_random_effects_$state.ID, levels=rev(u_random_effects_$state.ID))
u_random_effects_$state.ID<- trimws(u_random_effects_$state.ID)

#quick plot of state random effect. South-West state have statistically significant effects
library(ggplot2)
u_fp <- ggplot(data=u_random_effects_, aes(x=state.ID, y=state.mean, ymin=state.0.025quant, ymax=state.0.975quant)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(u_fp)


#map
u_map_df <- left_join(state_sf, u_random_effects_, by =c("NAME_1" = "state.ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
u_map <- tm_shape(u_map_df)+
  tm_polygons(col = "state.mean", midpoint =NA, palette = "-RdYlGn", breaks =c(-1.5, -1.0, -0.5, 0, 0.2, 1.0, 1.5, 2.0))+
  tm_text("NAME_1")

u_map


#regular urban inlamodel without random effect 
u_rmod <- inla(y ~ 1 + wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count) + humidindex, family = 'binomial',
               data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE)) 

summary(u_rmod)



#model with random intercept in state and random slope in education
urbandataset$state_2 <- urbandataset$state
u_iid_s <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                  hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
                + humidindex +f(state, model = "iid") + f(state_2, net_use, model = "iid"), family = 'binomial',
                data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
                control.compute = list(cpo=TRUE, dic = TRUE))

summary(u_iid_s) 




#model with random intercept in state 
u_iid <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
              + humidindex +f(state, model = "iid"), family = 'binomial',
              data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
              control.compute = list(cpo=TRUE, dic = TRUE))

summary(u_iid)


###################################################################################
####final urban model
###################################################################################

#model with random intercept in state and region
urbandataset$state_2 <- urbandataset$state
u_iid2 <- inla(y ~ 1+ wealth_2 + edu_a + net_use + hh_size + ACT_use_u5 +
                 hh_members_age + sex_f + log(annual_precipitation) +log(build_count)
               + humidindex +f(state, model = "iid") + f(region, model = "iid") +  f(state_2, net_use, model = "iid"), family = 'binomial',
               data = urbandataset, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE))

summary(u_iid2) # model with the lowest dic and marginal loglikelihood


#extraction and converting results to dataframe
u_iid2_fixed <- as.data.frame(u_iid2$summary.fixed)

#making column names a column
u_iid2_fixed <- cbind(coefs = rownames(u_iid2_fixed), u_iid2_fixed)
rownames(u_iid2_fixed) <- 1:nrow(u_iid2_fixed)

#changing row names
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.975quant"] <- "logodds975"
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.5quant"] <- "logodds5"
names(u_iid2_fixed)[names(u_iid2_fixed) == "0.025quant"] <- "logodds25"

#
u_iid2_prob <- u_iid2_fixed %>% mutate(probs_0.975 = plogis(logodds975))
u_iid2_prob <- u_iid2_prob %>% mutate(probs_0.5 = plogis(logodds5))
u_iid2_prob <- u_iid2_prob %>% mutate(probs_0.025 = plogis(logodds25))
u_iid2_prob <- u_iid2_prob[,c("coefs", "probs_0.025", "probs_0.5", "probs_0.975")]
u_iid2_prob

#pros <- sapply(u_iid2$marginals.fitted.values,
#               FUN = function(marg){1-inla.pmarginal(q = 0.1, marginal = marg)})

#probs_df <- as.data.frame(pros)

#plots of the posterior for the betas 

plot_fun<- function(data, x_label){
  ggplot(data.frame(inla.smarginal(data)), aes(x, y)) +
    geom_line(color = "green") +
    theme_bw()+
    geom_vline(xintercept=0, linetype="dashed", color = "red")+
    xlab(x_label)+
    ylab("")
}


data <- list(u_iid2$marginals.fixed$`(Intercept)`,u_iid2$marginals.fixed$wealth_2, u_iid2$marginals.fixed$edu_a, u_iid2$marginals.fixed$net_use, 
             u_iid2$marginals.fixed$hh_size, u_iid2$marginals.fixed$ACT_use_u5, u_iid2$marginals.fixed$hh_members_age,
             u_iid2$marginals.fixed$sex_f, u_iid2$marginals.fixed$`log(annual_precipitation)`, u_iid2$marginals.fixed$`log(build_count)`,
             u_iid2$marginals.fixed$humidindex)

labels_data <- list("intercept", "Highest wealth quintile", "Education", "Bednet Use", "Average Household size", "ACT_use",
                    "Average Household age", "Proportion of females", "log(annual precipitation)", 
                    "log(build_count)", "humidity index")

plots<-map2(data, labels_data, plot_fun)

figure<-ggarrange(plotlist = plots, nrow =4, ncol=3)
figure<-annotate_figure(figure, left = "Density")
figure


#extracting state random intercept 
r_random_effects_ <- u_iid2$summary.random[[1]]

#extracting factors 
r_random_effects_$ID <-str_to_title(r_random_effects_$ID)
r_random_effects_$ID <- factor(r_random_effects_$ID, levels=rev(r_random_effects_$ID))
r_random_effects_$ID<- trimws(r_random_effects_$ID)


#quick plot of state_random intercept 

r_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(r_fp)# no significant state variations 


#map 

#read in state shape file 
stateshp <- readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
state_sf <- st_as_sf(stateshp)




state_sf <- state_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                   NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                   TRUE ~ as.character(NAME_1)
))

r_map_df <- left_join(state_sf, r_random_effects_, by =c("NAME_1" = "ID"))


# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
r_map <- tm_shape(r_map_df)+
  tm_polygons(col = "mean", midpoint =NA, palette = "-RdYlGn")+
  tm_text("NAME_1")

r_map


#extracting region random intercept 
r_random_effects_ <- u_iid2$summary.random[[2]]
re_fp <- ggplot(data=r_random_effects_, aes(x=ID, y=mean, ymin=`0.025quant`, ymax=`0.975quant`)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Label") + ylab("Mean (95% CI)") +
  theme_bw()  # use a white background
print(re_fp)#significant variation in the south west increasing likelihood of transmission and decreased likelihood in the north 


#map 



#############################################################################################
#########regional map
#read in state shape file 
regionshp <-  readOGR(file.path(DataDir,"gadm36_NGA_shp"), layer ="gadm36_NGA_1", use_iconv=TRUE, encoding= "UTF-8")
region_sf <- st_as_sf(regionshp)



region_sf <- region_sf %>% mutate(NAME_1 = case_when(NAME_1 == "Federal Capital Territory" ~ "Fct Abuja",
                                                     NAME_1 == "Nassarawa" ~ "Nasarawa",
                                                     TRUE ~ as.character(NAME_1)
))



regions <- clu_variales_10_18[,c("state", "region")]


region_sf <- region_sf %>% mutate(NAME_1 = tolower(NAME_1))


region_sf <- left_join(region_sf, regions, by =c("NAME_1" = "state"))

#We have missing data in three states below, we assign the region mannually
region_sf$region[which(region_sf$NAME_1 =="kwara")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="plateau")]<-"north central"
region_sf$region[which(region_sf$NAME_1 =="zamfara")]<-"north west"

#grouping states into regions
region_sf <- region_sf %>% group_by(NAME_1) %>% slice(1L)
region_sf_na<-subset(region_sf,region_sf$region=="NA")

r_map_df <- left_join(region_sf, r_random_effects_, by =c("region" = "ID"))
#r_map_df  <- r_map_df %>% group_by(region) %>% slice(1L)
r_map_df <- st_as_sf(r_map_df)

r_map_df%>% group_by(region) %>% 
  summarise(geometry = sf::st_union(geometry)) %>% ungroup()

# we see spatial clustering of state-level random effects, although state effects are not statistically significant 
r_map <- tm_shape(r_map_df)+
  tm_fill(col = "mean", midpoint =NA, palette = "-RdYlGn") +
  tm_borders() +
  tm_text("NAME_1", size = 0.8, col = "snow4")

r_map
