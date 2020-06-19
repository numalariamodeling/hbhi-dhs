# clean and select pfpr data 

pfpr_data <- dhs_list[[3]] # uses the DHS person recode dataset 

look_for(dhs_list[[3]], "smear")

table(pfpr_data$hml32) # frequency table for smear test 

#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_df <- pfpr_data %>% filter(hv042 == "1" & hv103 == "1" & hc1 %in% c(6:59) & hml32 %in% c(0, 1,6))





# prep dataset for cluster level analysis - we start with urban cluster analysis 

val_labels(pfpr_df$hv025) # value labels for place of residence, 1 = urban and 2 = rural

pfpr_place <- pfpr_df %>% filter(hv025 == 1)
pfpr_dhs <- pfpr_data%>% filter(hv025 == 1)


# estimate cluster-level malaria prevalence

pfpr_place<- dataclean.para(pfpr_place, hv005, hc1, hml32, 'hml32', 'p_test') 

svy_mal <- svydesign.fun(pfpr_place)

clu_est <- result.fun('p_test', 'hv001', design=svy_mal, pfpr_place, "hv007")
head(clu_est)


# next we estimate proportion of people in high SES by cluster
# recode the weath quintile variable 
table(pfpr_df$hv270)

pfpr_wealth <- pfpr_df %>%  mutate(wealth = ifelse(hv270 <4, 0, 1))
table(pfpr_wealth$wealth)

pfpr_wealth<- dataclean.para(pfpr_wealth, hv005, hv005, wealth, 'wealth', 'wealth_2') 
table(pfpr_wealth$wealth_2)

svyd_wealth<- svydesign.fun(pfpr_wealth)

clu_wealth <- result.fun('wealth_2', 'hv001', design=svyd_wealth, pfpr_wealth, "hv007")
head(clu_wealth)

# next estimate the proportion of U5 children in each cluster 

look_for(dhs_list[[3]], "age")
summary(is.na(pfpr_dhs$hc1))

pfpr_u5 <- pfpr_dhs %>%  mutate(u5_prop = ifelse(hml16 <=4, 1,0))
table(pfpr_u5$u5_prop)


pfpr_u5<- dataclean.para(pfpr_u5, hv005, hv005, u5_prop, 'u5_prop', 'u5_prop') 
table(pfpr_u5$u5_prop)


svyd_u5 <- svydesign.fun(pfpr_u5)

clu_u5_prop <- result.fun('u5_prop', 'hv001', design=svyd_u5, pfpr_u5, "hv007")
head(clu_u5_prop)


# estimate proportion of pregnant women in each cluster 
look_for(dhs_list[[3]], "pregnant")
table(dhs_list[[3]]$ha54)

pfpr_preg<- dataclean.para(pfpr_dhs, hv005, hv005, ha54, 'ha54', 'preg') 
table(pfpr_preg$preg)


svyd_preg <- svydesign.fun(pfpr_preg)

clu_preg <- result.fun('preg', 'hv001', design=svyd_preg, pfpr_preg, "hv007")
head(clu_preg)

# proportion with secondary or greater education 
look_for(dhs_list[[3]], "education")
val_labels(dhs_list[[3]]$hv106)

pfpr_h_edu <- pfpr_dhs %>%  mutate(edu_a = ifelse(hv106 <2, 0,ifelse(hv106 == 8, NA, ifelse(hv106 == 2|3, 1, NA))))
table(pfpr_dhs$hv106)
table(pfpr_h_edu$edu_a)

pfpr_edu<- dataclean.para(pfpr_h_edu, hv005, hv005, hv106, 'edu_a', 'edu_a') 
table(pfpr_edu$edu_a)

svyd_edu <- svydesign.fun(pfpr_edu)

clu_edu <- result.fun('edu_a', 'hv001', design=svyd_edu, pfpr_edu, "hv007")
head(clu_edu)


#ITN use proportion among u5
var_label(pfpr_data$hml16)

# subsetting for ITN proportion 
pfpr_itn <- pfpr_data %>% filter(hv103 == "1"  & hml16 %in% c(0:4)) %>%  
              mutate(net_use = ifelse(hml12 %in% c(1,2), 1,0))

table(pfpr_itn$net_use)

pfpr_itn<- dataclean.para(pfpr_itn, hv005, hv005, net_use, 'net_use', 'net_use_u5') 
table(pfpr_itn$net_use_u5)

svyd_itn <- svydesign.fun(pfpr_itn)

clu_u5_net_use <- result.fun('net_use_u5', 'hv001', design=svyd_itn, pfpr_itn, "hv007")
head(clu_u5_net_use)


#ITN use proportion among pregnant women 
val_labels(pfpr_data$hv104)

# subsetting for ITN proportion 
pfpr_itn_preg <- pfpr_data %>% filter(hv103 == "1"  & hv104 == "2"& hml18 == "1" & hml16 %in% c(15:49)) %>%  
  mutate(net_use = ifelse(hml12 %in% c(1,2), 1,0))

table(pfpr_itn_preg$net_use)

pfpr_itn_preg<- dataclean.para(pfpr_itn_preg, hv005, hv005, net_use, 'net_use', 'net_use_preg') 
table(pfpr_itn_preg$net_use_preg)

svyd_itn_preg <- svydesign.fun(pfpr_itn_preg)

clu_u5_net_preg <- result.fun('net_use_preg', 'hv001', design=svyd_itn_preg, pfpr_itn, "hv007")
head(clu_u5_net_preg)


# median household size
look_for(dhs_list[[3]], "number")

table(pfpr_data$hv013)

pfpr_hh_size<- dataclean.para(pfpr_data, hv005, hv005, hv009, 'hv009', 'hh_size') 
table(pfpr_hh_size$hh_size)

svyd_hh_size <- svydesign.fun(pfpr_hh_size)
table(pfpr_hh_size$hh_size)

clu_hh_size<- svyby(~hh_size, ~hv001, design = svyd_hh_size, FUN=svyquantile, quantiles=0.5, ci=TRUE,vartype="ci")
head(clu_hh_size)

# care seeking proportion among u5 children

look_for(dhs_list[[2]], "Artemisinin")

table(dhs_list[[2]]$ml13e)

pfpr_care <- dhs_list[[2]] %>% filter(b5 == "1"  & b19 <  60 & h22 == "1") 

pfpr_care <- dataclean(pfpr_care, v005, v005,' ml13e', "ACT_use_u5")

svyd_care <- svydesign.fun(pfpr_care)
table(pfpr_care$ACT_use_u5)

clu_u5_care<- result.fun('ACT_use_u5', 'v001', design=svyd_care, pfpr_care, "v007")
head(clu_u5_care)
colnames(clu_u5_care)[1]<- "hv001"
colnames(clu_u5_care)[4]<- "hv007"

# population density
clu_pop_den <- read.csv("data/NG_2018_DHS_11072019_1720_86355/NGGC7BFL/NGGC7BFL.csv") %>%  dplyr::select(hv001 = DHSCLUST,
                                                                              pop_den = UN_Population_Density_2015) %>% 
                                                                              mutate(l_pop_den = log(pop_den))
                                                      
summary(clu_pop_den$l_pop_den)

# bind the datasets 

all_clu <- left_join(clu_est, clu_wealth, by = "hv001") %>% 
  left_join(., clu_u5_prop, by = "hv001") %>% 
  left_join(., clu_preg, by = "hv001") %>% 
  left_join(., clu_edu, by = "hv001") %>% 
  left_join(., clu_u5_net_use, by = "hv001") %>% 
  left_join(., clu_u5_net_preg, by = "hv001") %>% 
  left_join(., clu_hh_size, by = "hv001") %>% 
  left_join(., clu_u5_care, by = "hv001") %>% 
  left_join(., clu_pop_den, by = "hv001") 
  
# correlation coefficient and regression with simple linear model

require(stats)
reg<-lm(p_test~edu_a, data = all_clu)
reg
summary(reg)
coreg <- coefficients(reg)

y <- cor(all_clu$p_test, all_clu$edu_a, use = "pairwise.complete.obs")
y
# equation of line 

eq <- paste0("r = ", round(y, 2), ",", " ", "y =", round(coreg[1], 2), " ", "+" , " " , abs(round(coreg[2], 3)), "* x", ",",
             " ", "p = 1")
eq

plot<- ggplot(all_clu, aes(x=u5_prop, y=p_test)) + 
  geom_point(shape = 18, color = "red") + 
  xlab("Proportion of U5 in individual clusters")+ 
  ylab("U5 Parasite Prevalence") +
  geom_abline(intercept =  0.08901   , slope =  0.30515)+ 
  ggtitle(eq)+
  theme(plot.title = element_text(hjust = 0.5))
plot

png("results/malaria_DHS_paper/regression_results/urban_mal_u5_prop_v2.png")
print(plot)
dev.off()

reg<-lm(p_test~ wealth_2 + edu_a + net_use_u5 + net_use_preg + ACT_use_u5 + u5_prop + preg + hh_size + pop_den, 
        data = all_clu)
reg
summary(reg)


# fever proportion (maybe)