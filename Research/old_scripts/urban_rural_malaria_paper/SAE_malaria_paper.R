# clean and select pfpr data 

pfpr_data <- dhs_list[[3]] # uses the DHS person recode dataset 

look_for(dhs_list[[3]], "smear")

table(pfpr_data$hml32) # frequency table for smear test 


#subsetting for microscopy (denominator -hh selected for hemoglobin, child slept there last night and have result for test)

pfpr_data <- pfpr_data %>% filter(hv042 == "1" & hv103 == "1" & hml32 %in% c(0, 1))
dim(pfpr_data)


# remove missing data 
pfpr_data <- subset(pfpr_data, !is.na(pfpr_data$hml32))

# joins pfpr data to admin 2 data 

pfpr_df <- left_join(pfpr_data, key_2018)
head(pfpr_df)
head(key_2018)



# remove missing data 
pfpr_df_2 <- subset(pfpr_df, !is.na(pfpr_df$LGA)) 



#compute naive estimates 

pfpr_place<- dataclean.para(pfpr_df_2, hv005, hc1, hml32, 'hml32', 'p_test') 

svyd18 <- svydesign.fun(pfpr_place)



direct <- result.fun('p_test', 'LGA', design=svyd18, pfpr_place, "hv007")
head(direct)
 
LGA <- tibble(LGAshp$LGA)
colnames(LGA)<- "LGA"

dat <- left_join(LGA, direct, by = "LGA")

#compute smooth estimates 

# create a key for making sure the LGAs are in the right order for analysis and plotting

LGA_lgt_plot_2$row_num<-1:nrow(LGA_lgt_plot)
key<-LGA_lgt_plot_2[,c("State","LGA","row_num", "avg_night_lights", "log_den")]
head(key)

# handy functions for transforming the data
logit<-function(x){
  log(x/(1-x))
}


expit<-function(x){
  exp(x)/(1+exp(x))
}


# Setting up the transformed variables, variance=0 is a problem
summary(dat$se)


dat<-dat%>%mutate(logit_pfpr=ifelse(!is.na(p_test),logit(p_test),NA),
                  var_logit_pfpr=(se^2)/(p_test^2*(1-p_test)^2),
                  # var_logit_obese=ifelse(se==0,NA,var_logit_obese),
                  var_logit_pfpr=ifelse(se<0.00001,NA,var_logit_pfpr),
                  logit_pfpr=ifelse(is.na(var_logit_pfpr),NA,logit_pfpr))



summary(dat$se)
# merge in key and put data in right order
dim(dat)
dat<-dat%>%left_join(key)%>%arrange(row_num)
class(dat)

# setting up the spatial prior distribution for the smoothing

# this takes the shape file and figures out which areas are neighbors
library(spdep)
nb.r <- poly2nb(LGAshp, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix

# to double check that they are finding the neighbors, I like to plot a 
# random LGA in blue and then the neighbors in red.
row = 500
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx

# Setting up for spatial smoothing model

a<-1
b<-5e-05


smoothing.model.1 <- outcome ~  f(row_num, model="bym",graph=mat, param=c(a,b)) 
smoothing.model.2 <- outcome ~   + avg_night_lights+ log_den+ f(row_num, model="bym",graph=mat, param=c(a,b)) 


# setting up the outcome #
dat$outcome<-dat$logit_pfpr
dat$prec<-1/dat$var_logit_pfpr

# fitting model 1 #
system.time({
  mod1 <- inla(smoothing.model.2,
               family = "gaussian",
               data =dat,
               control.predictor=list(compute=TRUE),
               control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
               control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
               scale=prec, verbose =TRUE)
  
})#2.51s

summary(mod1)

#creating data frame of all estimates for model 1
saep.est = expit(mod1$summary.fitted.values$`0.5quant`)
saep.up = expit(mod1$summary.fitted.values$`0.975quant`)
saep.low = expit(mod1$summary.fitted.values$`0.025quant`)

#Generating estimate file model 2 
prevalence = data.frame(dat,saep.est,saep.low, saep.up)


DS_file <- left_join(LGA_sf, prevalence, by = "LGA")

par_18 <- tmap.fun4(DS_file, "U5 2018 parasitemia", "Prevalence", "saep.est")

par_18_low <- tmap.fun4(DS_file, "U5 2018 parasitemia_low", "Prevalence", "saep.low")

par_18_high <- tmap.fun4(DS_file, "U5 2018 parasitemia_high", "Prevalence", "saep.up")

par_18_naive <- tmap.fun4(DS_file, "U5 2018 parasitemia_naive", "Prevalence", "p_test")

all_para <- tmap_arrange(par_18_naive, par_18, par_18_low, par_18_high)

tmap_save(tm = all_para, filename = "results/malaria_DHS_paper/pfpr_predictions/para_ngt_pop_density.pdf",
          width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)


