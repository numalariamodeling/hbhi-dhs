# set document path to current script path 
setwd("C:/Users/pc/Box/NU-malaria-team/data/nigeria_dhs/data_analysis/data")


gender <- sample(c(0,1), size = 1000, replace = TRUE)
age <- round(runif(1000, 18, 80))
xb <- -9 + 3.5*gender + 0.2*age
p <- 1/(1 + exp(-xb))
y <- rbinom(n = 1000, size = 1, prob = p)
age <- as.data.frame(age)
gender <- as.data.frame(gender)
y <- as.data.frame(y)

df <- merge(y, age, by="row.names", all=TRUE)
#df <- df %>% remove_rownames %>% column_to_rownames(var="Row.names")

df2 <- merge(y, gender, by="row.names", all=TRUE)

df3<- left_join(df, df2, by = "Row.names")



mod <- glm(y.y ~ gender + age, data = df3, family = "binomial")
summary(mod)

r_rmod <- inla(y.y ~ gender + age, family = 'binomial',
               data = df3, control.family = list(link = "logit"), control.predictor = list(compute=TRUE),
               control.compute = list(cpo=TRUE, dic = TRUE)) 

summary(r_rmod)


clu_variales_10_18 <- read.csv(file.path(DataDir, "Nigeria_2010_2018_clustered_final_dataset.csv"), 
                               header = T, sep = ',')

ruraldataset <-clu_variales_10_18 %>% filter(Rural_urban == 2)

ruraldataset = ruraldataset[,c("build_count", "wealth_2", "edu_a", "hh_size", 
                       "ACT_use_u5", "pop_den","hh_members_age", "sex_f", "humidindex", 
                       "annual_precipitation", "l_pop_den", "net_use")]



df4 <- ruraldataset[sample(nrow(ruraldataset), 1000), ]


xb <- 0.3 + 0.5*df4$net_use + 0.2*df4$wealth_2
p <- 1/(1 + exp(-xb))
y <- rbinom(n = 1920, size = 1, prob = p)
y <- as.data.frame(y)
df4 <- merge(y, df4, by="row.names", all=TRUE)

df4 <- na.omit(df4)

mod_2 <- glm(y.x ~ net_use + wealth_2, data = df4, family = "binomial")
summary(mod_2)
