setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")
library(splitstackshape)

# For scenario 3, we first get an estimate of the averagechange in coverage per year 

cm_3 <- read.csv('bin/projection/s3_v3/cm_trend.csv')
head(cm_3)

plot(cm_3$year, cm_3$comboACT)

cm_split <- split(cm_3, cm_3$repDS)

#models 
# summary(ahoda)$coef
# summary(list.mod[[2]])$coef


model.fun <- function(x){g <- lm(comboACT ~ year, data = x)}
ex.fun <- function(x){y <- summary(x)$coef
                          y[2]}

list.mod <- map(cm_split, model.fun)
summary(list.mod[1]$`Ahoada West`)

# png("Ahoda_West.png")
# plot(comboACT ~ year, data = cm_split$`Ahoada West`, xlab = "Year", ylab = "Coverage Percent")
# abline(coef(list.mod[1]$`Ahoada West`)[1:2], col = "red")
# cf <- round(coef(list.mod[1]$`Ahoada West`), 3) 
# 
# eq <- paste0(" CM coverage = ", cf[1]," + ", abs(cf[2]), " year ")
# 
# mtext(eq, 3, line=-2)
# dev.off()


scale.factors <- map(list.mod, ex.fun)

values <- do.call(rbind.data.frame, scale.factors)

lst.names <- tibble(names(scale.factors))

df <- cbind (lst.names,values)

colnames(df)<- c("Rep_DS", "scale_values")

write.csv(df, 'bin/projection/s3_v3/scale_values.csv')

# now we use the df values 
cm_2 <- read.csv('bin/projection/s2_v2/HS_placeholder_v2.csv')
head(cm_2)

summary(cm_2$U5_coverage)
summary(cm_2$adult_coverage)

cm_3 <- expandRows(cm_2, count = 11,count.is.col=FALSE, 
                   drop = FALSE) 
head(cm_3, 11)

lookup_key <- c(1, 2, 3, 4, 5, 6, 7, 8, 9 , 10, 11) # 3, 4, 5, 6

cm_3$round <- rep(1:11)
cm_3$severe_cases <- 0.49
tail(cm_3,30)


sim_day <- c(71,436,801,1166,1531,1896, 2261, 2626,2991,3356, 3721)

year_sim <- c(2020, 2021, 2022, 2023, 2024, 2025, 2026, 2027, 2028, 2029, 2030)

duration <- c(365, 365, 365, 365, 365, 365, 365, 365, 365, 365, 365)

scale_multipliers <- c(2, 3,4, 5, 6, 7,8, 9, 10, 11, 12)


df_sim <- tibble(lookup_key, sim_day, year_sim, duration, scale_multipliers)
head(df_sim)


cm_3$simday[cm_3$round %in% df_sim$lookup_key == TRUE]<- 
  sim_day[df_sim$lookup_key %in% cm_3$round == TRUE]

cm_3$duration[cm_3$round %in% df_sim$lookup_key == TRUE] <- 
  duration[df_sim$lookup_key %in% cm_3$round == TRUE]

cm_3$year[cm_3$round %in% df_sim$lookup_key == TRUE] <- 
        year_sim[df_sim$lookup_key %in% cm_3$round == TRUE]

cm_3$scale_m[cm_3$round %in% df_sim$lookup_key == TRUE] <- 
  scale_multipliers[df_sim$lookup_key %in% cm_3$round == TRUE]

head(cm_3)

cm_3 <- cm_3 %>% left_join(df)

head(cm_3, 13)

max_v <- 0.80
cm_3 <- cm_3 %>% mutate(scale_values = scale_values*scale_m,
                        U5_coverage = pmin(U5_coverage + scale_values,max_v),
                        adult_coverage = U5_coverage, severe_cases = pmin(severe_cases + scale_values, max_v))

head(cm_3, 15)

file_path <- "C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_nigeria/simulation_inputs/projection_csvs/projection_v2"

write.csv(cm_3, paste0(file_path, "/cm_scen3_v4.csv"))


