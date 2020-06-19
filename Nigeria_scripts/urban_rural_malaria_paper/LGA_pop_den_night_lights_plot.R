# Examining the correlation between population density and nighttime lights 

#first join dataset containing pop density with LGA aggregated night time lights 

LGA_pop_lght <- left_join(NGA_pop, lgt_ex_2, by = "LGA")

summary(LGA_pop_lght$log_lght)

LGA_lgt <- LGA_pop_lght[LGA_pop_lght$avg_night_lights >0,]

# correlation coefficient and regression with simple linear model

require(stats)
reg<-lm(log_den~ avg_night_lights, data = LGA_pop_lght)
reg
summary(reg)

coreg <- coefficients(reg)

y <- cor(LGA_pop_lght$avg_night_lights, LGA_pop_lght$log_den)

# equation of line 

eq <- paste0("r = ", round(y, 2), ",", " ", "y =", round(coreg[1], 2), "+", round(coreg[2], 2), "*x", ",",
             " ", "p < 0.001")

plot<- ggplot(LGA_lgt, aes(x=avg_night_lights, y=log_den)) + 
  geom_point(shape = 18, color = "red") + 
  xlab("Night Time Lights")+ 
  ylab("Log Population Density") +
  geom_abline(intercept = 5.76652, slope = 0.28176)+ 
  ggtitle(eq)+
  theme(plot.title = element_text(hjust = 0.5))

png("results/malaria_DHS_paper/nightlights/pop_den_Lights_2016_annual.png")
print(plot)
dev.off()

# start here if you just want to look at raster coorelations 

# correlation between the nigerian pop density raster and the night time lights raster (local correlation - quick visual)

rc <- corLocal(lgt_crop_res, pop_crop, method='pearson') 
summary(rc)

png("results/malaria_DHS_paper/nightlights/local_cor_lgt_pop_2016_annual.png")
plot(rc, main = "Local coorelation between population density and night time lights")
dev.off()

#global correlation 

stack_lgt_pop <- stack(lgt_crop_res, pop_crop)

names(stack_lgt_pop) <- c("night time lights", "population density")

plot(stack_lgt_pop)

cor(values(stack_lgt_pop)[,1],
    values(stack_lgt_pop)[,2],
    use = "na.or.complete")

# linear model 

lm1 <- lm(values(stack_lgt_pop)[,2] ~ values(stack_lgt_pop)[,1])
summary(lm1)

# Retrieve residuals considering missing values
resid_lm <- raster(stack_lgt_pop, 1) * NA
values(resid_lm)[-lm1$na.action] <- lm1$residuals


# Figure to view spatial heterogenity of residuals
resid_lm_dat <- SDMSelect::gplot_data(
  resid_lm, maxpixels = 50000) %>%
  filter(value < 0) %>% 
  mutate(variable = "Residuals") %>%
  filter(!is.na(value))

summary(resid_lm_dat$value)


LGA_df <- fortify(LGAshp)




# map 
ggplot(resid_lm_dat) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_gradient2("Residuals",
                       low = scales::muted("red"),
                       high = scales::muted("blue"),
                       midpoint = 0) +
  geom_polygon(data = LGAshp,
               aes(long, lat, group = group),
               fill = NA,
               colour = "grey20", size = 0.1) +
  coord_quickmap(
    xlim = range(resid_lm_dat$x),
    ylim = range(resid_lm_dat$y)
  ) + xlab("") + ylab("")


# local correlation 

stack_lgt_pop_nb <- raster(stack_lgt_pop, 1)
values(stack_lgt_pop_nb) <- 1:ncell(stack_lgt_pop)

matrix_lgt_pop <- values(stack_lgt_pop) # stack as raster [MW]


focal_cor <- focal(
  x = stack_lgt_pop_nb,
  w = matrix(1, 5, 5),
  # fun = function(x, y = temp_chl_s){ # Original
  # cor(values(y)[x, 1], values(y)[x, 2], # Original
  # use = "na.or.complete")
  # },
  fun = function(x, y = matrix_lgt_pop){ # [MW]
    cor(y[x, 1], y[x, 2], # [MW]
        use = "na.or.complete")
  })#,
  # filename = file.path("results", "focal_cor.tif"),
  # overwrite = TRUE



# Get data for ggplot
focal_cor_dat <- SDMSelect::gplot_data(focal_cor, maxpixels = 50000) %>%
  mutate(variable = "Correlation") %>%
  filter(!is.na(value))

# Plot
ggplot(focal_cor_dat) +
  geom_tile(aes(x, y, fill = value)) +
  scale_fill_gradient2("Corr",
                       low = "#d7191c",
                       mid = "#ffffbf",
                       high = "#1a9641",
                       midpoint = 0) +
  geom_polygon(data = LGAshp,
               aes(long, lat, group = group),
               fill = NA,
               colour = "grey20", size = 0.1) +
  coord_quickmap(
    xlim = range(focal_cor_dat$x),
    ylim = range(focal_cor_dat$y)
  ) + xlab("") + ylab("")

summary(is.na(focal_cor_dat$value))
  
