rm(list=ls())
library(plyr)
library(ggplot2)

cm_3 <- read.csv('bin/projection/s3_v3/cm_trend.csv')
head(cm_3)

plot(cm_3$year, cm_3$comboACT)

cm_split <- split(cm_3, cm_3$repDS)

lm_eqn = function(df){
  m = lm(comboACT ~ year, df);
  eq <- paste0("CM coverage =", round(coef(m)[1], 3)," + ", round(abs(coef(m)[2]), 3), " year ")
  as.character(as.expression(eq));                 
}

mymax = function(df){
  max(df$year)
}

regs <- ddply(cm_3, .(repDS), lm_eqn)
regs.xpos <- ddply(cm_3, .(repDS), function(df) (min(df$year)+max(df$year))/2)
regs.ypos <- ddply(cm_3, .(repDS), function(df) min(df$year) + 0.05*(max(df$year)-min(df$year)))

regs$y <- regs.ypos$V1
regs$x <- regs.xpos$V1


gp <- ggplot(data=cm_3, aes(year, comboACT)) +
  geom_point(size = 1, alpha=0.75)+ geom_smooth(method="lm", se=FALSE, color="red")+
  facet_wrap(vars(repDS))



y <- gp+
  geom_text(data= regs,size = 3,
             mapping = aes(x = -Inf, y = -Inf, label = V1),
             hjust   = -0.2,
             vjust   = -20)

ggsave("results/archetype_sim_input/Intervention_files_LGA/case_management/linear_model_fit.pdf", y, scale=1.5, width=11, height=8)
