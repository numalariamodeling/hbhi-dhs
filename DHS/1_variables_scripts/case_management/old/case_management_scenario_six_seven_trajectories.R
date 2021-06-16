rm(list=ls())
library(plyr)
library(ggplot2)

setwd("C:/Users/ido0493/Box/NU-malaria-team/data/nigeria_dhs/data_analysis")
cm_3 <- read.csv('bin/projection/s3_v3/cm_trend.csv')
head(cm_3)

plot(cm_3$year, cm_3$comboACT)

cm_split <- split(cm_3, cm_3$repDS)

lm_eqn = function(df){
  m = lm(comboACT ~ year, df);
  eq <- paste0("CM coverage =", " ",  round(coef(m)[1], 3)," + ", "\n", round(abs(coef(m)[2]), 3), " year ")
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


gp<-ggplot(data=cm_3, aes(year, comboACT)) +
  geom_point(size = 1, alpha=0.75)+ geom_smooth(method="lm", se=FALSE, color="red")+
  scale_x_continuous(breaks = c(2013, 2015, 2017))+
  geom_text(data = regs, size = 3, aes(label =V1,y = 0.82, x = 2015.5)) +
  facet_wrap(vars(repDS))+
  ylab("Case Management Coverage")+
  theme_minimal()+
  theme(strip.text.x = element_text(size = 8.5, colour = "black", face = "bold"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5),
        axis.ticks.x = element_line(size = 0.5, colour = "black"),
        axis.ticks.y = element_line(size = 0.5, colour = "black"))





ggsave(file=paste0("results/archetype_sim_input/Intervention_files_LGA/case_management/linear_model_fit_", Sys.Date(),"_", ".png"), gp, scale=1.5, width=11, height=8)
