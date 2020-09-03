para2014 <-read.csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/parasitemia/pos_test_time_BF14CL.csv")
para2010 <-read.csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/DS DHS estimates/parasitemia/pos_test_time_BF10CL.csv")
para2010DS <- read.csv("C:/Users/ido0493/Box/NU-malaria-team/projects/hbhi_burkina/prevalence/pos_test_time_BF10DS_v2.csv")


class(para2010$time2)

para2014$time2<-as.Date(para2014$time2)

para2010DS$Number.of.Kids

para2014.mod <- lm(formula=p_test ~ time2 + num_kids, data = para2014)
summary(para2014.mod)