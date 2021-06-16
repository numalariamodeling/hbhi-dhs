#Reading in the necessary packages 
x <- c("tidyverse", "survey", "haven", "ggplot2", "purrr", "summarytools", "stringr", "sp", "rgdal", "raster",
       "lubridate", "RColorBrewer","sf", "shinyjs", "tmap", "knitr", "labelled", "plotrix", "arules", "foreign",
       "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "ggcorrplot", "hrbrthemes", "reshape", "caret", 
       "clusterSim", "gridExtra", "MASS")

lapply(x, library, character.only = TRUE) #applying the library function to packages


rural.cluster <- read.csv("ruralcluster.csv")

fullmodel <- lm((p_test+4) ~ wealth_2 + edu_a + net_use_u5 + net_use_preg + ACT_use_u5 
                + u5_prop + preg + hh_size + pop_den, data = clean.transf)
plot(fullmodel)

bc <- boxcox(fullmodel, lambda = seq(-3,3))

best.lam <- bc$x[which(bc$y == max(bc$y))]

fullmodel.inv <- lm((p_test+1)^-1 ~ wealth_2 + edu_a + net_use_u5 + net_use_preg 
                    + ACT_use_u5 + u5_prop + preg + hh_size + pop_den, data = rural.cluster)
plot(fullmodel.inv)

