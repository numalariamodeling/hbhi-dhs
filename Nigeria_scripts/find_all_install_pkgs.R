ip = as.data.frame(installed.packages()[,c(1,3:4)])
ip = ip[is.na(ip$Priority),1:2,drop=FALSE]
write.csv(ip, "C:/Users/ido0493/Documents/R/package_info_2020_04/packages.csv")
