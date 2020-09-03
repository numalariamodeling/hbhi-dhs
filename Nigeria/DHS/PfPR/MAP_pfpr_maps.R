file_lst <- list.files(path = "bin/MAP_admin1_pfpr", pattern = "*PfPR.*\\.csv", recursive = TRUE, full.names = TRUE)


pfpr_lst <- sapply(file_lst, read.csv, simplify = F)

head(pfpr_lst[[3]]$X2015)

pfpr_15 <- pfpr_lst[[3]][pfpr_lst[[3]]$Country == "Nigeria", ] %>%  dplyr::select(Name, X2015)
pfpr_15_l <- pfpr_lst[[1]][pfpr_lst[[1]]$Country == "Nigeria", ] %>%  dplyr::select(Name, X2015)
pfpr_15_h <- pfpr_lst[[2]][pfpr_lst[[2]]$Country == "Nigeria", ] #%>%  dplyr::select(Name, X2015)

pfpr_MAP <- cbind(pfpr_15, pfpr_15_l, pfpr_15_h)
