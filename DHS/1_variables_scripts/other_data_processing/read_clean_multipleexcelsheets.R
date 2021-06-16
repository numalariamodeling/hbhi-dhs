library(readxl)

library(tidyverse)

#pfpr microscopy 

sheet = excel_sheets("bin/pfpr/2010pfpr_SAS.xlsx")

df.pfpr.micro = lapply(setNames(sheet, sheet), function(x) read_excel("bin/pfpr/2010pfpr_SAS.xlsx", sheet=x))

pfpr.micro.list <- map(df.pfpr.micro, clean.xls)

pfpr.micro.df <- do.call("rbind", pfpr.micro.list ) 

pfpr.micro.df2 <- pfpr.micro.df %>% filter(frequency !=".") %>% dplyr::select(repDS, time2,
                                                              PfPr_microscopy, ci_l_micro = ci_l, ci_u_micro=ci_u, `Number of Kids_micro` = frequency)

write.csv(pfpr.micro.df2, "results/archetype_sim_input/2010pfprmicro.csv")

#pfpr RDT

sheet = excel_sheets("bin/pfpr/2010pfprRDT_SAS.xlsx")

df.pfpr.RDT = lapply(setNames(sheet, sheet), function(x) read_excel("bin/pfpr/2010pfprRDT_SAS.xlsx", sheet=x))

pfpr.RDT.list <- map(df.pfpr.RDT, clean.xls)

pfpr.RDT.df <- do.call("rbind", pfpr.RDT.list ) 

pfpr.RDT.df2 <- pfpr.RDT.df %>% filter(frequency !=".") %>% dplyr::select(repDS, time2,
                                                                              PfPr_RDT=PfPr_microscopy, ci_l_RDT =ci_l, ci_u_RDT=ci_u, `Number of Kids_RDT` = frequency)

write.csv(pfpr.RDT.df2, "results/archetype_sim_input/2010pfprRDT.csv")



# reading and cleaning U5 ITN data 

ITN.df <- read_excel("bin/ITN_DHS_defined/2010_U5ITN_SAS.xlsx")

ITN.df[[1,4]] <- "weig_frequency"
ITN.df[[1,10]] <- "U5_ITN_use"
ITN.df[[1,12]] <- "ci_l"
ITN.df[[1,13]] <- "ci_u"


ITN.df2 <- ITN.df %>% dplyr::select(!!c(1, 2, 4, 10, 12, 13))
names(ITN.df2) <- ITN.df2 [1, ]
ITN.df3 <- ITN.df2 [c(-1,-69, -68, -70, -71), ]
ITN.df3 <- ITN.df3 %>% fill(repDS) %>% dplyr::filter(hh_itn =="1") %>% dplyr::select(repDS, U5_ITN_use,
            ci_l_u5 = ci_l, ci_u_u5 = ci_u, `Number of Participants_u5` = weig_frequency)




# reading and cleaning six - nine ITN data 

ITN6.df <- read_excel("bin/ITN_DHS_defined/2010_six_tenITN_SAS.xlsx")

ITN6.df[[1,4]] <- "weig_frequency"
ITN6.df[[1,10]] <- "six_nine_ITN_use"
ITN6.df[[1,12]] <- "ci_l"
ITN6.df[[1,13]] <- "ci_u"


ITN6.df2 <- ITN6.df  %>% dplyr::select(!!c(1, 2, 4, 10, 12, 13))
names(ITN6.df2) <- ITN6.df2 [1, ]
ITN6.df3 <- ITN6.df2 [c(-1,-68, -69, -70, -71), ]
ITN6.df3 <- ITN6.df3 %>% fill(repDS) %>% dplyr::filter(hh_itn =="1") %>% dplyr::select(repDS, six_nine_ITN_use,
                                                                                     ci_l_six = ci_l, ci_u_six = ci_u, `Number of Participants_six` = weig_frequency)




# reading and cleaning ten - eighteen ITN data 

ITN18.df <- read_excel("bin/ITN_DHS_defined/2010_ten_eightITN_SAS.xlsx")

ITN18.df[[1,4]] <- "weig_frequency"
ITN18.df[[1,10]] <- "ten_eighteen_ITN_use"
ITN18.df[[1,12]] <- "ci_l_10_18_"
ITN18.df[[1,13]] <- "ci_u_10_18_"


ITN18.df2 <- ITN18.df  %>% dplyr::select(!!c(1, 2, 4, 10, 12, 13))
names(ITN18.df2) <- ITN18.df2 [1, ]
ITN18.df3 <- ITN18.df2 [c(-1,-68, -69, -70, -71), ]
ITN18.df3 <- ITN18.df3 %>% fill(repDS) %>% dplyr::filter(hh_itn =="1") %>% dplyr::select(repDS, ten_eighteen_ITN_use,
                                ci_l_10_18_, ci_u_10_18_, `Number of Participants_10_18` = weig_frequency)




# reading and cleaning eighteen and over ITN data 

ITN18g.df <- read_excel("bin/ITN_DHS_defined/2010_over_eighteenITN_SAS.xlsx")

ITN18g.df[[1,4]] <- "weig_frequency"
ITN18g.df[[1,10]] <- "over_eighteen_ITN_use"
ITN18g.df[[1,12]] <- "ci_l_18g"
ITN18g.df[[1,13]] <- "ci_u_18g"


ITN18g.df2 <- ITN18g.df  %>% dplyr::select(!!c(1, 2, 4, 10, 12, 13))
names(ITN18g.df2) <- ITN18g.df2 [1, ]
ITN18g.df3 <- ITN18g.df2 [c(-1,-68, -69, -70, -71), ]
ITN18g.df3 <- ITN18g.df3 %>% fill(repDS) %>% dplyr::filter(hh_itn =="1") %>% dplyr::select(repDS, over_eighteen_ITN_use,
                                                                                         ci_l_18g, ci_u_18g, `Number of Participants_18` = weig_frequency)


fin <- ITN.df3 %>% left_join(ITN6.df3) %>% left_join(ITN18.df3) %>% left_join(ITN18g.df3)


write.csv(fin, "results/archetype_sim_input/ITN_archetype_10_v2.csv")


# Add killing rate 

arch_ITN <- read.csv("results/archetype_sim_input/intervetion_files_archetype/ITN_archetype_10_v2.csv")

kill_rate <- read.csv("bin/ITN_DHS_defined/Killing_rate_nigeria_2010.csv") %>% dplyr::select(-X, LGA=DS)

kill_rate_2 <- kill_rate %>% left_join(rep_DS) %>% group_by(repDS) %>% summarise(mean(kill_rate_10)) 

kill_rate_2 <- kill_rate_2[-23, ]

write.csv(kill_rate_2 , "results/archetype_sim_input/arch_kill_rate.csv")
