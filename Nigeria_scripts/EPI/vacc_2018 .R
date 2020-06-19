table(NGAfiles[[20]]$h51) #penta1 2018 
table(NGAfiles[[20]]$h53) #penta3
table(NGAfiles[[20]]$h9) #measles


#renaming datatset 
vac.df <- NGAfiles[[20]]

#recode variables 

vac.df <- vac.df %>% mutate(pen1 = ifelse(h51 > 3, NA, ifelse(h51 ==0, 0, 1)),
                            pen3 = ifelse(h53 > 3, NA, ifelse(h53 ==0, 0, 1)), 
                            measles = ifelse(h9 >3, NA, ifelse(h9==0, 0, 1)))
table(vac.df$pen1) #penta1
table(vac.df$pen3) #penta3
table(vac.df$measles) #measles

vac.df <- vac.df %>% left_join(key_list[[7]])

#####################################################################################################
# vaccination estimates
####################################################################################################

#penta1
vac.df <-dataclean(vac.df,pen1, v005,'pen1', 'pen1')  
vac.svyd18 <- svydesign.fun(vac.df)

DS_pen1_pre_18 <- result.fun('pen1', 'LGA','num_p', design=vac.svyd18)
head(DS_pen1_pre_18)

write.csv(DS_pen1_pre_18, "results/vaccine/DS_pen1_pre_18.csv")


all_DS_pen1 <- LGAshp_sf %>% left_join(DS_pen1_pre_18) %>% rename(pen1_DS = pen1, ci_lDS = ci_l, ci_uDS = ci_u,
                                                                  Num_DS = `Number of Participants`)

#state estimates  
S_pen1_pre_18 <- result.fun('pen1', 'State','num_p', design=vac.svyd18)
head(S_pen1_pre_18)

fin.pen1.df <- all_DS_pen1 %>% left_join(S_pen1_pre_18) %>% 
                mutate(pen1_DS=ifelse(is.na(pen1_DS),pen1, pen1_DS),ci_lDS=ifelse(is.na(ci_lDS),ci_l,ci_lDS),
                       ci_uDS=ifelse(is.na(ci_uDS),ci_u,ci_uDS)) %>% 
                        dplyr::select(State,LGA, pen1_DS,ci_lDS,ci_uDS, Num_DS, pen1_state=pen1,
                                ci_l_state=ci_l, ci_u_state=ci_u, Num_state=`Number of Participants`) 

st_write(fin.pen1.df, "results/vaccine/DS.pen1_state_18.csv")
                                                                    


#penta3
vac.df <-dataclean(vac.df,pen3, v005,'pen3', 'pen3')  
vac.svyd18_pen3 <- svydesign.fun(vac.df)

DS_pen3_pre_18 <- result.fun('pen3', 'LGA','num_p', design=vac.svyd18_pen3)
head(DS_pen3_pre_18)

write.csv(DS_pen3_pre_18, "results/vaccine/DS_pen3_pre_18.csv")

all_DS_pen3 <- LGAshp_sf %>% left_join(DS_pen3_pre_18) %>% rename(pen3_DS = pen3, 
                                                                  ci_lDS = ci_l, ci_uDS = ci_u,
                                                                  Num_DS = `Number of Participants`)


#state estimates  

S_pen3_pre_18 <- result.fun('pen3', 'State','num_p', design=vac.svyd18_pen3)
head(S_pen3_pre_18)

#creating a file where missing LGA estimates is filled in with state values 

fin.pen3.df <- all_DS_pen3 %>% left_join(S_pen3_pre_18) %>% 
              mutate(pen3_DS=ifelse(is.na(pen3_DS),pen3, pen3_DS),ci_lDS=ifelse(is.na(ci_lDS),ci_l,ci_lDS),
                ci_uDS=ifelse(is.na(ci_uDS),ci_u,ci_uDS)) %>% 
                dplyr::select(State,LGA, pen3_DS,ci_lDS,ci_uDS, Num_DS, pen3_state=pen3,
                ci_l_state=ci_l, ci_u_state=ci_u, Num_state=`Number of Participants`) 

st_write(fin.pen3.df, "results/vaccine/DS.pen3_state_18.csv")


#measles
vac.df <-dataclean(vac.df,measles, v005,'measles', 'measles')  
vac.svyd18_mea <- svydesign.fun(vac.df)

DS_mea_pre_18 <- result.fun('measles', 'LGA','num_p', design=vac.svyd18_mea)
head(DS_mea_pre_18)

write.csv(DS_mea_pre_18, "results/vaccine/DS_mea_pre_18.csv")


all_DS_mea <- LGAshp_sf %>% left_join(DS_mea_pre_18) %>% rename(measles_DS = measles, 
                                                                  ci_lDS = ci_l, ci_uDS = ci_u,
                                                                  Num_DS = `Number of Participants`)


#state estimates  

S_mea_pre_18 <- result.fun('measles', 'State','num_p', design=vac.svyd18_mea )
head(S_mea_pre_18)

#creating a file where missing LGA estimates is filled in with state values 

fin.mea.df <- all_DS_mea %>% left_join(S_mea_pre_18) %>% 
              mutate(measles_DS=ifelse(is.na(measles_DS),measles,measles_DS),
                   ci_lDS=ifelse(is.na(ci_lDS),ci_l,ci_lDS),
                     ci_uDS=ifelse(is.na(ci_uDS),ci_u,ci_uDS)) %>% 
                       dplyr::select(State,LGA, measles_DS,ci_lDS,ci_uDS, Num_DS, measles_state=measles,
                        ci_l_state=ci_l, ci_u_state=ci_u, Num_state=`Number of Participants`) 

st_write(fin.mea.df, "results/vaccine/DS.measles_state_18.csv")


#cluster-level estimates 

# pen1 
clu_pen1_pre_18 <- result.clu.fun('pen1', 'v001', design=vac.svyd18,vac.df)
head(clu_pen1_pre_18)

write.csv(clu_pen1_pre_18, "results/vaccine/clu_pen1_pre_18.csv")


#pen3
clu_pen3_pre_18 <- result.clu.fun('pen3', 'v001', design=vac.svyd18_pen3,vac.df)
head(clu_pen3_pre_18)

write.csv(clu_pen3_pre_18, "results/vaccine/clu_pen3_pre_18.csv")


#measles
clu_mea_pre_18 <- result.clu.fun('measles', 'v001', design=vac.svyd18_mea,vac.df)
head(clu_mea_pre_18)

write.csv(clu_mea_pre_18, "results/vaccine/clu_mea_pre_18.csv")



#####################################################################################################
## Maps 
####################################################################################################


# 2018 transformations for pen1 maps 
DS_file <- LGAshp_sf %>% left_join(DS_pen1_pre_18 )

pts_file <- NGAshplist_sf[[7]] %>% left_join(clu_pen1_pre_18)


#pen3
DS_file_pen3 <- LGAshp_sf %>% left_join(DS_pen3_pre_18 )

pts_file_pen3 <- NGAshplist_sf[[7]] %>% left_join(clu_pen3_pre_18)


#measles
DS_file_mea <- LGAshp_sf %>% left_join(DS_mea_pre_18 )

pts_file_mea <- NGAshplist_sf[[7]] %>% left_join(clu_mea_pre_18)



NGA_pen1_18 <- tmap.fun3(DS_file, colname="pen1", legtitle="Pentavalent 1 coverage", 
                        maintitle="Pentavalent 1 coverage by District (2018)", ptsfile=pts_file, "Number of Participants",
                        "pen1") 


NGA_pen3_18 <- tmap.fun3(DS_file_pen3, colname="pen3", legtitle="Pentavalent 3 coverage", 
                        maintitle="Pentavalent 3 coverage by District (2018)", ptsfile=pts_file_pen3, "Number of Participants",
                        "pen3") 



NGA_mea_18 <- tmap.fun3(DS_file_mea, colname="measles", legtitle="Measles vaccine coverage", 
                         maintitle="Measles vaccine coverage by District (2018)", ptsfile=pts_file_mea, "Number of Participants",
                         "measles") 


all_vaccine <- tmap_arrange(NGA_pen1_18, NGA_pen3_18, NGA_mea_18)

tmap_save(tm = NGA_pen1_18,filename = "results/vaccine/pentavalent1_vaccine.pdf",width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)





