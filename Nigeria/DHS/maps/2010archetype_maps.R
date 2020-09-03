#ACT 

rep_DS_ACT <- rep_DS %>% left_join(comboACT_pre_10)

DS_file <- LGAshp_sf %>% left_join(rep_DS_ACT)

head(DS_file)

ACT_archetype_2010 <- tmap.fun4(DS_file, '2010 ACT by archetype', 
                                 'ACT 2010 values', 'comboACT')
tmap_save(tm = ACT_archetype_2010, filename = "results/maps/2010_archetype/ACT_2010_repDS.micro.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

#ITN 

ITN_archetype_2010 <- read.csv("bin/ITN/ITN_archetype_10.csv") %>% 
                        mutate(U5_ITN_use = U5_ITN_use/100, 
                               six_nine_ITN_use = six_nine_ITN_use/100,
                               ten_eighteen_ITN_use = ten_eighteen_ITN_use/100,
                               over_eighteen_ITN_use = over_eighteen_ITN_use/100) %>% 
                        dplyr:: select(repDS, U5_ITN_use, six_nine_ITN_use,
                                       ten_eighteen_ITN_use, over_eighteen_ITN_use)


rep_DS_ITN <- rep_DS %>% left_join(ITN_archetype_2010) 


DS_file <- LGAshp_sf %>% left_join(rep_DS_ITN)

head(DS_file)

U5.ITNarchetype_2010 <- tmap.fun4(DS_file, '2010 U5 ITN by archetype',
                                  'U5 2010 values',  'U5_ITN_use')

six_nine.ITNarchetype_2010 <- tmap.fun4(DS_file, '2010 six.nine ITN by archetype',
                                 'six.nine 2010 values',  'six_nine_ITN_use')

teneighteen.ITNarchetype_2010 <- tmap.fun4(DS_file,'2010 ten.eighteen ITN by archetype',
                              'ten.eighteen 2010 values', 'ten_eighteen_ITN_use')

over.eighteen.ITNarchetype_2010 <- tmap.fun4(DS_file, 
                            '2010 over.eighteen ITN by archetype',
                      'over.eighteen 2010 values','over_eighteen_ITN_use')

all_arch_ITN <- tmap_arrange(U5.ITNarchetype_2010,six_nine.ITNarchetype_2010,
                             teneighteen.ITNarchetype_2010,over.eighteen.ITNarchetype_2010)

tmap_save(tm =all_arch_ITN, filename = "results/maps/2010_archetype/ITN_2010_repDS.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)

#pfpr 

pfpr_archetype <- read.csv('bin/pfpr/PfPr_archetype_10_v2.csv') %>% 
                  mutate(PfPr_microscopy = PfPr_microscopy/100)

pfpr_arch_ls <- split(pfpr_archetype, pfpr_archetype$time2)

pfpr_arch_ls_v2 <- map2(rep_DS.ls, pfpr_arch_ls, left_join)

LGAshp_sf_ls <- list(LGAshp_sf)

pfpr_arch_ls_v3 <- map2(LGAshp_sf_ls,  pfpr_arch_ls_v2, left_join)

table(pfpr_arch_ls_v2[[3]]$time2)


pfpr.archetype_oct <- tmap.fun2(pfpr_arch_ls_v3[[1]], 
                                 'PfPr_microscopy', 'Oct 2010 PfPR values',
                                 '2010 Oct PfPR by archetype')


pfpr.archetype_nov <- tmap.fun2(pfpr_arch_ls_v3[[2]], 
                                 'PfPr_microscopy', 'Nov 2010 PfPR values',
                                 '2010 Nov PfPR by archetype')

pfpr.archetype_dec <- tmap.fun2(pfpr_arch_ls_v3[[3]], 
                                'PfPr_microscopy', 'Dec 2010 PfPR values',
                                '2010 Dec PfPR by archetype')


all_pfpr_2010 <- tmap_arrange(pfpr.archetype_oct,
                              pfpr.archetype_nov, pfpr.archetype_dec)


tmap_save(tm =all_pfpr_2010, filename = "results/maps/2010_archetype/pfpr_2010_repDS.pdf", width=13, height=13, units ="in", asp=0,
          paper ="A4r", useDingbats=FALSE)



