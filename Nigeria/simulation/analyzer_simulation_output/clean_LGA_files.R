
#Clean LGA files

LGA_funder = read.csv(file.path(WorkDir, '2020_to_2025_v6', 'LGA_funder.csv'))
scen_1 = read.csv(file.path(WorkDir, '2020_to_2030_v5', 'NGA projection scenario 1','malariaBurden_withAdjustments.csv'))

scen_1_LGA = unique(scen_1$LGA)
'%ni%' <- Negate('%in%')



LGA_funder$LGA =  str_replace(LGA_funder$LGA, '\\/', '-')

LGA_funder = LGA_funder %>%  mutate(LGA= case_when(LGA =='Ogba-Egbema/Ndoni' ~ "Ogba-Egbema-Ndoni",
                                                   LGA =='kiyawa' ~ 'Kiyawa',
                                                   LGA == 'kaita' ~'Kaita',
                                                   TRUE ~as.character(LGA)))