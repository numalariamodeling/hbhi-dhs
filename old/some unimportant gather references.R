###########################################################################################
#---Changing data into long format to aggregate observations for LLINS across children---# 
###########################################################################################

####This is for fever in the last 2 wks mis 2017 
mis17_fever_l<- mis17_work3%>%dplyr::select(b2_01:b2_04, h22_1:h22_4, caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  gather(key = fever_status, value = fever, h22_1:h22_4)

####This is for fever in the last 2 wks mis 2014 
mis14_fever_l<- mis14_work4%>%dplyr::select(b2_01:b2_04, h22_1:h22_4, caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  gather(key = fever_status, value = fever, h22_1:h22_4)

####This is for receiving medical treatment for fever in 2017 
mis17_medf_l<- mis17_work5%>%dplyr::select(b2_01:b2_04, h32z_1:h32z_3, caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  gather(key = medf_status, value = medical, h32z_1:h32z_3)

####This is for receiving medical treatment for fever in 2014 
mis14_medf_l<- mis14_work%>%dplyr::select(b2_01:b2_04, h32z_1:h32z_3, caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  gather(key = medf_status, value = medical, h32z_1:h32z_3)

####This is for receiving ACT fever in 2017 
mis17_act_l<- mis17_work%>%dplyr::select(b2_01:b2_04, ml13e_1:ml13e_3, caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  gather(key = medct_status, value = act, ml13e_1:ml13e_3)

####This is for receiving ACT fever in 2014 
mis14_act_l<- mis14_work%>%dplyr::select(b2_01:b2_04, ml13e_1:ml13e_3, caseid, v001, v005, v021, v022, v024,NOMDEP)%>%
  gather(key = medct_status, value = act, ml13e_1:ml13e_3)
