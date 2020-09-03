*u5 2010 act;

proc freq data=U5_act;
table comboACT;
run;


data U5_act_v2;
set U5_act;
if repDS = "NA" then delete;
run;


ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\U5_ACT_use\2010_U5_ACT_use_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = U5_act_v2;
tables repDS*comboACT/row col cl(type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
weight wt;
cluster v021;
run;

ods tagsets.excelxp close;
