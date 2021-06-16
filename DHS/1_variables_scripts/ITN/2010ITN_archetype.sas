*u5 itn;


proc freq data=Itn_u5;
table hh_itn;
run;

data Itn_u5_v2;
set Itn_u5;
if repDS = "NA" then delete;
run;



ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\ITN_DHS_defined\2010_U5ITN_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = Itn_u5_v2;
tables repDS*hh_itn/row col cl(type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
weight wt;
cluster v021;
run;

ods tagsets.excelxp close;


*six - nine;


proc freq data=Six_ten2010ITN_v2;
table hv105;
run;

data Six_nine2010ITN;
set Six_ten2010ITN;
if repDS = "NA" then delete;
run;



ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\ITN_DHS_defined\2010_six_nineITN_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = Six_nine2010ITN;
tables repDS*hh_itn/row col cl(type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum hv022;
weight wt;
cluster hv021;
run;

ods tagsets.excelxp close;





*10 - 18;


proc freq data=Ten_eight2010itn;
table hh_itn;
run;

data Ten_eight2010itn_v2;
set Ten_eight2010itn;
if repDS = "NA" then delete;
run;



ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\ITN_DHS_defined\2010_ten_eightITN_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = Ten_eight2010itn_v2;
tables repDS*hh_itn/row col cl(type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum hv022;
weight wt;
cluster hv021;
run;

ods tagsets.excelxp close;







* > 18;


proc freq data=Over_eighteen2010itn;
table repDS;
run;

data Over_eighteen2010itn_v2;
set Over_eighteen2010itn;
if repDS = "NA" then delete;
run;



ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\ITN_DHS_defined\2010_over_eighteenITN_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = Over_eighteen2010itn_v2;
tables repDS*hh_itn/row col cl(type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum hv022;
weight wt;
cluster hv021;
run;

ods tagsets.excelxp close;




