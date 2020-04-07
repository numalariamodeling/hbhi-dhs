
*2015 medical treatment for fever*;
ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\med_fever_15.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = rdata;
tables sstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
Cluster v021;
weight wt;
run;


ods tagsets.excelxp close;


*2013 medical treatment for fever*;
ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\med_fever_13.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );


proc surveyfreq data = rdata_13;
tables sstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
Cluster v021;
weight wt;
run;


ods tagsets.excelxp close;


*2010 medical treatment for fever*;
ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\med_fever_10.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = rdata_10;
tables sstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
Cluster v021;
weight wt;
run;


ods tagsets.excelxp close;


*2008 medical treatment for fever*;
ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\med_fever_08.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );


proc surveyfreq data = rdata_08;
tables sstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
Cluster v021;
weight wt;
run;


ods tagsets.excelxp close;


*2003 medical treatment for fever*;
ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\med_fever_03.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );


proc surveyfreq data = rdata_03;
tables sstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
Cluster v021;
weight wt;
run;


ods tagsets.excelxp close;



*1990 medical treatment for fever*;
ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\med_fever_90.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );


proc surveyfreq data = rdata_90;
tables sstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum v022;
Cluster v021;
weight wt;
run;


ods tagsets.excelxp close;
