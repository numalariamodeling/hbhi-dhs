libname nigeria "C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\data\SAS datasets";

*medical treatment for fever among adults in 2010;
data med_fev_10;
set nigeria.NGPR61FL; 
if hv105 < 6 then delete;
if sh13 = 98 then delete;
if sh13 NE . and sh13 < 31 then med_fever =1;
else if sh13 NE . and sh13 >= 31 then med_fever = 0;
wt=hv005/1000000;
run;


proc freq data = med_fev_10;
tables hv005;
run;


ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\med_fever_A_10_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );


proc surveyfreq data = med_fev_10;
tables shstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
 strata hv022;
 cluster hv021;
 weight wt;
  title "Overweight Prevalence In Nigeria";
run;  

ods tagsets.excelxp close;

libname ngaer "C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\data\SAS datasets\NGPR71SD";

*medical treatment for fever among adults in 2015;

data med_fev_15;
set ngaer.NGPR71FL; 
if hv105 < 6 then delete;
if sh15 = 98 then delete;
if sh15 NE . and sh15 < 8 then med_fever =1;
else if sh15 NE . and sh15 >= 8 then med_fever = 0;
wt=hv005/1000000;
run;


proc freq data = med_fev_15;
tables med_fever;
run;


ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\med_fever_A_15_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );


proc surveyfreq data = med_fev_15;
tables shstate*med_fever/row cl (type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
 strata hv022;
 cluster hv021;
 weight wt;
  title "Overweight Prevalence In Burkina Faso";
run;  

ods tagsets.excelxp close;
