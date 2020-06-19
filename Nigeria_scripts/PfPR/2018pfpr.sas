libname pfpr "C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\pfpr";


/*data pfpr;*/
/*set pfpr.pfpr2018;*/
/*run;*/
/*MM = hv008 - ((hv007 - 1900) * 12);*/
/*YYYY = floor((hv008 - 1)/12)+1900;*/
/*date = mdy(MM,1,YYYY);*/
/*format date yymms7.;*/
/*wt =hv005/1000000;*/
/*run;*/


*2010 microscopy pfpr after manual import;

proc freq data=Pfpr2010_v2;
table repDS;
run;



/*proc import datafile="C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\" */
/*out=mydata dbms=csv replace; */
/*getnames=no; */
/*run;*/

ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\pfpr\2010pfpr_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = Pfpr2010_v2;
tables repDS*p_test*time2/row col cl(type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum hv022;
weight wt;
run;

ods tagsets.excelxp close;



*2010 RDT pfpr after manual import;

proc freq data=Pfpr_rdt;
table repDS;
run;



/*proc import datafile="C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\" */
/*out=mydata dbms=csv replace; */
/*getnames=no; */
/*run;*/

ODS TAGSETS.EXCELXP
file='C:\Users\ido0493\Box\NU-malaria-team\data\nigeria_dhs\data_analysis\bin\pfpr\2010pfprRDT_SAS.xls'
STYLE=minimal
OPTIONS ( Orientation = 'landscape'
FitToPage = 'yes'
Pages_FitWidth = '1'
Pages_FitHeight = '100' );

proc surveyfreq data = Pfpr_rdt;
tables repDS*p_testRDT*time2/row col cl(type= CLOPPERPEARSON psmall=0.28)alpha=0.20;
stratum hv022;
weight wt;
run;

ods tagsets.excelxp close;

