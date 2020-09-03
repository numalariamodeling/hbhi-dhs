PROC IMPORT OUT= WORK.NIGPFPR_v2 
            DATAFILE= "C:\Users\ido0493\Box\NU-malaria-team\data\nigeria
_dhs\data_analysis\bin\pfpr\New folder\pfpr2018.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
RUN;
