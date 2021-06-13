Data ADM.Accident_Final;
Merge ADM.CASUALITIES ADM.Vehicle ADM.MakeModel ADM.Accident  ;
by Accident_Index ;
run;

proc export data=ADM.Accident_Final 
   outfile='/home/chiruvvs0/Accident_Final.csv'
   dbms=csv
   replace;
run;