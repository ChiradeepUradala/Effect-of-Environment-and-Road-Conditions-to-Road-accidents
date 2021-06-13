/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      13JUL18
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
data ADM.CASUALITIES    ;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile '/home/chiruvvs0/Cas.csv' delimiter = ',' MISSOVER DSD lrecl=32767 firstobs=2 ;
input
Accident_Index :$13.
Vehicle_Reference
Casualty_Reference
Casualty_Class
Sex_of_Casualty
Age_of_Casualty
Age_Band_of_Casualty
Casualty_Severity
Pedestrian_Location
Pedestrian_Movement
Car_Passenger
Bus_or_Coach_Passenger
Pedestrian_Rd_Main_Wrkr
Casualty_Type
Casualty_Home_Area_Type
Casualty_IMD_Decile
;

if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;


proc sort data=ADM.CASUALITIES (where=(Accident_Index ne '#############' )) nodupkey ;
by Accident_Index Vehicle_Reference ;
run;

