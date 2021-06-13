/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      13JUL18
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
data ADM.Accident    ;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile '/home/chiruvvs0/dftRoadSafety_Accidents_2016.csv' delimiter = ',' MISSOVER DSD  firstobs=2 ;
input
Accident_Index :$13.
Location_Easting_OSGR
Location_Northing_OSGR
Longitude
Latitude
Police_Force
Accident_Severity
Number_of_Vehicles
Number_of_Casualties
Date :$10.
Day_of_Week
Time :$5.
"Local_Authority_(District)"N 
"Local_Authority_(Highway)"N  :$9.
"1st_Road_Class"N
"1st_Road_Number"N
Road_Type
Speed_limit :$4.
Junction_Detail
Junction_Control
"2nd_Road_Class"N
"2nd_Road_Number"N
"Pedestrian_Crossing-Human_Contro"N
"Pedestrian_Crossing-Physical_Fac"N
Light_Conditions
Weather_Conditions
Road_Surface_Conditions
Special_Conditions_at_Site
Carriageway_Hazards
Urban_or_Rural_Area
Police_Ofcr_Atnd_Scn_of_Acc
LSOA_of_Accident_Location  :$9.
;
if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;

proc sort data=ADM.Accident (where=(Accident_Index ne '#############')) nodupkey ;
by Accident_Index ;
run;