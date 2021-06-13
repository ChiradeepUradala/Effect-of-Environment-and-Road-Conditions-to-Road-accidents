/**********************************************************************
*   PRODUCT:   SAS
*   VERSION:   9.4
*   CREATOR:   External File Interface
*   DATE:      13JUL18
*   DESC:      Generated SAS Datastep Code
*   TEMPLATE SOURCE:  (None Specified.)
***********************************************************************/
data ADM.Vehicle    ;
%let _EFIERR_ = 0; /* set the ERROR detection macro variable */
infile '/home/chiruvvs0/Veh.csv' delimiter = ',' MISSOVER DSD  firstobs=2 ;
input
Accident_Index :$13.
Vehicle_Reference
Vehicle_Type
Towing_and_Articulation
Vehicle_Manoeuvre
"Vehicle_Location-Restricted_Lane"N
Junction_Location
Skidding_and_Overturning
Hit_Object_in_Carriageway
Vehicle_Leaving_Carriageway
Hit_Object_off_Carriageway
"1st_Point_of_Impact"N
"Was_Vehicle_Left_Hand_Drive?"N
Journey_Purpose_of_Driver
Sex_of_Driver
Age_of_Driver
Age_Band_of_Driver
"Engine_Capacity_(CC)"N
Propulsion_Code
Age_of_Vehicle
Driver_IMD_Decile
Driver_Home_Area_Type
Vehicle_IMD_Decile
;
if _ERROR_ then call symputx('_EFIERR_',1);  /* set ERROR detection macro variable */
run;


proc sort data=ADM.Vehicle (where=(Accident_Index ne '#############')) nodupkey ;
by Accident_Index Vehicle_Reference;
run;