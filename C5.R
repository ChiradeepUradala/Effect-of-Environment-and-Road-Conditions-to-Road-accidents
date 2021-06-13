## Set the working directory
setwd("C:/Users/chira/OneDrive/Desktop/Road_Accident_2016") 

## Import Accident data into R
datafinal <- read.csv("Accident_Final.csv", stringsAsFactors = T , na.strings=c("NULL","Missing","-1","",".","NA")) #will autoencode the text attributes to factors

library('Metrics')
library(caret)
library('randomForest')
library('ggplot2')
library('ggthemes')
library('Boruta')
library('dplyr')

##Remove the ID variable Accident_Index
datafinal <- datafinal[-1]


## Dealing with factorial variables
datafinal$Accident_Severity <- factor(datafinal$Accident_Severity, levels = c(1,2,3), labels=c("Fatal", "Serious", "Slight"))
datafinal$Accident_Severity <- as.factor(datafinal$Accident_Severity) 
datafinal$Day_of_Week <- factor(datafinal$Day_of_Week, levels = c(1,2,3,4,5,6,7), labels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
datafinal$Police_Force <- factor(datafinal$Police_Force, levels = c(1,3,4,5,6,7,10,11,12,13,14,16,17,20,21,22,23,30,31,32,33,34,35,36,37,40,41,42,43,44,45,46,47,48,50,52,53,54,55,60,61,62,63,91,92,93,94,95,96,97,98),
                                 labels=c("Metropolitan Police","Cumbria","Lancashire","Merseyside","Greater Manchester","Cheshire","Northumbria","Durham","North Yorkshire","West Yorkshire","South Yorkshire","Humberside",
                                          "Cleveland","West Midlands","Staffordshire","West Mercia","Warwickshire","Derbyshire","Nottinghamshire","Lincolnshire",
                                          "Leicestershire","Northamptonshire","Cambridgeshire","Norfolk","Suffolk","Bedfordshire","Hertfordshire","Essex",
                                          "Thames Valley","Hampshire","Surrey","Kent","Sussex","City of London","Devon and Cornwall","Avon and Somerset",
                                          "Gloucestershire","Wiltshire","Dorset","North Wales","Gwent","South Wales","Dyfed-Powys","Northern","Grampian",
                                          "Tayside","Fife","Lothian and Borders","Central","Strathclyde","Dumfries and Galloway"))
datafinal$Local_Authority_.District. <- as.factor(datafinal$Local_Authority_.District.)
datafinal$Local_Authority_.Highway. <- as.factor(datafinal$Local_Authority_.Highway.)
datafinal$X1st_Road_Class <- as.factor(datafinal$X1st_Road_Class)
datafinal$Road_Type <- as.factor(datafinal$Road_Type)
datafinal$Junction_Detail <- as.factor(datafinal$Junction_Detail)
datafinal$Junction_Control <- as.factor(datafinal$Junction_Control) 
datafinal$X2nd_Road_Class <-as.factor(datafinal$X2nd_Road_Class)
datafinal$Pedestrian_Crossing.Human_Contro <- as.factor(datafinal$Pedestrian_Crossing.Human_Contro) 
datafinal$Pedestrian_Crossing.Physical_Fac <- as.factor(datafinal$Pedestrian_Crossing.Physical_Fac)
datafinal$Light_Conditions <- as.factor(datafinal$Light_Conditions)
datafinal$Weather_Conditions <- as.factor(datafinal$Weather_Conditions)
datafinal$Road_Surface_Conditions <- as.factor(datafinal$Road_Surface_Conditions)
datafinal$Special_Conditions_at_Site <- as.factor(datafinal$Special_Conditions_at_Site)
datafinal$Carriageway_Hazards <- as.factor(datafinal$Carriageway_Hazards)
datafinal$Urban_or_Rural_Area <- as.factor(datafinal$Urban_or_Rural_Area)
datafinal$Police_Ofcr_Atnd_Scn_of_Acc <- as.factor(datafinal$Police_Ofcr_Atnd_Scn_of_Acc)
datafinal$Vehicle_Type <- as.factor(datafinal$Vehicle_Type)
datafinal$Towing_and_Articulation <- as.factor(datafinal$Towing_and_Articulation)
datafinal$Vehicle_Manoeuvre <- as.factor(datafinal$Vehicle_Manoeuvre)
datafinal$Vehicle_Location.Restricted_Lane <- as.factor(datafinal$Vehicle_Location.Restricted_Lane)
datafinal$Junction_Location <- as.factor(datafinal$Junction_Location)
datafinal$Skidding_and_Overturning <- as.factor(datafinal$Skidding_and_Overturning)
datafinal$Hit_Object_in_Carriageway <- as.factor(datafinal$Hit_Object_in_Carriageway)
datafinal$Vehicle_Leaving_Carriageway <- as.factor(datafinal$Vehicle_Leaving_Carriageway)
datafinal$Hit_Object_off_Carriageway <- as.factor(datafinal$Hit_Object_off_Carriageway)
datafinal$X1st_Point_of_Impact <- as.factor(datafinal$X1st_Point_of_Impact)
datafinal$Was_Vehicle_Left_Hand_Drive <- as.factor(datafinal$Was_Vehicle_Left_Hand_Drive)
datafinal$Journey_Purpose_of_Driver <- as.factor(datafinal$Journey_Purpose_of_Driver)
datafinal$Sex_of_Driver <- as.factor(datafinal$Sex_of_Driver)
datafinal$Age_of_Driver <- as.factor(datafinal$Age_of_Driver)
datafinal$Age_Band_of_Driver <- as.factor(datafinal$Age_Band_of_Driver)
datafinal$Propulsion_Code <- as.factor(datafinal$Propulsion_Code)
datafinal$Casualty_Class <- as.factor(datafinal$Casualty_Class)
datafinal$Sex_of_Casualty <- as.factor(datafinal$Sex_of_Casualty)
datafinal$Casualty_Severity <- as.factor(datafinal$Casualty_Severity)
datafinal$Age_of_Casualty <- as.numeric(datafinal$Age_of_Casualty)
datafinal$Pedestrian_Location <- as.factor(datafinal$Pedestrian_Location)
datafinal$Pedestrian_Movement <- as.factor(datafinal$Pedestrian_Movement)
datafinal$Car_Passenger <- as.factor(datafinal$Car_Passenger)
datafinal$Bus_or_Coach_Passenger <- as.factor(datafinal$Bus_or_Coach_Passenger)
datafinal$Pedestrian_Rd_Main_Wrkr <- as.factor(datafinal$Pedestrian_Rd_Main_Wrkr)
datafinal$Casualty_Type <- as.factor(datafinal$Casualty_Type)
datafinal$Casualty_IMD_Decile <- as.factor(datafinal$Casualty_IMD_Decile)
datafinal$Casualty_Home_Area_Type <- as.factor(datafinal$Casualty_Home_Area_Type)
datafinal$Age_of_Driver <- as.numeric(datafinal$Age_of_Driver)
datafinal$Date <- as.Date(datafinal$Date)
datafinal$Driver_Home_Area_Type <- as.factor(datafinal$Driver_Home_Area_Type)

dim(datafinal)

sapply(datafinal,class)
data1 <- subset(datafinal, select = c(Casualty_IMD_Decile,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.,Engine_Capacity_.CC.,
                                         Age_Band_of_Driver,Age_of_Driver,Vehicle_Type,Number_of_Casualties,
                                         Junction_Location,Age_of_Vehicle,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                         Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,Vehicle_IMD_Decile,
                                         Driver_IMD_Decile,X2nd_Road_Class,Junction_Detail,X1st_Road_Class,
                                         Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                         Light_Conditions,Journey_Purpose_of_Driver,Junction_Control,Propulsion_Code,Vehicle_Leaving_Carriageway,
                                         Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                         Sex_of_Driver,Road_Surface_Conditions,Casualty_Home_Area_Type))


datawithoutna <- subset(datafinal, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                     ,Age_of_Driver,Vehicle_Type,
                                      Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                      Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                      Junction_Detail,X1st_Road_Class,
                                      Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                      Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                      Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                      Sex_of_Driver,Road_Surface_Conditions))

##Find sum of NA's
sum(is.na(data1$Casualty_Home_Area_Type))

## Omit all NA's since values less than 1000 out of 250000
datawithoutna <- na.omit(datawithoutna)

set.seed(1337)
index <- sample(1:length(datawithoutna$Accident_Severity), length(datawithoutna$Accident_Severity) * .25, replace=FALSE)
testing <- datawithoutna$Accident_Severity[index]

### Sampling the data
train_sample <- sample(100000,80000)
accident_train <- datawithoutna[train_sample,]
accident_test  <- datawithoutna[-train_sample,]#
library(e1071)
library(C50)

##C5.0 hold out
accident_model <- C5.0(Accident_Severity ~.,data=accident_train,trials = 20) 

##C5.0 cross validation
accident_model <- C5.0(Accident_Severity ~.,data=accident_train, control = C5.0Control(winnow = FALSE))
accuracy(accident_model)
##K-fold cross validation

#Sampling
x <- sample(100000,3000)
datawithoutna <- datawithoutna[x,] 

control <- trainControl(method="repeatedcv", number=5, repeats=5) #5 x 10-fold cv
metric <- "Kappa" 
train(Accident_Severity~., data=datawithoutna, method="C5.0", metric=metric, trControl=control)


plot(accident_model)

accident_pred <- predict(accident_model,accident_test)
library(gmodels)
library(caret)
CrossTable(accident_test$Accident_Severity, accident_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE)

confusionMatrix(accident_pred, accident_test$Accident_Severity, positive = "Yes")
