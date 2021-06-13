#############################################################################################################################################
#############################################################################################################################################
############################      IMPACT OF ENVIRONMENTAL AND ROAD CONDITIONS                               #################################
############################                  ON ROAD ACCIDENTS IN UNITED KINGDOM                           #################################
############################                                USING NAIVE BAYES TECHNIQUE                     #################################
#############################################################################################################################################
#############################################################################################################################################



##################################### Set the working directory  ###########################################################################

setwd("C:/Users/chira/OneDrive/Desktop/Road_Accident_2016") 

#####################################  Library Assignment        ###########################################################################

library('dplyr')
library('ROCR')
library('Metrics')
library('caret')
library('randomForest')
library('ggplot2')
library('ggthemes')
library('Boruta')
library('tidyr')
library('lazyeval')
library('VIM')
library('ebmc')
library('e1071')

################################################# Import Accident data into R    ###########################################################

datafinal <- read.csv("Accident_Final.csv", stringsAsFactors = T , na.strings=c("NULL","Missing","-1","",".","NA")) #will autoencode the text attributes to factors


#######################################   Datafinal having 72 Variables           ##########################################################
#######################################  Remove the ID variable Accident_Index    ##########################################################

datafinal <- datafinal[-1]


######################################## Dealing with factorial variables ##################################################################
######################################### Factors provided in data sheet  ##################################################################

datafinal$Accident_Severity <- factor(datafinal$Accident_Severity, levels = c(1,2,3), labels=c("Fatal", "Serious", "Slight"))
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


###################################################  Analysing Missing values in the data      #############################################

aggr_plot <- aggr(datafinal, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


gg_miss_var(datafinal) + labs(y = "Look at all the missing ones")





################################################ Collinearity check between scaled variables    ############################################

datafinal %>%
  filter(Accident_Severity == "1") %>%
  select_if(is.numeric) %>%
  cor() %>%
  corrplot::corrplot()

################################################ Checking the distribution of the scaled variables            ##############################

require(tidyr)
require(dplyr)
require(lazyeval)

datafinal %>% 
  select_if(is.numeric) %>% 
  gather(metric, value) %>% 
  ggplot(aes(value, fill = metric)) + 
  geom_density(show.legend = FALSE) + 
  facet_wrap(~ metric, scales = "free")


cd_plot(Accident_Severity ~ Speed_limit, data = datafinal, main = "TM")
spineplot(Improved ~ Age, data = datafinal)
cd_plot(Accident_Severity ~ as.numeric(Weather_Conditions), data = datafinal, main = "TM")



#################################################   Feature selection              #########################################################

###################    1.)   Feature selection using Random Forest

model_rf<-randomForest(Accident_Severity ~ ., data = datafinal)
order(importance(model_rf))

model_rf<-randomForest(Number_of_Casualties ~ ., data = datafinal)
importance(model_rf)

##################     2.)   Feature selection using Boruta 


boruta_output <- Boruta(Number_of_Casualties ~ ., data=na.omit(datafinal), doTrace=2)  # perform Boruta search Number of casualities
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 


boruta_output <- Boruta(Accident_Severity ~ ., data=na.omit(datafinal), doTrace=2)  # perform Boruta search Accident_Severity
boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  
plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance") 

################################################# Variables selected after feature selection ###############################################

data1 <- subset(datafinal, select = c(Casualty_IMD_Decile,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.,Engine_Capacity_.CC.,
                                      Age_Band_of_Driver,Age_of_Driver,Vehicle_Type,Number_of_Casualties,
                                      Junction_Location,Age_of_Vehicle,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                      Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,Vehicle_IMD_Decile,
                                      Driver_IMD_Decile,X2nd_Road_Class,Junction_Detail,X1st_Road_Class,
                                      Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                      Light_Conditions,Journey_Purpose_of_Driver,Junction_Control,Propulsion_Code,Vehicle_Leaving_Carriageway,
                                      Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                      Sex_of_Driver,Road_Surface_Conditions,Casualty_Home_Area_Type))


############################## Missing values pattern in the selected features

vis_miss(data1,warn_large_data=FALSE)



#################################################  Imputation , dealing with NA's  #########################################################
################################################# Due to high categorical data we have finalised with mice  ################################

imputed_Data <- mice(data1, m=5, maxit = 50, method = 'cart', seed = 500) ##Sample code for Imputation
data1 <- complete(imputed_Data,1)





#################################################   SECTION  1  START   ####################################################################
#################################################   Model selection     ####################################################################
############################################################################################################################################
############################################################################################################################################




##########################################################################################################################################
#####################################           BASE MODEL       #########################################################################


datawithoutna <- subset(data1, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                              ,Age_of_Driver,Vehicle_Type,
                                              Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                              Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                              Junction_Detail,X1st_Road_Class,
                                              Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                              Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                              Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                              Sex_of_Driver,Road_Surface_Conditions))




####################################10-fold cross validation     ##########################################################################
###########################################################################################################################################

#############################    Sampling

x <- sample(100000,20000)
datawithoutna <- datawithoutna[x,] 

control <- trainControl(method="repeatedcv", number=10, repeats=5) #5 x 10-fold cv
metric <- "Kappa" 
train(Accident_Severity~., data=datawithoutna, method="nb", metric=metric, trControl=control)

accident_pred <- predict(accident_model,accident_test)



############################## Sampling the data
set.seed(17157536)
train_sampleBASE <- sample(200000,160000)
accident_trainBASE <- datawithoutna[train_sampleBASE,]
accident_testBASE  <- datawithoutna[-train_sampleBASE,]#


model_nyve_train <- naiveBayes(accident_trainBASE[-1] , accident_trainBASE$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")

########################################## ROC Curve Graph       ##########################################################################


pWomenModel <- prediction(as.numeric(model_predict_nvye_bys),as.numeric(accident_test$Accident_Severity))
perfWomenModel <- performance(pWomenModel, measure = "tpr", x.measure = "fpr")
plot(perfWomenModel)

auc <- performance(pWomenModel, measure = "auc")
auc <- auc@y.values[[1]]
auc 
# 0.5765081

############################################################################################################################################
####################################################Check accuracy of model without few important features##################################


withoutall <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,X1st_Road_Number,Local_Authority_.District.
                                               ,Age_of_Driver,Vehicle_Type,
                                               Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                               Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                               Junction_Detail,X1st_Road_Class,
                                               X2nd_Road_Number,Hit_Object_in_Carriageway,
                                               Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                               Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                               Sex_of_Driver))

### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutall[train_sample,]
accident_test  <- withoutall[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")

###########################################################################################################################################
################################################## Model accuracy without Road Type########################################################


withoutRoad_Type <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                     ,Age_of_Driver,Vehicle_Type,
                                                     Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                     Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                     Junction_Detail,X1st_Road_Class,
                                                     X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                     Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                     Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                     Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutRoad_Type[train_sample,]
accident_test  <- withoutRoad_Type[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")

############################################################################################################################################
################################################## Model accuracy without Weather conditions################################################

withoutWeather_Conditions <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                              ,Age_of_Driver,Vehicle_Type,
                                                              Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                              Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                              Junction_Detail,X1st_Road_Class,
                                                              Road_Type,X2nd_Road_Number,Speed_limit,Hit_Object_in_Carriageway,
                                                              Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                              Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                              Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutWeather_Conditions[train_sample,]
accident_test  <- withoutWeather_Conditions[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")
############################################################################################################################################
################################################## Model accuracy without Speed Limit#######################################################

withoutSpeed_limit <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                       ,Age_of_Driver,Vehicle_Type,
                                                       Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                       Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                       Junction_Detail,X1st_Road_Class,
                                                       Road_Type,X2nd_Road_Number,Weather_Conditions,Hit_Object_in_Carriageway,
                                                       Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                       Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                       Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutSpeed_limit[train_sample,]
accident_test  <- withoutSpeed_limit[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")

############################################################################################################################################
################################################## Model accuracy without Light Conditions##################################################

withoutLight_Conditions <-subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                           ,Age_of_Driver,Vehicle_Type,
                                                           Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                           Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                           Junction_Detail,X1st_Road_Class,
                                                           Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                           Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                           Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                           Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutLight_Conditions[train_sample,]
accident_test  <- withoutLight_Conditions[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")

############################################################################################################################################
################################################## Model accuracy without Road surface conditions###########################################

withoutRoad_Surface_Conditions <-subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                                  ,Age_of_Driver,Vehicle_Type,
                                                                  Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                                  Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                                  Junction_Detail,X1st_Road_Class,
                                                                  Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                                  Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                                  Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                                  Sex_of_Driver))



### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutRoad_Surface_Conditions[train_sample,]
accident_test  <- withoutRoad_Surface_Conditions[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")


#############################################################################################################################################
################################################## Model accuracy without Age of Driver######################################################

withoutAge_of_Driver <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                         ,Vehicle_Type,
                                                         Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                         Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                         Junction_Detail,X1st_Road_Class,
                                                         Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                         Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                         Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                         Sex_of_Driver,Road_Surface_Conditions))



### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutAge_of_Driver[train_sample,]
accident_test  <- withoutAge_of_Driver[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")


#############################################################################################################################################
################################################## Model accuracy without vehicle Type#######################################################

withoutVehicle_Type <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                        ,Age_of_Driver,
                                                        Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                        Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                        Junction_Detail,X1st_Road_Class,
                                                        Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                        Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                        Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                        Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutVehicle_Type[train_sample,]
accident_test  <- withoutVehicle_Type[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")

#############################################################################################################################################
################################################## Model accuracy without Number of Vehicles#################################################

withoutNumber_of_Vehicles <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                              ,Age_of_Driver,Vehicle_Type,
                                                              Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                              Pedestrian_Crossing.Physical_Fac,
                                                              Junction_Detail,X1st_Road_Class,
                                                              Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                              Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                              Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                              Sex_of_Driver,Road_Surface_Conditions))



### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutNumber_of_Vehicles[train_sample,]
accident_test  <- withoutNumber_of_Vehicles[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")


#############################################################################################################################################
################################################## Model accuracy without Sex of Driver######################################################

withoutSex_of_Driver <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                         ,Age_of_Driver,Vehicle_Type,
                                                         Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                         Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                         Junction_Detail,X1st_Road_Class,
                                                         Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                         Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                         Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                         Road_Surface_Conditions))



### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutSex_of_Driver[train_sample,]
accident_test  <- withoutSex_of_Driver[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")


#############################################################################################################################################
################################################## Model accuracy without Journey Purpose of Driver##########################################

withoutJourney_Purpose_of_Driver <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                                     ,Age_of_Driver,Vehicle_Type,
                                                                     Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                                     Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                                     Junction_Detail,X1st_Road_Class,
                                                                     Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                                     Light_Conditions,Vehicle_Leaving_Carriageway,
                                                                     Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                                     Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutJourney_Purpose_of_Driver[train_sample,]
accident_test  <- withoutJourney_Purpose_of_Driver[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")


#############################################################################################################################################
################################################## Model accuracy without Junction Location##################################################

withoutJunction_Location <-  subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                                              ,Age_of_Driver,Vehicle_Type,
                                                              Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                              Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                              Junction_Detail,X1st_Road_Class,
                                                              Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                              Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                              Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                              Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutJunction_Location[train_sample,]
accident_test  <- withoutJunction_Location[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")

#############################################################################################################################################
################################################## Model accuracy without Day of week########################################################

withoutday_of_week <- subset(datawithoutna, select = c(Accident_Severity,Casualty_Type,X1st_Road_Number,Local_Authority_.District.
                                                       ,Age_of_Driver,Vehicle_Type,
                                                       Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                                       Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                                       Junction_Detail,X1st_Road_Class,
                                                       Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                                       Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                                       Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                                       Sex_of_Driver,Road_Surface_Conditions))


### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
accident_train <- withoutday_of_week[train_sample,]
accident_test  <- withoutday_of_week[-train_sample,]#
dim(accident_train)

model_nyve_train <- naiveBayes(accident_train[-1] , accident_train$Accident_Severity, laplace=0)

##model_predict_nvye_bys <- predict(model_nyve_train,accident_test)

## Control variables
model_predict_nvye_bys <- predict(model_nyve_train,accident_test,type = "class")

confusionMatrix(model_predict_nvye_bys, accident_test$Accident_Severity, positive = "Yes")


#########################################            SECTION 1     END                  #####################################################
#############################################################################################################################################
#############################################################################################################################################


########################################             SECTION 2     START              #######################################################
########################################             BOOSTING   ebmc                  #######################################################

datawithoutna <- subset(datafinal, select = c(Accident_Severity,Casualty_Type,Day_of_Week,X1st_Road_Number,Local_Authority_.District.
                                              ,Age_of_Driver,Vehicle_Type,
                                              Junction_Location,Vehicle_Manoeuvre,X1st_Point_of_Impact,
                                              Pedestrian_Crossing.Physical_Fac,Number_of_Vehicles,
                                              Junction_Detail,X1st_Road_Class,
                                              Road_Type,X2nd_Road_Number,Weather_Conditions,Speed_limit,Hit_Object_in_Carriageway,
                                              Light_Conditions,Journey_Purpose_of_Driver,Vehicle_Leaving_Carriageway,
                                              Vehicle_Location.Restricted_Lane,Skidding_and_Overturning,Hit_Object_in_Carriageway,
                                              Sex_of_Driver,Road_Surface_Conditions))

####################################### For ebmc , dependent variable must have 2 levels

datawithoutna$Accident_Severity <- as.number(datawithoutna$Accident_Severity)


####################################### Encoding levels 3 as 0 and level 1,2 as 1 ##########################################################
datawithoutnacat$Accident_Severity[which(datawithoutnacat$Accident_Severity=="3")]<-"0"
datawithoutnacat$Accident_Severity[which(datawithoutnacat$Accident_Severity=="1")]<-"1"
datawithoutnacat$Accident_Severity[which(datawithoutnacat$Accident_Severity=="2")]<-"1"


#######################################  converting back to factor
datawithoutnacat$Accident_Severity <- as.factor(datawithoutnacat$Accident_Severity)

### Sampling the data
set.seed(17157536)
train_sample <- sample(200000,160000)
train <- datawithoutnacat[train_sample,]
test  <- datawithoutnacat[-train_sample,]#

model1 <- adam2(Accident_Severity ~ ., data = train, size = 10, alg = "nb")
model_predict_nvye_bys <- predict(model1,test,na.action = na.pass)

confusionMatrix(model_predict_nvye_bys, test$Accident_Severity,positive = '1')


##########################################   2ND METHOD 
  
model2 <- rus(Accident_Severity ~ ., data = train, size = 10, alg = "nb")
model_predict_nvye_bys <- predict(model2,test,na.action = na.pass)

# Calculate measurements


prob <- predict(model2, newdata = test, type = "prob")
auc <- measure(label = test$Accident_Severity,probability = prob, metric = "auc")
gmean <- measure(label = test$Accident_Severity, probability = prob, metric = "gmean", threshold = 0.5)


##########################################   3RD METHOD



model <- ub(Accident_Severity ~ ., data = train, size = 10, alg = "nb") # Build UnderBagging model
prob <- predict(model, newdata = test, type = "prob") # return probability estimation
pred <- predict(model, newdata = test, type = "class") # return predicted class

confusionMatrix(model, test$Accident_Severity, positive = "Yes")


########################################             SECTION 2     END                 #######################################################
