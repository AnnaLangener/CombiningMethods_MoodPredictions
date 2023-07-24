
######## Shapley Values ##########

library("iml") # Feature Importance
library("partykit")
library("party")
library('dplyr') # To run best ml model again
library("tidyr")
library("caret")
library("doParallel")
library("Metrics")
library("ggplot2")
library("TSrepr") #mape
library("SHAPforxgboost")
library("ggplot2")
library("foreach")
library("forcats")

# Positive Affect: 1 (all),2 (all),3(X),4,5,6(all),7(all),8(all),9 
# Negative Affect: 4, 5, 6 (all), 7 (all), 8, 10 (all is neg), 11 

###### Choose participant and outcome ######
PNumbers <- 11
outcome <- "na_mean"
only_all = TRUE
samplesize = 1000 # https://arxiv.org/pdf/2110.01307.pdf
############################################


if(outcome == "na_mean" & only_all == FALSE){
Best <- read.csv("Results/Best_DataG_lag1_na_mean.csv")
}else if(outcome == "na_mean" & only_all == TRUE){
  Best <- read.csv("Results/Best_all_DataG_lag1_na_mean.csv")
}else if(outcome == "pa_mean" & only_all == FALSE){
  Best <- read.csv("Results/Best_DataG_lag1_pa_mean.csv")
}else if(outcome == "pa_mean" & only_all == TRUE){
  Best <- read.csv("Results/Best_all_DataG_lag1_pa_mean.csv")
}

index <- Best[Best$Participant == PNumbers,] # with this we can extract the best window and level of aggregation
predictors <- index$predictors # "all", onlypassive", "onlyESM", "onlyEgo" (extract the best set of predictors, if "only_all = TRUE" then all predictors are chosen)


####### Load and Prepare Data #######
Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetG_lag1.csv")[-1]
Affect_Passive$ParticipantNumber <- recode(Affect_Passive$ParticipantNumber,"TSCS2_2JV3YK" = 1,"TSCS2_3HJ9LJ" = 2, "TSCS2_4KUR2Z" = 3,"TSCS2_4NXMUS" = 4,
                                           "TSCS2_5MNS2N" = 5,"TSCS2_5W3RE9" = 6,"TSCS2_6B972L" = 7, "TSCS2_6BBRGP" = 8,"TSCS2_7NJKV7" = 9,"TSCS2_857ER8" = 10,"TSCS2_9C99HU" = 11)

if(predictors == "onlypassive"){
  Affect_Passive <- Affect_Passive %>% select(Date,timescale_beforeESM,ParticipantNumber,Lag,MissingData,na_mean,pa_mean,APP_USAGE_min,COMMUNICATION_min,
                                              SOCIAL_min,com.whatsapp_min,APPS_OPENED_number,UNIQUE_STAYPOINTS_number,Cluster_HOME_min,TIME_STATIONARY_min,AVERAGE_DISTANCE_HOME_km,
                                              CALL_TOTAL_min,CALL_incoming_min,CALL_outgoing_min,TOTAL_MACHASHES_number,UNIQUE_MACHASHES_number,SCREEN_onLocked_number,SCREEN_onUnlocked_number
  )
}else if(predictors == "onlyESM"){
  Affect_Passive <- Affect_Passive %>% select(Date,timescale_beforeESM,ParticipantNumber,Lag,MissingData,na_mean,pa_mean,
                                              Total_Minutes_f2f,Total_Minutes_calling,Total_Minutes_texting,Total_Minutes_striving_behavior,Total_Minutes_mundane_maintenance_behavior,
                                              Total_Minutes_work_school_talk,Total_Minutes_Negative_content,Last_Interaction_enjoy,Last_Interaction_other_enjoy,Last_Interaction_meaningful,
                                              Last_Interaction_myself,Last_Interaction_costenergy,Last_Interaction_giveenergy,Last_Interaction_happy,Last_Interaction_time
  )
}else if(predictors == "onlyEgo"){
  Affect_Passive <- Affect_Passive %>% select(Date,timescale_beforeESM,ParticipantNumber,Lag,MissingData,na_mean,pa_mean,
                                              Total_Minutes_partner,Total_Minutes_friend,Total_Minutes_family,Total_Minutes_fellowstud_colleague,Total_Minutes_flatmate,Total_Minutes_Teacher_Superior,
                                              Total_Minutes_closeness,Total_Minutes_discusspersonal,Total_Minutes_emotionalsupport,Total_Minutes_practicalsupport,Last_Interaction_partner_closeness,
                                              Last_Interaction_partner_energygive,Last_Interaction_partner_energycost,Last_Interaction_partner_myself,Last_Interaction_partner_f2f_frequency,
                                              Last_Interaction_partner_call_frequency,Last_Interaction_partner_text_frequency
  )
}

              
set.seed(12361488)

Participant1 = Affect_Passive[Affect_Passive["ParticipantNumber"] == PNumbers & Affect_Passive["timescale_beforeESM"] == index$Aggregation,  ]
Features <- Participant1 %>% select(-Date, -Lag,-MissingData, -timescale_beforeESM, -ParticipantNumber) %>% colnames()

Features <- Features[-which(Features == "na_mean")]
Features <- Features[-which(Features == "pa_mean")]


### An observation will be excluded if it only contains zeros values or missing values
mood <- Participant1[,outcome]
Participant1 <- Participant1[,which(colnames(Participant1) %in% Features)] # We remove all other variables
Participant1$mood <- mood + 1

source("BlockedCV.R")
timeSlices <- SlidingWindow_CV(Participant1,index$Window,3)   
trainSlices <- timeSlices[[1]]
testSlices <- timeSlices[[2]]


fitControl <- trainControl(method="timeslice", horizon = 1, initialWindow = index$Window-1) # leave last one out cross validation

##### PARALLEL COMPUTING #######
cl <- makePSOCKcluster(detectCores()-1)
registerDoParallel(cl)

#### Define function to run in parallel

#### START FEATURE IMPORTANCE CALCULATION ######

run_shapley <- function(i) {
  set.seed(12361488)
  Shapley_values <- list()
  
  counter <- 0

  ### Create prediction forumla and run the model
  formula2 <- as.formula(paste("mood ~", paste(colnames(Participant1[,colnames(Participant1) %in% Features]), collapse = "+")))
  model <- caret::train(formula2, data=Participant1[trainSlices[[i]],],trControl = fitControl, method="rf",metric = "MAE", preProcess = c("knnImpute","nzv"), 
                                   na.action = na.pass, tuneGrid=expand.grid(mtry = c(3:18)))
  
  
  ######## Calculate the feature importance for each datapoint #########
  for(datapoint in 1:length(trainSlices[[i]])){ 

    ### Create dataframes for features and response
    #x.intrest = Participant1[testSlices[[i]],colnames(Participant1) %in% Features]
    x.intrest = Participant1[trainSlices[[i]],colnames(Participant1) %in% Features] # calculate SHAPLEY values in the training set
    
    features <- Participant1[trainSlices[[i]],colnames(Participant1) %in% Features]
    response <-  as.data.frame(Participant1$mood[trainSlices[[i]]]) # Create a vector with the actual responses
    
    # Only used for a participant, the feature is not included anyway in the model but otherwise I got an error since the feature only had missing values 
    #zero_index <- names(which(colSums(is.na(features)) == nrow(features))) # what is zero
    #x.intrest[,colnames(x.intrest) %in% zero_index] <- 999999
    #features[,colnames(features) %in% zero_index] <- 999999
    
    # The following model will use the data in the TRAINING set to shuffle the predictors
    predictor <- Predictor$new(data = features,
                               y = response,
                               predict.fun = function(newdata) predict(model,newdata, na.action = na.pass)) # we add our own predict function so that missing data gets imputed

    
    shap <- Shapley$new(predictor = predictor, x.interest = x.intrest[datapoint,], sample.size = samplesize)
    
    variable_value <- data.frame(t(x.intrest[datapoint,]))
    colnames(variable_value) <- 'variable_value'
    
    counter <- counter + 1  
    Shapley_values[[counter]] = data.frame(ID = rep(testSlices[[i]][datapoint],nrow(shap$results[,1:3])), variable = shap$results[,1], value =  shap$results[,2],
                          variable_value = variable_value, index = trainSlices[[i]][datapoint])
    
  }
  results <- as.data.frame(do.call("rbind",Shapley_values))
  return(results)
}


trainSlices_n <- c(1:length(trainSlices))

Shapley_values <- foreach(i = trainSlices_n, .combine = rbind, .verbose = TRUE) %dopar% {
  library("iml") # Feature Importance
  library("dplyr")
  library("progress")
  set.seed(12361488)
  
  res <- run_shapley(i)
  
  #Return results
  res
}

stopCluster(cl)


###### Prepare Data for plot ######
# add column mean_value
Shapley_values <- Shapley_values %>% dplyr::group_by(variable) %>% mutate(mean_value = mean(abs(value))) %>% 
  ungroup()

# add standardized feature value

std1 <- function(x){
  return ((x - min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

Shapley_values <- Shapley_values %>% group_by(variable) %>% mutate(stdfvalue = std1(variable_value)) %>% 
  ungroup()

# Safe results for each participant

write.csv(Shapley_values,file = paste("Results/Shapley1000_train","_Participant",PNumbers,"_best_",outcome,"_",predictors,".csv",sep = ""))


############# Plot Results #################
############################################

### Positive Affect: 1 (all),2 (all),3,4,5,6(all),7(all),8(all),9 ####
predictors <- "all"
Shapley_pa <- list()
counter <- 0
for(PNumbers in c(1,2,4,5,6,7,8,9)){
counter <- counter + 1
Shapley_values <- read.csv(paste("Results/Shapley1000_train","_Participant",PNumbers,"_best_pa_mean","_",predictors,".csv",sep = ""))
Shapley_values$Pnumber <- PNumbers
Shapley_pa[[counter]] <- Shapley_values
}

Shapley_pa <- as.data.frame(do.call("rbind",Shapley_pa)) 

Shapley_pa <- Shapley_pa  %>% group_by(Pnumber,variable) %>% summarise(mean_value = mean(mean_value)) # Calculate the mean of each variable per participant
Shapley_pa <- Shapley_pa  %>% group_by(variable) %>% mutate(mean_value_var = mean(mean_value)) %>% ungroup() # calculate the mean of each variable across participants (used to sort the plot)

# The following code is used to sort the variables in the plot
Shapley_pa$measure <- NA

Shapley_pa$measure[Shapley_pa$variable %in% c(
  "APP_USAGE_min","COMMUNICATION_min","SOCIAL_min","com.whatsapp_min","APPS_OPENED_number","UNIQUE_STAYPOINTS_number","Cluster_HOME_min","TIME_STATIONARY_min","AVERAGE_DISTANCE_HOME_km",
  "CALL_TOTAL_min","CALL_incoming_min","CALL_outgoing_min","TOTAL_MACHASHES_number","UNIQUE_MACHASHES_number","SCREEN_onLocked_number","SCREEN_onUnlocked_number")] <- "Digital Phenotyping"

Shapley_pa$measure[Shapley_pa$variable %in% c(
  "Total_Minutes_f2f","Total_Minutes_calling","Total_Minutes_texting","Total_Minutes_striving_behavior","Total_Minutes_mundane_maintenance_behavior",
  "Total_Minutes_work_school_talk","Total_Minutes_Negative_content","Last_Interaction_enjoy","Last_Interaction_other_enjoy","Last_Interaction_meaningful",
  "Last_Interaction_myself","Last_Interaction_costenergy","Last_Interaction_giveenergy","Last_Interaction_happy","Last_Interaction_time")] <- "ESM"

Shapley_pa$measure[Shapley_pa$variable %in% c("Total_Minutes_partner","Total_Minutes_friend","Total_Minutes_family","Total_Minutes_fellowstud_colleague","Total_Minutes_flatmate","Total_Minutes_Teacher_Superior",
  "Total_Minutes_closeness","Total_Minutes_discusspersonal","Total_Minutes_emotionalsupport","Total_Minutes_practicalsupport","Last_Interaction_partner_closeness",
  "Last_Interaction_partner_energygive","Last_Interaction_partner_energycost","Last_Interaction_partner_myself","Last_Interaction_partner_f2f_frequency",
  "Last_Interaction_partner_call_frequency","Last_Interaction_partner_text_frequency")] <- "Egocentric Network"



#### Negative Affect: 4, 5, 6 (all), 7 (all), 8, 11 ####
Shapley_na <- list()
counter <- 0
for(PNumbers in c(4,5,6,7,8,11)){
  counter <- counter + 1
  Shapley_values <- read.csv(paste("Results/Shapley1000_train","_Participant",PNumbers,"_best_na_mean","_",predictors,".csv",sep = ""))
  Shapley_values$Pnumber <- PNumbers
  Shapley_na[[counter]] <- Shapley_values
}

Shapley_na <- as.data.frame(do.call("rbind",Shapley_na)) 

Shapley_na <- Shapley_na  %>% group_by(Pnumber,variable) %>% summarise(mean_value = mean(mean_value))
Shapley_na <- Shapley_na  %>% group_by(variable) %>% mutate(mean_value_var = mean(mean_value)) %>% ungroup()


# The following code is used to sort the variables in the plot
Shapley_na$measure <- NA

Shapley_na$measure[Shapley_na$variable %in% c(
  "APP_USAGE_min","COMMUNICATION_min","SOCIAL_min","com.whatsapp_min","APPS_OPENED_number","UNIQUE_STAYPOINTS_number","Cluster_HOME_min","TIME_STATIONARY_min","AVERAGE_DISTANCE_HOME_km",
  "CALL_TOTAL_min","CALL_incoming_min","CALL_outgoing_min","TOTAL_MACHASHES_number","UNIQUE_MACHASHES_number","SCREEN_onLocked_number","SCREEN_onUnlocked_number")] <- "Digital Phenotyping"

Shapley_na$measure[Shapley_na$variable %in% c(
  "Total_Minutes_f2f","Total_Minutes_calling","Total_Minutes_texting","Total_Minutes_striving_behavior","Total_Minutes_mundane_maintenance_behavior",
  "Total_Minutes_work_school_talk","Total_Minutes_Negative_content","Last_Interaction_enjoy","Last_Interaction_other_enjoy","Last_Interaction_meaningful",
  "Last_Interaction_myself","Last_Interaction_costenergy","Last_Interaction_giveenergy","Last_Interaction_happy","Last_Interaction_time")] <- "ESM"

Shapley_na$measure[Shapley_na$variable %in% c("Total_Minutes_partner","Total_Minutes_friend","Total_Minutes_family","Total_Minutes_fellowstud_colleague","Total_Minutes_flatmate","Total_Minutes_Teacher_Superior",
                                              "Total_Minutes_closeness","Total_Minutes_discusspersonal","Total_Minutes_emotionalsupport","Total_Minutes_practicalsupport","Last_Interaction_partner_closeness",
                                              "Last_Interaction_partner_energygive","Last_Interaction_partner_energycost","Last_Interaction_partner_myself","Last_Interaction_partner_f2f_frequency",
                                              "Last_Interaction_partner_call_frequency","Last_Interaction_partner_text_frequency")] <- "Egocentric Network"


###### Combine both to one dataframe #######

Shapley_pa$affect <- "PA"
Shapley_na$affect <- "NA"

Shapley_both <- rbind(Shapley_pa,Shapley_na)
Shapley_both <- Shapley_both  %>% group_by(variable) %>% mutate(mean_value_var = mean(mean_value)) %>% ungroup() # Calculate the overall mean of PA and NA

Shapley_both$affect <- factor(Shapley_both$affect, ordered = TRUE,levels = c("PA","NA"))

S_Both <- ggplot(Shapley_both, aes(x = as.factor(affect),y = fct_reorder(variable,mean_value_var), fill = mean_value)) + 
  geom_tile() +
  facet_grid(measure~as.factor(Pnumber), scales="free_y")  +
  scale_fill_gradient(low="#FFCC33", high="#6600CC", name = "Importance") +
  theme_minimal() +
  ylab("Variable") +
  xlab("Positive (PA) and Negative (NA) Affect for each Participant")


ggsave("Shapley_both.png",
       plot = S_Both,
       width = 22,
       height = 15,
       units = c("cm"),
       dpi = 300,
       limitsize = TRUE,
       bg = NULL)


######### Calculations
# Extract highest value
Shapley_both[which(Shapley_both$mean_value == max(Shapley_both$mean_value)),]

# Extract zero values
zero <- Shapley_both[which(Shapley_both$mean_value == 0),]
