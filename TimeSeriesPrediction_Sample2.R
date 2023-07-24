
#################################################
############### PREDICTION MODEL ################
#################################################

library('dplyr')
library("tidyr")
library("caret")
library("doParallel")
library("Metrics")
library("ggplot2")
library("TSrepr") #mape

set.seed(12361488)

########## Choose outcome & Data set (to save result) ########

# Outcome: negative affect, positive affect
# Predictors: all, only digital phenotyping (onlypassive), only ego (onlyEgo), only esm (onlyESM)
# Lag: 1 hour, 30 minutes
# Dataset: DataL, DataG

# Aggregation: 3, 6, 24 hours
# Rolling window size: 15, 20, 30


lag <- "lag1" # lag05 (Robustness check, 30 minutes prediction lag)
dataset <- "DataG" # DataL (Robustness check, only change one interaction)
predictors <- "onlypassive" # "all", onlypassive", "onlyESM", "onlyEgo" (select included set of predictor variables)

outcome <- "pa_mean" # na_mean
step <- 3 

############ Load and Prepare Data ###############
if(dataset == "DataL" & lag == "lag1") Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetL_lag1.csv")[-1]
if(dataset == "DataG" & lag == "lag1") Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetG_lag1.csv")[-1]
if(dataset == "DataL" & lag == "lag05") Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetL_lag05.csv")[-1]
if(dataset == "DataG" & lag == "lag05") Affect_Passive <- read.csv("/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetG_lag05.csv")[-1]

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


#################### START PREDICTIONS ###################

Overall_results_allparticipants <- list() # Saves the prediction performance
Overall_pred_allparticipants <- list() # Saves all predicted and observed values

# Start parallel computing 
cl <- makePSOCKcluster(detectCores()-1)

registerDoParallel(cl)

PNumbers <- c("TSCS2_2JV3YK","TSCS2_857ER8","TSCS2_3HJ9LJ","TSCS2_6B972L","TSCS2_9C99HU","TSCS2_4NXMUS","TSCS2_4KUR2Z","TSCS2_5MNS2N","TSCS2_6BBRGP",
              "TSCS2_7NJKV7","TSCS2_5W3RE9")

for(h in 1:length(PNumbers)){
  ##### Create empty lists
  Overall_results <- list() # Saves the prediction performance
  Overall_pred <- list() # Saves all predicted and observed values
  counter <- 1
  
  # **** We loop through the different levels of aggregation (n) ****
  for(n in c(3,6,24)){
    
    # **** We loop through the different moving window sizes (k) ****
  
    #### Rolling window size: 15, 20, 30  
    for(k in c(15,20,30)){
      
      Participant1 = Affect_Passive[Affect_Passive["ParticipantNumber"] == PNumbers[h] & Affect_Passive["timescale_beforeESM"] == n,  ]
      
      Features <- Participant1 %>% select(-Date, -Lag,-MissingData, -timescale_beforeESM, -ParticipantNumber) %>% colnames()
  
      Features <- Features[-which(Features == "na_mean")]
      Features <- Features[-which(Features == "pa_mean")]
      
      set.seed(12361488)
      
      mood <- Participant1[,outcome]
      Participant1 <- Participant1[,which(colnames(Participant1) %in% Features)] # We remove all other variables
      Participant1$mood <- mood + 1 # we transform it to 1-11 instead of having 0-11 (to be able to calculate MAPE)
  
      source("BlockedCV.R")
      timeSlices <- SlidingWindow_CV(Participant1,k,step)   
      trainSlices <- timeSlices[[1]]
      testSlices <- timeSlices[[2]]
      
      lengthtest <- length(unlist(testSlices))
      
      Results_pred <- data.frame(index = unlist(testSlices), pred = rep(NA,lengthtest), true = rep(NA,lengthtest))
      
      fitControl <- trainControl(method="timeslice", horizon = 1, initialWindow = k-1)
  
      
      for(i in 1:length(trainSlices)){
        
        
      ### Create prediction model
      formula2 <- as.formula(paste("mood ~", paste(colnames(Participant1[,colnames(Participant1) %in% Features]), collapse = "+")))
      
      
      try <- try(model <- caret::train(formula2, data=Participant1[trainSlices[[i]],],trControl = fitControl, method="rf",metric = "MAE", preProcess = c("knnImpute","nzv"), 
                      na.action = na.pass, tuneGrid=expand.grid(mtry = c(3:18)))) #mtry: number of randomly selected predictors
        
      # Next we store the results from the prediction model by making predictions for our test set. The predict() function automatically uses the "best" model (based on leave last one out from the trains set)
      dataindex <- which(Results_pred$index == testSlices[[i]])
      Results_pred$pred[dataindex] <- NA
      
      if(any(class(try) != "try-error")){
      try(Results_pred$pred[dataindex] <- predict(model,Participant1[testSlices[[i]],], na.action = na.pass)) #final model automatically used with predict (na.pass, because missing values are getting imputed)
      Results_pred$true[dataindex] <- Participant1$mood[testSlices[[i]]] # safe the predictions
      }    
      
      Results_pred$w <- k
      Results_pred$aggregation <- n
      Results_pred$Participant <- PNumbers[h]
  
      }
      
      # Performance rf
      rss <- sum((Results_pred$pred - Results_pred$true) ^ 2,na.rm = TRUE)  # residual sum of squares
      tss <- sum((Results_pred$true - mean(Results_pred$true, na.rm = TRUE))^ 2,na.rm = TRUE)  # total sum of squares
      rsq <- 1 - rss/tss
      if(rss/tss == 0) rsq <- NA
      
      complete <- complete.cases(Results_pred$true,Results_pred$pred)
      
      mape <- mape(Results_pred$true[complete],Results_pred$pred[complete])
      smape <- smape(Results_pred$true[complete],Results_pred$pred[complete])
      rae <- rae(Results_pred$true[complete],Results_pred$pred[complete])
      mae <- mae(Results_pred$true[complete],Results_pred$pred[complete])
      cor <- cor(Results_pred$true,Results_pred$pred, use = "pairwise.complete.obs")
      
      Results <- cbind(rsq,cor,mae,mape,rae,smape,k,n,PNumbers[h])
      
      print(Results)
      
      Overall_results[[counter]] <- Results
      Overall_pred[[counter]] <- Results_pred
      counter <- counter + 1
    }
  }
  
  Overall_results_allparticipants[[h]] <- as.data.frame(do.call("rbind",Overall_results)) 
  Overall_pred_allparticipants[[h]] <- as.data.frame(do.call("rbind",Overall_pred)) #Combine lists into one dataframe
  
}

##### Save results

Overall_results_allparticipants <- as.data.frame(do.call("rbind",Overall_results_allparticipants)) # Save performance
Overall_pred_allparticipants <- as.data.frame(do.call("rbind",Overall_pred_allparticipants)) # Save predictions
write.csv(Overall_results_allparticipants,file = paste("Results/OverallResults","_step",step,"_",dataset,"_",lag,"_",outcome,"_",predictors,".csv",sep = ""))
write.csv(Overall_pred_allparticipants,file = paste("Results/OverallPred","_step",step,"_",dataset,"_",lag,"_",outcome, "_",predictors,".csv",sep = ""))



#######################################################
############### USE MEAN AS PREDICTION ################
#######################################################

Overall_results_allparticipants_mean <- list() # Stores the prediction performance
Overall_pred_allparticipants_mean <- list() # create a list to store the predicted and obseverd values

for(h in 1:length(PNumbers)){
  
  counter <- 1
  
  Overall_results_mean <- list()
  Overall_pred_mean <- list()
  
  Participant1 = Affect_Passive[Affect_Passive["ParticipantNumber"] == PNumbers[h] & Affect_Passive["timescale_beforeESM"] == 6,  ] # Here the level of aggregation is not important (because we only use positive affect). Thus, we could have also used a different level than 6h
  
  
  mood <- Participant1[,outcome]
  Participant1$mood <- mood + 1
  
  for(k in c(15,20,30)){

    source("BlockedCV.R")
    timeSlices <- SlidingWindow_CV(Participant1,k,step)   
    trainSlices <- timeSlices[[1]]
    testSlices <- timeSlices[[2]]
    
    
    lengthtest <- length(unlist(testSlices))
    
    Results_pred_mean <- data.frame(index = unlist(testSlices),pred = rep(NA,lengthtest),true = rep(NA,lengthtest))
  
    for(i in 1:length(trainSlices)){
      dataindex <- which(Results_pred_mean$index == testSlices[[i]])
      
      Results_pred_mean$pred[dataindex] <- mean(Participant1$mood[trainSlices[[i]]], na.rm = TRUE) #we use the rolling mean as prediction
      Results_pred_mean$true[dataindex] <- Participant1$mood[testSlices[[i]]]
      Results_pred_mean$w <- k
      Results_pred_mean$Participant <- PNumbers[h]
      
      
    }
    
    # Performance
    rss <- sum((Results_pred_mean$pred - Results_pred_mean$true) ^ 2,na.rm = TRUE)  # residual sum of squares
    tss <- sum((Results_pred_mean$true - mean(Results_pred_mean$true, na.rm = TRUE))^ 2,na.rm = TRUE)  # total sum of squares
    rsq <- 1 - rss/tss
    
    complete <- complete.cases(Results_pred_mean$true,Results_pred_mean$pred)
    
    mape <- mape(Results_pred_mean$true[complete],Results_pred_mean$pred[complete])
    smape <- smape(Results_pred_mean$true[complete],Results_pred_mean$pred[complete])
    rae <- rae(Results_pred_mean$true[complete],Results_pred_mean$pred[complete])
    mae <- mae(Results_pred_mean$true[complete],Results_pred_mean$pred[complete])
    cor <- cor(Results_pred_mean$true,Results_pred_mean$pred, use = "pairwise.complete.obs")
    
  
    Overall_results_mean[[counter]] <- cbind(rsq,cor,mae,mape,rae,smape,k,n,PNumbers[h])
    Overall_pred_mean[[counter]] <- Results_pred_mean
    counter <- counter + 1
  }
  
  Overall_results_allparticipants_mean[[h]] <- as.data.frame(do.call("rbind",Overall_results_mean)) 
  Overall_pred_allparticipants_mean[[h]] <- as.data.frame(do.call("rbind",Overall_pred_mean)) #Combine lists into one dataframe
}

##### Save results

Overall_results_allparticipants_mean <- as.data.frame(do.call("rbind",Overall_results_allparticipants_mean))
Overall_pred_allparticipants_mean <- as.data.frame(do.call("rbind",Overall_pred_allparticipants_mean))
write.csv(Overall_results_allparticipants_mean,file = paste("Results/OverallResultsMEAN","_step",step,"_",outcome,".csv",sep = ""))
write.csv(Overall_pred_allparticipants_mean,file = paste("Results/OverallPredMEAN","_step",step,"_",outcome,".csv",sep = ""))

