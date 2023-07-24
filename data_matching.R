library(dplyr)
library(tidyverse)
library(tidyr)


################## ESM & Egocentic Network Data ################
################################################################


########## Load data & Preperation #########
Ego <- read.csv("/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/EgocentricNetwork.csv")[-1]
ESM_data <- read.csv('/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/ESMCleaned.csv')[-1]

# Change colnames 
colnames(ESM_data)[colnames(ESM_data) == 'index_time'] <- 'Date'
ESM_data$ParticipantNumber <- ESM_data$BehappID 

Interactions_ESM <- ESM_data[ESM_data$questionListName == "Interaction Assessment - Version 2" | ESM_data$questionListName == "Signal Contingent Interaction",]

#Create affect mean scores
ESM_data['pa_mean'] <- ESM_data %>% select(c("pa_happy_sliderNegPos","pa_energy_sliderNegPos","pa_relax_sliderNegPos")) %>% rowMeans()
ESM_data['na_mean'] <-  ESM_data %>% select(c("na_sad_sliderNegPos","na_anx_sliderNegPos","na_stress_sliderNegPos","na_irritate_sliderNegPos")) %>% rowMeans()


# Remove backlog questionnaires (Otherwise the affect measure would be twice included)
ESM_data <- ESM_data[ESM_data$questionListName != "Interaction Assessment - Version 2",]
ESM_data <- ESM_data[ESM_data$questionListName != "Signal Contingent Interaction",]


#### Select only participants that downloaded Behapp
BEHAPPID2 = c("TSCS2_2JV3YK","TSCS2_857ER8","TSCS2_3HJ9LJ","TSCS2_6B972L","TSCS2_9C99HU","TSCS2_4NXMUS","TSCS2_4KUR2Z","TSCS2_5MNS2N","TSCS2_6BBRGP",
              "TSCS2_7NJKV7","TSCS2_5W3RE9")

ESM_data = ESM_data[ESM_data$ParticipantNumber %in% BEHAPPID2,]
Interactions_ESM = Interactions_ESM[Interactions_ESM$ParticipantNumber %in% BEHAPPID2,]

##### Transform times & split dataset
Scheduled_ESM <- ESM_data


Scheduled_ESM$Date <- as.POSIXct(Scheduled_ESM$Date ) 
Interactions_ESM$Date <- as.POSIXct(Interactions_ESM$Date) 

Interactions_ESM$timeStampStart <- as.POSIXct(Interactions_ESM$timeStampStart, origin = "1970-01-01", tz = "GMT") 
Interactions_ESM$timeStampStop <- as.POSIXct(Interactions_ESM$timeStampStop, origin = "1970-01-01", tz = "GMT") 

# Change format to CET/CEST
Interactions_ESM$timeStampStart <- lubridate::force_tz(Interactions_ESM$timeStampStart, tzone = "CET")
Interactions_ESM$timeStampStop <- lubridate::force_tz(Interactions_ESM$timeStampStop, tzone = "CET")

########### Add start and end time of the interaction #############
library(lubridate)
# The start and end time from an interaction is recorded as seconds from 00:00. In the following lines of code we will add the correct date
# to the time. The challenge is to make sure that interactions that are going over two dates (e.g., 22:50 - 1:10) have the correct date.
# We will need the correct date later to create features.

# First we will create a variable that will show the origin of the time stamp (which shows only seconds from 00:00)
Interactions_ESM$Date_origin <-  lubridate::date(Interactions_ESM$Date) # Date origin

# We will transform the seconds into a date and time stamp.
# The start time will have the origin of when the ESM questionnaire was filled out
# The end time will have one day later, otherwise we will not be able to calculate the duration of interactions that ranged over two days.
Interactions_ESM$interact_time_end_time <- as.POSIXct(Interactions_ESM$interact_time_end_time, origin=Interactions_ESM$Date_origin + 1, tz="UTC")
Interactions_ESM$interact_time_start_time <- as.POSIXct(Interactions_ESM$interact_time_start_time, origin=Interactions_ESM$Date_origin, tz="UTC")


# We will calculate the duration here.
Interactions_ESM$Duration_Interaction <- difftime(Interactions_ESM$interact_time_end_time, Interactions_ESM$interact_time_start_time,  units="mins")

# We now create an index for those interactions that took only place on one day. We choose 1335 instead of 1440 to take some of those interactions into account 
# that put the start before the end date. This threshhold was chosen after manually inspecting the data.
index <- which(Interactions_ESM$Duration_Interaction > 1335) #all interactions that were only during one day.

# if the duration is more than 24hours, we will subtract 24 hours. The duration will only be NOT more than 24 hours if the interaction took place over two days.
# Or if start and endtime is mismatched
Interactions_ESM$Duration_Interaction[which(Interactions_ESM$Duration_Interaction > 1335)] <- Interactions_ESM$Duration_Interaction[which(Interactions_ESM$Duration_Interaction > 1335)] - 1440



### LOOK AT START TIME AND COMPARE IT TO FILLED IN
# We will adjust the end time date for those interaction that took not place over two days
Interactions_ESM$interact_time_end_time <- as.POSIXct(Interactions_ESM$interact_time_end_time,tz="UTC")
Interactions_ESM$interact_time_end_time[index] <- Interactions_ESM$interact_time_end_time[index] - lubridate::period(day = 1)

# Change format to CET/CEST
Interactions_ESM$interact_time_start_time <- lubridate::force_tz(Interactions_ESM$interact_time_start_time, tzone = "CET",)
Interactions_ESM$interact_time_end_time <- lubridate::force_tz(Interactions_ESM$interact_time_end_time, tzone = "CET",)

# Index for interaction that were longer than a day we have to subtract both dates by one (because we took the date when the questionnaire was filled out as origin for the start
# but it is actually the origin when the questionnaire ended)
index <- which(difftime(date(Interactions_ESM$interact_time_end_time),date(Interactions_ESM$interact_time_start_time)) > 0)

Interactions_ESM$interact_time_end_time[index] <- Interactions_ESM$interact_time_end_time[index] - lubridate::period(day = 1)
Interactions_ESM$interact_time_start_time[index] <- Interactions_ESM$interact_time_start_time[index] - lubridate::period(day = 1)

# As a next step we have to adjust the dates for those interactions that took place before 00:00 but were recorded after 00:00
# E.g., the interaction took place from 19:00 - 22:40 and was recorded the next day at 1:00
# 60 mins is the threshold we chose
index <- which(round(difftime(Interactions_ESM$Date,Interactions_ESM$interact_time_end_time, unit = "mins")) < -60)
Interactions_ESM$interact_time_end_time[index] <- Interactions_ESM$interact_time_end_time[index] - lubridate::period(day = 1)
Interactions_ESM$interact_time_start_time[index] <- Interactions_ESM$interact_time_start_time[index] - lubridate::period(day = 1)

# The following observations probably mismatched start and end time. Thus, this was adapted here
index <- which(Interactions_ESM$interact_time_start_time > Interactions_ESM$interact_time_end_time)
Interactions_ESM$Duration_Interaction[index] <- abs(Interactions_ESM$Duration_Interaction[index])

Interactions_ESM$interact_time_end_time[index] <- Interactions_ESM$interact_time_start_time[index] + Interactions_ESM$Duration_Interaction[index]


########## Add variable that shows content of interaction ##############
Interactions_ESM$interact_cont_mc_multipleChoice_string

content <- strsplit(Interactions_ESM$interact_cont_mc_multipleChoice_string, split = ",")

striving_behavior <- c("Expressing love or affection","Joking around","Meaningful conversation","Catching up")
mundane_maintenance_behavior <- c("Gossip","Task talk","Small talk","Making plans")
work_school_talk <- c("Work or school talk")
Negative_content <- c("Complaining or venting","Conflict or disagreement")

for(i in 1:nrow(Interactions_ESM)){
  ifelse(content[i] %in% striving_behavior, Interactions_ESM$striving_behavior[i] <- 1,  Interactions_ESM$striving_behaviori[i] <- 0)
  ifelse(content[i] %in% mundane_maintenance_behavior, Interactions_ESM$mundane_maintenance_behavior[i] <- 1,  Interactions_ESM$mundane_maintenance_behavior[i] <- 0)
  ifelse(content[i] %in% work_school_talk, Interactions_ESM$work_school_talk[i] <- 1,  Interactions_ESM$work_school_talk[i] <- 0)
  ifelse(content[i] %in% Negative_content, Interactions_ESM$Negative_content[i] <- 1,  Interactions_ESM$Negative_content[i] <- 0)
}

###### Create different datasets#######

######## Dataset 1 (L): #########
### Change observations manually that seems to be off
Interactions_ESM$index_number <- rownames(Interactions_ESM)

# Interaction happened before study start, thus I delete the interaction
index <- which(Interactions_ESM$index_number == 1)
Interactions_ESM <- Interactions_ESM[-index,]

# Probably wrong endtime (zero minutes)
index <- which(Interactions_ESM$index_number == 2074)
Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-22 02:30:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")

# # ########  Dataset 2 (G): ########
# ### Change observations manually that seems to be off
# Interactions_ESM$index_number <- rownames(Interactions_ESM)
# 
# # Interaction happened before study start, thus I delete the interaction
# index <- which(Interactions_ESM$index_number == 1)
# Interactions_ESM <- Interactions_ESM[-index,]
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 181)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-10-20 15:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-20 17:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 1208)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-11-04 17:12:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-11-04 19:50:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 1817)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-11-13 09:10:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-11-13 10:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 2065)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-10-21 20:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-21 21:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 2074)
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-22 02:30:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Date got mixed up
# index <- which(Interactions_ESM$index_number == 2102)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-10-22 23:27:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-23 00:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Adjust date and endtime
# index <- which(Interactions_ESM$index_number == 2364)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-11-01 20:15:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-11-01 21:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")


# ########  Dataset 3 (A, not using): ######## 
# 
# ### Change observations manually that seems to be off
# Interactions_ESM$index_number <- rownames(Interactions_ESM)
# 
# # Interaction happened before study start, thus I delete the interaction
# index <- which(Interactions_ESM$index_number == 1)
# Interactions_ESM <- Interactions_ESM[-index,]
# 
# # Start time was probably missed
# index <- which(Interactions_ESM$index_number == 38)
# Interactions_ESM$interact_time_start_time[index] <- Interactions_ESM$interact_time_end_time[index]
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-18 16:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 181)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-10-20 15:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-20 17:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 1208)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-11-04 17:12:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-11-04 19:50:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 1817)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-11-13 09:10:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-11-13 10:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 2065)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-10-21 20:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-21 21:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Probably wrong endtime
# index <- which(Interactions_ESM$index_number == 2074)
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-22 02:30:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Date got mixed up
# index <- which(Interactions_ESM$index_number == 2102)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-10-22 23:27:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-23 00:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Wrong date after data cleaning
# index <- which(Interactions_ESM$index_number == 2151)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-10-24 15:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-10-24 18:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# # Adjust Date, survey was open for quite long
# index <- which(Interactions_ESM$index_number == 2358)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-11-01 15:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-11-01 18:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 
# #Adjust date and endtime
# index <- which(Interactions_ESM$index_number == 2364)
# Interactions_ESM$interact_time_start_time[index] <- as.POSIXct("2022-11-01 20:15:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# Interactions_ESM$interact_time_end_time[index] <- as.POSIXct("2022-11-01 21:00:00",tz="CET",format="%Y-%m-%d %H:%M:%OS")
# 

#Clean up
rm(BEHAPPID2, i, mundane_maintenance_behavior,Negative_content,striving_behavior,work_school_talk,content, ESM_data)
write.csv(Interactions_ESM,"/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/Interactions_ESM.csv")

####################### Aggregate Variables ######################

source("/Users/annalangener/projects/CombiningMethods_MoodPredictions/CreateSocialVariable.R")

# CHANGE LAG
Interactions_Variable <- list() #List to save results
counter <- 0
for(m in 1:11){ # Because we have 11 participants
  for(l in c(3,6,24)){
    counter <- counter + 1
    Interactions_Variable[[counter]] <- CreateSocialVariable(Interactions_ESM, Scheduled_ESM, Ego, rule_int = l,number = m, lag = 60)
    print(paste("Participant:", m, "| Level of Aggregation:", l))
  }
}
Interactions_Variable <- as.data.frame(do.call("rbind",Interactions_Variable))


############## Match with Passive Data ###################### 
#############################################################

#### Passive Data #####
Passive <- read.csv("/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/PassiveMeasures.csv")[-1]

# select variables of intrest
Passive <- Passive %>% select(Date,timescale_beforeESM,Lag, ParticipantNumber,MissingData,
                              APP_USAGE_min,COMMUNICATION_min,SOCIAL_min, com.whatsapp_min,APPS_OPENED_number,UNIQUE_STAYPOINTS_number,
                              Cluster_HOME_min,TIME_STATIONARY_min,AVERAGE_DISTANCE_HOME_km,CALL_TOTAL_min, CALL_incoming_min, CALL_outgoing_min,
                              TOTAL_MACHASHES_number,UNIQUE_MACHASHES_number ,SCREEN_onLocked_number, SCREEN_onUnlocked_number)


Passive_subset <- Passive[Passive$Lag == 1,]
Passive_subset$Date <- as.POSIXct(Passive_subset$Date)

Interactions_Variable$timescale_beforeESM <- as.numeric(str_split_fixed(Interactions_Variable$timescale_beforeESM, pattern = "h", n = 2)[,1])

outcome <- Scheduled_ESM %>% select("na_mean", "pa_mean", "Date", "ParticipantNumber")
OverallDataset <- full_join(Interactions_Variable,Passive_subset, by = c("Date", "timescale_beforeESM", "ParticipantNumber"))

OverallDataset <- left_join(OverallDataset,outcome, by = c("Date", "ParticipantNumber"))

#### Save Datasets ####
#Overall
write.csv(OverallDataset,"/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetL_lag1.csv")
#write.csv(OverallDataset,"/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetL_lag05.csv")

#write.csv(OverallDataset,"/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetG_lag1.csv")
#write.csv(OverallDataset,"/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/DatasetG_lag05.csv")

