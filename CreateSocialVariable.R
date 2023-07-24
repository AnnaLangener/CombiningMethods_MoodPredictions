###### Function to create variables #######

# This function can be used to aggregate the social variables for ESM and egocentric networks

###################
CreateSocialVariable <- function(Interactions_ESM, Scheduled_ESM,Ego, rule_int,number,lag){
  
  Participant <- c("TSCS2_2JV3YK","TSCS2_857ER8","TSCS2_3HJ9LJ","TSCS2_6B972L","TSCS2_9C99HU","TSCS2_4NXMUS","TSCS2_4KUR2Z","TSCS2_5MNS2N","TSCS2_6BBRGP",
                   "TSCS2_7NJKV7","TSCS2_5W3RE9")
  
  Interactions_ESM_single <- Interactions_ESM[Interactions_ESM$ParticipantNumber == Participant[number],]
  Interactions_ESM_single <- Interactions_ESM_single[!is.na(Interactions_ESM_single$Duration_Interaction),] 
  
  Scheduled_ESM_single <- Scheduled_ESM[Scheduled_ESM$ParticipantNumber == Participant[number],]
  
  vec_seq <- Vectorize(seq.POSIXt, vectorize.args = c("from", "to"))
  
  
  Interaction_persecond <- Interactions_ESM_single %>%
    transmute(interact_mode_multipleChoice_string,striving_behavior, mundane_maintenance_behavior,work_school_talk,Negative_content,interact_partner_multipleChoice_string,
              Date = vec_seq(interact_time_start_time, interact_time_end_time, 1)) %>% #transmute() will create a new variable (Date) based on the vec_seq() function (which takes the following arguments: from = recorded_naive, to = end_time, by = 1 (by one second))
    unnest(cols = c(Date)) # displays each value of the time sequence as separate row
  
  #### Create dataframes that we will grow in our loop
  Interactions_Aggregated <- data.frame(Date = Scheduled_ESM_single$Date[1])
  
  
  for (i in 1:nrow(Scheduled_ESM_single)) {
    time = Scheduled_ESM_single$Date[i] # loop through each time point at which an ESM questionnaire was filled out, try out i = 30
    
    ########### ESM VARIABLES (AGGREGATED) ##########
    print(time)
    ####### Duration of Interactions #########
    Interaction_Duration <- Interaction_persecond %>%
      filter(Date > (time - lubridate::period(hours = rule_int, minutes = lag)) & Date < (time - lubridate::minutes(lag))) %>%
      group_by(content = interact_mode_multipleChoice_string) %>%
      summarize(TotalMinutes = n() / 60) %>%
      as.data.frame()
    
    Interaction_content_striving_behavior <- Interaction_persecond %>%
      filter(Date > (time - lubridate::period(hours = rule_int, minutes = lag)) & Date < (time - lubridate::minutes(lag))) %>%
      group_by(striving_behavior) %>%
      summarize(content = 'striving_behavior',TotalMinutes = n() / 60) %>%
      filter(striving_behavior != 0) %>%
      as.data.frame()
    
    Interaction_content_mundane_maintenance_behavior <- Interaction_persecond %>%
      filter(Date > (time - lubridate::period(hours = rule_int, minutes = lag)) & Date < (time - lubridate::minutes(lag))) %>%
      group_by(mundane_maintenance_behavior) %>%
      summarize(content = 'mundane_maintenance_behavior',TotalMinutes = n() / 60) %>%
      filter(mundane_maintenance_behavior != 0)%>%
      as.data.frame()
    
    Interaction_content_work_school_talk <- Interaction_persecond %>%
      filter(Date > (time - lubridate::period(hours = rule_int, minutes = lag)) & Date < (time - lubridate::minutes(lag))) %>%
      group_by(work_school_talk) %>%
      summarize(content = "work_school_talk",TotalMinutes = n() / 60) %>%
      filter(work_school_talk != 0) %>%
      as.data.frame()
    
    Interaction_content_Negative_content <- Interaction_persecond %>%
      filter(Date > (time - lubridate::period(hours = rule_int, minutes = lag)) & Date < (time - lubridate::minutes(lag))) %>%
      group_by(Negative_content) %>%
      summarize(content = "Negative_content",TotalMinutes = n() / 60) %>%
      filter(Negative_content != 0)  %>%
      as.data.frame()
    
    
    Interaction_Duration <- rbind(Interaction_Duration,Interaction_content_Negative_content[-1],Interaction_content_work_school_talk[-1],Interaction_content_mundane_maintenance_behavior[-1],
                                  Interaction_content_striving_behavior[-1])
    
    ####### Last Interaction: How long ago #########
    
    
    
    Last_Interaction <- Interactions_ESM_single[Interactions_ESM_single$interact_time_end_time <  (time - lubridate::minutes(lag)), ]
    Last_Interaction <- Last_Interaction[Last_Interaction$interact_time_end_time == max(Last_Interaction$interact_time_end_time),]
    Last_Interaction_time <- as.numeric(difftime(time,Last_Interaction$interact_time_end_time, units = "mins"))
    
    ############ Egocentric network variables (AGGREGATED) #############
    
    PSN_attr_Overall_single <- Ego[Ego$BehappID == Participant[number],]
    
    Interaction_Partner <- Interaction_persecond %>%
      filter(Date > (time - lubridate::period(hours = rule_int, minutes = lag)) & Date < (time - lubridate::minutes(lag))) %>%
      group_by(interact_partner_multipleChoice_string) %>%
      summarize(TotalMinutes = n() / 60) %>%
      as.data.frame()
    
    partner <- strsplit(Interaction_Partner$interact_partner_multipleChoice_string, split = ",")
    
    data <- data.frame(majority_role = rep(NA,length(partner)),personal_issues = rep(NA,length(partner)),emotional_support = rep(NA,length(partner)),practical_support = rep(NA,length(partner)),closeness_yn = rep(NA,length(partner)))
    
    if(length(partner) >= 1){                                                                             
      for(k in 1:length(partner)){
        names <- partner[[k]]
        
        if(any(PSN_attr_Overall_single$name %in% names == TRUE)){
          roles <- PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% names,] %>% select(name, relationship1, discuss_personal,emotional_support, practical_support, closeness_yn)
          data$majority_role[k] <- names(which.max(table(roles$relationship1)))
          if(data$majority_role[k] == "Other Relative" | data$majority_role[k] == "Parent" | data$majority_role[k] == "Sibling") {
            data$majority_role[k] <- "Family"
          }
          data$closeness_yn[k] <- ifelse(sum(roles$closeness_yn) > 0,1,0)
          data$personal_issues[k] <- ifelse(sum(roles$discuss_personal) > 0,1,0)
          data$emotional_support[k] <- ifelse(sum(roles$emotional_support) > 0,1,0)
          data$practical_support[k] <- ifelse(sum(roles$practical_support) > 0,1,0)
        }else{
          # Person not included in the network
          
          data$majority_role[k] <- "Other"
          data$closeness_yn[k] <- 0
          data$personal_issues[k] <- 0
          data$emotional_support[k] <- 0
          data$practical_support[k] <- 0
        }
      }
    }
      
      Interaction_Partner <- cbind(Interaction_Partner,data)
      
      Interaction_Partner_new <- Interaction_Partner %>% group_by(majority_role) %>% summarize(TotalMinutes = sum(TotalMinutes)) %>% 
        filter(majority_role == "Partner" | majority_role == "Friend" |majority_role == "Family" |majority_role == "Fellow Student/Colleague" |
                 majority_role == "Flatmate" | majority_role == "Teacher/Superior" ) %>% as.data.frame()
      
      personal_issues = Interaction_Partner %>% filter(personal_issues == 1) %>% summarize(majority_role = "personal_issues", TotalMinutes = sum(TotalMinutes))
      Interaction_Partner_new <- rbind(Interaction_Partner_new,personal_issues)
      
      emotional_support = Interaction_Partner %>% filter(emotional_support == 1) %>% summarize(majority_role = "emotional_support", TotalMinutes =  sum(TotalMinutes)) 
      Interaction_Partner_new <- rbind(Interaction_Partner_new,emotional_support)
      
      practical_support = Interaction_Partner %>% filter(practical_support == 1) %>% summarize(majority_role = "practical_support", TotalMinutes =  sum(TotalMinutes)) 
      Interaction_Partner_new <- rbind(Interaction_Partner_new,practical_support)
      
      closeness_yn = Interaction_Partner %>% filter(closeness_yn == 1) %>% summarize(majority_role = "closeness_yn", TotalMinutes =  sum(TotalMinutes)) 
      Interaction_Partner_new <- rbind(Interaction_Partner_new,closeness_yn)
      
      colnames(Interaction_Partner_new) <- c("content", "TotalMinutes")
 
    
    Min <- rbind(Interaction_Duration,Interaction_Partner_new)
    
    # Attributes last interaction partner
    Names <- unlist(strsplit(Last_Interaction$interact_partner_multipleChoice_string, ","))
    
    
    Interactions_data <-
      data.frame(
        Date = as.POSIXct(time),
        timescale_beforeESM = paste(rule_int,"h", sep = ""),
        ParticipantNumber = Participant[number],
        
        # Aggregated Interactions
        Total_Minutes_f2f = sum(Min$TotalMinutes[Min$content == "Face-to-face"]),
        Total_Minutes_calling = sum(Min$TotalMinutes[Min$content == "Audio call"],Min$TotalMinutes[Min$content == "Video call"]),
        Total_Minutes_texting = sum(Min$TotalMinutes[Min$content == "Instant text messaging (including voice messages)"]),
        Total_Minutes_striving_behavior = sum(Min$TotalMinutes[Min$content == "striving_behavior"]),
        Total_Minutes_mundane_maintenance_behavior = sum(Min$TotalMinutes[Min$content == "mundane_maintenance_behavior"]),
        Total_Minutes_work_school_talk = sum(Min$TotalMinutes[Min$content == "work_school_talk"]),
        Total_Minutes_Negative_content = sum(Min$TotalMinutes[Min$content == "Negative_content"]),
        
        # Last Interaction CHECKED
        Last_Interaction_enjoy = ifelse(length(Last_Interaction$interact_enjoy_sliderNegPos) != 0,Last_Interaction$interact_enjoy_sliderNegPos,NA),
        Last_Interaction_other_enjoy = ifelse(length(Last_Interaction$interact_other_enjoy_sliderNegPos) != 0,Last_Interaction$interact_other_enjoy_sliderNegPos,NA),
        Last_Interaction_meaningful = ifelse(length(Last_Interaction$interact_meaning_sliderNegPos) != 0,Last_Interaction$interact_meaning_sliderNegPos,NA),
        Last_Interaction_myself = ifelse(length(Last_Interaction$interact_myself_sliderNegPos) != 0,Last_Interaction$interact_myself_sliderNegPos,NA),
        Last_Interaction_costenergy = ifelse(length(Last_Interaction$interact_energy_cost_sliderNegPos) != 0,Last_Interaction$interact_energy_cost_sliderNegPos,NA),
        Last_Interaction_giveenergy = ifelse(length(Last_Interaction$interact_energy_give_sliderNegPos) != 0,Last_Interaction$interact_energy_give_sliderNegPos,NA),
        Last_Interaction_happy = ifelse(length(Last_Interaction$interact_pa_sliderNegPos) != 0,Last_Interaction$interact_pa_sliderNegPos,NA),
        Last_Interaction_time = ifelse(length(Last_Interaction_time) != 0, Last_Interaction_time,NA),
        
        # Egocentric network aggregated variables
        Total_Minutes_partner = sum(Min$TotalMinutes[Min$content == "Partner"]),
        Total_Minutes_friend = sum(Min$TotalMinutes[Min$content == "Friend"]),
        Total_Minutes_family = sum(Min$TotalMinutes[Min$content == "Family"]),
        Total_Minutes_fellowstud_colleague = sum(Min$TotalMinutes[Min$content == "Fellow Student/Colleague"]),
        Total_Minutes_flatmate = sum(Min$TotalMinutes[Min$content == "Flatmate"]),
        Total_Minutes_Teacher_Superior = sum(Min$TotalMinutes[Min$content == "Teacher/Superior"]),
        Total_Minutes_closeness = sum(Min$TotalMinutes[Min$content == "closeness_yn"]),
        Total_Minutes_discusspersonal = sum(Min$TotalMinutes[Min$content == "personal_issues"]),
        Total_Minutes_emotionalsupport = sum(Min$TotalMinutes[Min$content == "emotional_support"]),
        Total_Minutes_practicalsupport = sum(Min$TotalMinutes[Min$content == "practical_support"]),
        
        # Egocentric network last interaction CHECKED
        Last_Interaction_partner_closeness = ifelse(length(Last_Interaction_time) != 0, mean(PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% Names,]$closeness), NA),
        Last_Interaction_partner_energygive = ifelse(length(Last_Interaction_time) != 0, mean(PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% Names,]$gives_energy), NA),
        Last_Interaction_partner_energycost = ifelse(length(Last_Interaction_time) != 0, mean(PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% Names,]$costs_energy), NA),
        Last_Interaction_partner_myself = ifelse(length(Last_Interaction_time) != 0, mean(PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% Names,]$myself), NA),
        Last_Interaction_partner_f2f_frequency = ifelse(length(Last_Interaction_time) != 0, mean(PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% Names,]$f2fcontact), NA),
        Last_Interaction_partner_call_frequency = ifelse(length(Last_Interaction_time) != 0, mean(PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% Names,]$call_contact), NA),
        Last_Interaction_partner_text_frequency = ifelse(length(Last_Interaction_time) != 0, mean(PSN_attr_Overall_single[PSN_attr_Overall_single$name %in% Names,]$text_contact), NA)
      )
    
    Interactions_Aggregated = suppressMessages(full_join(Interactions_Aggregated, Interactions_data))
    
  }
  
  Interactions_Aggregated[,2:ncol(Interactions_Aggregated)] <- Interactions_Aggregated[,2:ncol(Interactions_Aggregated)] %>% mutate_all(~ifelse(is.nan(.), NA, .))
  return(Interactions_Aggregated)
}

