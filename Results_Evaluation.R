
#### Results Time Series Prediction ####
########################################

library(dplyr)
library(ggplot2)

# Outcome: negative affect, positive affect
# Predictors: only digital phenotyping, only ego, only esm
# Lag: 1 hour, 30 minutes
# Dataset

# Aggregation: 3, 6, 24 hours
# Rolling window size: 15, 20, 30

########## Choose outcome & Data set (to save result) ########

lag <- "lag1" # lag05 (change which data get reads in above)
dataset <- "DataG" # DataG (change which data get reads in above)
predictors <- "all" #"onlypassive", "onlyESM", "onlyEgo", "all" (select variables)

outcome <- "pa_mean" # pa_mean
step <- 3 # step 1

Overall_results_allparticipants <- read.csv(paste("Results/OverallResults","_step",step,"_",dataset,"_",lag,"_",outcome,"_",predictors,".csv",sep = ""))
Overall_pred_data <- read.csv(paste("Results/OverallPred","_step",step,"_",dataset,"_",lag,"_",outcome, "_",predictors,".csv",sep = ""))

Overall_results_mean <- read.csv(paste("Results/OverallResultsMEAN","_step",step,"_",outcome,".csv",sep = ""))
Overall_pred_mean <- read.csv(paste("Results/OverallPredMEAN","_step",step,"_",outcome,".csv",sep = ""))


Overall_results_allparticipants$Participant <- recode(Overall_results_allparticipants$V9,"TSCS2_2JV3YK" = 1,"TSCS2_3HJ9LJ" = 2, "TSCS2_4KUR2Z" = 3,"TSCS2_4NXMUS" = 4,
                                     "TSCS2_5MNS2N" = 5,"TSCS2_5W3RE9" = 6,"TSCS2_6B972L" = 7, "TSCS2_6BBRGP" = 8,"TSCS2_7NJKV7" = 9,"TSCS2_857ER8" = 10,"TSCS2_9C99HU" = 11)

Overall_pred_data$Participant <- recode(Overall_pred_data$Participant,"TSCS2_2JV3YK" = 1,"TSCS2_3HJ9LJ" = 2, "TSCS2_4KUR2Z" = 3,"TSCS2_4NXMUS" = 4,
                                                      "TSCS2_5MNS2N" = 5,"TSCS2_5W3RE9" = 6,"TSCS2_6B972L" = 7, "TSCS2_6BBRGP" = 8,"TSCS2_7NJKV7" = 9,"TSCS2_857ER8" = 10,"TSCS2_9C99HU" = 11)

Overall_results_mean$Participant <- recode(Overall_results_mean$V9,"TSCS2_2JV3YK" = 1,"TSCS2_3HJ9LJ" = 2, "TSCS2_4KUR2Z" = 3,"TSCS2_4NXMUS" = 4,
                                                      "TSCS2_5MNS2N" = 5,"TSCS2_5W3RE9" = 6,"TSCS2_6B972L" = 7, "TSCS2_6BBRGP" = 8,"TSCS2_7NJKV7" = 9,"TSCS2_857ER8" = 10,"TSCS2_9C99HU" = 11)

Overall_pred_mean$Participant <- recode(Overall_pred_mean$Participant,"TSCS2_2JV3YK" = 1,"TSCS2_3HJ9LJ" = 2, "TSCS2_4KUR2Z" = 3,"TSCS2_4NXMUS" = 4,
                                           "TSCS2_5MNS2N" = 5,"TSCS2_5W3RE9" = 6,"TSCS2_6B972L" = 7, "TSCS2_6BBRGP" = 8,"TSCS2_7NJKV7" = 9,"TSCS2_857ER8" = 10,"TSCS2_9C99HU" = 11)

### Maximum values
Best <- Overall_results_allparticipants %>% group_by(Participant) %>% top_n(1, rsq)
BestMean <- Overall_results_mean %>% group_by(Participant) %>% top_n(1, rsq)

# Percentage positive rsquared
Overall_results_allparticipants %>% group_by(Participant) %>% filter(rsq > 0) %>% summarize(n()/9)

Overall_pred_data %>% group_by(Participant, aggregation, w) %>% filter(is.na(pred)) %>% summarize(n()/max(index))

### Visualizations ####

#"TSCS2_2JV3YK","TSCS2_857ER8","TSCS2_3HJ9LJ","TSCS2_6B972L","TSCS2_9C99HU","TSCS2_4NXMUS","TSCS2_4KUR2Z","TSCS2_5MNS2N","TSCS2_6BBRGP",
#"TSCS2_7NJKV7","TSCS2_5W3RE9"


plot_best_mean(Overall_results_mean,Overall_pred_mean, ParticipantNumber = 5)

p1 <- plot_best(Overall_results_allparticipants,Overall_pred_data, ParticipantNumber = 1) +
  ggtitle("Participant: 1")

library(patchwork)
for(i in 2:11){
p2 <- plot_best(Overall_results_allparticipants,Overall_pred_data, ParticipantNumber = i) +
  ggtitle(paste("Participant:",i))
p1 <- p1 + p2
}

p1

Best2 <- Overall_pred_data %>% group_by(Participant,aggregation, w) %>% summarise(rsq = rsq(pred,true)) %>% group_by(Participant) %>% top_n(1, rsq)

### Used functions

plot_best <- function(Overall_results_allparticipants,Overall_pred_data, ParticipantNumber =1){
  Overall_results <- Overall_results_allparticipants[Overall_results_allparticipants$Participant == ParticipantNumber, ]
  Overall_pred_data <- Overall_pred_data[Overall_pred_data$Participant == ParticipantNumber,]
  
  Overall_pred_max <- Overall_pred_data[Overall_pred_data$w == Overall_results$k[which(Overall_results$rsq == max(Overall_results$rsq, na.rm = TRUE))] &
                                          Overall_pred_data$aggregation == Overall_results$n[which(Overall_results$rsq == max(Overall_results$rsq, na.rm = TRUE))],]
  
  # Plot
  plot <- ggplot() +
    geom_line(aes(x = c(1:length(Overall_pred_max$pred)), y = lead(Overall_pred_max$pred), color = "pred")) +
    geom_line(aes(x = c(1:length(Overall_pred_max$true)), y = Overall_pred_max$true, color = "true")) +
    theme_minimal() +
    geom_line(aes(x = c(1:length(Overall_pred_max$true)), y = mean(Overall_pred_max$true, na.rm = TRUE), color = "mean")) +
    ylab("") +
    xlab("")
  return(plot)
}

plot_best_mean <- function(Overall_results_allparticipants,Overall_pred_data, ParticipantNumber ="TSCS2_5W3RE9" ){
  Overall_results <- Overall_results_allparticipants[Overall_results_allparticipants$Participant == ParticipantNumber, ]
  Overall_pred_data <- Overall_pred_data[Overall_pred_data$Participant == ParticipantNumber,]
  
  Overall_pred_max <- Overall_pred_data[Overall_pred_data$w == Overall_results$k[Overall_results$rsq == max(Overall_results$rsq, na.rm = TRUE)],]
  
  # Plot
  plot <- ggplot() +
    geom_line(aes(x = c(1:length(Overall_pred_max$pred)), y = lead(Overall_pred_max$pred), color = "pred")) +
    geom_line(aes(x = c(1:length(Overall_pred_max$true)), y = Overall_pred_max$true, color = "true")) +
    theme_minimal() +
    geom_line(aes(x = c(1:length(Overall_pred_max$true)), y = mean(Overall_pred_max$true, na.rm = TRUE), color = "mean")) +
    ylab("") +
    xlab("")
  return(plot)
}


rsq <- function(pred,true){
  rss <- sum((pred - true)^2,na.rm = TRUE)  # residual sum of squares
  tss <- sum((true - mean(true, na.rm = TRUE))^2,na.rm = TRUE)  # total sum of squares
  rsq <- 1 - rss/tss
}