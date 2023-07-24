####################################################################
## Sample 2, Network cleaning (adapted from Marie Stadels script) ##
####################################################################

## 0. Packages ----

library(tidyverse) # for general data handling 
library(skimr)
library(janitor)

library(igraph) # for (network) visualizations
library(ggraph)
library(tidygraph)
library(gplots)
library(ggpubr)
library(RColorBrewer)
library(DT) # to highlight cells in dataframes

library(rjson) # to read json format

############ Network Data ############ 
######################################

# With this script we preprocess the personal network (as exported by Stijn) 

PSN_attr_Overall <- data.frame()

ids <- # DELETED FOR ANONYMIZATION # I only chose participants with behapp data

BEHAPPID = c("TSCS2_2JV3YK","TSCS2_857ER8","TSCS2_3HJ9LJ","TSCS2_6B972L","TSCS2_9C99HU","TSCS2_4NXMUS","TSCS2_4KUR2Z","TSCS2_5MNS2N","TSCS2_6BBRGP",
            "TSCS2_7NJKV7","TSCS2_5W3RE9")

mPath = # DELETED FOR ANONYMIZATION

for (i in 1:length(ids)){
  id <- ids[i]
  
  directory <- paste('/Users/annalangener/Nextcloud/Shared/Testing Methods to Capture Social Context/Sample 2/Single Participant Data/', 
                     id, sep = '')
  filename <- paste('PostPSN_', id, '.txt', sep = '')
  
  setwd(directory)
  PSN <- fromJSON(file = filename)  
  
  
  ## 2. Format data ----
  
  name <- unlist(sapply(PSN[[2]], function(x) x$name))
  generator <- sapply(PSN[[2]], function(x) x$generatorId)
  generator <- unlist(lapply(generator, function(x) 
    if(is.null(x)){x <- 'ESM'} else{x <- x-1}))
  # observations with no generator come from ESM assessments and should be labeled as such
  
  
  closeness_yn <- sapply(PSN[[2]], function(x) x$closeff)
  closeness_yn <- unlist(lapply(closeness_yn, function(x) 
    if(is.null(x)){x <- 0} else{x <- 1})) 
  
  # Now I will reformat the relationship variables: give labels & make them into one factor
  relationship <- sapply(PSN[[2]], function(x) x$relation)
  relationship <- relationship[-c(1)] # again, drop ego
  lapply(relationship, 
         function(x) str_replace_all(x,
                                     c('1' = 'partner','2' = 'parent','3' = 'sibling',
                                       '4' = 'other_relative','5' = 'friend','6' = 'aquaintance',
                                       '7' = 'flatmate','8' = 'fellow_student/colleague',
                                       '9' = 'superior/teacher'))) -> relationship 
  lapply(relationship, 
         function(x) str_replace_all(x,c('partner0' = 'other'))) -> relationship
  relationship <- unlist(lapply(relationship, 
                                function(x) paste(x, collapse = "-"))) # summarize all 10 into one
  
  # Here I make the attribute dataframe: 
  PSN_attr <- tibble(name = name[-c(1)],    # first observation 'You' should be omitted
                     generator = generator[-c(1)],
                     gender = unlist(sapply(PSN[[2]], function(x) x$gender)),
                     age = unlist(sapply(PSN[[2]], function(x) x$age)),
                     relationship = relationship,
                     closeness_yn = closeness_yn[-c(1)],
                     closeness = unlist(sapply(PSN[[2]], function(x) x$close)),
                     f2fcontact = unlist(sapply(PSN[[2]], function(x) x$face2face)),
                     call_contact = unlist(sapply(PSN[[2]], function(x) x$calls)),
                     text_contact = unlist(sapply(PSN[[2]], function(x) x$texts)),
                     gives_energy = unlist(sapply(PSN[[2]], function(x) x$givesenergy)),
                     costs_energy = unlist(sapply(PSN[[2]], function(x) x$costsenergy)),
                     myself = unlist(sapply(PSN[[2]], function(x) x$bemyself)),
                     discuss_personal = unlist(sapply(PSN[[2]], function(x) x$discussissues)),
                     emotional_support = unlist(sapply(PSN[[2]],
                                                       function(x) x$emotionalsupport)),
                     practical_support = unlist(sapply(PSN[[2]],
                                                       function(x) x$practicalsupport)))
  
  # still split the relationship variable in three for alters that have multiple roles (allows max 3):
  PSN_attr %>% separate(relationship, c("relationship1", "relationship2", "relationship3"), sep="-") -> PSN_attr 
  
  rm(PSN, generator, name, relationship) # clean up temporary objects
  
  # Now I still recode all the variables so that interview and PSN data are on the same scale:
  
  PSN_attr$gender <- recode(PSN_attr$gender, `1` = 1, `2` = 0, `3` = 2)
  PSN_attr$age <- recode(PSN_attr$age, 
                         `1` = 0, `2` = 1, `3` = 2, `4` = 3, `5` = 4, `6` = 5, 
                         `7` = 6, `8` = 7, `9` = 8, `10` = 9)
  
  PSN_attr$closeness <- recode(PSN_attr$closeness, 
                               `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0)
  PSN_attr$f2fcontact<- recode(PSN_attr$f2fcontact, 
                               `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0)
  PSN_attr$call_contact <- recode(PSN_attr$call_contact, 
                                  `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0)
  PSN_attr$text_contact <- recode(PSN_attr$text_contact, 
                                  `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0)
  
  PSN_attr$gives_energy <- recode(PSN_attr$gives_energy, 
                                  `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0)
  PSN_attr$costs_energy <- recode(PSN_attr$costs_energy, 
                                  `1` = 4, `2` = 3, `3` = 2, `4` = 1, `5` = 0)
  
  PSN_attr$discuss_personal <- recode(PSN_attr$discuss_personal,`2` = 0)
  PSN_attr$emotional_support <- recode(PSN_attr$emotional_support,`2` = 0)
  PSN_attr$practical_support <- recode(PSN_attr$practical_support,`2` = 0)
  PSN_attr$myself <- recode(PSN_attr$myself, `2` = 0)
  
  PSN_attr$relationship1 <- factor(PSN_attr$relationship1,
                                   levels = c("partner","parent", "sibling", 
                                              "other_relative","friend", "aquaintance",
                                              "flatmate", "fellow_student/colleague",
                                              "superior/teacher", "other"), 
                                   labels = c("Partner","Parent", "Sibling", 
                                              "Other Relative","Friend", "Aquaintance",
                                              "Flatmate", "Fellow Student/Colleague",
                                              "Teacher/Superior", "Other"))
  
  PSN_attr$relationship2 <- factor(PSN_attr$relationship2,
                                   levels = c("partner","parent", "sibling", 
                                              "other_relative","friend", "aquaintance",
                                              "flatmate", "fellow_student/colleague",
                                              "superior/teacher", "other"), 
                                   labels = c("Partner","Parent", "Sibling", 
                                              "Other Relative","Friend", "Aquaintance",
                                              "Flatmate", "Fellow Student/Colleague",
                                              "Teacher/Superior", "Other"))
  
  PSN_attr$relationship3 <- factor(PSN_attr$relationship3,
                                   levels = c("partner","parent", "sibling", 
                                              "other_relative","friend", "aquaintance",
                                              "flatmate", "fellow_student/colleague",
                                              "superior/teacher", "other"), 
                                   labels = c("Partner","Parent", "Sibling", 
                                              "Other Relative","Friend", "Aquaintance",
                                              "Flatmate", "Fellow Student/Colleague",
                                              "Teacher/Superior", "Other"))
  PSN_attr %>% distinct() -> PSN_attr
  
  PSN_attr$ParticipantNumber <- ids[i]
  PSN_attr$BehappID <- BEHAPPID[i]
  PSN_attr$mPath <- mPath[i]
  
  PSN_attr_Overall <- as.data.frame(rbind(PSN_attr_Overall,PSN_attr)) #Combine lists into one dataframe

}


write.csv(PSN_attr_Overall, "/Users/annalangener/Nextcloud/CombiningMethodsMoodPrediction_Data/EgocentricNetwork.csv")
