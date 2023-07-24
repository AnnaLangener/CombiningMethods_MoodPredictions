### This is the Github repository for "Predicting mood based on the social environment measured through the experience sampling method, digital phenotyping, and social networks"

### This repository contains the following files

**Scripts for data cleaning:**

*ESM*

1.  CleaningNew_Sample2.R: was used to clean the ESM data (based on the raw mpath files) --\> DataCleaned.csv (<https://github.com/AnnaLangener/TestingMethodsSocialContext/blob/main/Sample%202/CleaningNew_Sample2.R>)

2.  SocialContext_BehappID.ipynb: Was used to add the BehappID to the cleanded ESM dataset, to filter out participants from the cleaned ESM dataset that don't use Behapp, to filter out questionnaires that were scheduled but not started, and to bring the time stamp in the same format. Not uploaded for privacy reasons. --\> ESMCleaned.csv

*Egocentric Networks*

3.  Clean_egocentricnetwork.R: was used to clean egocentric network --\> EgocentricNetwork.csv

*Passive Measures*

4.  Sample 2 - Metadata - Exclusion.ipynb: was used to check Behapp data availability and to identfiy which participants will be excluded (whether 75% of mpath data was available was directly checked on the mpath website, script will not be published for privacy reasons). Not uploaded for privacy reasons.

5.  Match with ESM data_NewVersion.ipynb/ Match_ESM_Passive.py: Was used to aggregate the passive smartphone measures on different time scales and to use different missing data handling strategies --\> PassiveMeasures.csv

*Combine three datasets*

6.  CreateSocialVariable.R contains the function that was used to create features for social interactions

7.  data_matching.R: Was used to match the ESM, BehappID, Egocentric network ( --\> Scheduled_ESM.csv, Interaction_ESM.csv), final datasets were created (DatasetG_lag1, Dataset_G\_lag05, DatasetL_lag1, DatasetL_lag05)

**Scripts for data analysis:**

*Descriptive*

1.  Descriptive.Rmd

*Prediction*

2.  BlockedCV.R (Function for the Cross Validation)
3.  TimeSeriesPrediction_Sample2.R (all results are stored in the folder "Results")
4.  Prediction_Results.Rmd

*Feature Importance*

4.  TimeSeriesFeatureImportance_new.R (all results are stored in the folder "Results")
