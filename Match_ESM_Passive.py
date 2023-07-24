import behapp as bhp
import pandas as pd
import datetime
from datetime import date, timedelta
import numpy as np
from IPython.display import clear_output
import time
import numpy
from behapp.data_processing.app_cleaning import app_cleaning
from geopy.distance import geodesic



######## Define Function to calculate Features ##############
#############################################################
def ESM_passivemeasures(ESM_time, participant, rule = [3],Lag = [1], app_cleaning_yn = True, Missing_Time = 24):

    ######## Load Data and Prepare them ##############
    ##################################################


    # Info:
    # For intervall data we create a dataframe that shows everything per second. We can use this dataframe later to calculate the differen features
    # Code that was used to create dataframes per second (df1,df2,df3) is adapted from here: 
    # https://stackoverflow.com/questions/56341590/unstacking-shift-data-start-and-end-time-into-hourly-data.

    ### Calls

    calld = participant.data.call

    if calld is not None:
        #add end time to calls
        end_time = []
        for i in range(0,calld.shape[0]):
            durationN = calld["duration"][i]
            start_time = calld.index[i]
            end_time2 = calld.index[i] + timedelta(seconds=int(durationN))
            end_time.append(end_time2)
        calld["end_time"] = end_time

        calld = calld.reset_index()
        calld['recorded_naive'] = pd.to_datetime(calld['recorded_naive'])

        # Create a dataset that shows whether someone was calling or not per second

        df3 = pd.DataFrame([(z, w) for x, y, w in zip(calld['recorded_naive'], 
                                                      calld['end_time'] - pd.Timedelta(1, unit='s'), 
                                                      calld['dtype']) for z in pd.date_range(x, y, freq='s')], 
                           columns=['Date','dtype']) 

        # Then we do the same for the type pf call
        df3b = pd.DataFrame([(z, w) for x, y, w in zip(calld['recorded_naive'], 
                                                              calld['end_time'] - pd.Timedelta(1, unit='s'), 
                                                              calld['caller_hash']) for z in pd.date_range(x, y, freq='s')], 
                                   columns=['Date','caller_hash']) 

        # Merge both dataframes
        df3 = pd.merge(df3, df3b, how = 'left')

    ### Screen
    screen = participant.data.screen
    screen = screen.reset_index()

    ### Location
    staypoints = participant.views.staypoints # Staypoint dataset
    locraw = participant.data.location # Raw GPS points, used for data labeling
    locraw['index_time'] = locraw.index 

    if staypoints is not None:

        #### Rename clusters based on frequency of visits

        # Sort staypoints by frequency of visit 
        staypoints_sort = staypoints.groupby(['cluster_label']).count()
        staypoints_sort = staypoints_sort.reset_index()
        staypoints_sort = staypoints_sort.sort_values("time_spent_minutes",ascending=False)

        # Add new name
        staypoints_sort["cluster_label_new"] = 0
        staypoints_sort.loc[staypoints_sort["cluster_label"] != "HOME", "cluster_label_new"] = list(range(1,staypoints_sort.shape[0]))
        staypoints_sort.loc[staypoints_sort["cluster_label"] == "HOME", "cluster_label_new"] = "HOME"

        staypoints_sort = staypoints_sort[["cluster_label","cluster_label_new"]]

        # Merge with staypoint data set
        staypoints = pd.merge(staypoints,staypoints_sort, how = "left")
        
        # Add average distance from home
        home_latitude = staypoints.loc[staypoints.cluster_label=="HOME"].latitude.median()
        home_longitude = staypoints.loc[staypoints.cluster_label=="HOME"].longitude.median()

        staypoints["distance_from_home_kilometers"] = staypoints.apply(
            lambda x: round(geodesic((home_latitude, home_longitude), (x.latitude, x.longitude)).kilometers, 2), axis=1)

        # Create a dataset that shows on which staypoint someone was per second

        df2 = pd.DataFrame([(z, w) for x, y, w in zip(staypoints['arrival_time'], 
                                                              staypoints['leave_time'] - pd.Timedelta(1, unit='s'), 
                                                              staypoints['cluster_label_new']) for z in pd.date_range(x, y, freq='s')], 
                                   columns=['Date','Cluster']) 
    #Load trajectories
    trajectories = participant.views.trajectories
    trajectory = pd.concat([pd.DataFrame(trajectories[i]) for i in range(len(trajectories))], ignore_index=True)


    ### App
    # app_cleaning will avoid overlapping app usage.
    if app_cleaning_yn == True:
        app = app_cleaning(participant.data.app)
    if app_cleaning_yn == False:
        app = participant.data.app

    if app is not None:
    # add end time which we will need to calculate features
        end_time = []
        for i in range(0,app.shape[0]):
            durationN = app["duration"][i]
            start_time = app.index[i]
            end_time2 = app.index[i] + timedelta(seconds=int(durationN))
            end_time.append(end_time2)

        app["end_time"] = end_time

        app.category.fillna('unknown', inplace=True)

        # Create a dataset that shows which app someone was using per second
        df1 = pd.DataFrame([(z, w) for x, y, w in zip(app.index, 
                                                          app['end_time'] - pd.Timedelta(1, unit='s'), 
                                                          app['package_name']) for z in pd.date_range(x, y, freq='s')], 
                               columns=['Date','Package']) 

        # Do the same for Genre
        df1b = pd.DataFrame([(z, w) for x, y, w in zip(app.index, 
                                                          app['end_time'] - pd.Timedelta(1, unit='s'), 
                                                          app['category']) for z in pd.date_range(x, y, freq='s')], 
                               columns=['Date','Category']) 

        # Merge both dataframes
        df1 = pd.merge(df1, df1b, how = 'left')



    ### Wifi
    wifi = participant.data.wifi
    wifi = wifi.reset_index()

    #put each wifi macHash in a row

    wifi_new = []

    for i in range(wifi.shape[0]):
        wifi_hotspots = pd.DataFrame.from_dict(wifi.hotspots[i])
        recorded_naive = pd.DataFrame(wifi.iloc[[i]]["recorded_naive"])

        if wifi_hotspots.shape[0] > 0: #Check whether any connections are recorded
            recorded_naive = pd.concat([recorded_naive]*wifi_hotspots.shape[0]).reset_index(drop=True) #repeat time for each macHash

        wifi_combined = pd.concat([recorded_naive,wifi_hotspots], axis = 1)
        wifi_new.append([wifi_combined])    

    wifi_new = pd.concat([pd.DataFrame(wifi_new[i][0]) for i in range(len(wifi_new))], ignore_index=True)

    wifi = wifi_new

    # Here we create lists
    FullFeatureOverview = []
    
    
    for lag in Lag:

        for rule_int in rule:
            FeatureOverviewApps = []
            FeatureOverviewLocation = []
            FeatureOverviewWifi = []
            FeatureOverviewCall = [] 
            FeatureOverviewScreen = []


            # Now we loop through each time point from the ESM questionnaires (stored in ESM_time) and calculate the featuers in relation to our chosen temporal resolution

            for i in range(0,ESM_time.shape[0]-1):
                time = ESM_time.loc[i,'index_time'] #get time from ESM questionnaire

                ########## APP USAGE ##########
                ###############################

                if app is not None:
                    # next we select the rows that are between x hours before the esm questionnaire (time - time delta) but not after the questionnaire is filled out
                    HourAppDf = df1[(df1['Date'] > (time - timedelta(hours = rule_int + lag))) & (df1['Date'] < time - timedelta(hours = lag))].groupby(['Package']).size().div(60).to_frame('TotalMinutes')
                    HourAppDf = HourAppDf.reset_index()

                    if HourAppDf.shape[0] > 0:

                        HourAppDf['Date'] = time
                        # Minutes spent on WhatsAppa
                        HourAppDf['Package_min'] = HourAppDf.Package + "_min"

                        HourAppDf = HourAppDf.pivot_table(index=['Date'], columns= 'Package_min', values='TotalMinutes').reset_index()

                        # Minutes spent on all apps
                        HourAppDf['APP_USAGE_min'] = HourAppDf.sum(axis =1)

                        # Number of apps opened
                        HourAppDf['APPS_OPENED_number'] = app[(app['end_time'] > (time - timedelta(hours = rule_int + lag))) & (app['end_time'] < time - timedelta(hours = lag))].shape[0]

                        # Minutes spent on communication apps, social media apps
                        Category = df1[(df1['Date'] > (time - timedelta(hours = rule_int + lag))) & (df1['Date'] < time - timedelta(hours = lag))].groupby(['Category']).size().div(60).to_frame('Minutes')
                        Category = Category.reset_index()

                        Category["Date"] = time

                        Category['Category_min'] = Category.Category + "_min"

                        ReShapeCategory = Category.pivot(index=['Date'], columns='Category_min', values='Minutes').reset_index()


                        if HourAppDf.shape[0] > 0:
                            FeaturesCompleteApps = pd.merge(HourAppDf, ReShapeCategory, how = 'outer') 


                            FeatureOverviewApps.append([FeaturesCompleteApps])



                        ######### Location #############
                        ################################
                        if staypoints is not None:

                            # again we select the rows that are between x hours before the esm questionnaire (time - time delta) but not after the questionnaire is filled out
                            HourLocationDF = df2[(df2['Date'] > (time - timedelta(hours = rule_int + lag))) & (df2['Date'] < time - timedelta(hours = lag))].groupby(['Cluster']).size().div(60).to_frame('TotalMinutes')
                            HourLocationDF = HourLocationDF.reset_index()

                            HourLocationDF['Date'] = time
                            HourLocationDF['Cluster_min'] = "Cluster_" +  HourLocationDF.Cluster.astype('str') + "_min"

                            FeaturesLocation = HourLocationDF.pivot_table(index=['Date'], columns= 'Cluster_min', values='TotalMinutes').reset_index()


                            #Time spent stationary
                            TIME_STATIONARY_min = FeaturesLocation.sum(axis =1,numeric_only=True)

                            if len(TIME_STATIONARY_min) > 0:
                                TIME_STATIONARY_min = TIME_STATIONARY_min[0]

                            else:
                                TIME_STATIONARY_min = 0

                            #Number of staypoints
                            # Columns: Date has to be substracte, That's why I do "-1"
                            UNIQUE_STAYPOINTS_number = pd.notnull(FeaturesLocation.loc[:,"Date":]).sum(axis=1,numeric_only=True) - 1
                            if len(UNIQUE_STAYPOINTS_number) > 0:
                                UNIQUE_STAYPOINTS_number = UNIQUE_STAYPOINTS_number[0]
                            else:
                                UNIQUE_STAYPOINTS_number = 0
                                
                            
                            # Total distance travelled      
                            TOTAL_DISTANCE_TRAVELLED_km = trajectory[(trajectory['index_time'] > (time - timedelta(hours = rule_int + lag))) & (trajectory['index_time'] < time - timedelta(hours = lag))].point_to_point_distance_meters.sum()/1000
                            
                            # Average distance from home
                            AVERAGE_DISTANCE_HOME_km = staypoints[(staypoints['arrival_time'] > (time - timedelta(hours = rule_int + lag))) & (staypoints['arrival_time'] < time - timedelta(hours = lag))].distance_from_home_kilometers.mean()
                            
                                
                            if FeaturesLocation.shape[0] > 0:

                                FeaturesLocation['TIME_STATIONARY_min'] = TIME_STATIONARY_min
                                FeaturesLocation['UNIQUE_STAYPOINTS_number'] = UNIQUE_STAYPOINTS_number
                                FeaturesLocation['TOTAL_DISTANCE_TRAVELLED_km'] = TOTAL_DISTANCE_TRAVELLED_km
                                FeaturesLocation['AVERAGE_DISTANCE_HOME_km'] = AVERAGE_DISTANCE_HOME_km
                                
                                FeatureOverviewLocation.append([FeaturesLocation])


                        ######## Wifi ###########
                        #########################

                        if wifi is not None:

                            HourFeaturesWifi = pd.DataFrame({"Date": [time]})

                            # again we select the rows that are between x hours before the esm questionnaire (time - time delta) but not after the questionnaire is filled out
                            HourFeaturesWifi["TOTAL_MACHASHES_number"] = wifi[(wifi['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (wifi['recorded_naive'] < time - timedelta(hours = lag))].shape[0]
                            HourFeaturesWifi["UNIQUE_MACHASHES_number"] = wifi[(wifi['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (wifi['recorded_naive'] < time - timedelta(hours = lag))].macHash.unique().shape[0]

                            if HourFeaturesWifi.shape[0] > 0:

                                FeatureOverviewWifi.append([HourFeaturesWifi]) 


                        ###### Calls #######
                        ####################

                        if calld is not None:

                            CallFeatures = pd.DataFrame({"Date": [time]})
                            CallFeatures['CALL_TOTAL_min'] = int(df3[(df3['Date'] > (time - timedelta(hours = rule_int + lag))) & (df3['Date'] < time - timedelta(hours = lag))].shape[0])/60
                            CallFeatures['CALL_incoming_min'] = int(df3[(df3['Date'] > (time - timedelta(hours = rule_int + lag))) & (df3['Date'] < time - timedelta(hours = lag)) & (df3["dtype"] == "incoming")].shape[0])/60
                            CallFeatures['CALL_outgoing_min'] = int(df3[(df3['Date'] > (time - timedelta(hours = rule_int + lag))) & (df3['Date'] < time - timedelta(hours = lag)) & (df3["dtype"] == "outgoing")].shape[0])/60

                        # Number of calls
                            CallFeatures['MISSED_CALLS_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (calld['recorded_naive'] < time - timedelta(hours = lag)) & (calld["dtype"] == 'missed')].shape[0])
                            CallFeatures['CALL_TOTAL_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (calld['recorded_naive'] < time - timedelta(hours = lag))].shape[0])
                            CallFeatures['CALL_incoming_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (calld['recorded_naive'] < time - timedelta(hours = lag)) & (calld["dtype"] == "incoming")].shape[0])
                            CallFeatures['CALL_outgoing_number']= int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (calld['recorded_naive'] < time - timedelta(hours = lag)) & (calld["dtype"] == "outgoing")].shape[0])

                            CallFeatures['CALL_UNIQUE_CONTACTS_number'] = int(calld[(calld['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (calld['recorded_naive'] < time - timedelta(hours = lag))].caller_hash.unique().shape[0])

                            if len(CallFeatures) > 0:                  

                                FeatureOverviewCall.append([CallFeatures]) 




                        ###### Screen #######
                        ####################

                        if screen is not None:

                            ScreenFeatures = pd.DataFrame({"Date": [time]})

                            ScreenFeatures['SCREEN_onLocked_number'] = int(screen[(screen['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (screen['recorded_naive'] < time - timedelta(hours = lag)) & (screen["status"] == "onLocked")].shape[0])
                            ScreenFeatures['SCREEN_onUnlocked_number']= int(screen[(screen['recorded_naive'] > (time - timedelta(hours = rule_int + lag))) & (screen['recorded_naive'] < time - timedelta(hours = lag)) & (screen["status"] == "onUnlocked")].shape[0])



                            if screen is not None:                

                                    FeatureOverviewScreen.append([ScreenFeatures]) 

            ############# Labeling Missing Values #############
            ###################################################

            #Here we label missing values per sensor. For each day we check if more than X (X = Missing_time) subsequent hours are without recored data. If so, we exclude all (ESM) data from that day.

            #****** APP USAGE *******
            FeatureOverviewApps = pd.concat([pd.DataFrame(FeatureOverviewApps[i][0]) for i in range(len(FeatureOverviewApps))], ignore_index=True)
            FeatureOverviewApps = pd.merge(FeatureOverviewApps,ESM_time['Date'], on = ['Date'], how = 'right')

            ### Create an index whether or not to label an hour as NA
            #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
            Index_App = (df1.groupby([df1['Date'].dt.date.rename('Date'), 
                                           df1['Date'].dt.hour.rename('Time'), df1['Package'], df1['Category']]) 
                              .size().div(60).div(60).reset_index(name='APP_USAGE_pct'))
            Index_App = Index_App.groupby(['Date','Time'], axis=0, as_index=False).sum()

            Index_App["Difference_00"] = Index_App["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

            ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
            # Create time index
            Index_App["Date"] = pd.to_datetime(Index_App["Date"])
            Index_App["Time"] = pd.to_timedelta(Index_App["Time"], unit = "h")
            Index_App["index_time"] = Index_App["Date"] + Index_App["Time"]

            ## Check Direction 1 (for the following hours)

            Index_App["Difference"] = abs((Index_App['index_time']).groupby(Index_App['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

            Index_App.loc[np.isnan(Index_App.loc[:,"Difference"]),"Difference"] = Index_App["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

            Index_App.loc[0,"Difference"] = 0 # Label very first day as 0

            #Create an index whether or not the difference is more than X hours (X = Missing_Time)
            Index_App["Difference_index"] = 0
            Index_App["Difference_index"][abs(Index_App["Difference"]) > Missing_Time] = 1

            ## Check Direction 2 (for the last hours)

            Index_App["Difference"] = abs((Index_App['index_time']).groupby(Index_App['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

            Index_App.loc[np.isnan(Index_App.loc[:,"Difference"]),"Difference"] = 24 - Index_App["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

            Index_App.loc[Index_App.shape[0],"Difference"] = 0 # Label last day as 0

            # Add a one to the index
            Index_App["Difference_index"][abs(Index_App["Difference"]) > Missing_Time] = Index_App["Difference_index"] + 1

            ### Group by date
            Index_App = Index_App.groupby(Index_App['Date']).sum().reset_index() 

            ### Label Missing Values
            FeatureOverviewApps = FeatureOverviewApps.fillna(0)

            #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
            for i in range(FeatureOverviewApps.shape[0]):
                if  pd.to_datetime(FeatureOverviewApps['Date'].dt.date)[i] not in pd.to_datetime(Index_App['Date'][Index_App["Difference_index"] == 0]).to_list():
                    FeatureOverviewApps.iloc[i,1:] = float(-9999)  


            #****** Location *******
            FeatureOverviewLocation = pd.concat([pd.DataFrame(FeatureOverviewLocation[i][0]) for i in range(len(FeatureOverviewLocation))], ignore_index=True)
            FeatureOverviewLocation = pd.merge(FeatureOverviewLocation,ESM_time['Date'], on = ['Date'], how = 'right')

            ### Create an index whether or not to label an hour as NA
            #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
            Index_Loc = locraw.groupby([locraw['index_time'].dt.date.rename('Date'),locraw['index_time'].dt.hour.rename('Time')]).size().reset_index(name = 'Number')
            Index_Loc = Index_Loc.groupby(['Date','Time'], axis=0, as_index=False).sum()

            Index_Loc["Difference_00"] = Index_Loc["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

            ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
            # Create time index
            Index_Loc["Date"] = pd.to_datetime(Index_Loc["Date"])
            Index_Loc["Time"] = pd.to_timedelta(Index_Loc["Time"], unit = "h")
            Index_Loc["index_time"] = Index_Loc["Date"] + Index_Loc["Time"]

            ## Check Direction 1 (for the following hours)

            Index_Loc["Difference"] = abs((Index_Loc['index_time']).groupby(Index_Loc['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

            Index_Loc.loc[np.isnan(Index_Loc.loc[:,"Difference"]),"Difference"] = Index_Loc["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

            Index_Loc.loc[0,"Difference"] = 0 # Label very first day as 0

            #Create an index whether or not the difference is more than X hours (X = Missing_Time)
            Index_Loc["Difference_index"] = 0
            Index_Loc["Difference_index"][abs(Index_Loc["Difference"]) > Missing_Time] = 1

            ## Check Direction 2 (for the last hours)

            Index_Loc["Difference"] = abs((Index_Loc['index_time']).groupby(Index_Loc['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

            Index_Loc.loc[np.isnan(Index_Loc.loc[:,"Difference"]),"Difference"] = 24 - Index_Loc["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

            Index_Loc.loc[Index_Loc.shape[0],"Difference"] = 0 # Label last day as 0

            # Add a one to the index
            Index_Loc["Difference_index"][abs(Index_Loc["Difference"]) > Missing_Time] = Index_Loc["Difference_index"] + 1

            ### Group by date
            Index_Loc = Index_Loc.groupby(Index_Loc['Date']).sum().reset_index() 

            ### Label Missing Values
            FeatureOverviewLocation = FeatureOverviewLocation.fillna(0)

            #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
            for i in range(FeatureOverviewLocation.shape[0]):
                if  pd.to_datetime(FeatureOverviewLocation['Date'].dt.date)[i] not in pd.to_datetime(Index_Loc['Date'][Index_Loc["Difference_index"] == 0]).to_list():
                    FeatureOverviewLocation.iloc[i,1:] = float(-9999)  


            #****** Wifi *******
            FeatureOverviewWifi = pd.concat([pd.DataFrame(FeatureOverviewWifi[i][0]) for i in range(len(FeatureOverviewWifi))], ignore_index=True)
            FeatureOverviewWifi = pd.merge(FeatureOverviewWifi,ESM_time['Date'], on = ['Date'], how = 'right')

            ### Create an index whether or not to label an hour as NA
            #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
            Index_wifi = wifi.groupby([wifi['recorded_naive'].dt.date.rename('Date'),wifi['recorded_naive'].dt.hour.rename('Time'),wifi['macHash']]).size().reset_index(name = 'UNIQUE_MACHASHES_norm')

            Index_wifi = Index_wifi.groupby(['Date','Time'], axis=0, as_index=False).sum()

            Index_wifi["Difference_00"] = Index_wifi["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

            ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
            # Create time index
            Index_wifi["Date"] = pd.to_datetime(Index_wifi["Date"])
            Index_wifi["Time"] = pd.to_timedelta(Index_wifi["Time"], unit = "h")
            Index_wifi["index_time"] = Index_wifi["Date"] + Index_wifi["Time"]

            ## Check Direction 1 (for the following hours)

            Index_wifi["Difference"] = abs((Index_wifi['index_time']).groupby(Index_wifi['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

            Index_wifi.loc[np.isnan(Index_wifi.loc[:,"Difference"]),"Difference"] = Index_wifi["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

            Index_wifi.loc[0,"Difference"] = 0 # Label very first day as 0

            #Create an index whether or not the difference is more than X hours (X = Missing_Time)
            Index_wifi["Difference_index"] = 0
            Index_wifi["Difference_index"][abs(Index_wifi["Difference"]) > Missing_Time] = 1

            ## Check Direction 2 (for the last hours)

            Index_wifi["Difference"] = abs((Index_wifi['index_time']).groupby(Index_wifi['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

            Index_wifi.loc[np.isnan(Index_wifi.loc[:,"Difference"]),"Difference"] = 24 - Index_wifi["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

            Index_wifi.loc[Index_wifi.shape[0],"Difference"] = 0 # Label last day as 0

            # Add a one to the index
            Index_wifi["Difference_index"][abs(Index_wifi["Difference"]) > Missing_Time] = Index_wifi["Difference_index"] + 1

            ### Group by date
            Index_wifi = Index_wifi.groupby(Index_wifi['Date']).sum().reset_index() 

            ### Label Missing Values
            FeatureOverviewWifi = FeatureOverviewWifi.fillna(0)

            #Check if Difference_index is 0 (Thus, we also check for days that are completly without any data) 
            for i in range(FeatureOverviewWifi.shape[0]):
                if  pd.to_datetime(FeatureOverviewWifi['Date'].dt.date)[i] not in pd.to_datetime(Index_wifi['Date'][Index_wifi["Difference_index"] == 0]).to_list():
                    FeatureOverviewWifi.iloc[i,1:] = float(-9999)  



            #****** Call *******

            if len(calld) > 0:
                FeatureOverviewCall = pd.concat([pd.DataFrame(FeatureOverviewCall[i][0]) for i in range(len(FeatureOverviewCall))], ignore_index=True)
                FeatureOverviewCall = pd.merge(FeatureOverviewCall,ESM_time['Date'], on = ['Date'], how = 'right')

            else:
                FeatureOverviewCall = pd.DataFrame({"CALL_TOTAL_min": 0},index = range(ESM_time.shape[0])) 

            FeatureOverviewCall = FeatureOverviewCall.fillna(0)

            #****** Screen *******

            if len(screen) > 0:
                FeatureOverviewScreen = pd.concat([pd.DataFrame(FeatureOverviewScreen[i][0]) for i in range(len(FeatureOverviewScreen))], ignore_index=True)

                FeatureOverviewScreen = pd.merge(FeatureOverviewScreen,ESM_time['Date'], on = ['Date'], how = 'right')

                ### Create an index whether or not to label an hour as NA
                #Show AppUsage per hour to check how many subsequent hours we have during the day in which no app usage was reported
                Index_screen = screen.groupby([screen['recorded_naive'].dt.date.rename('Date'),screen['recorded_naive'].dt.hour.rename('Time')]).size().reset_index(name = 'Count')

                Index_screen = Index_screen.groupby(['Date','Time'], axis=0, as_index=False).sum()

                Index_screen["Difference_00"] = Index_screen["Time"] # as a reference to calculate hours from 00:00 to first measurement and from last measurement to 00:00

                ### Calculate the time difference between each recorded app usage (to check how many subsequent hours exist without app usage)
                # Create time index
                Index_screen["Date"] = pd.to_datetime(Index_screen["Date"])
                Index_screen["Time"] = pd.to_timedelta(Index_screen["Time"], unit = "h")
                Index_screen["index_time"] = Index_screen["Date"] + Index_screen["Time"]

                ## Check Direction 1 (for the following hours)

                Index_screen["Difference"] = abs((Index_screen['index_time']).groupby(Index_screen['Date']).diff(1).dt.total_seconds().div(60).div(60)) # Grouped by day

                Index_screen.loc[np.isnan(Index_screen.loc[:,"Difference"]),"Difference"] = Index_screen["Difference_00"] # Add difference from 00:00 to the first recorded hour of the day (because we don't always have app usage at 00:00)

                Index_screen.loc[0,"Difference"] = 0 # Label very first day as 0

                #Create an index whether or not the difference is more than X hours (X = Missing_Time)
                Index_screen["Difference_index"] = 0
                Index_screen["Difference_index"][abs(Index_screen["Difference"]) > Missing_Time] = 1

                ## Check Direction 2 (for the last hours)

                Index_screen["Difference"] = abs((Index_screen['index_time']).groupby(Index_screen['Date']).diff(-1).dt.total_seconds().div(60).div(60)) # same just that we go in the other direction

                Index_screen.loc[np.isnan(Index_screen.loc[:,"Difference"]),"Difference"] = 24 - Index_screen["Difference_00"] #Add difference from last measurement to 24:00 (because we don't always have app usage at 00:00)

                Index_screen.loc[Index_screen.shape[0],"Difference"] = 0 # Label last day as 0

                # Add a one to the index
                Index_screen["Difference_index"][abs(Index_screen["Difference"]) > Missing_Time] = Index_screen["Difference_index"] + 1

                ### Group by date
                Index_screen = Index_screen.groupby(Index_screen['Date']).sum().reset_index() 

                # Label Missing Values
                FeatureOverviewScreen = FeatureOverviewScreen.fillna(0)

                for i in range(FeatureOverviewScreen.shape[0]):
                    if  pd.to_datetime(FeatureOverviewScreen['Date'].dt.date)[i] not in pd.to_datetime(Index_screen['Date'][Index_screen["Difference_index"] == 0]).to_list():
                        FeatureOverviewScreen.iloc[i,1:] = float(-9999)  

            FeatureOverview = pd.concat([FeatureOverviewApps,FeatureOverviewLocation,FeatureOverviewWifi, FeatureOverviewCall, FeatureOverviewScreen], axis = 1)
            FeatureOverview["timescale_beforeESM"] = rule_int
            FeatureOverview["Lag"] = lag
            FeatureOverview["ParticipantNumber"] = participant.human_friendly_id
            FeatureOverview["MissingData"] = Missing_Time


            FullFeatureOverview.append([FeatureOverview]) 
            
    FullFeatureOverview = pd.concat([pd.DataFrame(FullFeatureOverview[i][0]).loc[:,~pd.DataFrame(FullFeatureOverview[i][0]).columns.duplicated()].copy() for i in range(len(FullFeatureOverview))],ignore_index = True)

    FullFeatureOverview = FullFeatureOverview.fillna(0)
    FullFeatureOverview = FullFeatureOverview.replace(-9999, "NaN")
            
    return FullFeatureOverview