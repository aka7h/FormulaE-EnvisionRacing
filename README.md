# FormulaE-EnvisionRacing

## Problem Statement

To build a machine learning model that predicts the Envision Racing drivers’ lap times for the all-important qualifying sessions that determine what position they start the race in. Winning races involves a combination of both a driver’s skills and data analytics. To help the team you’ll need to consider several factors that affect performance during a session, including weather, track conditions, and a driver’s familiarity with the track.

* Leaderboard topper - [baseline-lights-out-and-away.ipynb](https://github.com/aka7h/FormulaE-EnvisionRacing/blob/main/baseline-lights-out-and-away.ipynb)
-------------
## Data Description 

Weather dataset contains the following data from start time to end time of each session at 1 min interval

* Air Temp
* Humidity
* Pressure
* Track Temp
* Wind Speed
* Wind Direction
* Rain
* Location
* Event (Free Practice / Qualifying Session)

Train dataset contains Location 1-5 Free practice and Qualifying Events

* Driver detailes (Driver Number, Name, Team, Group)
* Sector times (S1,S2,S3 and S1_LARGE, S2_LARGE, S3_LARGE)
* Sector improvements
* Pit time
* Elapsed time**
* Power & Max Speed
* Event
* Location
-------------

## Approach

* Cleaned weather data as it contains corrupted data in track side variables
* Cleaned corrupted team name in train data
* Converted elapsed time into to minute index based on their sector times and merged it with the indexed weather data, we get weather available for each sector
* Pit time are overlaped with either S1 of next lap or S3 of previous lap, cleaned the data to get proper sector times for each lap
* Standardized the weather data as the each numeircal values are in different unit, replaced missing values with -999
* Train data is further split into 80/20 for cross validation

## Evalualtion Metrics
Submissions are evalualted based on the RMLSE values

## Models 

Catboost was the base model used

Best model - baseline-lights-out-and-away.ipynb - 0.7232

## Exprerimented Approaches

* Ran a H2O AutoML "GLM" model with just Sector and Pit times
* Used Bayesian Optimization on Catboost for hyperparameter tuning
* Considered the residual errors of the regression model and used it as a output variable for Catboost+Bayes
* Ran a AutoML H2O model

## Future Enhancement (TO-DO)

* Run A Deeplearning RNN/LSTM model
* Consider the lag values of previous lap
* Create a timeline based visualization for each event and location to check each driver position
