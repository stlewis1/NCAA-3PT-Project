True 3PT Percentage Project

##############################################################################################
# Code Files (for predictions):
##############################################################################################
Update Data:
  Run entire file to update data for 2026,
  update predictions, and create team dictionary

Team Predictions table:
  Contains function to create table to display predictions


##############################################################################################
# Code Files (to create model):
##############################################################################################
Get Player Stats
  Load in all box score data and calculate statistics for the model

First Model: 
  Create Random Forest model with the data. 


Predictions:
  First file to make predictions on current season data,
  evaluate error on last season data, and view distribution
  of predictions

Sample Visuals:
  Create Visuals for final presentation

Player IDS Espn:
  Unused due to data inconsistencies --> scrape player information from espn api
  
Get Roster Info (exploratory file):
  Unused due to data inconsistencies --> scrape player information from espn api
  
##############################################################################################
# Data Files
##############################################################################################

rf_model_3pt_pct.rds:
  Random forest model

full_team_ids.Rda:
  Data frame for easy team id matching
  
espn_mbb_player_info_.....rds (Folder):
  Data frames with player information from
  espn. Does not include all players
  

