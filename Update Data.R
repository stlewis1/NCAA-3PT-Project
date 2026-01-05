library(hoopR)
library(tidyverse)
library(randomForest)

# Load in Model
rf1 <- readRDS("True 3pt Project/rf_model_3pt_pct.rds")


# Load in data
current_season <- most_recent_mbb_season()
all_box <- load_mbb_player_box(seasons = current_season:2016)
# team_ids <- read_rds("full_team_ids.Rda")
team_ids <- read_rds("Rosters/bbr_team_ids.Rda") %>% filter(Season == "2025-26")
padding_3pt_szn <- 100
padding_3pt_career <- 100
padding_ft <- 25
ncaa_avg_3 <- sum(all_box$three_point_field_goals_made, na.rm = T) / sum(all_box$three_point_field_goals_attempted, na.rm = T)
ncaa_avg_ft <- sum(all_box$free_throws_made, na.rm = T) / sum(all_box$free_throws_attempted, na.rm = T)

# Create Data Frame for Prediction 
prediction_data <- all_box %>%
  select(season, game_date, athlete_id, athlete_display_name, team_id,
         team_display_name, minutes, field_goals_made, field_goals_attempted,
         three_point_field_goals_made, three_point_field_goals_attempted,
         free_throws_made, free_throws_attempted, points, assists, turnovers,
         starter, did_not_play, team_score, athlete_jersey, athlete_position_abbreviation) %>%
  filter(did_not_play == FALSE, minutes > 0,
         team_id %in% team_ids$team_id) %>%
  # Get data for each season
  group_by(season, athlete_id) %>%
  arrange(desc(season)) %>%
  summarise(
    full_name = first(athlete_display_name),
    team_id = first(team_id),
    team_display_name = first(team_display_name),
    position = first(athlete_position_abbreviation),
    jersey_no = first(athlete_jersey),
    games = sum(did_not_play == FALSE),
    starts = sum(starter == TRUE),
    total_minutes = sum(minutes, na.rm = TRUE),
    total_fg_made = sum(field_goals_made, na.rm = TRUE),
    total_fg_attempted = sum(field_goals_attempted, na.rm = TRUE),
    total_3pt_made = sum(three_point_field_goals_made, na.rm = TRUE),
    total_3pt_attempted = sum(three_point_field_goals_attempted, na.rm = TRUE),
    total_ft_made = sum(free_throws_made, na.rm = TRUE),
    total_ft_attempted = sum(free_throws_attempted, na.rm = TRUE),
    min_per_game = total_minutes / games,
    fg_per_game = total_fg_made / games,
    fg_att_per_game = total_fg_attempted / games,
    fg3_per_game = total_3pt_made / games,
    fg3_att_per_game = total_3pt_attempted / games,
    ft_per_game = total_ft_made / games,
    ft_att_per_game = total_ft_attempted / games,
    pts_g = sum(points, na.rm = TRUE) / games,
    ast_g = sum(assists, na.rm = TRUE) / games,
    tov_g = sum(turnovers, na.rm = TRUE) / games,
    pts_40min = sum(points, na.rm = TRUE) / total_minutes * 40,
    ast_40min = sum(assists, na.rm = TRUE) / total_minutes * 40,
    tov_40min = sum(turnovers, na.rm = TRUE) / total_minutes * 40,
    fg_pct = total_fg_made / total_fg_attempted,
    fg3_pct = total_3pt_made / total_3pt_attempted,
    ft_pct = total_ft_made / total_ft_attempted,
    efg_pct = (total_fg_made + 0.5 * total_3pt_made) / total_fg_attempted,
    stable_3pt_pct = (total_3pt_made + (padding_3pt_szn*ncaa_avg_3)) / (total_3pt_attempted + padding_3pt_szn),
    stable_ft_pct = (total_ft_made + (padding_ft*ncaa_avg_ft)) / (total_ft_attempted + padding_ft),
    pct_shots_3pt = total_3pt_attempted / total_fg_attempted,
    ast_tov_shots_40 = (sum(assists, na.rm = T) + sum(turnovers, na.rm = T) + sum(field_goals_attempted)) / total_minutes * 40,
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>%
  ungroup() %>% 
  # Calculate past and cumulative stats
  group_by(athlete_id) %>%
  filter(sum(total_3pt_attempted, na.rm = T) > 0) %>%
  arrange(season) %>%
  mutate(
    first_season = min(season),
    last_season = max(season),
    exp_year = row_number(),
    max_seasons = n(),
    changed_teams = if_else(team_id != lag(team_id) & !is.na(lag(team_id)), 1, 0),
    career_starts = cumsum(replace_na(starts, 0)),
    career_games = cumsum(replace_na(games, 0)),
    career_minutes = cumsum(replace_na(total_minutes, 0)),
    career_mpg = career_minutes / career_games,
    career_stable_3pt = (cumsum(replace_na(total_3pt_made, 0)) + (padding_3pt_career*ncaa_avg_3) ) /
      (cumsum(replace_na(total_3pt_attempted, 0)) + padding_3pt_career),
    career_stable_ft = (cumsum(replace_na(total_ft_made, 0)) + (padding_ft*ncaa_avg_ft) ) /
      (cumsum(replace_na(total_ft_attempted, 0)) + padding_ft),
    career_3pt_attempts = cumsum(replace_na(total_3pt_attempted, 0)),
    career_3pt_made = cumsum(replace_na(total_3pt_made, 0)),
    career_3pt_pct = career_3pt_made / career_3pt_attempts,
    career_pts_g = cumsum(replace_na(pts_g * games, 0)) / career_games,
    career_ast_g = cumsum(replace_na(ast_g * games, 0)) / career_games,
    career_tov_g = cumsum(replace_na(tov_g * games, 0)) / career_games,
    career_pct_shots_3pt = cumsum(replace_na(total_3pt_attempted, 0)) / cumsum(replace_na(total_fg_attempted, 0)),
    career_efg_pct = (cumsum(replace_na(total_fg_made, 0)) + 0.5 * cumsum(replace_na(total_3pt_made, 0))) / 
      cumsum(replace_na(total_fg_attempted, 0)),
    lag_starts = lag(starts),
    lag_games = lag(games),
    lag_minutes = lag(total_minutes),
    lag_fg_made = lag(total_fg_made),
    lag_fg_attempted = lag(total_fg_attempted),
    lag_3pt_made = lag(total_3pt_made),
    lag_3pt_attempted = lag(total_3pt_attempted),
    lag_ft_made = lag(total_ft_made),
    lag_ft_attempted = lag(total_ft_attempted),
    lag_fg_per_game = lag(fg_per_game),
    lag_fg_att_per_game = lag(fg_att_per_game),
    lag_fg3_per_game = lag(fg3_per_game),
    lag_fg3_att_per_game = lag(fg3_att_per_game),
    lag_ft_per_game = lag(ft_per_game),
    lag_ft_att_per_game = lag(ft_att_per_game),
    lag_mpg = lag(min_per_game),
    lag_pts_g = lag(pts_g),
    lag_ast_g = lag(ast_g),
    lag_tov_g = lag(tov_g),
    lag_pts_40min = lag(pts_40min),
    lag_ast_40min = lag(ast_40min),
    lag_tov_40min = lag(tov_40min),
    lag_fg_pct = lag(fg_pct),
    lag_fg3_pct = lag(fg3_pct),
    lag_ft_pct = lag(ft_pct),
    lag_efg_pct = lag(efg_pct),
    lag_stable_3pt_pct = lag(stable_3pt_pct),
    lag_stable_ft_pct = lag(stable_ft_pct),
    lag_pct_shots_3pt = lag(pct_shots_3pt),
    lag_ast_tov_shots_40 = lag(ast_tov_shots_40),
    lag_career_starts = lag(career_starts),
    lag_career_games = lag(career_games),
    lag_career_minutes = lag(career_minutes),
    lag_career_mpg = lag(career_mpg),
    lag_career_stable_3pt = lag(career_stable_3pt),
    lag_career_stable_ft = lag(career_stable_ft),
    lag_career_3pt_made = lag(career_3pt_made),
    lag_career_3pt_attempts = lag(career_3pt_attempts),
    lag_career_3pt_pct = lag(career_3pt_pct),
    lag_career_pts_g = lag(career_pts_g),
    lag_career_ast_g = lag(career_ast_g),
    lag_career_tov_g = lag(career_tov_g),
    lag_career_pct_shots_3pt = lag(career_pct_shots_3pt),
    lag_career_efg_pct = lag(career_efg_pct)
  ) %>%
  ungroup() %>%
  filter(season == current_season)

###########################################################################################
# Make Predictions 
###########################################################################################

# 3pt average for players with no games played
ncaa_avg_3pt_freshman <- all_box %>% group_by(athlete_id) %>%
  filter(team_id %in% team_ids$team_id) %>%
  mutate(first_season = min(season),
         seasons = n_distinct(season)) %>%
  ungroup() %>%
  filter(season == first_season, seasons == 1, first_season >= 2020) 

ncaa_avg_3pt_freshman <- sum(ncaa_avg_3pt_freshman$three_point_field_goals_made, na.rm = T) / 
  sum(ncaa_avg_3pt_freshman$three_point_field_goals_attempted, na.rm = T) 


# Predictions
predictions_2026 <- prediction_data %>%
  mutate(
    predicted_stable_3pt_pct = predict(rf1, newdata = prediction_data),
    predicted_stable_3pt_pct = ifelse(is.na(predicted_stable_3pt_pct), ncaa_avg_3pt_freshman, predicted_stable_3pt_pct),
    lag_career_3pt_attempts = ifelse(is.na(lag_career_3pt_attempts), 0, lag_career_3pt_attempts),
    lag_career_3pt_made = ifelse(is.na(lag_career_3pt_made), 0, lag_career_3pt_made),
    lag_career_3pt_pct = ifelse(is.na(lag_career_3pt_pct), 0, lag_career_3pt_pct),
    padding_adjust = ((8.1*lag_career_3pt_attempts)^0.5) + 80,
    padding_adjust = ifelse(padding_adjust > 125, 125, padding_adjust),
    # season_stable_adjusted_3pt_pct = (total_3pt_made + (padding_3pt_szn*predicted_stable_3pt_pct)) / 
    #   (total_3pt_attempted + padding_3pt_szn),
    season_stable_adjusted_3pt_pct = (total_3pt_made + (padding_adjust*predicted_stable_3pt_pct)) / 
      (total_3pt_attempted + padding_adjust),
    # alpha = total_3pt_made + padding_3pt_szn * predicted_stable_3pt_pct,
    # beta  = (total_3pt_attempted - total_3pt_made) + padding_3pt_szn * (1 - predicted_stable_3pt_pct),
    alpha = total_3pt_made + padding_adjust * predicted_stable_3pt_pct,
    beta  = (total_3pt_attempted - total_3pt_made) + padding_adjust * (1 - predicted_stable_3pt_pct),
    ci_lower = qbeta(0.05, alpha, beta),
    # p_stable = alpha / (alpha + beta),
    ci_upper = qbeta(0.95, alpha, beta),
    ci_length = (ci_upper - ci_lower) / 2,
    prior_confidence = sqrt((lag_career_3pt_attempts + total_3pt_attempted)*0.141),
    prior_confidence = ifelse(prior_confidence > 6.5, 6.5, prior_confidence),
    season_confidence = sqrt(total_3pt_attempted*0.06099),
    season_confidence = ifelse(season_confidence > 3.5, 3.5, season_confidence),
    confidence_score = (prior_confidence) + (season_confidence),
  ) %>% 
  filter(!is.na(predicted_stable_3pt_pct)) %>%
  select(athlete_id, full_name, team_display_name, team_id, position, jersey_no,
         exp_year, first_season, last_season, lag_career_3pt_attempts, lag_career_3pt_made,
         career_3pt_made, career_3pt_attempts, career_3pt_pct, career_stable_3pt, lag_career_3pt_pct,
         season, games, total_3pt_made, total_3pt_attempted, fg3_pct, season_stable_3pt_pct = stable_3pt_pct,
         predicted_stable_3pt_pct, ci_lower, season_stable_adjusted_3pt_pct, ci_upper, ci_length, padding_adjust,
         prior_confidence, season_confidence, confidence_score
  ) 

# table_data <- predictions_2026

# Player counts for each team (Team Dictionary)
team_dictionary <- predictions_2026 %>% group_by(team_id) %>% 
  summarise(Team = first(team_display_name), n = n()) %>% arrange(Team)


# Create Predictions
predictions_2026 <- prediction_data %>%
  mutate(
    predicted_stable_3pt_pct = predict(rf1, newdata = prediction_data),
    predicted_stable_3pt_pct = ifelse(is.na(predicted_stable_3pt_pct), ncaa_avg_3pt_freshman, predicted_stable_3pt_pct),
    lag_career_3pt_attempts = ifelse(is.na(lag_career_3pt_attempts), 0, lag_career_3pt_attempts),
    lag_career_3pt_made = ifelse(is.na(lag_career_3pt_made), 0, lag_career_3pt_made),
    lag_career_3pt_pct = ifelse(is.na(lag_career_3pt_pct), 0, lag_career_3pt_pct),
    padding_adjust = ((8.1*lag_career_3pt_attempts)^0.5) + 80,
    padding_adjust = ifelse(padding_adjust > 125, 125, padding_adjust),
  ) %>% 
  filter(!is.na(predicted_stable_3pt_pct)) %>%
  select(
    athlete_id, full_name, team_display_name, team_id, jersey_no,
    exp_year, lag_career_3pt_attempts, lag_career_3pt_made,
    career_3pt_made, career_3pt_attempts, career_3pt_pct, lag_career_3pt_pct,
    season, fg3_pct,
    predicted_stable_3pt_pct, padding_adjust
  )

# New Prediction Data
data_26 <- load_mbb_player_box(seasons = current_season) %>%
  select(season, game_date, athlete_id, athlete_display_name, team_id,
         team_display_name, minutes, field_goals_made, field_goals_attempted,
         three_point_field_goals_made, three_point_field_goals_attempted,
         free_throws_made, free_throws_attempted, points, assists, turnovers,
         starter, did_not_play, team_score, athlete_jersey, athlete_position_abbreviation) %>%
  filter(did_not_play == FALSE, minutes > 0, season == current_season,
         team_id %in% team_ids$team_id) %>%
  # Get data for each season
  group_by(athlete_id) %>%
  summarise(
    season_total_3pt_made = sum(three_point_field_goals_made, na.rm = TRUE),
    season_total_3pt_attempted = sum(three_point_field_goals_attempted, na.rm = TRUE),
  ) %>%
  mutate(across(where(is.numeric), ~replace_na(.x, 0)))


# Update Predictions
predictions_2026 <- predictions_2026 %>%
  left_join(data_26, by = "athlete_id") %>%
  mutate(
    career_3pt_attempts = lag_career_3pt_attempts + season_total_3pt_attempted,
    career_3pt_made = lag_career_3pt_made + season_total_3pt_made,
    career_3pt_pct = career_3pt_made / career_3pt_attempts,
    season_stable_adjusted_3pt_pct = (season_total_3pt_made + (padding_adjust*predicted_stable_3pt_pct)) / 
      (season_total_3pt_attempted + padding_adjust),
    prior_confidence = sqrt((career_3pt_attempts)*0.0755),
    prior_confidence = ifelse(prior_confidence > 5.5, 5.5, prior_confidence),
    season_confidence = sqrt(season_total_3pt_attempted*0.081),
    season_confidence = ifelse(season_confidence > 4.5, 4.5, season_confidence),
    confidence_score = (prior_confidence) + (season_confidence),
  ) %>% 
  select(athlete_id, full_name, team_display_name, team_id, jersey_no,
         exp_year, lag_career_3pt_attempts, lag_career_3pt_made,
         career_3pt_made, career_3pt_attempts, career_3pt_pct, lag_career_3pt_pct,
         season, season_total_3pt_made, season_total_3pt_attempted, fg3_pct,
         predicted_stable_3pt_pct, season_stable_adjusted_3pt_pct, padding_adjust,
         prior_confidence, season_confidence, confidence_score
  )

# Table Data
all_table_data <- predictions_2026 %>%
  select(jersey_no, full_name, team_display_name, lag_career_3pt_attempts, lag_career_3pt_pct, 
         season_total_3pt_made, season_total_3pt_attempted, fg3_pct, predicted_stable_3pt_pct,
         season_stable_adjusted_3pt_pct, confidence_score, career_3pt_attempts, exp_year, 
         athlete_id, team_id, padding_adjust)

# upload data to github
write_rds(predictions_2026, "True 3pt Project/predictions_2026.rds")
write_rds(all_table_data, "True 3pt Project/all_table_data.rds")
write_rds(team_dictionary, "True 3pt Project/team_dictionary_2026.rds")
write_rds(team_ids %>% select(Team, team_id), "True 3pt Project/team_ids_2026.rds")





