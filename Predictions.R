library(hoopR)
library(tidyverse)

###########################################################################################
# Get Prediction Data
###########################################################################################

# Get Player Stats
current_season <- most_recent_mbb_season()
all_box <- load_mbb_player_box(seasons = current_season:2017)

# padding_3pt_szn <- 100
# padding_3pt_career <- 100

# Create Data for predictions (same as model data)
prediction_data <- all_box %>%
  select(season, game_date, athlete_id, athlete_display_name, team_id,
         team_display_name, minutes, field_goals_made, field_goals_attempted,
         three_point_field_goals_made, three_point_field_goals_attempted,
         free_throws_made, free_throws_attempted, points, assists, turnovers,
         starter, did_not_play, team_score, athlete_jersey, , athlete_position_abbreviation) %>%
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
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% # (Optional, replace NAs with 0s)
  # Add player info
  # right_join(player_stats_20_25 %>% select(athlete_id, height, weight, full_name,
  #                                          birth_place_country, position_abbreviation), 
  #            by = "athlete_id") %>%
  # filter(!is.na(height), !is.na(weight)) %>%
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
    lag_career_stable_3pt = lag(career_stable_3pt),
    lag_career_stable_ft = lag(career_stable_ft),
    lag_career_3pt_made = lag(career_3pt_made),
    lag_career_3pt_attempts = lag(career_3pt_attempts),
    lag_career_3pt_pct = lag(career_3pt_pct),
    lag_career_pts_g = lag(career_pts_g),
    lag_career_ast_g = lag(career_ast_g),
    lag_career_tov_g = lag(career_tov_g),
    lag_career_pct_shots_3pt = lag(career_pct_shots_3pt)
  ) %>%
  ungroup() %>%
  # Subset for 2026
  filter( 
    season == current_season,
    #exp_year > 1,
    # athlete_id %in% ids_26
         )

# Player ids for 2026 season (not used)
# ids_26 <- all_box %>%
#   filter(team_id %in% team_ids$team_id) %>% 
#   filter(season == current_season) %>%  
#   distinct(athlete_id) 
#   pull(athlete_id)
  

# Freshman (no d1 experience) 3pt average
ncaa_avg_3pt_freshman <- all_box %>% group_by(athlete_id) %>%
  filter(team_id %in% team_ids$team_id) %>%
  mutate(first_season = min(season),
         seasons = n_distinct(season),
         ) %>%
  ungroup() %>%
  filter(season == first_season, seasons == 1, first_season >= 2020) #%>%
  # summarise(fg3_pct = sum(three_point_field_goals_made, na.rm = T) / sum(three_point_field_goals_attempted, na.rm = T)) %>%
  # view()

ncaa_avg_3pt_freshman <- sum(ncaa_avg_3pt_freshman$three_point_field_goals_made, na.rm = T) / 
  sum(ncaa_avg_3pt_freshman$three_point_field_goals_attempted, na.rm = T) 

# View roster count
prediction_data %>% group_by(team_display_name) %>% summarise(n = n()) %>% arrange(desc(n)) %>% view()

###########################################################################################
# Make Predictions for players who played in 2026
###########################################################################################
# OLD CONFIDENCE INTERVAL METHOD
# t_dist_value <- qt(0.975, df = nrow(tree_data) - 1)
# rf_rmse <- sqrt(rf1$mse[length(rf1$mse)])

# Make Predictions
predictions_2026 <- prediction_data %>%
  mutate(
    predicted_stable_3pt_pct = predict(rf1, newdata = prediction_data),
    predicted_stable_3pt_pct = ifelse(is.na(predicted_stable_3pt_pct), ncaa_avg_3pt_freshman, predicted_stable_3pt_pct),
    season_stable_adjusted_3pt_pct = (total_3pt_made + (padding_3pt_szn*predicted_stable_3pt_pct)) / 
      (total_3pt_attempted + padding_3pt_szn),
    # predicted_stable_3pt_pct = case_when(
    #   predicted_stable_3pt_pct <= 0 ~ ncaa_avg_3,
    #   predicted_stable_3pt_pct >= 1 ~ ncaa_avg_3,
    #   TRUE ~ predicted_stable_3pt_pct
    # )
    # lower_est = season_stable_adjusted_3pt_pct - 
    #   (t_dist_value * rf_rmse * (1/ifelse(total_3pt_attempted == 0, 1, total_3pt_attempted))^(0.15)),
    # upper_est = season_stable_adjusted_3pt_pct + 
    #   (t_dist_value * rf_rmse * (1/ifelse(total_3pt_attempted == 0, 1, total_3pt_attempted))^(0.15)),
    alpha = total_3pt_made + padding_3pt_szn * predicted_stable_3pt_pct,
    beta  = (total_3pt_attempted - total_3pt_made) + padding_3pt_szn * (1 - predicted_stable_3pt_pct),
    ci_lower = qbeta(0.05, alpha, beta),
    # p_stable = alpha / (alpha + beta), # Same as season_stable_adjusted_3pt_pct
    ci_upper = qbeta(0.95, alpha, beta),
    ci_length = (ci_upper - ci_lower) / 2 # Length of CI (+/- from predicted value)
  ) %>% 
  filter(!is.na(predicted_stable_3pt_pct)) %>%
  select(athlete_id, full_name, team_display_name, team_id, position, jersey_no,
         exp_year, first_season, last_season,
         career_3pt_made, career_3pt_attempts, career_3pt_pct, career_stable_3pt,
         season, games, total_3pt_made, total_3pt_attempted, fg3_pct, season_stable_3pt_pct = stable_3pt_pct,
         predicted_stable_3pt_pct, ci_lower, season_stable_adjusted_3pt_pct, ci_upper, ci_length
         ) 

# Player counts for each team
predictions_2026 %>% group_by(team_id) %>% 
  summarise(Team = first(team_display_name), n = n()) %>% arrange(Team) %>% view()

###########################################################################################
# Distribution of Predictions
###########################################################################################

hist(predictions_2026$predicted_stable_3pt_pct*100, breaks = 30)


###########################################################################################
# Evaluate Error for 2025
###########################################################################################
prediction_data <- all_box %>%
  select(season, game_date, athlete_id, athlete_display_name, team_id,
         team_display_name, minutes, field_goals_made, field_goals_attempted,
         three_point_field_goals_made, three_point_field_goals_attempted,
         free_throws_made, free_throws_attempted, points, assists, turnovers,
         starter, did_not_play, team_score, athlete_jersey, , athlete_position_abbreviation) %>%
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
  mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% # (Optional, replace NAs with 0s)
  # Add player info
  # right_join(player_stats_20_25 %>% select(athlete_id, height, weight, full_name,
  #                                          birth_place_country, position_abbreviation), 
  #            by = "athlete_id") %>%
  # filter(!is.na(height), !is.na(weight)) %>%
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
    lag_career_stable_3pt = lag(career_stable_3pt),
    lag_career_stable_ft = lag(career_stable_ft),
    lag_career_3pt_made = lag(career_3pt_made),
    lag_career_3pt_attempts = lag(career_3pt_attempts),
    lag_career_3pt_pct = lag(career_3pt_pct),
    lag_career_pts_g = lag(career_pts_g),
    lag_career_ast_g = lag(career_ast_g),
    lag_career_tov_g = lag(career_tov_g),
    lag_career_pct_shots_3pt = lag(career_pct_shots_3pt)
  ) %>%
  ungroup() %>%
  filter(
    season == current_season - 1,
    exp_year > 1,
    # athlete_id %in% ids_26
  )



predictions_2025 <- prediction_data %>%
  mutate(
    predicted_stable_3pt_pct = predict(rf1, newdata = prediction_data),
    # predicted_stable_3pt_pct = ifelse(is.na(predicted_stable_3pt_pct), ncaa_avg_3pt_freshman, predicted_stable_3pt_pct),
    season_stable_adjusted_3pt_pct = (total_3pt_made + (padding_3pt_szn*predicted_stable_3pt_pct)) / 
      (total_3pt_attempted + padding_3pt_szn),
    # predicted_stable_3pt_pct = case_when(
    #   predicted_stable_3pt_pct <= 0 ~ ncaa_avg_3,
    #   predicted_stable_3pt_pct >= 1 ~ ncaa_avg_3,
    #   TRUE ~ predicted_stable_3pt_pct
    # )
    # lower_est = season_stable_adjusted_3pt_pct - 
    #   (t_dist_value * rf_rmse * (1/ifelse(total_3pt_attempted == 0, 1, total_3pt_attempted))^(0.15)),
    # upper_est = season_stable_adjusted_3pt_pct + 
    #   (t_dist_value * rf_rmse * (1/ifelse(total_3pt_attempted == 0, 1, total_3pt_attempted))^(0.15)),
    alpha = total_3pt_made + padding_3pt_szn * predicted_stable_3pt_pct,
    beta  = (total_3pt_attempted - total_3pt_made) + padding_3pt_szn * (1 - predicted_stable_3pt_pct),
    ci_lower = qbeta(0.05, alpha, beta),
    # p_stable = alpha / (alpha + beta),
    ci_upper = qbeta(0.95, alpha, beta),
    ci_length = (ci_upper - ci_lower) / 2,
    pred_error = season_stable_adjusted_3pt_pct - predicted_stable_3pt_pct
  ) %>% 
  filter(!is.na(predicted_stable_3pt_pct)) %>%
  select(athlete_id, full_name, team_display_name, team_id, position, jersey_no,
         exp_year, first_season, last_season,
         career_3pt_made, career_3pt_attempts, career_3pt_pct, career_stable_3pt,
         season, games, total_3pt_made, total_3pt_attempted, fg3_pct, season_stable_3pt_pct = stable_3pt_pct,
         predicted_stable_3pt_pct, ci_lower, season_stable_adjusted_3pt_pct, ci_upper, pred_error
  ) 


# RMSE ~ 1.1%
sqrt(mean((predictions_2025$season_stable_adjusted_3pt_pct - predictions_2025$predicted_stable_3pt_pct)^2))*100
