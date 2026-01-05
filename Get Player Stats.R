library(hoopR)
library(tidyverse)

# Load in All box scores
current_season <- most_recent_mbb_season()
all_box <- load_mbb_player_box(seasons = current_season:2016)

# Load in team ids
# team_ids <- read_rds("full_team_ids.Rda")
# team_ids <- read_rds("team_ids.Rda")
team_ids <- read_rds("Rosters/bbr_team_ids.Rda") %>% filter(Season == "2025-26")


# Stable Adjustment
# padding_3pt <- 50
padding_3pt_szn <- 100
padding_3pt_career <- 100
padding_ft <- 25
ncaa_avg_3 <- sum(all_box$three_point_field_goals_made, na.rm = T) / sum(all_box$three_point_field_goals_attempted, na.rm = T)
ncaa_avg_ft <- sum(all_box$free_throws_made, na.rm = T) / sum(all_box$free_throws_attempted, na.rm = T)

# Get all variables for the Model
model_data <- all_box %>%
  filter(season < current_season,
         team_id %in% team_ids$team_id) %>%
  select(season, game_date, athlete_id, athlete_display_name, team_id,
         team_display_name, minutes, field_goals_made, field_goals_attempted,
         three_point_field_goals_made, three_point_field_goals_attempted,
         free_throws_made, free_throws_attempted, points, assists, turnovers,
         starter, did_not_play, team_score, athlete_jersey, athlete_position_abbreviation) %>%
  filter(#athlete_id %in% player_stats_20_25$athlete_id,
         did_not_play == FALSE, minutes > 0) %>%
  # Get data for each season
  group_by(season, athlete_id) %>%
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
    # pts = sum(points, na.rm = TRUE),
    # assists = sum(assists, na.rm = TRUE),
    # tovs = sum(turnovers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # mutate(across(where(is.numeric), ~replace_na(.x, 0))) %>% # (Optional, replace NAs with 0s)
  # # add player info
  # right_join(player_stats_20_25 %>% select(athlete_id, height, weight, full_name,
  #                                         birth_place_country, position_abbreviation), 
  #           by = "athlete_id") %>%
  # filter(!is.na(height), !is.na(weight)) %>%
  ungroup() %>% 
  # Calculate past and cumulative stats
  group_by(athlete_id) %>%
  filter(sum(total_3pt_attempted, na.rm = T) > 10) %>%
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
    # career_ast_tov_shots_40 = (cumsum(replace_na(ast, 0)) + cumsum(replace_na(tovs, 0)) + 
    #                              cumsum(replace_na(total_fg_attempted,0))) / career_minutes * 40,
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
    # lag_career_ast_tov_shots_40 = lag(career_ast_tov_shots_40),
    lag_career_efg_pct = lag(career_efg_pct)
  ) %>%
  ungroup() 

model_data %>% filter(exp_year != 1) %>% view() 

# colnames(all_box)
  