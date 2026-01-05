library(hoopR)
library(tidyverse)


# Master Schedule

all_schedule <- load_mbb_schedule(seasons = 2020:current_season)


# Get game id for each team in each season
# szn_g_ids <- all_schedule %>%
#   filter(
#     !is.na(away_conference_id), !is.na(home_conference_id),
#     home_id > 0, status_type_completed, !is.na(home_winner)) %>%
#   group_by(season, home_id, home_display_name) %>%
#   summarise(
#     game_id = first(game_id),
#     .groups = "drop"
#   ) 


  
# all_game_rosters <- map_dfr(szn_g_ids$game_id, espn_mbb_game_rosters, .progress = TRUE)
# 
# rosters_26 <- map_dfr(ncaa_colors$espn_name, get_roster, .progress = TRUE)
# rosters_25 <- map_dfr(ncaa_colors$espn_name, get_roster(tea season = "2024-25"), .progress = TRUE)
# rosters_25 <- map_dfr(ncaa_colors$espn_name, function(x) {
#   get_roster(team = x, season = "2023-24")
# }, .progress = TRUE)
# get_roster(ncaa_colors$espn_name[1], season = "2024-25") %>% view()
# 
# get_roster(ncaa_colors$espn_name[1], season = "2022-23") %>% view()
# 
# ncaa_colors %>% view()
# 
# 
# # Get roster and add team and season info
# 
# rosters_26 <- map_dfr(ncaa_colors$espn_name, function(x) {
#   tmp <- get_roster(team = x)
#   tmp$team <- x
#   tmp$season <- 2026
#   return(tmp) }, .progress = TRUE) 
# 
# rosters_25 <- map_dfr(ncaa_colors$espn_name, function(x) {
#   tmp <- get_roster(team = x, season = "2023-24")
#   tmp$team <- x
#   tmp$season <- 2025
#   return(tmp)
# }, .progress = TRUE) 

ncaahoopR::ids %>% view()

ids_25 <- all_box %>%
  filter(season == 2025,
         team_id %in% ncaahoopR::ids$id
         ) %>%
  group_by(athlete_id) %>%
  filter(sum(minutes, na.rm = T) > 0) %>%
  select(athlete_id) %>%
  distinct()

# Loop through all players
player_stats_25 <- map_dfr(ids_25$athlete_id, espn_mbb_player_stats, year = 2025, .progress = T) 

ids_24 <- all_box %>%
  filter(season == 2024,
         team_id %in% ncaahoopR::ids$id
  ) %>%
  group_by(athlete_id) %>%
  filter(sum(minutes, na.rm = T) > 0,
         !(athlete_id %in% player_stats_25$athlete_id)) %>%
  select(athlete_id) %>%
  distinct()

player_stats_24 <- map_dfr(ids_24$athlete_id, espn_mbb_player_stats, year = 2024, .progress = T)

ids_23 <- all_box %>%
  filter(season == 2023,
         team_id %in% ncaahoopR::ids$id
  ) %>%
  group_by(athlete_id) %>%
  filter(sum(minutes, na.rm = T) > 0,
         !(athlete_id %in% player_stats_25$athlete_id),
         !(athlete_id %in% player_stats_24$athlete_id)
         ) %>%
  select(athlete_id) %>%
  distinct()

player_stats_23 <- map_dfr(ids_23$athlete_id, espn_mbb_player_stats, year = 2023, .progress = T)

ids_22 <- all_box %>%
  filter(season == 2022,
         team_id %in% ncaahoopR::ids$id
  ) %>%
  group_by(athlete_id) %>%
  filter(sum(minutes, na.rm = T) > 0,
         !(athlete_id %in% player_stats_25$athlete_id),
         !(athlete_id %in% player_stats_24$athlete_id),
         !(athlete_id %in% player_stats_23$athlete_id)
  ) %>%
  select(athlete_id) %>%
  distinct()

player_stats_22 <- map_dfr(ids_22$athlete_id, espn_mbb_player_stats, year = 2022, .progress = T)

ids_21 <- all_box %>%
  filter(season == 2021,
         team_id %in% ncaahoopR::ids$id
  ) %>%
  group_by(athlete_id) %>%
  filter(sum(minutes, na.rm = T) > 0,
         !(athlete_id %in% player_stats_25$athlete_id),
         !(athlete_id %in% player_stats_24$athlete_id),
         !(athlete_id %in% player_stats_23$athlete_id),
         !(athlete_id %in% player_stats_22$athlete_id)
  ) %>%
  select(athlete_id) %>%
  distinct()
player_stats_21 <- map_dfr(ids_21$athlete_id, espn_mbb_player_stats, year = 2021, .progress = T)

ids_20 <- all_box %>%
  filter(season == 2020,
         team_id %in% ncaahoopR::ids$id
  ) %>%
  group_by(athlete_id) %>%
  filter(sum(minutes, na.rm = T) > 0,
         !(athlete_id %in% player_stats_25$athlete_id),
         !(athlete_id %in% player_stats_24$athlete_id),
         !(athlete_id %in% player_stats_23$athlete_id),
         !(athlete_id %in% player_stats_22$athlete_id),
         !(athlete_id %in% player_stats_21$athlete_id)
  ) %>%
  select(athlete_id) %>%
  distinct()

player_stats_20 <- map_dfr(ids_20$athlete_id, espn_mbb_player_stats, year = 2020, .progress = T)


player_stats_20_25 <- rbind(
  player_stats_25 %>% select(athlete_id, full_name, weight, height, weight, birth_place_country, headshot_href, jersey,
                             position_abbreviation, experience_years, active, general_per, team_id, team_location),
  player_stats_24 %>% select(athlete_id, full_name, weight, height, weight, birth_place_country, headshot_href, jersey,
                             position_abbreviation, experience_years, active, general_per, team_id, team_location),
  player_stats_23 %>% select(athlete_id, full_name, weight, height, weight, birth_place_country, headshot_href, jersey,
                             position_abbreviation, experience_years, active, general_per, team_id, team_location),
  player_stats_22 %>% select(athlete_id, full_name, weight, height, weight, birth_place_country, headshot_href, jersey,
                             position_abbreviation, experience_years, active, general_per, team_id, team_location),
  player_stats_21 %>% select(athlete_id, full_name, weight, height, weight, birth_place_country, headshot_href, jersey,
                             position_abbreviation, experience_years, active, general_per, team_id, team_location),
  player_stats_20 %>% select(athlete_id, full_name, weight, height, weight, birth_place_country, headshot_href, jersey,
                             position_abbreviation, experience_years, active, general_per, team_id, team_location)
)


# Save all Data
saveRDS(player_stats_20_25, file = "True 3pt Project/espn_mbb_player_info_20_25.rds")
saveRDS(player_stats_20, file = "True 3pt Project/espn_mbb_player_info_20.rds")
saveRDS(player_stats_21, file = "True 3pt Project/espn_mbb_player_info_21.rds")
saveRDS(player_stats_22, file = "True 3pt Project/espn_mbb_player_info_22.rds")
saveRDS(player_stats_23, file = "True 3pt Project/espn_mbb_player_info_23.rds")
saveRDS(player_stats_24, file = "True 3pt Project/espn_mbb_player_info_24.rds")
saveRDS(player_stats_25, file = "True 3pt Project/espn_mbb_player_info_25.rds")


# # Pull all roster data
# 
# get_rosters <- function(team, season) {
#   # print(paste0("https://raw.githubusercontent.com/lbenz730/ncaahoopR_data/master/",
#   #              season,"/rosters/", gsub(" ", "_", team), "_roster.csv"))
#   roster <- suppressWarnings(try(readr::read_csv(paste0("https://raw.githubusercontent.com/lbenz730/ncaahoopR_data/master/",
#                                                 season,"/rosters/", gsub(" ", "_", team), "_roster.csv"), show_col_types = F)))
#   if(any(class(roster) == 'try-error')) {
#     warning('No Roster Available')
#     return(NULL)
#   }
#   
#   return(roster)
# }
# 
# tmp <- get_rosters(ncaahoopR::ncaa_colors$espn_name[1], season = "2024-25")
# 
# rosters_25 <- map_dfr(ncaahoopR::ncaa_colors$espn_name, function(x) {
#   tmp <- get_rosters(team = x, season = "2024-25")
#   if(!is.null(tmp)) {
#     tmp$team <- x
#     tmp$season <- 2025
#   }
#   return(tmp)
# }, .progress = TRUE)

