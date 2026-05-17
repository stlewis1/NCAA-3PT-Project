# File to scrape ESPN Net points

# Other action_type: "3ptShooting", 
mbb_season_net_pts <- function(conf_games = 0, season = 2026, game_code_min = "20251001", game_code_max = "20260517", action_type = "total") {
  library(httr2)
  library(jsonlite)
  
  # url <- "https://qb9urbishl.execute-api.us-east-1.amazonaws.com/prod/mcbb_award_leaderboardv2?game_code_min=20251001&game_code_max=20260517&action_type=total&conf_games=1"
  url <- "https://qb9urbishl.execute-api.us-east-1.amazonaws.com/prod/mcbb_award_leaderboardv2?"
  
  params <- list(
    game_code_min = game_code_min,
    game_code_max = game_code_max,
    # season = as.character(season - 1),
    action_type = action_type,
    conf_games = as.character(conf_games)
  )
  
  resp <- request(url) |>
    req_url_query(!!!params) |>
    req_headers(
      accept = "application/json, text/plain, */*",
      `accept-language` = "en-US,en;q=0.9",
      origin = "https://espnanalytics.com",
      referer = "https://espnanalytics.com/",
      `user-agent` = "Mozilla/5.0 (compatible; R; httr2)",
      `x-api-key` = "x83lwhgUX58rTljeptg1n5yL0UUVq7wt9oVdPATF"
    ) |>
    req_perform()
  
  
  # txt <- resp_body_string(resp)
  json <- fromJSON(resp_body_string(resp), flatten = TRUE)
  
  json <- json %>%
    mutate(
      season = season,
      conf_only = ifelse(conf_games == 1, TRUE, FALSE)
    )
  
  return(json)
  
}


# mbb_net <- mbb_season_net_pts()
# saveRDS(mbb_net, "True 3pt Project/MBB Net Points 2026.RDS")

# mbb_net <- mbb_season_net_pts(action_type = "3ptShooting")
# saveRDS(mbb_net, "True 3pt Project/MBB 3pt Net Points 2026.RDS")
