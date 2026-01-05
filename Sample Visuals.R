library(hoopR)
library(tidyverse)

# GT table for any team
library(gt)
library(gtExtras)
library(paletteer)

# Create Table (turned into function later)
# table_data = predictions_2026 %>%
#   filter(team_id == 8) %>%
#   mutate(
#     pic = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/", athlete_id, ".png&w=350&h=254"),
#     # na image = https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop
#   )
#   
# table_data %>% 
#   select(full_name, pic, career_3pt_attempts, career_3pt_pct, total_3pt_made, total_3pt_attempted, fg3_pct, predicted_stable_3pt_pct, 
#          lower_est, season_stable_adjusted_3pt_pct, upper_est) %>%
#   arrange(desc(total_3pt_attempted)) %>%
#   gt() %>%
#   # gt_img_rows(pic, height = 50) %>%
#   text_transform(
#     locations = cells_body(columns = c(pic)),
#     fn = function(img_urls) {
#       vapply(img_urls, function(img) {paste0(
#         "<div style='height:70px; display:flex; align-items:flex-end; justify-content:center;'>",
#         gt::web_image(url = img, height = px(70)),"</div>" )}, character(1))}) %>%
#   cols_label(
#     full_name = "Player",
#     pic = "",
#     career_3pt_attempts = "Career 3PT Att.",
#     career_3pt_pct = "Career 3PT%",
#     total_3pt_made = "3PT Made",
#     total_3pt_attempted = "3PT Att.",
#     fg3_pct = "3PT %",
#     predicted_stable_3pt_pct = "Predicted Stable 3PT%",
#     lower_est = "Lower Est.",
#     season_stable_adjusted_3pt_pct = "Stable 3PT%",
#     upper_est = "Upper Est."
#   ) %>%
#   gt_theme_538(quiet = T) %>%
#   gt_color_rows(
#     columns = c(lower_est, season_stable_adjusted_3pt_pct, upper_est),
#     palette = c(paletteer_c("ggthemes::Red-Green-Gold Diverging", 30)),
#     domain = c(ncaa_avg_3 - .155, ncaa_avg_3 + .155),
#     # type = "continuous"
#   ) %>%
#   fmt_percent(
#     columns = c(career_3pt_pct, fg3_pct, predicted_stable_3pt_pct, lower_est, season_stable_adjusted_3pt_pct, upper_est),
#     decimals = 1
#   ) %>%
#   cols_align(align = "center", columns = everything()) %>% 
#   cols_width(
#     lower_est ~ px(80), upper_est ~ px(80), season_stable_adjusted_3pt_pct ~ px(80),
#   ) %>%
#   tab_header(
#     # title = "2025-26 Delaware Blue Hens 3-Point Shooting Predictions",
#     title = html(
#       web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 35),
#       paste0("2025-26 ", first(table_data$team_display_name), " 3-Point Shooting Predictions"),
#       web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 35)
#     ),
#     subtitle = "With 95% Prediction Intervals"
#   ) %>%
#   tab_options(
#     table.background.color = "#FFFFF0",
#     # table.align = "center",
#     column_labels.background.color = "#FFFFF0",
#     heading.align = "center",
#     heading.background.color = "#FFFFF0",
#     heading.title.font.size = 24,
#     # heading.subtitle.font.size = 14,
#     # table.font.color = "#343434",
#     # table.margin.left = px(0),
#     # table.margin.right = px(0),
#   )

# Function for any team
team_3pt_table <- function(team_id_input) {
  # load  packages if needed
  library(gt)
  library(gtExtras)
  library(paletteer)
  
  # Add player pictures 
  table_data = predictions_2026 %>%
    filter(team_id == team_id_input) %>%
    mutate(
      pic = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/", athlete_id, ".png&w=350&h=254"),
      # For later (add try fail image): na image = https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop
    )
  
  table_data %>% 
    mutate(color = "",
           full_name = ifelse(exp_year == 1, paste0(full_name, "*"), full_name) # Add * for freshman
           ) %>%
    select(jersey_no, full_name, pic, color, career_3pt_attempts, career_3pt_pct, total_3pt_made, total_3pt_attempted, fg3_pct, predicted_stable_3pt_pct,
           ci_lower, season_stable_adjusted_3pt_pct, ci_upper) %>%
    arrange(desc(total_3pt_attempted)) %>%
    gt() %>%
    text_transform(
      locations = cells_body(columns = c(pic)),
      fn = function(img_urls) {
        vapply(img_urls, function(img) {paste0(
          "<div style='height:70px; display:flex; align-items:flex-end; justify-content:center;'>",
          gt::web_image(url = img, height = px(70)),"</div>" )}, character(1))}) %>%
    cols_label(
      jersey_no = "#",
      full_name = "Player",
      pic = "", color = "",
      career_3pt_attempts = "Career Att.",
      career_3pt_pct = "Career 3PT%",
      total_3pt_made = "3PT Made",
      total_3pt_attempted = "3PT Att. ⬇",
      fg3_pct = "3PT%",
      predicted_stable_3pt_pct = "Predicted %",
      ci_lower = "Lower Est.",
      season_stable_adjusted_3pt_pct = "Stable Updated %",
      ci_upper = "Upper Est."
    ) %>%
    gt_theme_538(quiet = T) %>%
    gt_color_rows(
      columns = c(ci_lower, season_stable_adjusted_3pt_pct, ci_upper),
      palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                  paletteer_c("ggthemes::Red-Green-Gold Diverging", 30), 
                  "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
      domain = c(ncaa_avg_3 - .155, ncaa_avg_3 + .155),
    ) %>%
    data_color(
      target_columns = color,
      columns = c(season_stable_adjusted_3pt_pct),
      palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                  paletteer_c("ggthemes::Red-Green-Gold Diverging", 30), 
                  "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
      domain = c(ncaa_avg_3 - .155, ncaa_avg_3 + .155),
    ) %>%
    # gt_color_rows(
    #   columns = c(total_3pt_attempted),
    #   palette = c(paletteer_c("grDevices::Greens 3", 30)), direction = -1
    # ) %>%
    fmt_percent(
      columns = c(career_3pt_pct, fg3_pct, predicted_stable_3pt_pct, ci_lower, season_stable_adjusted_3pt_pct, ci_upper),
      decimals = 1
    ) %>%
    cols_align(align = "center", columns = everything()) %>% 
    cols_width(
      ci_lower ~ px(95), ci_upper ~ px(95), season_stable_adjusted_3pt_pct ~ px(95), predicted_stable_3pt_pct ~ px(100),
      career_3pt_attempts ~ px(95), career_3pt_pct ~ px(95), total_3pt_made ~ px(95), total_3pt_attempted ~ px(95), 
      fg3_pct ~ px(95)
    ) %>%
    tab_spanner(
      label = "Career",
      columns = c(career_3pt_attempts, career_3pt_pct)
    ) %>%
    tab_spanner(
      label = "Season",
      columns = c(total_3pt_made, total_3pt_attempted, fg3_pct)
    ) %>%
    tab_spanner(
      label = "Prediction",
      columns = c(predicted_stable_3pt_pct, ci_lower, season_stable_adjusted_3pt_pct, ci_upper)
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "right",
        color = "black",
        weight = px(1), 
        style = "solid"
      ),
      locations = cells_body(columns = c(career_3pt_pct, fg3_pct)) 
    ) %>%
    tab_header(
      title = html(
        web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 35),
        paste0("2025-26 ", first(table_data$team_display_name), " 3-Point Shooting Predictions"),
        web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 35)
      ),
      subtitle = "* = 32.8% Prior Prediction"
    ) %>%
    tab_options(
      # data_row.padding = px(3),
      table.background.color = "#FFFFF0",
      table.align = "left",
      column_labels.background.color = "#FFFFF0",
      heading.align = "center",
      heading.background.color = "#FFFFF0",
      heading.title.font.size = 24,
      # heading.subtitle.font.size = 14,
      # table.font.color = "#343434",
      table.margin.left = px(0),
      table.margin.right = px(0),
    )
}


team_3pt_table(2509)

team_3pt_table(97)
team_3pt_table(97) %>% gtsave("True 3pt Project/Predictions_2026_lv.png", vwidth = 1200, expand = 0, zoom = 3)
team_3pt_table(97) %>% gtsave("True 3pt Project/Predictions_2026_lv2.html")


team_3pt_table(48)
team_3pt_table(48) %>% gtsave("True 3pt Project/Predictions_2026_de.png", vwidth = 1200, expand = 0, zoom = 3)

###########################################################################################
# GT Table for Variable Importance 
###########################################################################################
var_imp_table <- as.data.frame(importance(rf1)) %>%
  rownames_to_column(var = "Variable") %>%
  arrange(desc(`%IncMSE`)) %>%
  mutate(
    Variable = case_when(
      Variable == "lag_career_3pt_attempts" ~ "Career 3PT Attempts",
      Variable == "lag_fg3_pct" ~ "FG3%",
      Variable == "lag_stable_3pt_pct" ~ "Stable 3PT%",
      Variable == "lag_pct_shots_3pt" ~ "3PT Attempt Rate",
      Variable == "lag_career_stable_3pt" ~ "Career Stable 3PT%",
      Variable == "lag_fg_att_per_game" ~ "FG Att. Per Game",
      Variable == "lag_mpg" ~ "Minutes Per Game",
      Variable == "lag_career_stable_ft	" ~ "Career Stable FT%",
      Variable == "lag_career_minutes	" ~ "Career Minutes",
      Variable == "lag_ft_att_per_game" ~ "FT Att. Per Game",
      TRUE ~ Variable
    )
  )

var_imp_table %>%
  select(Variable, `%IncMSE`) %>%
  mutate(`%IncMSE` = `%IncMSE` / 100) %>%
  gt_preview(top_n = 8, bottom_n = 5) %>%
  cols_label(
    Variable = "Variable",
    `%IncMSE` = "% Increase in Error",
  ) %>%
  gt_plt_bar(
    column = `%IncMSE`, keep_column = F,
    color = "#1F77B4",
    scale_type = "percent",
  ) %>%
  # gt_plt_bar_pct(
  #   column = `%IncMSE`,
  #   scaled = F
  #   # color = "#1F77B4",
  # ) %>%
  gt_theme_538(quiet = T) %>%
  # fmt_number(
  #   columns = c(`%IncMSE`),
  #   decimals = 2
  # ) %>%
  # fmt_percent(
  #   columns = c(`%IncMSE`),
  #   decimals = 2
  # ) %>%
  # gt_color_rows(
  #   columns = c(`%IncMSE`),
  #   palette = c(paletteer_dynamic("cartography::green.pal", 20)),
  #   domain = c(0,0.5),
  cols_align(align = "center", columns = everything()) %>% 
  tab_header(
    title = "Model Variable Importance"
  ) %>%
  tab_options(
    table.background.color = "#FFFFF0",
    table.align = "center",
    column_labels.background.color = "#FFFFF0",
    heading.align = "center",
    heading.background.color = "#FFFFF0",
    heading.title.font.size = 20,
  )


###########################################################################################
# Plot for Sample Player % Change
###########################################################################################

all_box %>%
  filter(minutes > 0, season == 2025) %>%
  group_by(athlete_id) %>%
  summarise(
    athlete_display_name = first(athlete_display_name),
    seasons = n_distinct(season),
    first_season = min(season),
    last_season = max(season),
    last_team = first(team_display_name),
    total_3pt_made = sum(three_point_field_goals_made, na.rm = T),
    total_3pt_attempted = sum(three_point_field_goals_attempted, na.rm = T),
    fg3_pct = total_3pt_made / total_3pt_attempted,
    .groups = 'drop'
  ) %>% view()

# Select 4433176
  
  
player_cumulative_3pt <- all_box %>%
  filter(minutes > 0, athlete_id == 4895737) %>%
  filter(season == 2025) %>%
  arrange(game_date) %>%
  mutate(
    game = row_number(),
    cum_3pt_made = cumsum(three_point_field_goals_made),
    cum_3pt_attempted = cumsum(three_point_field_goals_attempted),
    cum_fg3_pct = cum_3pt_made / cum_3pt_attempted,
    stable_3pt_pct = (cum_3pt_made + ((0.37)*padding_3pt_szn)) / (cum_3pt_attempted + padding_3pt_szn),
    stable_3pt_pct_avg = (cum_3pt_made + ((ncaa_avg_3)*padding_3pt_szn)) / (cum_3pt_attempted + padding_3pt_szn),
    end_3pt_pct = sum(three_point_field_goals_made, na.rm = T) / sum(three_point_field_goals_attempted, na.rm = T),
    # total_3pt_made = sum(three_point_field_goals_made, na.rm = T),
    # total_3pt_attempted = sum(three_point_field_goals_attempted, na.rm = T),
    # fg3_pct = total_3pt_made / total_3pt_attempted,
  ) 

# Line plot of cumulative 3pt% and stable 3pt%
ggplot(player_cumulative_3pt, aes(x = cum_3pt_attempted)) +
  geom_line(aes(y = cum_fg3_pct, color = "Cumulative 3PT%"), linewidth = .8) +
  geom_line(aes(y = stable_3pt_pct_avg, color = "Stable 3PT% (Prior: NCAA Avg.)"), linewidth = .8) +
  geom_hline(yintercept = player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)], linetype = "solid", color = "black") +
  # annotate("text", x = max(player_cumulative_3pt$cum_3pt_attempted)*0.8, 
  #          y = player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)] + 0.02, 
  #          label = paste0("Final 3PT%: ", round(player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)]*100,1), "%"),
  #          color = "black") +
  scale_color_manual(values = c("Cumulative 3PT%" = "#0c2340", "Stable 3PT% (Prior: NCAA Avg.)" = "#e4002b")) +
  ggimage::geom_image(data = ~ subset(.x, game == 1), aes(image = "https://a.espncdn.com/i/headshots/mens-college-basketball/players/full/4895737.png"),
                      x = 220, y = 0.55, size = 0.2, asp = 16/9) +
  labs(
    # title = paste0(player_cumulative_3pt$athlete_display_name[1], " Cumulative vs. Stable 3-Point Percentage (24-25)"),
    title = paste0("<img src='https://a.espncdn.com/i/teamlogos/ncaa/500/41.png' height='26' style='vertical-align: top; font-size:20pt;' />",
                   player_cumulative_3pt$athlete_display_name[1], " Cumulative vs. Stable 3-Point Percentage (24-25)",
                   "<img src='https://a.espncdn.com/i/teamlogos/ncaa/500/41.png' height='26' style='vertical-align: top; font-size:20pt;' />"),
    subtitle = paste0("Final Season 3-Point Percentage: ", round(player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)]*100,1), "%"),
    x = "Attempted 3-Point Shots",
    y = "3-Point Percentage",
    color = "Legend"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(.35,.4,.45,.5,.55)) +
  ggthemes::theme_wsj() +
  theme(
    plot.title = ggtext::element_markdown(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
    strip.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
    plot.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
    panel.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
  )

ggsave("True 3pt Project/Sample Player Stable 3pt_1.png", width = 10, height = 6)  



# Line plot of cumulative 3pt% and stable 3pt% with new prior
ggplot(player_cumulative_3pt, aes(x = cum_3pt_attempted)) +
  geom_line(aes(y = cum_fg3_pct, color = "Cumulative 3PT%"), linewidth = .8) +
  geom_line(aes(y = stable_3pt_pct_avg, color = "#7C878E"), linewidth = .8) +
  geom_line(aes(y = stable_3pt_pct, color = "Stable 3PT% (Prior: 37%)"), linewidth = .8) +
  geom_hline(yintercept = player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)], linetype = "solid", color = "black") +
  # annotate("text", x = max(player_cumulative_3pt$cum_3pt_attempted)*0.8, 
  #          y = player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)] + 0.02, 
  #          label = paste0("Final 3PT%: ", round(player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)]*100,1), "%"),
  #          color = "black") +
  scale_color_manual(values = c("Cumulative 3PT%" = "#0c2340", "Stable 3PT% (Prior: NCAA Avg.)" = "#7C878E", "Stable 3PT% (Prior: 37%)" = "#e4002b")) +
  ggimage::geom_image(data = ~ subset(.x, game == 1), aes(image = "https://a.espncdn.com/i/headshots/mens-college-basketball/players/full/4895737.png"),
                      x = 220, y = 0.55, size = 0.2, asp = 16/9) +
  labs(
    # title = paste0(player_cumulative_3pt$athlete_display_name[1], " Cumulative vs. Stable 3-Point Percentage (24-25)"),
    title = paste0("<img src='https://a.espncdn.com/i/teamlogos/ncaa/500/41.png' height='26' style='vertical-align: top; font-size:20pt;' />",
                   player_cumulative_3pt$athlete_display_name[1], " Cumulative vs. Stable 3-Point Percentage (24-25)",
                   "<img src='https://a.espncdn.com/i/teamlogos/ncaa/500/41.png' height='26' style='vertical-align: top; font-size:20pt;' />"),
    subtitle = paste0("Final Season 3-Point Percentage: ", round(player_cumulative_3pt$end_3pt_pct[nrow(player_cumulative_3pt)]*100,1), "%"),
    x = "Attempted 3-Point Shots",
    y = "3-Point Percentage",
    color = "Legend"
  ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), breaks = c(.35,.4,.45,.5,.55)) +
  ggthemes::theme_wsj() +
  theme(
    plot.title = ggtext::element_markdown(hjust = 0.5, size = 16, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
    strip.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
    plot.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
    panel.background = element_rect(fill = "#FFFFF0", color = "#FFFFF0"),
  )

ggsave("True 3pt Project/Sample Player Stable 3pt_2.png", width = 10, height = 6)  
