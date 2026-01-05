library(tidyverse)
library(gt)
library(gtExtras)
library(paletteer)



# Function for table
team_3pt_table <- function(team_id_input) {
  bg_color = "#FFFFF0"
  
  table_data = all_table_data %>%
    filter(team_id == team_id_input) %>%
    mutate(
      pic = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/", athlete_id, ".png&w=350&h=254"),
    )
  
  table_data %>% 
    mutate(color = "",
           full_name = ifelse(exp_year == 1, paste0(full_name, "*"), full_name) # Add * for freshman
    ) %>%
    arrange(desc(career_3pt_attempts)) %>%
    select(jersey_no, full_name, pic, color, lag_career_3pt_attempts, lag_career_3pt_pct, season_total_3pt_made, season_total_3pt_attempted, fg3_pct, predicted_stable_3pt_pct,
           season_stable_adjusted_3pt_pct, confidence_score) %>%
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
      # lag_career_3pt_attempts = "Career Att. ⬇",
      lag_career_3pt_attempts = "Career Att.",
      lag_career_3pt_pct = "Career %",
      season_total_3pt_made = "3PT Made",
      season_total_3pt_attempted = "3PT Att.",
      fg3_pct = "3PT%",
      predicted_stable_3pt_pct = "Prediction",
      # ci_lower = "Lower Est.",
      season_stable_adjusted_3pt_pct = "Forecast %",
      confidence_score = "Confidence"
    ) %>%
    gt_theme_538(quiet = T) %>%
    gt_color_rows(
      columns = c(season_stable_adjusted_3pt_pct),
      palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                  paletteer_c("ggthemes::Red-Green-Gold Diverging", 30), 
                  "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
      domain = c(ncaa_avg_3 - .11, ncaa_avg_3 + .11),
    ) %>%
    gt_color_rows(
      columns = c(confidence_score),
      palette = c(paletteer_c("ggthemes::Red-Green-Gold Diverging", 30)),
      domain = c(0,10),
    ) %>%
    data_color(
      target_columns = color,
      columns = c(season_stable_adjusted_3pt_pct),
      palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                  paletteer_c("ggthemes::Red-Green-Gold Diverging", 30), 
                  "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
      domain = c(ncaa_avg_3 - .17, ncaa_avg_3 + .17),
    ) %>%
    fmt_percent(
      columns = c(lag_career_3pt_pct, fg3_pct, predicted_stable_3pt_pct, season_stable_adjusted_3pt_pct),
      decimals = 1
    ) %>%
    fmt_number(
      columns = c(confidence_score),
      decimals = 1
    ) %>%
    cols_align(align = "center", columns = everything()) %>% 
    cols_width(
      confidence_score ~ px(95), season_stable_adjusted_3pt_pct ~ px(95), predicted_stable_3pt_pct ~ px(100),
      lag_career_3pt_attempts ~ px(115), lag_career_3pt_pct ~ px(95), season_total_3pt_made ~ px(95), season_total_3pt_attempted ~ px(95), 
      fg3_pct ~ px(95)
    ) %>%
    tab_spanner(
      label = "Prior Season(s)",
      columns = c(lag_career_3pt_attempts, lag_career_3pt_pct, predicted_stable_3pt_pct)
    ) %>%
    tab_spanner(
      label = "Season",
      columns = c(season_total_3pt_made, season_total_3pt_attempted, fg3_pct)
    ) %>%
    tab_spanner(
      label = "Forecast",
      columns = c(season_stable_adjusted_3pt_pct, confidence_score)
    ) %>%
    tab_style(
      style = cell_borders(
        sides = "right",  # Add a right border to the specified columns
        color = "black",  # Color of the border
        weight = px(1.5),   # Thickness of the border
        style = "solid"   # Style of the border (e.g., "solid", "dashed")
      ),
      locations = cells_body(columns = c(predicted_stable_3pt_pct, fg3_pct)) # Apply to body cells of col1 and col2
    ) %>%
    tab_header(
      title = html(
        web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 39),
        paste0("2025-26 ", first(table_data$team_display_name), " 3-Point Shooting Predictions"),
        web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 39)
      ),
      subtitle = "* = 32.8% Prior Prediction"
    ) %>%
    tab_options(
      table.background.color = bg_color,
      table.align = "center",
      column_labels.background.color = bg_color,
      heading.align = "center",
      heading.background.color = bg_color,
      heading.title.font.size = 28,
      # table.margin.left = px(0),
      # table.margin.right = px(0),
    )
}

# Example Usage
# team_3pt_table(48)
team_3pt_table(8) #%>% gtsave("True 3pt Project/Predictions_2026_tst.html")
## Example Save
# team_3pt_table(48) %>% gtsave("True 3pt Project/Predictions_2026_de.png", vwidth = 1200, expand = 0, zoom = 3)

# predictions_2026 %>% 
#   select(predicted_stable_3pt_pct, career_3pt_pct) %>%
#   arrange(desc(predicted_stable_3pt_pct)) %>%
#   view()
# 
# model_data %>%
#   filter(career_3pt_attempts >= 100,
#          season == 2024,
#          exp_year > 1
#          ) %>%
#   select( full_name,
#     career_3pt_pct, lag_career_3pt_pct, fg3_pct, lag_fg3_pct
#   ) %>% view()

# Shift predicted to career column, Confidence score, sort by stable %
"#143d59"

