#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#o

library(shiny)
library(hoopR)
library(tidyverse)
library(gt)
library(ggthemes)
library(gtExtras)
library(paletteer)
library(shinyscreenshot)

# Define UI for application that draws a histogram
ui <- fluidPage(
  # titlePanel("Team 3-Point Shooting Predictions"),
  tags$head(
    tags$style(HTML("
      body { background-color: #F2F3F4; }
      .top-bar {
        background-color: #2D68C4;
        color: #fff;
        padding: 18px 24px;
        margin-bottom: 16px;
        margin-top: 10px;
        border-radius: 6px;
      }
      .control-box {
        background-color: #F2F3F4;
        border: 1px solid #d9d9d9;
        border-radius: 6px;
        padding: 16px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
        margin-bottom: 18px;
      }
      .shiny-download-link { margin-top: 6px; }
    "))
  ),
  
  # Title bar
  fluidRow(
    column(
      width = 12, align = "center",
      div(class = "top-bar",
          h2("2025-2026 NCAA Player 3-Point Shooting Forecast", style = "margin: 0;")
      )
    )
  ),
  
  # Controls box
  fluidRow(
    column(
      width = 12,
      div(class = "control-box",
          fluidRow(
            column(
              width = 4, align = "center",
              selectInput("team", "Select Team:", choices = NULL, width = "60%")
            ),
            column(
              width = 4, align = "center",
              radioButtons("sort_by",
                           "Sort Table By:",
                           choices = list(
                             "Season 3PT Attempted" = "season_total_3pt_attempted",
                             "Career 3PT Attempts" = "career_3pt_attempts",
                             "Forecast 3PT %" = "season_stable_adjusted_3pt_pct",
                             "Confidence" = "confidence_score"
                           ),
                           selected = "season_total_3pt_attempted",
                           inline = F)
            ),
            # column(
            #   width = 4,
            #   br(),
            #   downloadButton("download_table", "Download table (PNG)", class = "btn-primary", width = "100%")
            # )
            column(
              width = 4,
              br(),
              actionButton("screenshot_btn", "Download table (PNG)", class = "btn-primary", width = "60%")
            )
          )
      )
    )
  ),
  
  # Table area, centered and widened
  # fluidRow(
  #   column(
  #     width = 12,
  #     div(style = "max-width: 1250px; margin: 0 auto;",
  #         gt_output("team_table")
  #     )
  #   )
  # ),
  # Table area, centered and widened - wrapped in a div with id for screenshot
  fluidRow(
    column(
      width = 12,
      div(id = "table_screenshot",
          style = "max-width: 1250px; margin: 0 auto; background-color: #F2F3F4;",
          gt_output("team_table")
      )
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 12,
      hr(),
      h3("Manual Forecast", align = "center"),
      p("For manual forecast, select a player from the dropdown and enter a new predicted 3PT%", 
        align = "center"),
      hr()
    )
  ),
  
  # Player override controls and mini table
  fluidRow(
    column(
      width = 12,
      div(class = "control-box",
          fluidRow(
            column(
              width = 6, align = "center",
              selectInput("player_select", "Select Player:", choices = NULL, width = "60%")
            ),
            column(
              width = 6, align = "center",
              numericInput(
                "player_pred_override",
                "Enter Predicted 3PT% (as decimal, e.g., 0.38):",
                value = NA, min = 0, max = 1, step = 0.001,
                width = "60%"
              )
            )
          ),
          gt_output("player_table")
      )
    )
  )
  
)

# Define Server
server <- function(input, output, session) {
  
  # Get 2026 Data
  
  team_dictionary <- read_rds("https://github.com/stlewis1/NCAA-3PT-Data/raw/main/team_dictionary_2026.rds")
  team_ids_2026 <- read_rds("https://github.com/stlewis1/NCAA-3PT-Data/raw/main/team_ids_2026.rds")
  current_season <- 2026
  
  data_26 <- load_mbb_player_box(seasons = current_season) %>%
    select(season, game_date, athlete_id, athlete_display_name, team_id,
           team_display_name, minutes, field_goals_made, field_goals_attempted,
           three_point_field_goals_made, three_point_field_goals_attempted,
           free_throws_made, free_throws_attempted, points, assists, turnovers,
           starter, did_not_play, team_score, athlete_jersey, athlete_position_abbreviation) %>%
    filter(did_not_play == FALSE, minutes > 0, season == current_season,
           team_id %in% team_ids_2026$team_id) %>%
    # Get data for each season
    group_by(athlete_id) %>%
    summarise(
      season_total_3pt_made = sum(three_point_field_goals_made, na.rm = TRUE),
      season_total_3pt_attempted = sum(three_point_field_goals_attempted, na.rm = TRUE),
    ) %>%
    mutate(across(where(is.numeric), ~replace_na(.x, 0)))
  
  # Update Predictions
  all_table_data <- read_rds("https://github.com/stlewis1/NCAA-3PT-Data/raw/main/predictions_2026.rds") %>%
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
    select(jersey_no, full_name, team_display_name, lag_career_3pt_attempts, lag_career_3pt_pct, 
           season_total_3pt_made, season_total_3pt_attempted, fg3_pct, predicted_stable_3pt_pct,
           season_stable_adjusted_3pt_pct, confidence_score, career_3pt_attempts, exp_year, 
           athlete_id, team_id, padding_adjust)
  
  rm(data_26)
  
  # all_table_data <- read_rds("https://github.com/stlewis1/NCAA-3PT-Data/raw/main/all_table_data.rds")
  team_dictionary <- read_rds("https://github.com/stlewis1/NCAA-3PT-Data/raw/main/team_dictionary_2026.rds")
  
  # Populate team dropdown
  observe({
    updateSelectInput(session, "team", choices = team_dictionary$Team)
  })
  
  # Reactive: raw team data for the selected team
  team_data_reactive <- reactive({
    req(input$team)
    selected_team_id <- team_dictionary %>%
      filter(Team == input$team) %>%
      pull(team_id)
    req(length(selected_team_id) == 1)
    all_table_data %>% filter(team_id == selected_team_id)
  })
  
  # Populate player dropdown based on team selection
  observeEvent(team_data_reactive(), {
    td <- team_data_reactive() %>% arrange(full_name)
    updateSelectInput(session, "player_select", choices = td$full_name, selected = first(td$full_name))
    
    # Default override value: use the selected player's predicted_stable_3pt_pct
    default_val <- td %>%
      filter(full_name == first(full_name)) %>%
      pull(predicted_stable_3pt_pct) %>%
      first()
    updateNumericInput(session, "player_pred_override", value = round(default_val, 3))
  })
  
  # Team 3-Point Table Function
  team_3pt_table <- function(team_id_input, sort_column) {
    bg_color = "#F2F3F4"
    ncaa_avg_3 <- 0.3405925
    na_image = "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop"
    
    table_data <- all_table_data %>%
      filter(team_id == team_id_input) %>%
      mutate(
        pic = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/", athlete_id, ".png&w=350&h=254"),
      ) 
    
    table_data %>%
      mutate(color = "",
             full_name = ifelse(exp_year == 1, paste0(full_name, "*"), full_name) # Add * for freshman
      ) %>%
      arrange(desc(!!sym(sort_column))) %>%
      select(jersey_no, full_name, pic, color, lag_career_3pt_attempts, lag_career_3pt_pct, 
             season_total_3pt_made, season_total_3pt_attempted, fg3_pct, predicted_stable_3pt_pct,
             season_stable_adjusted_3pt_pct, confidence_score) %>%
      gt() %>%
      text_transform(
        locations = cells_body(columns = c(pic)),
        # fn = function(img_urls) {
        #   vapply(img_urls, function(img) {paste0(
        #     "<div style='height:70px; display:flex; align-items:flex-end; justify-content:center;'>",
        #     gt::web_image(url = img, height = px(70)),"</div>"
        #   )}, character(1))
        fn = function(img_urls) {
          vapply(img_urls, function(img) {paste0(
            "<div style='height:70px; display:flex; align-items:flex-end; justify-content:center;'>",
            "<img src='", img, "' height='70' onerror=\"this.onerror=null;this.src='", na_image, "';\"/>",
            "</div>"
          )}, character(1))
        }) %>%
      cols_label(
        jersey_no = "#",
        full_name = "Player",
        pic = "", color = "",
        lag_career_3pt_attempts = "Career Att.",
        lag_career_3pt_pct = "Career %",
        season_total_3pt_made = "3PT Made",
        season_total_3pt_attempted = "3PT Att.",
        fg3_pct = "3PT%",
        predicted_stable_3pt_pct = "Prediction",
        season_stable_adjusted_3pt_pct = "Forecast %",
        confidence_score = "Confidence"
      ) %>%
      gt_theme_538(quiet = T) %>%
      gt_color_rows(
        columns = c(season_stable_adjusted_3pt_pct),
        palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    paletteer_c("ggthemes::Red-Green-Gold Diverging", 30),
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
        domain = c(0.3405925 - .22, 0.3405925 + .22),
      ) %>%
      gt_color_rows(
        columns = c(confidence_score),
        # palette = c(paletteer_c("ggthemes::Classic Blue", 30)),
        palette = c(paletteer_c("grDevices::Blues", 30)),
        direction = -1, domain = c(0,10),
      ) %>%
      data_color(
        target_columns = color,
        columns = c(season_stable_adjusted_3pt_pct),
        palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    paletteer_c("ggthemes::Red-Green-Gold Diverging", 30),
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
        domain = c(0.3405925 - .22, 0.3405925 + .22),
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
        locations = cells_body(columns = c(predicted_stable_3pt_pct, fg3_pct))
      ) %>%
      tab_header(
        title = html(
          web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 55),
          paste0("2025-26 ", first(table_data$team_display_name), " 3-Point Shooting Predictions"),
          web_image(paste0("https://a.espncdn.com/i/teamlogos/ncaa/500/", first(table_data$team_id),".png"), height = 55)
        ),
        subtitle = "* = 32.8% Prior Prediction"
      ) %>%
      tab_options(
        table.background.color = bg_color,
        table.align = "center",
        column_labels.background.color = bg_color,
        heading.align = "center",
        heading.background.color = bg_color,
        # data_row.padding.horizontal = px(4),
        )
  }
  
  # Render table based on user input
  # output$team_table <- render_gt({
  #   req(input$team, input$sort_by)
  #   
  #   selected_team_id <- team_dictionary %>% 
  #     filter(Team == input$team) %>% 
  #     pull(team_id)
  #   
  #   team_3pt_table(selected_team_id, input$sort_by)
  # })
  
  # Reactive table object for reuse (render + download)
  table_reactive <- reactive({
    req(input$team, input$sort_by)
    selected_team_id <- team_dictionary %>% 
      filter(Team == input$team) %>% 
      pull(team_id)
    req(length(selected_team_id) == 1)
    team_3pt_table(selected_team_id, input$sort_by)
  })
  
  # Render table
  output$team_table <- render_gt({
    table_reactive()
  })
  
  # Screenshot handler using shinyscreenshot
  observeEvent(input$screenshot_btn, {
    screenshot(
      id = "table_screenshot",
      filename = paste0(gsub("\\s+", "_", tolower(input$team)), "_team_3pt_table"),
      scale = 2
    )
  })
  
  # Mini player table with override
  player_table_reactive <- reactive({
    td <- team_data_reactive()
    req(nrow(td) > 0, input$player_select)
    player_row <- td %>% filter(full_name == input$player_select) %>% slice(1)
    
    override_val <- input$player_pred_override
    if (is.null(override_val) || is.na(override_val)) {
      override_val <- player_row$predicted_stable_3pt_pct
    }
    
    bg_color = "#F2F3F4"
    na_image = "https://a.espncdn.com/combiner/i?img=/i/headshots/nophoto.png&w=110&h=80&scale=crop"
    
    player_row %>%
      mutate(color = "",
             # full_name = ifelse(exp_year == 1, paste0(full_name, "*"), full_name), # Add * for freshman
             pic = paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/", athlete_id, ".png&w=350&h=254"),
             predicted_stable_3pt_pct = override_val,
             season_stable_adjusted_3pt_pct = (season_total_3pt_made + (padding_adjust*override_val)) / 
               (season_total_3pt_attempted + padding_adjust),
      ) %>%
      select(jersey_no, full_name, pic, color, lag_career_3pt_attempts, lag_career_3pt_pct,
             season_total_3pt_made, season_total_3pt_attempted, fg3_pct, predicted_stable_3pt_pct,
             season_stable_adjusted_3pt_pct, confidence_score) %>%
      gt() %>%
      text_transform(
        locations = cells_body(columns = c(pic)),
        fn = function(img_urls) {
          vapply(img_urls, function(img) {paste0(
            "<div style='height:70px; display:flex; align-items:flex-end; justify-content:center;'>",
            "<img src='", img, "' height='70' onerror=\"this.onerror=null;this.src='", na_image, "';\"/>",
            "</div>"
          )}, character(1))
        }) %>%
      cols_label(
        jersey_no = "#",
        full_name = "Player",
        pic = "", color = "",
        lag_career_3pt_attempts = "Career Att.",
        lag_career_3pt_pct = "Career %",
        season_total_3pt_made = "3PT Made",
        season_total_3pt_attempted = "3PT Att.",
        fg3_pct = "3PT%",
        predicted_stable_3pt_pct = "Prediction",
        season_stable_adjusted_3pt_pct = "Forecast %",
        confidence_score = "Confidence"
      ) %>%
      gt_theme_538(quiet = T) %>%
      gt_color_rows(
        columns = c(season_stable_adjusted_3pt_pct),
        palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    paletteer_c("ggthemes::Red-Green-Gold Diverging", 30),
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
        domain = c(0.3405925 - .22, 0.3405925 + .22),
      ) %>%
      gt_color_rows(
        columns = c(confidence_score),
        # palette = c(paletteer_c("ggthemes::Classic Blue", 30)),
        palette = c(paletteer_c("grDevices::Blues", 30)),
        direction = -1, domain = c(0,10),
      ) %>%
      data_color(
        target_columns = color,
        columns = c(season_stable_adjusted_3pt_pct),
        palette = c("#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF", "#BE2A3EFF",
                    paletteer_c("ggthemes::Red-Green-Gold Diverging", 30),
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF",
                    "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF", "#22763FFF"),
        domain = c(0.3405925 - .22, 0.3405925 + .22),
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
          sides = "right", color = "black", weight = px(1.5), style = "solid"
        ),
        locations = cells_body(columns = c(predicted_stable_3pt_pct, fg3_pct))
      ) %>%
      tab_options(
        table.background.color = bg_color,
        table.align = "center",
        column_labels.background.color = bg_color,
        heading.align = "center",
        heading.background.color = bg_color,
        # data_row.padding.horizontal = px(4),
      )
  })
  
  output$player_table <- render_gt({
    player_table_reactive()
  })
  
  
  # Download handler for PNG
  # output$download_table <- downloadHandler(
  #   filename = function() {
  #     paste0(gsub("\\s+", "_", tolower(input$team)), "_team_3pt_table", ". png")
  #   },
  #   content = function(file) {
  #     tbl <- table_reactive()
  #     
  #     # Save as HTML first, then convert to PNG using webshot (PhantomJS)
  #     tmp_html <- tempfile(fileext = ".html")
  #     gtsave(tbl, filename = tmp_html)
  #     
  #     # Use webshot with PhantomJS (works on shinyapps.io)
  #     webshot::webshot(url = tmp_html, file = file, vwidth = 1200, vheight = 800)
  #     
  #     # Clean up temp file
  #     unlink(tmp_html)
  #   }
  # )
  
}

# Run Application
shinyApp(ui = ui, server = server)


