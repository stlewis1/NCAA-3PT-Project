library(tidyverse)
# library(tree)
library(randomForest)
set.seed(89732649)


###########################################################################################
# Model Data
###########################################################################################
colnames(model_data)

tree_data <- model_data %>% filter(
  last_season >= 2020,
  season >= 2020,
  total_3pt_attempted >= 15,
  exp_year > 1,
  max_seasons >= 2,
  lag_career_3pt_attempts >= 20,
  !is.na(fg3_pct),
  !is.na(lag_ft_pct),
  !is.na(lag_fg3_pct)
)

# Show na counts for tree data
sapply(tree_data, function(x) sum(is.na(x)))

train <- sample(1: nrow(tree_data), nrow(tree_data)*0.75)
test <- (1:nrow(tree_data))[-train] 

# Number of players
n_distinct(tree_data$athlete_id)


###########################################################################################
# Decision Tree (Test)
###########################################################################################
tree1 <- tree(stable_3pt_pct ~ as.factor(position_abbreviation) + changed_teams + lag_starts + lag_games + lag_mpg +
               lag_fg_att_per_game + lag_ft_att_per_game + lag_pts_g + lag_ast_g + lag_tov_g + lag_ft_pct + lag_efg_pct +
               lag_stable_3pt_pct + lag_pct_shots_3pt + lag_ast_tov_shots_40 + lag_career_games + lag_career_minutes +
               lag_career_stable_3pt + lag_fg3_pct +  lag_career_3pt_attempts + lag_career_3pt_pct + lag_career_3pt_attempts, 
             tree_data, subset = train)

summary(tree1)

plot(tree1)
text(tree1, pretty =0, cex=1.5)

# Calculate test rmse
tree_pred <- predict(tree1, tree_data[test, ])
sqrt(mean((tree_data$stable_3pt_pct[test] - tree_pred)^2))*100

###########################################################################################
# Random Forrest
###########################################################################################

set.seed(89732649)
rf1 <- randomForest(fg3_pct ~ position + changed_teams + 
                      # Last Season Stats
                      lag_starts + lag_games + lag_mpg + lag_fg_att_per_game + lag_fg3_pct +
                      lag_ft_att_per_game + lag_pts_g + lag_ast_g + lag_tov_g + lag_stable_ft_pct + lag_efg_pct +
                      lag_stable_3pt_pct + lag_pct_shots_3pt + lag_ast_tov_shots_40 + lag_3pt_attempted +
                      # Career Stats
                      lag_career_starts + lag_career_games + lag_career_mpg + lag_career_minutes + lag_career_stable_3pt + 
                      lag_career_stable_ft + lag_career_3pt_made + lag_career_3pt_attempts + lag_career_efg_pct +
                      lag_career_3pt_pct + lag_career_pts_g + lag_career_ast_g + lag_career_tov_g + 
                      lag_career_pct_shots_3pt,
                    data = tree_data,  mtry = 9, importance = TRUE, ntree = 500)
rf1
# lag_career_mpg, lag_3pt_attempted, lag_career_efg_pct
# Save random forest model
# saveRDS(rf1, file = "True 3pt Project/rf_model_3pt_pct.rds")
# rf1 <- readRDS("True 3pt Project/rf_model_3pt_pct.rds")


# Variable Importance
varImpPlot(rf1)
importance(rf1)

# OOB RMSE
sqrt(rf1$mse[length(rf1$mse)])*100
sqrt(mean((rf1$predicted - tree_data$fg3_pct)^2) )*100
plot(rf1)     

# Test RMSE
# rf_pred <- predict(rf1, tree_data[test, ])
# sqrt(mean((tree_data$stable_3pt_pct[test] - rf_pred)^2))*100


# Grid Search for mtry
oob_err <- vector()
for (mtry in 7:22) {
  fit <- randomForest(fg3_pct ~ position + changed_teams + 
                        # Last Season Stats
                        lag_starts + lag_games + lag_mpg + lag_fg_att_per_game + lag_fg3_pct +
                        lag_ft_att_per_game + lag_pts_g + lag_ast_g + lag_tov_g + lag_stable_ft_pct + lag_efg_pct +
                        lag_stable_3pt_pct + lag_pct_shots_3pt + lag_ast_tov_shots_40 + lag_3pt_attempted +
                        # Career Stats
                        lag_career_starts + lag_career_games + lag_career_mpg + lag_career_minutes + lag_career_stable_3pt + 
                        lag_career_stable_ft + lag_career_3pt_made + lag_career_3pt_attempts + lag_career_efg_pct +
                        lag_career_3pt_pct + lag_career_pts_g + lag_career_ast_g + lag_career_tov_g + 
                        lag_career_pct_shots_3pt,
                      data = tree_data,  mtry = mtry, ntree = 500)
  oob_err[mtry] <- sqrt(fit$mse[length(fit$mse)])*100
  rm(fit)
  cat("Done with mtry =", mtry, "\n")
}

oob_err_df <- data.frame(
  mtry = 7:22,
  oob_rmse = oob_err[7:22]
)
view(oob_err_df)
plot(oob_err_df$mtry, oob_err_df$oob_rmse, type = "b",
     xlab = "mtry", ylab = "OOB RMSE", main = "OOB RMSE vs mtry")
# Best: 7,4,6,5,13

# Shap Analysis (incomplete)
# library(treeshap)
# ts <- treeshap(rf1, tree_data %>% select(-stable_3pt_pct))
# plot(ts, type = "beeswarm")


# Plot residuals vs lag_career_3pt_attempts
tree_data <- tree_data %>%
  mutate(
    rf_pred = predict(rf1, newdata = tree_data),
    residuals = fg3_pct - rf_pred
  )

ggplot(tree_data, aes(x = lag_career_3pt_attempts, y = residuals)) +
  geom_point(alpha = 0.3) +
  geom_hline(yintercept = 0, color = "red", linewidth = 1.8, linetype = "dashed") +
  geom_vline(xintercept = 50, color = "blue", linewidth = 1.2, linetype = "dashed") +
  # have labels for every 50
  scale_x_continuous(breaks = seq(0, max(tree_data$lag_career_3pt_attempts, na.rm = TRUE), by = 100)) +
  labs(
    title = "Residuals vs Career 3PT Attempts",
    x = "Career 3PT Attempts (Lagged)",
    y = "Residuals (Actual - Predicted Stable 3PT%)"
  ) 

ggsave("True 3pt Project/Residuals_vs_Career3PTAttempts2.png", width = 10, height = 6)

hist(tree_data$total_3pt_attempted)
# Histogram of residuals
ggplot(tree_data, aes(x = residuals)) +
  geom_histogram(binwidth = 0.005, fill = "blue", color = "black", alpha = 0.7) +
  labs(
    title = "Histogram of Residuals",
    x = "Residuals (Actual - Predicted Stable 3PT%)",
    y = "Frequency"
  ) +
  theme_minimal()

# Find proportion of resuduals within +/- 0.03
tree_data %>%
  filter(abs(residuals) >= 0.05,
         lag_career_3pt_attempts > 200
         ) %>%
  nrow() / nrow(tree_data)


# Plot the proportion of resuduals mmore than +/- 0.03 by career 3pt attempts bins
tree_data %>%
  mutate(
    attempts_bin = case_when(
      lag_career_3pt_attempts < 25 ~ "1. (<25)",
      lag_career_3pt_attempts >= 25 & lag_career_3pt_attempts < 100 ~ "2. (25-100)",
      lag_career_3pt_attempts >= 100 & lag_career_3pt_attempts < 200 ~ "3. (100-199)",
      lag_career_3pt_attempts >= 200 & lag_career_3pt_attempts < 300 ~ "4. (200-299)",
      lag_career_3pt_attempts >= 300 ~ "5. (300+)"
    )
  ) %>%
  group_by(attempts_bin) %>%
  summarise(
    total_players = n(),
    high_residuals = sum(abs(residuals) >= 0.05),
    proportion_high_residuals = high_residuals / total_players
  ) %>%
  ggplot(aes(x = attempts_bin, y = proportion_high_residuals)) +
  geom_bar(stat = "identity", fill = "orange", color = "black", alpha = 0.7) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Proportion of High Residuals (+/- 0.05) by Career 3PT Attempts Bins",
    x = "Career 3PT Attempts Bins (Lagged)",
    y = "Proportion of Residuals >= +/- 0.05"
  ) +
  theme_minimal()

# Find the mean residuals by career 3pt attempts bins (bin size = 10)

tree_data %>%
  mutate(
    attempts_bin = cut(lag_career_3pt_attempts, breaks = seq(0, max(lag_career_3pt_attempts, na.rm = TRUE) + 10, by = 10), right = FALSE),
    bin_start_num = as.numeric(sub("\\[(\\d+),.*", "\\1", attempts_bin)) + 5
  ) %>%
  group_by(attempts_bin, bin_start_num) %>%
  summarise(
    mean_residual = mean(abs(residuals), na.rm = TRUE),
    pct_25 = quantile(abs(residuals), 0.25, na.rm = TRUE),
    pct_75 = quantile(abs(residuals), 0.75, na.rm = TRUE),
    total_players = n(),
    .groups = "drop"
  ) %>% #view()
  filter(total_players >= 5) %>% 
  ggplot(aes(x = bin_start_num, y = mean_residual)) +
  geom_line(color = "purple", size = 1.2) +
  geom_point(aes(size = total_players / 100)) +
  # Add error bars for 25th and 75th percentiles
  geom_errorbar(aes(ymin = pct_25, ymax = pct_75), width = 2, color = "gray50", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, max(tree_data$lag_career_3pt_attempts, na.rm = TRUE), by = 50)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Mean abs. Residuals by Career 3PT Attempts Bins (Size = 10)",
    x = "Career 3PT Attempts Bins (Lagged)",
    y = "Mean Residuals abs(Actual - Predicted Stable 3PT%)"
  ) 
ggsave("True 3pt Project/MeanResiduals_vs_Career3PTAttempts.png", width = 10, height = 6)
