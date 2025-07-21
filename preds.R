xgb_predictions<- read.csv("xgb_predictions.csv")

players_ranked_u_clean <- read.csv("player_protection.csv")


library(dplyr)

combined_data <- players_ranked_u_clean |>
  left_join(xgb_predictions, by = "player")

