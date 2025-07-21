c_years<- read_excel("data/contracts.xlsx")

c_years <- c_years %>% select(player, contract_y)

salary_player_wide <- all_advanced %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  select(player, team, year, per) %>%
  pivot_wider(
    names_from = year,
    values_from = c (per)
  )

salary_players_wide <- players_wide %>%
  mutate(
    per_2023 = as.numeric(per_2023),
    per_2024 = as.numeric(per_2024),
    per_2025 = as.numeric(per_2025)
  )

weighted_mean_ignore_na <- function(values, weights) {
  # Remove NAs
  keep <- !is.na(values)
  if (sum(keep) == 0) {
    return(NA_real_)  # All values missing
  }
  sum(values[keep] * weights[keep]) / sum(weights[keep])
}


salary_players_weighted <- players_wide %>%
  rowwise() %>%
  mutate(
    weighted_per = weighted_mean_ignore_na(
      c(per_2023, per_2024, per_2025),
      c(0.3, 0.5, 0.2)
    )
  ) %>%
  ungroup()


salary_players_weighted_summarised <- salary_players_weighted %>%
  group_by(player) %>%
  summarise(
    weighted_per = mean(weighted_per, na.rm = TRUE),
    team = last(team)
  ) %>%
  ungroup()

salary_players_weighted_summarised <- salary_players_weighted_summarised %>%
  mutate(
    player = str_squish(player),         # Remove extra spaces
    player = str_to_title(player)        # Convert to Title Case ("Aari Mcdonald")
  )


c_years <- c_years %>%
  mutate(
    player = str_squish(player),
    player = str_to_title(player)
  )


salary_players_weighted_summarised <- salary_players_weighted_summarised %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))


c_years <- c_years %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))

age <- read_csv("data/age2.csv")
age <- age %>%
  rename(player = PLAYER) %>%  # Rename first
  mutate(
    player = stri_trans_general(player, "Latin-ASCII"),
    player = str_trim(player),
    player = str_to_lower(player),
    player = str_replace_all(player, "-", " "),  # Replace hyphens
    player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2")
  )



salary_players_weighted_final <- salary_players_weighted_summarised %>%
  right_join(c_years, by = "player")



model_data_s <- salary_players_weighted_final %>%
  right_join(age, by = "player") 
  
model_data_s <- model_data_s %>% right_join(salary, by="player")




library(stringr)
# making a df with just players, name, year, and weighted per
per_df <- salary_stats |>
  select(player, team, pos, year, `Protection Status`, Salary) |>
  rename(protection_status = `Protection Status`, 
         salary = Salary) |>
  left_join(total_weighted_per, by = "player") |>
  mutate(
    player = player |> 
      str_to_lower() |>              # make lowercase 
      stringi::stri_trans_general("Latin-ASCII") ) # remove accents 












model_data <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  select(player, team, age, weighted_per_total, salary, salary_share)









xg_train1 <- model_data |>
  select(age, weighted_per_total) |>
  as.matrix()

yg_train1 <- model_data$salary  # continuous target variable

# Create hyperparameter grid
xg_grid1 <- crossing(
  nrounds = seq(20, 150, 10),
  eta = c(0.01, 0.05, 0.1),
  gamma = 0,
  max_depth = c(2, 3, 4),
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Set seed and tune
set.seed(1234)
xg_tune1 <- train(
  x = xg_train1,
  y = yg_train1,
  tuneGrid = xg_grid1,
  trControl = trainControl(method = "cv", number = 5),
  method = "xgbTree",
  metric = "RMSE"
)


# fit final model  data
xg_fit <- xgboost(
  data = xg_train1,
  label = yg_train1,
  objective = "reg:squarederror",
  nrounds = xg_tune1$bestTune$nrounds,
  params = as.list(select(xg_tune1$bestTune, -nrounds)),
  verbose = 0
)

library(vip)
xg_fit |> 
  vip()


#predicted_salaries <- predict(xg_fit, xg_train1)


#model_data <- model_data |>
#   mutate(
#     predicted_salary = predicted_salaries,
#     residual = predicted_salary - salary
#   )
# 
# model_data |>
#   select(player, age, weighted_per_total, salary, predicted_salary, residual) |>
#   arrange(desc(abs(residual)))









set.seed(2025)
model_data <- model_data |>
  mutate(fold = sample(1:4, size = n(), replace = TRUE))


wnba_cv4 <- function(fold_num) {
  test_data <- model_data |> filter(fold == fold_num)
  train_data <- model_data |> filter(fold != fold_num)
  
  # Linear model
  lm_fit <- lm(salary ~ age + weighted_per_total + salary_share, data = train_data)
  
  # Polynomial model
  poly_fit <- lm(salary ~ poly(age, 2) + weighted_per_total + salary_share, data = train_data)
  
  # Model matrices for LASSO/Ridge
  X_train <- model.matrix(salary ~ age + weighted_per_total + salary_share, data = train_data)[, -1]
  X_test  <- model.matrix(salary ~ age + weighted_per_total + salary_share, data = test_data)[, -1]
  y_train <- train_data$salary
  
  # LASSO model
  lasso_fit <- cv.glmnet(X_train, y_train, alpha = 1)
  
  # Ridge model
  ridge_fit <- cv.glmnet(X_train, y_train, alpha = 0)
  
  # XGBoost tuning with caret
  library(caret)
  xgb_grid <- expand.grid(
    nrounds = c(50, 100),
    max_depth = c(2, 3),
    eta = c(0.05, 0.1),
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
  
  xgb_train <- train_data |> 
    select(age, weighted_per_total, salary_share) |> 
    mutate(across(everything(), as.numeric)) |> 
    as.matrix()
  
  xgb_test  <- test_data  |> 
    select(age, weighted_per_total, salary_share) |> 
    mutate(across(everything(), as.numeric)) |> 
    as.matrix()
  
  
  set.seed(42 + fold_num)  # different seed per fold
  xgb_tune <- train(
    x = xgb_train,
    y = y_train,
    method = "xgbTree",
    tuneGrid = xgb_grid,
    trControl = trainControl(method = "cv", number = 3),  # inner CV
    metric = "RMSE",
    verbose = FALSE
  )
  
  xgb_pred <- predict(xgb_tune, newdata = xgb_test)
  
  # Combine predictions
  out <- tibble(
    lm_pred = predict(lm_fit, newdata = test_data),
    poly_pred = predict(poly_fit, newdata = test_data),
    lasso_pred = predict(lasso_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
    ridge_pred = predict(ridge_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
    xgb_pred = xgb_pred,
    actual = test_data$salary,
    fold = fold_num, 
    player = test_data$player,
    team = test_data$team
  )
  
  return(out)
}


cv_results <- map_dfr(1:4, wnba_cv4)






cv_results <- map_dfr(1:N_FOLDS, wnba_cv4) # insert version of wnba_cv4

cv_results_named <- cv_results |>
  left_join(per_df |> select(player, pos), by = "player")

cv_long <- cv_results |>
  pivot_longer(cols = ends_with("_pred"),
               names_to = "model", values_to = "prediction")


cv_summary <- cv_long |>
  group_by(model, fold) |>
  summarise(rmse = rmse_vec(actual, prediction), .groups = "drop")

cv_summary_stats <- cv_summary |>
  group_by(model) |>
  summarise(
    avg_cv_rmse = mean(rmse),
    sd_rmse = sd(rmse),
    k = n(),
    se_rmse = sd_rmse / sqrt(k),
    lower_rmse = avg_cv_rmse - se_rmse,
    upper_rmse = avg_cv_rmse + se_rmse
  ) |>
  arrange(avg_cv_rmse)

# Order model factor levels by average RMSE
model_order <- cv_summary_stats |> arrange(avg_cv_rmse) |> pull(model)

cv_summary |>
  mutate(model = factor(model, levels = model_order)) |>
  ggplot(aes(x = model, y = rmse)) +
  geom_point(size = 4, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", color = "red", size = 4) +
  stat_summary(fun.data = mean_se, geom = "errorbar", color = "red", width = 0.2) +
  scale_x_discrete(labels = c(
    lm_pred = "Linear Model",
    poly_pred = "Polynomial (Age²)",
    lasso_pred = "LASSO",
    ridge_pred = "Ridge",
    xgb_pred = "XGBoost"
  )) +
  labs(
    title = "5-Fold Cross-Validation RMSE by Model",
    x = "Model Type",
    y = "RMSE (Salary)"
  ) +
  theme_minimal()

xgb_output <- cv_results |>
  select(player, team, actual, xgb_pred) |>
  mutate(
    residual = actual - xgb_pred,
    predicted_salary = round(xgb_pred, 2),
    actual_salary = round(actual, 2)
  ) |>
  select(player, team, predicted_salary, actual_salary, residual)


write.csv(xgb_output, "xgb_predictions.csv", row.names = FALSE)

salary_model_df<- read_csv("salary_model_df.csv")

player_ranked_salary <- players_ranked_u_clean %>% 
  left_join(salary_model_df, by = "player") 









