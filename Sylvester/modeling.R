source("data/data-sal-model.R")
data <- get_wnba_data()
advanced_stats <- data$all_advanced
salary_stats <- data$salary_stat
library(dplyr)
library(tidyr)
library(stringr)

# Original year weights
year_weights <- tibble(
  year = c(2023, 2024, 2025),
  weight = c(0.3, 0.5, 0.2)
)

advanced_weighted <- advanced_stats |>
  inner_join(year_weights, by = "year") |>
  filter(!is.na(per)) |>
  mutate(per = as.numeric(per))

advanced_weighted_norm <- advanced_weighted |>
  group_by(player) |>
  mutate(
    norm_weight = weight / sum(weight),
    weighted_per = per * norm_weight
  ) |>
  ungroup()

total_weighted_per <- advanced_weighted_norm |>
  group_by(player) |>
  summarise(
    weighted_per_total = sum(weighted_per, na.rm = TRUE),
    .groups = "drop"
  )

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




##### adding the age dataframe

source("data/ages_data.R")  # Load the get_ages() function

ages <- get_ages()

# Ensure player name format matches for clean join
per_df <- per_df %>%
  mutate(player = player |>
           str_to_lower() |>
           stri_trans_general("Latin-ASCII"))

# Join age into per_df
## NOW INCLUDES: age (static as of July), weighted per
per_df <- per_df |>
     left_join(ages |>
     select(player, age), by = "player") |>
     mutate(age = if_else(player == "megan gustafson", 28, age))


#### adding proportion of salary 

#getting total team spending 
team_salary_totals <- per_df |>
  group_by(team) |>
  summarise(team_salary_total = sum(salary, na.rm = TRUE), .groups = "drop")


per_df <- per_df |>
  left_join(team_salary_totals, by = "team") |>
  mutate(
    salary_share = salary / team_salary_total
  )

per_df |>
  select(player, team, salary, team_salary_total, salary_share) |>
  arrange(desc(salary_share)) |>
  slice_head(n = 10)


# starting the modeling comparisons: 
# simple linear model first
lm <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  lm(salary ~ age + weighted_per_total + salary_share, data = _)
summary(lm)

library(broom)
augment(lm) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs. Fitted",
    x = "Fitted Salary",
    y = "Residuals"
  ) +
  theme_minimal() #cone shape-- definitely violated LINE conditions

#trying a log transformation on the outcome 
model_log <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  lm(log(salary) ~ age + weighted_per_total + salary_share, data = _)

augment(model_log) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted (Log Model)") #still concerning linearity 
# some bad outliers 


#trying polynomial age 
model_poly <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  lm(log(salary) ~ poly(age, 2) + weighted_per_total + salary_share, data = _)

augment(model_poly) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Residuals vs. Fitted (Poly Model)") 



# trying interaction models:
model_interaction <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  lm(salary ~ age * weighted_per_total + salary_share, data = _)
summary(model_interaction)

augment(model_interaction) |>
  ggplot(aes(.fitted, .resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Fitted (Interaction Model)",
    x = "Predicted Salary",
    y = "Residuals"
  ) +
  theme_minimal() #more conditions violations



## regularized model for prediction

###adding a LASSO model 
library(glmnet)

# Prepare matrix
X <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  select(age, weighted_per_total, salary_share) |>
  as.matrix()

y <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  pull(salary)

# Fit lasso
model_lasso <- cv.glmnet(X, y, alpha = 1)
model_lasso$lambda.min       # Lambda that minimizes cross-validation error
model_lasso$lambda.1se       # Simpler model within 1 SE of minimum

coef(model_lasso, s = "lambda.min")

pred_df <- per_df |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_share)) |>
  mutate(
    predicted_salary_lasso = predict(model_lasso, newx = X, s = "lambda.min") |> as.numeric(),
    residual_lasso = salary - predicted_salary_lasso
  )


library(yardstick)
tibble(
  truth = y,
  estimate = predict(model_lasso, newx = X, s = "lambda.min") |> as.numeric()
) |>
  metrics(truth = truth, estimate = estimate)



ggplot(pred_df, aes(x = predicted_salary_lasso, y = salary)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Actual vs. Predicted Salary (LASSO)",
    x = "Predicted Salary",
    y = "Actual Salary"
  ) +
  theme_minimal()


##### adding Isabella's contract data 
library(stringi)
library(stringr)
library(readxl)
source("Isabella/salary_proportion_code.R")



# getting better.. only 18 players in my per DF that do not appear in Isabella's
# contract DF. this is likely because they are short term contracts, overseas, hardships, or waived

# Leaving the missing player in with NA salary (? )
# OR jUST REMOVE COMPLETELY because they aren't on active contracts (?)
###### definitely need to at least leave the NAs out during the modeling phase 

# DATA TO USE FOR MODELING: 
# Isabella's contract df with my weighted PER variable 

library(stringr)


clean_names <- function(x) {
  x |>
    str_to_lower() |>
    stri_trans_general("Latin-ASCII") |>
    str_squish()
}

# Apply consistently
per_df <- per_df |>
  mutate(player = clean_names(player))

p_salary <- p_salary |>
  mutate(player = clean_names(player))


p_salary_joined <- p_salary |>
  left_join(
    per_df |> select(player, weighted_per_total, age),
    by = "player"
  )


#looking at who has na is age and weighted PER 
p_salary_joined |>
  filter(is.na(weighted_per_total) | is.na(age)) |>
  pull(player) |>
  unique()




# clean_df <- p_salary_joined |> 
#   filter(player %in% p_salary$player)
# 
# p_salary |>
#   filter(is.na(weighted_per_total)) |>
#   select(player) # 13 players without PER. rookies, injured players without enough minutes
# # remove NAs during modeling (?)
# 
# clean_df <- clean_df |> 
#   mutate(weighted_per_total = coalesce(weighted_per_total.x, weighted_per_total.y)) |> 
#   select(-weighted_per_total.x, -weighted_per_total.y)


###### retrying LASSO regression
model_data <- p_salary_joined |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_proportion))

# Step 2: Create X and y
X <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = model_data)[, -1]
y <- model_data$salary

# Step 3: Fit LASSO
cv_lasso <- cv.glmnet(X, y, alpha = 1)

# Step 4: Plot CV error vs lambda
plot(cv_lasso, xvar = "lambda")

coef(cv_lasso, s = "lambda.min")
coef(cv_lasso, s = "lambda.1se")


library(yardstick)
library(tibble)

lasso_pred <- predict(cv_lasso, newx = X, s = "lambda.min")

metrics(
  data = tibble(truth = y, estimate = as.numeric(lasso_pred)),
  truth = truth,
  estimate = estimate
)
# RSME: 13k with a R^2 of 0.946
# really high R^2






#####################
# cross validation for model comparison: 
# comparing LASSO, polynomial, linear, and ridge

set.seed(100)
N_FOLDS <- 5

# Filter out incomplete rows, then assign folds
model_data_cv <- model_data |>
  filter(!is.na(age), !is.na(weighted_per_total), !is.na(salary), !is.na(salary_proportion)) |>
  mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))

wnba_cv <- function(fold_num) {
  test_data <- model_data_cv |> filter(fold == fold_num)
  train_data <- model_data_cv |> filter(fold != fold_num)
  
  # Lm
  lm_fit <- lm(salary ~ age + weighted_per_total + salary_proportion, data = train_data)
  
  # Polynomial model
  poly_fit <- lm(salary ~ poly(age, 2) + weighted_per_total + salary_proportion, data = train_data)
  
  # LASSO model
  X_train <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = train_data)[, -1]
  y_train <- train_data$salary
  lasso_fit <- cv.glmnet(X_train, y_train, alpha = 1)
  
  # Ridge model (α = 0)
  ridge_fit <- cv.glmnet(X_train, y_train, alpha = 0)
  
  # Prepare test predictors
  X_test <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = test_data)[, -1]
  
  out <- tibble(
    lm_pred = predict(lm_fit, newdata = test_data),
    poly_pred = predict(poly_fit, newdata = test_data),
    lasso_pred = predict(lasso_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
    ridge_pred = predict(ridge_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
    actual = test_data$salary,
    fold = fold_num
  )
  return(out)
}

cv_results <- map_dfr(1:N_FOLDS, wnba_cv)

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


# Reorder model factor levels by average RMSE
model_order <- cv_summary_stats |>
  arrange(avg_cv_rmse) |>
  pull(model)

cv_summary |>
  mutate(model = factor(model, levels = model_order)) |>
  ggplot(aes(x = model, y = rmse)) +
  geom_point(size = 4, alpha = 0.7) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2) +
  scale_x_discrete(labels = c(
    lm_pred = "Linear Model",
    poly_pred = "Polynomial (Age²)",
    lasso_pred = "LASSO",
    ridge_pred = "Ridge"
  )) +
  labs(
    title = "5-Fold Cross-Validation RMSE by Model",
    x = "Model Type",
    y = "RMSE (Salary)"
  ) +
  theme_minimal()


### adding random forest model (?)

# install.packages("stacks")
library(tidyverse)
theme_set(theme_light())
library(stacks)
glimpse(model_data)

# fitting a plain random forest model with no tuning
library(ranger)
salary_rf <- ranger(salary ~ age + weighted_per_total + salary_proportion, 
                   num.trees = 500, importance = "impurity", data = model_data)
salary_rf


# looking at variable importance
library(vip)
vip(salary_rf)


# Model Evaluation again
model_data |> 
  mutate(pred = salary_rf$predictions) |> 
  summarize(rmse = sqrt(mean((salary - pred) ^ 2)))
# rmse is 14,973k 

model_data |>
  mutate(pred = salary_rf$predictions) |>
  ggplot(aes(salary, pred)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed")


# now, need to work on tuning rf 


# trying XGBoost
# Prepare training matrix
xg_train1 <- model_data |>
  select(age, weighted_per_total, salary_proportion) |>
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
  tuneGrid = xg_grid,
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




#MODEL COMPARISON:
### redoing cross-validation
# wnba_cv2 <- function(fold_num) {
#   test_data <- model_data_cv |> filter(fold == fold_num)
#   train_data <- model_data_cv |> filter(fold != fold_num)
#   
#   # Linear model
#   lm_fit <- lm(salary ~ age + weighted_per_total + salary_proportion, data = train_data)
#   
#   # Polynomial model
#   poly_fit <- lm(salary ~ poly(age, 2) + weighted_per_total + salary_proportion, data = train_data)
#   
#   # LASSO model
#   X_train <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = train_data)[, -1]
#   y_train <- train_data$salary
#   lasso_fit <- cv.glmnet(X_train, y_train, alpha = 1)
#   
#   # Ridge model
#   ridge_fit <- cv.glmnet(X_train, y_train, alpha = 0)
#   
#   # XGBoost model
#   xgb_train <- train_data |> select(age, weighted_per_total, salary_proportion) |> as.matrix()
#   xgb_test <- test_data |> select(age, weighted_per_total, salary_proportion) |> as.matrix()
#   
#   xgb_fit <- xgboost(
#     data = xgb_train,
#     label = y_train,
#     objective = "reg:squarederror",
#     nrounds = 100,
#     max_depth = 3,
#     eta = 0.1,
#     verbose = 0
#   )
#   
#   # Predictions
#   out <- tibble(
#     lm_pred = predict(lm_fit, newdata = test_data),
#     poly_pred = predict(poly_fit, newdata = test_data),
#     lasso_pred = predict(lasso_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
#     ridge_pred = predict(ridge_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
#     xgb_pred = predict(xgb_fit, newdata = xgb_test),
#     actual = test_data$salary,
#     fold = fold_num
#   )
#   
#   return(out)
# }


wnba_cv3 <- function(fold_num) {
  test_data <- model_data_cv |> filter(fold == fold_num)
  train_data <- model_data_cv |> filter(fold != fold_num)
  
  # Linear model
  lm_fit <- lm(salary ~ age + weighted_per_total + salary_proportion, data = train_data)
  
  # Polynomial model
  poly_fit <- lm(salary ~ poly(age, 2) + weighted_per_total + salary_proportion, data = train_data)
  
  # Model matrices for LASSO/Ridge
  X_train <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = train_data)[, -1]
  X_test  <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = test_data)[, -1]
  y_train <- train_data$salary
  
  # LASSO model
  lasso_fit <- cv.glmnet(X_train, y_train, alpha = 1)
  
  # Ridge model
  ridge_fit <- cv.glmnet(X_train, y_train, alpha = 0)
  
  # XGBoost
  xgb_train <- train_data |> select(age, weighted_per_total, salary_proportion) |> as.matrix()
  xgb_test  <- test_data  |> select(age, weighted_per_total, salary_proportion) |> as.matrix()
  
  xgb_fit <- xgboost(
    data = xgb_train,
    label = y_train,
    objective = "reg:squarederror",
    nrounds = 100,
    max_depth = 3,
    eta = 0.1,
    verbose = 0
  )
  
  # Predictions
  out <- tibble(
    lm_pred = predict(lm_fit, newdata = test_data),
    poly_pred = predict(poly_fit, newdata = test_data),
    lasso_pred = predict(lasso_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
    ridge_pred = predict(ridge_fit, newx = X_test, s = "lambda.min") |> as.numeric(),
    xgb_pred = predict(xgb_fit, newdata = xgb_test),
    actual = test_data$salary,
    fold = fold_num
  )
  
  return(out)
}




#adding CV for a further tuned model 
wnba_cv4 <- function(fold_num) {
  test_data <- model_data_cv |> filter(fold == fold_num)
  train_data <- model_data_cv |> filter(fold != fold_num)
  
  # Linear model
  lm_fit <- lm(salary ~ age + weighted_per_total + salary_proportion, data = train_data)
  
  # Polynomial model
  poly_fit <- lm(salary ~ poly(age, 2) + weighted_per_total + salary_proportion, data = train_data)
  
  # Model matrices for LASSO/Ridge
  X_train <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = train_data)[, -1]
  X_test  <- model.matrix(salary ~ age + weighted_per_total + salary_proportion, data = test_data)[, -1]
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
  
  xgb_train <- train_data |> select(age, weighted_per_total, salary_proportion) |> as.matrix()
  xgb_test  <- test_data  |> select(age, weighted_per_total, salary_proportion) |> as.matrix()
  
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


library(ggplot2)
library(dplyr)

# Filter to XGBoost predictions only
xgb_preds <- cv_results |> 
  select(actual, xgb_pred)


library(scales)
xgb_preds <- cv_results |> 
  select(actual, xgb_pred) |>
  mutate(residual = xgb_pred - actual)

ggplot(xgb_preds, aes(x = actual, y = xgb_pred, color = residual)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_color_gradient2(
    low = "#d73027",    # red for negative residual (overpaid)
    mid = "#d3d3d3",    # light gray near zero residual
    high = "#1a9850",   # green for positive residual (underpaid)
    midpoint = 0,
    name = "Residual\n(Predicted - Actual)"
  ) +
  labs(
    title = "XGBoost Predicted vs. Actual WNBA 2025 Salaries",
    x = "Actual 2025 Salary",
    y = "Predicted 2025 Salary"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )




ggplot(xgb_preds, aes(x = actual, y = xgb_pred, color = residual)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_color_gradient2(
    low = "#8C6BB1",    # Blue or whatever you choose
    mid = "#D3D3D3",    # Light grey for zero
    high = "#66C2A5",   # Orange or green or your pick
    midpoint = 0,
    name = "Residual\n(Predicted - Actual)"
  ) +
  labs(
    title = "XGBoost Predicted vs. Actual WNBA 2025 Salaries",
    x = "Actual 2025 Salary",
    y = "Predicted 2025 Salary"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )



ggplot(xgb_preds, aes(x = actual, y = xgb_pred, color = residual)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  scale_x_continuous(labels = dollar_format(prefix = "$")) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_color_gradient2(
    low = "firebrick",      # uverpaid
    mid = "gray80",           # fair
    high =  "forestgreen",       #underpaid
    midpoint = 0
   # name = "Estimated Team Savings"
  ) +
  labs(
    title = "XGBoost Predicted vs. Actual WNBA 2025 Salaries",
    x = "Actual 2025 Salary",
    y = "Predicted 2025 Salary"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(),
    legend.title = element_text()
  )


#thinking about how to rank top players 

xgboost_preds <- cv_results_named |>
  select(player, team, pos, actual, xgb_pred) |>
  mutate(residual = xgb_pred - actual)

top_underpaid <- xgboost_preds |>
  arrange(desc(residual)) |>
  select(player, team, pos, actual, xgb_pred, residual)

top_underpaid |>
  mutate(across(c(actual, xgb_pred, residual), scales::dollar_format()))


underpaid_by_position <- xgboost_preds |>
  filter(residual > 0) |>
  group_by(pos) |>
  arrange(desc(residual), .by_group = TRUE) |>
  slice_head(n = 5)  # top 5 per position

underpaid_players <- xgboost_preds |>
  mutate(residual = xgb_pred - actual) |>
  filter(residual > 0)  # only underpaid

underpaid_ranked <- underpaid_players |>
  group_by(pos) |>
  arrange(desc(residual), .by_group = TRUE) |>
  mutate(rank_within_pos = row_number()) |>
  ungroup()

underpaid_ranked |>
  group_by(pos) |>
  slice_max(residual, n = 5, with_ties = FALSE) |>
  select(pos, rank_within_pos, player, team, actual, xgb_pred, residual) |>
  arrange(pos, rank_within_pos)


underpaid_players |>
  group_by(pos) |>
  slice_max(residual, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(player, pos, actual, xgb_pred, residual) |>
  arrange(desc(residual)) |>
  mutate(
    actual = scales::dollar(actual),
    xgb_pred = scales::dollar(xgb_pred),
    residual = scales::dollar(residual)
  )


#' Create a table of most underpaid players by position
#'
#' @param underpaid_players A data frame with columns: player, pos, actual, xgb_pred, residual
#' @return A formatted data frame with dollar-formatted columns
#' @export
get_top_underpaid_by_pos <- function(underpaid_players) {
  underpaid_players |>
    group_by(pos) |>
    slice_max(residual, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(player, pos, actual, xgb_pred, residual) |>
    arrange(desc(residual)) |>
    mutate(
      actual = scales::dollar(actual),
      xgb_pred = scales::dollar(xgb_pred),
      residual = scales::dollar(residual)
    )
}

