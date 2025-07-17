library(readxl)
salary <- read_excel("data/contracts.xlsx")
age <- read_csv("data/age2.csv")

all_advanced_24 <- all_advanced %>% filter(year== 2024)
all_advanced_25 <- all_advanced %>% filter(year== 2025)


all_advanced_24 <- all_advanced_24 %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))


all_advanced_25 <- all_advanced_25 %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))


salary <- salary %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))

age <- age %>% 
  mutate(`PLAYER` = stri_trans_general(`PLAYER`, "Latin-ASCII"),
         `PLAYER` = str_trim(`PLAYER`),
         `PLAYER` = str_to_lower(`PLAYER`),
         `PLAYER` = str_replace_all(`PLAYER`, "-", " "),  # Replace hyphen with space
         `PLAYER` = str_replace(`PLAYER`, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))

colnames(age)

salary_stats_24 <- all_advanced_24 %>%
  right_join(salary, by = "player")




salary_stats_25 <- all_advanced_25 %>% right_join(salary, by = "player")

salary_stats_25 <- salary_stats_25 %>%
  left_join(
    age %>% select(player = PLAYER, age = AGE),
    by = "player"
  )

# Scale 2025 stats
salary_stats_25 <- salary_stats_25 %>%
  mutate(
    salary = as.numeric(salary)
  )

salary_stats_24 <- salary_stats_24 %>%
  mutate(
    salary = as.numeric(salary)
  )

# Pick numeric stat columns
# List exactly the columns you want to scale
stat_vars <- c(
  "mp",
  "per",
  "ts_percent",
  "usg_percent",
  "trb_percent",
  "ws",
  "age"
)
salary_stats_25[stat_vars] <- lapply(
  salary_stats_25[stat_vars],
  function(x) as.numeric(as.character(x))
)

salary_stats_25[stat_vars] <- scale(salary_stats_25[stat_vars])


# Option B (Predict what they should earn based on current season):
#   
#   Predict salary_2025 using 2025 stats.
# This shows current market value—i.e., if contracts were set today.
# ✅ To identify undervalued players right now, you mainly care about Option B:
#   
#   How much are they worth based on current performance compared to their actual salary?
columns_to_check <- c(
  "salary","age"
)

salary_stats_25_clean <- salary_stats_25 %>%
  drop_na(all_of(columns_to_check))


set.seed(123)
split <- sample(1:nrow(salary_stats_25_clean), size = 0.8 * nrow(salary_stats_25_clean))
train_data <- salary_stats_25_clean[split, ]
test_data <- salary_stats_25_clean[-split, ]


s_lm_model <- lm(
  salary ~ usg_percent + per + age + mp +ws,
  data = train_data
)

summary(s_lm_model)


library(randomForest)

rf_model <- randomForest(
  salary ~ mp + per + usg_percent + ws + age,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

importance(rf_model)

varImpPlot(rf_model)

####### Updated random forest model

library(Metrics)
library(randomForest)
library(dplyr)

set.seed(123)

ntree_vals <- c(100, 300, 500, 700)
nodesize_vals <- c(1, 3, 5, 10)

tune_results <- expand.grid(ntree = ntree_vals, nodesize = nodesize_vals)
tune_results$RMSE <- NA

for (i in 1:nrow(tune_results)) {
  cat("Fitting model", i, "ntree =", tune_results$ntree[i], 
      "nodesize =", tune_results$nodesize[i], "\n")
  
  rf_model <- randomForest(
    salary ~ mp + age + per +usg_percent + ws,
    data = train_data,
    ntree = tune_results$ntree[i],
    nodesize = tune_results$nodesize[i]
  )
  
  preds <- predict(rf_model, newdata = test_data)
  tune_results$RMSE[i] <- rmse(test_data$salary, preds)
}

best_combo <- tune_results %>% arrange(RMSE) %>% slice(1)

print(tune_results)

cat("Best ntree:", best_combo$ntree, "\n")
cat("Best nodesize:", best_combo$nodesize, "\n")

cat("Best RMSE:", round(best_combo$RMSE, 2), "\n")

final_rf <- randomForest(
  salary ~ mp + age + per +usg_percent +ws,
  data = salary_stats_25_clean,  # combine train_data and test_data if you want
  ntree = 300,
  nodesize = 10
)
importance(final_rf)
varImpPlot(final_rf)

preds_all <- predict(final_rf, newdata = salary_stats_25_clean)

library(Metrics)
rmse_value <- rmse(salary_stats_25_clean$salary, preds_all)
cat("RMSE:", round(rmse_value, 2), "\n")

# Sum of squares total
sst <- sum((salary_stats_25_clean$salary - mean(salary_stats_25_clean$salary))^2)

# Sum of squares residual
ssr <- sum((salary_stats_25_clean$salary - preds_all)^2)

# R-squared
r_squared <- 1 - ssr / sst
cat("R-squared:", round(r_squared, 4), "\n")


residuals <- salary_stats_25_clean$salary - preds_all
head(residuals)


###### cross validation

#install.packages("caret")   # if you haven't yet
library(caret)

set.seed(123)

fitControl <- trainControl(
  method = "cv",
  number = 5
)


set.seed(123)

rf_caret <- train(
  salary ~ mp + age + usg_percent + ws+ per ,
  data = salary_stats_25_clean,
  method = "rf",
  trControl = fitControl,
  tuneGrid = expand.grid(.mtry = 2),  # mtry can be tuned; 2 is just an example
  ntree = 300
)
print(rf_caret)

lm_model <- train(
  salary ~ mp + age + usg_percent + ws + per,
  data = salary_stats_25_clean,
  method = "lm",
  trControl = fitControl
)


# 
ridge_model <- train(
  salary ~ mp + age + usg_percent,
  data = salary_stats_25_clean,
  method = "ridge",
  trControl = fitControl,
  tuneLength = 5  # automatically tries 5 lambda values
)
print(ridge_model)
# 
# 
lasso_model <- train(
  salary ~ mp + age + usg_percent,
  data = salary_stats_25_clean,
  method = "lasso",
  trControl = fitControl, tuneLength = 5
)
print(lasso_model)

gbm_model <- train(
  salary ~ mp + age + usg_percent + per +ws,
  data = salary_stats_25_clean,
  method = "xgbTree",
  trControl = fitControl,
  tuneLength = 5
)
print(gbm_model)


rf_caret$results
lm_model$results



results_df <- data.frame(
  Model = c("Random Forest", "Linear Regression", "Gradient Boosting", "Ridge"),
  RMSE = c(
    min(rf_caret$results$RMSE),
    min(lm_model$results$RMSE),
    min(gbm_model$results$RMSE),
    min(ridge_model$results$RMSE)
  ),
  Rsquared = c(
    max(rf_caret$results$Rsquared),
    max(lm_model$results$Rsquared),
    max(gbm_model$results$Rsquared),
    max(ridge_model$results$Rsquared)
  )
)
print(results_df)



######## TUNING XGBOOST MODEL

gbm_model$results

grid <- expand.grid(
  nrounds = c(100, 200, 300),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.05, 0.1),
  gamma = c(0, 0.1),
  colsample_bytree = 0.8,
  min_child_weight = 1,
  subsample = 0.8
)

set.seed(123)
gbm_tuned <- train(
  salary ~ mp + age + usg_percent + per + ws,
  data = salary_stats_25_clean,
  method = "xgbTree",
  trControl = fitControl,  # your 5-fold CV control
  tuneGrid = grid,
  verbose = TRUE
)


gbm_tuned$results %>%
  arrange(RMSE) %>%
  head(10)

final_gbm <- train(
  salary ~ mp + age + usg_percent + per + ws,
  data = salary_stats_25_clean,
  method = "xgbTree",
  trControl = trainControl(method = "none"),
  tuneGrid = gbm_tuned$bestTune
)

#########
library(tidyverse)
library(xgboost)
library(ModelMetrics)
# Convert predictors to matrix
x_vars <- c("mp", "age", "usg_percent", "per", "ws")
X_train <- as.matrix(salary_stats_25_clean[, x_vars])
y_train <- salary_stats_25_clean$salary

str(salary_stats_25_clean[, x_vars])


dtrain <- xgb.DMatrix(data = X_train, label = y_train)

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.1
)

xgb_cv1 <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 500,
  nfold = 5,
  early_stopping_rounds = 10,
  verbose = 0
)

best_nrounds <- xgb_cv1$best_iteration
cat("Best nrounds:", best_nrounds, "\n")

grid <- expand.grid(
  max_depth = c(3, 5, 7),
  min_child_weight = c(1, 3, 5)
)

results <- data.frame()

for (i in 1:nrow(grid)) {
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = 0.1,
    max_depth = grid$max_depth[i],
    min_child_weight = grid$min_child_weight[i]
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = best_nrounds,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  results <- rbind(
    results,
    data.frame(
      max_depth = grid$max_depth[i],
      min_child_weight = grid$min_child_weight[i],
      mean_rmse = min(cv$evaluation_log$test_rmse_mean),
      best_iter = cv$best_iteration
    )
  )
}

results[order(results$mean_rmse), ]

etas <- c(0.1, 0.05, 0.01)
results <- data.frame()

for (eta_val in etas) {
  nrounds_val <- ifelse(eta_val == 0.01, 500, ifelse(eta_val == 0.05, 200, 100))
  
  params <- list(
    objective = "reg:squarederror",
    eval_metric = "rmse",
    eta = eta_val,
    max_depth = 5,             # best from previous step
    min_child_weight = 5
  )
  
  cv <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = nrounds_val,
    nfold = 5,
    early_stopping_rounds = 10,
    verbose = 0
  )
  
  best_iter <- which.min(cv$evaluation_log$test_rmse_mean)
  mean_rmse <- min(cv$evaluation_log$test_rmse_mean)
  
  results <- rbind(
    results,
    data.frame(
      eta = eta_val,
      nrounds = nrounds_val,
      best_iter = best_iter,
      mean_rmse = mean_rmse
    )
  )
}

results[order(results$mean_rmse), ]

final_params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.10,               # example best eta
  max_depth = 5,            # example best depth
  min_child_weight = 5
)

final_model <- xgboost(
  params = final_params,
  data = dtrain,
  nrounds = 100             # example best nrounds
)


results[order(results$mean_rmse), ]


importance_matrix <- xgb.importance(model = final_model)
print(importance_matrix)

# Plot
xgb.plot.importance(importance_matrix)


preds_all <- predict(final_model, dtrain)

rmse_value <- rmse(y_train, preds_all)
cat("RMSE:", round(rmse_value, 2), "\n")

sst <- sum((y_train - mean(y_train))^2)
ssr <- sum((y_train - preds_all)^2)
r_squared <- 1 - ssr / sst
cat("R-squared:", round(r_squared, 4), "\n")

X_test <- as.matrix(
  test_data[, x_vars] %>% 
    mutate(across(everything(), as.numeric))
)

y_test <- as.numeric(test_data$salary)

dtest <- xgb.DMatrix(data = X_test)



preds_test <- predict(final_model, dtest)
library(Metrics)

# RMSE
rmse_test <- rmse(y_test, preds_test)
cat("Test RMSE:", round(rmse_test, 2), "\n")

# R-squared
sst_test <- sum((y_test - mean(y_test))^2)
ssr_test <- sum((y_test - preds_test)^2)
r_squared_test <- 1 - ssr_test / sst_test
cat("Test R-squared:", round(r_squared_test, 4), "\n")

residuals_test <- y_test - preds_test
summary(residuals_test)

# Plot actual vs predicted
plot(y_test, preds_test,
     xlab = "Actual Salary",
     ylab = "Predicted Salary",
     main = "Actual vs Predicted Salary (Test Set)")
abline(a=0, b=1, col="red")

# All data DMatrix
X_all <- as.matrix(
  salary_stats_25_clean[, x_vars] %>%
    mutate(across(everything(), as.numeric))
)

dall <- xgb.DMatrix(data = X_all)

# Predicted salaries
predicted_salaries <- predict(final_model, dall)

salary_stats_25_clean <- salary_stats_25_clean %>%
  mutate(
    predicted_salary = predicted_salaries,
    residual = predicted_salary - salary
  )

undervalued_players <- salary_stats_25_clean %>%
  arrange(desc(residual)) %>%
  select(player, team.x, salary, predicted_salary, residual)

head(undervalued_players, 10)









