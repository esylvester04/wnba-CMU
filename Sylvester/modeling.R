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


# making a df with just players, name, year, and weighted per
per_df <- salary_stats |>
  select(player, team, pos, year, `Protection Status`, Salary) |>
  rename(protection_status = `Protection Status`, 
         salary = Salary) |>
  left_join(total_weighted_per, by = "player") |>
  mutate(
    player = player |> 
      str_to_lower() |>              # make lowercase 
      stri_trans_general("Latin-ASCII") ) # remove accents 




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

source("Isabella/salary_proportion_code.R")
p_salary <- p_salary |> # 
  mutate(player = player |>
        str_to_lower() |>
        stri_trans_general("Latin-ASCII")) # making lowercase/ no accents




