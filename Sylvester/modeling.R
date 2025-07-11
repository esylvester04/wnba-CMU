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







