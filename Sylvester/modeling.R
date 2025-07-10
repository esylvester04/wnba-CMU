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

advanced_weighted <- advanced_stats %>%
  inner_join(year_weights, by = "year") %>%
  filter(!is.na(per)) %>%
  mutate(per = as.numeric(per))

advanced_weighted_norm <- advanced_weighted %>%
  group_by(player) %>%
  mutate(
    norm_weight = weight / sum(weight),
    weighted_per = per * norm_weight
  ) %>%
  ungroup()

# Total weighted PER per player
total_weighted_per <- advanced_weighted_norm %>%
  group_by(player) %>%
  summarise(weighted_per_total = sum(weighted_per, na.rm = TRUE), .groups = "drop")

# Pivot weighted PER by year into wide format
weighted_per_wide <- advanced_weighted_norm %>%
  mutate(year = paste0("per_", str_sub(year, 3, 4))) %>%
  select(player, year, weighted_per) %>%
  pivot_wider(
    names_from = year,
    values_from = weighted_per,
    values_fill = 0
  )

# Merge total weighted PER into salary stats
salary_stats <- salary_stats %>%
  #select(-weighted_per_total) %>%  # remove old if exists to avoid .x/.y
  left_join(total_weighted_per, by = "player")




