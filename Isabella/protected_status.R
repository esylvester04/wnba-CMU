
######## Updated model including past years performance as well, wit more weight on 2025 performance####
library(tidyr)
players_wide <- all_advanced %>%
  filter(year %in% c(2023, 2024, 2025)) %>%
  select(player, team, year, mp, per, ws) %>%
  pivot_wider(
    names_from = year,
    values_from = c(mp, per, ws)
  )

players_wide <- players_wide %>%
  mutate(
    per_2023 = as.numeric(per_2023),
    per_2024 = as.numeric(per_2024),
    per_2025 = as.numeric(per_2025),
    ws_2023 = as.numeric(ws_2023),
    ws_2024 = as.numeric(ws_2024),
    ws_2025 = as.numeric(ws_2025),
    mp_2023 = as.numeric(mp_2023),
    mp_2024 = as.numeric(mp_2024),
    mp_2025 = as.numeric(mp_2025)
  )

weighted_mean_ignore_na <- function(values, weights) {
  # Remove NAs
  keep <- !is.na(values)
  if (sum(keep) == 0) {
    return(NA_real_)  # All values missing
  }
  sum(values[keep] * weights[keep]) / sum(weights[keep])
}


players_weighted <- players_wide %>%
  rowwise() %>%
  mutate(
    weighted_per = weighted_mean_ignore_na(
      c(per_2023, per_2024, per_2025),
      c(0.1, 0.3, 0.6)
    ),
    weighted_ws = weighted_mean_ignore_na(
      c(ws_2023, ws_2024, ws_2025),
      c(0.1, 0.3, 0.6)
    ),
    weighted_mp = weighted_mean_ignore_na(
      c(mp_2023, mp_2024, mp_2025),
      c(0.1, 0.3, 0.6)
    )
  ) %>%
  ungroup()

players_ranked <- players_weighted %>%
  group_by(team) %>%
  mutate(
    per_minute_score = scale(weighted_mp) + scale(weighted_ws)
  ) %>%
  arrange(team, desc(per_minute_score)) %>%
  mutate(
    rank_within_team = row_number(),
    protected = if_else(rank_within_team <= 5, 1, 0)
  ) %>%
  ungroup()


players_ranked %>%
  select(team, player, weighted_mp, weighted_ws, per_minute_score, protected) %>%
  arrange(team, desc(per_minute_score))



protected_players <- players_ranked %>%
  filter(protected == 1) %>%
  arrange(team)
unprotected_players <- players_ranked %>%
  filter(protected == 0) %>%
  arrange(team)

######      
library(readxl)
c_years<- read_excel("contract years left.xlsx")

c_years <- c_years %>% select(player, contract_y, ufa)


########
library(dplyr)

c_years_clean <- c_years %>%
  select(player, contract_y)

players_weighted_c <- players_weighted %>%
  left_join(c_years_clean, by = "player")
colnames(players_weighted_c)



summary(players_weighted_c$contract_y)


########
library(stringi)

players_weighted_summarised <- players_weighted_c %>%
  group_by(player) %>%
  summarise(
    weighted_mp = sum(weighted_mp, na.rm = TRUE),
    weighted_ws = sum(weighted_ws, na.rm = TRUE),
    weighted_per = mean(weighted_per, na.rm = TRUE),
    team = last(team)
  ) %>%
  ungroup()

players_weighted_summarised <- players_weighted_summarised %>%
  mutate(
    player = str_squish(player),         # Remove extra spaces
    player = str_to_title(player)        # Convert to Title Case ("Aari Mcdonald")
  )


c_years_clean <- c_years_clean %>%
  mutate(
    player = str_squish(player),
    player = str_to_title(player)
  )


players_weighted_summarised <- players_weighted_summarised %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))


c_years_clean <- c_years_clean %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))





players_weighted_final <- players_weighted_summarised %>%
  right_join(c_years_clean, by = "player")


players_ranked <- players_weighted_final %>%
  group_by(team) %>%
  mutate(
    per_minute_score = scale(weighted_mp) +
      scale(weighted_ws) +
      scale(weighted_per) +
      0.3 * scale(contract_y)
  ) %>%
  arrange(team, desc(per_minute_score)) %>%
  mutate(
    rank_within_team = row_number(),
    protected = if_else(rank_within_team <= 5, 1, 0)
  ) %>%
  ungroup()


