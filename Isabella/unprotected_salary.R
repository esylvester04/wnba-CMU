unprotected_with_salary <- salary_stats_25_clean %>%
  semi_join(unprotected_players, by = "player") %>%
  arrange(desc(residual))

unprotected_with_salary_u <- salary_stats_25_clean %>%
  semi_join(unprotected_players_u, by = "player") %>%
  arrange(desc(residual))
