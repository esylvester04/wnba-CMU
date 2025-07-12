p_salary <- read_excel("data/contracts.xlsx")

library(dplyr)

p_salary <- p_salary %>%
  mutate(
    salary = as.numeric(salary) 
  )

p_salary <- p_salary %>% filter(!is.na(salary)) %>%
  group_by(team) %>%
  mutate(
    team_total_salary = sum(salary, na.rm = TRUE),
    salary_proportion = salary / team_total_salary
  ) %>%
  ungroup()
