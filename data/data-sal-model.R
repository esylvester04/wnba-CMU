get_wnba_data <- function() {
  library(rvest)
  library(dplyr)
  library(janitor)
  library(lubridate)
  library(tidyverse)
  library(FactoMineR)
  library(factoextra)
  library(stringr)
  
  years <- 2022:2025
  
  # Helper to read and clean advanced stats
  read_advanced <- function(year) {
    url <- paste0("https://www.basketball-reference.com/wnba/years/", year, "_advanced.html")
    read_html(url) %>%
      html_element("table.stats_table") %>%
      html_table() %>%
      clean_names() %>%
      select(-g_2, -mp_2, -x) %>%
      mutate(year = year)
  }
  
  # Stack all advanced
  all_advanced <- map_dfr(years, read_advanced)
  all_advanced <- all_advanced[!all_advanced$player == "Player",]
  
  # Fix traded players: keep updated TOT row
  multi_rows <- all_advanced %>% group_by(player, year) %>% filter(n() > 1) %>% ungroup() %>% select(player, year) %>% distinct()
  last_team <- all_advanced %>% filter(team != "TOT") %>% semi_join(multi_rows, by = c("player", "year")) %>%
    group_by(player, year) %>% slice_tail(n = 1) %>% select(player, year, last_team = team)
  tot_rows <- all_advanced %>% filter(team == "TOT") %>% inner_join(multi_rows, by = c("player", "year")) %>%
    left_join(last_team, by = c("player", "year")) %>% mutate(team = last_team) %>% select(-last_team)
  no_trade_rows <- all_advanced %>% anti_join(multi_rows, by = c("player", "year"))
  all_advanced <- bind_rows(no_trade_rows, tot_rows) %>% arrange(player, year)
  
  # Get 2025 data cleaned for salary join
  a_wnba_2025_u <- all_advanced %>% filter(year == 2025)
  
  # Salary data
  wnba_salaries <- read_csv("salaries.csv")
  salary_cleaned <- wnba_salaries %>% filter(Year == 2025) %>% distinct(Player, Year, .keep_all = TRUE)
  a_wnba_2025_u <- a_wnba_2025_u %>% mutate(player = str_trim(player))
  salary_cleaned <- salary_cleaned %>% mutate(Player = str_trim(Player))
  
  salary_stat <- left_join(a_wnba_2025_u, salary_cleaned, by = c("player" = "Player")) %>%
    mutate(
      Salary = parse_number(Salary),
      per = as.numeric(per),
      per = if_else(per < 0, 0, per)
    ) %>%
    filter(!is.na(Salary), !is.na(per))
  
  return(list(
    all_advanced = all_advanced,
    salary_stat = salary_stat
  ))
}



get_wnba_data2 <- function() {
  library(rvest)
  library(dplyr)
  library(janitor)
  library(lubridate)
  library(tidyverse)
  library(FactoMineR)
  library(factoextra)
  library(stringr)
  
  years <- 2022:2025
  
  # Helper to read and clean advanced stats
  read_advanced <- function(year) {
    url <- paste0("https://www.basketball-reference.com/wnba/years/", year, "_advanced.html")
    
    page <- tryCatch(read_html(url),
                     error = function(e) {
                       message("Error reading page for year ", year)
                       return(NULL)
                     })
    
    if (is.null(page)) return(NULL)
    
    table_node <- html_element(page, "table.stats_table")
    
    if (is.null(table_node)) {
      message("No advanced stats table found for year ", year)
      return(NULL)
    }
    
    df <- html_table(table_node) %>%
      clean_names()
    
    # Check if df is data frame and has expected columns
    if (!("player" %in% names(df))) {
      message("Table for year ", year, " missing 'player' column")
      return(NULL)
    }
    
    df %>%
      select(-g_2, -mp_2, -x) %>%
      mutate(year = year)
  }
  
  
  # Stack all advanced
  all_advanced <- map_dfr(years, ~ {
    df <- read_advanced(.x)
    if (is.null(df)) {
      tibble()  # return empty tibble for that year
    } else {
      df
    }
  })
  all_advanced <- all_advanced[!all_advanced$player == "Player",]
  
  # Fix traded players: keep updated TOT row
  multi_rows <- all_advanced %>% group_by(player, year) %>% filter(n() > 1) %>% ungroup() %>% select(player, year) %>% distinct()
  last_team <- all_advanced %>% filter(team != "TOT") %>% semi_join(multi_rows, by = c("player", "year")) %>%
    group_by(player, year) %>% slice_tail(n = 1) %>% select(player, year, last_team = team)
  tot_rows <- all_advanced %>% filter(team == "TOT") %>% inner_join(multi_rows, by = c("player", "year")) %>%
    left_join(last_team, by = c("player", "year")) %>% mutate(team = last_team) %>% select(-last_team)
  no_trade_rows <- all_advanced %>% anti_join(multi_rows, by = c("player", "year"))
  all_advanced <- bind_rows(no_trade_rows, tot_rows) %>% arrange(player, year)
  
  # Get 2025 data cleaned for salary join
  a_wnba_2025_u <- all_advanced %>% filter(year == 2025)
  
  # Salary data
  wnba_salaries <- read_csv("salaries.csv")
  salary_cleaned <- wnba_salaries %>% filter(Year == 2025) %>% distinct(Player, Year, .keep_all = TRUE)
  a_wnba_2025_u <- a_wnba_2025_u %>% mutate(player = str_trim(player))
  salary_cleaned <- salary_cleaned %>% mutate(Player = str_trim(Player))
  
  salary_stat <- left_join(a_wnba_2025_u, salary_cleaned, by = c("player" = "Player")) %>%
    mutate(
      Salary = parse_number(Salary),
      per = as.numeric(per),
      per = if_else(per < 0, 0, per)
    ) %>%
    filter(!is.na(Salary), !is.na(per))
  
  return(list(
    all_advanced = all_advanced,
    salary_stat = salary_stat
  ))
}

