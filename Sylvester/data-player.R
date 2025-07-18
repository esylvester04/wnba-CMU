library(rvest)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(stringr)

##########        ADVANCED STATS   ###########

a_url_2022 <- "https://www.basketball-reference.com/wnba/years/2022_advanced.html"

a_wnba_2022<- read_html(a_url_2022) %>% 
  html_element("table.stats_table") %>% html_table()

a_wnba_2022 <- a_wnba_2022 %>% clean_names() %>% 
  select(-g_2, -mp_2, -x)


a_url_2023 <- "https://www.basketball-reference.com/wnba/years/2023_advanced.html"

a_wnba_2023<- read_html(a_url_2023) %>% 
  html_element("table.stats_table") %>% html_table()

a_wnba_2023 <- a_wnba_2023 %>% clean_names() %>% 
  select(-g_2, -mp_2, -x)


a_url_2024 <- "https://www.basketball-reference.com/wnba/years/2024_advanced.html"

a_wnba_2024<- read_html(a_url_2024) %>% 
  html_element("table.stats_table") %>% html_table()

a_wnba_2024 <- a_wnba_2024 %>% clean_names() %>% 
  select(-g_2, -mp_2, -x)


a_url_2025 <- "https://www.basketball-reference.com/wnba/years/2025_advanced.html"

a_wnba_2025<- read_html(a_url_2025) %>% 
  html_element("table.stats_table") %>% html_table()

a_wnba_2025 <- a_wnba_2025 %>% clean_names() %>% 
  select(-g_2, -mp_2, -x)


#########           Adding a Year Column        #########

# wnba_2022 <- wnba_2022 %>% mutate(year = 2022)
# wnba_2023 <- wnba_2023 %>% mutate(year = 2023)
# wnba_2024 <- wnba_2024 %>% mutate(year = 2024)
# wnba_2025 <- wnba_2025 %>% mutate(year = 2025)

a_wnba_2022 <- a_wnba_2022 %>% mutate(year = 2022)
a_wnba_2023 <- a_wnba_2023 %>% mutate(year = 2023)
a_wnba_2024 <- a_wnba_2024 %>% mutate(year = 2024)
a_wnba_2025 <- a_wnba_2025 %>% mutate(year = 2025)


#########          Stacking sets together     #############

all_advanced <- bind_rows( a_wnba_2022, a_wnba_2023, a_wnba_2024, a_wnba_2025)

# all_season_stats <- bind_rows( wnba_2022, wnba_2023, wnba_2024, wnba_2025)

#########         Removing Player Rows       ###############

all_advanced <- all_advanced[!all_advanced$player== "Player",]


# all_season_stats <- all_season_stats[!all_season_stats$player== "Player",]


########      Removing player-year multiple rows (trades)       ############
#                 keeping the TOT of that year

# Step 1: Identify players-years with multiple rows (traded players)
multi_rows <- all_advanced %>%
  group_by(player, year) %>%
  filter(n() > 1) %>%          # more than one row for that player-year
  ungroup() %>%
  select(player, year) %>%
  distinct()

# Step 2: Find last team per player-year (excluding TOT)
last_team_per_player_year <- all_advanced %>%
  filter(team != "TOT") %>%
  semi_join(multi_rows, by = c("player", "year")) %>%  # only for multi-row player-years
  group_by(player, year) %>%
  slice_tail(n = 1) %>%
  select(player, year, last_team = team)

# Step 3: Update TOT rows for these multi-row player-years
tot_rows_updated <- all_advanced %>%
  filter(team == "TOT") %>%
  inner_join(multi_rows, by = c("player", "year")) %>%  # only TOT rows for multi-row players
  left_join(last_team_per_player_year, by = c("player", "year")) %>%
  mutate(team = last_team) %>%
  select(-last_team)

# Step 4: For multi-row player-years, keep only the updated TOT rows (drop single teams)
multi_rows_cleaned <- tot_rows_updated

# Step 5: For player-years with only 1 row or those not traded, keep as is
single_or_no_trade <- all_advanced %>%
  anti_join(multi_rows, by = c("player", "year"))

# Step 6: Combine everything
all_advanced <- bind_rows(single_or_no_trade, multi_rows_cleaned) %>%
  arrange(player, year)


# page <- read_html("https://www.basketball-reference.com/wnba/years/2022.html")
# # Find all table elements
# tables <- page %>% html_elements("table")
# table_ids <- tables %>% html_attr("id")
# print(table_ids)


#########        Team Data            ########

t_url_2022<- "https://www.basketball-reference.com/wnba/years/2022.html"

t_wnba_2022<- read_html(t_url_2022) %>% 
  html_element("#per_game-team") %>% html_table() %>% clean_names()


t_url_2023<- "https://www.basketball-reference.com/wnba/years/2023.html"

t_wnba_2023<- read_html(t_url_2023) %>% 
  html_element("#per_game-team") %>% html_table() %>% clean_names()


t_url_2024<- "https://www.basketball-reference.com/wnba/years/2024.html"

t_wnba_2024<- read_html(t_url_2024) %>% 
  html_element("#per_game-team") %>% html_table()%>% clean_names()


t_url_2025<- "https://www.basketball-reference.com/wnba/years/2025.html"

t_wnba_2025<- read_html(t_url_2025) %>% 
  html_element("#per_game-team") %>% html_table()%>% clean_names()


#######       Advanced Team Stats         ######

a_t_url_2022 <- "https://www.basketball-reference.com/wnba/years/2022.html"

a_t_wnba_2022 <- read_html(a_t_url_2022) %>%
  html_element("#advanced-team") %>%
  html_table(header = FALSE) # Read the table WITHOUT headers

colnames(a_t_wnba_2022) <- as.character(a_t_wnba_2022[2, ])

# remove first two rows (the header rows)
a_t_wnba_2022 <- a_t_wnba_2022[-c(1,2), ] %>% clean_names()


a_t_url_2023 <- "https://www.basketball-reference.com/wnba/years/2023.html"

a_t_wnba_2023 <- read_html(a_t_url_2023) %>%
  html_element("#advanced-team") %>%
  html_table(header = FALSE) # Read the table WITHOUT headers

colnames(a_t_wnba_2023) <- as.character(a_t_wnba_2023[2, ])

# remove first two rows (the header rows)
a_t_wnba_2023 <- a_t_wnba_2023[-c(1,2), ] %>% clean_names()


a_t_url_2024 <- "https://www.basketball-reference.com/wnba/years/2024.html"

a_t_wnba_2024 <- read_html(a_t_url_2024) %>%
  html_element("#advanced-team") %>%
  html_table(header = FALSE) # Read the table WITHOUT headers

colnames(a_t_wnba_2024) <- as.character(a_t_wnba_2024[2, ])

# remove first two rows (the header rows)
a_t_wnba_2024 <- a_t_wnba_2024[-c(1,2), ] %>% clean_names()


a_t_url_2025 <- "https://www.basketball-reference.com/wnba/years/2025.html"

a_t_wnba_2025 <- read_html(a_t_url_2025) %>%
  html_element("#advanced-team") %>%
  html_table(header = FALSE) # Read the table WITHOUT headers

colnames(a_t_wnba_2025) <- as.character(a_t_wnba_2025[2, ])

# remove first two rows (the header rows)
a_t_wnba_2025 <- a_t_wnba_2025[-c(1,2), ] %>% clean_names()


#########           Adding a Year Column & If made playoffs       #########

t_wnba_2022 <- t_wnba_2022 %>% mutate(year = 2022)
t_wnba_2023 <- t_wnba_2023 %>% mutate(year = 2023)
t_wnba_2024 <- t_wnba_2024 %>% mutate(year = 2024)
t_wnba_2025 <- t_wnba_2025 %>% mutate(year = 2025)

a_t_wnba_2022 <- a_t_wnba_2022 %>% mutate(year = 2022)
a_t_wnba_2023 <- a_t_wnba_2023 %>% mutate(year = 2023)
a_t_wnba_2024 <- a_t_wnba_2024 %>% mutate(year = 2024)
a_t_wnba_2025 <- a_t_wnba_2025 %>% mutate(year = 2025)



#t_all_advanced <- t_all_advanced %>% mutate()



#Las Vegace Aces, Conneticut Sun, Chicago Sky, Seattle Storm, 
#New York Liberty, Dallas Wings, Phoenix Mercury, Washington Mystics


#########          Stacking team sets together     #############

t_all_advanced <- bind_rows( a_t_wnba_2022, a_t_wnba_2023, a_t_wnba_2024, a_t_wnba_2025) %>% 
  select(-na_3,-na, -na_2, -arena)
t_per_game <- bind_rows( t_wnba_2022, t_wnba_2023, t_wnba_2024, t_wnba_2025)

#########         Removing League Avg. Rows       ###############

t_all_advanced <- t_all_advanced[!t_all_advanced$team== "League Average",]

t_per_game <- t_per_game[!t_per_game$team== "League Average",]

##########      Adding Playoff Column         #########

t_all_advanced <- t_all_advanced %>%
  mutate(
    made_playoffs = if_else(grepl("\\*$", team), 1, 0),
    team = gsub("\\*$", "", team)  # optional: remove * from name
  )

t_per_game <- t_per_game %>%
  mutate(
    made_playoffs = if_else(grepl("\\*$", team), 1, 0),
    team = gsub("\\*$", "", team)  # optional: remove * from name
  )

#########         Salary Data         ############

s_url_2022 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2022/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"

s_wnba_2022<- read_html(s_url_2022) %>% 
  html_element("table.salary-stat") %>% html_table() %>% select(Player, `2022 Salary`, `2022 Signing`)


s_url_2023 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2023/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"

s_wnba_2023<- read_html(s_url_2023) %>% 
  html_element("table.salary-stat") %>% html_table()%>% select(Player, `2023 Salary`, `2023 Signing`)


s_url_2024 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2024/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"

s_wnba_2024<- read_html(s_url_2024) %>% 
  html_element("table.salary-stat") %>% html_table() %>% select(Player, `2024 Salary`, `2024 Signing`)


s_url_2025 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2025/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"

s_wnba_2025<- read_html(s_url_2025) %>% 
  html_element("table.salary-stat") %>% html_table() %>% select(Player, `2025 Salary`, `2025 Signing`)


s_wnba<- bind_rows(s_wnba_2022,s_wnba_2023,s_wnba_2024,s_wnba_2025)

#####################################
####################################.      Combing datasets
#####################################


wnba_salaries <- read_csv("salaries.csv")


multi_rows <- a_wnba_2025 %>%
  group_by(player, year) %>%
  filter(n() > 1) %>%          # more than one row for that player-year
  ungroup() %>%
  select(player, year) %>%
  distinct()

# Step 2: Find last team per player-year (excluding TOT)
last_team_per_player_years <- a_wnba_2025 %>%
  filter(team != "TOT") %>%
  semi_join(multi_rows, by = c("player", "year")) %>%  # only for multi-row player-years
  group_by(player, year) %>%
  slice_tail(n = 1) %>%
  select(player, year, last_team = team)

# Step 3: Update TOT rows for these multi-row player-years
tot_rows_updateds <- a_wnba_2025 %>%
  filter(team == "TOT") %>%
  inner_join(multi_rows, by = c("player", "year")) %>%  # only TOT rows for multi-row players
  left_join(last_team_per_player_years, by = c("player", "year")) %>%
  mutate(team = last_team) %>%
  select(-last_team)

# Step 4: For multi-row player-years, keep only the updated TOT rows (drop single teams)
multi_rows_cleaneds <- tot_rows_updateds

# Step 5: For player-years with only 1 row or those not traded, keep as is
single_or_no_trades <- a_wnba_2025 %>%
  anti_join(multi_rows, by = c("player", "year"))

# Step 6: Combine everything
a_wnba_2025_u <- bind_rows(single_or_no_trades, multi_rows_cleaneds) %>%
  arrange(player, year)

salary_recent <- wnba_salaries %>% filter(Year == 2025)
salary_cleaned <- salary_recent %>%
  distinct(Player, Year, .keep_all = TRUE)

a_wnba_2025_u <- a_wnba_2025_u %>%
  mutate(player = str_trim(player))

salary_cleaned <- salary_cleaned %>%
  mutate(Player = str_trim(Player))


salary_stat <- left_join(a_wnba_2025_u, 
                         salary_cleaned, by = c("player" = "Player")) 

salary_stat <- salary_stat |>
  mutate(
    Salary = parse_number(Salary),   # in case it's not already done
    per = as.numeric(per)            # this is the key fix
  ) %>%
  filter(!is.na(Salary), !is.na(per))

salary_stat <- salary_stat |>
  mutate(per = if_else(per < 0, 0, per))



#####################################
####################################.      
#####################################



class(all_advanced$g)
all_advanced <- all_advanced %>%
  mutate(g = as.numeric(g))

filtered_all_advanced <- all_advanced %>% filter(year != 2025) 

filtered_all_advanced<- filtered_all_advanced %>% filter(g > 5)   

filtered_all_advanced <- filtered_all_advanced %>% 
  select(-g,-e_fg_percent, -orb_percent, -usg_percent, -dws,-ows,-ws_40, -x3p_ar,-f_tr,-d_rtg,-o_rtg)


# Convert all non-ID columns to numeric safely
filtered_all_advanced <- filtered_all_advanced %>%
  mutate(across(
    -c(player, year, team, pos),  # leave these as-is
    ~ as.numeric(as.character(.))              # ensures factors/characters become numeric
  ))

# Optional: Remove rows with any NAs
filtered_all_advanced <- filtered_all_advanced %>%
  drop_na()

# Start from filtered_all_advanced (already cleaned and numeric-converted earlier)
# Select only numeric columns for clustering
scaled_players <- filtered_all_advanced %>%
  select(where(is.numeric)) %>%      # keep numeric columns only
  drop_na() %>%                      # drop rows with NA values
  scale() %>%                        # scale the numeric values
  as.data.frame()                    # turn back into data.frame



library(dplyr)
library(stringr)

# Step 1: Identify players active in 2025
active_players_2025 <- all_advanced |>
  filter(year == 2025) |>
  pull(player) |>
  unique() |>
  as.data.frame()    

# # Step 2: Filter multi-year data for those players only
# active_player_history <- all_advanced %>%
#   filter(player %in% active_players_2025)
# 
# # Step 3: Choose variables to aggregate (add/remove as needed)
# agg_vars <- c("g", "mp", "per", "ts_percent", "e_fg_percent", "x3p_ar", "f_tr", "orb_percent", "trb_percent", 
#              "ast_percent", "stl_percent", "blk_percent","tov_percent", "usg_percent", 
#              "o_rtg", "d_rtg", "ows", "dws", "ws", "ws_40")
# 
# # Step 4: Compute simple averages (no weighting)
# player_aggregates <- active_player_history %>%
#   select(player, year, all_of(agg_vars)) %>%
#   mutate(across(all_of(agg_vars), ~ as.numeric(as.character(.)))) %>%  # Force numeric
#   group_by(player) %>%
#   summarise(across(all_of(agg_vars), ~ mean(.x, na.rm = TRUE))) %>%
#   ungroup()
# 
# # Step 5: Clean player names in both datasets
# player_aggregates <- player_aggregates %>%
#   mutate(player = str_trim(player))
# 
# salary_cleaned <- salary_stat %>%
#   select(player, Salary) %>%
#   mutate(player = str_trim(player))
# 
# # Step 6: Merge with 2025 salary data
# three_seasons_df <- left_join(player_aggregates, salary_cleaned, by = "player") |>
#   filter(!is.na(Salary)) |>  # Only keep players with known salary |>
#   rename(salary_2025 = Salary)




  

