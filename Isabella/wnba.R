library(rvest)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(tidyr)
library(ggplot2)
library(broom)

######        Season Level Stats        ############

# url_2022<- "https://www.basketball-reference.com/wnba/years/2022_per_game.html"
# 
# wnba_2022<- read_html(url_2022) %>% 
#   html_element("table.stats_table") %>% html_table()
# 
# wnba_2022 <- wnba_2022 %>% clean_names() %>% 
#   select(-g_2, -mp_2, -gs, -pf)
# 
# 
# url_2023 <- "https://www.basketball-reference.com/wnba/years/2023_per_game.html"
# 
# wnba_2023<- read_html(url_2023) %>% 
#   html_element("table.stats_table") %>% html_table()
# 
# wnba_2023 <- wnba_2023 %>% clean_names() %>% 
#   select(-g_2, -mp_2, -gs, -pf)
# 
# 
# url_2024 <- "https://www.basketball-reference.com/wnba/years/2024_per_game.html"
# 
# wnba_2024<- read_html(url_2024) %>% 
#   html_element("table.stats_table") %>% html_table()
# 
# wnba_2024 <- wnba_2024 %>% clean_names() %>% 
#   select(-g_2, -mp_2, -gs, -pf)
# 
# 
url_2025<- "https://www.basketball-reference.com/wnba/years/2025_per_game.html"
# 
wnba_2025<- read_html(url_2025) %>%
  html_element("table.stats_table") %>% html_table()
# 
wnba_2025 <- wnba_2025 %>% clean_names() %>% 
  select(-g_2, -mp_2, -gs, -pf)



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
wnba_2025 <- wnba_2025 %>% mutate(year = 2025)

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

# s_url_2022 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2022/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"
# 
# s_wnba_2022<- read_html(s_url_2022) %>% 
#   html_element("table.salary-stat") %>% html_table() %>% select(Player, `2022 Salary`, `2022 Signing`)
# 
# 
# s_url_2023 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2023/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"
# 
# s_wnba_2023<- read_html(s_url_2023) %>% 
#   html_element("table.salary-stat") %>% html_table()%>% select(Player, `2023 Salary`, `2023 Signing`)
# 
# 
# s_url_2024 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2024/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"
# 
# s_wnba_2024<- read_html(s_url_2024) %>% 
#   html_element("table.salary-stat") %>% html_table() %>% select(Player, `2024 Salary`, `2024 Signing`)

# 
# s_url_2025 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2025/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"
# 
# s_wnba_2025<- read_html(s_url_2025) %>% 
#   html_element("table.salary-stat") %>% html_table() %>% select(Player, `2025 Salary`, `2025 Signing`)


# s_wnba<- bind_rows(s_wnba_2022,s_wnba_2023,s_wnba_2024,s_wnba_2025)



########       Combining Datasets    ###########
wnba_salaries <- read_csv("WNBA Salaries 2.csv")

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

library(stringi)

a_wnba_2025_u <- a_wnba_2025_u %>%
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))

salary_cleaned <- salary_cleaned %>%
  mutate(Player = stri_trans_general(Player, "Latin-ASCII"),
         Player = str_trim(Player),
         Player = str_to_lower(Player),
         Player = str_replace_all(Player, "-", " "),  # Replace hyphen with space
         Player = str_replace(Player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"),
         Player = if_else(Player == "kariata diaby", "kadidja diaby", Player))

# Join after cleaning & fixing
salary_stat <- left_join(a_wnba_2025_u, salary_cleaned, by = c("player" = "Player"))

salary_stat <- salary_stat %>%
  filter(tolower(player) != "aerial powers")


########## cleaning the salary column to be numeric


salary_stat <- salary_stat %>%
  mutate(
    salary_clean = str_trim(Salary),                            # remove leading/trailing spaces
    salary_clean = str_replace_all(salary_clean, "[$,]", ""),   # remove $ and commas
    salary_clean = str_replace(salary_clean, "\\.00$", ""),     # optionally remove .00 if desired
    salary_clean = na_if(salary_clean, "N/A"),                  # turn "N/A" to NA
    salary_clean = na_if(salary_clean, ""),                     # turn blank strings to NA
    salary_clean = as.numeric(salary_clean)                     # convert to numeric
  )

#######.   box score simple stats. ############
wnba_2025 <- wnba_2025 %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))


# Check for duplicates in salary_stat
salary_stat %>%
  count(player) %>%
  filter(n > 1)

multi_rows <- wnba_2025 %>%
  group_by(player, year) %>%
  filter(n() > 1) %>%          # more than one row for that player-year
  ungroup() %>%
  select(player, year) %>%
  distinct()

# Step 2: Find last team per player-year (excluding TOT)
last_team_per_player_years <- wnba_2025 %>%
  filter(team != "TOT") %>%
  semi_join(multi_rows, by = c("player", "year")) %>%  # only for multi-row player-years
  group_by(player, year) %>%
  slice_tail(n = 1) %>%
  select(player, year, last_team = team)

# Step 3: Update TOT rows for these multi-row player-years
tot_rows_updateds <- wnba_2025 %>%
  filter(team == "TOT") %>%
  inner_join(multi_rows, by = c("player", "year")) %>%  # only TOT rows for multi-row players
  left_join(last_team_per_player_years, by = c("player", "year")) %>%
  mutate(team = last_team) %>%
  select(-last_team)

# Step 4: For multi-row player-years, keep only the updated TOT rows (drop single teams)
multi_rows_cleaneds <- tot_rows_updateds

# Step 5: For player-years with only 1 row or those not traded, keep as is
single_or_no_trades <- wnba_2025 %>%
  anti_join(multi_rows, by = c("player", "year"))

# Step 6: Combine everything
wnba_2025_u <- bind_rows(single_or_no_trades, multi_rows_cleaneds) %>%
  arrange(player, year)

salary_stat %>%
  count(player) %>%
  filter(n > 1)

salary_stat <- left_join(salary_stat, wnba_2025_u, by = c("player" = "player"))

##########
fviz_contrib(pca_result, choice = "var", axes = 1)  # for PC1
fviz_contrib(pca_result, choice = "var", axes = 2)  # for PC2

pca_result$var$coord  # PC loadings: how much each stat contributes to each PC


fviz_cluster(player_kmeans, data = scaled_players) +
  labs(title = "Player Clusters Visualization")

fviz_cluster(player_kmeans, data = scaled_players)


class(all_advanced$g)
all_advanced <- all_advanced %>%
  mutate(g = as.numeric(g))

filtered_all_advanced <- all_advanced %>% filter(year != 2025) 

filtered_all_advanced<- filtered_all_advanced %>% filter(g > 5)   

filtered_all_advanced <- filtered_all_advanced %>% 
  select(-g,-e_fg_percent, -orb_percent, -usg_percent, -dws,-ows,-ws_40, -x3p_ar,-f_tr,-d_rtg,-o_rtg)

set.seed(42)
player_kmeans <- kmeans(scaled_players, centers = 4, nstart = 25)
filtered_all_advanced$player_type <- player_kmeans$cluster

summary(scaled_players)

fviz_nbclust(scaled_players, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Number of Clusters")

player_kmeans$centers

fviz_cluster(player_kmeans, data = scaled_players) +
  labs(title = "Player Clusters Visualization")



pca_result <- PCA(scaled_players, graph = FALSE)
fviz_screeplot(pca_result)  # to see how many PCs explain most variance



filtered_all_advanced <- filtered_all_advanced %>%
  mutate(across(
    -c(player_type, year, team, player, pos),  # keep non-numeric columns as-is
    ~ as.numeric(.)
  ))


cluster_profiles <- filtered_all_advanced %>%
  group_by(player_type) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  arrange(player_type) 

print(cluster_profiles)


###############################

all_advanced_with_type <- all_advanced %>%
  left_join(
    filtered_all_advanced %>% select(player, year, player_type),
    by = c("player", "year")
  )

all_advanced_with_type <- all_advanced_with_type %>% filter(year != 2025)


# Count how many players of each player_type are on each team in each year
team_cluster_counts <- filtered_all_advanced %>%
  group_by(team, year, player_type) %>%
  summarise(player_count = n(),
            total_mp = sum(mp, na.rm = TRUE),       # total minutes by cluster on team
            avg_per = mean(per, na.rm = TRUE)) %>%  # average PER for that cluster in team
  ungroup()


# Spread player types into columns for easier comparison per team-year
team_cluster_wide <- team_cluster_counts %>%
  pivot_wider(names_from = player_type,
              values_from = c(player_count, total_mp, avg_per),
              names_glue = "type{player_type}_{.value}") %>%
  arrange(team, year)


ggplot(team_cluster_counts, aes(x = player_type, y = total_mp, fill = factor(player_type))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ team + year) +
  labs(title = "Minutes Played by Player Type per Team-Year",
       x = "Player Type", y = "Total Minutes")


#############################################
t_all_advanced <- t_all_advanced %>%
  mutate(
    w = as.numeric(w),  # ensure both columns are numeric
    l = as.numeric(l),
    win_pct = w / (w + l)  # calculate win percentage
  )
t_all_advanced <- t_all_advanced %>%
  mutate(team = case_when(
    team == "Atlanta Dream" ~ "ATL",
    team == "Chicago Sky" ~ "CHI",
    team == "Los Angeles Sparks" ~ "LAS",
    team == "Connecticut Sun" ~ "CON",
    team == "Las Vegas Aces" ~ "LVA",
    team == "Dallas Wings" ~ "DAL",
    team == "Minnesota Lynx" ~ "MIN",
    team == "Indiana Fever" ~ "IND",
    team == "Seattle Storm" ~ "SEA",
    team == "Washington Mystics" ~ "WAS",
    team == "Phoenix Mercury" ~ "PHO",
    team == "New York Liberty" ~ "NYL",
    team == "Golden State Valkyries" ~ "GSV",
  ))

t_all_advanced<- t_all_advanced %>% filter(year != 2025)

team_win_data <- t_all_advanced %>%
  select(team, year, win_pct)

team_cluster_counts <- team_cluster_counts %>%
  left_join(team_win_data, by = c("team", "year"))

#A. Correlate total minutes by player type with win %

ggplot(team_cluster_counts, aes(x = total_mp, y = win_pct, color = factor(player_type))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Total Minutes by Player Type vs. Team Win %",
       x = "Total Minutes Played by Player Type", y = "Win Percentage",
       color = "Player Type")

#B. Model team success with archetypes (optional)

team_archetype_summary <- team_cluster_counts %>%
  select(team, year, player_type, total_mp) %>%
  pivot_wider(names_from = player_type, values_from = total_mp, values_fill = 0, names_prefix = "type") %>%
  left_join(team_win_data, by = c("team", "year"))



######      This is a linear regression that estimates how a team’s win percentage is     ###########
#####  affected by the number of total minutes played by players of each cluster (player type).###########

win_model <- lm(win_pct ~ type1 + type2 + type3 + type4, data = team_archetype_summary)
summary(win_model)

# Why This Model Is Useful
# Quantifies Impact of Player Archetypes
# Helps you identify which player types (e.g., “glue players” or “stars”) actually drive winning.
# Informs Expansion Draft Strategy
# You can prioritize selecting players from clusters that the model shows have a positive correlation with team success.
# Validates Your Clustering
# If Type 3 has a strong positive coefficient, then you know your clustering effectively found a winning player archetype.
# Builds a Blueprint for a Winning Roster
# Instead of just picking the best available player, you can simulate how adding X minutes of Type 3 and Y minutes of Type 4 affects your team’s projected win %.
# 


tidy(win_model) %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Estimated Impact of Player Types on Win Percentage",
       x = "Player Type", y = "Estimated Coefficient") +
  theme_minimal()



model_data <- team_cluster_counts %>%
  select(team, year, player_type, total_mp) %>%
  pivot_wider(
    names_from = player_type,
    values_from = total_mp,
    values_fill = 0,
    names_prefix = "type"
  ) %>%
  left_join(
    t_all_advanced %>% select(team, year, win_pct),
    by = c("team", "year")
  )


model_data$predicted_win_pct <- predict(win_model)
#Predicted vs. Actual Win %
ggplot(model_data, aes(x = win_pct, y = predicted_win_pct)) +
  geom_point() +
  geom_abline(linetype = "dashed", color = "gray") +
  labs(title = "Actual vs Predicted Win %", x = "Actual", y = "Predicted") +
  theme_minimal()


########. 




#type 3 is the best, you can see teams with type 3 players that play a lot of minutes
#have the best win pct

# Step-by-Step: Simulate Protected Players
# 🔹 Step 1: Rank players within each team-year
# 
# We’ll assume teams protect players who:
#   
#   Played a lot of minutes
# Had a strong PER
# You can create a composite score:

protected_candidates <- filtered_all_advanced %>%
  group_by(team, year) %>%
  mutate(per_minute_score = scale(ws) + scale(mp)) %>%  # combines impact and playing time
  arrange(team, year, desc(per_minute_score)) %>%
  mutate(rank_within_team = row_number())

protected_candidates <- protected_candidates %>%
  mutate(protected = if_else(rank_within_team <= 5, 1, 0))




####### This is the model for only the 2025 year

players_2025 <- all_advanced %>%
  filter(year == 2025)  

players_2025 <- players_2025 %>%
  mutate(
    mp = as.numeric(mp),
    ws = as.numeric(ws),
    per = as.numeric(per)
  )

players_2025 <- players_2025 %>%
  group_by(team) %>%
  mutate(per_minute_score = scale(mp) + scale(ws)) %>%
  arrange(team, desc(per_minute_score)) %>%
  mutate(rank_within_team = row_number(),
         protected = if_else(rank_within_team <= 5, 1, 0)) %>%
  ungroup()

protected_2025 <- players_2025 %>%
  filter(protected == 1) %>%
  select(team, player, mp, per, protected) %>%
  arrange(team)

########## players in the 2024 year #####
players_2024 <- all_advanced %>%
  filter(year == 2024)  

players_2024 <- players_2024 %>%
  mutate(
    mp = as.numeric(mp),
    ws = as.numeric(ws),
    per = as.numeric(per)
  )

players_2024 <- players_2024 %>%
  group_by(team) %>%
  mutate(per_minute_score = scale(mp) + scale(ws)) %>%
  arrange(team, desc(per_minute_score)) %>%
  mutate(rank_within_team = row_number(),
         protected = if_else(rank_within_team <= 5, 1, 0)) %>%
  ungroup()

protected_2024 <- players_2024 %>%
  filter(protected == 1) %>%
  select(team, player, mp, per, protected) %>%
  arrange(team)


library(ggplot2)

ggplot(protected_2024, aes(x = reorder(player, -per), y = per, fill = team)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ team, scales = "free_x") +
  theme_minimal() +
  labs(
    title = "Protected Players by Team (2024)",
    subtitle = "Top 5 per team based on scaled MP + WS",
    x = "Player",
    y = "Player Efficiency Rating (PER)"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

team_cluster_counts <- players_2024 %>%
  group_by(team, cluster_label) %>%
  summarise(
    n_players = n(),.data = 
    total_minutes = sum(mp, na.rm = TRUE)
  ) %>%
  ungroup()


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

library(ggplot2)

ggplot(players_ranked, aes(
  x = reorder(player, per_minute_score),
  y = per_minute_score,
  fill = factor(protected)
)) +
  geom_col() +
  coord_flip() +
  facet_wrap(~team, scales = "free_y") +
  labs(
    title = "Player Rankings and Protection Status",
    x = "Player",
    y = "Per-Minute Score",
    fill = "Protected"
  ) +
  theme_minimal()

protected_players <- players_ranked %>%
  filter(protected == 1) %>%
  arrange(team)
unprotected_players <- players_ranked %>%
  filter(protected == 0) %>%
  arrange(team)

######      







##########  Decision Tree ###
# - can use this to determine what the seed will be for the expansion team
# - can use this to see which predictors are most valuable when predicting a teams seed/success
# - then using those predictors we can predict how teams are going to do in the 2025 season as of right now data
# - can we maybe apply that to see how the new expansion team is going to do


############ Random Forest Model #####
#   - predict Salary, maybe can see what varibles are most important for predicting whos protected and who i snot protected

## XGBoost Model #
# to predict the probability that a player is oging to be protected
# to predict a players performance in the next year, predicting their per/rating




