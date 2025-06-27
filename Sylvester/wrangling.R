library(rvest)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyverse)

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
# url_2024 <- "https://www.basketball-reference.com/wnba/years/2022_per_game.html"
# 
# wnba_2024<- read_html(url_2024) %>% 
#   html_element("table.stats_table") %>% html_table()
# 
# wnba_2024 <- wnba_2024 %>% clean_names() %>% 
#   select(-g_2, -mp_2, -gs, -pf)
# 
# 
# url_2025<- "https://www.basketball-reference.com/wnba/years/2022_per_game.html"
# 
# wnba_2025<- read_html(url_2025) %>% 
#   html_element("table.stats_table") %>% html_table()
# 
# wnba_2025 <- wnba_2025 %>% clean_names() %>% 
#   select(-g_2, -mp_2, -gs, -pf)



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



#########         Salary Data         ############


selected_players <- c("A'ja Wilson", "Breanna Stewart", "Sabrina Ionescu")

all_advanced |>
  filter(player %in% selected_players) |>
  ggplot(aes(x = year, 
             y = per, 
             color = player)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "PER Over Time for Selected Players",
       x = "Year",
       y = "Player Efficiency Rating (PER)") +
  theme_minimal()




library(tidyr)

stats_to_track <- c("per", "e_fg_percent", "ts_percent")
player_name <- "A'ja Wilson"

all_advanced |>
  filter(player == player_name) |>
  select(year, all_of(stats_to_track)) |>
  pivot_longer(cols = all_of(stats_to_track),
               names_to = "stat",
               values_to = "value") |>
  ggplot(aes(x = year, y = value, color = stat)) +
  geom_line() +
  geom_point() +
  labs(title = paste("Stat Growth Over Time for", player_name),
       x = "Year",
       y = "Value",
       color = "Statistic") +
  theme_minimal()

# PCA:
# group by team, clustering analysis for positions on a single team

library(cluster)
library(factoextra)
library(ggrepel)

# Clean version of all_advanced — tidyverse style
all_advanced_clean <- all_advanced |> 
  mutate(across(c("g":"ws_40"),
                as.numeric)) |>
  filter(team == "GSV") |>  
  drop_na() |>
  clean_names()

View(all_advanced_clean)

# Prepare numeric matrix for PCA 
pca_data <- all_advanced_clean |>
  select(where(is.numeric), -year) |>
  scale()

# Run PCA
pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Add player info back
pca_scores <- as_tibble(pca$x[, 1:2]) |>
  bind_cols(all_advanced_clean |>
              select(player, team, year) |>
              slice(1:nrow(pca$x)))


ggplot(pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA of Golden State Valkyeries Advanced Stats (first 15 games)",
       subtitle = "First two principal components",
       x = "PC1",
       y = "PC2") +
  theme_minimal()



fviz_pca_biplot(pca,
                label = "var",          # label variables (stats)
                alpha.ind = 0.25,       # transparency of points (players)
                alpha.var = 0.75,       # transparency of variable arrows
                labelsize = 5,          # size of variable labels
                col.var = "darkblue",   # color of variable arrows and labels
                repel = TRUE)           # avoid overlapping labels


# How many pcas should we use?


