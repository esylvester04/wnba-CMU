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


s_url_2025 <- "https://herhoopstats.com/salary-cap-sheet/wnba/players/salary_2025/stats_2024/?stats_to_show=per_game&stat_to_show=pts_per_game"

s_wnba_2025<- read_html(s_url_2025) %>% 
  html_element("table.salary-stat") %>% html_table() %>% select(Player, `2025 Salary`, `2025 Signing`)


# s_wnba<- bind_rows(s_wnba_2022,s_wnba_2023,s_wnba_2024,s_wnba_2025)



########       Comnbining Datasets    ###########



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

salary_stat <- salary_stat |>
  mutate(per = as.numeric(per)) # convert back after join


library(scales)

ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  geom_point(alpha = 0.7, color = "#1f78b4", size = 3) +
  labs(
    title = "Player Efficiency Rating vs. WNBA Salary (2025 Season)",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)"
  ) +
  scale_y_continuous(labels = dollar_format(scale = 1)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5), limits = c(0, 35)) +
  theme_minimal(base_size = 14)







model <- lm(salary_clean ~ per, data = salary_stat)
salary_stat <- salary_stat %>%
  mutate(predicted_salary = predict(model, .),
         residual = salary_clean - predicted_salary,
         undervalued = residual < -5000)  # threshold can be adjusted


library(ggrepel)

ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  geom_point(aes(color = undervalued), alpha = 0.7, size = 3) +
  # geom_label_repel(data = filter(salary_stat, undervalued),
  #                  aes(label = str_to_title(player)),
  #                  size = 3,
  #                  fill = alpha("white", 0.8),
  #                  box.padding = 0.4,
  #                  point.padding = 0.2,
  #                  segment.color = "gray50") +
  geom_smooth(method = "lm", se = TRUE, linetype = "dashed", color = "gray30") +
  scale_color_manual(values = c("TRUE" = "#33a02c", "FALSE" = "#1f78b4"), guide = "none") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5), limits = c(0, 35)) +
  labs(
    title = "Undervalued WNBA Players: PER vs. Salary (2025)",
    subtitle = "Players below the dashed line (with labels) are paid less than expected",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Source: Basketball-Reference & Her Hoop Stats"
  ) +
  theme_minimal(base_size = 14)













salary_stat <- salary_stat %>%
  mutate(
    predicted_salary = predict(model, .),
    residual = salary_clean - predicted_salary,
    undervalued = residual < -5000,
    overvalued = residual > 100000,  # Adjust as needed
    top_paid = rank(-salary_clean) <= 5  # Top 5 salaries
  )
salary_stat <- salary_stat %>%
  mutate(label_type = case_when(
    undervalued ~ "Undervalued",
    top_paid ~ "Top Paid",
    overvalued ~ "Overvalued",
    TRUE ~ NA_character_
  ),
  label_player = ifelse(!is.na(label_type), str_to_title(player), NA)
  )

library(dplyr)
library(ggrepel)

# Filter salary_stat for plotting
plot_data <- salary_stat %>%
  filter(
    !is.na(per), !is.na(salary_clean),
    is.finite(per), is.finite(salary_clean),
    per >= 0, per <= 35,
    salary_clean > 0  # assuming salary should be positive
  )

# Filter label data similarly
label_data <- plot_data %>%
  filter(!is.na(label_type))

salary_stat <- salary_stat %>%
  mutate(per = if_else(per < 0, 0, per))


# Example logic to pick top paid and underpaid players
top_paid <- salary_stat %>% top_n(5, salary_clean)
underpaid <- salary_stat %>% filter(salary_clean < 60000 & per > 15)

ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  geom_point(data = underpaid, color = "red", size = 4) +
  geom_point(data = top_paid, color = "green", size = 4) +
  geom_label_repel(data = bind_rows(underpaid, top_paid),
                   aes(label = player),
                   size = 3,
                   box.padding = 0.3,
                   point.padding = 0.2,
                   segment.color = "gray50") +
  labs(
    title = "Player Efficiency Rating vs. WNBA Salary (2025 Season)",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Source: Basketball-Reference & Her Hoop Stats"
  ) +

  # geom_hline(yintercept = 65000, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = 80000, linetype = "dashed", color = "black") +
  # geom_hline(yintercept = 208000, linetype = "dashed", color = "green") +
  # 
  scale_y_continuous(
    labels = dollar_format(),
    breaks = seq(0, 300000, by = 40000)
  ) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  coord_cartesian(xlim = c(0, 35)) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 15))




ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  # Highlight rookie salary range
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 66000, ymax = 78000,
           fill = "red", alpha = 0.3) +
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +  # base layer
  
  # Highlight underpaid in red
  geom_point(data = underpaid, color = "red") +
  
  # Highlight top paid in green
  geom_point(data = top_paid, color = "green") +
  
  # Highlight star players in blue
  geom_point(data = star_players_df, color = "green") +
  
  # Label all highlighted players
  geom_label_repel(data = highlight_all,
                   aes(label = player),
                   size = 3,
                   box.padding = 0.3,
                   point.padding = 0.2,
                   segment.color = "gray50", 
                   alpha = 0.4) +
  
  # Add dashed lines for rookie salary range
  geom_hline(yintercept = 65000, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 80000, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 208000, linetype = "dashed", color = "green") +
  
  # Optional: Add annotation text
  annotate("text", x = 34, y = 72000, label = "Rookie Salary Range: 66k-78k ", hjust = 1, vjust = -0.5, size = 4) +
  annotate("text", x = 34, y = 100000, label = "Regular Max: $208k", hjust = 1, vjust = -0.5, size = 4) +
  
  labs(
    title = "Player Efficiency Rating vs. WNBA Salary (2025 Season)",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Source: Basketball-Reference & Her Hoop Stats"
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    breaks = seq(0, 300000, by = 40000)
  ) +
  coord_cartesian(xlim = c(0, 35)) +
  theme_minimal(base_size = 14)









ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  # Highlight rookie salary range with background color
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 66000, ymax = 78000,
           fill = "red", alpha = 0.15) +
  
  # Main scatterplot
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  # geom_point(data = underpaid, color = "red", size = 3) +
  # geom_point(data = top_paid, color = "green", size = 3) +
  # geom_point(data = star_players_df, color = "green", size = 3) +
  
  # Labels for key players
  geom_label_repel(data = salary_stat,
                   aes(label = player),
                   size = 3,
                   box.padding = 0.3,
                   point.padding = 0.2,
                   segment.color = "gray50") +
  
  # Horizontal lines for salary thresholds
  geom_hline(yintercept = 65000, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 80000, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 208000, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 242000, linetype = "dashed", color = "blue") +
  
  # Improved salary range annotations as labels to the side
  geom_label(aes(x = 40, y = 80000, label = "Rookie Salary Range\n($66k–$78k)"),
             fill = "white", label.size = 0.2, size = 3, hjust = 0, 
             alpha = 0.3) +
  geom_label(aes(x = 40, y = 190000, label = "Regular Max Salary\n($208k)"),
             fill = "white", label.size = 0.2, size = 3, hjust = 0) +
    geom_label(aes(x = 40, y = 242000, label = "Supermax Salary\n($242k)"),
               fill = "white", label.size = 0.2, size = 3, hjust = 0, color = "blue") +
  
  # Titles and axes
  labs(
    title = "Player Efficiency Rating vs. WNBA Salary (2025 Season)",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Source: Basketball-Reference & Her Hoop Stats"
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    breaks = seq(0, 300000, by = 40000)
  ) +
  coord_cartesian(xlim = c(0, 50)) +  # Added space on right for labels
  theme_classic(base_size = 14)



#####################################################################################################################
##############################################################################

# Assuming salary_stat has up-to-date PER values
top15_per <- salary_stat %>%
  arrange(desc(per)) %>%
  slice_head(n = 15)

ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  # Rookie range
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 66000, ymax = 78000,
           fill = "red", alpha = 0.15) +
  
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  
  # Label only top 15 players by PER
  geom_label_repel(
    data = top15_per,
    aes(label = player),
    size = 3,
    box.padding = 0.3,
    point.padding = 0.2,
    segment.color = "gray50",
    max.overlaps = 20
  ) +
  
  # Salary thresholds
  geom_hline(yintercept = 65000,  linetype = "dashed", color = "red",   alpha = 0.5) +
  geom_hline(yintercept = 80000,  linetype = "dashed", color = "red",   alpha = 0.5) +
  geom_hline(yintercept = 208000, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 242000, linetype = "dashed", color = "blue") +
  
  # Range annotations
  geom_label(aes(x = max(salary_stat$per) + 5, y = 80000,
                 label = "Rookie Salary Range\n($66k–$78k)"),
             fill = "white", label.size = 0.2, size = 3, hjust = 0, alpha = 0.3) +
  geom_label(aes(x = max(salary_stat$per) + 5, y = 190000,
                 label = "Regular Max Salary\n($208k)"),
             fill = "white", label.size = 0.2, size = 3, hjust = 0) +
  geom_label(aes(x = max(salary_stat$per) + 5, y = 242000,
                 label = "Supermax Salary\n($242k)"),
             fill = "white", label.size = 0.2, size = 3, hjust = 0, color = "blue") +
  
  labs(
    title = "Top 15 WNBA Players by PER (2025) vs. Salary",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Labels: Top 15 PER | Source: Basketball‑Reference & Her Hoop Stats"
  ) +
  scale_y_continuous(labels = scales::dollar_format(),
                     breaks = seq(0, 300000, by = 40000)) +
  scale_x_continuous(breaks = seq(0, 35, by = 5),
                     limits = c(0, max(salary_stat$per) + 10)) +
  theme_classic(base_size = 14)






###########################################################################################



# Define the nine star players to highlight
star_players <- c(
  "caitlin clark", "arike ogunbowale", "a'ja wilson",
  "breanna stewart", "aliyah boston", "sabrina ionescu",
  "napheesa collier", "paige bueckers", "kelsey plum"
)

# Filter the data for labeling
star_df <- salary_stat %>% filter(player %in% star_players)

star_df <-star_df <- star_df %>%
  mutate(
    nudge_x = case_when(
      player == "arike ogunbowale"  ~ -2,
      # player == "a'ja wilson"       ~ 1,
      # player == "breanna stewart"   ~ 2,
      # player == "aliyah boston"     ~ -3,
      # player == "sabrina ionescu"   ~ 1.5,
      player == "napheesa collier"  ~ 10,
      player == "paige bueckers"    ~ 10,
      player == "kelsey plum"       ~ -20,
      TRUE                         ~ 0
    ),
    nudge_y = case_when(
      player == "arike ogunbowale"  ~ -5,
      # player == "a'ja wilson"       ~ 10,
      # player == "breanna stewart"   ~ 7,
      # player == "aliyah boston"     ~ -8,
      player == "sabrina ionescu"   ~ 10,
      player == "napheesa collier"  ~ -5,
      player == "paige bueckers"    ~ 30,
      player == "kelsey plum"       ~ 10,
      TRUE                         ~ 0
    )
  )

# model data scaled 
#write.csv(salary_stat, "eda_df.csv", row.names = FALSE)

ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  # Highlight rookie salary range
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 66000, ymax = 78000,
           fill = "red", alpha = 0.15) +
  
  # Base scatter
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  
  # Label only specified stars
  geom_label_repel(
    data = star_df,
    aes(label = player),
    size = 3.5,
    box.padding = 0.3,
    point.padding = 0.2,
    segment.color = "gray50",
    segment.size = 0.7,
    min.segment.length = 0,
    direction = "y", # prefer vertical adjustment to avoid overlap
    alpha = 0.7
  ) +

  
  # Lines at key salary thresholds
  geom_hline(yintercept = 66000,  linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 78000,  linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 214000, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 250000, linetype = "dashed", color = "blue") +
  
  # Annotate salary ranges on the right
  # geom_label(aes(x = max(salary_stat$per) + 5, y = 80000,
  #                label = "Rookie Salary Range\n($66k–$78k)"),
  #            fill = "white", label.size = 0.2, size = 3, hjust = 0, alpha = 0.3) +
  # geom_label(aes(x = max(salary_stat$per) + 5, y = 190000,
  #                label = "Regular Max Salary\n($208k)"),
  #            fill = "white", label.size = 0.2, size = 3, hjust = 0) +
  # geom_label(aes(x = max(salary_stat$per) + 5, y = 242000,
  #                label = "Supermax Salary\n($242k)"),
  #            fill = "white", label.size = 0.2, size = 3, hjust = 0, color = "blue") +
  
  # Titles and formatting
  labs(
    title = "Player Efficiency vs WNBA Salary 2025",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
  ) +
  scale_y_continuous(labels = dollar_format(),
                     breaks = seq(0, 300000, by = 40000)) +
  scale_x_continuous(breaks = seq(0, max(salary_stat$per) + 5, by = 5),
                     limits = c(0, max(salary_stat$per) + 10)) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(face = "bold")
  )



ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  # Highlight rookie salary range
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 66000, ymax = 78000,
           fill = "red", alpha = 0.15) +
  # Base scatter
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  geom_hline(yintercept = 66000,  linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 78000,  linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 214000, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 250000, linetype = "dashed", color = "blue") +
  labs(
    title = "Player Efficiency Rating vs WNBA Salary 2025",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
  ) +
  scale_y_continuous(labels = dollar_format(),
                     breaks = seq(0, 300000, by = 40000)) +
  scale_x_continuous(breaks = seq(0, max(salary_stat$per) + 5, by = 5),
                     limits = c(0, max(salary_stat$per) + 10)) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(face = "bold")
  )


ggplot(salary_stat, aes(x = per, y = salary_clean)) +
  
  # Base scatter
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  geom_smooth(method = lm, se = TRUE, color = "black", line = "dashed") +
  labs(
    title = "Player Efficiency vs WNBA Salary 2025",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
  ) +
  scale_y_continuous(labels = dollar_format(),
                     breaks = seq(0, 300000, by = 40000)) +
  scale_x_continuous(breaks = seq(0, max(salary_stat$per), by = 5),
                     limits = c(0, max(salary_stat$per) + 10)) +
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    axis.title = element_text(face = "bold")
  )












library(ggplot2)
library(scales)
library(plotly)

p <- ggplot(salary_stat, aes(x = per, y = salary_clean, text = player)) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 66000, ymax = 78000,
           fill = "red", alpha = 0.15) +
  
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  geom_point(data = underpaid, color = "#1f78b4", size = 3) +
  geom_point(data = top_paid, color = "#1f78b4", size = 3) +
  geom_point(data = star_players, color = "#1f78b4", size = 3) +
  
  # Static salary range labels at left side or just inside plot, to avoid pushing plot width
  annotate("text", x = 28, y = 73000, label = "Rookie Range\n($66k–$78k)",
           size = 4, hjust = 0, color = "red", fontface = "bold") +
  annotate("text", x = 5, y = 214000, label = "Regular Max\n($214k)",
           size = 4, hjust = 0, color = "green", fontface = "bold") +
  annotate("text", x = 5, y = 249000, label = "Supermax\n($249k)",
           size = 4, hjust = 0, color = "blue", fontface = "bold") +
  
  geom_hline(yintercept = 65000, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 80000, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 214000, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 249000, linetype = "dashed", color = "blue") +
  
  labs(
    title = "Player Efficiency Rating vs. WNBA Salary (2025 Season)",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Source: Basketball-Reference & Her Hoop Stats"
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    breaks = seq(0, 300000, by = 40000)
  ) +
  scale_x_continuous(breaks = seq(0, 35, by = 10)) +
  theme_classic(base_size = 14)

ggplotly(p, tooltip = c("text", "x", "y"))



p <- ggplot(salary_stat, aes(x = per, y = salary_clean,
                             text = paste0("Player: ", player,
                                           "<br>PER: ", round(per, 1),
                                           "<br>Salary: $", format(salary_clean, big.mark = ",")))) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = 66000, ymax = 78000,
           fill = "red", alpha = 0.15) +
  
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  # geom_point(data = underpaid, aes(x = per, y = salary_clean), color = "#1f78b4", size = 3) +
  # geom_point(data = top_paid, aes(x = per, y = salary_clean), color = "#1f78b4", size = 3) +
  # geom_point(data = star_players_df, aes(x = per, y = salary_clean), color = "#1f78b4", size = 3) +
  
  annotate("text", x = 28, y = 73000, label = "Rookie Range\n($66k–$78k)",
           size = 4, hjust = 0, color = "red", fontface = "bold") +
  annotate("text", x = 5, y = 214000, label = "Regular Max\n($214k)",
           size = 4, hjust = 0, color = "green", fontface = "bold") +
  annotate("text", x = 5, y = 249000, label = "Supermax\n($249k)",
           size = 4, hjust = 0, color = "blue", fontface = "bold") +
  
  geom_hline(yintercept = 65000, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 80000, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = 214000, linetype = "dashed", color = "green") +
  geom_hline(yintercept = 249000, linetype = "dashed", color = "blue") +
  
  labs(
    title = "Player Efficiency Rating vs. WNBA Salary (2025 Season)",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Source: Basketball-Reference & Her Hoop Stats"
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    breaks = seq(0, 300000, by = 40000)
  ) +
  scale_x_continuous(breaks = seq(0, 35, by = 10)) +
  theme_classic(base_size = 14)

ggplotly(p, tooltip = "text")





library(ggplot2)
library(plotly)
library(scales)

# Add any custom thresholds as constants
rookie_min <- 66000
rookie_max <- 78000
regular_max <- 214000
supermax <- 249000

# Create ggplot
p <- ggplot(salary_stat, aes(x = per, y = salary_clean,
                             text = paste0("Player: ", player,
                                           "<br>PER: ", round(per, 1),
                                           "<br>Salary: $", format(salary_clean, big.mark = ",")))) +
  
  # Rookie salary range shading
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymin = rookie_min, ymax = rookie_max,
           fill = "red", alpha = 0.15) +
  
  # Main scatterplot
  geom_point(alpha = 0.7, size = 3, color = "#1f78b4") +
  
  # # Threshold annotations
  # annotate("text", x = 28, y = 73000, label = "Rookie Range\n($66k–$78k)",
  #          size = 4, hjust = 0, color = "red", fontface = "bold") +
  # annotate("text", x = 5, y = regular_max + 6000, label = "Regular Max\n($214k)",
  #          size = 4, hjust = 0, color = "green", fontface = "bold") +
  # annotate("text", x = 5, y = supermax + 6000, label = "Supermax\n($249k)",
  #          size = 4, hjust = 0, color = "blue", fontface = "bold") +
  
  # Threshold lines
  geom_hline(yintercept = rookie_min, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = rookie_max, linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = regular_max, linetype = "dashed", color = "green") +
  geom_hline(yintercept = supermax, linetype = "dashed", color = "blue") +
  
  # Labels & theme
  labs(
    title = "Player Efficiency Rating vs. WNBA Salary (2025 Season)",
    x = "Player Efficiency Rating (PER)",
    y = "Salary (USD)",
    caption = "Source: Basketball-Reference & Her Hoop Stats"
  ) +
  scale_y_continuous(
    labels = dollar_format(),
    breaks = seq(0, 300000, by = 40000)
  ) +
  scale_x_continuous(breaks = seq(0, 35, by = 5)) +
  coord_cartesian(xlim = c(0, 35)) +
  theme_classic(base_size = 14)

# Convert to interactive
ggplotly(p, tooltip = "text")

