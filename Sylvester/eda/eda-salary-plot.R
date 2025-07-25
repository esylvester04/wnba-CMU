# salary_plot.R

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
library(ggrepel)
library(plotly)
library(scales)

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
  mutate(per = as.numeric(per))  # convert back after join





# Filter salary_stat for plotting
plot_data <- salary_stat %>%
  filter(
    !is.na(per), !is.na(salary_clean),
    is.finite(per), is.finite(salary_clean),
    per >= 0, per <= 35,
    salary_clean > 0  # assuming salary should be positive
  )
# 
# # Filter label data similarly
# label_data <- plot_data %>%
#   filter(!is.na(label_type))

salary_stat <- salary_stat %>%
  mutate(per = if_else(per < 0, 0, per))


# Example logic to pick top paid and underpaid players
top_paid <- salary_stat %>% top_n(5, salary_clean)
underpaid <- salary_stat %>% filter(salary_clean < 60000 & per > 15)



rookie_min <- 66000
rookie_max <- 78000
regular_max <- 214000
supermax <- 249000



make_salary_plot <- function() {
p <-  ggplot(salary_stat, aes(x = per, y = salary_clean,
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
  
    # Threshold annotations
    annotate("text", x = 5, y = regular_max + 6000, label = "Regular Max\n($214k)",
             size = 4, hjust = 0, color = "green", fontface = "bold") +
    annotate("text", x = 5, y = supermax + 6000, label = "Supermax\n($249k)",
             size = 4, hjust = 0, color = "blue", fontface = "bold") +
    annotate("text", x = 28, y = 73000, label = "Rookie Range\n($66k–$78k)",
           size = 4, hjust = 0, color = "red", fontface = "bold") +
  
  # Add subtitle as annotation near top-left, adjust y for placement
  annotate("text", x = 10, y = 285000, label = "Salaries and Statistics from the 2025 WNBA season",
           hjust = 0, size = 5, color = "gray40") +
    
    # Threshold lines
    geom_hline(yintercept = rookie_min, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = rookie_max, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = regular_max, linetype = "dashed", color = "green") +
    geom_hline(yintercept = supermax, linetype = "dashed", color = "blue") +
    
    # Labels & theme
    labs(
      title = "Player Efficiency Rating vs. WNBA Salary",
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
  theme_classic(base_size = 14)+
  theme(plot.title = element_text(size = 27, face = "bold", hjust = -2))  


  
  # Convert to interactive
  ggplotly(p, tooltip = "text")
}



model_plot <- function() {
ggplot(salary_stat, aes(x = per, y = salary_clean,
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
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed", size = 1) +
    
    # Threshold annotations
    annotate("text", x = 5, y = regular_max + 6000, label = "Regular Max\n($214k)",
             size = 4, hjust = 0, color = "green", fontface = "bold") +
    annotate("text", x = 5, y = supermax + 6000, label = "Supermax\n($249k)",
             size = 4, hjust = 0, color = "blue", fontface = "bold") +
    annotate("text", x = 28, y = 73000, label = "Rookie Range\n($66k–$78k)",
             size = 4, hjust = 0, color = "red", fontface = "bold") +
    
    # Add subtitle as annotation near top-left, adjust y for placement
    annotate("text", x = 10, y = 285000, label = "Salaries and Statistics from the 2025 WNBA season",
             hjust = 0, size = 5, color = "gray40") +
    
    # Threshold lines
    geom_hline(yintercept = rookie_min, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = rookie_max, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = regular_max, linetype = "dashed", color = "green") +
    geom_hline(yintercept = supermax, linetype = "dashed", color = "blue") +
    
    # Labels & theme
    labs(
      title = "Player Efficiency Rating vs. WNBA Salary",
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
    theme_classic(base_size = 14)+
    theme(plot.title = element_text(size = 27, face = "bold", hjust = -2))  
}


make_salary_plot2 <- function() {
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
    
    # Add linear model line
    geom_smooth(method = "lm", se = FALSE, color = "black", size = 1.2, inherit.aes = TRUE) +
    
    # Threshold annotations
    annotate("text", x = 5, y = regular_max + 6000, label = "Regular Max\n($214k)",
             size = 4, hjust = 0, color = "green", fontface = "bold") +
    annotate("text", x = 5, y = supermax + 6000, label = "Supermax\n($249k)",
             size = 4, hjust = 0, color = "blue", fontface = "bold") +
    annotate("text", x = 28, y = 73000, label = "Rookie Range\n($66k–$78k)",
             size = 4, hjust = 0, color = "red", fontface = "bold") +
    
    # Add subtitle
    annotate("text", x = 10, y = 285000, label = "Salaries and Statistics from the 2025 WNBA season",
             hjust = 0, size = 5, color = "gray40") +
    
    # Threshold lines
    geom_hline(yintercept = rookie_min, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = rookie_max, linetype = "dashed", color = "red", alpha = 0.5) +
    geom_hline(yintercept = regular_max, linetype = "dashed", color = "green") +
    geom_hline(yintercept = supermax, linetype = "dashed", color = "blue") +
    
    # Labels & theme
    labs(
      title = "Player Efficiency Rating vs. WNBA Salary",
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
    theme_classic(base_size = 14) +
    theme(plot.title = element_text(size = 27, face = "bold", hjust = -2))
  
  # Convert to interactive
  ggplotly(p)  # just to confirm the lm appears
  
}

