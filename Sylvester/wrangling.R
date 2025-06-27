library(rvest)
library(dplyr)
library(janitor)
library(lubridate)
library(tidyverse)



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

# 
# #########        Team Data            ########
# 
# t_url_2022<- "https://www.basketball-reference.com/wnba/years/2022.html"
# 
# t_wnba_2022<- read_html(t_url_2022) %>% 
#   html_element("#per_game-team") %>% html_table() %>% clean_names()
# 
# 
# t_url_2023<- "https://www.basketball-reference.com/wnba/years/2023.html"
# 
# t_wnba_2023<- read_html(t_url_2023) %>% 
#   html_element("#per_game-team") %>% html_table() %>% clean_names()
# 
# 
# t_url_2024<- "https://www.basketball-reference.com/wnba/years/2024.html"
# 
# t_wnba_2024<- read_html(t_url_2024) %>% 
#   html_element("#per_game-team") %>% html_table()%>% clean_names()
# 
# 
# t_url_2025<- "https://www.basketball-reference.com/wnba/years/2025.html"
# 
# t_wnba_2025<- read_html(t_url_2025) %>% 
#   html_element("#per_game-team") %>% html_table()%>% clean_names()
# 
# 
# #######       Advanced Team Stats         ######
# 
# a_t_url_2022 <- "https://www.basketball-reference.com/wnba/years/2022.html"
# 
# a_t_wnba_2022 <- read_html(a_t_url_2022) %>%
#   html_element("#advanced-team") %>%
#   html_table(header = FALSE) # Read the table WITHOUT headers
# 
# colnames(a_t_wnba_2022) <- as.character(a_t_wnba_2022[2, ])
# 
# # remove first two rows (the header rows)
# a_t_wnba_2022 <- a_t_wnba_2022[-c(1,2), ] %>% clean_names()
# 
# 
# a_t_url_2023 <- "https://www.basketball-reference.com/wnba/years/2023.html"
# 
# a_t_wnba_2023 <- read_html(a_t_url_2023) %>%
#   html_element("#advanced-team") %>%
#   html_table(header = FALSE) # Read the table WITHOUT headers
# 
# colnames(a_t_wnba_2023) <- as.character(a_t_wnba_2023[2, ])
# 
# # remove first two rows (the header rows)
# a_t_wnba_2023 <- a_t_wnba_2023[-c(1,2), ] %>% clean_names()
# 
# 
# a_t_url_2024 <- "https://www.basketball-reference.com/wnba/years/2024.html"
# 
# a_t_wnba_2024 <- read_html(a_t_url_2024) %>%
#   html_element("#advanced-team") %>%
#   html_table(header = FALSE) # Read the table WITHOUT headers
# 
# colnames(a_t_wnba_2024) <- as.character(a_t_wnba_2024[2, ])
# 
# # remove first two rows (the header rows)
# a_t_wnba_2024 <- a_t_wnba_2024[-c(1,2), ] %>% clean_names()
# 
# 
# a_t_url_2025 <- "https://www.basketball-reference.com/wnba/years/2025.html"
# 
# a_t_wnba_2025 <- read_html(a_t_url_2025) %>%
#   html_element("#advanced-team") %>%
#   html_table(header = FALSE) # Read the table WITHOUT headers
# 
# colnames(a_t_wnba_2025) <- as.character(a_t_wnba_2025[2, ])
# 
# # remove first two rows (the header rows)
# a_t_wnba_2025 <- a_t_wnba_2025[-c(1,2), ] %>% clean_names()
# 
# 
# #########           Adding a Year Column & If made playoffs       #########
# 
# t_wnba_2022 <- t_wnba_2022 %>% mutate(year = 2022)
# t_wnba_2023 <- t_wnba_2023 %>% mutate(year = 2023)
# t_wnba_2024 <- t_wnba_2024 %>% mutate(year = 2024)
# t_wnba_2025 <- t_wnba_2025 %>% mutate(year = 2025)
# 
# a_t_wnba_2022 <- a_t_wnba_2022 %>% mutate(year = 2022)
# a_t_wnba_2023 <- a_t_wnba_2023 %>% mutate(year = 2023)
# a_t_wnba_2024 <- a_t_wnba_2024 %>% mutate(year = 2024)
# a_t_wnba_2025 <- a_t_wnba_2025 %>% mutate(year = 2025)
# 
# 
# 
# #t_all_advanced <- t_all_advanced %>% mutate()
# 
# 
# 
# #Las Vegace Aces, Conneticut Sun, Chicago Sky, Seattle Storm, 
# #New York Liberty, Dallas Wings, Phoenix Mercury, Washington Mystics
# 
# 
# 
# 
# 
# 
# 
# 
# #########          Stacking team sets together     #############
# 
# t_all_advanced <- bind_rows( a_t_wnba_2022, a_t_wnba_2023, a_t_wnba_2024, a_t_wnba_2025) %>% 
#   select(-na_3,-na, -na_2, -arena)
# t_per_game <- bind_rows( t_wnba_2022, t_wnba_2023, t_wnba_2024, t_wnba_2025)
# 
# #########         Removing League Avg. Rows       ###############
# 
# t_all_advanced <- t_all_advanced[!t_all_advanced$team== "League Average",]
# 
# t_per_game <- t_per_game[!t_per_game$team== "League Average",]



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
# group by team, clustering analysis for positions on a single team- Golden State

library(cluster)
library(factoextra)
library(ggrepel)

# Clean version of all_advanced 
gsv_advanced <- all_advanced |> 
  mutate(across(c("g":"ws_40"),
                as.numeric)) |>
  #filter(team == "GSV") |>  
  drop_na() |>
  clean_names()

View(gsv_advanced)

# Prepare numeric matrix for PCA 
pca_data <- gsv_advanced |>
  select(where(is.numeric), -year) |>
  scale()

# Run PCA
GSV_pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Add player info back
GSV_pca_scores <- as_tibble(GSV_pca$x[, 1:2]) |>
  bind_cols(gsv_advanced |>
              select(player, team, year) |>
              slice(1:nrow(GSV_pca$x)))


ggplot(GSV_pca_scores, aes(x = PC1, y = PC2)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA of Golden State Valkyeries Advanced Stats (first 15 games)",
       subtitle = "First two principal components",
       x = "PC1",
       y = "PC2") +
  theme_minimal()




# Clean version of all_advanced for the whole WNBA
wnba_advanced <- all_advanced |> 
  mutate(across(c("g":"ws_40"), as.numeric)) |>
  drop_na() |>
  clean_names()

# Prepare numeric matrix for PCA (exclude year)
pca_data <- wnba_advanced |> 
  select(where(is.numeric), -year) |> 
  scale()

# Run PCA
wnba_pca <- prcomp(pca_data, center = TRUE, scale. = TRUE)

# Add player/team info back
wnba_pca_scores <- as_tibble(wnba_pca$x[, 1:2]) |> 
  bind_cols(wnba_advanced |> 
              select(player, team, year) |> 
              slice(1:nrow(wnba_pca$x)))

# Plot
ggplot(wnba_pca_scores, aes(x = PC1, y = PC2, color = team)) +
  geom_point(alpha = 0.7) +
  labs(title = "PCA of WNBA Players (Advanced Stats)",
       subtitle = "First two principal components",
       x = "PC1",
       y = "PC2") +
  theme_minimal()



fviz_pca_biplot(GSV_pca,
                label = "var",          # label variables (stats)
                alpha.ind = 0.25,       # transparency of points (players)
                alpha.var = 0.75,       # transparency of variable arrows
                labelsize = 5,          # size of variable labels
                col.var = "darkblue",   # color of variable arrows and labels
                repel = TRUE)           # avoid overlapping labels


# How many pcas should we use?

summary(GSV_pca)
# maybe 3 based on proportion of variance?

# elbow plot:
GSV_pca |> 
  fviz_eig(addlabels = TRUE) +
  geom_hline(
    yintercept = 100 * (1 / ncol(GSV_pca$x)), 
    linetype = "dashed", 
    color = "darkred",
  ) # indicates maybe 5??


# 3D plot of first 3 dimensions:
library(plotly)
pca_scores_3d <- as_tibble(GSV_pca$x[, 1:3]) |>
  bind_cols(gsv_advanced |> select(player, team, year) |> slice(1:nrow(GSV_pca$x)))

plot_ly(pca_scores_3d, x = ~PC1, y = ~PC2, z = ~PC3,
        color = ~team, text = ~player,
        type = "scatter3d", mode = "markers") %>%
  layout(title = "3D PCA of Golden State Valkyries Stats")

#install.packages("GGally")
library(GGally)
pca_scores_df <- as_tibble(GSV_pca$x[, 1:5]) |>
  bind_cols(gsv_advanced |> select(player, team, year) |> slice(1:nrow(GSV_pca$x)))

ggpairs(pca_scores_df, columns = 1:5, aes(color = team))


# Clustering:

wnba_advanced |> 
  ggplot(aes(x = e_fg_percent)) + 
  geom_histogram() 


wnba_advanced <- wnba_advanced |>
  mutate(
    std_per = as.numeric(scale(per, center = TRUE, scale = TRUE)),
    std_e_fg_per = as.numeric(scale(e_fg_percent, center = TRUE, scale = TRUE))
  )


init_kmeans <- wnba_advanced |> 
  select(std_per, std_e_fg_per) |> 
  kmeans(algorithm = "Lloyd", centers = 4, nstart = 1)
  
  
wnba_clustered <- wnba_advanced |> 
  mutate(clusters = as.factor(init_kmeans$cluster))

# K means clustering of WNBA 
ggplot(wnba_clustered, aes(x = std_per, 
                           y = std_e_fg_per, color = clusters)) +
  geom_point(size = 3, alpha = 0.8) +
  ggthemes::scale_color_colorblind(name = "Cluster") +
  labs(
    title = "K-means Clustering of WNBA Players 2025",
    subtitle = "Clusters based on standardized PER and eFG%",
    x = "Player Efficiency Rating (PER)",
    y = "Effective Field Goal Percentage (eFG%)",
    caption = "Data: basketball-reference.com | Clustering on scaled PER & eFG%"
  ) +
  theme_minimal(base_size = 14) +
  coord_fixed(ratio = 1.5)



# 
# stars_advanced <-all_advanced |>
#   filter(year )
  





# Convert centroids to a tibble and label them
centroids <- as_tibble(init_kmeans$centers) |>
  mutate(clusters = as.factor(1:n()))

# Plot with centroids
ggplot(wnba_clustered, aes(x = std_per, y = std_e_fg_per, color = clusters)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = centroids, aes(x = std_per, y = std_e_fg_per),
             shape = 4, size = 5, stroke = 2, color = "black") +
  ggthemes::scale_color_colorblind(name = "Cluster") +
  labs(
    title = "K-means Clustering of WNBA Players 2025",
    subtitle = "With Cluster Centroids (black X's)",
    x = "Standardized PER",
    y = "Standardized eFG%",
    caption = "Data: basketball-reference.com | Clustering on scaled PER & eFG%"
  ) +
  theme_minimal(base_size = 14) +
  coord_fixed(ratio = 1.5)



#Adding names for the centriods

# Step 1: Add cluster number and distances to centroids
clustered_with_distance <- wnba_advanced |> 
  select(player, std_per, std_e_fg_per) |> 
  mutate(cluster = init_kmeans$cluster) |>
  left_join(
    as_tibble(init_kmeans$centers) |> 
      mutate(cluster = 1:n()),
    by = "cluster",
    suffix = c("", "_centroid")
  ) |>
  mutate(
    distance = sqrt((std_per - std_per_centroid)^2 + (std_e_fg_per - std_e_fg_per_centroid)^2)
  )

# Step 2: Get the closest player to each centroid
representative_players <- clustered_with_distance |> 
  group_by(cluster) |> 
  slice_min(order_by = distance, n = 1, with_ties = FALSE) |> 
  select(cluster, player, std_per, std_e_fg_per)

print(representative_players)


# Reuse centroids and clusters
centroids <- as_tibble(init_kmeans$centers) |> 
  mutate(cluster = as.factor(1:n())) |> 
  left_join(representative_players |> mutate(cluster = as.factor(cluster)), by = "cluster")

centroids <- centroids |> 
  rename(
    std_per = std_per.x,
    std_e_fg_per = std_e_fg_per.x,
    player_name = player
  )



library(ggthemes)
ggplot(wnba_clustered, aes(x = std_per, y = std_e_fg_per, color = clusters)) +
  geom_point(size = 3, alpha = 0.5) +
  
  # Red centroid dots
  geom_point(data = centroids, aes(x = std_per, y = std_e_fg_per),
             shape = 21, fill = "red", color = "black", size = 5, stroke = 1.5) +
  
  # Transparent labels with smaller font
  geom_label_repel(
    data = centroids,
    aes(x = std_per, y = std_e_fg_per, label = player_name),
    fill = alpha("white", 0.4),   # fully transparent box
    color = "black",
    size = 3,                     # smaller text
    fontface = "bold",
    box.padding = 0.3,
    label.size = NA              # remove label border
  ) +
  
  ggthemes::scale_color_colorblind(name = "Cluster") +
  labs(
    title = "K-means Clustering of WNBA Players (2025)",
    subtitle = "Red dots are centroids, labeled with closest player",
    x = "Standardized PER",
    y = "Standardized eFG%",
    caption = "Data: basketball-reference.com | Clustering on scaled PER & eFG%"
  ) +
  theme_minimal(base_size = 14) +
  coord_fixed(ratio = 1.5)
