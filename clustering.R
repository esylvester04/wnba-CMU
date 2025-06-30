library(dplyr)

players_2024 <- all_advanced %>%
  filter(year == 2024) %>%
  filter(g > 5)  # Filter out players with very few games (optional)

vars_to_use <- c("mp", "per", "ts_percent", "trb_percent", "ast_percent", 
                 "stl_percent", "blk_percent", "tov_percent", "ws")

players_2024_selected <- players_2024 %>%
  select(all_of(vars_to_use)) %>%
  mutate(across(everything(), as.numeric))  # Ensure numeric
scaled_2024 <- scale(players_2024_selected)
library(factoextra)

fviz_nbclust(scaled_2024, kmeans, method = "wss") +
  labs(title = "Elbow Method for Optimal Number of Clusters (2024)")
set.seed(42)
kmeans_2024 <- kmeans(scaled_2024, centers = 4, nstart = 25)

players_2024 <- players_2024 %>%
  mutate(player_type = kmeans_2024$cluster)



players_2024 <- players_2024 %>%
  mutate(across(
    -c(player_type, year, team, player, pos),  # keep non-numeric columns as-is
    ~ as.numeric(.)
  ))


cluster_profiles <- players_2024 %>%
  group_by(player_type) %>% 
  summarise(across(where(is.numeric), mean, na.rm = TRUE)) %>%
  arrange(player_type) 

print(cluster_profiles)

summary(pca_2024)
pca_2024$rotation

ggplot(pca_scores_2024, aes(x = -PC1, y = PC2, color = cluster_label)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_text_repel(data = top_players_per_cluster, aes(label = player), size = 3) +
  labs(
    title = "WNBA Player Clusters",
    x = "Efficiency & Impact",
    y = "Rebounding vs Assisting",
    color = "Player Type"
  ) +
  theme_minimal() + theme(
    plot.title = element_text(
      size = 18,
      face = "bold",
      hjust = 0.5
    ),
    legend.title = element_text(
      face = "bold",
      size = 12   
    )
  )

library(corrplot)
corrplot(cor(players_2024_selected), method = "color", type = "upper", tl.cex = 0.8)


fviz_eig(pca_2024, addlabels = TRUE, barfill = "skyblue")


# 
# 
# 
# | Variable     | PC1 Loading | PC2 Loading |
#   | ------------ | ----------- | ----------- |
#   | mp           | –0.40       | +0.28       |
#   | per          | –0.52       | +0.00       |
#   | ts\_percent  | –0.40       | +0.03       |
#   | trb\_percent | –0.25       | –0.52       |
#   | ast\_percent | –0.14       | **+0.58**   |
#   | stl\_percent | –0.20       | +0.15       |
#   | blk\_percent | –0.20       | –0.53       |
#   | tov\_percent | +0.16       | +0.13       |
#   | ws           | –0.47       | +0.06       |
#   






cluster_labels <- c(
  "1" = "Low Impact Players",
  "2" = "Defensive Specialists",
  "3" = "Play Makers",
  "4" = "Star Scorer"
)

players_2024 <- players_2024 %>%
  mutate(cluster_label = cluster_labels[as.character(player_type)])
pca_2024 <- prcomp(scaled_2024)
pca_scores_2024 <- as.data.frame(pca_2024$x[, 1:2])  # PC1, PC2
pca_scores_2024$player <- players_2024$player
pca_scores_2024$cluster_label <- players_2024$cluster_label
pca_scores_2024$per <- players_2024$per  # For filtering top players if needed


library(ggplot2)
library(ggrepel)
library(dplyr)

top_players_per_cluster <- pca_scores_2024 %>%
  group_by(cluster_label) %>%
  slice_max(order_by = per, n = 2) %>%
  ungroup()

ggplot(pca_scores_2024, aes(x = -PC1, y = PC2, color = cluster_label)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(data = top_players_per_cluster, aes(label = player), size = 4) +
  labs(title = "2024 WNBA Player Clusters ",
       x = "Flipped PC1 (Efficiency / Impact)",
       y = "PC2 (Rebounding vs Assisting)",
       color = "Player Type") +
  theme_minimal()
