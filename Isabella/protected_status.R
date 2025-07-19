all_advanced <- read.csv("all_advanced.csv")

######## Updated model including past years performance as well, wit more weight on 2025 performance####
library(tidyr)
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


######      
library(readxl)
c_years<- read_excel("data/contracts.xlsx")

c_years <- c_years %>% select(player, contract_y)

########
library(stringi)

players_weighted_summarised <- players_weighted %>%
  group_by(player) %>%
  summarise(
    weighted_mp = sum(weighted_mp, na.rm = TRUE),
    weighted_ws = sum(weighted_ws, na.rm = TRUE),
    weighted_per = mean(weighted_per, na.rm = TRUE),
    team = last(team)
  ) %>%
  ungroup()

players_weighted_summarised <- players_weighted_summarised %>%
  mutate(
    player = str_squish(player),         # Remove extra spaces
    player = str_to_title(player)        # Convert to Title Case ("Aari Mcdonald")
  )


c_years <- c_years %>%
  mutate(
    player = str_squish(player),
    player = str_to_title(player)
  )


players_weighted_summarised <- players_weighted_summarised %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))


c_years <- c_years %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))





players_weighted_final <- players_weighted_summarised %>%
  right_join(c_years, by = "player")


# players_ranked %>%
#   select(team, player, weighted_mp, weighted_ws, per_minute_score, protected) %>%
#   arrange(team, desc(per_minute_score))
# 




# players_ranked <- players_weighted_final %>%
#   group_by(team) %>%
#   mutate(
#     per_minute_score = scale(weighted_mp) +
#       0.2*scale(weighted_ws) +
#       0.1*scale(weighted_per) +
#       0.2 * scale(contract_y)
#   ) %>%
#   arrange(team, desc(per_minute_score)) %>%
#   mutate(
#     rank_within_team = row_number(),
#     protected = if_else(rank_within_team <= 5, 1, 0)
#   ) %>%
#   ungroup()


players_ranked_u <- players_weighted_final %>%
  group_by(team) %>%
  mutate(
    per_minute_score =
      0.3*scale(weighted_ws) +
      0.1*scale(weighted_per) +
      0.2 * scale(contract_y)
  ) %>%
  arrange(team, desc(per_minute_score)) %>%
  mutate(
    rank_within_team = row_number(),
    protected = if_else(rank_within_team <= 5, 1, 0)
  ) %>%
  ungroup()

library(shiny)
library(ggplot2)
library(dplyr)




# Clean up your dataset (remove NA teams)
players_ranked_u_clean <- players_ranked_u %>%
  filter(!is.na(team))


write.csv(players_ranked_u_clean, "player_protection.csv")

players_ranked_u_clean <- read.csv("player_protection.csv")

ui <- fluidPage(
  titlePanel("WNBA Expansion Draft - Predicted Protection Status"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "team_choice",
        label = "Select a Team:",
        choices = sort(unique(players_ranked_u_clean$team)),
        selected = sort(unique(players_ranked_u_clean$team))[1]
      )
    ),
    mainPanel(
      plotOutput("protection_plot", height = "500px"),
      br(),
      tableOutput("protection_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    players_ranked_u_clean %>%
      filter(team == input$team_choice)
  })
  
  # Render the plot
  output$protection_plot <- renderPlot({
    data <- filtered_data()
    
    min_rank <- min(data$rank_within_team, na.rm = TRUE)
    max_rank <- max(data$rank_within_team, na.rm = TRUE)
    
    ggplot(data, aes(
      x = reorder(player, rank_within_team),
      y = rank_within_team
    )) +
      geom_point(
        aes(color = factor(protected, levels = c(1, 0))),
        size = 4
      ) +
      geom_text(
        aes(label = rank_within_team),
        vjust = -0.7,
        color = "black"
      ) +
      scale_color_manual(
        values = c("1" = "forestgreen", "0" = "firebrick"),
        labels = c("Protected", "Unprotected"),
        name = "Status"
      ) +
      scale_y_continuous(
        breaks = seq(min_rank, max_rank, by = 1),
        limits = c(min_rank, max_rank)
      ) +
      labs(
        x = "Player",
        y = "Protection Rank",
        title = paste("Protection Status -", input$team_choice)
      ) +
      theme_minimal(base_size = 14) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Render the table
  output$protection_table <- renderTable({
    filtered_data() %>%
      arrange(rank_within_team) %>%
      select(player, rank_within_team, protected)
  })
}

# Run the app
shinyApp(ui = ui, server = server)

# ui <- fluidPage(
#   titlePanel("WNBA Expansion Draft - Protection Viewer"),
#   sidebarLayout(
#     sidebarPanel(
#       selectInput(
#         inputId = "team_choice",
#         label = "Select a Team:",
#         choices = sort(unique(players_ranked_u_clean$team)),
#         selected = sort(unique(players_ranked_u_clean$team))[1]
#       )
#     ),
#     mainPanel(
#       plotOutput("protection_plot", height = "500px"),
#       br(),
#       tableOutput("protection_table")
#     )
#   )
# )
# 
# # Server
# server <- function(input, output) {
#   
#   # Reactive filtered data
#   filtered_data <- reactive({
#     players_ranked_u_clean %>%
#       filter(team == input$team_choice)
#   })
#   
#   # Render the plot
#   output$protection_plot <- renderPlot({
#     ggplot(filtered_data(), aes(
#       x = reorder(player, rank_within_team),
#       y = rank_within_team
#     )) +
#       geom_point(
#         aes(color = factor(protected, levels = c(1, 0))),
#         size = 4
#       ) +
#       geom_text(
#         aes(label = rank_within_team),
#         vjust = -0.7,
#         color = "black"
#       ) +
#       scale_color_manual(
#         values = c("1" = "forestgreen", "0" = "firebrick"),
#         labels = c("Protected", "Unprotected"),
#         name = "Status"
#       ) +
#       scale_y_continuous(
#         breaks = 1:13,
#         limits = c(1, 13)
#       ) +
#       labs(
#         x = "Player",
#         y = "Rank",
#         title = paste("Protection Status -", input$team_choice)
#       ) +
#       theme_minimal(base_size = 14) +
#       theme(axis.text.x = element_text(angle = 45, hjust = 1))
#   })
#   
#   # Render the table
#   output$protection_table <- renderTable({
#     filtered_data() %>%
#       arrange(rank_within_team) %>%
#       select(player, rank_within_team, protected)
#   })
# }
# 
#     
# shinyApp(ui = ui, server = server)
# 
# 
# 
# 
# # For example, pick team ATL
# data <- players_ranked_u_clean %>%
#   filter(team == "LVA") %>%
#   arrange(rank_within_team)
# 
# ggplot(data, aes(
#   x = reorder(player, rank_within_team),
#   y = rank_within_team
# )) +
#   geom_point(
#     aes(color = factor(protected, levels = c(1,0))),
#     size = 4
#   ) +
#   geom_text(
#     aes(label = rank_within_team),
#     vjust = -0.7,
#     color = "black"
#   ) +
#   scale_color_manual(
#     values = c("1" = "forestgreen", "0" = "firebrick"),
#     labels = c("Protected", "Unprotected"),
#     name = "Status"
#   ) +
#   scale_y_continuous(
#     breaks = 1:10,   # show every integer rank
#     limits = c(1, 10)  # optional, to be sure the scale starts at 1 and ends at 10
#   ) +
#   labs(
#     title = "Las Vegas Aces Player Ranking",
#     x = "Player",
#     y = "Rank"
#   ) +
#   theme_minimal(base_size = 14) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
# 













