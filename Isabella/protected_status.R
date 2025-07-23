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







players_weighted_final_a <- players_weighted_summarised %>%
  right_join(c_years, by = "player")


players_weighted_final_a <- players_weighted_final %>%
  right_join(ages %>% select(player, age), by = "player")

players_weighted_final_a <- players_weighted_final_a %>%
  mutate(
    age = case_when(
      player == "te paopao" ~ 22,
      player == "shatori kimbrough" ~ 30,
      player == "cameron brink" ~23,
      player == "iliana rupert" ~24,
      player== "shatori kimbrough" ~ 30,
      player == "hailey lith" ~23,
      player == "olivia ododa"~24,
      player== "myisha allen" ~29,
      player== "katie samuelson" ~ 28,
      player=="sarah barker" ~23,
      player== "megan gustafson"~28,
      player=="anastasiia kosu" ~20,
      player == "dorka juhasz" ~ 25,
      player== "betnijah hamilton"~ 21,
      player== "monique makani"~ 24,
      player == "jordan horston"~ 24,
      player == "nika muhl"~ 24,
      TRUE ~ age
    )
  )



players_ranked_u <- players_weighted_final_a %>%
  group_by(team) %>%
  mutate(
    age_penalty = if_else(age > 35, scale(age), 0),  # decay only for age > 33
    per_minute_score = 0.2 * scale(weighted_ws)
      0.3 * scale(weighted_per) +
      0.1 * scale(contract_y) -
      0.3 * age_penalty
  ) %>%
  arrange(team, desc(per_minute_score)) %>%
  mutate(
    rank_within_team = row_number(),
    protected = if_else(rank_within_team <= 5, 1, 0)
  ) %>%
  ungroup()



p_salary <- read_csv("salary_model_df.csv")

p_salary<- p_salary %>% 
  mutate(player = stri_trans_general(player, "Latin-ASCII"),
         player = str_trim(player),
         player = str_to_lower(player),
         player = str_replace_all(player, "-", " "),  # Replace hyphen with space
         player = str_replace(player, "^([\\w']+)\\s+.*\\s+([\\w']+)$", "\\1 \\2"))








testing <- players_ranked_u %>%
  full_join(p_salary, by = "player")




testing <- testing %>% select(player,weighted_mp,weighted_ws,weighted_per,
                                          team.x, contract_y,age,per_minute_score,rank_within_team,
                                          protected,pos, actual, xgb_pred, residual)


library(shiny)
library(ggplot2)
library(dplyr)


write.csv(draft_ranking, "draft_ranking.csv")

draft_ranking <- read.csv("draft_ranking.csv")

colnames(draft_ranking)


# UI
ui <- fluidPage(
  titlePanel("WNBA Expansion Draft Predicted Protection Status"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "team_choice",
        label = "Select a Team:",
        choices = sort(unique(draft_ranking$team.x)),
        selected = sort(unique(draft_ranking$team.x))[1]
      )
    ),
    mainPanel(
      uiOutput("protection_table")  # Dynamically generated UI for two tables
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    draft_ranking %>%
      filter(team.x == input$team_choice)
  })
  
  # Create two side-by-side tables
  output$protection_table <- renderUI({
    fluidRow(
      column(
        width = 6,
        h4("✅ Protected Players"),
        tableOutput("protected_table")
      ),
      column(
        width = 6,
        h4("❌ Unprotected Players"),
        tableOutput("unprotected_table")
      )
    )
  })
  
  output$protected_table <- renderTable({
    filtered_data() %>%
      filter(protected == 1) %>%
      select(player)
  })
  
  output$unprotected_table <- renderTable({
    filtered_data() %>%
      filter(protected == 0) %>%
      select(player)
  })
}

# Run the app
shinyApp(ui = ui, server = server)








ui <- fluidPage(
  titlePanel("WNBA Expansion Draft - Protection Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "team_choice",
        label = "Select a Team:",
        choices = sort(unique(draft_ranking$team.x)),
        selected = sort(unique(draft_ranking$team.x))[1]
      )
    ),
    mainPanel(
      plotOutput("protection_plot", height = "500px"), br(),
      tableOutput("protection_table")
    )
  )
)
 
# # Server
server <- function(input, output) {
#   
#   # Reactive filtered data
  filtered_data <- reactive({
    draft_ranking %>%
      filter(team.x == input$team_choice)
  })
#   
#   # Render the plot
  output$protection_table <- renderUI({
    protected <- filtered_data() %>% filter(protected == 1) %>% select(player)
    unprotected <- filtered_data() %>% filter(protected == 0) %>% select(player)
    
    fluidRow(
      column(6, h4("Protected Players"), tableOutput("protected_table")),
      column(6, h4("Unprotected Players"), tableOutput("unprotected_table"))
    )
  })
  
  output$protected_table <- renderTable({
    filtered_data() %>% filter(protected == 1) %>% select(player)
  })
  
  output$unprotected_table <- renderTable({
    filtered_data() %>% filter(protected == 0) %>% select(player)
  })
  
# 
#     
shinyApp(ui = ui, server = server)
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






draft_ranking_u <- draft_ranking %>% filter(protected==0)

centers <- draft_ranking_u %>% filter(pos_group== "C")

forwars<- draft_ranking_u %>% filter(pos_group=="F")

guards <- draft_ranking_u %>% filter(pos_group=="G")


library(ggplot2)
library(dplyr)


library(shiny)
library(dplyr)
library(ggplot2)

# UI
ui <- fluidPage(
  titlePanel("WNBA Expansion Draft - Protection Viewer"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "team_choice",
        label = "Select a Team:",
        choices = sort(unique(draft_ranking$team.x)),
        selected = sort(unique(draft_ranking$team.x))[1]
      )
    ),
    mainPanel(
      plotOutput("protection_plot", height = "600px")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    draft_ranking %>%
      filter(team.x == input$team_choice)
  })
  
  # Dot plot showing protection status
  output$protection_plot <- renderPlot({
    ggplot(filtered_data(), aes(
      x = factor(protected, levels = c(1, 0), labels = c("Protected", "Unprotected")),
      y = reorder(player, protected),
      color = factor(protected)
    )) +
      geom_point(size = 4) +
      scale_color_manual(
        values = c("1" = "forestgreen", "0" = "firebrick"),
        labels = c("Protected", "Unprotected"),
        name = "Status"
      ) +
      labs(
        title = paste("Protection Status -", input$team_choice),
        x = "Status",
        y = "Player"
      ) +
      theme_minimal(base_size = 14)
  })
}

# Run the app
shinyApp(ui = ui, server = server)



##



library(dplyr)

# Create weight grid including age penalty
weights <- expand.grid(
  w_ws = seq(0.1, 0.9, by = 0.2),
  w_per = seq(0.1, 0.9, by = 0.2),
  w_contract = seq(0.1, 0.9, by = 0.2),
  w_age = seq(-0.5, 0, by = 0.1)  # Age penalty weight
) %>%
  filter(abs(w_ws + w_per + w_contract - 1) < 1e-6)  # Only apply sum=1 to the main 3 weights

# Store results
results <- list()

for (i in 1:nrow(weights)) {
  w <- weights[i, ]
  
  df <- draft_ranking %>%
    group_by(team.x) %>%
    mutate(
      age_penalty = if_else(age > 35, as.numeric(scale(age)), 0),
      test_score =
        w$w_ws * as.numeric(scale(weighted_ws)) +
        w$w_per * as.numeric(scale(weighted_per)) +
        w$w_contract * as.numeric(scale(contract_y)) +
        w$w_age * age_penalty,
      test_rank = rank(-test_score, ties.method = "first"),
      test_protected = if_else(test_rank <= 5, 1, 0)
    ) %>%
    ungroup()
  
  # Compare to original protection
  accuracy <- mean(df$test_protected == df$protected, na.rm = TRUE)
  
  results[[i]] <- c(w, accuracy = accuracy)
}

# Combine and sort results
results_df <- do.call(rbind, results) %>% as.data.frame()
results_df <- results_df %>% arrange(desc(accuracy))

# View top combos
head(results_df, 10)











library(dplyr)

# 1. Create the grid of weight combinations
weights <- expand.grid(
  w_ws = seq(0.1, 0.9, by = 0.2),
  w_per = seq(0.1, 0.9, by = 0.2),
  w_contract = seq(0.1, 0.9, by = 0.2),
  w_age = seq(-0.5, 0, by = 0.1)
) %>%
  filter(abs(w_ws + w_per + w_contract - 1) < 1e-6)

# 2. Define a safe scaling function
safe_scale <- function(x) {
  if (all(is.na(x))) return(rep(0, length(x)))
  as.numeric(scale(x))
}

# 3. Pre-scale variables globally to avoid group-wise NA errors
scaled_draft <- draft_ranking %>%
  mutate(
    scaled_ws = safe_scale(weighted_ws),
    scaled_per = safe_scale(weighted_per),
    scaled_contract = safe_scale(contract_y),
    scaled_age = if_else(age > 35, safe_scale(age), 0)
  )


eligible_players <- scaled_draft %>%
  filter(!is.na(per_minute_score))

# 4. Grid search
results <- list()

for (i in 1:nrow(weights)) {
  w <- weights[i, ]
  
  df <- eligible_players %>%
    group_by(team.x) %>%
    mutate(
      test_score =
        w$w_ws * scaled_ws +
        w$w_per * scaled_per +
        w$w_contract * scaled_contract +
        w$w_age * scaled_age,
      test_rank = rank(-test_score, ties.method = "first"),
      test_protected = if_else(test_rank <= 5, 1, 0)
    ) %>%
    ungroup()
  
  accuracy <- mean(df$test_protected == df$protected, na.rm = TRUE)
  
  results[[i]] <- c(w, accuracy = accuracy)
}

results_df <- do.call(rbind, results) %>% as.data.frame()
results_df <- results_df %>% arrange(desc(accuracy))

head(results_df, 10)









