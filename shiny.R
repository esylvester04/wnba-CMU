library(tidyr)
library(shiny)
library(ggplot2)
library(dplyr)
library(stringi)

draft_ranking <- read.csv("draft_ranking.csv")


draft_ranking <- draft_ranking %>%
  mutate(
    age = case_when(
      player == "te paopao" ~ 22,
      player == "shatori kimbrough" ~ 30,
      player == "cameron brink" ~23,
      player == "iliana rupert" ~24,
      player == "hailey lith" ~23,
      player == "olivia ododa"~24,
      player== "lou senechal" ~ 27,
      player== "myisha allen" ~29,
      player== "katie samuelson" ~ 28,
      player=="sarah barker" ~23,
      player == "kristy wallace" ~ 29,
      player== "megan gustafson"~28,
      player=="anastasiia kosu" ~20,
      player == "dorka juhasz" ~ 25,
      player == "marine johannes" ~ 30,
      player== "betnijah hamilton"~ 21,
      player== "monique makani"~ 24,
      player == "jordan horston"~ 24,
      player == "nika muhl"~ 24,
      player== "cheyenne tyus" ~ 32,
      TRUE ~ age
    )
  )


draft_ranking <- draft_ranking %>%
  mutate(
    pos = case_when(
      player == "te paopao" ~ "F",
      player == "shatori kimbrough" ~ "G",
      player == "cameron brink" ~ "PF",
      player == "iliana rupert" ~ "C",
      player == "hailey lith" ~ "G",
      player == "olivia ododa" ~ "C",
      player == "lou senechal" ~ "G",
      player == "myisha allen" ~ "F",
      player == "katie samuelson" ~ "F",
      player == "sarah barker" ~ "G",
      player == "kristy wallace" ~ "G",
      player == "megan gustafson" ~ "C",
      player == "anastasiia kosu" ~ "F",
      player == "dorka juhasz" ~ "F",
      player == "marine johannes" ~ "G",
      player == "betnijah hamilton" ~ "G",
      player == "monique makani" ~ "G",
      player == "jordan horston" ~ "F",
      player == "nika muhl" ~ "G",
      player == "cheyenne tyus" ~ "F",
      TRUE ~ pos  # <- this keeps type consistent
    )
  )







library(shiny)
library(dplyr)
library(stringr)

draft_ranking <- read.csv("draft_ranking.csv")


# UI
ui <- fluidPage(
  titlePanel("2026 WNBA Expansion Draft Predicted Protection Status"),
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
      uiOutput("protection_table")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Reactive filtered and cleaned data
  filtered_data <- reactive({
    draft_ranking %>%
      filter(team.x == input$team_choice) %>%
      mutate(
        player = str_to_title(player)  # Capitalize player names
      )
  })
  
  # Render two side-by-side tables
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
      mutate(age = as.integer(age)) %>%
      select(Player = player, Position = pos, Age = age)
  })
  
  output$unprotected_table <- renderTable({
    filtered_data() %>%
      filter(protected == 0) %>%
      mutate(age = as.integer(age)) %>%
      select(Player = player, Position = pos, Age = age)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)








