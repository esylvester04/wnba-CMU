library(rvest)
library(tidyverse)
storm_url <- "https://www.basketball-reference.com/wnba/teams/SEA/2025.html"
storm_url |> 
  read_html()

nhl_url |> 
  read_html() |> 
  html_element()