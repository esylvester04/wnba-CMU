#WNBA CT
wnba_salaries <- read_csv("~/Desktop/SURE RStudio/WNBA Salaries.csv")
install.packages("janitor")
library(tidyverse)

Protect_count <- wnba_salaries |>
  count(`Protection Status` )

wnba_salaries |>
  mean(Salary)

Wba_clean <- na.omit(wnba_salaries)


wnba_salaries <- read.csv("~/Desktop/SURE RStudio/WNBA Salaries.csv")

wnba_salaries2 <- read.csv("~/Desktop/SURE RStudio/WNBA Salaries 2.csv")


df <- wnba_salaries %>%
  filter(Salary!="")

VB <- a_wnba_2024 |>
  filter(player == "Veronica Burton")

Salary <- df |>
  filter(Year == "2025")
