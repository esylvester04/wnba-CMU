#WNBA CT
wnba_salaries <- read_csv("~/Desktop/SURE RStudio/WNBA Salaries.csv")
install.packages("janitor")
library(tidyverse)
library(dplyr)

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

install.packages("writexl")

library(writexl)

write_xlsx(x = daily, path = "daily.xlsx", col_names = TRUE)


write.csv(protected_players, file = "mydata.csv")

# Install the package if not already installed
install.packages("writexl")

# Load the package
library(writexl)

# Export the data frame to Excel
write_xlsx(protected_players, "protected_players.xlsx")

library(writexl)
write_xlsx(protected_players, "protected_players.xlsx")


wnba_players <- read_csv("players_ranked_salary.csv")

protected <- wnba_players |>
  filter(protected == 1)

unprotected <- wnba_players |>
  filter(protected == 0)


Guards <- unprotected |>
  filter(pos == "G")

G_F <- unprotected |>
  filter(pos == "G-F")

Forwards <- unprotected |>
  filter(pos == "F" | pos == "F-C")

Centers <- unprotected |>
  filter(pos == "C" | pos == "C-F")

ATL <- unprotected |>
  filter(team.x == "ATL")

CHI <- unprotected |>
  filter(team.x == "CHI")

CON <- unprotected |>
  filter(team.x == "CON")

DAL <- unprotected |>
  filter(team.x == "DAL")

GSV <- unprotected |>
  filter(team.x == "GSV")

IND <- unprotected |>
  filter(team.x == "IND")

LAS <- unprotected |>
  filter(team.x == "LAS")

LVA <- unprotected |>
  filter(team.x == "LVA")

MIN <- unprotected |>
  filter(team.x == "MIN")

NYL <- unprotected |>
  filter(team.x == "NYL")

PHO <- unprotected |>
  filter(team.x == "PHO")

SEA <- unprotected |>
  filter(team.x == "SEA")

WAS <- unprotected |>
  filter(team.x == "WAS")


advanced_stats <- read_csv("all_advanced.csv")

library(dplyr)

new_dataset <- semi_join(advanced_stats, unprotected, by = "player")
new_dataset <- advanced_stats %>%
  filter(player %in% unprotected$player)

library(dplyr)

# Clean up names
advanced_stats <- advanced_stats %>%
  mutate(player = tolower(trimws(player)))

unprotected <- unprotected %>%
  mutate(player = tolower(trimws(player)))

# Match players
#new_dataset <- semi_join(advanced_stats, unprotected, by = "player")
names <- unprotected$player
ind <- c()
for(i in 1:647){
  if(any(grepl(advanced_stats$player[i],names,ignore.case=TRUE))){
    ind <- c(ind,i)
  }
}
new_dataset <- advanced_stats[ind,]

# View result
View(new_dataset)  # or use head(new_dataset)



model_df <- read_csv("salary_model_df.csv")
model_df_scaled <- read_csv("model_df_scaled.csv")
eda_df <- read_csv("eda_df.csv")




unprotected_csv <- read_csv("unprotected_df")

finalized_pool <- read_csv("draft_ranking.csv")



unprotected <- finalized_pool |>
  filter(protected == 0)


Guards <- unprotected |>
  filter(pos_group == "G")


Forwards <- unprotected |>
  filter(pos_group == "F")

Centers <- unprotected |>
  filter(pos_group == "C")
