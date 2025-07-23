#WNBA CT
wnba_salaries <- read_csv("~/Downloads/WNBA Salaries 2.csv")
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

<<<<<<< Updated upstream
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
=======

Salary |>
  count(Protection.Status)


Eclude_waived <- 
  
KLS <- a_wnba_2025 |>
  filter(player == "Katie Lou Samuelson")

Salary |>
  filter(Protection.Status == "Pregnancy/Childbirth")

  
Guards <- salary_stat |>
  filter(pos == "G" & pos == "G-F")

salary_stat |>
  count(pos == "G-F")


graph_ss <- salary_stat |>
  select(player,salary_clean)

graph_ss |> 
  ggplot(aes(x = salary_clean)) +
  geom_histogram()


library(readr)
library(ggplot2)

# Read your data
df <- read_csv("salary_data.csv")

library(g)
# Plot
ggplot(graph_ss, aes(x = salary_clean)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count) * 100)),
    bins = 30,
    fill = "skyblue",
    color = "steelblue",
    alpha = 0.4,
    boundary = 0
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(labels = scales::dollar_format()) +
  labs(
    title = "Distribution of Player Salary for 2025-26 Season",
    x = "$",
    y = "Percentage of players"
  ) +
  theme_minimal()


library(dplyr)
library(tidyr)
library(tidyverse)
GSV <- a_wnba_2024 |>
  filter(player == "Tiffany Hayes" | "Stephanie Talbot" |"Kayla Thornton" |"Monique Billings" |
                "Cecilia Zandalasini" |
                "Temi Fagbenle" |
                "Veronica Burton" |
                "Carla Leite" |
                "Kate Martin" |
                "Janelle Salaün" |
                "Julie Vanloo" |
                "Laeticia Amihere" |
                "Bree Hall" |
                "Maria Conde" |
                "Iliana Rupert" |
                "Kyara Linskens" |
                "Aerial Powers")

GSV <- a_wnba_2024 |>
  filter(player %in% c("Tiffany Hayes", 
                       "Stephanie Talbot",
                       "Kayla Thornton",
                       "Monique Billings",
                         "Cecilia Zandalasini",
                         "Temi Fagbenle",
                         "Veronica Burton",
                         "Carla Leite",
                         "Kate Martin",
                         "Janelle Salaün",
                         "Julie Vanloo",
                         "Laeticia Amihere",
                         "Bree Hall",
                         "Maria Conde",
                         "Iliana Rupert",
                         "Kyara Linskens",
                         "Aerial Powers"))

GSV <- wnba_2024 |>
  filter(player %in% c("Tiffany Hayes", 
                       "Stephanie Talbot",
                       "Kayla Thornton",
                       "Monique Billings",
                       "Cecilia Zandalasini",
                       "Temi Fagbenle",
                       "Veronica Burton",
                       "Carla Leite",
                       "Kate Martin",
                       "Janelle Salaün",
                       "Julie Vanloo",
                       "Laeticia Amihere",
                       "Bree Hall",
                       "Maria Conde",
                       "Iliana Rupert",
                       "Kyara Linskens",
                       "Aerial Powers"))
library(dplyr)
library(tidyverse)
library(tidyr)
Guards <- a_wnba_2025 |>
  filter(pos %in% c("G",
                    "G-F"))

Fowards <- a_wnba_2025 |>
  filter(pos %in% c("F",
                    "F-G",
                    "F-C"))

Centers <- a_wnba_2025 |>
  filter(pos %in% c("C",
                    "C-F"))

Centers |>
  arrange()
  
  
a_wnba_2025 |>
  count(pos)
                    
                    
                    
                    
>>>>>>> Stashed changes
