ages <- read_csv("data/age2.csv") 


library(dplyr)
library(stringr)
library(stringi)


# getting ages of all active players, ages as of July 2025. Not updating real time
ages <- ages |>
  rename(
    player = PLAYER, 
    team = TEAM, 
    age = AGE
  ) |>
  mutate(
    player = player |> 
      str_to_lower() |>              # make lowercase
      stri_trans_general("Latin-ASCII")  # remove accents
  )


  
