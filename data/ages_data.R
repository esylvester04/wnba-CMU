ages <- read_csv("data/age2.csv") 


library(dplyr)
library(stringr)
library(stringi)

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


  
