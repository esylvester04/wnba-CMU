pets_dt <- rowwiseDT(pet =, n_classes=, hours_hw=, 
                     "cat", 3, 18, 
                     "dog", 4, 24, 
                     "fish", 2, 10, 
                     "dog", 3, 12, 
                     "dog", 1, 8, 
                     "cat", 1, 3, 
                     "fish", 4, 16, 
                     "fish", 1, 6)
library(tidyverse)
#install.packages("data.table")
library(data.table)

pets_dt[, .(pet,n_classes)]

pets_dt[,hrs_class:= hours_hw/n_classes]
#   :=  defining operator 
pets_dt[, .(pet,hrs_class)]

pets_dt[pet!= "cat"]

pets_dt[order(pet),]

# group by and summarize

pets_dt[, .(avg_hrs_hw=mean(hours_hw)), by= pet]

pets_dt_new <-pets_dt[, .(avg_hrs_hw=mean(hours_hw)), by= pet]

pets_dt

#sounds_dt another dataset, inner join: find common elements between two set to combine thme on
sounds_dt[pets_dt,on=.(pet), nomatch=NULL]

#piping statements together, pairing multoiple of the six main verbs together

pets_dt[pet!= "cat"][order(hours_hw)]

# fread and fwrite functions, fread can use nrow and skip also selcting columns to keep, or drop to remove columns


# pets_dt <- rowwiseDT(pet =, n_classes=, hours_hw=, 
#                      "cat", 3, 18, 
#                      "dog", 4, 24, 
#                      "fish", 2, 10)

gt(pets_dt)

# house_prices[, print(
#   ggplot(.SD, aes(x=Lot.Area, y=SalePrice))+
#     geom_point(alpha=0.5)+
#     theme_classic()+
#     ggtitle(paste("Paved.Drive:", Paved.Drive[1]))), by=.(Paved.Drive)]

# grouped_models <- house_prices[, .(mods = list(
#   lm(SalePrice ~ Lot.Area + Bedroom.AbvGr + Full.Bath + Year.Built*Yr.Sold, data = .SD))),
#   by = Neighborhood ]

library(dtplyr)



