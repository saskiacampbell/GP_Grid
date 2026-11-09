library(tidyverse)
library(dplyr)
july_map <- read.csv(
  "invasives_map_processing(july).csv"
)



july_map<-july_map%>%
  rename(
    grid_id = What.grid.square.are.you.reporting.on..choose.1.49.,
    point_one = Sample.Point.1..don.t.forget.your.photo.point.,
    point_two = Sample.Point.2,
    point_three = Sample.Point.3,
    point_four = Sample.Point.4,
    point_five = Sample.Point.5,
    point_six = Sample.Point.6,
    point_seven = Sample.Point.7...don.t.forget.your.photo.point.
  )

library(dplyr)
library(dplyr)
library(stringr)

#cleaning data
july_map <- july_map %>%
  mutate(across(starts_with("point_"), ~ 
                  .x %>%
                  str_remove("\\?") %>%   # remove question marks
                  str_trim()              # remove leading/trailing spaces
  ))
july_map <- july_map %>%
  mutate(across(starts_with("point_"), ~ 
                  .x %>%
                  str_replace_all("[^[:alnum:]\\-\\+ ]", "") %>% 
                  str_trim()
  ))

july_map <- july_map %>%
  mutate(across(starts_with("point_"), ~ case_when(
    .x == "0 individuals present" ~ 0,
    .x == "1-25 individuals present" ~ 1,
    .x == "25-50 individuals present" ~ 2,
    .x == "50-100 individuals present" ~ 3,
    .x == "100+ individuals present" ~ 4,
    TRUE ~ NA_real_
  )))



#creating sum of scores

july_map <- july_map %>%
  mutate(total_score = rowSums(across(starts_with("point_")), na.rm = TRUE))

july_map<-july_map %>%
  select(-c(X, X.1, X.2, X.3, X.4))


#cleaning data
#getting rid of duplicate surveys
july_map <- july_map[-18, ]
july_map <- july_map[-43, ]

july_map$grid_id[47] <- 34
nrow(july_map)



july_map<-july_map%>%
  rename(
    GRID_ID = grid_id)
july_map_final<-july_map%>%
  select(GRID_ID, total_score)
write.csv(july_map_final, "july_map.csv")

str(july_map_final)

library(dplyr)

july_map_final <- july_map_final %>%
  mutate(GRID_ID = as.character(GRID_ID))

str(july_map_final)

unique(july_map_final$GRID_ID)
july_map_final %>%
  mutate(check = paste0("[", grid_id, "]")) %>%
  distinct(check)
print(n = ...)

library(dplyr)
library(stringr)

july_map_final <- july_map_final %>%
  mutate(
    grid_id = as.character(GRID_ID),
    grid_id = str_replace_all(GRID_ID, "[^0-9]", ""),  # keep only numbers
    grid_id = str_trim(GRID_ID)
  )
write.csv(july_map_final, "july_map_final.csv", row.names = FALSE)
