library(tidyverse)
library(dplyr)
september_map <- read.csv(
  "invasives_map_processing(september).csv"
)



september_map<-september_map%>%
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
library(stringr)

#cleaning data
september_map <- september_map %>%
  mutate(across(starts_with("point_"), ~ 
                  .x %>%
                  str_remove("\\?") %>%   # remove question marks
                  str_trim()              # remove leading/trailing spaces
  ))
september_map <- september_map %>%
  mutate(across(starts_with("point_"), ~ 
                  .x %>%
                  str_replace_all("[^[:alnum:]\\-\\+ ]", "") %>% 
                  str_trim()
  ))

september_map <- september_map %>%
  mutate(across(starts_with("point_"), ~ case_when(
    .x == "0 individuals present" ~ 0,
    .x == "1-25 individuals present" ~ 1,
    .x == "25-50 individuals present" ~ 2,
    .x == "50-100 individuals present" ~ 3,
    .x == "100+ individuals present" ~ 4,
    TRUE ~ NA_real_
  )))



#creating sum of scores

september_map <- september_map %>%
  mutate(total_score = rowSums(across(starts_with("point_")), na.rm = TRUE))



#cleaning data
#getting rid of duplicate surveys by deleting a row
september_map <- september_map[-21, ]

#taking out unnessessary columns
september_map<-september_map %>%
  select(-c(Think.about.the.distribution.of.woodies.in.the.grid.section..Are.they.mostly..., How.tall.is.the.tallest.invasive.woody.plant.in.this.grid.section.,Of.the.multiflora.rose.in.this.grid.section..select.from.the.following.))

#sections 17 and 18 got skipped. Adding rows for them and keeping the same score as they got in july
#both have been average/yellow colored throughout the season
#using function add_row

september_map <- september_map %>%
  add_row(
    Date.Collected = 46295,
    Collector.Name = "Saskia Campbell",
    grid_id = 17,
    point_one = 1,
    point_two = 1,
    point_three = 1,
    point_four = 2,
    point_five = 1,
    point_six = 1,
    point_seven = 1,
    total_score = 8
  )

september_map <- september_map %>%
  add_row(
    Date.Collected = 46295,
    Collector.Name = "Saskia Campbell",
    grid_id = 18,
    point_one = 1,
    point_two = 1,
    point_three = 1,
    point_four = 1,
    point_five = 1,
    point_six = 1,
    point_seven = 1,
    total_score = 7
  )


september_map_final<-september_map%>%
  select(grid_id, total_score)
write.csv(september_map_final, "september_map_final.csv")



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
