library(tidyverse)
library(dplyr)
june_map <- read.csv(
  "invasives_map_processing(june) (2).csv"
)

june_map<-june_map%>%
  select(What.grid.square.are.you.reporting.on..choose.1.49., Sample.Point.1,Sample.Point.2,
         Sample.Point.3, Sample.Point.4,Sample.Point.5,Sample.Point.6,Sample.Point.7, Think.about.the.distribution.of.woodies.in.the.grid.section..Are.they.mostly...,
         How.tall.is.the.tallest.invasive.woody.plant.in.this.grid.section., Of.the.multiflora.rose.in.this.grid.section..select.from.the.following.)

june_map<-june_map%>%
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

june_map <- june_map %>%
  mutate(across(starts_with("point_"), ~ 
                  .x %>%
                  str_remove("\\?") %>%   # remove question marks
                  str_trim()              # remove leading/trailing spaces
  ))
june_map <- june_map %>%
  mutate(across(starts_with("point_"), ~ 
                  .x %>%
                  str_replace_all("[^[:alnum:]\\-\\+ ]", "") %>% 
                  str_trim()
  ))

june_map <- june_map %>%
  mutate(across(starts_with("point_"), ~ case_when(
    .x == "0 individuals present" ~ 0,
    .x == "1-25 individuals present" ~ 1,
    .x == "25-50 individuals present" ~ 2,
    .x == "50-100 individuals present" ~ 3,
    .x == "100+ individuals present" ~ 4,
    TRUE ~ NA_real_
  )))


library(dplyr)
#combines grid_id 25
#april_map <- april_map %>%
 # group_by(grid_id) %>%
  #summarise(across(starts_with("point_"), ~ first(na.omit(.x))),
           # .groups = "drop")

#creating sum of scores

june_map <- june_map %>%
  mutate(total_score = rowSums(across(starts_with("point_")), na.rm = TRUE))

june_map<-june_map%>%
  rename(
    GRID_ID = grid_id)
june_map_final<-june_map%>%
  select(GRID_ID, total_score)
write.csv(june_map_final, "june_map.csv")

str(june_map_final)

library(dplyr)

june_map_final <- june_map_final %>%
  mutate(GRID_ID = as.character(GRID_ID))

str(june_map_final)

unique(june_map_final$GRID_ID)
june_map_final %>%
  mutate(check = paste0("[", grid_id, "]")) %>%
  distinct(check)
print(n = ...)

library(dplyr)
library(stringr)

june_map_final <- june_map_final %>%
  mutate(
    grid_id = as.character(GRID_ID),
    grid_id = str_replace_all(GRID_ID, "[^0-9]", ""),  # keep only numbers
    grid_id = str_trim(GRID_ID)
  )
write.csv(june_map_final, "june_map_final.csv", row.names = FALSE)
