#script to process invasives mapping from 2026 season
library(tidyverse)
library(dplyr)

#reading in months
april<-read.csv("april_map_final.csv")
june<-read.csv("june_map_final.csv")
july<-read.csv("july_map_final.csv")
september<-read.csv("september_map_final.csv")

#cleaning months up for purpose of combining 4 dataframes into 1
april<-april%>%
  select(grid_id, total_score)

june<-june%>%
  select(grid_id, total_score)

july<-july%>%
  select(grid_id, total_score)

september<-september%>%
  select(grid_id, total_score)



#renaming total_score to be month specific
april <- april %>%
  rename(april_score = total_score)

june <- june %>%
  rename(june_score = total_score)


july <- july %>%
  rename(july_score = total_score)

september <- september %>%
  rename(september_score = total_score)
#july had two scores for section 15
july %>%
  filter(grid_id == 27)
july <- july %>%
  distinct(grid_id, .keep_all = TRUE)

#joining them together
GP_invasives_2026 <- april %>%
  left_join(june, by = "grid_id") %>%
  left_join(july, by = "grid_id") %>%
  left_join(september, by = "grid_id")

#adding in corresponding field names
field_to_grid<-read.csv("field to grid_id.csv")

#joining to bigger df
GP_invasives_2026<-GP_invasives_2026 %>%
  left_join(field_to_grid, by = "grid_id")

GP_invasives_2026 <- GP_invasives_2026 %>%
  relocate(field, .before = grid_id)

#reading in impact and labor/treatment record
labor_and_impact<-read.csv("labor_and_impact.csv")


labor_and_impact <- labor_and_impact %>%
  mutate(Field = tolower(Field))

#tallying up different types of impacts)

impact_count <- labor_and_impact %>%
  group_by(grid_id, impact.type) %>%
  summarise(count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = impact.type,
    values_from = count,
    values_fill = 0
  )

#making a total_impacts column
impact_count <- impact_count %>%
  mutate(total_mech_impacts = rowSums(across(c(
    `rotary mow`,
    `small machinery mow`,
    `hand work`
  ))))

#hours

labor_and_impact <- labor_and_impact %>%
  mutate(hours = as.numeric(hours))
total_hours <- labor_and_impact %>%
  group_by(grid_id) %>%
  summarise(
    total_hours = sum(hours, na.rm = TRUE),
    .groups = "drop"
  )

#now can add total hours, impact types to gp_invasives_2026

total_hours <- total_hours %>%
  mutate(grid_id = as.integer(grid_id))

impact_count <- impact_count %>%
  mutate(grid_id = as.integer(grid_id))
GP_invasives_2026 <- GP_invasives_2026 %>%
  left_join(total_hours, by = "grid_id") %>%
  left_join(impact_count, by = "grid_id")








