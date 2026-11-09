library(tidyverse)
library(dplyr)
comp_mois <-read.csv("2026 soil moisture and compaction(data) (1).csv")

comp_mois <- comp_mois%>%
  select(Field, Moisture.12cm, Compaction..6.in, Compaction.18.in)

library(dplyr)

comp_mois <- comp_mois %>%
  mutate(Field = recode(as.character(Field),
                        "gp4c" = "GP4C",
                        "GP3"  = "GP3S",
                        "gp3s" = "GP3S",
                        "GP3s" = "GP3S",
                        "gp5b" = "GP5B",
                        "gp6b" = "GP6B",
                      "gp6c" = "GP6C",
                      "GP6c" = "GP6C"))


library(ggplot2)


ggplot(comp_mois, aes(x = Moisture.12cm, y = Compaction..6.in)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Soil Moisture vs. Soil Compaction",
    x = "Soil Moisture",
    y = "Soil Compaction"
  ) +
  theme_minimal()  

ggplot(comp_mois, aes(x = Moisture.12cm, y = Compaction.18.in)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Soil Moisture vs. Soil Compaction",
    x = "Soil Moisture",
    y = "Soil Compaction"
  ) +
  theme_minimal()  
