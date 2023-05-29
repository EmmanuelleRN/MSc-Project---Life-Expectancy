library(tidyverse)
library(ggridges)

# Read dataset from World bank data
dt <- read.csv("dataset/b0312c21-b332-4f49-b781-9c0aa9c13825_Data.csv", na.strings = "..")

# Separate dataset
dt_countries <- dt[1:82243,]
dt_world_regions <- dt[82244:100435,]
dt_world <- dt[100436:100819,]

# remove original dataet for space
rm(dt)

# Organise data in tidy format
dt_to_use <- dt_countries |>
  select(-Series.Code) |>
  gather("year", "value", -c(Country.Name, Country.Code, Series.Name)) |>
  spread(Series.Name, value) |>
  janitor::clean_names() |>
  mutate(year = str_remove(year, "X\\d{4}..YR"))

# year as numeric
dt_to_use <- dt_to_use |>
  mutate(year = as.numeric(gsub("[.]", "", dt_to_use$year)))
  

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years) +
  geom_histogram(fill = "orange", col = "white")

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years) +
  geom_histogram(fill = "orange", col = "white") +
  facet_wrap(~year)

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years, y = year, group = year, fill = stat(x)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)
