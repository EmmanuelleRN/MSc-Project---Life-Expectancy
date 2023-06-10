library(tidyverse)
library(ggridges)
library(countrycode)
library(ggpubr)
library(gtsummary)

# Read dataset from World bank data
dt <- read.csv("dataset/b0312c21-b332-4f49-b781-9c0aa9c13825_Data.csv", na.strings = "..")
dt_countries <- read.csv("dataset/P_Data_Extract_From_World_Development_Indicators_filtered/b99ba33f-2507-4508-90ff-3a41690c4277_Data.csv", na.strings = "..")
dt_countries <- dt_countries[1:49910,]

# Separate dataset
dt_countries <- dt[1:82243,]
dt_world_regions <- dt[82244:100435,]
dt_world <- dt[100436:100814,]

# remove original dataet for space
rm(dt)

# Organise data in tidy format
dt_to_use <- dt_countries |>
  select(-Series.Code) |>
  gather("year", "value", -c(Country.Name, Country.Code, Series.Name)) |>
  spread(Series.Name, value) |>
  janitor::clean_names() |>
  mutate(year = str_remove(year, "X\\d{4}..YR")) 

# Organise data in tidy format
dt_to_use <- dt_world |>
  select(-c(Country.Name, Country.Code, Series.Code)) |>
  gather("year", "value", -Series.Name) |>
  spread(Series.Name, value) |>
  janitor::clean_names() |>
  mutate(year = str_remove(year, "X\\d{4}..YR")) 

# year as numeric
dt_to_use <- dt_to_use |>
  mutate(year = as.numeric(gsub("[.]", "", dt_to_use$year)))
  
# add region information
dt_to_use <- dt_to_use |>
  mutate(country_name = ifelse(country_name == "Turkiye", "Turkey", country_name)) |>
  mutate(region = as.factor(countrycode(sourcevar = country_name, 
                                        origin = "country.name",
                                        destination = "region"))) |>
  mutate(country_name = as.factor(country_name),
         country_code = as.factor(country_code))

#we don't have the year of 2022 -> remove it
dt_to_use <- dt_to_use |>
  filter(year != 2022)

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years) +
  geom_histogram(fill = "orange", col = "white")

gghistogram(dt_to_use, x = "life_expectancy_at_birth_total_years", 
             add = "mean", rug = TRUE) 

dt_to_use |>
  tbl_summary(
    include = c(life_expectancy_at_birth_total_years),
    type = all_continuous() ~ "continuous2",
    statistic = all_continuous() ~ c("{median} ({p25}, {p75})", "{min}, {max}", "{mean} ± {sd}"),
    missing = "always",
    digits = list(all_continuous() ~ 2)) %>%
  as_gt() %>%
  gt::as_latex()

dt_to_use |>
  filter(life_expectancy_at_birth_total_years < 30)

# + 
  # stat_summary(
  #   aes(
  #     y = 200, 
  #     label = after_stat(paste("m", mean))
  #   ),
  #   geom = "text",
  #   fun.data = ~ round(data.frame(mean = mean(.x), 2))
  # )
  # stat_summary(fun.data = function(x) data.frame(y=200, label = paste("Mean=",mean(x))), geom="text") +
  # theme(legend.position="none")

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years) +
  geom_histogram(fill = "orange", col = "white") +
  facet_wrap(~year)

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years) +
  geom_histogram(fill = "orange", col = "white") +
  facet_wrap(~region)

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years, y = year, group = year, fill = stat(x)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01)

dt_to_use |>
  ggplot() +
  aes(x = life_expectancy_at_birth_total_years, y = year, group = year, fill = stat(x)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  facet_wrap(~region)
