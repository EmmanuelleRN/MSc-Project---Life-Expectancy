dt_to_use %>% summary()

library(purrr)

dt_to_use %>%
  map_df(function(x) sum(is.na(x))) %>%
  gather(feature, num_nulls) %>%
  filter(num_nulls == 0)

# each year has 217 observations
# most recent years don't have most observations -> late notification
dt_to_use |>
  group_by(year) |>
  summarise(across(where(is.numeric),  ~sum(is.na(.)))) |>
  gather("series_name", "missing", -c(year)) |>
  mutate(missing_pct = missing / 217 * 100) |>
  arrange(desc(missing_pct))
  #spread(year, value)

# no notification in the years before 2000s
dt_to_use |>
  group_by(year) |>
  summarise(across(where(is.numeric),  ~sum(is.na(.)))) |>
  gather("series_name", "missing", -c(year)) |>
  mutate(missing_pct = missing / 217 * 100) |>
  arrange(desc(missing_pct)) %>% 
  filter(year < 2000, missing_pct == 100) %>% 
  select(series_name) %>% 
  unique() %>% View()

dt_to_use |>
  group_by(year, country_name) |>
  summarise(across(where(is.numeric),  ~sum(is.na(.)))) |>
  gather("series_name", "missing", -c(year, country_name)) |>
  arrange(desc(missing))

vars_to_drop <- dt_to_use |>
  group_by(year) |>
  summarise(across(where(is.numeric),  ~sum(is.na(.)))) |>
  gather("series_name", "missing", -c(year)) |>
  filter(missing == 217) |>
  group_by(series_name) |>
  summarise(missing_years = n()) |>
  mutate(pct_missing = missing_years / 24 * 100) |>
  arrange(desc(missing_years)) |>
  filter(pct_missing > 50) %>%
  .$series_name

dt_to_use |>
  group_by(year, country_name) |>
  summarise(across(where(is.numeric),  ~sum(is.na(.)))) |>
  gather("series_name", "missing", -c(year, country_name)) |>
  filter(missing == 1) |>
  group_by(series_name, country_name) |>
  summarise(missing_years = n()) |>
  filter(series_name %in% vars_to_drop)


dt_filtered <- dt_to_use |>
  select(-all_of(vars_to_drop))

is.na(dt_filtered) %>% colSums()

library(doBy)
summaryBy(. ~ region,
          data = dt_to_use,
          FUN = summary
)

# missing data
library(mice)

md.pattern(dt_to_use)

library(Amelia)
#specify columns and run amelia
amelia_fit <- amelia(dt_to_use, m=5, parallel = "multicore", ts = "year", 
                     cs = "country_name",
                     id = c("country_code", "region"))

iris.imp <- missForest(dt_to_use %>% select(-c(country_name, country_code, year, region)))
