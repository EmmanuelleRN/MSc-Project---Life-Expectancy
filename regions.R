rf_pred <- augment(final_rf_res)

rf_pred <- rf_pred |> 
  mutate(region = as.factor(countrycode(sourcevar = country_name, 
                                        origin = "country.name",
                                        destination = "region")))
rf_pred |>
  ggplot(aes(x = life_expectancy_at_birth_total_years , y = .pred)) + 
  geom_point(alpha = .15) +
  geom_abline(color = "red") + 
  facet_wrap(~region) +
  coord_obs_pred() + 
  ylab("Predicted Life Expectancy")

rf_pred |>
  group_by(region) |>
  summarise(rmse = sqrt(sum((life_expectancy_at_birth_total_years - .pred)^2) / n()),
            mae = sum(abs(life_expectancy_at_birth_total_years - .pred)) / n(),
            n = n(),
            max = max(life_expectancy_at_birth_total_years - .pred),
            min = min(life_expectancy_at_birth_total_years - .pred),
            mean = mean(life_expectancy_at_birth_total_years - .pred),
            sd = sd(life_expectancy_at_birth_total_years - .pred))
