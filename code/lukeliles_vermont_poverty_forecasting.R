# Vermont County-Level Poverty Forecasting (STAT 4840)
# This script:
# 1) Cleans and merges SAIPE, SNAP, and IRS poverty-related data using county FIPS codes
# 2) Builds log-linear time series regression models (TSLM) for county-level poverty counts
# 3) Evaluates model fit and residuals (incl. Ljung-Box)
#
# Note: Raw data files are not included in this repo. See the PDF report / README for sources.


library(tidyverse)
library(fpp3)


vt <- read.csv('SAIPE_04-22-2025.csv')

vt <- vt |>
  filter(!Name %in% c("United States", "Vermont")) |>
  mutate(Poverty.Universe = as.numeric(gsub(",", "", Poverty.Universe)),
         Number.in.Poverty = as.numeric(gsub(",", "", Number.in.Poverty)),
         county_fips = ID %% 1000,        
         state_fips = ID %/% 1000) |>
  rename(county_population = Poverty.Universe, Number_in_Poverty = Number.in.Poverty, county_name = Name) |>
  select(Year, ID, state_fips, county_fips, county_name, county_population, Number_in_Poverty) 


glimpse(vt)


# largest county with highest population
vt |>
  filter(Year == 2023) |>
  slice_max(county_population, n = 1) |>
  pull(county_name)

# top 9 counties with highest population over the years
top9 <- vt |>
  filter(Year == 2023) |>
  slice_max(county_population, n = 9) |>
  select(county_name, county_population)

top9


# visualizing poverty trend in 9 largest counties 
vt |>
  filter(county_name %in% top9$county_name) |>
  ggplot(aes(x = Year, y = Number_in_Poverty)) +
  geom_line() +
  facet_wrap(~ county_name, scales = "free_y") +
  labs(title = "Number in Poverty Over Time. Vermont's 9 Largest Counties", y = "Number in Poverty")



snap <- read_csv('cntysnap.csv', skip = 3)

snap <- snap |>
  mutate(across(`Jul-2006`:`Jul-1997`, ~ as.numeric(gsub(",", "", .)))) |>
  rename(state_fips = `State FIPS code`, county_fips = `County FIPS code`) |>
  filter(state_fips == 50 & county_fips != 0)



snap <- snap |> pivot_longer(cols = starts_with("Jul-"), names_to = "Year",
                                               values_to = "snap_benefits") |> 
  mutate(Year = as.integer(sub("Jul-", "", Year))) |>
  separate(Name, into = c("county_name", "state_abbr"), sep = ", ")
  

glimpse(snap)


snap |>
filter(county_name %in% top9$county_name) |>
  ggplot(aes(x = Year, y = snap_benefits, group = county_name)) +
  geom_line() +
  facet_wrap(~ county_name, scales = "free_y") +
  labs(title = "SNAP Benefits Over Time Vermont's 9 Largest Counties",
       x = "Month", y = "Number of SNAP Beneficiaries")


irs <- read_csv('irs.csv', skip = 3)

irs <- irs |>
  filter(`State FIPS code` == 50) |>
  rename(state_fips = `State FIPS code`, poor_exemptions = `Poor exemptions`) |>
  select(-Name)

glimpse(irs)

irs |>
ggplot(aes(x = Year, y = poor_exemptions)) +
  geom_line() +
  labs(title = "Poor Exemptions Filed in Vermont Over Time",
    x = "Year", y = "Number of Poor Exemptions") +
  theme_minimal()


glimpse(vt)
glimpse(snap)
glimpse(irs)



merged_df <- vt |>
  left_join(snap, by = c("state_fips", "county_fips", "Year")) |>
  select(-county_name.y) |>
  rename(county_name = county_name.x)


merged_df <- merged_df |>
  left_join(irs, by = c('state_fips', 'Year')) |> 
  select(Year, state_fips, county_fips, county_name, Number_in_Poverty, county_population,
    snap_benefits, poor_exemptions) |>
  filter(Year >= 1997)


clean_df <- merged_df |>
  filter(!is.na(snap_benefits), !is.na(poor_exemptions))


final_tsibble <- clean_df |>
  as_tsibble(key = c(county_fips, county_name), index = Year)

glimpse(final_tsibble)

clean_df |>
ggplot(aes(x = snap_benefits, y = Number_in_Poverty)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Poverty vs SNAP Recipients", x = "SNAP Recipients", y = "Number in Poverty") +
  theme_minimal()


clean_df |>
ggplot(aes(x = poor_exemptions, y = Number_in_Poverty)) +
  geom_point(alpha = 0.6, color = "forestgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(title = "Poverty vs Poor Exemptions", x = "Poor Exemptions (State-level)",
    y = "Number in Poverty") +
  theme_minimal()


clean_df |>
ggplot(aes(x = Year, y = Number_in_Poverty)) +
  geom_line(aes(color = county_name), show.legend = FALSE) +
  facet_wrap(~ county_name, scales = "free_y") +
  labs(title = "Trends in Poverty Over Time by County", x = "Year", y = "Number in Poverty") +
  theme_minimal()

clean_df |>
  ggplot(aes(x = Year, y = poor_exemptions)) +
  geom_line(aes(color = county_name), show.legend = FALSE) +
  facet_wrap(~ county_name, scales = "free_y") +
  labs(title = "Trends in Poor Exemptions Over Time by County", x = "Year", y = "Poor Exemptions") +
  theme_minimal()

clean_df |>
  ggplot(aes(x = Year, y = snap_benefits)) +
  geom_line(aes(color = county_name), show.legend = FALSE) +
  facet_wrap(~ county_name, scales = "free_y") +
  labs(title = "Trends in SNAP Recipients Over Time by County", x = "Year", y = "SNAP Recipients") +
  theme_minimal()


## 2. Linear Models


# all 7 linear models
models <- final_tsibble |>
  model(
    model1 = TSLM(log(Number_in_Poverty) ~ log(county_population)),
    model2 = TSLM(log(Number_in_Poverty) ~ log(snap_benefits)),
    model3 = TSLM(log(Number_in_Poverty) ~ log(poor_exemptions)),
    model4 = TSLM(log(Number_in_Poverty) ~ log(county_population) + log(snap_benefits)),
    model5 = TSLM(log(Number_in_Poverty) ~ log(county_population) + log(log(poor_exemptions))),
    model6 = TSLM(log(Number_in_Poverty) ~ log(snap_benefits) + log(poor_exemptions)),
    model7 = TSLM(log(Number_in_Poverty) ~ log(county_population) + log(poor_exemptions) +
                                                                          log(poor_exemptions)))

glance(models) |>
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) |> 
  arrange(desc(adj_r_squared))

glance(models) |>
  select(.model, adj_r_squared, CV, AIC, AICc, BIC) |> 
  arrange(CV)


models |>
  select(model4) |>
  augment() |>
  filter(county_name %in% top9$county_name) |>
  ggplot(aes(x = Year)) +
  geom_line(aes(y = Number_in_Poverty, color = "Actual")) +
  geom_line(aes(y = .fitted, color = "Predicted")) +
  facet_wrap(~ county_name, scales = "free_y") +
  labs(title = "Actual vs Predicted Number in Poverty (Top 9 Counties)",
       y = "Number in Poverty", color = "Legend") +
  theme_minimal()


models |>
  select(model4) |>
  augment() |>
  filter(county_name %in% top9$county_name) |>
  ggplot(aes(x = Year, y = .resid)) +
  geom_line() +
  facet_wrap(~county_name, scales = "free_y") +
  labs(title = "Innovation Residuals (log scale) for Largest 9 Counties",
       y = "Residuals (log scale)", x = "Year")



models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Addison County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Bennington County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Caledonia County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Chittenden County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Essex County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Franklin County') |>
  features(.innov, ljung_box, lag = 10)


models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Grand Isle County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Lamoille County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Orange County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Rutland County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Washington County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Windham County') |>
  features(.innov, ljung_box, lag = 10)

models |>
  select(model4) |>
  augment() |>
  filter(county_name == 'Windsor County') |>
  features(.innov, ljung_box, lag = 10)




