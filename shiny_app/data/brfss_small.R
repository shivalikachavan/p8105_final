library(readr)
library(dplyr)

source(here::here("source", "load_clean_brfss.R"))
brfss_data <- load_clean_brfss(
  here::here("data", "brfss_clean_2017_2024.csv.zip")
)

# Select only the columns the dashboard actually needs
brfss_small <- brfss_data |> 
  select(
    age_group_5yr, urban_status, sex, marital_status,
    education_status, employment_status, income_level, race,
    insurance_coverage, mental_health_not_good_days,
    physical_health_not_good_days, depressive_disorder,
    binge_drink
  ) |> 
  drop_na()

saveRDS(brfss_small, "data/brfss_small.rds")
