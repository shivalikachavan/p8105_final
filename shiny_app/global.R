library(shiny)
library(shinydashboard)
library(tidyverse)   
library(plotly)
library(here)

brfss_data <- readRDS("data/brfss_small.rds")

brfss_data <- brfss_data |> 
  mutate(
    marital_status = factor(
      marital_status,
      levels = c(
        "Married",
        "Member of an unmarried couple (Partner)",
        "Never Married",
        "Divorced",
        "Separated",
        "Widowed")
    ),
    education_status = factor(
      education_status,
      levels = c(
        "Did not graduate High School",
        "Graduated High School",
        "Attended College or Technical School",
        "Graduated from College or Technical School")
    ),
    income_level = factor(
      income_level,
      levels = c(
        "Less than $10,000",
        "$10,000 to < $15,000",
        "$15,000 to < $20,000",
        "$20,000 to < $25,000",
        "$25,000 to < $35,000",
        "$35,000 to < $50,000",
        "$50,000 to < $75,000",
        "$75,000 or more")
    ),
    employment_status = factor(
      employment_status,
      levels = c(
        "Employed for wages",
        "Self-employed",
        "Homemaker",
        "Student",
        "Out of work (<1 year)",
        "Out of work (>1 year)",
        "Retired",
        "Unable to work (Disabled)")
    )
  ) |> 
  drop_na(
    age_group_5yr, urban_status, sex, marital_status, education_status,
    employment_status, income_level, race, insurance_coverage,
    mental_health_not_good_days, physical_health_not_good_days,
    depressive_disorder, binge_drink
  ) |> 
  
  rename(
    "Age Group" = age_group_5yr,
    "Urban Status" = urban_status,
    "Sex" = sex,
    "Marital Status" = marital_status,
    "Education Status" = education_status,
    "Employment Status" = employment_status,
    "Income Level" = income_level,
    "Race" = race,
    "Insurance Coverage" = insurance_coverage
  )

demographics <- c(
  "Age Group", "Sex", "Race", "Education Status",
  "Income Level", "Employment Status", "Marital Status",
  "Insurance Coverage", "Urban Status"
)