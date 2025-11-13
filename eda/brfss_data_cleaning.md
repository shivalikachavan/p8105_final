brfss_data_cleaning.Rmd
================
Shivalika Chavan
2025-11-13

I’ve written `clean_brfss_data.R` (essentially converting what was done
in the initial data exploration to a function) and
`standardize_brfss_variable.R`. Some variable names have changes over
the years, though they ask the same question and they are coded the same
way. For example `hlthpl2` in 2024 and `hlthpl1` in 2023 answer the same
question: Adults who had some form of health insurance and are both
coded as 1 = Yes, 2 = No, 9 = don’t know/refused/missing.

Importing the raw files one at a time. Takes a while so I only want to
do this once per session. Commented out so knitting isn’t slow.

``` r
# brfss_2024_data_raw = read_xpt("./data/raw_data/brfss/LLCP2024.XPT") |> janitor::clean_names()
# brfss_2023_data_raw = read_xpt("./data/raw_data/brfss/LLCP2023.XPT") |> janitor::clean_names()
# brfss_2022_data_raw = read_xpt("./data/raw_data/brfss/LLCP2022.XPT") |> janitor::clean_names()
# brfss_2021_data_raw = read_xpt("./data/raw_data/brfss/LLCP2021.XPT") |> janitor::clean_names()
# brfss_2020_data_raw = read_xpt("./data/raw_data/brfss/LLCP2020.XPT") |> janitor::clean_names()
# brfss_2019_data_raw = read_xpt("./data/raw_data/brfss/LLCP2019.XPT") |> janitor::clean_names()
# brfss_2018_data_raw = read_xpt("./data/raw_data/brfss/LLCP2018.XPT") |> janitor::clean_names()
# brfss_2017_data_raw = read_xpt("./data/raw_data/brfss/LLCP2017.XPT") |> janitor::clean_names()
```

Testing the sequence of functions for cleaning/standardizing. Will
re-test each one when changes are made. Commented out so knitting isn’t
slow.

``` r
# brfss_2024_data_clean =  brfss_2024_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2024)
# brfss_2023_data_clean =  brfss_2023_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2023)
# brfss_2022_data_clean =  brfss_2022_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2022)
# brfss_2021_data_clean =  brfss_2021_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2021)
# brfss_2020_data_clean =  brfss_2020_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2020)
# brfss_2019_data_clean =  brfss_2019_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2019)
# brfss_2018_data_clean =  brfss_2018_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2018)
# brfss_2017_data_clean =  brfss_2017_data_raw |>  standardize_brfss_variable() |>  clean_brfss_data(data_year = 2017)
```

``` r
read_standardize_clean = function(datayear, filepath) {
  
  cleaned_data = 
    read_xpt(filepath) |> 
    janitor::clean_names() |> 
    standardize_brfss_variable() |> 
    clean_brfss_data(data_year = datayear)
  
  cleaned_data
}

data_years = 2017:2024
filepaths <- paste0("./data/raw_data/brfss/LLCP", data_years, ".XPT")

data = 
  tibble(
    data_year = data_years,
    filepath = filepaths,
    cleaned_dfs = map2(data_year, filepath, read_standardize_clean)
  ) |> 
  select(cleaned_dfs) |> 
  unnest(cols = c(cleaned_dfs)) |> 
  write_csv("./data/brfss_clean_2017_2024.csv")
```

Summarizing the changes made:

- `hlthpl_standard` handles different versions of the question: Adults
  who had some form of health insurance. `hlthpl1` was used until 2023.
  Starting in 2024, `hlthpl2` was used.
- `rfdrh_standard` handles the different response variable for “Heavy
  drinkers (adult men having more than 14 drinks per week and adult
  women having more than 7 drinks per week).” There are several versions
  of this over the years.
- `rfbing_standard` handles the different response variable for “Binge
  drinkers (males having five or more drinks on one occasion, females
  having four or more drinks on one occasion)” There are several
  versions of this over the years.
- `medcost_standard` handles the different response variable for “Was
  there a time in the past 12 months when you needed to see a doctor but
  could not because you could not afford it?” There are several versions
  of this over the years.
- `addepe_standard` handles the different response variable for ” (Ever
  told) (you had) a depressive disorder (including depression, major
  depression, dysthymia, or minor depression)?” There are several
  versions of this over the years.
- `sdlonely` is variable to track responses to “How often do you feel
  lonely?” which was only included in 2023 and 2024.
- `lsatisfy` is variable to track responses to “In general, how
  satisfied are you with your life?” which was only included starting in
  2023.
- `emtsuprt` is variable to track responses to “How often do you get the
  social and emotional support you need?” which was only included
  starting in 2023.
- `sdhemply` is variable to track responses to “In the past 12 months
  have you lost employment or had hours reduced?” which was only
  included starting in 2022.
- `sdhbills` is variable to track responses to “During the last 12
  months, was there a time when you were not able to pay your mortgage,
  rent or utility bills?” which was only included starting in 2022.
- `sdhutils` is variable to track responses to “During the last 12
  months was there a time when an electric, gas, oil, or water company
  threatened to shut off services?” which was only included starting in
  2022.
- `medcost1` is variable to track responses to which was only included
  starting in 2021.
- `sex1` renamed to `sex` for years before 2019.
- `urbstat` was not asked in 2017.
