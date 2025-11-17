brfss_data_cleaning.Rmd
================
Shivalika Chavan
2025-11-13

I’ve written `filter_brfss_data.R` (essentially converting what was done
in the initial data exploration to a function) and
`standardize_brfss_variable.R`. Some variable names have changes over
the years, though they ask the same question and they are coded the same
way. For example `hlthpl2` in 2024 and `hlthpl1` in 2023 answer the same
question: Adults who had some form of health insurance and are both
coded as 1 = Yes, 2 = No, 9 = don’t know/refused/missing.

#### Reading, Standardizing, and Filtering the BRFSS Data from 2017 to 2024

``` r
read_standardize_clean = function(datayear, filepath) {
  
  print(datayear)
  
  cleaned_data = 
    read_xpt(filepath) |> 
    janitor::clean_names() |> 
    standardize_brfss_variable() |> 
    filter_brfss_data(data_year = datayear)
  
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
  unnest(cols = c(cleaned_dfs)) 
```

    ## [1] 2017
    ## [1] 2018
    ## [1] 2019
    ## [1] 2020
    ## [1] 2021
    ## [1] 2022
    ## [1] 2023
    ## [1] 2024

``` r
data |> write_csv("./data/brfss_clean_2017_2024.csv") # has some duplicate variables, handled in load/clean step
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

#### Command for loading the saved csv into a clean, factored format

``` r
cleaned_data = load_clean_brfss("./data/brfss_clean_2017_2024.csv") # command to load the cleaned csv
```

    ## Rows: 2749477 Columns: 39
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (2): imonth, iday
    ## dbl  (36): qstver, dispcode, state, seqno, iyear, sex, marital, educag, empl...
    ## date  (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
cleaned_data
```

    ## # A tibble: 2,749,477 × 32
    ##    id    date       qstver state urban_status age_group_5yr sex   marital_status
    ##    <chr> <date>      <dbl> <dbl> <fct>        <fct>         <fct> <fct>         
    ##  1 1201… 2017-01-30     10     1 <NA>         70-74         Fema… Widowed       
    ##  2 1201… 2017-01-12     10     1 <NA>         65-69         Male  Married       
    ##  3 1201… 2017-01-10     10     1 <NA>         70-74         Male  Married       
    ##  4 1201… 2017-01-30     10     1 <NA>         65-69         Fema… Widowed       
    ##  5 1201… 2017-01-30     10     1 <NA>         75-79         Male  Widowed       
    ##  6 1201… 2017-01-05     10     1 <NA>         65-69         Male  Married       
    ##  7 1201… 2017-02-02     10     1 <NA>         65-69         Fema… Divorced      
    ##  8 1201… 2017-01-24     10     1 <NA>         80 or older   Male  Married       
    ##  9 1201… 2017-01-12     10     1 <NA>         45-49         Male  Divorced      
    ## 10 1201… 2017-01-03     10     1 <NA>         65-69         Fema… Widowed       
    ## # ℹ 2,749,467 more rows
    ## # ℹ 24 more variables: education_status <fct>, employment_status <fct>,
    ## #   children_in_household <dbl>, income_level <fct>, race <fct>,
    ## #   insurance_coverage <fct>, medical_cost_barrier <fct>, general_health <fct>,
    ## #   general_health_refactored <fct>, michd <fct>, physical_health <dbl>,
    ## #   physical_health_not_good_days <fct>,
    ## #   leisure_physical_activity_last_30_days <fct>, mental_health <dbl>, …
