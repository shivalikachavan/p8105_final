EDA: Outcome Variables Years Available
================
Shivalika Chavan
2025-11-19

Purpose: Identify which years outcome variables are available to use for
analysis

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
source(here::here("source", "load_clean_brfss.R"))
brfss_data = load_clean_brfss(here::here("data", "brfss_clean_2017_2024.csv"))
```

    ## Rows: 36 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): state, abbr
    ## dbl  (1): fips
    ## date (3): first_start, online, offline
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
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
outcome_vars = c("general_health", "general_health_refactored", "michd",
                 "physical_health", "physical_health_not_good_days", "leisure_physical_activity_last_30_days",
                 "mental_health", "mental_health_not_good_days", "poor_health", "depressive_disorder",
                 "difficulty_self_care", "life_satisfaction", "emotional_support", "loneliness", "binge_drink",
                 "heavy_drink", "lost_reduced_employment", "financial_strain_bills", "financial_strain_utilities")

summarize_outcome_by_year = function(outcome_var, df = brfss_data) {
  
  outcome_sym = rlang::sym(outcome_var) 
  
  df |> 
    group_by(year(date), !!outcome_sym) |> 
    summarize(n = n(), .groups = "drop") |> 
    pivot_wider(
      names_from = `year(date)`, 
      values_from = n
    )
}

find_complete_years = function(outcome_var, summary_df){
  
  outcome_sym = rlang::sym(outcome_var) 
  
  summary_df |> 
    filter(!is.na(!!outcome_sym)) |> 
    summarize(
      across(
        .cols = everything(),
        .fns = ~ !all(is.na(.x)) 
        )
      )|>
    select(-!!outcome_sym) |> 
    pivot_longer(
      cols = everything(),
      names_to = "year",
      values_to = "is_complete"
      ) |> 
    summarize(
      n_complete_years = sum(is_complete)
      ) |> 
    pull(n_complete_years)
    
}

summary_by_year = tibble(
  outcome = outcome_vars,
  summary = map(outcome_vars, summarize_outcome_by_year),
  n_complete_years = map2(outcome_vars, summary, find_complete_years)
  ) |> 
  arrange(n_complete_years)

summary_by_year
```

    ## # A tibble: 19 × 3
    ##    outcome                                summary           n_complete_years
    ##    <chr>                                  <list>            <list>          
    ##  1 general_health                         <tibble [6 × 9]>  <int [1]>       
    ##  2 general_health_refactored              <tibble [3 × 9]>  <int [1]>       
    ##  3 michd                                  <tibble [3 × 9]>  <int [1]>       
    ##  4 physical_health                        <tibble [32 × 9]> <int [1]>       
    ##  5 physical_health_not_good_days          <tibble [4 × 9]>  <int [1]>       
    ##  6 leisure_physical_activity_last_30_days <tibble [3 × 9]>  <int [1]>       
    ##  7 mental_health                          <tibble [32 × 9]> <int [1]>       
    ##  8 mental_health_not_good_days            <tibble [4 × 9]>  <int [1]>       
    ##  9 poor_health                            <tibble [32 × 9]> <int [1]>       
    ## 10 depressive_disorder                    <tibble [3 × 9]>  <int [1]>       
    ## 11 difficulty_self_care                   <tibble [3 × 9]>  <int [1]>       
    ## 12 binge_drink                            <tibble [3 × 9]>  <int [1]>       
    ## 13 heavy_drink                            <tibble [3 × 9]>  <int [1]>       
    ## 14 life_satisfaction                      <tibble [5 × 9]>  <int [1]>       
    ## 15 emotional_support                      <tibble [6 × 9]>  <int [1]>       
    ## 16 financial_strain_bills                 <tibble [3 × 9]>  <int [1]>       
    ## 17 financial_strain_utilities             <tibble [3 × 9]>  <int [1]>       
    ## 18 loneliness                             <tibble [6 × 9]>  <int [1]>       
    ## 19 lost_reduced_employment                <tibble [3 × 9]>  <int [1]>

``` r
summary_by_year |> 
  filter(
    n_complete_years == 8
  ) |> 
  pull(outcome)
```

    ##  [1] "general_health"                        
    ##  [2] "general_health_refactored"             
    ##  [3] "michd"                                 
    ##  [4] "physical_health"                       
    ##  [5] "physical_health_not_good_days"         
    ##  [6] "leisure_physical_activity_last_30_days"
    ##  [7] "mental_health"                         
    ##  [8] "mental_health_not_good_days"           
    ##  [9] "poor_health"                           
    ## [10] "depressive_disorder"                   
    ## [11] "difficulty_self_care"                  
    ## [12] "binge_drink"                           
    ## [13] "heavy_drink"

This last set of health outcomes have data for every year, so we can use
them in a time series analysis with sports legalization.
