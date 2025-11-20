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
outcome_vars = c("general_health_refactored", "physical_health_not_good_days","mental_health_not_good_days", "education_status", "income_level", "depressive_disorder", "life_satisfaction", "loneliness")

summarize_outcome_by_year = function(outcome_var, df = brfss_data) {
  
  outcome_sym = rlang::sym(outcome_var) 
  
  df |> 
    group_by(year(date), !!outcome_sym) |> 
    summarize(n = n()) |> 
    ungroup() |> 
    pivot_wider(
      names_from = !!outcome_sym, 
      values_from = n
    ) |>
    rename(year = `year(date)`)
}

summary_by_year = tibble(
  outcome = outcome_vars,
  summary = map(outcome_vars, summarize_outcome_by_year)
  ) 
```

    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.
    ## `summarise()` has grouped output by 'year(date)'. You can override using the
    ## `.groups` argument.

``` r
for(outcome_var in summary_by_year) {
  print(outcome_var)
}
```

    ## [1] "general_health_refactored"     "physical_health_not_good_days"
    ## [3] "mental_health_not_good_days"   "education_status"             
    ## [5] "income_level"                  "depressive_disorder"          
    ## [7] "life_satisfaction"             "loneliness"                   
    ## [[1]]
    ## # A tibble: 8 × 4
    ##    year `Fair or Poor Health` `Good or Better Health`  `NA`
    ##   <dbl>                 <int>                   <int> <int>
    ## 1  2017                 71884                  295828   900
    ## 2  2018                 69253                  288657   875
    ## 3  2019                 64999                  264610   753
    ## 4  2020                 49942                  270536   692
    ## 5  2021                 57396                  283753   835
    ## 6  2022                 60516                  273021   870
    ## 7  2023                 64285                  271919   931
    ## 8  2024                 70438                  285641   943
    ## 
    ## [[2]]
    ## # A tibble: 8 × 5
    ##    year    `0` `1-13` `14+`  `NA`
    ##   <dbl>  <int>  <int> <int> <int>
    ## 1  2017 224841  85111 51255  7405
    ## 2  2018 219874  82017 49798  7096
    ## 3  2019 198864  77318 46331  7849
    ## 4  2020 221182  58275 35063  6650
    ## 5  2021 223922  70372 40596  7094
    ## 6  2022 200416  82141 44188  7662
    ## 7  2023 198984  84181 45895  8075
    ## 8  2024 207917  90586 50299  8220
    ## 
    ## [[3]]
    ## # A tibble: 8 × 5
    ##    year    `0` `1-13` `14+`  `NA`
    ##   <dbl>  <int>  <int> <int> <int>
    ## 1  2017 245993  76386 40594  5639
    ## 2  2018 237363  75561 40356  5505
    ## 3  2019 211714  73230 38768  6650
    ## 4  2020 204134  72547 38467  6022
    ## 5  2021 211403  82104 42621  5856
    ## 6  2022 199501  83654 44925  6327
    ## 7  2023 199663  86524 44902  6046
    ## 8  2024 211120  91894 47984  6024
    ## 
    ## [[4]]
    ## # A tibble: 8 × 6
    ##    year Attended College or Tech…¹ Did not graduate Hig…² Graduated from Colle…³
    ##   <dbl>                      <int>                  <int>                  <int>
    ## 1  2017                     103194                  25551                 139624
    ## 2  2018                      99934                  25549                 135281
    ## 3  2019                      93278                  22409                 126545
    ## 4  2020                      90263                  20123                 125164
    ## 5  2021                      94803                  19222                 141239
    ## 6  2022                      91744                  18432                 143192
    ## 7  2023                      90290                  18338                 145927
    ## 8  2024                      95141                  19742                 153000
    ## # ℹ abbreviated names: ¹​`Attended College or Technical School`,
    ## #   ²​`Did not graduate High School`,
    ## #   ³​`Graduated from College or Technical School`
    ## # ℹ 2 more variables: `Graduated High School` <int>, `NA` <int>
    ## 
    ## [[5]]
    ## # A tibble: 8 × 10
    ##    year `$10,000 to < $15,000` `$15,000 to < $20,000` `$20,000 to < $25,000`
    ##   <dbl>                  <int>                  <int>                  <int>
    ## 1  2017                  16052                  22703                  28079
    ## 2  2018                  15028                  21240                  26672
    ## 3  2019                  12936                  18609                  24008
    ## 4  2020                  11206                  17024                  22544
    ## 5  2021                   9252                  11910                  16821
    ## 6  2022                   8640                  11039                  15799
    ## 7  2023                   7937                  10119                  14302
    ## 8  2024                   8389                  10732                  15725
    ## # ℹ 6 more variables: `$25,000 to < $35,000` <int>,
    ## #   `$35,000 to < $50,000` <int>, `$50,000 to < $75,000` <int>,
    ## #   `$75,000 or more` <int>, `Less than $10,000` <int>, `NA` <int>
    ## 
    ## [[6]]
    ## # A tibble: 8 × 4
    ##    year     No   Yes  `NA`
    ##   <dbl>  <int> <int> <int>
    ## 1  2017 292079 74878  1655
    ## 2  2018 288513 68623  1649
    ## 3  2019 264496 64112  1754
    ## 4  2020 257406 62145  1619
    ## 5  2021 272037 68070  1877
    ## 6  2022 262047 70372  1988
    ## 7  2023 264728 70481  1926
    ## 8  2024 278117 76946  1959
    ## 
    ## [[7]]
    ## # A tibble: 8 × 6
    ##    year Dissatisfied Satisfied `Very dissatisfied` `Very satisfied`   `NA`
    ##   <dbl>        <int>     <int>               <int>            <int>  <int>
    ## 1  2017          689      8457                 152             9196 350118
    ## 2  2018           NA        NA                  NA               NA 358785
    ## 3  2019           NA        NA                  NA               NA 330362
    ## 4  2020           NA        NA                  NA               NA 321170
    ## 5  2021           NA        NA                  NA               NA 341984
    ## 6  2022         9726    110453                2738           102457 109033
    ## 7  2023         8436     97415                2315            88939 140030
    ## 8  2024         8141     91706                2242            83175 171758
    ## 
    ## [[8]]
    ## # A tibble: 8 × 7
    ##    year   `NA` Always Never Rarely Sometimes Usually
    ##   <dbl>  <int>  <int> <int>  <int>     <int>   <int>
    ## 1  2017 368612     NA    NA     NA        NA      NA
    ## 2  2018 358785     NA    NA     NA        NA      NA
    ## 3  2019 330362     NA    NA     NA        NA      NA
    ## 4  2020 321170     NA    NA     NA        NA      NA
    ## 5  2021 341984     NA    NA     NA        NA      NA
    ## 6  2022 334407     NA    NA     NA        NA      NA
    ## 7  2023 139177   5208 78502  61466     45616    7166
    ## 8  2024 171096   4602 70214  60360     43880    6870
