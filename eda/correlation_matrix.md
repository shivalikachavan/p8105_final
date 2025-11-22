Correlation Matrix
================
Angelica Bailey
2025-11-19

Identifying relationships between several variables in the dataset with
correlation matrix, summarizing the strength and direction of the
relationship between pair of variables.

Loading in data

``` r
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

Testing correlation

``` r
#selecting variables 

correlation_vars = c("general_health","general_health_refactored", 
                     "mental_health", "mental_health_not_good_days",
                     "physical_health", "physical_health_not_good_days",
                     "poor_health", "depressive_disorder",
                     "difficulty_self_care", "michd",
                     "binge_drink", "heavy_drink", 
                     "leisure_physical_activity_last_30_days")


#stratifying datasets by sex

brfss_males = brfss_data |>
  filter(sex == "Male") |>
  select(all_of(correlation_vars)) |> 
  mutate_all(as.numeric)

brfss_females = brfss_data |>
  filter(sex == "Female") |>
  select(all_of(correlation_vars)) |> 
  mutate_all(as.numeric)


#creating matrices and visualizing side by side

corr_males = 
  cor(brfss_males, 
      method = "spearman",
      use = "pairwise.complete.obs") |> 
  ggcorrplot(
    type = "upper",  
    method = "square", 
    lab = TRUE,               
    lab_size = 3, 
    title = "Correlations: males") +
  theme(plot.title = element_text(hjust = 0.5))
  
corr_females =
  cor(brfss_females,
      method = "spearman",
      use = "pairwise.complete.obs") |> 
  ggcorrplot(
    type = "upper",
    method = "square",
    lab = TRUE,
    lab_size = 3,
    title = "Correlations: females") +
  theme(plot.title = element_text(hjust = 0.5))
```

Correlations between health variables stratified by sex does not show
significant difference between males and females.

Creating non stratified correlation matrix:

``` r
brfss_all = brfss_data |> 
  select(all_of(correlation_vars)) |> 
  mutate_all(as.numeric)

corr = 
  cor(brfss_all, 
      method = "spearman",
      use = "pairwise.complete.obs") |> 
  ggcorrplot(
    type = "upper",
    method = "square",
    lab = TRUE,
    lab_size = 3,
    title = "Correlations",
    theme(plot.title = element_text(hjust = 0.5))
  )

corr
```

![](correlation_matrix_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Comments:

- `mental_health` vs. `mental_health_not_good_days` and
  `physical_health` vs. `physical_health_not_good_days` are highly
  correlated (0.99). These are redundant variables.

- Some variables related to general well-being that show moderate
  positive correlations: `depressive_disorder` correlates moderately
  with both `mental_health` and `difficulty_self_care`. Individuals with
  depressive disorders report more poor mental health days and
  difficulty taking care of themselves. Additionally, `poor_health`
  correlates with `physical_health`(0.43) and
  `difficulty_self_care`(0.40).

- `heavy_drink` and `binge_drink` have very weak correlations with
  almost everything else. `michd` shows generally weak correlations with
  the mental health variables.

- `general_health_refactored` shows negative correlations with variables
  like `physical_health` and `mental_health`. As general health gets
  better (lower score), the number of bad health days goes up (?) This
  is an unexpected result.
