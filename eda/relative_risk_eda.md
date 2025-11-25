relative_risk_eda
================

According to PEW research, “Young adults are more likely than older
Americans to say they’ve placed a sports bet in the past year.”

``` r
run_fishers_test = function(df){
  summary_df_wide = 
    df |> 
    mutate(
    sports_betting_legal = 
      case_match(
        sb_legal,
        1 ~ TRUE,
        0 ~ FALSE,
        NA ~ FALSE
      )
    ) |> 
    group_by(sports_betting_legal, outcome) |> 
    summarize(n = n(), .groups = "drop") |> 
    pivot_wider(
      names_from = outcome,
      values_from = n,
      names_prefix = "outcome_"
    )
  
  contingency_matrix = summary_df_wide |>
    select(outcome_TRUE, outcome_FALSE) |> 
    as.matrix()
  
  p_value = fisher.test(contingency_matrix) |> broom::tidy() |> pull(p.value)
  
  rr_result = riskratio(contingency_matrix, method = "wald")
  
  unexposed_poor_outcome = contingency_matrix[1,1] 
  unexposed_good_outcome = contingency_matrix[1,2] 
  exposed_poor_outcome = contingency_matrix[2,1] 
  exposed_good_outcome = contingency_matrix[2,2] 
  
  risk_unexposed = unexposed_poor_outcome / (unexposed_poor_outcome + unexposed_good_outcome)
  risk_exposed = exposed_poor_outcome / (exposed_poor_outcome + exposed_good_outcome)
  
  rr <- risk_exposed / risk_unexposed
  
  
  return_df = tibble(
    summary = list(summary_df_wide),
    p.value = p_value,
    risk_ratio = rr
  )
  

}
```

In 2024, what is the risk of having poor mental health outcomes based on
state legalization? Exposure: betting legalization Outcomes:
any_physical_health_not_good_days, any_mental_health_not_good_days,
has_depressive_disorder, has_binge_drink

``` r
phys_health_results = 
  brfss_data |>
  filter(year(date) == 2024, age_group_5yr %in% c("18-24", "25-29"), !is.na(any_physical_health_not_good_days)) |>
  select(sb_legal, any_physical_health_not_good_days) |> 
  rename(outcome = any_physical_health_not_good_days) |> 
  run_fishers_test()

ment_health_results = 
  brfss_data |>
  filter(year(date) == 2024, age_group_5yr %in% c("18-24", "25-29"), !is.na(any_mental_health_not_good_days)) |>
  select(sb_legal, any_mental_health_not_good_days) |> 
  rename(outcome = any_mental_health_not_good_days) |> 
  run_fishers_test()

depression_results = 
  brfss_data |>
  filter(year(date) == 2024, age_group_5yr %in% c("18-24", "25-29"), !is.na(has_depressive_disorder)) |>
  select(sb_legal, has_depressive_disorder) |> 
  rename(outcome = has_depressive_disorder) |> 
  run_fishers_test()

binge_drink_results = 
  brfss_data |>
  filter(year(date) == 2024, age_group_5yr %in% c("18-24", "25-29"), !is.na(has_binge_drink)) |>
  select(sb_legal, has_binge_drink) |> 
  rename(outcome = has_binge_drink) |> 
  run_fishers_test()
```

Among BRFSS participants in 2024 between the ages of 18 and 29, those
who resided in states with legalized sports betting had:

- 1.06x risk of having at least 1 day of poor physical health
- 1.05x risk of having at least 1 day of poor mental health
- 1.07x risk of having depression
- 1.15x risk of binge drinking

compared to those residing in states where sports betting is not legal.
