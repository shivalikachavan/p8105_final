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