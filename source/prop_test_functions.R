run_prop_test_state = function(state_df){
  
  counts = 
    state_df |> 
    group_by(year = year(date), outcome_value) |>
    summarize(
      n = n(), 
      .groups = "drop"
    ) |> 
    filter(!is.na(outcome_value)) |> 
    pivot_wider(
      names_from = outcome_value,
      values_from = n
    ) |> 
    mutate(total_responses = `TRUE` + `FALSE`) 
  
  x_poor_outcome <- counts |> pull(`TRUE`)
  n_total_counts <- counts |> pull(total_responses)
  
  if (nrow(counts) < 2) {
    return(tibble(NA))
  }
  
  # alternative is less since we are testing that 2017 < 2024
  prop_test_result = prop.test(
    x = x_poor_outcome, 
    n = n_total_counts, 
    alternative = "less", 
    conf.level = 0.95
  ) 
  
  prop_test_result |> 
    broom::tidy()
  
}

run_prop_test_state_legalization = function(state_df){
  
  counts = 
    state_df |> 
    group_by(sb_legal, outcome_value) |>
    summarize(
      n = n(), 
      .groups = "drop"
    ) |> 
    filter(!is.na(outcome_value)) |> 
    pivot_wider(
      names_from = outcome_value,
      values_from = n
    ) |> 
    mutate(total_responses = `TRUE` + `FALSE`) 
  
  x_poor_outcome <- counts |> pull(`TRUE`)
  n_total_counts <- counts |> pull(total_responses)
  
  if (nrow(counts) < 2) {
    return(tibble(NA))
  }
  
  prop_test_result = prop.test(
    x = x_poor_outcome, 
    n = n_total_counts, 
    alternative = "less", 
    conf.level = 0.95
  ) 
  
  prop_test_result |> 
    broom::tidy()
  
}

plot_outcome_props = function(outcome, df){
  
  title_map = c(
    "any_physical_health_not_good_days" = "At Least 1 Not Good Physical Health Day",
    "any_mental_health_not_good_days" = "At Least 1 Not Good Mental Health Day",
    "has_depressive_disorder" = "Has Depressive Disorder",
    "has_binge_drink" = "Has Binge Drink"
  )
  
  plot_title = title_map[outcome]
  
  p = df |> 
    ggplot(aes(y = fct_reorder(abbr, estimate1))) + 
    geom_point(aes(x = estimate1), color = "blue", size = 2) +
    geom_point(aes(x = estimate2), color = "red", size = 2) +
    geom_segment(aes(x = estimate1, xend = estimate2, color = significant_increase)) + 
    scale_color_manual(
      values = c("TRUE" = "red", "FALSE" = "gray"),
      labels = c("Significant Increase (p < 0.05)", "No Significant Increase"),
      name = "Change Magnitude"
    ) +
    xlim(0, 0.65) + 
    labs(
      title = plot_title,
      y = "State"
    ) +
    theme_minimal() +
    theme(legend.position = "none") 
  
  p
}


factor_significant = function(df){
  df |>
    select(-data) |> 
    unnest(cols = c(test_result)) |> 
    select(state, outcome, estimate1, estimate2, p.value, conf.low, conf.high) |> 
    mutate(significant_increase = p.value < 0.05) |> 
    rename(fips = state) |> 
    left_join(state_fips, by = "fips") |> 
    drop_na(estimate1, estimate2) |>
    mutate(significant_increase = factor(significant_increase, levels = c(TRUE, FALSE)))
}


combine_plots = function(plotting_df){
  
  outcome_plots = 
    plotting_df |> 
    group_by(outcome) |> 
    nest() |> 
    mutate(
      plot = map2(outcome, data, plot_outcome_props)
    )
  
  plot_list = outcome_plots |> pull(plot)
  
  plots_combined = wrap_plots(plot_list, ncol = 2) +
    plot_layout(guides = "collect") & theme(legend.position = "bottom")
  
  plots_combined
  
}


