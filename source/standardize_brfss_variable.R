standardize_brfss_variable = function(raw_data){
  
  
  raw_data = 
    raw_data |> 
    # Question:  Adults who had some form of health insurance
    rename_with(.fn = ~ "hlthpl_standard", .cols = starts_with("hlthpl", ignore.case = FALSE)) |> 
    # Question:  Heavy drinkers (adult men having more than 14 drinks per week and adult women having more than 7 drinks per week)
    rename_with(.fn = ~ "rfdrh_standard", .cols = starts_with("rfdrhv", ignore.case = FALSE))
  
  raw_data
}

