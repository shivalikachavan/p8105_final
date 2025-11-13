standardize_brfss_variable = function(raw_data){
  
  column_names = colnames(raw_data)
  
  raw_data = 
    raw_data |> 
    rename_with(.fn = ~ "hlthpl_standard", .cols = starts_with("hlthpl", ignore.case = FALSE)) |> 
    rename_with(.fn = ~ "rfdrh_standard", .cols = starts_with("rfdrhv", ignore.case = FALSE)) |> 
    rename_with(.fn = ~ "rfbing_standard", .cols = starts_with("rfbing", ignore.case = FALSE)) |> 
    rename_with(.fn = ~ "income_standard", .cols = starts_with("income", ignore.case = FALSE)) |> 
    rename_with(.fn = ~ "medcost_standard", .cols = starts_with("medcost", ignore.case = FALSE))|> 
    rename_with(.fn = ~ "addepe_standard", .cols = starts_with("addepe", ignore.case = FALSE))
  
  
  
    
  if (!("sdlonely" %in% column_names)) {
    raw_data = raw_data |> mutate(sdlonely = NA_integer_)
  }
  
  if (!("lsatisfy" %in% column_names)) {
    raw_data = raw_data |> mutate(lsatisfy = NA_integer_)
  }
  
  if (!("emtsuprt" %in% column_names)) {
    raw_data = raw_data |> mutate(emtsuprt = NA_integer_)
  }
  
  if (!("sdhemply" %in% column_names)) {
    raw_data = raw_data |> mutate(sdhemply = NA_integer_)
  }
  
  if (!("sdhbills" %in% column_names)) {
    raw_data = raw_data |> mutate(sdhbills = NA_integer_)
  }
  
  if (!("sdhutils" %in% column_names)) {
    raw_data = raw_data |> mutate(sdhutils = NA_integer_)
  }
  
  if ("sex1" %in% column_names) {
    raw_data = raw_data |> mutate(sex = sex1)
  }
  
  if (!("urbstat" %in% column_names)) {
    raw_data = raw_data |> mutate(urbstat = NA_integer_)
  }

  raw_data
}



