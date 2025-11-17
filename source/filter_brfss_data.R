filter_brfss_data = function(brfss_data_raw, data_year = 2024){
  
  column_names = colnames(brfss_data_raw)
  
  brfss_data_clean = 
    brfss_data_raw |> 
    select(qstver, dispcode, state, seqno, iyear, imonth, iday, 
           sex, marital, educag, employ1, children, income_standard, urbstat, imprace, ageg5yr, hlthpl_standard, 
           genhlth, rfhlth, physhlth, phys14d, menthlth, ment14d, poorhlth, medcost_standard, totinda, michd, addepe_standard, 
           decide, diffalon, lsatisfy, emtsuprt, sdlonely, sdhemply, sdhbills, sdhutils, rfbing_standard, rfdrh_standard
    ) |> 
    mutate(
      date = as.Date(paste(iyear, imonth, iday, sep = "-"))
    ) |> 
    filter(
      dispcode == 1100, 
      year(date) == data_year
      ) # only selecting completed interviews in the right year
  
  brfss_data_clean
}