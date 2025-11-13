clean_brfss_data = function(brfss_data_raw, year = 2024){
  
  
  
  brfss_data_clean = 
    brfss_data_raw |> 
    select(qstver, dispcode, state, seqno, iyear, imonth, iday, 
           sexvar, marital, educag, employ1, children, incomg1, urbstat, imprace, ageg5yr, hlthpl_standard, 
           genhlth, rfhlth, physhlth, phys14d, menthlth, ment14d, poorhlth, medcost1, totinda, michd, addepev3, 
           decide, diffalon, lsatisfy, emtsuprt,  sdlonely, sdhemply, sdhbills, sdhutils, rfbing6, rfdrh_standard
    ) |> 
    filter(dispcode == 1100) |> # only selecting completed interviews
    mutate(
      
      id = paste0(state, seqno), # concatenating state and seqno to get a unique row id for each interview
      
      date = as.Date(paste(iyear, imonth, iday, sep = "-")),
      
      sex = case_match(sexvar, 1 ~ "Male", 2 ~ "Female", .default = NA) |> as.factor(),
      
      marital_status =
        case_match(
          marital,
          1 ~ "Married",
          2 ~ "Divorced",
          3 ~ "Widowed",
          4 ~ "Separated",
          5 ~ "Never Married",
          6 ~ "Member of an unmarried couple (Partner)", 
          .default = NA) |> as.factor(),
      
      education_status =
        case_match(
          educag,
          1 ~ "Did not graduate High School", 
          2 ~ "Graduated High School",           
          3 ~ "Attended College or Technical School",
          4 ~ "Graduated from College or Technical School",
          .default = NA) |> as.factor(),
      
      employment_status =
        case_match(
          employ1,
          1 ~ "Employed for wages",
          2 ~ "Self-employed",
          3 ~ "Out of work (<1 year)",
          4 ~ "Out of work (>1 year)",
          5 ~ "Homemaker",
          6 ~ "Student",
          7 ~ "Retired",
          8 ~ "Unable to work (Disabled)",
          .default = NA) |> as.factor(),
      
      children_in_household = case_match(children, 88 ~ 0, 99 ~ NA, .default = children),
      
      income_level =
        case_match(
          incomg1,
          1 ~ "Less than $15,000",
          2 ~ "$15,000 to < $25,000",
          3 ~ "$25,000 to < $50,000",
          4 ~ "$50,000 to < $75,000",
          5 ~ "$50,000 to < $100,000",
          6 ~ "$100,000 to < $200,000",
          7 ~ "$200,000 or more",
          .default = NA 
        ) |> as.factor(),
      
      insurance_coverage =
        case_match(
          hlthpl_standard,
          1 ~ "Have some form of insurance",
          2 ~ "Do not have some form of health insurance",
          9 ~ NA,
          .default = NA) |> as.factor(),
      
      race =
        case_match(
          imprace,
          1 ~ "White, Non-Hispanic",
          2 ~ "Black, Non-Hispanic",
          3 ~ "Asian, Non-Hispanic",
          4 ~ "American Indian, Alaska Native, Non-Hispanic",
          5 ~ "Hispanic",
          6 ~ "Other/Multiracial, Non-Hispanic",
          .default = NA) |> as.factor(),
      
      age_group_5yr =
        case_match(
          ageg5yr,
          1 ~ "18-24",
          2 ~ "25-29",
          3 ~ "30-34",
          4 ~ "35-39",
          5 ~ "40-44",
          6 ~ "45-49",
          7 ~ "50-54",
          8 ~ "55-59",
          9 ~ "60-64",
          10 ~ "65-69",
          11 ~ "70-74",
          12 ~ "75-79",
          13 ~ "80 or older",
          .default = NA) |> as.factor(),
      
      urban_status =
        case_match(
          urbstat,
          1 ~ "Urban counties",
          2 ~ "Rural counties",
          .default = NA) |> as.factor(),
      
      general_health =
        case_match(
          genhlth,
          1 ~ "Excellent",
          2 ~ "Very Good",
          3 ~ "Good",
          4 ~ "Fair",
          5 ~ "Poor",
          .default = NA) |> as.factor(),
      general_health_refactored =
        case_match(
          rfhlth,
          1 ~ "Good or Better Health",
          2 ~ "Fair or Poor Health",
          .default = NA) |> as.factor(),
      
      michd = case_match(michd, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      
      physical_health = case_match(physhlth, 88 ~ 0, 77 ~ NA, 99 ~ NA, .default = physhlth),
      physical_health_not_good_days = case_match(phys14d, 1 ~ "0", 2 ~ "1-13", 3 ~ "14+", 9 ~ NA, .default = NA) |> as.factor(),
      leisure_physical_activity_last_30_days = case_match(totinda, 1 ~ "Yes", 2 ~ "No", 9 ~ NA, .default = NA) |> as.factor(),
      
      mental_health = case_match(menthlth, 88 ~ 0, 77 ~ NA, 99 ~ NA, .default = menthlth),
      mental_health_not_good_days = case_match(ment14d, 1 ~ "0", 2 ~ "1-13", 3 ~ "14+", 9 ~ NA, .default = NA) |> as.factor(),
      
      depressive_disorder = case_match(addepev3, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      
      poor_health = case_match(poorhlth, 88 ~ 0, 77 ~ NA, 99 ~ NA, .default = poorhlth),
      
      binge_drink = case_match(rfbing6, 1 ~ "No", 2 ~ "Yes", 9 ~ NA, .default = NA) |> as.factor(),
      heavy_drink = case_match(rfdrh_standard, 1 ~ "No", 2 ~ "Yes", 9 ~ NA, .default = NA) |> as.factor(),
      
      medical_cost_barrier = case_match(medcost1, 1 ~ "Yes", 2 ~ "No", 7 ~ NA, 9 ~ NA, .default = NA) |> as.factor(),
      
      difficulty_self_care = case_match(diffalon, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      
      life_satisfaction =
        case_match(
          lsatisfy,
          1 ~ "Very satisfied",
          2 ~ "Satisfied",
          3 ~ "Dissatisfied",
          4 ~ "Very dissatisfied",
          .default = NA) |> as.factor(),
      
      emotional_support =
        case_match(
          emtsuprt,
          1 ~ "Always",
          2 ~ "Usually",
          3 ~ "Sometimes",
          4 ~ "Rarely",
          5 ~ "Never",
          .default = NA) |> as.factor(),
      
      loneliness =
        case_match(
          sdlonely,
          1 ~ "Always",
          2 ~ "Usually",
          3 ~ "Sometimes",
          4 ~ "Rarely",
          5 ~ "Never",
          .default = NA) |> as.factor(),
      
      lost_reduced_employment = case_match(sdhemply, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      
      financial_strain_bills = case_match(sdhbills, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      financial_strain_utilities = case_match(sdhbills, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor()
    ) |> 
    filter(year(date) == data_year) |>
    # dropping any overwritten variables
    select( 
      #Survey Identifiers
      id, date, qstver, 
      # Demographic/Predictor Variables
      state, urban_status, age_group_5yr, sex, marital_status, education_status, employment_status, children_in_household, income_level, race, insurance_coverage, 
      # Health/Financial Outcomes Variables
      medical_cost_barrier, general_health, general_health_refactored, michd, 
      physical_health, physical_health_not_good_days, leisure_physical_activity_last_30_days,
      mental_health, mental_health_not_good_days, poor_health, depressive_disorder, difficulty_self_care,
      life_satisfaction, emotional_support, loneliness,
      lost_reduced_employment, financial_strain_bills, financial_strain_utilities
    ) 
  
  brfss_data_clean
}