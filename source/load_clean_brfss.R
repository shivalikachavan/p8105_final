load_clean_brfss = function(filepath){
  
  #get date when sports betting became legal by state
  sb_legal_dates = 
    read_csv(here::here("data", "legal_sports_report","state_legalization_dates.csv")) |> 
    select(first_start, abbr, fips)
  
  cleaned_df = 
    read_csv(filepath) |> 
    mutate(
      
      id = paste0(state, seqno), # concatenating state and seqno to get a unique row id for each interview
      
      sex = case_match(sex, 1 ~ "Male", 2 ~ "Female", .default = NA) |> as.factor(),
      
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
          income_standard,
          1 ~ "Less than $10,000",
          2 ~ "$10,000 to < $15,000",
          3 ~ "$15,000 to < $20,000",
          4 ~ "$20,000 to < $25,000",
          5 ~ "$25,000 to < $35,000",
          6 ~ "$35,000 to < $50,000",
          7 ~ "$50,000 to < $75,000",
          c(8, 9, 10, 11) ~ "$75,000 or more",
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
      
      depressive_disorder = case_match(addepe_standard, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      
      poor_health = case_match(poorhlth, 88 ~ 0, 77 ~ NA, 99 ~ NA, .default = poorhlth),
      
      binge_drink = case_match(rfbing_standard, 1 ~ "No", 2 ~ "Yes", 9 ~ NA, .default = NA) |> as.factor(),
      heavy_drink = case_match(rfdrh_standard, 1 ~ "No", 2 ~ "Yes", 9 ~ NA, .default = NA) |> as.factor(),
      
      medical_cost_barrier = case_match(medcost_standard, 1 ~ "Yes", 2 ~ "No", 7 ~ NA, 9 ~ NA, .default = NA) |> as.factor(),
      
      difficulty_self_care = case_match(diffalon, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
    
      life_satisfaction =
        case_match(
          lsatisfy,
          1 ~ "Very satisfied",
          2 ~ "Satisfied",
          3 ~ "Dissatisfied",
          4 ~ "Very dissatisfied",
          .default = NA_character_
        ) |> as.factor(),
      
      emotional_support = 
        case_match(
          emtsuprt,
          1 ~ "Always",
          2 ~ "Usually",
          3 ~ "Sometimes",
          4 ~ "Rarely",
          5 ~ "Never",
          .default = NA_character_
        ) |> as.factor(),
      
      loneliness =
        case_match(
          sdlonely,
          1 ~ "Always",
          2 ~ "Usually",
          3 ~ "Sometimes",
          4 ~ "Rarely",
          5 ~ "Never",
          .default = NA_character_
        ) |> as.factor(),
      
      lost_reduced_employment = case_match(sdhemply, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      
      financial_strain_bills = case_match(sdhbills, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      financial_strain_utilities = case_match(sdhbills, 1 ~ "Yes", 2 ~ "No", .default = NA) |> as.factor(),
      
      # Adding binary versions of outcomes for prop tests and regression
      any_physical_health_not_good_days = 
        case_match(
          physical_health_not_good_days,
          "0" ~ FALSE,
          "1-13" ~ TRUE,
          "14+" ~ TRUE
        ),
      any_mental_health_not_good_days = 
        case_match(
          mental_health_not_good_days,
          "0" ~ FALSE,
          "1-13" ~ TRUE,
          "14+" ~ TRUE
        ),
      has_depressive_disorder = 
        case_match(
          depressive_disorder,
          "No" ~ FALSE, 
          "Yes" ~ TRUE
        ),
      has_binge_drink = 
        case_match(
          binge_drink,
          "No" ~ FALSE, 
          "Yes" ~ TRUE
        )
    ) |>
    
    #add in whether sports betting was legal at the time of the interview
    #NOTE: legalization dates are set to the first of the month so we will check that the interview was the following month (not day) by temporarily creating a new column month_of_interview
    merge(sb_legal_dates, by.x = "state", by.y = "fips", all = TRUE) |> 
    mutate(
      month_of_interview = floor_date(date, unit = "month"),
      sb_legal = (as.numeric(month_of_interview > first_start))
    ) |> 
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
        binge_drink, heavy_drink,
        lost_reduced_employment, financial_strain_bills, financial_strain_utilities,
        any_physical_health_not_good_days, any_mental_health_not_good_days, 
        has_depressive_disorder, has_binge_drink,
        #Sports Betting Legal
        sb_legal
      ) 
  
  
  cleaned_df
}
