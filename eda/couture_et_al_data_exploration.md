couture_et_al_data_exploration
================
Shivalika Chavan
2025-11-10

## Reading Couture Data (from STATA)

``` r
couture_data <- read_dta("../data/couture_et_al_2024_data_code/brfss17_20.dta") |> 
  select(-`_ageg5yr`, -`_age80`) |> 
  janitor::clean_names() |>
  mutate(
    date = as.Date(paste(iyear, imonth, iday, sep = "-")),
    general_health =
      case_match(
        genhlth,
        1 ~ "Excellent",
        2 ~ "Very Good",
        3 ~ "Good",
        4 ~ "Fair",
        5 ~ "Poor",
        .default = NA),
    general_health = as.factor(general_health),
    physical_health =
      case_match(
        physhlth,
        88 ~ 0,
        77 ~ NA,
        99 ~ NA,
        .default = physhlth),
    mental_health =
      case_match(
        menthlth,
        88 ~ 0,
        77 ~ NA,
        99 ~ NA,
        .default = menthlth),
    sex =
      case_match(
        sex,
        1 ~ "Male",
        2 ~ "Female",
        .default = NA),
    sex = as.factor(sex),
    marital_status =
      case_match(
        marital,
        1 ~ "Married",
        2 ~ "Divorced",
        3 ~ "Widowed",
        4 ~ "Separated",
        5 ~ "Never Married",
        6 ~ "Member of an unmarried couple (Partner)", 
        .default = NA),
    marital_status = as.factor(marital_status),
    education_status =
      case_match(
        educa,
        1 ~ "Less than High School", 
        2 ~ "Elementary",           
        3 ~ "Some High School",
        4 ~ "High School Graduate/GED",
        5 ~ "Some College/Tech School",
        6 ~ "College Graduate",
        .default = NA),
    education_status = as.factor(education_status),
    children =
      case_match(
        children,
        88 ~ 0,
        99 ~ NA,
        .default = children),
    age_group_5yr =
      case_match(
        ageg5yr,
        14 ~ NA,
        .default = ageg5yr),
    age_group_5yr = as.factor(age_group_5yr),
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
        .default = NA),
    employment_status = as.factor(employment_status),
    income =
      case_match(
        income2,
        9 ~ NA, 
        .default = income2),
    income = as.factor(income),
    race =
      case_match(
        imprace,
        1 ~ "White, Non-Hispanic",
        2 ~ "Black, Non-Hispanic",
        3 ~ "Asian, Non-Hispanic",
        4 ~ "American Indian, Alaska Native, Non-Hispanic",
        5 ~ "Hispanic",
        6 ~ "Other/Multiracial, Non-Hispanic",
        .default = NA),
    race = as.factor(race)
  )
```

**Coding Variables from BRFSS:**

- General Health: 1 - Excellent, 2 - Very Good, 3 - Good, 4 - Fair, 5 -
  Poor, 7 - Don’t Know, Not Sure, 9 - Refused
- Mental/Physical Health Days: 0-30 - Actual number of days poor health
  was experienced, 77 - Don’t Know, Not Sure, 88 - None (Recoded to 0),
  99 - Refused
- Sex: 1 - Male, 2 - Female, 7 - Don’t Know, Not Sure, 9 - Refused
- Marital: 1 - Married, 2 - Divorced, 3 - Widowed, 4 - Separated, 5 -
  Never Married, 6 - Member of an unmarried couple (Partner), 9 -
  Refused
- Education: 1 - Never attended school or only kindergarten, 2 - Grades
  1 through 8 (Elementary), 3 - Grades 9 through 11 (Some High School),
  4 - Grade 12 or GED (High School Graduate), 5 - College 1 year to 3
  years (Some College), 6 - College 4 years or more (College Graduate),
  9 - Don’t Know, Not Sure, Refused
- Children: Actual \# of children, 88 - None (Recoded to 0), 99 -
  Refused
- ageg5yr: age category by 5 years
- age80: actual age
- State: State FIPS
- Employment: 1 - Employed for wages, 2 - Self-employed, 3 - Out of work
  for less than 1 year, 4 - Out of work for 1 year or more, 5 -
  Homemaker, 6 - Student, 7 - Retired, 8 - Unable to work (Disabled),
  9 - Refused
- Income: 1 - Less than \$10,000, 2 - \$10,000 to less than \$15,000,
  3 - \$15,000 to less than \$20,000, 4 - \$20,000 to less than
  \$25,000, 5 - \$25,000 to less than \$35,000, 6 - \$35,000 to less
  than \$50,000, 7 - \$50,000 to less than \$75,000, 8 - \$75,000 or
  more, 9 - Don’t Know, Not Sure, Refused
- Race: 1 - White, Non-Hispanic, 2 - Black, Non-Hispanic, 3 - Asian,
  Non-Hispanic, 4 - American Indian, Alaska Native, Non-Hispanic, 5 -
  Hispanic, 6 - Other, Multiracial, Non-Hispanic, 9 - Not Sure, Refused

``` r
summary(couture_data) |> knitr::kable()
```

|     | imonth         | iday          | iyear        | genhlth       | physhlth      | menthlth     | sex           | marital       | educa         | children       | ageg5yr        | age80         | state         | employ1       | income2       | imprace       | date               | general_health   | physical_health | mental_health  | marital_status                                 | education_status                | age_group_5yr  | employment_status                | income         | race                                                |
|:----|:---------------|:--------------|:-------------|:--------------|:--------------|:-------------|:--------------|:--------------|:--------------|:---------------|:---------------|:--------------|:--------------|:--------------|:--------------|:--------------|:-------------------|:-----------------|:----------------|:---------------|:-----------------------------------------------|:--------------------------------|:---------------|:---------------------------------|:---------------|:----------------------------------------------------|
|     | Min. : 1.000   | Min. : 1.00   | Min. :2017   | Min. :1.000   | Min. : 1.00   | Min. : 1.0   | Female:936353 | Min. :1.000   | Min. :1.000   | Min. : 0.000   | Min. : 1.000   | Min. :18.00   | Min. : 1.00   | Min. :1.000   | Min. : 1.00   | Min. :1.000   | Min. :2017-01-02   | Excellent:293855 | Min. : 0.00     | Min. : 0.000   | Divorced :229702                               | College Graduate :645846        | 10 :184125     | Employed for wages :698001       | 8 :482303      | American Indian, Alaska Native, Non-Hispanic: 30682 |
|     | 1st Qu.: 4.000 | 1st Qu.: 8.00 | 1st Qu.:2017 | 1st Qu.:2.000 | 1st Qu.:20.00 | 1st Qu.:20.0 | Male :769928  | 1st Qu.:1.000 | 1st Qu.:4.000 | 1st Qu.: 0.000 | 1st Qu.: 5.000 | 1st Qu.:41.00 | 1st Qu.:18.00 | 1st Qu.:1.000 | 1st Qu.: 5.00 | 1st Qu.:1.000 | 1st Qu.:2017-12-19 | Fair :228939     | 1st Qu.: 0.00   | 1st Qu.: 0.000 | Married :876967                                | Elementary : 38912              | 9 :182575      | Retired :510269                  | 7 :224468      | Asian, Non-Hispanic : 40386                         |
|     | Median : 7.000 | Median :14.00 | Median :2018 | Median :2.000 | Median :88.00 | Median :88.0 | NA’s : 1397   | Median :1.000 | Median :5.000 | Median : 0.000 | Median : 8.000 | Median :58.00 | Median :28.00 | Median :3.000 | Median : 7.00 | Median :1.000 | Median :2018-12-16 | Good :533956     | Median : 0.00   | Median : 0.000 | Member of an unmarried couple (Partner): 59781 | High School Graduate/GED:460601 | 8 :162405      | Self-employed :151601            | 6 :193129      | Black, Non-Hispanic : 134767                        |
|     | Mean : 6.582   | Mean :14.83   | Mean :2018   | Mean :2.572   | Mean :61.36   | Mean :62.5   | NA            | Mean :2.353   | Mean :4.957   | Mean : 0.515   | Mean : 7.749   | Mean :54.95   | Mean :29.53   | Mean :3.902   | Mean :20.21   | Mean :1.692   | Mean :2018-12-28   | Poor : 84999     | Mean : 4.21     | Mean : 3.761   | Never Married :291412                          | Less than High School : 2474    | 11 :161836     | Unable to work (Disabled):120716 | 99 :159720     | Hispanic : 148487                                   |
|     | 3rd Qu.:10.000 | 3rd Qu.:22.00 | 3rd Qu.:2019 | 3rd Qu.:3.000 | 3rd Qu.:88.00 | 3rd Qu.:88.0 | NA            | 3rd Qu.:3.000 | 3rd Qu.:6.000 | 3rd Qu.: 1.000 | 3rd Qu.:11.000 | 3rd Qu.:69.00 | 3rd Qu.:42.00 | 3rd Qu.:7.000 | 3rd Qu.: 8.00 | 3rd Qu.:1.000 | 3rd Qu.:2019-12-19 | Very Good:561629 | 3rd Qu.: 3.00   | 3rd Qu.: 3.000 | Separated : 35520                              | Some College/Tech School:472613 | 13 :137917     | Homemaker : 83277                | 5 :143487      | Other/Multiracial, Non-Hispanic : 56162             |
|     | Max. :12.000   | Max. :31.00   | Max. :2021   | Max. :9.000   | Max. :99.00   | Max. :99.0   | NA            | Max. :9.000   | Max. :9.000   | Max. :82.000   | Max. :14.000   | Max. :80.00   | Max. :72.00   | Max. :9.000   | Max. :99.00   | Max. :6.000   | Max. :2021-03-18   | NA’s : 4300      | Max. :30.00     | Max. :30.000   | Widowed :200623                                | Some High School : 80182        | (Other):849302 | (Other) :121556                  | (Other):482607 | White, Non-Hispanic :1297194                        |
|     | NA             | NA            | NA           | NA’s :62      | NA’s :64      | NA’s :50     | NA            | NA’s :103     | NA’s :78      | NA’s :25287    | NA             | NA            | NA            | NA’s :7026    | NA’s :21964   | NA            | NA                 | NA               | NA’s :37395     | NA’s :30924    | NA’s : 13673                                   | NA’s : 7050                     | NA’s : 29518   | NA’s : 22258                     | NA’s : 21964   | NA                                                  |
