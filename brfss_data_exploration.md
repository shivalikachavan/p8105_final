BRFSS Data Exploration
================
Shivalika Chavan
2025-11-12

This document takes an initial look at the BRFSS data so we can
understand what variables exist in the data set and which
predictors/outcome/identifiers we want to use in our project.

## Reading BRFSS Raw Data (from XTP)

``` r
brfss_2024_data_raw <- read_xpt("./data/raw_data/brfss/LLCP2024.XPT") |> 
  janitor::clean_names()
colnames(brfss_2024_data_raw)
```

    ##   [1] "state"    "fmonth"   "idate"    "imonth"   "iday"     "iyear"   
    ##   [7] "dispcode" "seqno"    "psu"      "ctelenm1" "pvtresd1" "colghous"
    ##  [13] "statere1" "celphon1" "ladult1"  "numadult" "respslc1" "landsex3"
    ##  [19] "safetime" "ctelnum1" "cellfon5" "cadult1"  "cellsex3" "pvtresd3"
    ##  [25] "cclghous" "cstate1"  "landline" "hhadult"  "sexvar"   "genhlth" 
    ##  [31] "physhlth" "menthlth" "poorhlth" "primins2" "persdoc3" "medcost1"
    ##  [37] "checkup1" "exerany2" "lastden4" "rmvteth4" "cvdinfr4" "cvdcrhd4"
    ##  [43] "cvdstrk3" "asthma3"  "asthnow"  "chcscnc1" "chcocnc1" "chccopd3"
    ##  [49] "addepev3" "chckdny2" "havarth4" "diabete4" "diabage4" "marital" 
    ##  [55] "educa"    "renthom1" "numhhol4" "numphon4" "cpdemo1c" "veteran3"
    ##  [61] "employ1"  "children" "income3"  "pregnant" "weight2"  "height3" 
    ##  [67] "deaf"     "blind"    "decide"   "diffwalk" "diffdres" "diffalon"
    ##  [73] "hadmam"   "howlong"  "cervscrn" "crvclcnc" "crvclpap" "crvclhpv"
    ##  [79] "hadhyst2" "hadsigm4" "colnsigm" "colntes1" "sigmtes1" "lastsig4"
    ##  [85] "colncncr" "vircolo1" "vclntes2" "smalstol" "stoltest" "stooldn2"
    ##  [91] "bldstfit" "sdnates1" "smoke100" "smokday2" "usenow3"  "ecignow3"
    ##  [97] "lcsfirst" "lcsnumcg" "lcsctsc1" "lcsscncr" "lcsctwhn" "alcday4" 
    ## [103] "avedrnk4" "drnk3ge5" "maxdrnks" "flushot7" "flshtmy3" "imfvpla5"
    ## [109] "pneuvac4" "hivtst7"  "hivtstd3" "hivrisk5" "pdiabts1" "prediab2"
    ## [115] "diabtype" "insulin1" "chkhemo3" "eyeexam1" "diabeye1" "diabedu1"
    ## [121] "feetsore" "arthexer" "shingle2" "hpvadvc4" "hpvadsh1" "tetanus1"
    ## [127] "cncrdiff" "cncrage"  "cncrtyp2" "csrvtrt3" "csrvdoc1" "csrvsum" 
    ## [133] "csrvrtrn" "csrvinst" "csrvinsr" "csrvdein" "csrvclin" "csrvpain"
    ## [139] "csrvctl2" "psatest1" "psatime1" "pcpsars2" "psasugs1" "pcstalk2"
    ## [145] "cimemlo1" "cdworry"  "cddiscu1" "cdhous1"  "cdsocia1" "caregiv1"
    ## [151] "crgvrel5" "crgvprb4" "crgvalzd" "crgvnurs" "crgvper2" "crgvhou2"
    ## [157] "crgvhrs2" "crgvlng2" "acedeprs" "acedrink" "acedrugs" "aceprisn"
    ## [163] "acedivrc" "acepunch" "acehurt1" "aceswear" "acetouch" "acetthem"
    ## [169] "acehvsex" "aceadsaf" "aceadned" "lsatisfy" "emtsuprt" "sdlonely"
    ## [175] "sdhemply" "foodstmp" "sdhfood1" "sdhbills" "sdhutils" "sdhtrnsp"
    ## [181] "howsafe1" "marijan1" "marjsmok" "marjeat"  "marjvape" "marjdab" 
    ## [187] "marjothr" "usemrjn4" "lastsmk2" "stopsmk2" "mentcigs" "mentecig"
    ## [193] "heattbco" "ssbsugr2" "ssbfrut3" "firearm5" "gunload"  "loadulk2"
    ## [199] "rcsgend1" "rcsxbrth" "rcsrltn2" "casthdx2" "casthno2" "somale"  
    ## [205] "sofemale" "hadsex"   "pfpprvn4" "typcntr9" "nobcuse8" "qstver"  
    ## [211] "qstlang"  "hpvdsht"  "icfqstvr" "metstat"  "urbstat"  "mscode"  
    ## [217] "ststr"    "strwt"    "rawrake"  "wt2rake"  "imprace"  "chispnc" 
    ## [223] "crace1"   "cageg"    "cllcpwt"  "dualuse"  "dualcor"  "llcpwt2" 
    ## [229] "llcpwt"   "rfhlth"   "phys14d"  "ment14d"  "hlthpl2"  "hcvu654" 
    ## [235] "totinda"  "exteth3"  "alteth3"  "denvst3"  "michd"    "ltasth1" 
    ## [241] "casthm1"  "asthms1"  "drdxar2"  "mrace1"   "hispanc"  "race"    
    ## [247] "raceg21"  "racegr3"  "raceprv"  "sex"      "ageg5yr"  "age65yr" 
    ## [253] "age80"    "age_g"    "htin4"    "htm4"     "wtkg3"    "bmi5"    
    ## [259] "bmi5cat"  "rfbmi5"   "chldcnt"  "educag"   "incomg1"  "rfmam23" 
    ## [265] "mam402y"  "crvscrn"  "rfpap37"  "hpv5yr1"  "paphpv1"  "hadcoln" 
    ## [271] "clnscp2"  "hadsigm"  "sgmscp2"  "sgms102"  "rfblds6"  "stoldn2" 
    ## [277] "vircol2"  "sbonti2"  "crcrec3"  "smoker3"  "rfsmok3"  "cureci3" 
    ## [283] "lcslast"  "lcsnumc"  "lcsage"   "lcsysmk"  "packday"  "packyrs" 
    ## [289] "lcsyqts"  "lcssmkg"  "lcselig"  "lcsctsn"  "lcspstf"  "drnkany6"
    ## [295] "drocdy4"  "rfbing6"  "drnkwk3"  "rfdrhv9"  "flshot7"  "pneumo3" 
    ## [301] "aidtst4"

Lots of columns (301) to be exact. We definitely don’t need all of them.
Anticipating some variable name changes between 2017 and 2024 so will
probably need to search for those to make sure the right ones are being
selected when the below code turns into a function.

Coding the ones we want for a single data set from 2024.

``` r
data_year = 2024

brfss_2024_data_clean = 
  brfss_2024_data_raw |> 
  select(qstver, dispcode, state, seqno, iyear, imonth, iday, 
         sexvar, marital, educag, employ1, children, incomg1, urbstat, imprace, ageg5yr, hlthpl2, 
         genhlth, rfhlth, physhlth, phys14d, menthlth, ment14d, poorhlth, medcost1, totinda, michd, addepev3, decide, diffalon, lsatisfy, emtsuprt,  sdlonely, sdhemply, sdhbills, sdhutils, rfbing6, rfdrhv9
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
        hlthpl2,
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
    heavy_drink = case_match(rfdrhv9, 1 ~ "No", 2 ~ "Yes", 9 ~ NA, .default = NA) |> as.factor(),
    
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
  
colnames(brfss_2024_data_clean)
```

    ##  [1] "id"                                    
    ##  [2] "date"                                  
    ##  [3] "qstver"                                
    ##  [4] "state"                                 
    ##  [5] "urban_status"                          
    ##  [6] "age_group_5yr"                         
    ##  [7] "sex"                                   
    ##  [8] "marital_status"                        
    ##  [9] "education_status"                      
    ## [10] "employment_status"                     
    ## [11] "children_in_household"                 
    ## [12] "income_level"                          
    ## [13] "race"                                  
    ## [14] "insurance_coverage"                    
    ## [15] "medical_cost_barrier"                  
    ## [16] "general_health"                        
    ## [17] "general_health_refactored"             
    ## [18] "michd"                                 
    ## [19] "physical_health"                       
    ## [20] "physical_health_not_good_days"         
    ## [21] "leisure_physical_activity_last_30_days"
    ## [22] "mental_health"                         
    ## [23] "mental_health_not_good_days"           
    ## [24] "poor_health"                           
    ## [25] "depressive_disorder"                   
    ## [26] "difficulty_self_care"                  
    ## [27] "life_satisfaction"                     
    ## [28] "emotional_support"                     
    ## [29] "loneliness"                            
    ## [30] "lost_reduced_employment"               
    ## [31] "financial_strain_bills"                
    ## [32] "financial_strain_utilities"

Now I will write functions in `source` folder to deal with the remaining
data sets from 2017-2023 and any challenges that come with that. This is
done in `brfss_data_cleaning.Rmd`
