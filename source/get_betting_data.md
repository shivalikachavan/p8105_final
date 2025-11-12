Get Betting Data
================
2025-11-09

## Get Sports Betting Handle by Month and State from Legal Sports Report

``` r
#Choose local version (for debugging) or live
#url = "https://www.legalsportsreport.com/sports-betting-states/revenue/"
url = "./raw_data/2025_11_09_legal_sports_report.html"

#Pull all tables
lsr_html = read_html(url)

tables = lsr_html |>
  html_table()

table_headers = as_tibble(lsr_html |> 
  html_elements(css = "h2") |> 
  html_text(trim = TRUE)
)

#Get all states names
states = as.tibble(lsr_html |> 
  html_elements('.question__button') |> 
  html_text(trim = TRUE)
) |> #NO Data for Florida, Washington
  filter(! value %in% c("Florida","New Mexico", "North Dakota", "Washington", "Wisconsin"))
```

    ## Warning: `as.tibble()` was deprecated in tibble 2.0.0.
    ## ℹ Please use `as_tibble()` instead.
    ## ℹ The signature and semantics have changed, see `?as_tibble`.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
#First table is: US sports betting revenue by month
sb_rev_market_df = tables[[1]]

#Second table is: US sports betting revenue by market
sb_rev_month_df = tables[[2]]

#Combine all state/month data into single table
all_state_data = list()
for (i in seq_along(pull(states, value))) {
  state_table = tables[[i+2]] |> 
    mutate(State = pull(states, value)[i])
  all_state_data[[i]] = state_table
}

sb_rev_market_month_df = bind_rows(all_state_data)

#Save output
write_csv(sb_rev_market_df, file = "./clean_data/lsr_sb_rev_market.csv")
write_csv(sb_rev_month_df, file = "./clean_data/lsr_sb_rev_month.csv")
write_csv(sb_rev_market_month_df, file = "./clean_data/lsr_sb_rev_market_month.csv")
```

## Get State Legalization Dates

``` r
#https://www.americangaming.org/research/state-of-play-map/
#table from Hollenbeck et al. (2024)

state_legal_df = read_csv("./raw_data/state_legalization_dates.csv")
```

    ## Rows: 33 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): State, First Start, Online, Offline
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
state_legal_df[nrow(state_legal_df) + 1,] = list("Kentucky", "Sep-23", "Sep-23", "Sep-23")
state_legal_df[nrow(state_legal_df) + 1,] = list("Maine", "Nov-23", "Nov-23", "Sep-24")
state_legal_df[nrow(state_legal_df) + 1,] = list("Vermont", "Jan-24", "Jan-24", NA)

#Save updated version
write_csv(sb_rev_market_df, file = "./clean_data/state_legalization_dates.csv")
```
