library(tidyverse)
library(rvest)
library(httr)

#Get sports betting data by month/state from legal sports report

#Choose local version (for debugging) or live
#url = "https://www.legalsportsreport.com/sports-betting-states/revenue/"
url = "./data/raw_data/2025_11_09_legal_sports_report.html"


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

string_replacements = c("," = "", "$" = "")

#First table is: US sports betting revenue by month
sb_rev_market_df = tables[[1]] |> 
  janitor::clean_names() |> 
  mutate(
    handle = as.numeric(str_replace_all(handle, c("\\$" = "", "," = ""))),
    revenue = as.numeric(str_replace_all(revenue, c("\\$" = "", "," = ""))),
    taxes = as.numeric(str_replace_all(taxes, c("\\$" = "", "," = ""))),
    hold = as.numeric(str_replace_all(hold, c("\\%" = ""))),
  )

#Second table is: US sports betting revenue by market
sb_rev_month_df = tables[[2]] |> 
  janitor::clean_names() |> 
  filter(month != "Total") |> 
  mutate(
    month = lubridate::myd(str_c(month, " 01")),
    handle = as.numeric(str_replace_all(handle, c("\\$" = "", "," = ""))),
    revenue = as.numeric(str_replace_all(revenue, c("\\$" = "", "," = ""))),
    taxes = as.numeric(str_replace_all(taxes, c("\\$" = "", "," = ""))),
    hold = as.numeric(str_replace_all(hold, c("\\%" = ""))),
  )


#Combine all state/month data into single table
all_state_data = list()
for (i in seq_along(pull(states, value))) {
  state_table = tables[[i+2]] |> 
    janitor::clean_names() |> 
    filter(month != "Total") |> 
    mutate(
      state = pull(states, value)[i],
      month = lubridate::myd(str_c(month, " 01")),
      handle = as.numeric(str_replace_all(handle, c("\\$" = "", "," = ""))),
      revenue = as.numeric(str_replace_all(revenue, c("\\$" = "", "," = ""))),
      taxes = as.numeric(str_replace_all(taxes, c("\\$" = "", "," = ""))),
      hold = as.numeric(str_replace_all(hold, c("\\%" = ""))),
    )
    
  all_state_data[[i]] = state_table
}

sb_rev_market_month_df = bind_rows(all_state_data)

#Save output
write_csv(sb_rev_market_df, file = "./data/legal_sports_report/sb_rev_by_state.csv")
write_csv(sb_rev_month_df, file = "./data/legal_sports_report/sb_rev_by_month.csv")
write_csv(sb_rev_market_month_df, file = "./data/legal_sports_report/sb_rev_by_state_month.csv")


#Get legalization dates
#https://www.americangaming.org/research/state-of-play-map/
#table from Hollenbeck et al. (2024)

state_fips_df = read_csv("./data/legal_sports_report/state_fips.csv")

state_legal_df = read_csv("./data/raw_data/state_legalization_dates.csv") |> 
  janitor::clean_names()

state_legal_df[nrow(state_legal_df) + 1,] = list("Kentucky", "Sep-23", "Sep-23", "Sep-23")
state_legal_df[nrow(state_legal_df) + 1,] = list("Maine", "Nov-23", "Nov-23", "Sep-24")
state_legal_df[nrow(state_legal_df) + 1,] = list("Vermont", "Jan-24", "Jan-24", NA)

state_legal_df = state_legal_df |> 
  mutate(
    first_start = lubridate::myd(str_c(first_start, "-01")),
    online = lubridate::myd(str_c(online, "-01")),
    offline = lubridate::myd(str_c(offline, "-01"))
  )

state_legal_df = left_join(state_legal_df, state_fips_df, by = "state")

#Save updated version
write_csv(state_legal_df, file = "./data/legal_sports_report/state_legalization_dates.csv")
