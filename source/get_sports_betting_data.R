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

#First table is: US sports betting revenue by market 
sb_rev_market_df = tables[[1]] |> 
  janitor::clean_names() |> 
  mutate(
    handle = as.numeric(str_replace_all(handle, c("\\$" = "", "," = ""))),
    revenue = as.numeric(str_replace_all(revenue, c("\\$" = "", "," = ""))),
    taxes = as.numeric(str_replace_all(taxes, c("\\$" = "", "," = ""))),
    hold = as.numeric(str_replace_all(hold, c("\\%" = ""))),
  )

#Second table is: US sports betting revenue by month
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
state_legal_df[nrow(state_legal_df) + 1,] = list("Nebraska", "Jun-23", NA, "Jun-23")

state_legal_df = state_legal_df |> 
  mutate(
    first_start = lubridate::myd(str_c(first_start, "-01")),
    online = lubridate::myd(str_c(online, "-01")),
    offline = lubridate::myd(str_c(offline, "-01"))
  )

state_legal_df = left_join(state_legal_df, state_fips_df, by = "state")

#Save updated version
write_csv(state_legal_df, file = "./data/legal_sports_report/state_legalization_dates.csv")


#Get sports betting data by channel (online or in person)

#url = "https://sportshandle.com/sports-betting-revenue/"
url = "./data/raw_data/2025_11_19_sports_handle.html"

sh_html = read_html(url)

tables = sh_html |>
  html_table()

table_headers = as_tibble(sh_html |> 
                            html_elements(css = "h2") |> 
                            html_text(trim = TRUE)
)

#First table is: Monthly U.S. Totals: Handle, Revenue, Hold, and Taxes
sh_rev_month_df = tables[[1]] |> 
  janitor::clean_names() |> 
  filter(
    month != "",
    !str_detect(month, "TOTAL")
  ) |> 
  mutate(
    month = lubridate::myd(str_c(month, "-01")),
    handle = as.numeric(str_replace_all(handle, c("\\$" = "", "," = ""))),
    gross_revenue = as.numeric(str_replace_all(gross_revenue, c("\\$" = "", "," = ""))),
    state_taxes = as.numeric(str_replace_all(state_taxes, c("\\$" = "", "," = "")))
  )
  
#Second table is: Summary Table: Total Sports Betting Figures by State
sh_rev_market_df = tables[[2]] |> 
  janitor::clean_names() |> 
  filter(
    state != "",
    !str_detect(state, "Totals")
  ) |> 
  mutate(
    total_handle = as.numeric(str_replace_all(total_handle, c("\\$" = "", "," = ""))),
    gross_sportsbook_revenue = as.numeric(str_replace_all(gross_sportsbook_revenue, c("\\$" = "", "," = ""))),
    state_taxes_collected = as.numeric(str_replace_all(state_taxes_collected, c("\\$" = "", "," = "")))
  )

all_state_data = list()
all_columns = list()


dim(table_headers)[1]
for (i in 3:dim(table_headers)[1]) {
  state = str_replace_all(table_headers[[i, 1]], c(" Sports Betting Totals"="", " sports betting totals"="", " Sport Betting Totals"=""))

  state_table = tables[[i]] |> 
    janitor::clean_names() |> 
    rename(
      month = any_of(c("date")),
      sports_books = any_of(c("casinos_online_operators", "casinos", "number_of_casinos_number_online",
                       "number_of_venues_mobile_operators", "casinos_online",
                       "number_of_operators", "number_of_casinos_online", "number_of_casinos_online_operators",
                       "operators", "sportsbooks_online", "number_of_sportsbooks", "number_of_sportsbooks_number_online",
                       "number_of_casino_sportsbooks_number_online")),
      handle = any_of(c("total_handle", "overall_handle")),
      #mobile_handle_percentage = any_of(),
      mobile_handle_dollars = any_of(c("mobile_handle")),
      gross_revenue = any_of(c("gross_revenue", "revenue", "gross_gaming_revenue",
                               "sportsbook_revenue", "gross_sportsbook_revenue", "sportsbook_revenues",
                               "sportsbook_gross_revenue")),
      gross_win_rate = any_of(c("gross_revenue_win_rate", "win_rate", "ggr_win_rate")),
      adjusted_revenue = any_of(c("adjusted_revenue", "adjusted_gross_revenue", "adjusted_gaming_revenue",
                                  "adjusted_sportsbook_revenue", "sportsbook_adjusted_gross_revenue")),
      adjusted_win_rate = any_of(c("adjusted_revenue_win_rate", "agr_win_rate")),
      taxes = any_of(c("state_taxes", "district_taxes", "state_taxes_collected")),
      hold = any_of(c("hold_percentage"))
    ) |> 
    mutate(
      state = state,
      month = lubridate::myd(str_c(month, "-01")),
      mobile_handle_dollars = tryCatch(as.numeric(str_replace_all(mobile_handle_dollars, c("\\$" = "", "," = ""))), error = function(e) return(NA)),
      sports_books = tryCatch(as.character(sports_books), error = function(e) return(NA)),
      handle = tryCatch(as.numeric(str_replace_all(handle, c("\\$" = "", "," = ""))), error = function(e) return(NA)),
      mobile_handle_dollars = tryCatch(as.numeric(str_replace_all(mobile_handle_dollars, c("\\$" = "", "," = ""))), error = function(e) return(NA)),
      gross_revenue = tryCatch(as.numeric(str_replace_all(gross_revenue, c("\\$" = "", "," = ""))), error = function(e) return(NA)),
      gross_win_rate = tryCatch(as.numeric(str_replace_all(gross_win_rate, c("\\%" = ""))), error = function(e) return(NA)),
      adjusted_revenue = tryCatch(as.numeric(str_replace_all(adjusted_revenue, c("\\$" = "", "," = ""))), error = function(e) return(NA)),
      adjusted_win_rate = tryCatch(as.numeric(str_replace_all(adjusted_win_rate, c("\\%" = ""))), error = function(e) return(NA)),
      mobile_handle_percentage = tryCatch(as.numeric(str_replace_all(adjusted_win_rate, c("\\%" = ""))), error = function(e) return(NA)),
      taxes = tryCatch(as.numeric(str_replace_all(taxes, c("\\$" = "", "," = ""))), error = function(e) return(NA)),
      hold = tryCatch(as.numeric(str_replace_all(hold, c("\\%" = ""))), error = function(e) return(NA)),
    )
  
  #filter(month != "Total") |> 
  all_state_data[[i-2]] = state_table
  for (item in colnames(state_table)){
    if (!(item %in% all_columns)) {
          all_columns <- append(all_columns, item)
        }
      }
}

sh_rev_market_month_df = bind_rows(all_state_data) |> 
  filter(!is.na(month)) |> 
  select(state, month, handle, sports_books, gross_revenue, adjusted_revenue, gross_win_rate, adjusted_win_rate, mobile_handle_percentage, mobile_handle_dollars,
         hold, taxes, number_of_regions)
  
    
#Save output
write_csv(sh_rev_market_df, file = "./data/legal_sports_report/sh_rev_by_state.csv")
write_csv(sh_rev_month_df, file = "./data/legal_sports_report/sh_rev_by_month.csv")
write_csv(sh_rev_market_month_df, file = "./data/legal_sports_report/sh_rev_by_state_month.csv")


