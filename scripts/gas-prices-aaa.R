library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(DatawRappr)
library(httr2)
library(jsonlite)
library(purrr)
library(tibble)

# Load environment variables
tryCatch(load_dot_env(), error = function(e) {}) 
dw_api_key <- Sys.getenv("DW_API_KEY")

# Authenticate with Datawrapper
datawrapper_auth(api_key = dw_api_key)


prices_updated_date <- Sys.Date()
prices_updated_date_pretty <- format(prices_updated_date, "%b %d, %Y")

#national prices
prices_resp <- request("https://gasprices.aaa.com/wp-admin/admin-ajax.php") |>
  req_headers(
    `X-Requested-With` = "XMLHttpRequest",
    Origin = "https://gasprices.aaa.com",
    Referer = "https://gasprices.aaa.com/",
    `User-Agent` = "Mozilla/5.0"
  ) |>
  req_body_form(
    action = "states_cost_data",
    `data[locL]` = "US",
    `data[locR]` = "US"
  ) |>
  req_perform()

prices_txt <- resp_body_string(prices_resp)
prices_dat <- fromJSON(prices_txt, simplifyVector = TRUE)
prices_data <- prices_dat$data
prices <- as.data.frame(prices_data) %>% 
  distinct() %>% 
  mutate(period = c(
    "Current Avg.",
    "Yesterday Avg.",
    "Week Ago Avg.",
    "Month Ago Avg.",
    "Year Ago Avg."
  )) %>% 
  relocate(period)

national_prices <- prices %>%
  mutate(Date = prices_updated_date) %>%
  rename(Period = period,
         Regular = unleaded,
         `Mid-Grade` = midgrade,
         Premium = premium,
         Diesel = diesel) %>% 
  filter(Period == "Current Avg.") %>% 
  select(Date, Regular, `Mid-Grade`, Premium, Diesel) %>% 
  mutate(Regular = as.numeric(Regular),
         `Mid-Grade` = as.numeric(`Mid-Grade`),
         Premium = as.numeric(Premium),
         Diesel = as.numeric(Diesel)
         )

#add today's prices to historical data 
aaa_historical <- read.csv("data/aaa_historical_gas_prices.csv") %>% 
  janitor::clean_names() %>%
  mutate(date = as.Date(date)) %>%
  distinct(date, .keep_all = TRUE) %>%
  filter(date != prices_updated_date) %>%
  rename(Date = date,
         Regular = regular,
         `Mid-Grade` = mid_grade,
         Premium = premium,
         Diesel = diesel,
         E85 = e85) %>% 
  bind_rows(national_prices)

write.csv(aaa_historical, "data/aaa_historical_gas_prices.csv", row.names = FALSE)

historical_note <- paste0("As of ", prices_updated_date_pretty, ".")

  
national_prices_regular <- prices %>%
  select(period, unleaded) %>%
  rename(price = unleaded) %>%
  mutate(price = as.numeric(str_replace(price, "\\$", ""))) %>% 
  mutate(period = str_replace(period, "Current Avg.", "Today"),
         period = str_replace(period, "Yesterday Avg.", "Yesterday"),
         period = str_replace(period, "Week Ago Avg.", "Last week"),
         period = str_replace(period, "Month Ago Avg.", "Last month"),
         period = str_replace(period, "Year Ago Avg.", "Last year")) %>% 
  mutate(index = case_when(
    period == "Today" ~ 1,
    period == "Yesterday" ~ 2,
    period == "Last week" ~ 3,
    period == "Last month" ~ 4,
    period == "Last year" ~ 5
  )) %>% 
  arrange(desc(index)) %>% 
  select(-index)

today_price <- national_prices_regular %>% filter(period == "Today") %>% pull(price)
yesterday_price <- national_prices_regular %>% filter(period == "Yesterday") %>% pull(price)
last_month_price <- national_prices_regular %>% filter(period == "Last month") %>% pull(price)
last_year_price <- national_prices_regular %>% filter(period == "Last year") %>% pull(price)

today_vs_yesterday <- round(today_price - yesterday_price, 2)
today_vs_last_month <- round(today_price - last_month_price, 2)
today_vs_last_year <- round(today_price - last_year_price, 2)


description <- paste0("On ", prices_updated_date_pretty, ", the average cost of gas nationwide was <b>$", round(today_price, 2), " per gallon</b>. That's <b>$",round(today_vs_yesterday, 2), " ", ifelse(today_vs_yesterday > 0, "higher", "lower"), "</b> than the day before, <b>$",round(today_vs_last_month, 2), " ", ifelse(today_vs_last_month > 0, "higher", "lower"), "</b> than a month ago and <b>$",round(today_vs_last_year, 2), " ", ifelse(today_vs_last_year > 0, "higher", "lower"), "</b> than a year ago.")

line_1y_description <- paste0('On ', prices_updated_date_pretty, ', the average cost of gas nationwide was <b>$', round(today_price, 2), ' per gallon</b>. That is <b>$',round(today_vs_yesterday, 2), ' ', ifelse(today_vs_yesterday > 0, 'higher', 'lower'), '</b> than the day before, <b>$',round(today_vs_last_month, 2), ' ', ifelse(today_vs_last_month > 0, 'higher', 'lower'), '</b> than a month ago and <b>$',round(today_vs_last_year, 2), ' ', ifelse(today_vs_last_year > 0, 'higher', 'lower'), '</b> than a year ago. <br><br>Choose a time frame : <br><span style="line-height:30px"><a target="_self" href="https://datawrapper.dwcdn.net/FinjE/" style="background:#014c12; padding:1px 6px; border-radius:5px; border:1px solid black; color:#ffffff; font-weight:400; cursor:pointer;"> 1 year </a> &nbsp;<a target="_self" href="https://datawrapper.dwcdn.net/VKUkr/" style="background:#858585; padding:1px 4px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;"> 5 years </a> &nbsp;<a target="_self" href="https://datawrapper.dwcdn.net/FveIx/" style="background:#858585; padding:1px 4px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;"> 30 days </a>')

line_5y_description <- paste0('On ', prices_updated_date_pretty, ', the average cost of gas nationwide was <b>$', round(today_price, 2), ' per gallon</b>. That is <b>$',round(today_vs_yesterday, 2), ' ', ifelse(today_vs_yesterday > 0, 'higher', 'lower'), '</b> than the day before, <b>$',round(today_vs_last_month, 2), ' ', ifelse(today_vs_last_month > 0, 'higher', 'lower'), '</b> than a month ago and <b>$',round(today_vs_last_year, 2), ' ', ifelse(today_vs_last_year > 0, 'higher', 'lower'), '</b> than a year ago. <br><br>Choose a time frame : <br>
<span style="line-height:30px"><a target="_self" href="https://datawrapper.dwcdn.net/FinjE/" style="background:#858585; padding:1px 4px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;"> 1 year </a> &nbsp;<a target="_self" href="https://datawrapper.dwcdn.net/VKUkr/" style="background:#014c12; padding:1px 6px; border-radius:5px; border:1px solid black; color:#ffffff; font-weight:400; cursor:pointer;"> 5 years </a>&nbsp;<a target="_self" href="https://datawrapper.dwcdn.net/FveIx/" style="background:#858585; padding:1px 4px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;"> 30 days </a>')


line_30d_description <- paste0('On ', prices_updated_date_pretty, ', the average cost of gas nationwide was <b>$', round(today_price, 2), ' per gallon</b>. That is <b>$',round(today_vs_yesterday, 2), ' ', ifelse(today_vs_yesterday > 0, 'higher', 'lower'), '</b> than the day before, <b>$',round(today_vs_last_month, 2), ' ', ifelse(today_vs_last_month > 0, 'higher', 'lower'), '</b> than a month ago and <b>$',round(today_vs_last_year, 2), ' ', ifelse(today_vs_last_year > 0, 'higher', 'lower'), '</b> than a year ago. <br><br>Choose a time frame : <br><span style="line-height:30px"><a target="_self" href="https://datawrapper.dwcdn.net/FinjE/" style="background:#858585; padding:1px 4px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;"> 1 year </a> &nbsp;<a target="_self" href="https://datawrapper.dwcdn.net/VKUkr/" style="background:#858585; padding:1px 4px; border-radius:5px; color:#ffffff; font-weight:400; box-shadow:0px 0px 7px 2px rgba(0,0,0,0.07); cursor:pointer;"> 5 years </a>&nbsp;<a target="_self" href="https://datawrapper.dwcdn.net/FveIx/" style="background:#014c12; padding:1px 6px; border-radius:5px; border:1px solid black; color:#ffffff; font-weight:400; cursor:pointer;"> 30 days </a>')




#5 year chart
aaa_historical_5years <- aaa_historical %>% 
  select(Date, Regular, Diesel) %>%
  filter(Date >= (prices_updated_date - years(5)))
write.csv(aaa_historical_5years, "data/aaa_historical_gas_prices_5years.csv", row.names = FALSE)

# Upload data and publish 5 year chart
dw_data_to_chart(aaa_historical_5years, chart_id = "VKUkr", api_key = dw_api_key)
dw_edit_chart(chart_id = "VKUkr", intro = line_5y_description, annotate = historical_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "VKUkr", api_key = dw_api_key)

#1 year chart
aaa_historical_1year <- aaa_historical %>% 
  select(Date, Regular, Diesel) %>%
  filter(Date >= (prices_updated_date - years(1)))
write.csv(aaa_historical_1year, "data/aaa_historical_gas_prices_1year.csv", row.names = FALSE)

# Upload data and publish 1 year chart
dw_data_to_chart(aaa_historical_1year, chart_id = "FinjE", api_key = dw_api_key)
dw_edit_chart(chart_id = "FinjE", intro = line_1y_description, annotate = historical_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "FinjE", api_key = dw_api_key)

#30 days chart
aaa_historical_30days <- aaa_historical %>% 
  select(Date, Regular, Diesel) %>%
  filter(Date >= (prices_updated_date - 30))
write.csv(aaa_historical_30days, "data/aaa_historical_gas_prices_30days.csv", row.names = FALSE)

# Upload data and publish 30 days chart
dw_data_to_chart(aaa_historical_30days, chart_id = "FveIx", api_key = dw_api_key)
dw_edit_chart(chart_id = "FveIx", intro = line_30d_description, annotate = historical_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "FveIx", api_key = dw_api_key)


# Upload data and publish bar chart
dw_data_to_chart(national_prices_regular, chart_id = "rT08j", api_key = dw_api_key)
dw_edit_chart(chart_id = "rT08j", intro = description, api_key = dw_api_key)
dw_publish_chart(chart_id = "rT08j", api_key = dw_api_key)


# test_state <- "CA"
# 
# resp <- request("https://gasprices.aaa.com/wp-admin/admin-ajax.php") |>
#   req_headers(
#     Accept = "application/json, text/javascript, */*; q=0.01",
#     `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8",
#     `X-Requested-With` = "XMLHttpRequest",
#     Origin = "https://gasprices.aaa.com",
#     Referer = paste0("https://gasprices.aaa.com/?state=", test_state),
#     `User-Agent` = "Mozilla/5.0"
#   ) |>
#   req_body_form(
#     action = "states_cost_data",
#     `data[locL]` = test_state,
#     `data[locR]` = "US"
#   ) |>
#   req_perform()
# 
# txt <- resp_body_string(resp)
# dat <- fromJSON(txt, simplifyVector = TRUE)
# data <- dat$data
# ca_df <- as.data.frame(data)




# get state abbreviations, including DC
state_abbs <- c(state.abb, "DC")

# create lookup vector for full state names
state_lookup <- c(setNames(state.name, state.abb),
                  "DC" = "District of Columbia")

period_labels <- c(
  "Current Avg.-state",
  "Current Avg.-national",
  "Yesterday Avg.-state",
  "Yesterday Avg.-national",
  "Week Ago Avg.-state",
  "Week Ago Avg.-national",
  "Month Ago Avg.-state",
  "Month Ago Avg.-national",
  "Year Ago Avg.-state",
  "Year Ago Avg.-national"
)

get_state_gas <- function(state_code) {
  Sys.sleep(runif(1, 0.8, 1.8))
  
  resp <- request("https://gasprices.aaa.com/wp-admin/admin-ajax.php") |>
    req_headers(
      Accept = "application/json, text/javascript, */*; q=0.01",
      `Content-Type` = "application/x-www-form-urlencoded; charset=UTF-8",
      `X-Requested-With` = "XMLHttpRequest",
      Origin = "https://gasprices.aaa.com",
      Referer = paste0("https://gasprices.aaa.com/?state=", state_code),
      `User-Agent` = "Mozilla/5.0"
    ) %>% 
    req_body_form(
      action = "states_cost_data",
      `data[locL]` = state_code,
      `data[locR]` = "US"
    ) %>% 
    req_perform()
  
  txt <- resp_body_string(resp)
  
  if (!startsWith(trimws(txt), "{")) {
    return(tibble(
      state = state_code,
      period = NA_character_,
      prices_updated_date = prices_updated_date,
      error = "Non-JSON response",
      raw_response = substr(txt, 1, 300)
    ))
  }
  
  dat <- fromJSON(txt, simplifyVector = TRUE)
  
  df <- as_tibble(dat$data) %>% 
    distinct() %>% 
    mutate(
      period = period_labels[seq_len(n())],
      state = state_code,
      state_name = state_lookup[state_code],
      prices_updated_date = prices_updated_date
    ) %>% 
    relocate(state, state_name, period, prices_updated_date) %>% 
    filter(!grepl("national", period)) %>% 
    select(state_name, period, unleaded)
  
  df
}

all_states_prices <- map_dfr(state_abbs, get_state_gas) %>% 
  pivot_wider(names_from = period, values_from = unleaded) %>% 
  rename(
    State = state_name,
    Today = `Current Avg.-state`,
    Yesterday = `Yesterday Avg.-state`,
    `Last week` = `Week Ago Avg.-state`,
    `Last month` = `Month Ago Avg.-state`,
    `Last year` = `Year Ago Avg.-state`
  )

state_prices_clean <- all_states_prices %>% 
  mutate(Today = as.numeric(Today),
         Yesterday = as.numeric(Yesterday),
         `Last week` = as.numeric(`Last week`),
         `Last month` = as.numeric(`Last month`),
         `Last year` = as.numeric(`Last year`)) %>% 
  mutate(today_vs_yesterday = round((Today - Yesterday), 2),
         today_vs_last_month = round(Today - `Last month`, 2),
         today_vs_last_year = round(Today - `Last year`, 2))




map_note <- paste0("As of ", prices_updated_date_pretty, ".")

# Upload data and publish chart
dw_data_to_chart(state_prices_clean, chart_id = "812II", api_key = dw_api_key)
dw_edit_chart(chart_id = "812II", annotate = map_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "812II", api_key = dw_api_key)

write.csv(national_prices, "data/national_gas_prices_aaa.csv", row.names = FALSE)
write.csv(state_prices_clean, "data/state_gas_prices_aaa.csv", row.names = FALSE)


#table 

state_prices_for_table <- state_prices_clean %>% 
  select(State, `Last year`, `Last month`, `Last week`, `Yesterday`, `Today`) %>% 
  arrange(desc(`Today`))

# Upload data and publish chart
dw_data_to_chart(state_prices_for_table, chart_id = "TIphM", api_key = dw_api_key)
dw_edit_chart(chart_id = "TIphM", annotate = map_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "TIphM", api_key = dw_api_key)







