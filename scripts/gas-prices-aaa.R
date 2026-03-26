library(tidyverse)
library(rvest)
library(janitor)
library(stringr)
library(lubridate)
library(readr)
library(DatawRappr)

# Load environment variables
tryCatch(load_dot_env(), error = function(e) {}) 
dw_api_key <- Sys.getenv("DW_API_KEY")

# Authenticate with Datawrapper
datawrapper_auth(api_key = dw_api_key)


url <- "https://gasprices.aaa.com/"

page <- read_html(url)

# Get the date of the most recent price update
prices_updated <- page %>% 
  html_element(xpath = "/html/body/main/div[2]/div/div[1]/div/span") %>% 
  html_text(trim = TRUE) %>% 
  str_replace("Price as of", "")

prices_updated_date <- mdy(prices_updated)
prices_updated_date_pretty <- format(prices_updated_date, "%b %d, %Y")

#national prices
prices <- page %>% 
  html_element(xpath = "//h1[contains(., 'National average gas prices')]/following::table[1]") %>% 
  html_table(fill = TRUE) %>% 
  janitor::clean_names() 

national_prices <- prices %>%
  mutate(Date = prices_updated_date) %>%
  rename(Period = x,
         Regular = regular,
         `Mid-Grade` = mid_grade,
         Premium = premium,
         Diesel = diesel,
         E85 = e85) %>% 
  filter(Period == "Current Avg.") %>% 
  select(Date, Regular, `Mid-Grade`, Premium, Diesel, E85) %>% 
  mutate(Regular = as.numeric(str_replace(Regular, "\\$", "")),
         `Mid-Grade` = as.numeric(str_replace(`Mid-Grade`, "\\$", "")),
         Premium = as.numeric(str_replace(Premium, "\\$", "")),
         Diesel = as.numeric(str_replace(Diesel, "\\$", "")),
         E85 = as.numeric(str_replace(E85, "\\$", "")))

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
  select(x, regular) %>%
  rename(period = x,
         price = regular) %>%
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


# get state abbreviations, including DC
state_abbs <- c(state.abb, "DC")

# create lookup vector for full state names
state_lookup <- c(setNames(state.name, state.abb),
                  "DC" = "District of Columbia")

# set URL base for state prices
url_base <- "https://gasprices.aaa.com/?state="

get_state_prices <- function(state_abb) {
  
  message("Scraping: ", state_abb)
  
  # get full state name
  state_full <- state_lookup[[state_abb]]
  
  url <- paste0(url_base, state_abb)
  page <- read_html(url)
  
  tbl <- page %>%
    html_element(xpath = "//h1[contains(., 'average gas prices')]/following::table[1]") %>%
    html_table(fill = TRUE) %>% 
    clean_names() %>%
    mutate(
      state = state_abb,
      state_name = state_full
    ) %>%
    select(state, state_name, x, regular) %>% 
    setNames(c("state", "state_name", "period", "price")) %>% 
    mutate(
      price = as.numeric(str_replace(price, "\\$", "")),
      period = str_replace(period, "Current Avg.", "Today"),
      period = str_replace(period, "Yesterday Avg.", "Yesterday"),
      period = str_replace(period, "Week Ago Avg.", "Last week"),
      period = str_replace(period, "Month Ago Avg.", "Last month"),
      period = str_replace(period, "Year Ago Avg.", "Last year")
    )
  
  print(head(tbl))
  
  return(tbl)
}

safe_get_state_prices <- possibly(get_state_prices, otherwise = NULL)

all_state_prices <- map_dfr(state_abbs, safe_get_state_prices)


state_prices_clean <- all_state_prices %>% 
  pivot_wider(names_from = period, values_from = price) %>% 
  mutate(today_vs_yesterday = round(Today - Yesterday, 2),
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
  select(state_name, `Last year`, `Last month`, `Last week`, `Yesterday`, `Today`) %>% 
  rename(State = state_name) %>%
  arrange(desc(`Today`))

# Upload data and publish chart
dw_data_to_chart(state_prices_for_table, chart_id = "TIphM", api_key = dw_api_key)
dw_edit_chart(chart_id = "TIphM", annotate = map_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "TIphM", api_key = dw_api_key)







