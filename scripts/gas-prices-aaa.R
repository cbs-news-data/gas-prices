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
national_prices <- page %>% 
  html_element(xpath = "//h1[contains(., 'National average gas prices')]/following::table[1]") %>% 
  html_table(fill = TRUE) %>% 
  janitor::clean_names() %>%
  select(x, regular) %>% 
  setNames(c("period", "price")) %>% 
  mutate(price = as.numeric(str_replace(price, "\\$", ""))) %>% 
  mutate(period = str_replace(period, "Current Avg.", "Today"),
         period = str_replace(period, "Yesterday Avg.", "Yesterday"),
         period = str_replace(period, "Week Ago Avg.", "Last week"),
         period = str_replace(period, "Month Ago Avg.", "Last month"),
         period = str_replace(period, "Year Ago Avg.", "Last year"))

today_price <- national_prices %>% filter(period == "Today") %>% pull(price)
yesterday_price <- national_prices %>% filter(period == "Yesterday") %>% pull(price)
last_month_price <- national_prices %>% filter(period == "Last month") %>% pull(price)
last_year_price <- national_prices %>% filter(period == "Last year") %>% pull(price)

today_vs_yesterday <- round(today_price - yesterday_price, 2)
today_vs_last_month <- round(today_price - last_month_price, 2)
today_vs_last_year <- round(today_price - last_year_price, 2)


description <- paste0("On ", prices_updated_date_pretty, ", the average cost of gas nationwide was <b>$", round(today_price, 2), " per gallon</b>. That's <b>$",round(today_vs_yesterday, 2), " ", ifelse(today_vs_yesterday > 0, "higher", "lower"), "</b> than the day before, <b>$",round(today_vs_last_month, 2), " ", ifelse(today_vs_last_month > 0, "higher", "lower"), "</b> than a month ago and <b>$",round(today_vs_last_year, 2), " ", ifelse(today_vs_last_year > 0, "higher", "lower"), "</b> than a year ago.")

# Upload data and publish chart
dw_data_to_chart(national_prices, chart_id = "rT08j", api_key = dw_api_key)
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
  pivot_wider(names_from = period, values_from = price)

# Upload data and publish chart
dw_data_to_chart(state_prices_clean, chart_id = "812II", api_key = dw_api_key)
#dw_edit_chart(chart_id = "812II", intro = map_description, api_key = dw_api_key)
dw_publish_chart(chart_id = "812II", api_key = dw_api_key)

write.csv(national_prices, "data/national_gas_prices_aaa.csv", row.names = FALSE)
write.csv(state_prices_clean, "data/state_gas_prices_aaa.csv", row.names = FALSE)
