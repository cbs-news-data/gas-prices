#devtools::install_github("RamiKrispin/EIAapi")

library(tidyverse)
library(jsonlite)
library(DatawRappr)


api_key <- Sys.getenv("EIA_API_KEY")

# Load environment variables
tryCatch(load_dot_env(), error = function(e) {}) 
dw_api_key <- Sys.getenv("DW_API_KEY")

# Authenticate with Datawrapper
datawrapper_auth(api_key = dw_api_key)

today = Sys.Date()
end = paste0(as.character(today), "T00")
last_year = Sys.Date()-365
start = paste0(as.character(last_year), "T00")

series = "EMM_EPM0_PTE_NUS_DPG"
# start = "2025-01-08T00"
# end = "2026-01-08T00"

URL <- paste0("https://api.eia.gov/v2/petroleum/pri/gnd/data/?frequency=weekly&data[0]=value&facets[series][]=",series,"&start=",start,"&end=",end,"&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=",api_key)

api_call <- fromJSON(URL, flatten = TRUE)

response <- api_call$response

data <- response$data

data_clean <- data %>% 
  select(period, value) %>% 
  mutate(value = round(as.numeric(value), digits=2)) %>% 
  arrange(period)

write.csv(data_clean, "data/weekly_gas_prices_eia.csv", row.names = FALSE)


#weekly residential heating oil price
series = "W_EPD2F_PRS_NUS_DPG"
# start = "2025-01-08T00"
# end = "2026-01-08T00"

URL <- paste0("https://api.eia.gov/v2/petroleum/pri/wfr/data/?frequency=weekly&data[0]=value&facets[series][]=",series,"&start=",start,"&end=",end,"&sort[0][column]=period&sort[0][direction]=desc&offset=0&length=5000&api_key=",api_key)

api_call <- fromJSON(URL, flatten = TRUE)

response <- api_call$response

data <- response$data

data_clean <- data %>% 
  select(period, value) %>% 
  mutate(value = round(as.numeric(value), digits=2)) %>% 
  arrange(period)

max_date <- max(as.Date(data_clean$period))
max_date_pretty <- format(max_date, "%b %d, %Y")

historical_note <- paste0("Data through ", max_date_pretty, ". Updated weekly.")

# Upload data and publish residential heating oil chart
dw_data_to_chart(data_clean, chart_id = "3UbWJ", api_key = dw_api_key)
dw_edit_chart(chart_id = "3UbWJ", annotate = historical_note, api_key = dw_api_key)
dw_publish_chart(chart_id = "3UbWJ", api_key = dw_api_key)

write.csv(data_clean, "data/weekly_heating_oil_residential_eia.csv", row.names = FALSE)

