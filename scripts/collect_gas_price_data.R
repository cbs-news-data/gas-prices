#devtools::install_github("RamiKrispin/EIAapi")

library(tidyverse)
library(jsonlite)

api_key <- Sys.getenv("EIA_API_KEY")

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
  arrange(period)
