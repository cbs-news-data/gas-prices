library(tidyverse)
library(stringr)
library(janitor)
library(xml2)

gas_prices <- read.csv("data/weekly_gas_prices_eia.csv")

date_max <- max(gas_prices$period)
date_min <- min(gas_prices$period)
value_max <- max(gas_prices$value)
value_min <- min(gas_prices$value)

#get pretty min date
date_min_pretty <- format(min(as.Date(gas_prices$period)), "%b %d, %Y")
date_min_pretty <-str_replace_all(date_min_pretty, " 0", " ")
#get pretty max date
date_max_pretty <- format(max(as.Date(gas_prices$period)), "%b %d, %Y")
date_max_pretty <-str_replace_all(date_max_pretty, " 0", " ")


# Generate 4 to 5 axis ticks using pretty()
ticks <- pretty(c(0, value_max), n = 4)
#get max ticks value
ticks_max <- max(ticks)
# Convert to character string with "|" separator
ticks_string <- paste(ticks, collapse = "|")
ticks_string <- str_replace(ticks_string, "0\\|", "")


#simplify to "label" and "value" column + "showLabel" + showValue" + "valueToShow" column...change columns 
data_df_forXML <- gas_prices %>% 
  select(period, value) %>% 
  rename(label = period,
         value = value) %>% 
  mutate(showLabel = case_when(label == date_min ~ "1",
                               label == date_max ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(showValue = case_when(label == date_min ~ "1",
                               label == date_max ~ "1",
                               TRUE ~ "0")) %>% 
  mutate(label = format(as.Date(label), "%b %d, %Y")) %>% 
  mutate(label = str_replace_all(label, " 0", " ")) %>% 
  mutate(valueToShow = paste0("$", value))

#get labels for x axis
price_labels = paste0(date_min_pretty, "|", date_max_pretty)


#variables 
xml_title <- "Weekly Retail Gas Prices"
xml_subtitle <- paste0("U.S. all grades all formulations, $/gallon")
xml_xaxis <- price_labels #labels/values for x axis
xml_yaxis <- ticks_string #labels/values for y axis, only fill out in necessary
xml_ymax <-  ticks_max #float value for max value OF AXIS
xml_source <- "U.S. Energy Information Administration"
xml_date <- paste0("As of ", date_max_pretty)
xml_type <- "line" #line, bar, pie, etc
xml_qualifier <- " "#one line note, if needed


# Create chart node
chart_xml <- xml_new_root("chart")

#add children (title, subtitle, type)
xml_add_child(chart_xml, "title", xml_title)
xml_add_child(chart_xml, "subtitle", xml_subtitle)
xml_add_child(chart_xml, "type", xml_type)
xml_add_child(chart_xml, "x-axis", xml_xaxis)
xml_add_child(chart_xml, "y-axis", xml_yaxis)
xml_add_child(chart_xml, "y-max", xml_ymax)

# Add data rows
for (i in 1:nrow(data_df_forXML)) {
  row_node <- xml_add_child(chart_xml, "dataPoint")
  for (col_name in names(data_df_forXML)) {
    xml_add_child(row_node, col_name, as.character(data_df_forXML[i, col_name]))
  }
}


xml_add_child(chart_xml, "source", xml_source)
xml_add_child(chart_xml, "date", xml_date)
xml_add_child(chart_xml, "qualifier", xml_qualifier)


# Write XML to file
write_xml(chart_xml, "data/weekly_gas_prices.xml")