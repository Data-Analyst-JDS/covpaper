# Load packages -----------------------------------------------------------

library(tidyverse)
library(googlesheets4)

# Fetch data --------------------------------------------------------------

covid19 <- 
  read_sheet(
    ss = config::get(value = "url", config = "covid19"),
    sheet = config::get(value = "sheet", config = "covid19"),
    col_types = "Diii____"
  )

congestion <- 
  read_sheet(
    ss = config::get(value = "url", config = "congestion"),
    sheet = config::get(value = "sheet", config = "congestion"),
    col_types = "Ddc"
  )

mobility <- 
  read_sheet(
    ss = config::get(value = "url", config = "mobility"),
    sheet = config::get(value = "sheet", config = "mobility"),
    col_types = "Dcd"
  )

stayput <- 
  read_sheet(
    ss = config::get(value = "url", config = "stayput"),
    sheet = config::get(value = "sheet", config = "stayput"),
    col_types = "D_d"
  )

policy <- 
  read_sheet(
    ss = config::get(value = "url", config = "default"),
    sheet = config::get(value = "sheet", config = "default"),
    col_types = "DDccc__"
  )

# Save data into project --------------------------------------------------

usethis::use_data(
  policy,
  covid19, 
  congestion, 
  mobility, 
  stayput,
  overwrite = TRUE
)