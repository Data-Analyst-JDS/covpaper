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

traffic <- 
  read_sheet(
    ss = config::get(value = "url", config = "traffic"),
    sheet = config::get(value = "sheet", config = "traffic"),
    col_types = "cDd"
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
    col_types = "cDdd"
  )

policy <- 
  read_sheet(
    ss = config::get(value = "url", config = "default"),
    sheet = config::get(value = "sheet", config = "default"),
    col_types = "DDccccc__"
  ) %>% 
  mutate(
    event = fct_reorder(event_english, start),
    group = factor(
      group,
      levels = c("Bandung Raya", "Bodebek", "Jawa Barat", "DKI Jakarta", "Indonesia")
    )
  )

policy_period <- 
  tibble(
    period = c("Period I", "Period II"),
  start = as.Date(c("2020-03-01", "2020-06-01")),
  end = as.Date(c("2020-06-01", "2020-09-30"))
)

# Save data into project --------------------------------------------------

usethis::use_data(covid19, overwrite = TRUE)
usethis::use_data(traffic, overwrite = TRUE)
usethis::use_data(mobility, overwrite = TRUE)
usethis::use_data(stayput, overwrite = TRUE)
usethis::use_data(policy, overwrite = TRUE)
usethis::use_data(policy_period, overwrite = TRUE)
