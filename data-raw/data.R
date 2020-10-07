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
    col_types = "cD_d"
  )

policy <- 
  read_sheet(
    ss = config::get(value = "url", config = "default"),
    sheet = config::get(value = "sheet", config = "default"),
    col_types = "DDccc__"
  ) %>% 
  mutate(
    event = fct_reorder(event, start),
    group = factor(
      group,
      levels = c("DKI Jakarta", "Bodebek", "Bandung Raya", "Jawa Barat", "Indonesia")
    )
  )

policy_period <- 
  policy %>% 
  group_by(period = periode) %>%
  summarise(start = min(start),
            end = max(end)) %>%
  mutate(end = if_else(period == "1", max(start), end))

# Save data into project --------------------------------------------------

usethis::use_data(
  covid19, 
  congestion, 
  mobility, 
  stayput,
  policy,
  policy_period,
  overwrite = TRUE
)
