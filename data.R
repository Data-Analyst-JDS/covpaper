library(tidyverse)
library(googlesheets4)

covid19 <- 
  read_sheet(
    ss = config::get(value = "url", config = "covid19"),
    sheet = config::get(value = "sheet", config = "covid19"),
    col_types = "Diiiiiii"
  )

traffic <- 
  read_sheet(
    ss = config::get(value = "url", config = "traffic"),
    sheet = config::get(value = "sheet", config = "traffic"),
    col_types = "Ddc"
  )

mobility <- 
  read_sheet(
    ss = config::get(value = "url", config = "mobility"),
    sheet = config::get(value = "sheet", config = "mobility"),
    col_types = "Dcdc"
  )

stayput <- 
  read_sheet(
    ss = config::get(value = "url", config = "stayput"),
    sheet = config::get(value = "sheet", config = "stayput"),
    col_types = "Dddc"
  )
