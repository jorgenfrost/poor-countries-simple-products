#' This file reads population data from the WDIs and cleans it up a bit.
#' Author: Søren Post
#' 
#' @param min_year numeric: remove all observations from the sample before
#' this year.
#' @return Returns a tidy tibble of country-year total populations.
#' @export

clean_pop_data <- function(pop_data, min_year) {
  
  # READ DATA -------------------------------------------------------
  
  pop_raw <- read_csv(here(pop_data), skip = 4)
  
  # PREPARE DATA ----------------------------------------------------
  # population data is in the usual terrible format from the WDI. I make it tidy.
  
  pop_tbl <-
    pop_raw %>%
    select(-c(`Indicator Name`, `Indicator Code`)) %>%
    gather(-c(`Country Name`, `Country Code`), key = year, value = pop) %>%
    clean_names(case = "snake") %>%
    mutate(year = as.numeric(year)) %>%
    filter(year >= min_year) %>% # earliest year of hs92 data
    filter(!is.na(pop))
  
  return(pop_tbl)
  
} # End
