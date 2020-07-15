#' This function joins and cleans the data from the Power Supply Position
#' of states from the CEA. It fixes state names and state codes so they 
#' the same scheme as in the ASI. It also computes lagged shortage variables
#' for the plant-entry analysis.
#' 
#' @param old_avg_raw The csv file from indiaenergydata (Allcott et all).
#' @param avg_2003_data The csv file with the missing 2003-data.
#' @param new_cea_files A list of all the csv files digitally extracted 
#' from the CEA reports.
#' @param old_state_scheme_tbl The .xls table containing the state scheme
#' used in ASI years 1998 to 2011.
#' @param new_state_scheme_tbl The .xls table contatining the state scheme
#' used in ASI years 2012 and on.
#' 
#' @return table containg the average and lagged average shortage in each
#' state in each year.
#' 
#' @export

clean_energy_supply <- function(
  old_avg_raw = read_csv(here("data/external/power_supply_position/from_allcott_et_al/EnergyRequirement.csv")),
  avg_2003_data = read_csv(here("data/external/power_supply_position/from_allcott_et_al/energy_requirement_2003_data.csv")),
  new_cea_files = list.files(
    here("data/external/power_supply_position/from_cea/csv/"),
    pattern = "*.csv",
    full.names = TRUE
  ),
  old_state_scheme_tbl = read_excel(here("data/external/asi/asi_2010_2016/State Master 1998-99 to 2011-12.xls"), skip = 2),
  new_state_scheme_tbl = read_excel(here("data/external/asi/asi_2010_2016/State Master 2012-13 onwards.xls"), skip = 2) 
) {
  
  
  # Read data --------------------------------------------------
  
  year_range <- 1998:2016
  
  # old cea data
  old_avg_raw <- bind_rows(old_avg_raw, avg_2003_data)
  
  old_avg_tbl <- old_avg_raw %>%
    clean_names(case = "snake") %>%
    select(year, state, requirement_mu, availability_mu) %>% # exclude any empty cols
    filter(year %in% year_range)
  
  # newer cea data
  # separate
  new_avg_files <- new_cea_files[!str_detect(new_cea_files, "peak")]
  
  # read into df with all years
  new_avg_tbl <- map_dfr(new_avg_files, read_csv) %>%
    filter(year %in% year_range)
  
  # Remove aggregated regions ----------------------------------
  
  aggr_region <- c("All India", "Northern Region", "North-Eastern Region", "Eastern Region", "Southern Region", "Western Region")
  
  new_avg_tbl <- new_avg_tbl %>% 
    filter(!state %in% aggr_region) 
  
  # Fix state names --------------------------------------------
  
  
  # Prepare the state-scheme used in the ASI
  old_state_scheme_years <- 1998:2011 
  new_state_scheme_years <- 2012:2016
  
  old_state_scheme_tbl <- old_state_scheme_tbl %>%
    clean_names(case = "snake") %>%
    mutate(state_scheme = "old")
  
  new_state_scheme_tbl <- new_state_scheme_tbl %>%
    clean_names(case = "snake") %>%
    mutate(state_scheme = "new") 
  
  state_scheme_tbl <- bind_rows(old_state_scheme_tbl, new_state_scheme_tbl)
  
  old_avg_tbl <- 
    old_avg_tbl %>%
    mutate(
      state_scheme = case_when(
        year %in% old_state_scheme_years ~ "old",
        year %in% new_state_scheme_years ~ "new",
        TRUE ~ NA_character_
      )
    )
  
  new_avg_tbl <- 
    new_avg_tbl %>%
    mutate(
      state_scheme = case_when(
        year %in% old_state_scheme_years ~ "old",
        year %in% new_state_scheme_years ~ "new",
        TRUE ~ NA_character_
      )
    )
  
  # Change names to fit in ASI scheme
  fix_states <- function(df, source) {
    if (source == "old") {
      df <- df %>%
        mutate(
          state = case_when(
            state_scheme == "old" ~ case_when(
              state == "Andaman- Nicobar" ~ "Andaman & N. Island",
              state == "AndhraPradesh" ~ "Andhra Pradesh",
              state == "Arunachal PR." ~ "Arunachal Pradesh", 
              state == "Chandigarh" ~ "Chandigarh(U.T.)",
              state == "Pondicheny" ~ "Pondicherry",
              state == "Chhattisgarh" ~ "Chattisgarh",
              state == "Dadar Nagar Haveli" ~ "Dadra & Nagar Haveli",
              state == "Uttarakhand" ~ "Uttaranchal",
              state == "Lakshadweep#" ~ "Lakshadweep",
              state == "W.Bengal + Sikkim" ~ "West Bengal + Sikkim",
              state == "DVC" ~ "Damodar Valley Corporation",
              TRUE ~ as.character(state)
            ),
            state_scheme == "new" ~ case_when( # only old scheme in old source
              TRUE ~ as.character(state)
            )
          )
        )
    } else if (source == "new") {
      df <- df %>%
        mutate(
          state = case_when(
            state_scheme == "old" ~ case_when(
              state == "Chandigarh" ~ "Chandigarh(U.T.)",
              state == "Uttarakhand" ~ "Uttaranchal",
              state == "Puducherry" ~ "Pondicherry",
              state == "Chhattisgarh" ~ "Chattisgarh",
              TRUE ~ as.character(state)
            ),
            state_scheme == "new" ~ case_when(
              state == "Andaman & Nicobar" ~ "Andaman & N. Island",
              state == "Chandigarh" ~ "Chandigarh(U.T.)",
              state == "Chhattisgarh" ~ "Chattisgarh",
              state == "Odisha" ~ "Orissa",
              TRUE ~ as.character(state)
            )
          )
        )
    } # end of if
  } # end of function
  
  old_avg_tbl <- fix_states(old_avg_tbl, source = "old") 
  
  # For 3 years (2002,03,04) Sikkim and West Bengal is listed together. 
  # I set these to West Bengal, since Sikkim is extremely sparsely represented
  # in the ASI.
  
  old_avg_tbl <-
    old_avg_tbl %>%
    mutate(
      state = ifelse(state == "West Bengal + Sikkim", "West Bengal", state)
    )
  
  new_avg_tbl <- fix_states(new_avg_tbl, source = "old") 
  new_avg_tbl <- fix_states(new_avg_tbl, source = "new") 
  
  # Join the two series and add state_codes: since the old data is typed and 
  # the new data is concverted digitally, I use the new data for overlapping
  # years. 
  
  old_avg_tbl <- old_avg_tbl %>%
    filter(year < min(new_avg_tbl$year)) 
  
  avg_tbl <- bind_rows(old_avg_tbl, new_avg_tbl)
  
  
  avg_tbl <- left_join(avg_tbl, state_scheme_tbl, by = c("state" = "state_name", "state_scheme")) %>%
    rename(state_code = codes) %>%
    select(-state_scheme)
  
  
  
  # Calculate shortage -------------------------------------------------
  
  # Average avg_shortage
  avg_tbl <- 
    avg_tbl %>%
    mutate(
      avg_shortage = (requirement_mu - availability_mu) / requirement_mu
    )
  
  # Lagged avg_shortage
  avg_tbl <- 
    avg_tbl %>%
    group_by(state_code) %>%
    arrange(year) %>%
    mutate(
      avg_shortage_1lag = lag(avg_shortage, n = 1),
      avg_shortage_2lag = lag(avg_shortage, n = 2),
      avg_shortage_3lag = lag(avg_shortage, n = 3)
    ) 
  
  # Return
  return(avg_tbl)
  
} # End function
