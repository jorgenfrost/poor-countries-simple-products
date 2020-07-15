#' This functions links the Wholesale price index (WPI) series based from
#' 1993-94 with the series based from 2004-2005. The output only lists 
#' the index for all goods together, not by product. 
#' 
#' @param wpi_1993_94_path Path to .xls file containing the wpi series based 
#' in 1993.
#' @param wpi_2004_05_path Path to .xls file containing the wpi series based
#' in 2004.
#' @param linking_table_path Path to .csv file containing the linking factor 
#' to use when combining the two series.
#'
#' @return Long data frame containing the joined series
#'
#' @export


clean_wpi <- function(wpi_1993_94_path, wpi_2004_05_path, linking_table_path) {
  
  
  # CLEAN 1993-1994 index ---------------------------------------------
  # Read 1993
  wpi93_raw <-
    read_excel(wpi_1993_94_path)
  
  # Format
  wpi93_tbl <- 
    wpi93_raw %>%
    filter(COMM_NAME == "ALL COMMODITIES") %>%
    pivot_longer(
      -c(COMM_NAME, COMM_CODE, COMM_WT),
      names_to = "fin_year",
      values_to = "index_val"
    ) %>%
    select(fin_year, index_val)
  
  wpi93_tbl <- 
    wpi93_tbl %>%
    mutate(
      fin_year = str_replace(
        string = fin_year,
        pattern = "IN",
        replacement = ""
      ),
      year = str_sub(
        string = fin_year,
        start = 1,
        end = 4
      ) %>% as.numeric()
    )
  
  # CLEAN 2004-2005 index ---------------------------------------------
  # Read 2004
  wpi04_raw <-
    read_excel(wpi_2004_05_path)
  
  # Format
  wpi04_tbl <- 
    wpi04_raw %>%
    filter(COMM_NAME == "ALL COMMODITIES") %>%
    pivot_longer(
      -c(COMM_NAME, COMM_CODE, COMM_WT),
      names_to = "fin_year",
      values_to = "index_val"
    ) %>%
    select(fin_year, index_val)
  
  wpi04_tbl <- 
    wpi04_tbl %>%
    mutate(
      fin_year = str_replace(
        string = fin_year,
        pattern = "IN",
        replacement = ""
      ),
      year = str_sub(
        string = fin_year,
        start = 1,
        end = 4
      ) %>% as.numeric()
    )
  
  # LINK INDICES ------------------------------------------------------
  link_tbl <- read_csv(linking_table_path) %>%
    filter(group == "All commodities")
  
  link_factor <- link_tbl$linking_factor
  
  # Prep 1993
  adj_wpi93_tbl <- 
    wpi93_tbl %>%
    mutate(
      index_val = index_val / link_factor,
      source = "base93"
    )
  # Prep 2004
  wpi04_tbl <-
    wpi04_tbl %>%
    mutate(
      source = "base04"
    )
  
  # Join and clean
  joined_series_tbl <-
    bind_rows(adj_wpi93_tbl, wpi04_tbl) %>%
    mutate(
      keep = case_when(
        year < 2005 & source == "base93" ~ 1,
        year %in% 2005:2016 & source == "base04" ~ 1,
        TRUE ~ 0
      )
    ) %>%
    filter(keep == 1 & year %in% 2000:2016) %>%
    select(-c(source, keep))
  
  # Write
  return(joined_series_tbl)
  
}
# END 
