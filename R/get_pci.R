#' This function calculates product complexity values from normalized 
#' export data using the HH algortihm. The function uses the eigen-value
#' method to extract the complexity values, rather then run the algorithm 
#' a set number of iterations.
#' 
#' @param source Where does the data come from? If "oec" then input it already
#' calculated complexity values, and they are just cleaned. If "rca" then input 
#' is trade data normalized to RCA values. If "rpca" then trade data is normalized
#' to RpcA values.
#' @param oec_data Path to the file containing the OEC complexity values.
#' @param rca_data The data frame containing the trade data normalized
#' to RCA
#' @param rpca_data The data frame containing the trade data normalized
#' to RpcA.
#' 
#' @return A tibble containing PCI values for each year.
#'
#' @export

get_pci <- function(source, oec_data = NULL, rca_data = NULL, rpca_data = NULL) {
  
  source <- str_to_lower(source)
  
  if (!(source %in% c("oec", "rca", "rpca"))) {
    stop("'source' does not match any of the possible options.")
  }
  
  # There are three ways I construct product complexity values (PCI):
  # 1. Get them "ready-made" from the Observatory of Economic Complexity (OEC)
  # 2. Calculate them from trade data normalized by RCA
  # 3. Calculate them from trade data normalized by RpcA.
  
  #################################################################
  ##            DEFINE ALGORITHM AND HELPER FUNCTION             ##
  #################################################################
  
  # Define HH algorithm (eigen method) --------------------------------
  eigenmethod_complexity <- function(Mcp, return = "PCI") {
    
    if (!is.matrix(Mcp)) stop("'Mcp' has to be a matrix.")
    if (!return %in% c("PCI", "ECI")) stop("'return' has to be either 'PCI' or 'ECI'")
    
    # Get Mcp, Kc, Kp ---------------------------------------------------
    # Transform to Mcp
    
    # Get Kc0 and Kp0
    Kc0 <- rowSums(Mcp) # diversity
    
    Kp0 <- colSums(Mcp) # ubuiquity
    
    # Get ECI and PCI ---------------------------------------------------
    # Method taken from the economic complexity package on CRAN ('economicomplexity').
    # ECI
    Mcc <- (Mcp / Kc0) %*% (t(Mcp) / Kp0)
    K_vec <- eigen(Mcc)$vectors[, 2] %>% # get eigen-vector ass. with second largest eigen val
      
      Re()
    ECI <- (K_vec - mean(K_vec)) / sd(K_vec) # standardize (Z)
    ECI <- setNames(ECI, rownames(Mcp))
    
    # PCI 
    Mpp <- (t(Mcp) / Kp0) %*% (Mcp / Kc0) 
    Q_vec <- eigen(Mpp)$vectors[, 2] %>% # get eigen-vector ass. with second largest eigen val
      Re()
    PCI <- (Q_vec - mean(Q_vec)) / sd(Q_vec) # standardize (Z)
    PCI <- setNames(PCI, colnames(Mcp)) 
    
    # Return output -----------------------------------------------------
    
    if (return == "PCI") {
      return(PCI)
    } else if (return == "ECI") {
      return(ECI)
    }
    
  } # End eigenmethod complexity
  
  # Define helper function --------------------------------------------
  # Helper functions takes the long data frame for each year and turns 
  # it into the Mcp matrix. Then applies the algorithm and reformats into
  # long data and returns it.
  
  helper_fun <- function(tbl, metric) {
    
    incidence_mat <- 
      tbl %>%
      pivot_wider(names_from = hs_product_code, values_from = val) %>%
      column_to_rownames("country_code") %>%
      as.matrix()
    
    output <- eigenmethod_complexity(Mcp = incidence_mat, return = metric)
    
    if (metric == "PCI") {
      return(tibble("hs96_code" = names(output), "pci" = output))
    } else if (metric == "ECI") {
      return(tibble("country_code" = names(output), "eci" = output))
    }
    
  }
  
  #################################################################
  ##                           OEC PCI                           ##
  #################################################################
  
  if (source == "oec") {
    
    # Check if data is supplied -----------------------------------------
    if (!is.character(oec_data)) stop("'oec_data' should be the path to the downloaded file.")
    
    # Read and clean the data: ------------------------------------------
    # I get 4 digit HS96 PCI values from OEC PRO. They are listed with 5-6 digits 
    # (first 1 and 2 are section headings (leading 0's not included. I remove these
    # headings first, leaving me with PCI values at the HS96 digit level (see 
    # email from Alex).
    
    pci_raw <- read_csv(here(oec_data))
    
    pci_tbl <-
      pci_raw %>%
      gather(-c(`HS4`,`HS4 ID`), key = year, val = pci) %>%
      clean_names(case = "snake") %>%
      mutate(
        hs96_code = case_when(
          str_length(hs4_id) == 5 ~ str_sub(hs4_id, start = 2, end = 5),
          str_length(hs4_id) == 6 ~ str_sub(hs4_id, start = 3, end = 6),
          TRUE ~ NA_character_
        )
      ) %>%
      select(year, hs96_code, pci) %>%
      mutate(year = as.numeric(year))
    
    
  } # end of oec if
  
  #################################################################
  ##                           RCA PCI                           ##
  #################################################################
  
  if (source == "rca") {
    
    # Check if data is supplied -----------------------------------------
    if (!is.data.frame(rca_data)) stop("'rca_data' should be a data frame containing the export data normalized by RCA.")
    
    rca_nest <-
      rca_data %>%
      select(-export_value) %>% # not needed
      rename(val = rca) %>%
      mutate(val = ifelse(val >= 1, 1, 0)) %>% # binarize RCA to get Mcp
      nest(data = -year) 
    
    pci_tbl <- 
      rca_nest %>%
      mutate(
        pci = map(data, helper_fun, metric = "PCI"),
      ) %>%
      select(year, pci) %>%
      unnest(pci) 
    
  } # end of rca if
  
  ##################################################################
  ##                           RPCA PCI                           ##
  ##################################################################
  
  if (source == "rpca") {
    
    # Check if data is supplied -----------------------------------------
    if (!is.data.frame(rpca_data)) stop("'rpca_data' should be a data frame containing the export data normalized by RpcA.")
    
    rpca_nest <-
      rpca_data %>%
      select(-export_value) %>% # not needed
      rename(val = rpca) %>%
      mutate(val = ifelse(val >= 1, 1, 0)) %>% # binarize RCA to get Mcp
      nest(data = -year) 
    
    pci_tbl <- 
      rpca_nest %>%
      mutate(
        pci = map(data, helper_fun, metric = "PCI"),
      ) %>%
      select(year, pci) %>%
      unnest(pci) 
    
  } # end of rpca if
  
  # Return ------------------------------------------------------------
  
  return(pci_tbl)
  
}


