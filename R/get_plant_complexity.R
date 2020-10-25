#' This function calculates the complexity of the ASI plants based on the products they
#' produce.
#'
#' @param output_tbl Block J of the ASI after concording the ASICC and NPCMS
#' codes to HS96.
#' @param rca_pci_tbl Data frame with HS96 PCI values based on RCA-normalized
#' trade data.
#' @param rpca_pci_tbl Data frame with HS96 PCI values based on RpcA-normalized
#' trade data.
#' @param product_match Whether or not products should be match according to the 
#' "strict" or "lenient" method.
#' @return Data frame with the plant complexity values.
#' @export

# For testing:
# output_tbl <- readd("hs96_output_tbl")
# rca_pci_tbl <- readd("own_rca_pci_tbl")
# rpca_pci_tbl <- readd("own_rpca_pci_tbl")
# product_match <- "lenient"

get_plant_complexity <- function(output_tbl, rca_pci_tbl, rpca_pci_tbl, product_match) {
  
  # Prepare data ----------------------------------------------
  
  rca_pci_tbl <- 
    rca_pci_tbl %>%
    rename(rca_pci = pci)
  
  rpca_pci_tbl <- 
    rpca_pci_tbl %>%
    rename(rpca_pci = pci)
  
  pci_tbl <- full_join(rca_pci_tbl, rpca_pci_tbl)
  
  
  # Attach product complexity values
  
  if (product_match == "strict") {
    
    output_tbl <-
      left_join(output_tbl, pci_tbl, by = c("year", "strict_hs96" = "hs96_code")) 
    
  } else if (product_match == "lenient") {
    
    output_tbl <-
      left_join(output_tbl, pci_tbl, by = c("year", "lenient_hs96" = "hs96_code")) 
    
  }
  
  # Calculate maximum complexity
  
  max_pci_tbl <-
    output_tbl %>%
    group_by(year, factory_id) %>%
    summarize(
      max_rca_pci = max(rca_pci),
      max_rpca_pci = max(rpca_pci)
    )
  
  # Calculate weighted average complexity
  
  # share of product in total value
    # Ex-factory is the value leaving the factory gate
  output_tbl <- 
    output_tbl %>%
    group_by(year, factory_id) %>%
    mutate(
      total_ex_factory_output = sum(ex_factory_val),
      share_of_total_output = ex_factory_val / total_ex_factory_output
    ) 
  
  # Get weighted average complexity
  avg_pci_tbl <-
    output_tbl %>%
    group_by(year, factory_id) %>%
    mutate(
      weighted_rca_pci = (rca_pci * share_of_total_output),
      weighted_rpca_pci = (rpca_pci * share_of_total_output)
    ) %>%
    summarize(
      avg_rca_pci = sum(weighted_rca_pci),
      avg_rpca_pci = sum(weighted_rpca_pci)
    )
  
  plant_complexity_tbl <-
    full_join(max_pci_tbl, avg_pci_tbl)
  
  return(plant_complexity_tbl)
  
}
