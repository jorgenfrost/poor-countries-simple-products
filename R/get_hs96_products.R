#' This function converts the output block (J) from the ASI into HS96 product
#' codes (from ASICC and NPCMS-11). It creates a strict and a lenient match.
#' Strict matches uses only the unpartial matches throughout the concordance
#' chain. Lenient matches uses the first match when there is more than one 
#' match between classifications for a single code.
#' 
#' @param block_j_tbl The cleaned ASI block "J" (output products).
#' 
#' @return An output tbl with HS96 codes added to the original codes.
#' 
#' @export

# TODO: Perform quality check

# For testing
# block_ls_path <- "data/temp/asi_blocks_clean.rds"

get_hs96_products <- function(block_ls_path) {
  
  # Prepare block J
	block_ls <- readRDS(block_ls_path)
	block_j_tbl <- 
		block_ls %>%
		filter(block == "J") %>% 
		select(data) %>%
		unnest(data)

	rm(block_ls)


  # Remove products with qty sold = 0.
	block_j_tbl <- block_j_tbl %>%
		filter(qty_sold > 0)

	# The product classiciation change in 2010.
  npcms_years <- 2010:2015
  asicc_years <- 2000:2009
  
  block_j_tbl <- 
    block_j_tbl %>%
    mutate(
      code_scheme = case_when(
        year %in% npcms_years ~ "npcms",
        year %in% asicc_years ~ "asicc",
        TRUE ~ NA_character_
      )
    )
  
  # Remove non-products and totals -------------------------------------
  
  # NPCMS:
  # 10-11, 11-12, 12-13, 13-14, 14-15, 15-16:
  # Other products/by-products = 9921100
  # Total = 9995000
  # Products = sno = 1-10
  
  # ASICC:
  # 99-00, 00-01, 01-02, 02-03, 03-04, 04-05, 05-06, 06-07, 07-08, 08-09, 09-10:
  # Other Products/ By-Products = 99211, 
  # Total (1 to 11) = 99950
  # Products: sno = 1-10.
  
  block_j_tbl <- 
    block_j_tbl %>%
    mutate(
      non_product = case_when(
        code_scheme == "npcms" & (item_code == "9921100" | item_code == "9995000") ~ 1,
	code_scheme == "asicc" & (item_code == "99211" | item_code == "99950") ~1,
        TRUE ~ 0
      )
    )  %>% 
    filter(non_product != 1) %>% 
    filter(sno %in% 1:10) %>%
    select(-non_product) %>%
    filter(ex_factory_val > 0 & gross_sale_val > 0 & qty_sold > 0)
  
  # Add product concordances -------------------------------------------
  
  asicc_cpc2_tbl <- get_asicc_cpc2_concordance()
  cpc2_hs07_tbl <- get_cpc2_hs07_concordance()
  hs07_hs96_tbl <- get_hs07_hs96_concordance()
  
  
  # Convert to CPC-2 --------------------------------------------------
  # Remove last two digits (which are "india specific").

  # Convert ASICC years to CPC-2
  asicc_cpc2_tbl <- 
	  asicc_cpc2_tbl %>%
	  select(
		 asicc_code,
		 strict_cpc2,
		 lenient_cpc2
		 ) %>%
	  distinct()

	  asicc_tbl <-
		  block_j_tbl %>%
		  filter(code_scheme == "asicc") %>%
		  left_join(asicc_cpc2_tbl, 
			    by = c("item_code" = "asicc_code"))
  
  npcms_tbl <-
    block_j_tbl %>%
    filter(code_scheme == "npcms") %>%
    mutate(
      lenient_cpc2 = str_sub(
        string = item_code,
        start = 1,
        end = 5
      ),
      strict_cpc2 = lenient_cpc2 # same in NPCMS years
    )
  
  output_tbl <- bind_rows(npcms_tbl, asicc_tbl)
  
  
  # Convert CPC-2 to HS07 -----------------------------------------------
  
  # Create strict matches
  strict_cpc2_hs07_tbl <- 
    cpc2_hs07_tbl %>%
    select(cpc2_code, strict_hs07) %>%
    distinct()
  
  output_tbl <-
    left_join(output_tbl, strict_cpc2_hs07_tbl, by = c("strict_cpc2" = "cpc2_code"))
  
  # Create lenient matches
  lenient_cpc2_hs07_tbl <- 
    cpc2_hs07_tbl %>%
    select(cpc2_code, lenient_hs07) %>%
    distinct()
  
  output_tbl <- 
    left_join(output_tbl, lenient_cpc2_hs07_tbl, by = c("lenient_cpc2" = "cpc2_code"))
  
  
  # Convert HS07 to HS96 -----------------------------------------------
  
  # Create strict matches
  strict_hs07_hs96_tbl <- 
    hs07_hs96_tbl %>%
    select(hs07_code, strict_hs96) %>%
    distinct()
  
  output_tbl <- 
    left_join(output_tbl, strict_hs07_hs96_tbl, by = c("strict_hs07" = "hs07_code"))
  
  # Create lenient matches
  lenient_hs07_hs96_tbl <- 
    hs07_hs96_tbl %>%
    select(hs07_code, lenient_hs96) %>%
    distinct()
  
  output_tbl <- 
    left_join(output_tbl, lenient_hs07_hs96_tbl, by = c("lenient_hs07" = "hs07_code"))
  
  # Return
  output_tbl <-
    output_tbl %>%
    select(year, factory_id, item_code, qty_sold, ex_factory_val, gross_sale_val, multiplier, strict_hs96, lenient_hs96)
  
  return(output_tbl)
  
}
