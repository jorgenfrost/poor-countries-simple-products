#' This function reads the ASI blocks from 2000-01 to 2009-2010. Blocks are
#' provided separately in .TXT files, and does not contain headers. Instead,
#' for each block, there are "Supporting files" that lists how many characters
#' that each variable inside each block contains. This is the information used 
#' separate the .TXT blocks into data frames. 
#'
#' @return A data frame with a list column containing all the blocks, as a 
#' 'year' and 'block' column to index different blocks. I clean the blocks
#' in different function.
#'
#' @export

# for testing
#                  early_2000_2007_file_path <- "data/temp/early_blocks_2000_2007_raw.rds"
#                  early_2008_file_path <- "data/temp/early_blocks_2008_raw.rds"
#                  early_2009_file_path <- "data/temp/early_blocks_2009_raw.rds"


read_earlier_asi_blocks <- function(
		 early_2000_2007_file_path,
		 early_2008_file_path,
		 early_2009_file_path
	) {

	# Read in all the files
  asi_files_2000_2009 <- list.files(
    path = here("data/external/asi/asi_2000_2009"),
    pattern = ".TXT",
    full.names = TRUE,
    recursive = TRUE
  )

	# Prepare the index'ed list columns. I use the block and the year
	# variable to filter for reading in the correct blocks
  asi_tbl <- 
	  tibble(file_path = asi_files_2000_2009) %>%
	  mutate(
		  year = str_extract(
			  file_path,
			  pattern = "[0-9]{4}-[0-9]{2}"
			  ),
		  year = str_sub(
			  year,
			  start = 1,
			  end = 4
			  ) %>% as.numeric(),
		  block = str_extract( # Get block line
			  file_path,
			  pattern = "OAS.*TXT"
			  ),
		  block = case_when( # File naming convention is different for 2009-2010 files
			  year == 2009 ~ str_replace_all(
				  block,
				  pattern = "(OAS)|([0-9]{4}.TXT)",
				  replacement = ""
				  ),
			  TRUE ~ str_replace_all( # Clean up block
				  block,
				  pattern = "(OASIBL)|(OASI)|(OAS)|(\\.TXT)",
				  replacement = ""
				  ) %>% 	
			  str_replace_all( # remove number
				  pattern = "[0-9]",
				  replacement = ""
			  )
		)
		  )

  asi_tbl <-
	  asi_tbl %>%
	  mutate(
		  data = map(file_path, read_csv, col_names = FALSE)
		  )

##################################################################
##            PREPARE 2000-2001 to 2007-2008 BLOCKS             ##
##################################################################


  # The blocks from 2000-01 to 2007-08 have the same panel data
  # structure. To avoid a lot of copy-pasting, I define a function
  # to fix it.

  clean_blocks <- function(asi_tbl, panel_structure, current_block) {


	  # This function is not super clean or clever, but boy 
	  # is it long. There are three "panel_structure", each referring to
	  # a different scheme the ASI blocks are structured after 
	  # ("2000-2007", "2008", "2009"). First, the function checks the 
	  # panel structure. Then, within the relevant scheme, the given block
	  # is fixed and returned. 

	  blk_raw <- asi_tbl

	  # Clean 2000-2007 blocks
	  if (panel_structure == "2000") {

	  if (current_block == "A") {

  # Block A ---------------------------------------------------------

  blk_tbl <- 
	  blk_raw %>%
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "scheme",
        "nic4code",
        "nic5code",
        "rural_urban",
        "sro",
        "no_units",
        "unit_status",
        "mw_days",
        "nw_days",
        "wdays",
        "production_cost",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13,  14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 55, 64)
    ) %>%
    select(-X1)

	  } else if (current_block == "B") {

  # Block B ---------------------------------------------------------
  blk_tbl <-
	  blk_raw %>%
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "type_organisation",
        "type_ownership",
	"no_of_units",
	"no_units_in_same_state",
        "initial_production",
        "accounting_year_from",
        "accounting_year_to",
        "months_of_operation",
        "ac_system",
        "asi_floppy",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 16, 20, 24, 28, 37, 46, 48, 49, 50, 59)
      )%>%
    select(-X1)

	  } else if (current_block == "C") {

  # Block C ---------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "sno",
        "opening_gross",
        "addition_reval",
        "addition_add",
        "deduction_adj",
        "closing_gross",
        "deprec_begin_year",
        "deprec_during_year",
        "deprec_end_year",
        "opening_net",
        "closing_net",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 27, 39, 51, 63, 75, 87, 99, 111, 123, 135, 144)
    ) %>%
    select(-X1)

	  } else if (current_block == "D") {

  # Block D ---------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "sno",
        "w_cap_opening",
        "w_cap_closing",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 27, 39, 48)
    ) %>%
    select(-X1)

	  } else if (current_block == "E") {

  # Block E ---------------------------------------------------------

  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "man_days",
        "non_man_days",
        "tot_days",
        "avg_person_worked",
        "days_paid",
        "wages",
        "bonus",
        "contrib_provident_fund",
        "staff_welfare_exp",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 23, 31, 41, 49, 59, 71, 83, 95, 107, 116)
    ) %>%
    select(-X1)

	  } else if (current_block == "F") {

  # Block F ---------------------------------------------------------

  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "exp_work_others",
        "exp_building_repair",
	"exp_plant_machinery",
	"exp_pollution_control",
        "exp_other_fixed_assets",
        "exp_operating",
        "exp_non_operating",
        "insurance",
        "rent_machinery",
        "exp_total",
        "rent_buildings",
        "rent_land",
        "interests_paid",
        "purc_val_goods_resold",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 169, 181, 190)
    ) %>%
    select(-X1)

	  } else if (current_block == "G") {

  # Block G ---------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "income_services",
        "var_semi_fin",
        "val_electricity_sold",
        "val_own_construction",
        "net_balance_goods_resold",
        "rent_income_machinery",
        "total_receipts",
        "rent_income_building",
        "rent_income_land",
        "interest_income",
        "sale_val_goods_resold",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 154)
    ) %>%
    select(-X1)

	  } else if (current_block == "H") {

  # Block H ---------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_consumed",
        "purchase_val",
        "unit_rate",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 51, 66, 75)
      ) %>%
    select(-X1)
  
	  } else if (current_block == "I") {

  # BLOCK I -----------------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_consumed",
        "purchase_val",
        "unit_rate",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 51, 66, 75)
    ) %>%
    select(-X1)

	  } else if (current_block == "J") {

  # BLOCK J -----------------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_made",
        "qty_sold",
        "gross_sale_val",
        "excise_duty",
        "sales_tax",
        "others",
        "total",
        "per_unit_sale_val",
        "ex_factory_val",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 55, 67, 79, 91, 103, 115, 130, 142, 151)      
    ) %>%
    select(-X1)

	  } else {
		  stop("Something is wrong with the 2000-2007 years.")

	  } 

	  } else if (panel_structure == "2008") {

#################################################################
##                  PREPARE 2008-09 BLOCKS                     ##
#################################################################

		  if (current_block == "A") {
  # Block A ---------------------------------------------------------

  blk_tbl <- 
	  blk_raw %>%
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "scheme",
        "nic4code",
        "nic5code",
        "rural_urban",
        "sro",
        "no_units",
        "unit_status",
        "mw_days",
        "nw_days",
        "wdays",
        "production_cost",
	"export_share",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13,  14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 55, 58, 67)
    ) %>%
    select(-X1)

	  } else if (current_block == "B") {

  # Block B ---------------------------------------------------------

  blk_tbl <-
	  blk_raw %>%
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "type_organisation",
        "type_ownership",
	"no_of_units",
        "initial_production",
        "accounting_year_from",
        "accounting_year_to",
        "months_of_operation",
        "ac_system",
        "asi_floppy",
	"investment_in_p_m",
	"iso",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 16, 20, 24, 33, 42, 44, 45, 46, 47, 48, 57)
      )%>%
    select(-X1)

	  } else if (current_block == "C") {

  # Block C ---------------------------------------------------------
  
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "sno",
        "opening_gross",
        "addition_reval",
        "addition_add",
        "deduction_adj",
        "closing_gross",
        "deprec_begin_year",
        "deprec_during_year",
	"deprec_adj_during_year",
        "deprec_end_year",
        "opening_net",
        "closing_net",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 27, 39, 51, 63, 75, 87, 99, 111, 123, 135, 147, 156)
    ) %>%
    select(-X1)

	  } else if (current_block == "D") {

  # Block D ---------------------------------------------------------
  
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "sno",
        "w_cap_opening",
        "w_cap_closing",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 27, 39, 48)
    ) %>%
    select(-X1)

	  } else if (current_block == "E") {

  # Block E ---------------------------------------------------------

  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "man_days",
        "non_man_days",
        "tot_days",
        "avg_person_worked",
        "days_paid",
        "wages",
        "bonus",
        "contrib_provident_fund",
        "staff_welfare_exp",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 23, 31, 41, 49, 59, 71, 83, 95, 107, 116)
    ) %>%
    select(-X1)

	  } else if (current_block == "F") {

  # Block F ---------------------------------------------------------

  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "exp_work_others",
        "exp_building_repair",
        "exp_other_fixed_assets",
        "exp_operating",
        "exp_non_operating",
        "insurance",
        "rent_machinery",
        "exp_total",
        "rent_buildings",
        "rent_land",
        "interests_paid",
        "purc_val_goods_resold",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 166)
    ) %>%
    select(-X1)

	  } else if (current_block == "G") {

  # Block G ---------------------------------------------------------

  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "income_services",
        "var_semi_fin",
        "val_electricity_sold",
        "val_own_construction",
        "net_balance_goods_resold",
        "rent_income_machinery",
        "total_receipts",
        "rent_income_building",
        "rent_income_land",
        "interest_income",
        "sale_val_goods_resold",
	"total_subsidies",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 166)
    ) %>%
    select(-X1)

	  } else if (current_block == "H") {

  # Block H ---------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_consumed",
        "purchase_val",
        "unit_rate",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 51, 66, 75)
      ) %>%
    select(-X1)
  
	  } else if (current_block == "I") {

  # BLOCK I -----------------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_consumed",
        "purchase_val",
        "unit_rate",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 51, 66, 75)
    ) %>%
    select(-X1)

	  } else if (current_block == "J") {

  # BLOCK J -----------------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_made",
        "qty_sold",
        "gross_sale_val",
        "excise_duty",
        "sales_tax",
        "others",
        "total",
        "per_unit_sale_val",
        "ex_factory_val",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 55, 67, 79, 91, 103, 115, 130, 142, 151)      
    ) %>%
    select(-X1)

	  } else {
		  stop("Something is wrong with the 2008-2009 year.")

	  } 

	  } else if (panel_structure == "2009") {

#################################################################
##                   PREPARE 2009-10 BLOCKS                    ##
#################################################################

  # Block A ---------------------------------------------------------

		  if (current_block == "A") {
  blk_tbl <- 
	  blk_raw %>%
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "scheme",
        "nic4code",
        "nic5code",
        "rural_urban",
        "sro",
        "no_units",
        "unit_status",
        "mw_days",
        "nw_days",
        "wdays",
        "production_cost",
	"export_share",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 55, 58, 67)
    ) %>%
    select(-X1)

	  } else if (current_block == "B") {

  # Block B ---------------------------------------------------------

  blk_tbl <-
	  blk_raw %>%
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "type_organisation",
        "type_ownership",
	"no_of_units",
        "initial_production",
        "accounting_year_from",
        "accounting_year_to",
        "months_of_operation",
        "ac_system",
        "asi_floppy",
	"investment_in_p_m",
	"iso",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 16, 20, 24, 33, 42, 44, 45, 46, 47, 48, 57)
      )%>%
    select(-X1)

	  } else if (current_block == "C") {

  # Block C ---------------------------------------------------------

  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "sno",
        "opening_gross",
        "addition_reval",
        "addition_add",
        "deduction_adj",
        "closing_gross",
        "deprec_begin_year",
        "deprec_during_year",
	"deprec_adj_during_year",
        "deprec_end_year",
        "opening_net",
        "closing_net",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 27, 39, 51, 63, 75, 87, 99, 111, 123, 135, 147, 156)
    ) %>%
    select(-X1)

	  } else if (current_block == "D") {

  # Block D ---------------------------------------------------------

  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block",
        "sno",
        "w_cap_opening",
        "w_cap_closing",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 27, 39, 48)
    ) %>%
    select(-X1)

	  } else if (current_block == "E") {

  # Block E ---------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "man_days",
        "non_man_days",
        "tot_days",
        "avg_person_worked",
        "days_paid",
        "wages",
        "bonus",
        "contrib_provident_fund",
        "staff_welfare_exp",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 23, 31, 41, 49, 59, 71, 85, 99, 113, 122)
    ) %>%
    select(-X1)

	  } else if (current_block == "F") {

  # Block F ---------------------------------------------------------

  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "exp_work_others",
        "exp_building_repair",
        "exp_other_fixed_assets",
        "exp_operating",
        "exp_non_operating",
        "insurance",
        "rent_machinery",
        "exp_total",
        "rent_buildings",
        "rent_land",
        "interests_paid",
        "purc_val_goods_resold",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 166)
    ) %>%
    select(-X1)

	  } else if (current_block == "G") {

  # Block G ---------------------------------------------------------

  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "income_services",
        "var_semi_fin",
        "val_electricity_sold",
        "val_own_construction",
        "net_balance_goods_resold",
        "rent_income_machinery",
        "total_receipts",
        "rent_income_building",
        "rent_income_land",
        "interest_income",
        "sale_val_goods_resold",
	"total_subsidies",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 25, 37, 49, 61, 73, 85, 97, 109, 121, 133, 145, 157, 166)
    ) %>%
    select(-X1)

	  } else if (current_block == "H") {

  # Block H ---------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_consumed",
        "purchase_val",
        "unit_rate",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 54, 69, 78)
      ) %>%
    select(-X1)
  
	  } else if (current_block == "I") {

  # BLOCK I -----------------------------------------------------------------
  
  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_consumed",
        "purchase_val",
        "unit_rate",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 54, 69, 78)
    ) %>%
    select(-X1)

	  } else if (current_block == "J") {

  # BLOCK J -----------------------------------------------------------------

  # Separate into correct columns
  blk_tbl <-
    blk_raw %>% 
    separate(
      col = X1,
      into = c(
        "year",
        "factory_id",
        "block", 
        "sno",
        "item_code",
        "qty_unit",
        "qty_made",
        "qty_sold",
        "gross_sale_val",
        "excise_duty",
        "sales_tax",
        "others",
        "total",
        "per_unit_sale_val",
        "ex_factory_val",
        "multiplier"
      ),
      remove = FALSE,
      sep = c(4, 12, 13, 15, 20, 23, 39, 55, 70, 85, 100, 115, 130, 145, 160, 169)
    ) %>%
    select(-X1)

	  } else {
		  stop("Something is wrong with the 2009-2010 year.")
	  } 



	  }

  # Trim whitespace
  blk_tbl <- 
	  blk_tbl %>% map_dfr(str_trim, side = "left")

  # A few bars also have some extra white space on the right side.
  # Fx (only) the 2006 data has extra white space on the right in 
  # the factory_id variable.
  blk_tbl <- 
	  blk_tbl %>% map_dfr(str_trim, side = "right")

	  return(blk_tbl)

  }

  # Read all the 2000-01 to 2007-08 blocks at the same time

  # To do this, I create a small helper function. The helper function
  # just aids in supplying the block letter to the cleaning function.
  # (which is probrbly a bad name, since it just organises and names
  # the vars). I then "map" (or apply) the cleaning function to each
  # row in the list column, that is, each raw block. This gives me 
  # a new asi_tbl where I replace the one with the raw data with one
  # with the organised data.
  
  # I also just need to fix the year in each data frame. Just as with the later data,
	# data for financial year April-March is listed as March year. I want it to be April.
	# Fx currently April 2010-March 2011 = 2011. I want it to be 2010.

helper_function <- function(block_tbl, help_block, help_panel_structure) {

	# organize variables
		cleaned_block_tbl <- 
			clean_blocks(
			asi_tbl = block_tbl,
			current_block = help_block,
			panel_structure = help_panel_structure
			) 

	# fix the year
cleaned_block_tbl <- 
	cleaned_block_tbl %>%
	mutate(year = as.numeric(year) - 1)

		return(cleaned_block_tbl)
}
	
# Apply the functions. I first apply the function to the correct years separately, the 
# bind them together (stack).

block_letters <- 
	c(
		"A",
		"B",
		"C",
		"D",
		"E",
		"F",
		"G",
		"H", 
		"I",
		"J"
		)

# Because of memory issues, I save each individual "panel_structure" part after cleaning it. This 
# is then loaded in the real "cleaning_asi_blocks" function (see the plan).

asi_2000_2007_tbl <- 
	asi_tbl %>%
	filter(year %in% 2000:2007) %>%
	filter(block %in% block_letters) %>%
	mutate(
		data = map2(.x = data, .y = block, .f = helper_function, help_panel_structure = "2000")
		) 

saveRDS(
	asi_2000_2007_tbl,
	early_2000_2007_file_path
	)



rm("asi_2000_2007_tbl")

asi_2008_tbl <- 
	asi_tbl %>%
	filter(year == 2008) %>%
	filter(block %in% block_letters) %>%
	mutate(
		data = map2(.x = data, .y = block, .f = helper_function, help_panel_structure = "2008")
		) 

saveRDS(
	asi_2008_tbl, 
	early_2008_file_path 
	)

rm("asi_2008_tbl")

asi_2009_tbl <- 
	asi_tbl %>%
	filter(year == 2009) %>%
	filter(block %in% block_letters) %>%
	mutate(
		data = map2(.x = data, .y = block, .f = helper_function, help_panel_structure = "2009")
		) 

saveRDS(
	asi_2009_tbl,
	early_2009_file_path 
	)

rm("asi_2009_tbl")

# TODO:
return()

}

