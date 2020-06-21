# Read all
library(here)
library(tidyverse)
library(vroom)


# First issue: the data is provided in .TXT format without column headers.
# Instead, a .xlxs file is provided for each year, which lists the column names.



#################################################################
##                        1: READ FILES                        ##
#################################################################

# Get file paths for each individual block (2010-11 to 2015-16)
asi_files <- list.files(
    path = here("data/external/asi/asi_2010_2016"),
    pattern = ".TXT",
    full.names = TRUE,
    recursive = TRUE
  )

# Create index information for raw files
asi_tbl <- tibble(file_path = asi_files) %>%
	mutate(
		block_year = str_extract(
			file_path,
			pattern = "[A-J]([0-9]{4}|[0-9]{2})"
			),
		year = str_extract(
			block_year,
			pattern = "([0-9]{4}|[0-9]{2})"
			),
		block = str_extract(
			block_year,
			pattern = "[A-J]"
			)
		) %>%
	select(-block_year)

# Make all "year" entries into YYYY format (some are in YY).
asi_tbl <- asi_tbl %>%
	mutate(
		year = as.numeric(year),
		year = ifelse(year < 20, year + 2000, year)
		)

# Read all blocks to list column
asi_tbl <- asi_tbl %>%
	mutate(
		data = map(file_path, read_csv, col_names = FALSE)
		)


##################################################################
##                   PREPARE 2010-2011 BLOCKS                   ##
##################################################################

# BLOCK A -----------------------------------------------------------------
blk_A11_raw <- asi_tbl %>%
	filter(year == 2011 & block == "A") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_A11_tbl <- blk_A11_raw %>% 
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
		sep = c(4, 12, 13, 14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 57, 60, 69)
		) %>%
	select(-X1)

# Trim whitespace
blk_A11_tbl <- blk_A11_tbl %>% map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK B -----------------------------------------------------------------
blk_B11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "B") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_B11_tbl <-
	blk_B11_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"type_organisation",
			"type_ownership",
			"initial_production",
			"accounting_year_from",
			"accounting_year_to",
			"months_of_operation",
			"ac_system",
			"asi_floppy",
			"iso",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 15, 16, 20, 29, 38, 40, 41, 42, 43, 52),
		) %>%
	select(-X1)

# Trim whitespace
blk_B11_tbl <- blk_B11_tbl %>%
	map_dfr(str_trim, side = "both")

# -------------------------------------------------------------------------

# BLOCK C -----------------------------------------------------------------
blk_C11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "C") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_C11_tbl <-
	blk_C11_raw %>% 
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
			"deprec_adjustment",
			"deprec_end_year",
			"opening_net",
			"closing_net",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 15, 29, 43, 57, 71, 85, 99, 113, 127, 141, 155, 169, 178)
		) %>%
	select(-X1)

# Trim whitespace
blk_C11_tbl <- blk_C11_tbl %>%
	map_dfr(str_trim, side = "left")

# BLOCK D -----------------------------------------------------------------
blk_D11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "D") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_D11_tbl <-
	blk_D11_raw %>% 
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
		sep = c(4, 12, 13, 15, 29, 43, 52)
		) %>%
	select(-X1)

# Trim whitespace
blk_D11_tbl <- blk_D11_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK E -----------------------------------------------------------------
blk_E11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "E") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_E11_tbl <-
	blk_E11_raw %>% 
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
		sep = c(4, 12, 13, 15, 23, 31, 41, 49, 59, 73, 87, 101, 115, 124)
		) %>%
	select(-X1)

# Trim whitespace
blk_E11_tbl <- blk_E11_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK F -----------------------------------------------------------------
blk_F11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "F") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_F11_tbl <-
	blk_F11_raw %>% 
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
		sep = c(4, 12, 13, 27, 41, 55, 69, 83, 97, 111, 125, 139, 153, 167, 181, 190)
		) %>%
	select(-X1)

# Trim whitespace
blk_F11_tbl <- blk_F11_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK G -----------------------------------------------------------------
blk_G11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "G") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_G11_tbl <-
	blk_G11_raw %>% 
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
		sep = c(4, 12, 13, 27, 41, 55, 69, 83, 97, 111, 125, 139, 153, 167, 181, 190)
		) %>%
	select(-X1)

# Trim whitespace
blk_G11_tbl <- blk_G11_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK H -----------------------------------------------------------------
blk_H11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "H") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_H11_tbl <-
	blk_H11_raw %>% 
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
		sep = c(4, 12, 13, 15, 22, 25, 39, 53, 67, 76)
		) %>%
	select(-X1)

# Trim whitespace
blk_H11_tbl <- blk_H11_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK I -----------------------------------------------------------------
blk_I11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "I") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_I11_tbl <-
	blk_I11_raw %>% 
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
		sep = c(4, 12, 13, 15, 22, 25, 39, 53, 67, 76)
		) %>%
	select(-X1)

# Trim whitespace
blk_I11_tbl <- blk_I11_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK J -----------------------------------------------------------------
blk_J11_raw <- 
	asi_tbl %>%
	filter(year == 2011 & block == "J") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_J11_tbl <-
	blk_J11_raw %>% 
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
		sep = c(4, 12, 13, 15, 22, 25, 39, 53, 67, 81, 95, 109, 123, 137, 151, 160)
		) %>%
	select(-X1)

# Trim whitespace
blk_J11_tbl <- blk_J11_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

##################################################################
##                   PREPARE 2011-2012 BLOCKS                   ##
##################################################################

# BLOCK A -----------------------------------------------------------------
blk_A12_raw <- asi_tbl %>%
	filter(year == 2012 & block == "A") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_A12_tbl <- blk_A12_raw %>% 
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
		sep = c(4, 12, 13, 14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 57, 60, 69)
		) %>%
	select(-X1)

# Trim whitespace
blk_A12_tbl <- blk_A12_tbl %>% map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK B -----------------------------------------------------------------
blk_B12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "B") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_B12_tbl <-
	blk_B12_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"type_organisation",
			"type_ownership",
			"initial_production",
			"accounting_year_from",
			"accounting_year_to",
			"months_of_operation",
			"ac_system",
			"asi_floppy",
			"iso",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 15, 16, 20, 29, 38, 40, 41, 42, 43, 52),
		) %>%
	select(-X1)

# Trim whitespace
blk_B12_tbl <- blk_B12_tbl %>%
	map_dfr(str_trim, side = "both")

# -------------------------------------------------------------------------

# BLOCK C -----------------------------------------------------------------
blk_C12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "C") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_C12_tbl <-
	blk_C12_raw %>% 
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
			"deprec_adjustment",
			"deprec_end_year",
			"opening_net",
			"closing_net",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 15, 29, 43, 57, 71, 85, 99, 113, 127, 141, 155, 169, 178)
		) %>%
	select(-X1)

# Trim whitespace
blk_C12_tbl <- blk_C12_tbl %>%
	map_dfr(str_trim, side = "left")

# BLOCK D -----------------------------------------------------------------
blk_D12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "D") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_D12_tbl <-
	blk_D12_raw %>% 
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
		sep = c(4, 12, 13, 15, 29, 43, 52)
		) %>%
	select(-X1)

# Trim whitespace
blk_D12_tbl <- blk_D12_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK E -----------------------------------------------------------------
blk_E12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "E") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_E12_tbl <-
	blk_E12_raw %>% 
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
		sep = c(4, 12, 13, 15, 23, 31, 41, 49, 59, 73, 87, 101, 115, 124)
		) %>%
	select(-X1)

# Trim whitespace
blk_E12_tbl <- blk_E12_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK F -----------------------------------------------------------------
blk_F12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "F") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_F12_tbl <-
	blk_F12_raw %>% 
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
		sep = c(4, 12, 13, 27, 41, 55, 69, 83, 97, 111, 125, 139, 153, 167, 181, 190)
		) %>%
	select(-X1)

# Trim whitespace
blk_F12_tbl <- blk_F12_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK G -----------------------------------------------------------------
blk_G12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "G") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_G12_tbl <-
	blk_G12_raw %>% 
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
		sep = c(4, 12, 13, 27, 41, 55, 69, 83, 97, 111, 125, 139, 153, 167, 181, 190)
		) %>%
	select(-X1)

# Trim whitespace
blk_G12_tbl <- blk_G12_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK H -----------------------------------------------------------------
blk_H12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "H") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_H12_tbl <-
	blk_H12_raw %>% 
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
		sep = c(4, 12, 13, 15, 22, 25, 39, 53, 67, 76)
		) %>%
	select(-X1)

# Trim whitespace
blk_H12_tbl <- blk_H12_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK I -----------------------------------------------------------------
blk_I12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "I") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_I12_tbl <-
	blk_I12_raw %>% 
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
		sep = c(4, 12, 13, 15, 22, 25, 39, 53, 67, 76)
		) %>%
	select(-X1)

# Trim whitespace
blk_I12_tbl <- blk_I12_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK J -----------------------------------------------------------------
blk_J12_raw <- 
	asi_tbl %>%
	filter(year == 2012 & block == "J") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_J12_tbl <-
	blk_J12_raw %>% 
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
		sep = c(4, 12, 13, 15, 22, 25, 39, 53, 67, 81, 95, 109, 123, 137, 151, 160)
		) %>%
	select(-X1)

# Trim whitespace
blk_J12_tbl <- blk_J12_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

##################################################################
##                   PREPARE 2012-2013 BLOCKS                   ##
##################################################################

# BLOCK A -----------------------------------------------------------------
blk_A13_raw <- asi_tbl %>%
	filter(year == 2013 & block == "A") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_A13_tbl <-
	blk_A13_raw %>% 
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
		sep = c(4, 12, 13, 14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 57, 60, 73)
		) %>%
	select(-X1)

# Trim whitespace
blk_A13_tbl <-
	blk_A13_tbl %>% map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK B -----------------------------------------------------------------
blk_B13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "B") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_B13_tbl <-
	blk_B13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"type_organisation",
			"type_ownership",
			"initial_production",
			"accounting_year_from",
			"accounting_year_to",
			"months_of_operation",
			"ac_system",
			"asi_floppy",
			"iso",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 21, 25, 34, 43, 45, 46, 47, 48, 61)
		) %>%
	select(-X1)

# Trim whitespace
blk_B13_tbl <-
	blk_B13_tbl %>%
	map_dfr(str_trim, side = "both")

# -------------------------------------------------------------------------

# BLOCK C -----------------------------------------------------------------
blk_C13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "C") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_C13_tbl <-
	blk_C13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"opening_gross",
			"addition_reval",
			"addition_add",
			"deduction_adj",
			"closing_gross",
			"deprec_begin_year",
			"deprec_during_year",
			"deprec_adjustment",
			"deprec_end_year",
			"opening_net",
			"closing_net",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 62, 76, 90, 104, 118, 132, 146, 160, 174, 187)
		) %>%
	select(-X1)

# Trim whitespace
blk_C13_tbl <-
	blk_C13_tbl %>%
	map_dfr(str_trim, side = "left")

# BLOCK D -----------------------------------------------------------------
blk_D13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "D") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_D13_tbl <-
	blk_D13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"w_cap_opening",
			"w_cap_closing",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 61)
		) %>%
	select(-X1)

# Trim whitespace
blk_D13_tbl <-
	blk_D13_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK E -----------------------------------------------------------------
blk_E13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "E") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_E13_tbl <-
	blk_E13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
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
		sep = c(4, 12, 13, 18, 20, 28, 36, 46, 54, 64, 78, 92, 106, 120, 133)
		) %>%
	select(-X1)

# Trim whitespace
blk_E13_tbl <-
	blk_E13_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK F -----------------------------------------------------------------
blk_F13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "F") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_F13_tbl <-
	blk_F13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
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
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_F13_tbl <-
	blk_F13_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK G -----------------------------------------------------------------
blk_G13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "G") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_G13_tbl <-
	blk_G13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
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
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_G13_tbl <-
	blk_G13_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK H -----------------------------------------------------------------
blk_H13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "H") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_H13_tbl <-
	blk_H13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 27, 30, 44, 58, 72, 85)
		) %>%
	select(-X1)

# Trim whitespace
blk_H13_tbl <-
	blk_H13_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK I -----------------------------------------------------------------
blk_I13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "I") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_I13_tbl <-
	blk_I13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 27, 30, 44, 58, 72, 85)
		) %>%
	select(-X1)

# Trim whitespace
blk_I13_tbl <- 
	blk_I13_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK J -----------------------------------------------------------------
blk_J13_raw <- 
	asi_tbl %>%
	filter(year == 2013 & block == "J") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_J13_tbl <-
	blk_J13_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
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
		sep = c(4, 12, 13, 18, 20, 27, 30, 44, 58, 72, 86, 100, 114, 128, 142, 156, 169)
		) %>%
	select(-X1)

# Trim whitespace
blk_J13_tbl <-
	blk_J13_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

##################################################################
##                   PREPARE 2013-2014 BLOCKS                   ##
##################################################################

# BLOCK A -----------------------------------------------------------------
blk_A14_raw <- asi_tbl %>%
	filter(year == 2014 & block == "A") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_A14_tbl <-
	blk_A14_raw %>% 
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
		sep = c(4, 12, 13, 14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 57, 60, 73)
		) %>%
	select(-X1)

# Trim whitespace
blk_A14_tbl <-
	blk_A14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK B -----------------------------------------------------------------
blk_B14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "B") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_B14_tbl <-
	blk_B14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"type_organisation",
			"type_ownership",
			"initial_production",
			"accounting_year_from",
			"accounting_year_to",
			"months_of_operation",
			"ac_system",
			"asi_floppy",
			"iso",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 21, 25, 34, 43, 45, 46, 47, 48, 61)
		) %>%
	select(-X1)

# Trim whitespace
blk_B14_tbl <-
	blk_B14_tbl %>%
	map_dfr(str_trim, side = "both")

# -------------------------------------------------------------------------

# BLOCK C -----------------------------------------------------------------
blk_C14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "C") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_C14_tbl <-
	blk_C14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"opening_gross",
			"addition_reval",
			"addition_add",
			"deduction_adj",
			"closing_gross",
			"deprec_begin_year",
			"deprec_during_year",
			"deprec_adjustment",
			"deprec_end_year",
			"opening_net",
			"closing_net",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 62, 76, 90, 104, 118, 132, 146, 160, 174, 187)
		) %>%
	select(-X1)

# Trim whitespace
blk_C14_tbl <-
	blk_C14_tbl %>%
	map_dfr(str_trim, side = "left")

# BLOCK D -----------------------------------------------------------------
blk_D14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "D") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_D14_tbl <-
	blk_D14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"w_cap_opening",
			"w_cap_closing",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 61)
		) %>%
	select(-X1)

# Trim whitespace
blk_D14_tbl <-
	blk_D14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK E -----------------------------------------------------------------
blk_E14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "E") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_E14_tbl <-
	blk_E14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
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
		sep = c(4, 12, 13, 18, 20, 28, 36, 46, 54, 64, 78, 92, 106, 120, 133)
		) %>%
	select(-X1)

# Trim whitespace
blk_E14_tbl <-
	blk_E14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK F -----------------------------------------------------------------
blk_F14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "F") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_F14_tbl <-
	blk_F14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
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
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_F14_tbl <-
	blk_F14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK G -----------------------------------------------------------------
blk_G14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "G") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_G14_tbl <-
	blk_G14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
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
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_G14_tbl <-
	blk_G14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK H -----------------------------------------------------------------
blk_H14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "H") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_H14_tbl <-
	blk_H14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 21, 28, 31, 45, 59, 73, 86)
		) %>%
	select(-X1)

# Trim whitespace
blk_H14_tbl <-
	blk_H14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK I -----------------------------------------------------------------
blk_I14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "I") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_I14_tbl <-
	blk_I14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 27, 30, 44, 58, 72, 85)
		) %>%
	select(-X1)

# Trim whitespace
blk_I14_tbl <- 
	blk_I14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK J -----------------------------------------------------------------
blk_J14_raw <- 
	asi_tbl %>%
	filter(year == 2014 & block == "J") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_J14_tbl <-
	blk_J14_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
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
		sep = c(4, 12, 13, 18, 20, 27, 30, 44, 58, 72, 86, 100, 114, 128, 142, 156, 169)
		) %>%
	select(-X1)

# Trim whitespace
blk_J14_tbl <-
	blk_J14_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

##################################################################
##                   PREPARE 2014-2015 BLOCKS                   ##
##################################################################

# BLOCK A -----------------------------------------------------------------
blk_A15_raw <- asi_tbl %>%
	filter(year == 2015 & block == "A") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_A15_tbl <-
	blk_A15_raw %>% 
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
		sep = c(4, 12, 13, 14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 57, 60, 73)
		) %>%
	select(-X1)

# Trim whitespace
blk_A15_tbl <-
	blk_A15_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK B -----------------------------------------------------------------
blk_B15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "B") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_B15_tbl <-
	blk_B15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"type_organisation",
			"type_ownership",
			"initial_production",
			"accounting_year_from",
			"accounting_year_to",
			"months_of_operation",
			"ac_system",
			"asi_floppy",
			"iso",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 21, 25, 34, 43, 45, 46, 47, 48, 61)
		) %>%
	select(-X1)

# Trim whitespace
blk_B15_tbl <-
	blk_B15_tbl %>%
	map_dfr(str_trim, side = "both")

# -------------------------------------------------------------------------

# BLOCK C -----------------------------------------------------------------
blk_C15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "C") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_C15_tbl <-
	blk_C15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"opening_gross",
			"addition_reval",
			"addition_add",
			"deduction_adj",
			"closing_gross",
			"deprec_begin_year",
			"deprec_during_year",
			"deprec_adjustment",
			"deprec_end_year",
			"opening_net",
			"closing_net",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 62, 76, 90, 104, 118, 132, 146, 160, 174, 187)
		) %>%
	select(-X1)

# Trim whitespace
blk_C15_tbl <-
	blk_C15_tbl %>%
	map_dfr(str_trim, side = "left")

# BLOCK D -----------------------------------------------------------------
blk_D15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "D") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_D15_tbl <-
	blk_D15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"w_cap_opening",
			"w_cap_closing",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 61)
		) %>%
	select(-X1)

# Trim whitespace
blk_D15_tbl <-
	blk_D15_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK E -----------------------------------------------------------------
blk_E15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "E") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_E15_tbl <-
	blk_E15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
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
		sep = c(4, 12, 13, 18, 20, 28, 36, 46, 54, 64, 78, 92, 106, 120, 133)
		) %>%
	select(-X1)

# Trim whitespace
blk_E15_tbl <-
	blk_E15_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK F -----------------------------------------------------------------
blk_F15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "F") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_F15_tbl <-
	blk_F15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
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
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_F15_tbl <-
	blk_F15_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK G -----------------------------------------------------------------
blk_G15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "G") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_G15_tbl <-
	blk_G15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
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
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_G15_tbl <-
	blk_G15_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK H -----------------------------------------------------------------
blk_H15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "H") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_H15_tbl <-
	blk_H15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 21, 28, 31, 45, 59, 73, 86)
		) %>%
	select(-X1)

# Trim whitespace
blk_H15_tbl <-
	blk_H15_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK I -----------------------------------------------------------------
blk_I15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "I") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_I15_tbl <-
	blk_I15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 27, 30, 44, 58, 72, 85)
		) %>%
	select(-X1)

# Trim whitespace
blk_I15_tbl <- 
	blk_I15_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK J -----------------------------------------------------------------
blk_J15_raw <- 
	asi_tbl %>%
	filter(year == 2015 & block == "J") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_J15_tbl <-
	blk_J15_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
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
		sep = c(4, 12, 13, 18, 21, 28, 30, 44, 58, 72, 86, 100, 114, 128, 142, 156, 169)
		) %>%
	select(-X1)

# Trim whitespace
blk_J15_tbl <-
	blk_J15_tbl %>%
	map_dfr(str_trim, side = "both")

# Fix "year" variable: for some reason, year is listed as "15" instead of 2015. 
blk_J15_tbl <- blk_J15_tbl %>%
	mutate(year = ifelse(year == "15", "2015", year))

# -------------------------------------------------------------------------

##################################################################
##                   PREPARE 2015-2016 BLOCKS                   ##
##################################################################

# BLOCK A -----------------------------------------------------------------
blk_A16_raw <- asi_tbl %>%
	filter(year == 2016 & block == "A") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_A16_tbl <-
	blk_A16_raw %>% 
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
		sep = c(4, 12, 13, 14, 18, 23, 24, 29, 32, 34, 37, 40, 43, 57, 60, 73)
		) %>%
	select(-X1)

# Trim whitespace
blk_A16_tbl <-
	blk_A16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK B -----------------------------------------------------------------
# Note: There is a problem in the source data on this block. Accounting year only
# listed as a single digit. 4 for from and 3 for to. This matches April to March
# in the other years. However, it is mis-specified in the supporting docs as being
# 9 characters long, when it is in fact only 6. I've changed it.
blk_B16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "B") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_B16_tbl <-
	blk_B16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"type_organisation",
			"corporate_id",
			"initial_production",
			"accounting_year_from",
			"accounting_year_to",
			"months_of_operation",
			"capital_share_foreign",
			"contain_r_and_d",
			"iso",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 41, 45, 51, 57, 59, 60, 61, 62, 75)
		) %>%
	select(-X1)

# Trim whitespace
blk_B16_tbl <-
	blk_B16_tbl %>%
	map_dfr(str_trim, side = "both")

# -------------------------------------------------------------------------

# BLOCK C -----------------------------------------------------------------
blk_C16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "C") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_C16_tbl <-
	blk_C16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"opening_gross",
			"addition_reval",
			"addition_add",
			"deduction_adj",
			"closing_gross",
			"deprec_begin_year",
			"deprec_during_year",
			"deprec_adjustment",
			"deprec_end_year",
			"opening_net",
			"closing_net",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 62, 76, 90, 104, 118, 132, 146, 160, 174, 187)
		) %>%
	select(-X1)

# Trim whitespace
blk_C16_tbl <-
	blk_C16_tbl %>%
	map_dfr(str_trim, side = "left")

# BLOCK D -----------------------------------------------------------------
blk_D16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "D") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_D16_tbl <-
	blk_D16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block",
			"nic5code",
			"sno",
			"w_cap_opening",
			"w_cap_closing",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 34, 48, 61)
		) %>%
	select(-X1)

# Trim whitespace
blk_D16_tbl <-
	blk_D16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK E -----------------------------------------------------------------
blk_E16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "E") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_E16_tbl <-
	blk_E16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
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
		sep = c(4, 12, 13, 18, 20, 28, 36, 46, 54, 64, 78, 92, 106, 120, 133)
		) %>%
	select(-X1)

# Trim whitespace
blk_E16_tbl <-
	blk_E16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK F -----------------------------------------------------------------
blk_F16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "F") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_F16_tbl <-
	blk_F16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
			"exp_work_others",
			"exp_building_repair",
			"exp_other_fixed_assets",
			"exp_operating",
			"exp_construction_materials",
			"insurance",
			"rent_machinery",
			"exp_r_and_d",
			"rent_buildings",
			"rent_land",
			"interests_paid",
			"purc_val_goods_resold",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_F16_tbl <-
	blk_F16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK G -----------------------------------------------------------------
blk_G16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "G") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_G16_tbl <-
	blk_G16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
			"receipts_man_services",
			"receipts_non_man_services",
			"val_electricity_sold",
			"val_own_construction",
			"net_balance_goods_resold",
			"rent_income_machinery",
			"var_semi_fin",
			"rent_income_building",
			"rent_income_land",
			"interest_income",
			"sale_val_goods_resold",
			"other_production_subsidies",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 32, 46, 60, 74, 88, 102, 116, 130, 144, 158, 172, 186, 199)
		) %>%
	select(-X1)

# Trim whitespace
blk_G16_tbl <-
	blk_G16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK H -----------------------------------------------------------------
blk_H16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "H") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_H16_tbl <-
	blk_H16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5code",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 21, 28, 31, 45, 59, 73, 86)
		) %>%
	select(-X1)

# Trim whitespace
blk_H16_tbl <-
	blk_H16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK I -----------------------------------------------------------------
blk_I16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "I") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_I16_tbl <-
	blk_I16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
			"sno",
			"item_code",
			"qty_unit",
			"qty_consumed",
			"purchase_val",
			"unit_rate",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 20, 27, 30, 44, 58, 72, 85)
		) %>%
	select(-X1)

# Trim whitespace
blk_I16_tbl <- 
	blk_I16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------

# BLOCK J -----------------------------------------------------------------
blk_J16_raw <- 
	asi_tbl %>%
	filter(year == 2016 & block == "J") %>%
	select(data) %>%
	unnest(data)

# Separate into correct columns
blk_J16_tbl <-
	blk_J16_raw %>% 
	separate(
		col = X1,
		into = c(
			"year",
			"factory_id",
			"block", 
			"nic5digit",
			"sno",
			"item_code",
			"qty_unit",
			"qty_made",
			"qty_sold",
			"gross_sale_val",
			"excise_duty",
			"sales_tax",
			"others",
			"subsidy",
			"per_unit_sale_val",
			"ex_factory_val",
			"multiplier"
			),
		remove = FALSE,
		sep = c(4, 12, 13, 18, 21, 28, 30, 44, 58, 72, 86, 100, 114, 128, 142, 156, 169)
		) %>%
	select(-X1)

# Trim whitespace
blk_J16_tbl <-
	blk_J16_tbl %>%
	map_dfr(str_trim, side = "left")

# -------------------------------------------------------------------------
