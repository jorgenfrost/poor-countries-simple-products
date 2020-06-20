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
			"purch_val_goods_sold",
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
			"purch_val_goods_sold",
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

