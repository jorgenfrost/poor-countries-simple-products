#' This functions creates a a table that contains plant-specific observations
#' (complexity and power shortages information not included). 
#' 
#' @param block_list List containing the merged ASI blocks.
#' 
#' @return A data frame (plant_tbl) containing plant specific observations.
#' @export

get_plant_tbl <- function(block_list) {

##---------------------------------------------------------------
##                              ID                              -
##---------------------------------------------------------------

block_a_tbl <- block_list$block_a_tbl

id_tbl <- 
	block_a_tbl %>%
	select(-block)

# Create state information ------------------------------------------------
# States change a bit during the period. A little bit of messy code is needed
# to fix it. Correct state names is supplied with the panel data. The 6th and
# 7th character in factory_id lists the state code. I use this code to assign
# state names.
id_tbl <-
	id_tbl %>%
	mutate(
		state_code = str_sub(
			string = factory_id,
			start = 6,
			end = 7
			) %>% as.numeric()
		) 

# Read state reference up to 2011-2012
state_names_99_to_12 <-
	read_excel(
	here("data/external/asi/asi_2010_2016/State Master 1998-99 to 2011-12.xls"),
	skip = 2
	) %>%
	clean_names(case = "snake") %>%
	mutate(codes = as.numeric(codes)) %>%
	rename(old_state_name = state_name)


# Read state reference from 2012-13 to 2015-16. 
state_names_13_to_16 <-
	read_excel(
	here("data/external/asi/asi_2010_2016/State Master 2012-13 onwards.xls"),
	skip = 2
	) %>%
	clean_names(case = "snake") %>%
	mutate(codes = as.numeric(codes)) %>%
	rename(new_state_name = state_name)

# Join method: create index on whether or not to join. If in 99-12 range, "old_states" 
# is equal to 1, 0 otherwise. If in 13-16 range, "new_states" is equal to 1, 0 if not.
# All entries in state_names_99_to_12 have "old_states" == 1, all entries in 
# state_names_13_to_16 have "new_states" == 1. Join based on this. Clean up mess after.

state_names_99_to_12 <- 
	state_names_99_to_12 %>%
	mutate(old_states = 1)

state_names_13_to_16 <- 
	state_names_13_to_16 %>%
	mutate(new_states = 1)

id_tbl <-
	id_tbl %>%
	mutate(
		old_states = ifelse(year %in% 1999:2012, 1, 0),
		new_states = ifelse(year %in% 2013:2016, 1, 0)
		)

id_tbl <- 
	id_tbl %>%
	left_join(state_names_99_to_12, by = c("state_code" = "codes", "old_states")) %>%
	left_join(state_names_13_to_16, by = c("state_code" = "codes", "new_states")) %>%
	mutate(
		state_name = case_when(
			old_states == 1 ~ old_state_name,
			new_states == 1 ~ new_state_name,
			TRUE ~ NA_character_
			)
		) %>%
	select(-c(old_states, new_states, old_state_name, new_state_name))

##---------------------------------------------------------------
##                        ELECTRICITY USE                       -
##---------------------------------------------------------------

block_h_tbl <- block_list$block_h_tbl


# Electricity generated (kwh): 
# 10-11: sno = 15, code = 9990400
# 11-12: sno = 15, code = 9990400
# 12-13: sno = 15, code = 9990400
# 13-14: sno = 15, code = 9990400
# 14-15: sno = 15, code = 9990400
# 15-16: sno = 15, code = 9990400

e_self_gen_tbl <-
	block_h_tbl %>%
	filter(item_code == "9990400") %>%
	select(
		year,
		factory_id,
		electricity_self_gen_kwh = qty_consumed
		) 

# Electricity purchased and consumed (kwh): 
# 10-11: sno = 16, code = 9990500
# 11-12: sno = 16, code = 9990500
# 12-13: sno = 16, code = 9990500
# 13-14: sno = 16, code = 9990500
# 14-15: sno = 16, code = 9990500
# 15-16: sno = 16, code = 9990500

e_purch_tbl <-
	block_h_tbl %>%
	filter(item_code == "9990500") %>%
	select(
		year,
		factory_id,
		electricity_purch_kwh = qty_consumed,
		electricity_purch_val = purchase_val
		)

# Join electricity tables
electricity_tbl <-
	left_join(e_purch_tbl, e_self_gen_tbl) %>%
	mutate( # If a plant does not make electricity it becomes NA. Make 0 instead.
		electricity_self_gen_kwh = ifelse(is.na(electricity_self_gen_kwh), 0, electricity_self_gen_kwh)
		)

##----------------------------------------------------------------
##                            REVENUES                           -
##----------------------------------------------------------------

block_j_tbl <- block_list$block_j_tbl

# 11-16: Total items = 9995000 or sno = 12

revenue_tbl <-
	block_j_tbl %>% 
	filter(item_code == "9995000") %>%
	select(
		year,
		factory_id,
		unadj_revenue = gross_sale_val
		)

##---------------------------------------------------------------
##                    ELECTRICITY INTENSITY                     -
##---------------------------------------------------------------

	# TODO:
	# - adjusted revenues should be used (otherwise, just because money is worth less over time it seems that firms are more electricity intensive).

##---------------------------------------------------------------
##                      EMPLOYEES AND WAGES                     -
##---------------------------------------------------------------

block_e_tbl <- block_list$block_e_tbl

# 2011-16: Total employees: sno = 9

labor_tbl <-
	block_e_tbl %>%
	filter(sno == 9) %>%
	select(
		year,
		factory_id,
		avg_total_employees = avg_person_worked,
		total_wages = wages
		)

##---------------------------------------------------------------
##                        GATHER TABLES                         -
##---------------------------------------------------------------

rm("block_list")

plant_tbl <- id_tbl %>%
	left_join(electricity_tbl) %>%
	left_join(labor_tbl) %>%
	left_join(revenue_tbl)

return(plant_tbl)
}
