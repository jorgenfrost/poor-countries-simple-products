#' This functions creates a a table that contains plant-specific observations
#' (complexity and power shortages information not included). It cleans and 
#' filters the observations based on a number of "flags".
#' 
#' @param block_list List containing the merged ASI blocks.
#' 
#' @return A data frame (plant_tbl) containing plant specific observations.
#' @export

#  For testing:
#   block_list_path <- here(file_in("data/temp/asi_blocks_clean.rds"))
#   wpi_index <- readd("wpi_tbl")
# 
   # TODO: !!! En del af cleaningen er kun lavet for nyere observation (2010 og senere.)
   # TODO: !!! INPUT CHANGE FLAGS er et problem. Ændringer kan være drevet af pris ændringer.
   # TODO: !!! FILTERING! Which flags excludes observation? How many flags?
   # TODO: Skal have lavet :FILTER
 # TODO: CONSIDERATIONS: should observations w/o electricity use be included?
 # TODO: CONSIDERATIONS: Make electricity consumption "missing" for all observations that have electricity consumed = 0.

 get_plant_tbl <- function(block_list_path, wpi_index) {
  
  #################################################################
  ##                        1: CLEAN DATA                        ##
  #################################################################
  
	block_list <- 
		readRDS(block_list_path)

  ##---------------------------------------------------------------
  ##                              ID                              -
  ##---------------------------------------------------------------
  
  block_a_tbl <- 
	  block_list %>%
	  filter(block == "A") %>%
	  select(data) %>% 
	  unnest(data)
  
  id_tbl <- 
    block_a_tbl %>%
    select(-block)
  
  # Create state information ------------------------------------------------
  # States change a bit during the period. A little bit of messy code is needed
  # to fix it. Correct state names is supplied with the panel data. The 6th and
  # 7th character in factory_id lists the state code. I use this code to assign
  # state names.

  # 140 factories from 2006 has a mistake in the format of their factory_id.
  # They have too few digits. That is, their format is something like "99321F"
  # wheres the proper format is "3319805F". I drop these observations.

  id_tbl <-
    id_tbl %>%
    mutate(
      state_code = str_sub(
        string = factory_id,
        start = 6,
        end = 7
      ) %>% as.numeric()
    ) %>% filter(!is.na(state_code))
  
  # Read state reference up to 2011-2012
  state_names_98_to_11 <-
    read_excel(
      here("data/external/asi/asi_2010_2015/State Master 1998-99 to 2011-12.xls"),
      skip = 2
    ) %>%
    clean_names(case = "snake") %>%
    mutate(codes = as.numeric(codes)) %>%
    rename(old_state_name = state_name)
  
  
  # Read state reference from 2012-13 to 2015-16. 
  state_names_12_to_15 <-
    read_excel(
      here("data/external/asi/asi_2010_2015/State Master 2012-13 onwards.xls"),
      skip = 2
    ) %>%
    clean_names(case = "snake") %>%
    mutate(codes = as.numeric(codes)) %>%
    rename(new_state_name = state_name)
  
  # Join method: create index on whether or not to join. If in 99-11 range,
  # "old_states" is equal to 1, 0 otherwise. If in 12-15 range, "new_states" is
  # qual to 1, 0 if not. All entries in state_names_99_to_11 have "old_states"
  # == 1, all entries in state_names_12_to_15 have "new_states" == 1. Join based
  # on this. Clean up mess after.
  
  state_names_98_to_11 <- 
    state_names_98_to_11 %>%
    mutate(old_states = 1)
  
  state_names_12_to_15 <- 
    state_names_12_to_15 %>%
    mutate(new_states = 1)
  
  id_tbl <-
    id_tbl %>%
    mutate(
      old_states = ifelse(year %in% 1998:2011, 1, 0),
      new_states = ifelse(year %in% 2012:2015, 1, 0)
    )
  
  id_tbl <- 
    id_tbl %>%
    left_join(
      state_names_98_to_11, by = c("state_code" = "codes", "old_states")
    ) %>%
    left_join(
      state_names_12_to_15, by = c("state_code" = "codes", "new_states")
    ) %>%
    mutate(
      state_name = case_when(
        old_states == 1 ~ old_state_name,
        new_states == 1 ~ new_state_name,
        TRUE ~ NA_character_
      )
    ) %>%
    select(-c(old_states, new_states, old_state_name, new_state_name))
 
# At the moment, states have uniform codes, but different names in the two periods.
# For many plots, I want to use the names for easier viewing. I use the final state
# scheme as labels.

state_lbl_tbl <-  
	state_names_12_to_15 %>%
	select(state_label = new_state_name, codes)

id_tbl <-
	id_tbl %>%
	left_join(state_lbl_tbl, by = c("state_code" = "codes")) 

  ##----------------------------------------------------------------
  ##                            REVENUES                           -
  ##----------------------------------------------------------------

 # This section adjust total plant revenue to 2004-5 prices and saves it 
 # in a seperate block.

  block_j_tbl <- 
	  block_list %>%
	  filter(block == "J") %>%
	  select(data) %>% 
	  unnest(data) 
  
  # 99-00: Total = 99950 or sno = 12
  # 00-01: Not supplied
  # 01-02: Total = 99950 or sno = 12
  # 02-03: Total = 99950 or sno = 12
  # 03-04: Total = 99950 or sno = 12
  # 04-05: Total = 99950 or sno = 12
  # 05-06: Total = 99950 or sno = 12
  # 06-07: Total = 99950 or sno = 12
  # 07-08: Total = 99950 or sno = 12
  # 08-09: Total = 99950 or sno = 12
  # 09-10: Total = 99950 or sno = 12
  # 10-15: Total items = 9995000 or sno = 12
  
  revenue_tbl <-
    block_j_tbl %>% 
    filter(item_code %in% c("9995000", "99950") & sno == 12) %>%
    select(
      year,
      factory_id,
      unadj_revenue = gross_sale_val
    ) %>%
    distinct() # remove duplicate entries

    # Check if factories report more than one "total items"
    revenue_reporting_tbl <-
	    revenue_tbl %>%
	  group_by(factory_id, year) %>%
	  summarize(n = n()) %>%
	  filter(n != 1)

  # 1 factory has two observations in a year. Since it is impossible to 
  # determine which is the correct observation, I drop it.
  revenue_tbl <- 
	  revenue_tbl %>%
	  anti_join(revenue_reporting_tbl)

  # Price-adjust revenues to financial year 2004-2005 prices using WPI.
  revenue_tbl <- 
    revenue_tbl %>%
    left_join(wpi_index, by = "year")
  
  revenue_tbl <- 
    revenue_tbl %>% 
    mutate(
      adj_revenue = unadj_revenue / (index_val / 100)
    ) %>%
    select(year, factory_id, unadj_revenue, adj_revenue)
  
  ##---------------------------------------------------------------
  ##                        ELECTRICITY USE                       -
  ##---------------------------------------------------------------
  
  block_h_tbl <- 
	  block_list %>%
	  filter(block == "H") %>%
	  select(data) %>% 
	  unnest(data) 
  
  # Get electricity consumed -----------------------------------------
  
  # Electricity generated (kwh): 
  # 99-00: sno = 10, code = 99904
  # 00-01: sno = 10, code = 99904
  # 01-02: sno = 10, code = 99904
  # 02-03: sno = 10, code = 99904
  # 03-04: sno = 15, code = 99904
  # 04-05: sno = 15, code = 99904
  # 05-06: sno = 15, code = 99904
  # 06-07: sno = 15, code = 99904
  # 07-08: sno = 15, code = 99904
  # 08-09: sno = 15, code = 99904
  # 09-10: sno = 15, code = 99904
  # 10-11: sno = 15, code = 9990400
  # 11-12: sno = 15, code = 9990400
  # 12-13: sno = 15, code = 9990400
  # 13-14: sno = 15, code = 9990400
  # 14-15: sno = 15, code = 9990400
  # 15-16: sno = 15, code = 9990400
  
  e_self_gen_tbl <-
    block_h_tbl %>%
    filter(item_code %in% c("9990400", "99904") & sno %in% c(10, 15)) %>%
    select(
      year,
      factory_id,
      electricity_self_gen_kwh = qty_consumed
    ) %>%
    distinct()
  
    # Check if factories report more than one "electricty self gen"
    e_self_gen_reporting_tbl <-
	    e_self_gen_tbl %>%
	  group_by(factory_id, year) %>%
	  summarize(n = n()) %>%
	  filter(n != 1)

  # 1 factoriy has two observations in a year. Since it is impossible to 
  # determine which is the correct observation, I drop it.
  e_self_gen_tbl <-
	  e_self_gen_tbl %>%
		  anti_join(e_self_gen_reporting_tbl)

  # Electricity purchased and consumed (kwh): 
  # 99-00: sno = 11, code = 99905
  # 00-01: sno = 11, code = 99905
  # 01-02: sno = 11, code = 99905
  # 02-03: sno = 11, code = 99905
  # 03-04: sno = 16, code = 99905
  # 04-05: sno = 16, code = 99905
  # 05-06: sno = 16, code = 99905
  # 06-07: sno = 16, code = 99905
  # 07-08: sno = 16, code = 99905
  # 08-09: sno = 16, code = 99905
  # 09-10: sno = 16, code = 99905
  # 10-11: sno = 16, code = 9990500
  # 11-12: sno = 16, code = 9990500
  # 12-13: sno = 16, code = 9990500
  # 13-14: sno = 16, code = 9990500
  # 14-15: sno = 16, code = 9990500
  # 15-16: sno = 16, code = 9990500
  
  e_purch_tbl <-
    block_h_tbl %>%
    filter(item_code %in% c("9990500", "99905") & sno %in% c(11, 16)) %>%
    select(
      year,
      factory_id,
      electricity_purch_kwh = qty_consumed,
      electricity_purch_val = purchase_val
    ) %>% 
    distinct()
  
    # Check if factories report more than one "electricity purchased"
    e_purch_reporting_tbl <-
	    e_purch_tbl %>%
	  group_by(factory_id, year) %>%
	  summarize(n = n()) %>%
	  filter(n != 1)

  # 1 factory has two observations in a year. Since it is impossible to 
  # determine which is the correct observation, I drop it.
	  e_purch_tbl <-
		  e_purch_tbl %>%
		  anti_join(e_purch_reporting_tbl)
  
  # Join electricity tables
  electricity_tbl <-
    full_join(e_purch_tbl, e_self_gen_tbl) %>%
    mutate( # If a plant does not make electricity it becomes NA. Make 0 instead.
      electricity_self_gen_kwh = ifelse(
        is.na(electricity_self_gen_kwh), 0, electricity_self_gen_kwh
      ),
      electricity_consumed_kwh = electricity_purch_kwh + electricity_self_gen_kwh
    )
  
  # Get electricity intensity -----------------------------------------
  # Electricity is measured in kWh, adj_revenue is measured in 2004 Indian Rs.
  # To get electricity intensity of revenues I divide electriciy consumed by 
  # adjusted revenues to get kWh/Rs.
  
  electricity_tbl <- 
	  revenue_tbl %>%
	  full_join(electricity_tbl, by = c("year", "factory_id")) %>%
    mutate(
      electricity_intensity = electricity_consumed_kwh / adj_revenue
    ) %>%
    select(-c(unadj_revenue, adj_revenue))
  
  ##----------------------------------------------------------------
  ##                    INTERMEDIATE INPUT USE                     -
  ##----------------------------------------------------------------
  
  # Indigenous inputs ------------------------------------------------
  # 99-00: total basic items = 99901, sno = 7, excludes electriciy purchased,
  # electriciy generated, non-basic, chemicals, packing items, fuel, coal, etc.
  # 00-01: total basic items = 99901, sno = 7
  # 01-02: total basic items = 99901, sno = 7
  # 02-03: total basic items = 99901, sno = 7
  # 03-04: total basic items = 99901, sno = 12
  # 04-05: total basic items = 99901, sno = 12
  # 05-06: total basic items = 99901, sno = 12
  # 06-07: total basic items = 99901, sno = 12
  # 07-08: total basic items = 99901, sno = 12
  # 08-09: total basic items = 99901, sno = 12
  # 09-10: total basic items = 99901, sno = 12
  # 10-15: total basic items = 9990100 excludes electricity, non-basic
  # chemicals packing items, electricity generated, fuel, coal, etc.
  
  basic_inputs_tbl <- 
    block_h_tbl %>%
    filter(item_code %in% c("9990100", "99901") & sno %in% c(7, 12)) %>%
    rename(unadj_basic_input_val = purchase_val) %>%
    select(year, factory_id, unadj_basic_input_val) %>%
    distinct()
  
    # Check if factories report more than one "total basic inputs"
    basic_inputs_reporting_tbl <-
	    basic_inputs_tbl %>%
	    group_by(factory_id, year) %>%
	    summarize(n = n()) %>%
	    filter(n != 1)

    # There aren't any observations in the reporting table, so
    # the following code shouldn't do anything. I keep it for
    # ease of reuse if something change.
    basic_inputs_tbl <-
	    basic_inputs_tbl %>%
	    anti_join(basic_inputs_reporting_tbl)
  
  # 99-00: total inputs = 99930, sno = 17
  # 00-01: total inputs = 99930, sno = 17
  # 01-02: total inputs = 99930, sno = 17
  # 02-03: total inputs = 999930, sno = 17
  # 03-04: total inputs = 99930, sno = 22
  # 04-05: total inputs = 99930, sno = 22
  # 05-06: total inputs = 99930, sno = 22
  # 06-07: total inputs = 99930, sno = 22
  # 07-08: total inputs = 99930, sno = 22
  # 08-09: total inputs = 99930, sno = 23 (from here on, an item called "total non basic" is added)
  # 09-10: total inputs = 99930, sno = 23
  # 10-15: total inputs = 9993000 (includes everything)

  total_inputs_tbl <-
    block_h_tbl %>%
    filter(item_code %in% c("9993000", "99930") & sno %in% c(17, 22, 23)) %>%
    rename(unadj_total_local_input_val = purchase_val) %>%
    select(year, factory_id, unadj_total_local_input_val) %>%
    distinct()

    # Check if factories report more than one "total inputs"
    total_inputs_reporting_tbl <-
	    total_inputs_tbl %>%
	    group_by(factory_id, year) %>%
	    summarize(n = n()) %>%
	    filter(n != 1)

    # 2 factories have two observations each. It is impossible to determine which
    # is correct. I drop them.
    total_inputs_tbl <-
	    total_inputs_tbl %>%
	    anti_join(total_inputs_reporting_tbl)
  
  # Imported inputs -------------------------------------------------
  block_i_tbl <- 
	  block_list %>%
	  filter(block == "I") %>%
	  select(data) %>% 
	  unnest(data) 
  
  # 99-00: total imports consumed = 99940, sno = 7
  # 00-01: total imports consumed = 99940, sno = 7
  # 01-02: total imports consumed = 99940, sno = 7
  # 02-03: total imports consumed = 99940, sno = 7
  # 03-04: total imports consumed = 99940, sno = 7
  # 04-05: total imports consumed = 99940, sno = 7
  # 05-06: total imports consumed = 99940, sno = 7
  # 06-07: total imports consumed = 99940, sno = 7
  # 07-08: total imports consumed = 99940, sno = 7
  # 08-09: total imports consumed = 99940, sno = 7
  # 09-10: total imports consumed = 99940, sno = 7
  # 10-15: total imports consumed = 9994000, sno = 7

  total_imports_tbl <- 
    block_i_tbl %>%
    filter(item_code %in% c("9994000", "99940") & sno %in% c(7)) %>%
    rename(unadj_total_import_input_val = purchase_val) %>%
    select(year, factory_id, unadj_total_import_input_val) %>%
    distinct()
  
    # Check if factories report more than one "total imports"
    total_imports_reporting_tbl <-
	    total_imports_tbl %>%
	    group_by(factory_id, year) %>%
	    summarize(n = n()) %>%
	    filter(n != 1)

    # No factories have two observations each. Code won't do anything.
    # I still keep it around.
    total_imports_tbl <-
	    total_imports_tbl %>%
	    anti_join(total_imports_reporting_tbl)
  
  # Combine basic and total inputs (domestic and imports) -----------
  inputs_tbl <- 
	  basic_inputs_tbl %>%
    full_join(total_inputs_tbl, by = c("year", "factory_id")) %>%
    full_join(total_imports_tbl, by = c("year", "factory_id")) %>%
    # If a factory does not list any imports, assume import value is 0
    mutate(
      unadj_total_import_input_val = ifelse(
        is.na(unadj_total_import_input_val), 0, unadj_total_import_input_val
      ), # Create a "total input value" variable (total inputs +
	   # total imported inputs)
      unadj_total_input_val = unadj_total_local_input_val + unadj_total_import_input_val
    ) 
  
  # Get input share of revenue --------------------------------------
  
  inputs_tbl <-
	  revenue_tbl %>%
    full_join(inputs_tbl, by = c("year", "factory_id")) %>%
    mutate(
      basic_input_share = unadj_basic_input_val / unadj_revenue,
      local_input_share = unadj_total_local_input_val / unadj_revenue, 
      import_input_share = unadj_total_import_input_val / unadj_revenue,
      total_input_share = unadj_total_input_val / unadj_revenue
    ) %>%
    select(-c(unadj_revenue, adj_revenue))
  
  ##---------------------------------------------------------------
  ##                      EMPLOYEES AND WAGES                     -
  ##---------------------------------------------------------------

  block_e_tbl <- 
	  block_list %>%
	  filter(block == "E") %>%
	  select(data) %>% 
	  unnest(data) 
  
  # 99-00: Total employees: sno = 9
  # 00-01: Total employees: sno = 10
  # 01-02: Total employees: sno = 10
  # 02-03: Total employees: sno = 10
  # 03-04: Total employees: sno = 10
  # 04-05: Total employees: sno = 10
  # 05-06: Total employees: sno = 10
  # 06-07: Total employees: sno = 10
  # 07-08: Total employees: sno = 10
  # 08-09: Total employees: sno = 9
  # 09-10: Total employees: sno = 9
  # 10-15: Total employees: sno = 9
  
  labor_tbl <-
    block_e_tbl %>%
    filter(
	   (year == 2000 & sno == 9) | (year %in% 2001:2008 & sno == 10) | (year %in% 2009:2015 & sno == 9)
	   ) %>%
    select(
      year,
      factory_id,
      avg_total_employees = avg_person_worked,
      unadj_total_wages = wages
    ) %>% 
    distinct()
  
    # Check if factories report more than one "total employees"
    labor_reporting_tbl <-
	labor_tbl %>%
	    group_by(factory_id, year) %>%
	    summarize(n = n()) %>%
	    filter(n != 1)

    # There aren't any observations in the reporting table, so
    # the following code shouldn't do anything. I keep it for
    # ease of reuse if something change.
    labor_tbl <-
	    labor_tbl %>%
	    anti_join(labor_reporting_tbl)

  ##----------------------------------------------------------------
  ##                      WAGE-REVENUE SHARE                       -
  ##----------------------------------------------------------------
  
  # To get the wage-revenue share, I just divide wages by the unadjusted 
  # revenue. Since all the amounts are in the same monetary units (current Rs)
  # there is no effect of adjusting.
  
  labor_tbl <-
	  revenue_tbl %>% 
	  full_join(labor_tbl, by = c("year", "factory_id")) %>%
	  mutate(
		 wage_share = unadj_total_wages / unadj_revenue
	  ) %>%
	  select(-c(unadj_revenue, adj_revenue))
  
  #################################################################
  ##                       2: CREATE FLAGS                       ##
  #################################################################
  
  # I now "flag" osbervations that are suspicious. I create two kinds
  # of flags: value flags (wage share of revenues and electricity use)
  # and change flags (does an observation differ strongly for previous
  # and subsequent observation of same plant). Based on these flags
  # I drop observations.

  # Revenue share flags: ----------------------------------------------
  
  # Wage share of revenue (not 2+)
  wage_flag_tbl <-
    labor_tbl %>%
    mutate(
      wage_revenue_flag = wage_share > 2 | is.na(wage_share)
    ) %>%
    select(year, factory_id, wage_revenue_flag)
  
  # Input share of revenue (not 2+)
  input_flag_tbl <- 
    inputs_tbl %>%
    mutate(
      total_input_revenue_flag = total_input_share > 2 | is.na(total_input_share)
    ) %>%
    select(
      year,
      factory_id,
      total_input_revenue_flag,
    )
  
  # Electricity use (not 0 and not NA) 
  electricity_flag_tbl <- 
    revenue_tbl %>%
	    full_join(electricity_tbl) %>%
	    mutate(
		   zero_electricity_flag = ifelse(is.na(electricity_consumed_kwh) | (electricity_consumed_kwh == 0), TRUE, FALSE),
		   electricity_value_flag = (electricity_purch_val / unadj_revenue) > 1 | is.na(electricity_purch_val) | is.na(unadj_revenue)
		   ) %>%
	    select(
		   year, 
		   factory_id,
		   zero_electricity_flag,
		   electricity_value_flag
		   )

  # Within plant flags ("change" flags): ------------------------------
  
  # Change in revenue
  revenue_change_flag_tbl <-
    revenue_tbl %>%
    mutate(ln_adj_revenue = log(adj_revenue)) %>%
    group_by(factory_id) %>%
    arrange(year) %>%
    mutate(
      # Positive if higher than prev obs
      change_from_prev = ln_adj_revenue - lag(ln_adj_revenue), 
      # Positive if higher than next obs
      change_from_next = ln_adj_revenue - lead(ln_adj_revenue) 
    ) %>%
    mutate(
      adj_revenue_change_flag = case_when(
        # if one obs is mistakenly much higher than prev and next
        change_from_prev >=3.5 & change_from_next >= 3.5 ~ TRUE, 
        # if one obs is mistakenly much lower than prev and next
        change_from_prev <=-3.5 & change_from_next <= -3.5 ~ TRUE, 
        # if first obs is much higher or lower than next obs
        is.na(lag(ln_adj_revenue)) & (change_from_next >= 3.5 | change_from_next <=-3.5) ~ TRUE, 
        # if last obs is much higher or lower than prev obs
        is.na(lead(ln_adj_revenue)) & (change_from_prev >=3.5 | change_from_prev <=-3.5) ~ TRUE, 
        TRUE ~ FALSE
      )
    ) %>%
    select(
      year,
      factory_id,
      adj_revenue_change_flag
    )
  
  # Change in employees
  employees_change_flag_tbl <- 
    labor_tbl %>%
    mutate(ln_total_employees = log(avg_total_employees)) %>%
    group_by(factory_id) %>%
    arrange(year) %>%
    mutate(
      change_from_prev = ln_total_employees - lag(ln_total_employees), # Positive if higher than prev obs
      change_from_next = ln_total_employees - lead(ln_total_employees) # Positive if higher than next obs
    ) %>%
    mutate(
      employees_change_flag = case_when(
        change_from_prev >=3.5 & change_from_next >= 3.5 ~ TRUE, # if one obs is mistakenly much higher
        change_from_prev <= -3.5 & change_from_next <= -3.5 ~ TRUE, # if one obs is mistakenly much lower
        is.na(lag(ln_total_employees)) & (change_from_next >= 3.5 | change_from_next <=-3.5) ~ TRUE, # if first obs is much higher or lower than next obs
        is.na(lead(ln_total_employees)) & (change_from_prev >=3.5 | change_from_prev <=-3.5) ~ TRUE, # if last obs is much higher or lower than prev obs
        TRUE ~ FALSE
      )
    ) %>%
    select(
      year,
      factory_id,
      employees_change_flag
    )
  
  # Change in electricity consumed (purchased and self-generated)
  electricity_change_flag_tbl <- 
    electricity_tbl %>%
    mutate(
      ln_electricity_consumed_kwh = log(electricity_consumed_kwh),
      ln_electricity_selfgen = log(electricity_self_gen_kwh)
    ) %>%
    group_by(factory_id) %>%
    arrange(year) %>%
    mutate(
      purch_change_from_prev = ln_electricity_consumed_kwh - lag(ln_electricity_consumed_kwh), # Positive if higher than prev obs
      purch_change_from_next = ln_electricity_consumed_kwh - lead(ln_electricity_consumed_kwh), # Positive if higher than next obs
      selfgen_change_from_prev = ln_electricity_selfgen - lag(ln_electricity_selfgen), # Positive if higher than prev obs
      selfgen_change_from_next = ln_electricity_selfgen - lead(ln_electricity_selfgen) # Positive if higher than next obs
    ) %>%
    mutate(
      electricity_consumed_change_flag = case_when(
        purch_change_from_prev >=3.5 & purch_change_from_next >= 3.5 ~ TRUE, # if one obs is mistakenly much higher
        purch_change_from_prev <=-3.5 & purch_change_from_next <= -3.5 ~ TRUE, # if one obs is mistakenly much lower
        is.na(lag(ln_electricity_consumed_kwh)) & (purch_change_from_next >= 3.5 | purch_change_from_next <=-3.5) ~ TRUE, # if first obs is much higher or lower than next obs
        is.na(lead(ln_electricity_consumed_kwh)) & (purch_change_from_prev >=3.5 | purch_change_from_prev <=-3.5) ~ TRUE, # if first obs is much higher or lower than next obs
        TRUE ~ FALSE
      ),
      electricity_selfgen_change_flag = case_when(
        selfgen_change_from_prev >=3.5 & selfgen_change_from_next >= 3.5 ~ TRUE, # if one obs is mistakenly much higher
        selfgen_change_from_prev <=-3.5 & selfgen_change_from_next <= -3.5 ~ TRUE, # if one obs is mistakenly much lower
        is.na(lag(ln_electricity_selfgen)) & (selfgen_change_from_next >= 3.5 | selfgen_change_from_next <=-3.5) ~ TRUE, # if first obs is much higher or lower than next obs
        is.na(lead(ln_electricity_selfgen)) & (selfgen_change_from_prev >=3.5 | selfgen_change_from_prev <=-3.5) ~ TRUE, # if last obs is much higher or lower than prev obs
        TRUE ~ FALSE
      )
    ) %>%
    select(
      year,
      factory_id,
      electricity_consumed_change_flag
    #   electricity_selfgen_change_flag <- not really needed. it should change a lot given the shortages change
    )
  
  # TODO: Change in amount of inputs used - PROBLEM: not adjusted.
    inputs_change_flag_tbl <-
	    inputs_tbl %>%
	    mutate(
		   ln_total_input_val = log(unadj_total_input_val),
		   input_val_change_from_prev =  ln_total_input_val - lag(ln_total_input_val), # positive if higher than prev obs 
		   input_val_change_from_next =  ln_total_input_val - lead(ln_total_input_val), # positive if higher than next obs 
		   input_val_change_flag = case_when(
						     input_val_change_from_prev >= 3.5 & input_val_change_from_next >= 3.5 ~ TRUE, # if one obs is much higher 
						     input_val_change_from_prev <= -3.5 & input_val_change_from_next <= -3.5 ~ TRUE, # if one obs is much higher 
						     is.na(lag(ln_total_input_val)) & (input_val_change_from_next >= 3.5 | input_val_change_from_next <= -3) ~ TRUE,
						     is.na(lag(ln_total_input_val)) & (input_val_change_from_prev >= 3.5 | input_val_change_from_prev <= -3) ~ TRUE,
						     TRUE ~ FALSE
						     )
		   ) %>%
	    select(year, factory_id, input_val_change_flag)

  ##---------------------------------------------------------------
  ##                        GATHER FLAG TABLES                    -
  ##---------------------------------------------------------------
  
  value_flag_tbl <- 
    wage_flag_tbl %>%
    full_join(input_flag_tbl) %>%
    full_join(electricity_flag_tbl) 
 
change_flag_tbl <-
	    revenue_change_flag_tbl %>%
	    full_join(employees_change_flag_tbl) %>%
	    full_join(electricity_change_flag_tbl) %>%
	    full_join(inputs_change_flag_tbl) %>%
	    mutate(change_flags = adj_revenue_change_flag + employees_change_flag + electricity_consumed_change_flag + input_val_change_flag)

  plant_tbl <-
	  id_tbl %>%
	  full_join(revenue_tbl) %>%
	  full_join(electricity_tbl) %>%
	  full_join(labor_tbl) %>%
	  full_join(inputs_tbl)
 
  ##################################################################
  ##                        3: FILTER DATA                        ##
  ##################################################################

  # Remove closed factories
  plant_tbl <-
	  plant_tbl %>%
		  filter(unit_status == 1)

  return(list("plant_tbl" = plant_tbl,
	      "change_flag_tbl" = change_flag_tbl,
	      "value_flag_tbl" = value_flag_tbl))
	  
}

