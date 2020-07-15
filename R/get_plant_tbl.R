#' This functions creates a a table that contains plant-specific observations
#' (complexity and power shortages information not included). It cleans and 
#' filters the observations based on a number of "flags".
#' 
#' @param block_list List containing the merged ASI blocks.
#' 
#' @return A data frame (plant_tbl) containing plant specific observations.
#' @export

# For testing:
# block_list <- readd("asi_blocks_clean")
# wpi_index <- readd("wpi_tbl")

get_plant_tbl <- function(block_list, wpi_index) {
  
  #################################################################
  ##                        1: CLEAN DATA                        ##
  #################################################################
  
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
  state_names_98_to_11 <-
    read_excel(
      here("data/external/asi/asi_2010_2016/State Master 1998-99 to 2011-12.xls"),
      skip = 2
    ) %>%
    clean_names(case = "snake") %>%
    mutate(codes = as.numeric(codes)) %>%
    rename(old_state_name = state_name)
  
  
  # Read state reference from 2012-13 to 2015-16. 
  state_names_12_to_15 <-
    read_excel(
      here("data/external/asi/asi_2010_2016/State Master 2012-13 onwards.xls"),
      skip = 2
    ) %>%
    clean_names(case = "snake") %>%
    mutate(codes = as.numeric(codes)) %>%
    rename(new_state_name = state_name)
  
  # Join method: create index on whether or not to join. If in 99-12 range,
  # "old_states" is equal to 1, 0 otherwise. If in 13-16 range, "new_states" is
  # qual to 1, 0 if not. All entries in state_names_99_to_12 have "old_states"
  # == 1, all entries in state_names_13_to_16 have "new_states" == 1. Join based
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
  
  ##----------------------------------------------------------------
  ##                            REVENUES                           -
  ##----------------------------------------------------------------
  
  block_j_tbl <- block_list$block_j_tbl
  
  # 10-15: Total items = 9995000 or sno = 12
  
  revenue_tbl <-
    block_j_tbl %>% 
    filter(item_code == "9995000") %>%
    select(
      year,
      factory_id,
      unadj_revenue = gross_sale_val
    )
  
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
  
  block_h_tbl <- block_list$block_h_tbl
  
  # Get electricity consumed -----------------------------------------
  
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
    left_join(revenue_tbl, electricity_tbl, by = c("year", "factory_id")) %>%
    mutate(
      electricity_intensity = electricity_consumed_kwh / adj_revenue
    ) %>%
    select(-c(unadj_revenue, adj_revenue))
  
  ##----------------------------------------------------------------
  ##                    INTERMEDIATE INPUT USE                     -
  ##----------------------------------------------------------------
  
  # Indigenous inputs ------------------------------------------------
  # 10-15: total basic items = 9990100 excludes electricity, non-basic
  # chemicals packing items, electricity generated, fuel, coal, etc.
  
  basic_inputs_tbl <- 
    block_h_tbl %>%
    filter(item_code == "9990100") %>%
    rename(unadj_basic_input_val = purchase_val) %>%
    select(year, factory_id, unadj_basic_input_val) 
  
  # 10-15: total inputs = 9993000 (includes everything)
  total_inputs_tbl <-
    block_h_tbl %>%
    filter(item_code == "9993000") %>%
    rename(unadj_total_input_val = purchase_val) %>%
    select(year, factory_id, unadj_total_input_val) 
  
  # Imported inputs -------------------------------------------------
  block_i_tbl <- block_list$block_i_tbl
  
  # 10-15: total imports consumed = 9995000
  total_imports_tbl <- 
    block_i_tbl %>%
    filter(item_code == "9994000") %>%
    rename(unadj_total_import_val = purchase_val) %>%
    select(year, factory_id, unadj_total_import_val) 
  
  # Combine total inputs, domestic and imports ----------------------
  total_inputs_tbl <- 
    total_inputs_tbl %>% 
    left_join(total_imports_tbl, by = c("year", "factory_id")) %>%
    mutate(
      unadj_total_import_val = ifelse(
        is.na(unadj_total_import_val), 0, unadj_total_import_val
      ),
      unadj_total_input_val = unadj_total_input_val + unadj_total_import_val
    ) %>%
    select(year, factory_id, unadj_total_input_val)
  
  # Join
  inputs_tbl <- 
    left_join(total_inputs_tbl, basic_inputs_tbl, by = c("year", "factory_id"))
  
  # Get input share of revenue --------------------------------------
  
  inputs_tbl <-
    left_join(revenue_tbl, inputs_tbl, by = c("year", "factory_id")) %>%
    mutate(
      total_input_share = unadj_total_input_val / unadj_revenue,
      basic_input_share = unadj_basic_input_val / unadj_revenue
    ) %>%
    select(-c(unadj_revenue, adj_revenue))
  
  ##---------------------------------------------------------------
  ##                      EMPLOYEES AND WAGES                     -
  ##---------------------------------------------------------------
  
  block_e_tbl <- block_list$block_e_tbl
  
  # 2010-15: Total employees: sno = 9
  
  labor_tbl <-
    block_e_tbl %>%
    filter(sno == 9) %>%
    select(
      year,
      factory_id,
      avg_total_employees = avg_person_worked,
      unadj_total_wages = wages
    )
  
  ##----------------------------------------------------------------
  ##                      WAGE-REVENUE SHARE                       -
  ##----------------------------------------------------------------
  
  # To get the wage-revenue share, I just divide wages by the unadjusted 
  # revenue. Since all the amounts are in the same monetary units (current Rs)
  # there is no effect of adjusting.
  
  labor_tbl <-
    left_join(labor_tbl, revenue_tbl, by = c("year", "factory_id")) %>%
    mutate(
      wage_share = unadj_total_wages / unadj_revenue
    ) %>%
    select(-c(unadj_revenue, adj_revenue))
  
  #################################################################
  ##                       2: CREATE FLAGS                       ##
  #################################################################
  
  # Revenue share flags: ----------------------------------------------
  
  # Wage share of revenue (not 2+)
  wage_flag_tbl <-
    labor_tbl %>%
    mutate(
      wage_revenue_flag = ifelse(wage_share >=2, 1, 0)
    ) %>%
    select(year, factory_id, wage_revenue_flag)
  
  # Input share of revenue (not 2+)
  input_flag_tbl <- 
    inputs_tbl %>%
    mutate(
      total_input_revenue_flag = ifelse(total_input_share >= 2, 1, 0),
      basic_input_revenue_flag = ifelse(basic_input_share >= 2, 1, 0)
    ) %>%
    select(
      year,
      factory_id,
      total_input_revenue_flag,
      basic_input_revenue_flag
    )
  
  # Electricity use (not 0) 
  electricity_flag_tbl <- 
    left_join(id_tbl, electricity_tbl, by = c("year", "factory_id")) %>%
    left_join(revenue_tbl, by = c("year", "factory_id")) %>%
    mutate(
      zero_electricity_flag = case_when(
        electricity_purch_kwh > 0 ~ 0,
        electricity_self_gen_kwh > 0 ~ 0,
        TRUE ~ 1
      ),
      electricity_val_share = ifelse(electricity_purch_val / unadj_revenue >= 1, 1, 0)
    ) %>%
    select(
      year,
      factory_id,
      zero_electricity_flag,
      electricity_val_share
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
        # if one obs is mistakenly much higher
        change_from_prev >=3.5 & change_from_next >= 3.5 ~ 1, 
        # if one obs is mistakenly much lower
        change_from_prev <=-3.5 & change_from_next <= -3.5 ~ 1, 
        # if first obs is much higher or lower than next obs
        is.na(lag(ln_adj_revenue)) & (change_from_next >= 3.5 | change_from_next <=-3.5) ~ 1, 
        # if first obs is much higher or lower than next obs
        is.na(lead(ln_adj_revenue)) & (change_from_prev >=3.5 | change_from_prev <=-3.5) ~ 1, 
        TRUE ~ 0
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
        change_from_prev >=3.5 & change_from_next >= 3.5 ~ 1, # if one obs is mistakenly much higher
        change_from_prev <=-3.5 & change_from_next <= -3.5 ~ 1, # if one obs is mistakenly much lower
        is.na(lag(ln_total_employees)) & (change_from_next >= 3.5 | change_from_next <=-3.5) ~ 1, # if first obs is much higher or lower than next obs
        is.na(lead(ln_total_employees)) & (change_from_prev >=3.5 | change_from_prev <=-3.5) ~ 1, # if first obs is much higher or lower than next obs
        TRUE ~ 0
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
      ln_electricity_purch = log(electricity_purch_kwh),
      ln_electricity_selfgen = log(electricity_self_gen_kwh)
    ) %>%
    group_by(factory_id) %>%
    arrange(year) %>%
    mutate(
      purch_change_from_prev = ln_electricity_purch - lag(ln_electricity_purch), # Positive if higher than prev obs
      purch_change_from_next = ln_electricity_purch - lead(ln_electricity_purch), # Positive if higher than next obs
      selfgen_change_from_prev = ln_electricity_selfgen - lag(ln_electricity_selfgen), # Positive if higher than prev obs
      selfgen_change_from_next = ln_electricity_selfgen - lead(ln_electricity_selfgen) # Positive if higher than next obs
    ) %>%
    mutate(
      electricity_purch_change_flag = case_when(
        purch_change_from_prev >=3.5 & purch_change_from_next >= 3.5 ~ 1, # if one obs is mistakenly much higher
        purch_change_from_prev <=-3.5 & purch_change_from_next <= -3.5 ~ 1, # if one obs is mistakenly much lower
        is.na(lag(ln_electricity_purch)) & (purch_change_from_next >= 3.5 | purch_change_from_next <=-3.5) ~ 1, # if first obs is much higher or lower than next obs
        is.na(lead(ln_electricity_purch)) & (purch_change_from_prev >=3.5 | purch_change_from_prev <=-3.5) ~ 1, # if first obs is much higher or lower than next obs
        TRUE ~ 0
      ),
      electricity_selfgen_change_flag = case_when(
        selfgen_change_from_prev >=3.5 & selfgen_change_from_next >= 3.5 ~ 1, # if one obs is mistakenly much higher
        selfgen_change_from_prev <=-3.5 & selfgen_change_from_next <= -3.5 ~ 1, # if one obs is mistakenly much lower
        is.na(lag(ln_electricity_selfgen)) & (selfgen_change_from_next >= 3.5 | selfgen_change_from_next <=-3.5) ~ 1, # if first obs is much higher or lower than next obs
        is.na(lead(ln_electricity_selfgen)) & (selfgen_change_from_prev >=3.5 | selfgen_change_from_prev <=-3.5) ~ 1, # if first obs is much higher or lower than next obs
        TRUE ~ 0
      )
    ) %>%
    select(
      year,
      factory_id,
      electricity_purch_change_flag,
    #   electricity_selfgen_change_flag <- not really needed. it should change a lot given the shortages change
    )
  
  # TODO: Change in amount of inputs used
  
  ##---------------------------------------------------------------
  ##                        GATHER FLAG TABLES                    -
  ##---------------------------------------------------------------
  
  flag_tbl <- 
    wage_flag_tbl %>%
    left_join(input_flag_tbl) %>%
    left_join(electricity_flag_tbl) %>%
    mutate(
	    rev_share_flags = wage_revenue_flag + basic_input_revenue_flag + electricity_val_share
	    ) 
  
  change_flag_tbl <- 
    revenue_change_flag_tbl %>%
    # TODO:	left_join(input_change_flag_tbl)
    left_join(employees_change_flag_tbl) %>%
    left_join(electricity_change_flag_tbl) %>%
    mutate(
	    change_flags = adj_revenue_change_flag + employees_change_flag + electricity_purch_change_flag
	    )

  plant_tbl <-
	  id_tbl %>%
	  left_join(revenue_tbl) %>%
	  left_join(electricity_tbl) %>%
	  left_join(labor_tbl) %>%
	  left_join(inputs_tbl) %>%
	  left_join(flag_tbl) %>%
	  left_join(change_flag_tbl) 
 
  ##################################################################
  ##                        3: FILTER DATA                        ##
  ##################################################################

  plant_tbl <- 
	  plant_tbl %>%
	  filter(unit_status == 1) %>% # Remove non-open factories
	  filter((rev_share_flags + change_flags) < 2) # Remove observations that have two or more flags (rev_share and change flags combined)

 # TODO: CONSIDERATIONS: should observations w/o electricity use be included?
 # TODO: CONSIDERATIONS: Make electricity consumption "missing" for all observations that have electricity consumed = 0.
	  
 joined_flag_tbl <- full_join(flag_tbl, change_flag_tbl)

 return(list("plant_tbl" = plant_tbl, "flag_tbl" = joined_flag_tbl))
  
}

