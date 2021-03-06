#' This function takes all the individual ASI blocks, selects variables,
#' fixes variable types, and merges data frames based on their block letter
#' (so that all "block A"-data frames becomes one, etc). Returns a list of 
#' the merge blocks.
#' 
#' @param file_path A string containing the path to the ASI blocks in .RDS 
#' format.
#' 
#' @return a list of data frames, where each data frame contains all the data 
#' from a specific block (A, B, C, etc).

# for testing
#   late_file_path <- here(file_in("data/temp/later_blocks_raw.rds"))
#   early_2000_2007_file_path <- here(file_in("data/temp/early_blocks_2000_2007_raw.rds"))
#   early_2008_file_path <- here(file_in("data/temp/early_blocks_2008_raw.rds"))
#   early_2009_file_path <- here(file_in("data/temp/early_blocks_2009_raw.rds"))
 # out_path <- here(file_out("data/temp/asi_blocks_clean.rds"))

clean_asi_blocks <- function(
	late_file_path,
	early_2000_2007_file_path,
	early_2008_file_path,
	early_2009_file_path,
	out_path
	) {
  
	# A bit of hacky code joining the temporarily saved data sets.
  
  early_2000_blocks <- 
    readRDS(early_2000_2007_file_path) %>%
    select(year, block, data) %>%
    mutate(panel_structure = "2000")

  early_2008_blocks <- 
    readRDS(early_2008_file_path) %>%
    select(year, block, data) %>%
    mutate(panel_structure = "2008")

  early_2009_blocks <- 
    readRDS(early_2009_file_path) %>%
    select(year, block, data) %>%
    mutate(panel_structure = "2009")

  late_blocks <- 
    readRDS(late_file_path) %>%
    mutate(year = as.numeric(year)) %>%
    select(year, block, data) %>% 
    mutate(panel_structure = "late")

  asi_blocks <- 
    bind_rows(
	    early_2000_blocks,
	    early_2008_blocks,
	    early_2009_blocks,
	    late_blocks
	    )

  # Remove all the files that are not the joined data.
  rm(
	  list = ls()[!ls() %in% c("asi_blocks", "out_path")]
	  )
  
 # 1: SELECT VARIABLES ----------------------------------------------
  
  # Create function that selects the variables I from a list of variable names.
  select_block_vars <- function(df, var_list) {
    
    # for each data frame the blk is the same across all obs.
    # If not, an error is thrown.
    current_block <- unique(df$block)
    current_year <- unique(df$year)
    
    # early return for blocks that are not relevant
    if (!(current_block %in% names(var_list))) {
      return(df)
    }
    
    # This is mainly used for easier identifying which block and which vars 
    # throw error.
    print(str_glue("Now selecting the relevant variables for {current_block} in year {current_year}."))
    print(names(df))
    
    relevant_variables <- block_variable_list[[current_block]]
    
    df <- df %>%
      select(all_of(relevant_variables))
    
    return(df)
    # end function
  }
  
  # Create list of variables I want from each block
  block_variable_list <- list(
    A = c(
      "year",
      "factory_id",
      "block",
      "scheme",
      "nic5code",
      "rural_urban",
      "no_units",
      "unit_status",
      "production_cost",
      "multiplier"
    ),
    B = c(
      "year",
      "factory_id",
      "block",
      "type_organisation",
      "initial_production",
      "multiplier"
    ),
    C = c(
      "year",
      "factory_id",
      "block",
      "sno",
      "opening_gross",
      "closing_gross",
      "opening_net",
      "closing_net",
      "multiplier"
    ),
    D = c(
      "year",
      "factory_id",
      "block",
      "sno",
      "w_cap_opening",
      "w_cap_closing",
      "multiplier"
    ),
    E = c(
      "year",
      "factory_id",
      "block",
      "sno",
      "avg_person_worked",
      "wages",
      "multiplier"
    ),
    #	 F = c( 
    #      ),
    #      G = c(
    #      ),
    H = c(
      "year",
      "factory_id",
      "block",
      "sno",
      "qty_unit",
      "qty_consumed",
      "item_code",
      "purchase_val",
      "multiplier"
    ),
    I = c(
      "year",
      "factory_id",
      "block",
      "sno",
      "qty_unit",
      "qty_consumed",
      "item_code",
      "purchase_val",
      "multiplier"
    ),
    J = c(
      "year",
      "factory_id",
      "block",
      "sno",
      "item_code",
      "qty_unit",
      "qty_sold",
      "ex_factory_val",
      "gross_sale_val",
      "multiplier"
    )
  )
  
  # Select variables
  asi_blocks <- asi_blocks %>%
    filter(block %in% names(block_variable_list)) %>%
    mutate(
      data = map(data, select_block_vars, var_list = block_variable_list)
    )
  
  # FIX VARIABLE TYPES ------------------------------------------------------
  
  # I fix all variable types in two steps. First I create a vector with
  # all the variables that should be changed into numeric format. Since
  # all variables are characters per default, I don't need to convert 
  # into character for any. Second I apply a function that goes into 
  # each nested data frame and converts the columns.
  
  numeric_vars <-
    c(
      "year", # BLOCK A STARTS
      "scheme",
      "rural_urban",
      "no_units",
      "unit_status",
      "production_cost",
      "multiplier", # BLOCK A STOPS
      "type_organisation", # BLOCK B STARTS
      "initial_production", # BLOCK B STOPS
      "sno", # BLOCK C STARTS
      "opening_gross",
      "closing_gross",
      "opening_net",
      "closing_net", # BLOCK C STOPS
      "w_cap_opening", # BLOCK D STARTS
      "w_cap_closing", # BLOCK D STOPS
      "avg_person_worked", # BLOCK E STARTS
      "wages", # BLOCK E STOPS
      "purchase_val", # BLOCK H, I STARTS
      "qty_consumed", # BLOCK H, I STOPS
      "qty_sold", # BLOCK J STARTS
      "ex_factory_val",
      "gross_sale_val" # BLOCK J STOPS
    )
  
  # Make the relevant variables numeric
  asi_blocks <-
    asi_blocks %>%
    mutate(data = map(
      .x = data,
      .f = function(df) df %>% mutate_at(
        names(.)[names(.) %in% numeric_vars],
        as.numeric
      )
    )
    )
  

  # Again, due to memory issues that arise in returning the block, I write it to file.
    saveRDS(
	    object = asi_blocks,
	    file = out_path
	    )

  return("Done")

}

  # GATHER BLOCKS -----------------------------------------------------------
  
  # Merge all similar blocks to one data set (so that all block A's become one 
  # block A, etc).
  
  # BLOCK A
#   block_a_tbl <-
#     asi_blocks %>%
#     filter(block == "A") %>%
#     select(data) %>%
#     unnest(data)
  
  # BLOCK B
#   block_b_tbl <-
#     asi_blocks %>%
#     filter(block == "B") %>%
#     select(data) %>%
#     unnest(data)
  
  # BLOCK C
#   block_c_tbl <-
#     asi_blocks %>%
#     filter(block == "C") %>%
#     select(data) %>%
#     unnest(data)
  
  # BLOCK D
#   block_d_tbl <-
#     asi_blocks %>%
#     filter(block == "D") %>%
#     select(data) %>%
#     unnest(data)
#   
  # BLOCK E
#   block_e_tbl <-
#     asi_blocks %>%
#     filter(block == "E") %>%
#     select(data) %>%
#     unnest(data)
  
  # BLOCK F
  # block_f_tbl <-
  #         asi_blocks %>%
  #         filter(block == "F") %>%
  #         select(data) %>%
  #         unnest(data)
  
  # BLOCK H
#   block_h_tbl <-
#     asi_blocks %>%
#     filter(block == "H") %>%
#     select(data) %>%
#     unnest(data)
#   
  # BLOCK I
#   block_i_tbl <-
#     asi_blocks %>%
#     filter(block == "I") %>%
#     select(data) %>%
#     unnest(data)
#   
  # BLOCK J
#   block_j_tbl <-
#     asi_blocks %>%
#     filter(block == "J") %>%
#     select(data) %>%
#     unnest(data)
#   
  # Add all blocks together in a list
#   index <- str_detect(
#     string = ls(),
#     pattern = "block_[a-z]_tbl"
#   )
#   
#   block_list <- mget(ls()[index])
  
