
  # Free up memory by removing old objects
  rm(list = ls()[ls() != "block_list"])

  # Read them into a list column and add indexing:
  # 1: Create function to extract year or block
  get_year_block <- function(tbl, extract) {
    
    year <- 
      tbl %>%
      distinct(year) %>%
      pull(year)
    
    block <-
      tbl %>%
      distinct(block) %>%
      pull(block)
    
    if (extract == "year") {
      return(year)
    } else if (extract == "block") {
      return(block)
    }
  }
  
  # 2: Apply function
  all_blocks_tbl <- 
    tibble(name = names(block_list), data = block_list) %>%
    mutate(
      year = map_chr(data, get_year_block, extract = "year"),
      block = map_chr(data, get_year_block, extract = "block")
    )
 
  rm("block_list")

  return(all_blocks_tbl)
}
