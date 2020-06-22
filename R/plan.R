the_plan <-
  drake::drake_plan(

    ## Plan targets in here.
	  npcms_blocks_raw = read_npcms_blocks(),
	  asi_blocks_clean = clean_asi_blocks(
		  file_path = npcms_blocks_raw
		  ),
	  plant_tbl = get_plant_tbl(
		  block_list = asi_blocks_clean
		  )
    
  )
