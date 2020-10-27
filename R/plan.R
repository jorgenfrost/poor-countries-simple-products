the_plan <-
  drake::drake_plan(
    
    # Clean miscellaneous data --------------------------------------

    wpi_tbl = clean_wpi(
      wpi_1993_94_path = file_in("data/external/deflators/wpi_eaindustry_nic/wpi_base_1993_94.xls"),
      wpi_2004_05_path = file_in("data/external/deflators/wpi_eaindustry_nic/wpi_base_2004_05.xls"),
      linking_table_path = file_in("data/external/deflators/wpi_eaindustry_nic/linking_factors.csv")
    ),

    pop_tbl = clean_pop_data(
      pop_data = file_in("data/external/pop_international/population_total.csv"),
      min_year = 1998
    ),

    # TODO: Clean enterprise surveys
    # TODO: Clean NSS data
    # TODO: Clean RBI data
    # TODO: Clean rainfall data
    # TODO: Clean temperature data
    
    # Read and clean ASI data  --------------------------------------
    
    # Clean blocks
    earlier_blocks_raw = read_earlier_asi_blocks(
		 early_2000_2007_file_path = file_out("data/temp/early_blocks_2000_2007_raw.rds"),
		 early_2008_file_path = file_out("data/temp/early_blocks_2008_raw.rds"),
		 early_2009_file_path = file_out("data/temp/early_blocks_2009_raw.rds")
	    ),
    later_blocks_raw = read_later_asi_blocks(),
    asi_blocks_clean = clean_asi_blocks(
		 late_file_path = file_in("data/temp/later_blocks_raw.rds"),
		 early_2000_2007_file_path = file_in("data/temp/early_blocks_2000_2007_raw.rds"),
		 early_2008_file_path = file_in("data/temp/early_blocks_2008_raw.rds"),
		 early_2009_file_path = file_in("data/temp/early_blocks_2009_raw.rds"),
		 out_path = file_out("data/temp/asi_blocks_clean.rds")

         ),
    
    # Get plant-level data (one obs per plant)
    # TODO: Make decisions on flags and exclusion criteria
      plant_ls = get_plant_tbl(
                        block_list_path = file_in("data/temp/asi_blocks_clean.rds"),
                         wpi_index = wpi_tbl
			 ),
    
    # Get plant complexity ------------------------------------------
    
    export_tbl = clean_export_data(
      export_data = file_in("data/external/international_trade/year_origin_hs96_4.tsv"),
      pop_data = pop_tbl
    ),
    rca_tbl = get_comparative_advantage( 
      export_data = export_tbl,
      pop_data = pop_tbl,
      metric = "rca"
    ),
    rpca_tbl = get_comparative_advantage(
      export_data = export_tbl,
      pop_data = pop_tbl,
      metric = "rpca"
    ),
     hs96_output_tbl = get_hs96_products(
       block_ls_path = file_in("data/temp/asi_blocks_clean.rds")
     ),
    oec_pci_tbl = get_pci(
      source = "oec",
      oec_data = file_in("data/external/oec/pci_hs4_hs96_98-18.csv")
    ),
    own_rca_pci_tbl = get_pci(
      source = "rca",
      rca_data = rca_tbl
    ),
    own_rpca_pci_tbl = get_pci(
      source = "rpca",
      rpca_data = rpca_tbl
    ),
		    lenient_plant_pci_tbl = get_plant_complexity(
								 output_tbl = hs96_output_tbl,
								 rca_pci_tbl = own_rca_pci_tbl,
								 rpca_pci_tbl = own_rpca_pci_tbl,
								 product_match = "lenient"
								 ),
		    strict_plant_pci_tbl = get_plant_complexity(
								output_tbl = hs96_output_tbl,
								rca_pci_tbl = own_rca_pci_tbl,
								rpca_pci_tbl = own_rpca_pci_tbl,
								product_match = "strict"
								),

    # Finalize base sample ------------------------------------------
		    base_sample_tbl = get_base_sample(
						      plant_tbl = plant_ls$plant_tbl,
						      value_flag_tbl = plant_ls$value_flag_tbl,
						      change_flag_tbl = plant_ls$change_flag_tbl,
						      pci_tbl = lenient_plant_pci_tbl,
						      path_to_exclusion_overview = file_out("data/temp/base_sample_exclusion_overview.csv")
						      ),

    # Clean power data ----------------------------------------------
    energy_supply_tbl = clean_energy_supply(),
		    # TODO: Currently fails:
#    input_shortage_tbl = get_input_shortage(),
    
    # Prepare analysis ----------------------------------------------
    # Perform analysis ----------------------------------------------
    # Robustness checks ---------------------------------------------
	# TODO: Check how good product conversion is:
		    # 1. For firms observed both in the last year of asicc
		    # and the first year in npcms - do they have the same
		    # HS 96 code?
		    # 2. Are their complexity sort of the same?
		    # 3. How much of output is lost?
	# check_hs96_concordance_quality()
	
	# TODO: Check if the complexity of plants changes when using gross sale val

        # TODO: Check results by using strict pci 
        # TODO: Check resutls by using different revenue share flag threshold
        # TODO: Check results by excluding change flagged obs
	# TODO: Rerun results using strict pci
	# TODO: Check results using peak shortage instead

    # Create figures ------------------------------------------------
		    # TODO: Currently fails:
		    #     strict_lenient_pci_plot = plot_strict_vs_lenient_pci(
		    #             strict_plant_pci = strict_plant_pci_tbl,
		    #             lenient_plant_pci = lenient_plant_pci_tbl,
		    #             plant_tbl = plant_ls$plant_tbl
		    #             ),    
		    #     shortage_dist_plot = plot_shortage_distribution(
		    #             shortage_tbl = energy_supply_tbl
		    #             ),

    # TODO: Distribution of plant complexity in strict vs lenient matches
    # TODO: Distribution of plant complexity in different states
    # TODO: Distribution of product complexity
    
  )

