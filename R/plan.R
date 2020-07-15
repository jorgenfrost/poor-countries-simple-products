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
    npcms_blocks_raw = read_npcms_blocks(),
    asi_blocks_clean = clean_asi_blocks(
      file_path = file_in("data/temp/npcms_blocks_raw.rds")
    ),
    
    # Get plant-level data (one obs per plant)
    plant_ls = get_plant_tbl(
                       block_list = asi_blocks_clean,
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
      block_j_tbl = asi_blocks_clean$block_j_tbl
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
    
    # Clean power data ----------------------------------------------
    energy_supply_tbl = clean_energy_supply()
    
    # Prepare analysis ----------------------------------------------
    # Perform analysis ----------------------------------------------
    # Robustness checks ---------------------------------------------
    # Create figures ------------------------------------------------
    
    # TODO: Distribution of plant complexity in strict vs lenient matches
    # TODO: Distribution of plant complexity in different states
    # TODO: Distribution of product complexity
    
  )
