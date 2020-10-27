# TODO: Unfinished

# for testing
#  plant_ls <- readd("plant_ls")
#  plant_tbl <- plant_ls$plant_tbl
#  change_flag_tbl <- plant_ls$change_flag_tbl
#  value_flag_tbl <- plant_ls$value_flag_tbl

# pci_tbl <- readd("lenient_plant_pci_tbl")

get_base_sample <- function(plant_tbl, value_flag_tbl, change_flag_tbl, pci_tbl, path_to_exclusion_overview) {

	# Remove closed plants
	n_closed_plants <- sum(plant_tbl$unit_status != 1, na.rm = TRUE)

	plant_tbl <- 
		plant_tbl %>%
		filter(unit_status == 1)

	# Remove plants without revenues
	n_no_revenues <- sum(is.na(plant_tbl$unadj_revenue))

	plant_tbl <-
		plant_tbl %>%
		filter(!is.na(unadj_revenue))

	# Remove plants without a manufacturing industry code
	# TODO: Get manufacturing industry code list
	# TODO: Harmonize nic codes
	n_non_manuf_nic <- NA

	# More than one revenue share flag
	revenue_share_exclusions <- 
		value_flag_tbl %>%
		semi_join(plant_tbl) %>%
		mutate(
		       no_of_input_share_flags = wage_revenue_flag + total_input_revenue_flag + electricity_value_flag
		       ) %>%
		filter(no_of_input_share_flags > 1) %>%
		select(year, factory_id, no_of_input_share_flags)

	plant_tbl <- 
		plant_tbl %>%
		anti_join(revenue_share_exclusions, by = c("year", "factory_id"))


	# Join with plant complexity tbl and remove NA values (which are introduced because codes
        # could not be converted to HS96).
	n_pci_exclusions <- 
		plant_tbl %>%
		left_join(pci_tbl, by = c("year", "factory_id")) %>%
		filter(is.na(max_rca_pci)) %>%
		filter(is.na(avg_rca_pci)) %>%
		nrow()


	plant_tbl <-
		plant_tbl %>%
		left_join(pci_tbl, by = c("year", "factory_id")) %>%
		filter(!is.na(max_rca_pci)) %>%
		filter(!is.na(avg_rca_pci))

	# Remove plants that have NA in multiplier (weights) (a few factories in 2006)
	multiplier_exclusions <- 
		plant_tbl %>%
		filter(is.na(multiplier)) %>%
		nrow()

	plant_tbl <- 
		plant_tbl %>%
		filter(!is.na(multiplier))

	# Collect and write an overview of plants that are excluded
	tibble(
	       "Closed plants" = n_closed_plants,
	       "Plants without revenue" = n_no_revenues,
	       "Plants without a manufacturing NIC code" = n_non_manuf_nic,
	       "Plants with more than one revenue-share flag" = revenue_share_exclusions %>% nrow(),
	       "Plants that without product matches in HS96" = n_pci_exclusions,
	       "Plants with missing values in various varibles (fx weights)" = multiplier_exclusions
	       ) %>%
	gather(key = "Exclusion reason", value = "n") %>%
	write_csv(path_to_exclusion_overview)

	return(plant_tbl)

}
