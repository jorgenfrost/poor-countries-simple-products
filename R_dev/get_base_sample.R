# TODO: Unfinished

# for testing

# plant_tbl <- readd("plant_ls")$plant_tbl
# pci_tbl <- readd("lenient_plant_pci_tbl")

get_base_sample <- function(plant_tbl, pci_tbl) {
	base_sample <- 
		plant_tbl %>%
		left_join(pci_tbl)

	return(base_sample)

}



