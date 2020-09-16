oec_pci <- readd("oec_pci_tbl")

own_rca_pci <- readd("own_rca_pci_tbl")

own_rpca_pci <- readd("own_rpca_pci_tbl")

# year 2010

oec10 <- 
	oec_pci %>%
	filter(year == 2010) %>%
	rename(oec_pci = pci)

rca10 <- 
	own_rca_pci %>%
	filter(year == 2010) %>%
	rename(rca_pci = pci)

rpca10 <-
	own_rpca_pci %>%
	filter(year == 2010) %>%
	rename(rpca_pci = pci)


pci_tbl <- rca10 %>%
	left_join(oec10) %>%
	left_join(rpca10) 
