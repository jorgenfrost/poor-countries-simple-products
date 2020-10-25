
check_hs96_concordance_quality <- function() {
# TODO: Get list of factories that are present in the last asicc year
# and the first npcms year
last_asicc_factories <- 
	output_tbl %>%
	  filter(code_scheme == "asicc") %>%
	  filter(year == max(year)) %>%
	  distinct(factory_id)

first_npcms_factories %>%
	  filter(code_scheme == "asicc") %>%
	  filter(year == max(year)) %>%
	  distinct(factory_id)

  # last asicc year
  last_asicc_tbl <-
	  output_tbl %>%
	  filter(code_scheme == "asicc") %>%
	  filter(year == max(year)) %>%
	  select(year,
		 factory_id,
		 strict_hs96,
		 lenient_hs96)

  # first NPCMS year
  first_npcms_tbl <-
	  output_tbl %>%
	  filter(code_scheme == "npcms") %>%
	  filter(year == min(year)) %>%
	  select(year,
		 factory_id,
		 strict_hs96,
		 lenient_hs96)

test_tbl <- 
	full_join(last_asicc_tbl, first_npcms_tbl, 
	  by = c("year", "factory_id"),
	  suffix = c("_asicc", "_npcms")) %>%	
	mutate(
	       strict_match = strict_hs96_asicc == strict_hs96_npcms,
	       lenient_match = lenient_hs96_asicc == lenient_hs96_npcms
	       ) %>%
	select(year, factory_id, strict_match, lenient_match) 
}
