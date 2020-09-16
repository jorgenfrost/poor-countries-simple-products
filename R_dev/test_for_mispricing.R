asi_blocks_ls <- readd("asi_blocks_clean")

# CHECK PRICING DISTRIBUTION OF INPUTS

# Domestic inputs
h_tbl <- asi_blocks_ls$block_h_tbl %>%
	uncount(multiplier)

# Remove non-basic inputs (that is, input non-items - like electricity, chemicals, etc)


h_tbl <- 
	h_tbl %>%
	select(-block) %>%
	filter(sno %in% 1:10) %>%
	filter(qty_consumed != 0) %>%
	mutate(unit_price = purchase_val / qty_consumed) %>%
	group_by(year, item_code, qty_unit) %>%
	mutate(
		avg_unit_price = mean(unit_price),
		perc_diff_from_avg_price = (unit_price - avg_unit_price) / avg_unit_price
		) %>%
	ungroup()


h_summary_tbl <- 
	h_tbl %>%
	group_by(year, item_code, qty_unit) %>%
	summarize(
		sd_unit_price = sd(unit_price),
		sd_perc_diff_from_unit_price = sd(perc_diff_from_avg_price),
		median_perc_diff = median(perc_diff_from_avg_price),
		mean_perc_diff = mean(perc_diff_from_avg_price)
		)

# Imported inputs
i_tbl <- asi_blocks_ls$block_i_tbl

# Remove "total" (9994000) and "other" (9922100). Items are in 1:5.
i_tbl %>%
	filter(!item_code %in% c("9994000", "9922100") & sno %in% 1:5)

# Remove totals
i_tbl
