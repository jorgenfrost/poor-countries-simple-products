
# shortage_tbl <- readd("energy_supply_tbl")

plot_shortage_distribution <- function(shortage_tbl) {

shortage_tbl <-
	shortage_tbl %>%
	filter(!is.na(state_label)) # removes states not in the ASI (DVC, fx)

# get mean val for plot
shortage_tbl <-
	shortage_tbl %>%
	group_by(state_code) %>%
	mutate(period_mean_shortage = mean(avg_shortage))

# Line plot: shortage in individual states
shortage_line_p <-
	ggplot(shortage_tbl, aes(x = year, y = avg_shortage, group = state_code)) +
	geom_line() +
	geom_line(data = shortage_tbl, aes(x = year, y = period_mean_shortage, group = state_code), linetype = "dotted") +
	facet_wrap(. ~ state_label, ncol = 4) +
	ylab("shortage")

# boxplot distribution of shortages
shortage_box_p <-
	ggplot(shortage_tbl, aes(x = state_label, y = avg_shortage)) +
	geom_boxplot() +
	coord_flip() +
	xlab("state") +
	ylab("shortage")


# Joined plot
joined_p <- ggarrange(shortage_line_p, shortage_box_p)

# save plots
w_h <- 11
h_h <- 12.8

ggsave(
	plot = joined_p,
	filename = here("doc/figures/appendix/shortage_distribution.pdf"),
	units = c("in"),
	width = w_h,
	height = h_h
	)

return("done")
}
