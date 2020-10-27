#' This function plots the distribution of plant complexity using different 
#' metrics (max vs avg), different trade normalizations (rca vs rpca) and 
#' different product matching schemes (lenient vs strict). 
#' 
#' @param strict_plant_pci The table containing the plant complexity based 
#' on the strict matching scheme.
#' @param lenient_plant_pci The table containing the plant complexity based 
#' on the lenient matching scheme.
#' @param plant_tbl The table containing plant-level observations.
#' @return No important return. Just a dummy to satisfy the make process.
#' @export


# For testing
  strict_plant_pci <- readd("strict_plant_pci_tbl") 
# 
  lenient_plant_pci <- readd("lenient_plant_pci_tbl") 
 
  base_sample_tbl <- readd("base_sample_tbl")

plot_strict_vs_lenient_pci <- function(strict_plant_pci, lenient_plant_pci, base_sample_tbl) {

# Remove plants removed in filtering
strict_plant_pci <- 
	strict_plant_pci %>%
        mutate(product_match = "strict") %>%
	semi_join(base_sample_tbl, by = c("year", "factory_id")) %>%
	left_join(base_sample_tbl %>% select(year, factory_id, multiplier))

lenient_plant_pci <-
	lenient_plant_pci %>%
        mutate(product_match = "lenient") %>%
	semi_join(base_sample_tbl, by = c("year", "factory_id")) %>%
	left_join(base_sample_tbl %>% select(year, factory_id, multiplier))

#################################################################
##      PLOT STRICT VS LENIENT MATCHES: DIFFERENT METRICS      ##
#################################################################

wide_plant_pci_tbl <-
	bind_rows(strict_plant_pci, lenient_plant_pci)

long_plant_pci_tbl <-
	wide_plant_pci_tbl %>%
	pivot_longer(-c(year, factory_id, product_match, multiplier), names_to = "metric", values_to = "pci")


# DEFINE PLOTTING FUNCTION ------------------------------------------
make_metric_density_plot <- function(tbl, the_title, x_lab) {

	the_plot <-
		ggplot(tbl, aes(x = pci, color = product_match, weight = multiplier)) +
		geom_density() +
		labs(
		title = the_title,
		x = x_lab
		) + 
		facet_wrap(vars(year))

	the_nice_plot <- 
		(the_plot +
		theme(plot.title = element_text(size=10)) + 
		scale_color_manual(values = c(plot_blue, plot_red), name = "Product match")) %>%
	apply_theme() +
	theme(legend.position = "bottom")

	return(the_nice_plot)

}

# RCA AVG -----------------------------------------------------------
avg_rca_pci_p <-
	make_metric_density_plot(
		tbl = long_plant_pci_tbl %>% filter(metric == "avg_rca_pci"),
		the_title = "Plant complexity (C), RCA-based",
		x_lab = TeX("C")
		) 

# RCA MAX -----------------------------------------------------------
max_rca_pci_p <-
	make_metric_density_plot(
		tbl = long_plant_pci_tbl %>% filter(metric == "max_rca_pci"),
		the_title = TeX("Plant complexity (C^{max}), RCA-based"),
		x_lab = TeX("C^{max}")
		)

# RPCA AVG ----------------------------------------------------------
avg_rpca_pci_p <-
	make_metric_density_plot(
		tbl = long_plant_pci_tbl %>% filter(metric == "avg_rpca_pci"),
		the_title = "Plant complexity (C), RpcA-based",
		x_lab = TeX("C")
		)

# RPCA MAX ----------------------------------------------------------
max_rpca_pci_p <-
	make_metric_density_plot(
		tbl = long_plant_pci_tbl %>% filter(metric == "max_rpca_pci"),
		the_title = TeX("Plant complexity (C^{max}), RpcA-based"),
		x_lab = TeX("C^{max}")
		)

# Join figures ------------------------------------------------------
rca_metrics_joined <- 
	ggarrange(
		avg_rca_pci_p,
		max_rca_pci_p,
		common.legend = TRUE,
		legend = "bottom"
		)

rpca_metrics_joined <- 
	ggarrange(
				 avg_rpca_pci_p,
				 max_rpca_pci_p,
				 common.legend = TRUE
				 )

# Create plots separated by states ----------------------------------
state_tbl <-
	long_plant_pci_tbl %>%
	left_join(base_sample_tbl) 

# Density
plot_density <- function(tbl, x_lab) {

	the_plot <- 
		ggplot(
			data = tbl,
			aes(x = pci, color = match)
			) + 
		geom_density() +
		xlab(TeX(x_lab)) +
		facet_wrap(. ~ state_name)

	the_nice_plot <-
		the_plot +
		scale_color_manual(values = c("#a50f15", "#253494"), name = "")

	return(the_nice_plot)

}

avg_rca_density_p <-
	plot_density(
	tbl = state_tbl %>% filter(metric == "avg_rca_pci") %>% uncount(multiplier),
	x_lab = "C (RCA)"
	)

avg_rpca_density_p <-
	plot_density(
	tbl = state_tbl %>% filter(metric == "avg_rpca_pci") %>% uncount(multiplier),
	x_lab = "C (RpcA)"
	)

max_rca_density_p <-
	plot_density(
	tbl = state_tbl %>% filter(metric == "max_rca_pci") %>% uncount(multiplier),
	x_lab = "C^{max} (RCA)"
	)

max_rpca_density_p <-
	plot_density(
	tbl = state_tbl %>% filter(metric == "max_rpca_pci") %>% uncount(multiplier),
	x_lab = "C^{max} (RpcA)"
	)

# Histograms --------------------------------------------------------

plot_histograms <- function(tbl, x_lab) {

	the_plot <- 
		ggplot(
			data = tbl,
			aes(x = pci, fill = match)
			) + 
	geom_histogram(color = "black", alpha = 0.8) +
	scale_fill_manual(values = c("#a50f15", "#253494"), name = "") +
	facet_wrap(. ~ state_name) +
	xlab(TeX(x_lab))

	return(the_plot)

}

avg_rca_hist_p <- 
	plot_histograms(
	tbl = state_tbl %>% filter(metric == "avg_rca_pci"),
	x_lab = "C (RCA)"
	)

avg_rpca_hist_p <- 
	plot_histograms(
	tbl = state_tbl %>% filter(metric == "avg_rpca_pci"),
	x_lab = "C (RpcA)"
	)

max_rca_hist_p <- 
	plot_histograms(
	tbl = state_tbl %>% filter(metric == "max_rca_pci"),
	x_lab = "C^{max} (RCA)"
	)

max_rpca_hist_p <- 
	plot_histograms(
	tbl = state_tbl %>% filter(metric == "max_rpca_pci"),
	x_lab = "C^{max} (RpcA)"
	)

##################################################################
##          PLOT PLANT COMPLEXITY IN DIFFERENT METRICS          ##
##################################################################

# TODO: theme?
make_nice <- function(the_plot) {

	the_nice_plot <- 
		the_plot 

	return(the_nice_plot)

}

# Evaluate complexity of observations across different specs (scatter)
# Use a single year (one for ASICC and one for NPCMS)

plot_year <- 2010

max_across_p <- 
	ggplot(wide_plant_pci_tbl %>% filter(year == plot_year), aes(x = max_rca_pci, y = max_rpca_pci)) +
	geom_point(alpha = 0.2, size = 0.3) +
	facet_wrap(. ~ match) +
	scale_x_continuous(name = TeX("C^{max} (RCA)"), breaks = -3:3) +
	scale_y_continuous(name = TeX("C^{max} (RpcA)"), breaks = -6:6) 

avg_across_p <- 
	ggplot(wide_plant_pci_tbl %>% filter(year == plot_year), aes(x = avg_rca_pci, y = avg_rpca_pci)) +
	geom_point(alpha = 0.2, size = 0.3) +
	facet_wrap(. ~ match) +
	scale_x_continuous(name = TeX("C (RCA)"), breaks = -3:3) +
	scale_y_continuous(name = TeX("C (RpcA)"), breaks = -6:6)

# Plot within
rca_within_p <- 
	ggplot(wide_plant_pci_tbl %>% filter(year == plot_year), aes(x = max_rca_pci, y = avg_rca_pci)) +
	geom_point(alpha = 0.2, size = 0.3) +
	facet_wrap(. ~ match) +
	scale_x_continuous(name = TeX("C (RCA)"), breaks = -3:3) +
	scale_y_continuous(name = TeX("C^{max} (RCA)"), breaks = -6:6)

rpca_within_p <- 
	ggplot(wide_plant_pci_tbl %>% filter(year == plot_year), aes(x = max_rpca_pci, y = avg_rpca_pci)) +
	geom_point(alpha = 0.2, size = 0.3) +
	facet_wrap(. ~ match) +
	scale_x_continuous(name = TeX("C (RpcA)"), breaks = -6:6) +
	scale_y_continuous(name = TeX("C^{max} (RpcA)"), breaks = -6:6)

scatters_joined <-
	ggarrange(max_across_p, avg_across_p, rca_within_p, rpca_within_p)


##################################################################
##                          SAVE PLOTS                          ##
##################################################################

# Width and length (works pretty well, a bit on the small side of indiviudal plots)
w_h <- 9.8
h_h <- 10.8

# Density plots

ggsave(
	plot = different_metrics_joined,
	filename = here("doc/figures/appendix/density_metrics_combined.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)


# Density by state
ggsave(
	plot = max_rpca_density_p,
	filename = here("doc/figures/appendix/density_by_state_max_rpca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)

ggsave(
	plot = avg_rpca_density_p,
	filename = here("doc/figures/appendix/density_by_state_avg_rpca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)


ggsave(
	plot = avg_rca_density_p,
	filename = here("doc/figures/appendix/density_by_state_avg_rca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)

ggsave(
	plot = max_rca_density_p,
	filename = here("doc/figures/appendix/density_by_state_max_rca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)
# Histograms
ggsave(
	plot = avg_rca_hist_p,
	filename = here("doc/figures/appendix/histogram_avg_rca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)

ggsave(
	plot = avg_rpca_hist_p,
	filename = here("doc/figures/appendix/histogram_avg_rpca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)

ggsave(
	plot = max_rca_hist_p,
	filename = here("doc/figures/appendix/histogram_max_rca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)

ggsave(
	plot = max_rpca_hist_p,
	filename = here("doc/figures/appendix/histogram_max_rpca.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)

# Scatterplots

ggsave(
	plot = scatters_joined,
	filename = here("doc/figures/appendix/scatterplot_plant_complexity_across_metrics.pdf"),
	        units = c("in"),
	         width = w_h,
	         height = h_h
	)

return("Done")

}
