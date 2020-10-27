# Theming function
apply_theme <- function(plot_object) {
	nice_plot <- 
		plot_object +
		theme_bw()

	return(nice_plot)
}

# Colors 
plot_red <- "#a50f15"
plot_blue <- "#253494"
