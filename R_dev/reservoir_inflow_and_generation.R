res_tbl <- 
	read_csv(here("data/external/hydro_generation/reservoir_inflow_and_generation/reservoir_inflow_and_generation_final.csv"))

lm(generation_gwh ~ inflows_mcm + as.factor(reservoir_scheme), data = res_tbl) %>%
	summary()

# Match plants med reservoir schemes --------------------------------

# Read matching table
reservoir_to_plant_tbl <- 
	read_csv(here("data/external/hydro_generation/reservoir_to_plant_match/from_cea/csv/tidy/reservoir_to_plant_match_stacked.csv"))

new_names_tbl <- 
	read_csv("/home/post/university/poor_countries_simple_products/data/external/hydro_generation/reservoir_to_plant_match/from_cea/csv/tidy/reservoir_plant_names.csv")

# Read hydro plant table
hydro_plant_tbl <-
	read_csv("data/external/hydro_generation/hydro_plant_generation_performance/cea_hydro_plant_generation.csv")

test_plants <- 
hydro_plant_tbl %>%
	group_by(station_group_composite, year) %>%
	summarize(
		installed_capacity_mw = sum(installed_capacity_mw)
		) %>%
	semi_join(new_names_tbl)

test_res <- 
	reservoir_to_plant_tbl %>%
	group_by(reservoir_name, year) %>%
	summarize(installed_capacity_mw = sum(installed_capacity_mw))

# Test if no run-of-river plants are matched to reservoirs


