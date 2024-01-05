# load spatially aggregated data
ag_table <- readxl::read_xlsx(
  path = "C:/Users/mzamzo/Documents/impetus/output/output_table.xlsx",
  sheet = 2)


# translation between Qsim ID and verknet ID
t_table <- read.table(
  file = "C:/Users/mzamzo/Documents/impetus/process/river_id_table.csv",
  header = TRUE,
  sep = ";")

aggregated_data <- qsim_to_verknet_id(
  aggregated_data = ag_table,
  translation_table = t_table)

rivers <- qsimVis::load_rivers(aggregated_data = aggregated_data)

# plot empty map
qsimVis::plot_empty_map(rivers = rivers)

# Add Shape Background
add_polygons()

# Add colored Rivers
qsimVis::add_coloredRivers(
  rivers = rivers,
  aggregated_data = aggregated_data,
  varName = "adverse_dev",
  sixBreaks = c(0, 0.1, 0.3, 0.5, 0.7, 0.9),
  dataType = "time",
  LegendTitle = "Durchschnittlicher \nAbwassergehalt in %")

# Add CSO volume
# qsimVis::add_inflow(
#   filname = file.path(
#     "C:/Users/mzamzo/Documents/R/git",
#     "qsimVis/inst/extdata/cso_data",
#     "E2_2011_20221209_14-52_stats.csv"),
#   varName = "tVol_m3",
#   sizeMax = 100000)


# Add flow (average during time period or before event)

# Path for saving
saving_path <- file.path(scenario_path, "5_assessment_output")


