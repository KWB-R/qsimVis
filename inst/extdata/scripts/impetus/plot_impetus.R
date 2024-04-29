# load spatially aggregated data
ag_table <- readxl::read_xlsx(
  path = "C:/Users/mzamzo/Documents/impetus/output/output_table.xlsx",
  sheet = 2)

# translation between Qsim ID and verknet ID
t_table <- read.table(
  file = system.file(package = "qsimVis",
                     "extdata/scripts/impetus/river_id_table.csv"),
  header = TRUE,
  sep = ";")

# der zweite Teil der Bezeichnung von des Strangs (section) sollte in die Übersetzungstabelle eingehen
# sonst wird zum Beispiel ein Teil des WHK nicht geplotted, weil er als BSK erkannt wird
aggregated_data <- qsimVis::qsim_to_verknet_id(
  aggregated_data = ag_table,
  translation_table = t_table)

rivers <- qsimVis::load_rivers(aggregated_data = aggregated_data)

rivers_ext <- lapply(
  X = names(rivers), FUN = qsimVis::extend_riverTable,
  rivers = rivers,
  aggregated_data = aggregated_data,
  varName = "adverse_dev",
  sixBreaks = c(0, 0.1, 0.3, 0.5, 0.7, 0.9),
  NA_processing = "interpolation")
names(rivers_ext) <- names(rivers)

# plot empty map
# qsimVis::plot_empty_map(rivers = rivers_ext, plot_toner = FALSE)
qsimVis::plot_empty_map(
  bbox = list(c(13, 13.8),
              c(52.35, 52.68))
)

# Add Shape Background
qsimVis::Berlin_add_boarder()
qsimVis::Berlin_add_waterbodies()

# Add colored Rivers
qsimVis::add_coloredRivers(
  ext_rivers = rivers_ext,
  aggregated_data = aggregated_data,
  sixBreaks = c(0, 0.1, 0.3, 0.5, 0.7, 0.9),
  dataType = "time",
  LegendTitle = "Durchschnittlicher \nAbwassergehalt in %"
)

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


