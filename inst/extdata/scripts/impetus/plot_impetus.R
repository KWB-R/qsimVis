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

# der zweite Teil der Bezeichnung des Strangs (section) sollte in die Übersetzungstabelle eingehen
# sonst wird zum Beispiel ein Teil des WHK nicht geplotted, weil er als BSK erkannt wird
aggregated_data <- qsimVis::add_qsimVis_id(
  aggregated_data = ag_table,
  translation_table = t_table,
  id_source = "river_name"
)

# hier sollte nur "section_name" und "km" ausgegeben werden
rivers <- qsimVis::load_rivers(
  aggregated_data = ag_table,
  translation_table = t_table,
  path_manual = system.file(package = "qsimVis", "extdata/manually_added_rivers")
)

# prepare plot
sixBreaks = c(0, 0.05, 0.1, 0.2, 0.4, 0.7,0.9)

####### Example: Difference between "interpolation" and "steps" ################
example_river <- "Neukoellner Schifffahrtskanal"

qsimVis::extend_riverTable(
  rivers = rivers,
  river_id = example_river,
  aggregated_data = aggregated_data,
  varName = "adverse_dev",
  sixBreaks = sixBreaks,
  NA_processing = "interpolation")

qsimVis::extend_riverTable(
  rivers = rivers,
  river_id = example_river,
  aggregated_data = aggregated_data,
  varName = "adverse_dev",
  sixBreaks = sixBreaks,
  NA_processing = "steps")

################################################################################

rivers_ext <- lapply(
  X = names(rivers), FUN = qsimVis::extend_riverTable,
  rivers = rivers,
  aggregated_data = aggregated_data,
  varName = "adverse_dev",
  sixBreaks = sixBreaks,
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
  sixBreaks = sixBreaks,
  dataType = "time",
  LegendTitle = "Durchschnittlicher \nAbwassergehalt"
)




