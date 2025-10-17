# Documentation of aggregation and visualization of Qsim results

project_path <-
  "Y:/iGB/Projects/IMPETUS/"
# "C:/Users/dwicke/Documents/work/IMPETUS"

data_path <- "Work-packages/WP4_Demonstration_KWB/CS-Berlin/04_Modelling/OGewaesser/BerlinWaterModel/Ergebnisse"
file_name <- "qsimVis_input_days_test_250905.csv"

# find out about column names --------------------------------------------------
colNames <- read.csv(
  file = file.path(project_path, data_path, file_name),
  header = FALSE, nrows = 1, sep = ";")
print(colNames)

# load and prepare qsim data
df_in <- qsimVis::QSIM_prepare(
  qsim_output_file = file.path(project_path, data_path, file_name),
  parameter_name = "tracer.wwtp",
  date_column_name = "Datum",
  id_column_name = "GewaesserId",
  km_column_name = "Km",
  flow_column_name = "Q",
  section_column_name = "Strang"
)

# Aggregate data
df_pro <- df_in$para
unit_factor <- 1/2
df_pro[,-1] <- df_pro[,-1] * unit_factor
reference_vector <- rep(0, nrow(df_pro))

output <- list(
  "def_hours" =
    qsimVis::deviating_hours(
      dataFrame = df_pro,
      thresholds = c(5, 10, 15, 20, 40, 60)/100, # Anzahl flexibel
      # thresholds = c(0,10, 20, 40, 60, 80)/100,
      dev_type = "egt", # auch "elt" = equal or lower than möglich
      relative = TRUE), # If TRUE --> relative values in %
  "adv_deviation" =
    qsimVis::adverse_deviation_from_reference(
      dataFrame = df_pro,
      reference = reference_vector,
      worst = 1,
      good_values = "low"),
  "crit_events" = qsimVis::critical_events(
    dataFrame = df_pro,
    dev_type = "egt", # auch "elt" = equal or lower than möglich
    deficiency_hours = 24,
    separating_hours = 0,
    threshold = 0.6,
    recovery_value = NULL,
    return_event_positions = FALSE),
  "stats" = qsimVis::stats(
    dataFrame = df_pro),
  "flow_mean" = qsimVis::flow_weighted_mean(
    dataFrame = df_pro,
    df_flow = df_in$flow
  )
)

head(output$adv_deviation)
head(output$def_hours)
head(output$stats)
head(output$flow_mean) # der Flow ist überall immer 19.37.
# es gibt keinen CVK

output_table <-
  "adv_deviation"
  # "def_hours"

if(output_table == "adv_deviation"){
  output_column <- "adverse_dev"
  classBreaks <- c(0, 0.05, 0.1, 0.15, 0.25, 0.5, 1)
  colorVector <- NULL # -> MisaColor
  LegendTitle <- "Durchschnittliger Abwassergehalt"
}
if(output_table == "def_hours"){
  output_column <- "above_0.2"
  classBreaks <- c(0, 5, 10, 15, 25, 50, 100)
  colorVector <- NULL # -> MisaColor
  LegendTitle <- "Anteil mit mehr als 20% Abwasser in %"
}

if(output_table == "def_hours"){
  output_column <- "above_0.2"
  classBreaks <- c(seq(0,50, 5), seq(60,100,10))
  colorVector <- c("blue", "green", "yellow", "orange", "darkred")
  LegendTitle <- "Anteil mit mehr als 20% Abwasser in %"
}

# Combine river stretch and simulations data
mapping_table <- read.table(
  file = system.file(package = "qsimVis",
                     "extdata/scripts/impetus/BelinWaterModel_id_table.csv"),
  header = TRUE,
  sep = ";")
rivers <- qsimVis::prepare_rivers(
  mapping_table = mapping_table,
  aggregated_data = output[[output_table]],
  value_column = output_column,
  path_manual = system.file(package = "qsimVis", "extdata/manually_added_rivers"),
  gap_filling = "steps"
)
# add classes and colors
rivers <- qsimVis::value_to_classes(
  river_list = rivers,
  classBreaks = classBreaks,
  colorVector = colorVector
)

# plot data
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
  ext_rivers = rivers
)

qsimVis::add_river_legend(
  ext_rivers = rivers,
  LegendTitle = LegendTitle,
  LegendLocation = "right"
)

# Save as png
qsimVis::saveActiveDevice(
  filename = "WaterModelPlot",
  path = file.path(project_path, data_path),
  type = "",
  resolution = "medium"
)

# save as svg (vector graphic)
qsimVis::saveActiveDevice(
  filename = "WaterModelPlot",
  path = file.path(project_path, data_path),
  type = "vector"
)

# Write output table
# writexl::write_xlsx(x = output, path = file.path(path, "Viewer_Skript", "output_table.xlsx"))




