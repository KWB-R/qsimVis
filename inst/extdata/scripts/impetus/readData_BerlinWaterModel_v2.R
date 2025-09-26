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
reference_vector <- rep(0, nrow(df_pro))

output <- list(
  "def_hours" =
    qsimVis::deviating_hours(
    dataFrame = df_pro,
    thresholds = c(0,10, 20, 40, 60, 80)/100,
    #thresholds = c(1, 2, 4, 8, 16),
    dev_type = "egt"),
  "adv_deviation" =
    qsimVis::adverse_deviation_from_reference(
      dataFrame = df_pro,
      reference = reference_vector,
      worst = 1,
      good_values = "low"),
  "crit_events" = qsimVis::critical_events(
    dataFrame = df_pro,
    dev_type = "egt",
    deficiency_hours = 24,
    separating_hours = 0,
    threshold = 0.6,
    recovery_value = NULL,
    return_event_positions = FALSE)
)

head(output$def_hours)
head(output$crit_events)

# Combine river stretch and simulations data
mapping_table <- read.table(
  file = system.file(package = "qsimVis",
                     "extdata/scripts/impetus/BelinWaterModel_id_table.csv"),
  header = TRUE,
  sep = ";")

rivers <- qsimVis::prepare_rivers(
  mapping_table = mapping_table,
  aggregated_data = output$adv_deviation,
  value_column = "adverse_dev",
  path_manual = system.file(package = "qsimVis", "extdata/manually_added_rivers"),
  gap_filling = "steps"
)

rivers <- qsimVis::value_to_classes(
  river_list = rivers,
  classBreaks = c(0, 0.05, 0.1, 0.2, 0.4, 0.7, 0.9)
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
  ext_rivers = rivers,
  aggregated_data = aggregated_data,
  sixBreaks = c(0, 0.05, 0.1, 0.2, 0.4, 0.7,0.9),
  dataType = "time",
  LegendTitle = "Durchschnittlicher \nAbwassergehalt"
)

# Write output table
# writexl::write_xlsx(x = output, path = file.path(path, "Viewer_Skript", "output_table.xlsx"))






