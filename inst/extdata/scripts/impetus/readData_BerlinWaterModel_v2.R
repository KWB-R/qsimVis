# Documentation of aggregation and visualization of Qsim results
library(qsimVis)

project_path <-
#  "Y:/iGB/Projects/IMPETUS/"
# "C:/Users/dwicke/Documents/work/IMPETUS"
  "C:/Users/dwicke/Documents/R/Github"

#data_path <- "Work-packages/WP4_Demonstration_KWB/CS-Berlin/04_Modelling/OGewaesser/BerlinWaterModel/Ergebnisse"
data_path <- "kwb.BerlinWaterModel"
file_name <- "qsimVis_input_hours_2017-2022_Fluoranthen.csv"

# find out about column names --------------------------------------------------
colNames <- read.csv(
  file = file.path(project_path, data_path, file_name),
  header = FALSE, nrows = 1, sep = ";")
print(colNames)

# load and prepare qsim data
df_in <- qsimVis::QSIM_prepare(
  qsim_output_file = file.path(project_path, data_path, file_name),
  parameter_name = "Fluoranthen.mg.m3", # "tracer.wwtp", "tracer.rain"
  date_column_name = "Datum",
  id_column_name = "GewaesserId",
  km_column_name = "Km",
  flow_column_name = "Q",
  section_column_name = "Strang"
)

# Aggregate data
df_pro <- df_in$para  # df_in$para oder df_in$flow
unit_factor <- 1   # Umrechnungsfaktor, z.B. g/m3 in µg/L
df_pro[,-1] <- df_pro[,-1] * unit_factor
reference_vector <- rep(0, nrow(df_pro))

output <- list(
  "def_hours" =
    qsimVis::deviating_hours(
      dataFrame = df_pro,
      # thresholds = c(5, 10, 15, 20, 40, 60)/100, # Anzahl flexibel
      thresholds = c(0.0063, 0.12),
      # thresholds = c(0,10, 20, 40, 60, 80)/100,
      dev_type = "gt", # "elt" = equal or lower than möglich, "egt", "gt", "lt"
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

output_table <-
  #"adv_deviation"
  "stats"
  #"def_hours"

head(output$adv_deviation)
head(output$def_hours)
head(output$stats)
head(output$flow_mean)
# es gibt keinen CVK

# Zeitdauer Abwasseranteil > x%
if(output_table == "def_hours"){
  output_column <- "above_0.2"
  classBreaks <- c(0, 5, 10, 15, 25, 50, 100)
#  classBreaks <- c(seq(0,50, 5), seq(60,100,10))
  colorVector <- NULL # -> MisaColor
  LegendTitle <- "Zeitanteil mit mehr als 20% Abwasser in %"
}

# Zeitdauer Regenabflussanteil > x%
if(output_table == "def_hours"){
  output_column <- "above_0.1"
  classBreaks <- c(0, 5, 10, 20, 33, 50, 100)
  colorVector <- NULL # -> MisaColor
  LegendTitle <- "Zeitanteil mit mehr als 10% Regenabfluss 2002-2022 [%]"
}

# Zeitdauer Durchfluss < 0
if(output_table == "def_hours"){
  output_column <- "below_0"
  classBreaks <- c(0, 1, 10, 15, 25, 50, 75, 100)
  colorVector <- c("deepskyblue4", "gold", "orange", "darkorange3", "red", "red3", "darkred") # "dodgerblue4"
  LegendTitle <- "Zeitanteil mit Durchfluss<0 in 2019 [%]"
}

# Zeitdauer Fluoranthen > x µg/L
if(output_table == "def_hours"){
  output_column <- "above_0.12"
  classBreaks <- c(0, 1, 10, 20, 40, 60, 80, 100)
  colorVector <- c("deepskyblue4", "gold", "orange", "darkorange3", "red", "red3", "darkred") # "dodgerblue4"
  LegendTitle <- "Zeitanteil mit Konzentration Fluoranthen >0.12µg/L = ZHK-UQN (2017-2022) [%]"
}

# Valsartansäure
if(output_table == "stats"){
  output_column <- "mean"
  classBreaks <- c(0, 0.25, 0.5, 1, 2.5, 4, 6)
  colorVector <- NULL
  LegendTitle <- "Konzentration Valsartansäure [µg/L]"
}

# Fluoranthen
if(output_table == "stats"){
  output_column <- "mean"
  classBreaks <- c(0, 0.0063, 0.04, 0.08, 0.12, 0.3, 0.5)
  colorVector <- c("deepskyblue4", "gold", "orange", "darkorange3", "red", "darkred")
  LegendTitle <- "Konzentration Fluoranthen 2017-2022, Mittelwert [µg/L]"
}

##########################################################################################################
# Create map #############################################################################################
##########################################################################################################

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
  filename = "WaterModelPlot_Fuoranthen_2017-2022_Zeitanteil_above_ZHK-UQN",
  path = file.path(project_path, data_path),
  type = "", # "vector" = svg-file
  resolution = "low"
)

# save as svg (vector graphic)
qsimVis::saveActiveDevice(
  filename = "WaterModelPlot",
  path = file.path(project_path, data_path),
  type = "vector"
)

# Write output table
# writexl::write_xlsx(x = output, path = file.path(path, "Viewer_Skript", "output_table.xlsx"))




