# Documentation of aggregation and visualization of Qsim results
library(qsimVis)

example <- system.file(package = "qsimVis",
                         "extdata/example_data/fluoranthen_berlin_2021.csv")

# find out about column names --------------------------------------------------
colNames <- read.csv(
  file = example,
  header = FALSE, nrows = 1, sep = ";")
print(colNames)

# load and prepare qsim data
df_in <- qsimVis::QSIM_prepare(
  qsim_output_file = example,
  parameter_name = "Fluoranthen.mg.m3",
  date_column_name = "Datum",
  id_column_name = "GewaesserId",
  km_column_name = "Km",
  flow_column_name = "Q",
  section_column_name = "Strang",
  dec = "."
)

# Aggregate data
df_pro <- df_in$para  # df_in$para or df_in$flow
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

head(output$adv_deviation)
head(output$def_hours)
head(output$stats)
head(output$flow_mean)

################################################################################
# Configuration of maps ########################################################
################################################################################
output_table <- "stats"
output_column <- "mean"
classBreaks <- c(0, 0.0063, 0.01, 0.02, 0.04, 0.08, 0.12, 0.5)
colorVector <- c("deepskyblue4", "gold", "orange", "darkorange3", "red", "darkred")


################################################################################
# Load and process river data ##################################################
################################################################################

# Combine river stretch and simulations data
mapping_table <- read.table(
  file = system.file(package = "qsimVis",
                     "extdata/scripts/impetus/BelinWaterModel_id_table.csv"),
  header = TRUE,
  sep = ";"
)

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

################################################################################
# plot map #####################################################################
################################################################################

# plot data ####################################################################
# qsimVis::plot_empty_map(rivers = rivers_ext, plot_toner = FALSE)
qsimVis::plot_empty_map(
  bbox = list(c(13, 13.8),
              c(52.35, 52.68))
)

# Add Shape Background
# qsimVis::Berlin_add_boarder(bg_color = "#BFF2C1", frame = NA)
qsimVis::Berlin_add_boarder(
  bg_color = "gray90",
  frame = NA
)
qsimVis::Berlin_add_waterbodies()

# Add Title
mtext(
  text = "Mittlere Konzentration Fluoranthen (2021)",
  side = 3, line = 1, cex = 1.2, font = 1
)

# Add colored Rivers
qsimVis::add_coloredRivers(
  ext_rivers = rivers
)

qsimVis::Berlin_add_poi(
  poiType = "wwtp",
  poiTitle = "Kläranlagen",
  fillColor = "sienna3",
  lineColor =  "#6B3E3A", # "#FF796D"
  plotNames = FALSE,
  sw_connection = TRUE,
  rivers = rivers,
  pCex = 1.8,
  legendPosition = "topright",
  dashed_connection = FALSE)

qsimVis::Berlin_add_poi(
  poiType = "dwtp",
  poiTitle = "Wasserwerke",
  fillColor = "steelblue3",
  lineColor = "#25496B", ##9DD0FF",
  plotNames = TRUE,
  sw_connection = TRUE,
  rivers = rivers,
  pCex = 1.8,
  legendPosition = "topright")

# on single POI (--> KWB)
qsimVis::add_POI(
  lat = 52.488674,
  lon = 13.342737,
  lineColor = "black",
  fillColor = "black",
  pch = 20,
  cex = 1,
  textbg = "orange",
  title = "KWB"
)

# Add Logos
qsimVis::add_logo(
  logo_filename = "KWB_Logo.png",
  position = "bottomright",
  size = 0.8,
  indent = 0.1,
  bg_col = rgb(red = 1, green = 1, blue = 1, alpha = 0)  # alpha = 0 behält transparenten Hintergrund
)

# Add legend
qsimVis::add_river_legend(
  ext_rivers = rivers,
  LegendTitle = "Konzentration in µg/L",
  LegendLocation = "right"
)

path <- getwd()

# Save as png
qsimVis::saveActiveDevice(
  filename = "WaterModelPlot_Valsartansäure_2002-2022_Ozonung_alle_KW_2",
  path = path,
  type = "png", # "vector" = svg-file
  resolution = "medium"
)

# save as svg (vector graphic)
qsimVis::saveActiveDevice(
  filename = "WaterModelPlot",
  path = path,
  type = "vector"
)



