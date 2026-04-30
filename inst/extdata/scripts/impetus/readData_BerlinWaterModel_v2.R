# Documentation of aggregation and visualization of Qsim results
library(qsimVis)

project_path <-
  #  "Y:/iGB/Projects/IMPETUS/"
   "C:/Users/dwicke/Documents/work/IMPETUS"
 #  "C:/Users/dwicke/Documents/R/Github"

data_path <- "Work-packages/WP4_Demonstration_KWB/CS-Berlin/04_Modelling/OGewaesser/BerlinWaterModel/Ergebnisse/MapPlots"
# file_name <-  "qsimVis_input_days_test_250905.csv"
# data_path <- "kwb.BerlinWaterModel"
file_name <- "qsimVis_input_days_2002-2022_Valsartansäure-Äq_no-OWA_ohne_Ozonung_mit-Zuflusskonz-OSK_V4.csv"

# find out about column names --------------------------------------------------
colNames <- read.csv(
  file = file.path(project_path, data_path, file_name),
  header = FALSE, nrows = 1, sep = ";")
print(colNames)

# load and prepare qsim data
df_in <- qsimVis::QSIM_prepare(
  qsim_output_file = file.path(project_path, data_path, file_name),
  parameter_name = "ValsartansaeureAeq.mg.m3", # "Fluoranthen.mg.m3", "ValsartansaeureAeq.mg.m3", "Valsartan.mg.m3" "tracer.wwtp", "tracer.rain"
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
#    deficiency_hours = 24,
#    separating_hours = 0,
    deficiency_hours = 1,
    separating_hours = 6,
    threshold = 0.12,
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
  #"crit_events"

if(FALSE) {
head(output$adv_deviation)
head(output$def_hours)
head(output$stats)
head(output$crit_events)
head(output$flow_mean)
}

##########################################################################################################
# Configuration of maps ##################################################################################
##########################################################################################################

# Zeitdauer Abwasseranteil > x%
if(output_table == "def_hours"){
  output_column <- "above_0.12"
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
#  classBreaks <- c(0, 0.15, 0.3, 0.5, 1, 2, 3, 4, 6)
  classBreaks <- c(0, 0.25, 0.5, 1, 2, 3, 4, 6, 10, 20)
  colorVector <- NULL
  LegendTitle <- "Konzentration Valsartansäure-äq (2002-2022) \n [µg/L]"
}

# Fluoranthen
if(output_table == "stats"){
  output_column <- "mean"
  classBreaks <- c(0, 0.0063, 0.04, 0.08, 0.12, 0.3, 0.5)
  colorVector <- c("deepskyblue4", "gold", "orange", "darkorange3", "red", "darkred")
  LegendTitle <- "Konzentration Fluoranthen 2017-2022, Mittelwert [µg/L]"
}

# Fluoranthen - Anzahl krit. events
if(output_table == "crit_events"){
  output_column <- "events"
  classBreaks <- c(0, 0.5, 5, 20, 100, 500)
  colorVector <- c("seagreen", "gold", "orange", "red", "darkred") # "darkgreen"
  LegendTitle <- "Anzahl Ereignisse >0.12 µg/L \n(ZHK-UQN)"
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

# plot data ############################################################################################

# qsimVis::plot_empty_map(rivers = rivers_ext, plot_toner = FALSE)
qsimVis::plot_empty_map(
  bbox = list(c(13, 13.8),
              c(52.35, 52.68))
)

# Add Shape Background
qsimVis::Berlin_add_boarder(
  bg_color = "gray90",
  frame = NA)
qsimVis::Berlin_add_waterbodies()

# Add Title
#mtext(text = "Relative Überschreitungsdauer der ZHK-UQN für Fluoranthen (2017-2022)", side = 3, line = 0.6, cex = 1.2, font = 1)
#mtext(text = "Anzahl Überschreitungen der ZHK-UQN für Fluoranthen (2017-2022)", side = 3, line = 0.6, cex = 1.2, font = 1)
#mtext(text = "Mittlere Konzentrationen für Fluoranthen (2017-2022)", side = 3, line = 0.6, cex = 1.2, font = 1)
mtext(text = "Mittlere Konzentration Valsartansäure-äq, ohne Ozonung\n(Zufluss [µg/L]: Spree & OSK=0.46, Dahme=0.36, Havel=0.24, OWA=0)", side = 3, line = 0.6, cex = 1.19, font = 1)
#mtext(text = "Ozonung in allen 5 Berliner Klärwerken und Reduktion Zuflusskonzentrationen", side = 3, line = 0.6, cex = 1.19, font = 1)

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
  fillColor = "steelblue3", # "skyblue3" "steelblue2"
  lineColor = "#25496B", # #9DD0FF",
  plotNames = FALSE,
  sw_connection = FALSE,
  rivers = rivers,
  pCex = 1.8,
  legendPosition = "topright")

# one single POI (--> KWB)
#qsimVis::add_POI(
#  lat = 52.488674,
#  lon = 13.342737,
#  lineColor = "black",
#  fillColor = "black",
#  pch = 20,
#  cex = 1,
#  textbg = "orange",
#  title = "KWB"
#)

# Add Logos
qsimVis::add_logo(
  logo_filename = "KWB_Logo.png",
  position = "bottomright",
  size = 0.8,
  indent = 0.1,
  bg_col = rgb(red = 1, green = 1, blue = 1, alpha = 0)  # alpha = 0 behält transparenten Hintergrund
)

qsimVis::add_logo(
  logo_filename = "IMPETUS_Logo.png",
  position = "bottomleft",
  size = 1.8,
  indent = 0.1,
  bg_col = rgb(red = 1, green = 1, blue = 1, alpha = 0)  # alpha = 0 behält transparenten Hintergrund
)

# Add legend
qsimVis::add_river_legend(
  ext_rivers = rivers,
  LegendTitle = LegendTitle,
  LegendLocation = "right"
)

# Save as png
qsimVis::saveActiveDevice(
  filename = "WaterModelPlot_Valsartansäure_2002-2022_ohne_Ozonung_mit-POI2",
  path = file.path(project_path, data_path),
  type = "png", # "vector" = svg-file
  resolution = "medium"
)

# save as svg (vector graphic)
qsimVis::saveActiveDevice(
  filename = "WaterModelPlot",
  path = file.path(project_path, data_path),
  type = "vector"
)


output_clean <- output$stats %>%
  dplyr::select("section_id", "section_name", "mean", "sd", "median", "q90") %>%
  dplyr::mutate(dplyr::across(where(is.numeric), \(x) round(x , 3))) %>%
  dplyr::arrange(section_id) %>%
  dplyr::group_by(section_id, section_name) %>%
  dplyr::summarize(dplyr::across(where(is.numeric), \(x) mean(x)))

# Write output table
openxlsx::write.xlsx(x = output_clean, "results_sections_2002-2022_Valsartansäure-Äq_no-OWA_Ozonung-alle-KW_mit-Zuflusskonz-OSK.xlsx")
# writexl::write_xlsx(x = output, path = file.path(path, "Viewer_Skript", "output_table.xlsx"))



