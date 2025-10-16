# Documentation of aggregation and visualization of Qsim results

project_path <-"Y:/wGB/Projects/2023_MISA5"
data_path <- "Data-Work packages/AP2_Szenarienrechnung/berechnungen"
scenario_name <- "S8"

# load scenario data
load(file = file.path(
  project_path,
  data_path,
  scenario_name,
  "5_assessment_output",
  paste0("misa_tool_", scenario_name, ".RData"))
)
# add site info
df_aggr$qsim_site <- gsub(pattern = "_", replacement = "__", x = df_aggr$qsim_site)
site_info <- lapply(df_aggr$qsim_site,  qsimVis::site_info_from_qsimID)
site_info <- do.call(rbind, lapply(site_info, as.data.frame))
df_aggr <- cbind(df_aggr, site_info)

# Select data
output_column <- "hours.below_1.5"
classBreaks <- c(0, 25, 50, 100, 200, 300, Inf)
colorVector <- NULL # -> MisaColor
LegendTitle <- "Unterschreitungsdauer in h (1,5 mg/L)"

#
# Combine river stretch and simulations data
mapping_table <- read.table(
  file = system.file(package = "qsimVis",
                     "extdata/scripts/misa/misa_id_table.csv"),
  header = TRUE,
  sep = ";")

rivers <- qsimVis::prepare_rivers(
  mapping_table = mapping_table,
  aggregated_data = df_aggr,
  value_column = output_column,
  path_manual = system.file(package = "qsimVis", "extdata/manually_added_rivers"),
  gap_filling = "interpolation"
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
  bbox = list(c(13.18, 13.47),
              c(52.45, 52.57))
)


# catchments <- c("Bln IX", "Bln VII", "Bln IV", "Bln V", "Bln VII", "Bln I", "Bln II", "Nkn I", "Nkn II")
catchments <- NULL
# qsimVis::Berlin_add_boarder()
qsimVis::Berlin_add_waterbodies(bg_color = "lightblue")
qsimVis::Berlin_add_catchments(
  plot_names = TRUE,
  highlight_catchments = catchments,
  highlight_style = "beige"
)


# Add colored Rivers
qsimVis::add_coloredRivers(
  ext_rivers = rivers
)

qsimVis::add_river_legend(
  ext_rivers = rivers,
  LegendTitle = LegendTitle,
  LegendLocation = "top", cex = 0.8
)

# Write output table
# writexl::write_xlsx(x = output, path = file.path(path, "Viewer_Skript", "output_table.xlsx"))






