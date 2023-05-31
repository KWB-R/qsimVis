berlin_rivers <- qsimVis::load_rivers(
  path = file.path("C:/Users/mzamzo/Documents/R/win-library/4.1",
                   "qsimVis/extdata/berlin_data"))


# plot empty map
qsimVis::plot_empty_map(rivers = berlin_rivers)

# Add Shape Background
add_polygons()

# Add colored Rivers
qsimVis::add_coloredRivers(
  rivers = berlin_rivers,
  output_table = output$def_hours,
  varName = "below_1.5",
  sixBreaks = c(0, 0.5, 2, 4, 10, 20),
  dataType = "time",
  LegendTitle = "Unterschreitunsdauer\n in h (Grenzwerte in mg/L)")

# Add CSO volume
qsimVis::add_inflow(
  filname = file.path(
    "C:/Users/mzamzo/Documents/R/git",
    "qsimVis/inst/extdata/cso_data",
    "E2_2011_20221209_14-52_stats.csv"),
  varName = "tVol_m3",
  sizeMax = 100000)


# Add flow (average during time period or before event)

# Path for saving
saving_path <- file.path(scenario_path, "5_assessment_output")


