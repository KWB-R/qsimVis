# Documentation of aggregation and visualization of Qsim results
library(qsimVis)

input_csv <- system.file(package = "qsimVis",
                       "extdata/example_data/fluoranthen_berlin_2021.csv")

input_csv <- file.path(
  "Y:/iGB/Projects/IMPETUS/Work-packages/WP4_Demonstration_KWB",
  "CS-Berlin/04_Modelling/OGewaesser/BerlinWaterModel/Ergebnisse/MapPlots",
  "qsimVis_input_hours_2017-2022_Fluoranthen_KW-0,0037_RW-0,22_MWÜ-0,20_CSO-correlation.csv")

if(FALSE){
  # find out about column names --------------------------------------------------
  colNames <- read.csv(
    file = input_csv,
    header = FALSE, nrows = 1, sep = ";")
  print(colNames)

  # load and prepare qsim data
  df_in <- qsimVis::QSIM_prepare(
    qsim_output_file = input_csv,
    parameter_name = "Fluoranthen.mg.m3",
    date_column_name = "Datum",
    id_column_name = "GewaesserId",
    km_column_name = "Km",
    flow_column_name = "Q",
    section_column_name = "Strang",
    dec = ","
  )

  # 02.11.2020 - 07.11.2020
  plot_timeSeries(
    df_in = df_in,
    tBeg = "2020-11-02 00:00", # "2021-06-29 21:00"
    tEnd = "2020-11-07 07:00",
    classBreaks = c(0, 0.0063, 0.01, 0.02, 0.04, 0.08, 0.12, 0.5),
    colorVector = c("deepskyblue4", "gold", "orange", "darkorange3", "red", "darkred"),
    output_path = "C:/Users/mzamzo/Documents/tmp/qsimVis"
  )

  plot_timeSeries(
    df_in = df_in,
    tBeg = "2021-09-29 07:00", # "2021-06-29 21:00"
    tEnd = "2021-09-29 13:00",
    classBreaks = seq(0, 0.5, 0.005),
    colorVector = c("deepskyblue4", "gold", "orange", "darkorange3", "red", "darkred"),
    output_path = "C:/Users/mzamzo/Documents/tmp/qsimVis"
  )
}




plot_timeSeries <- function(
    df_in, tBeg, tEnd, output_path,
    classBreaks = c(0, 0.0063, 0.01, 0.02, 0.04, 0.08, 0.12, 0.5),
    colorVector = c("deepskyblue4", "gold", "orange", "darkorange3", "red", "darkred")
){
  df_pro <- df_in$para
  # Combine river stretch and simulations data
  mapping_table <- read.table(
    file = system.file(package = "qsimVis",
                       "extdata/scripts/impetus/BelinWaterModel_id_table.csv"),
    header = TRUE,
    sep = ";"
  )
  tDates <- seq(as.POSIXct(tBeg), as.POSIXct(tEnd), by = 1 * 60 * 60)
  for(i in seq_along(tDates)){
    tDate <- tDates[i]
    df_date <- df_pro[df_pro$posixDateTime == tDate,]
    output <- list(
      "stats" = qsimVis::stats(
        dataFrame = df_date)
    )
    rivers <- qsimVis::prepare_rivers(
      mapping_table = mapping_table,
      aggregated_data = output[["stats"]],
      value_column = "mean",
      path_manual = system.file(package = "qsimVis", "extdata/manually_added_rivers"),
      gap_filling = "steps"
    )
    # add classes and colors
    rivers <- qsimVis::value_to_classes(
      river_list = rivers,
      classBreaks = classBreaks,
      colorVector = colorVector
    )
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
      text = paste0("Fluoranthen ", format(tDate, "%d.%m.%Y")),
      side = 3, line = 1, cex = 1.2, font = 1
    )

    # Add colored Rivers
    qsimVis::add_coloredRivers(
      ext_rivers = rivers
    )
    #if(tDate == rev(tDates)[1]){
      # last plot only
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
    #}


    qsimVis::saveActiveDevice(
      filename = paste0("Fluoranthen_", i),
      path = output_path,
      type = "png", # "vector" = svg-file
      resolution = "medium"
    )
    dev.off()
  }
}





