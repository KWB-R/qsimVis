# Documentation of aggregation and visualization of Qsim results

path <- "C:/Users/dwicke/Documents/work/IMPETUS/Work-packages/WP4_Demonstration_KWB/CS-Berlin/04_Modelling/OGewaesser"
# path <- "C:/Users/mzamzo/Documents/impetus"

# load and prepare qsim data
df_in <- qsimVis::QSIM_prepare(
  qsim_output_file = file.path(path,
                               "BerlinWaterModel/Ergebnisse",
                               "qsimVis_input.csv"),
  parameter_name = "tracer.wwtp"
)

df_pro <- df_in$para %>%
  dplyr::mutate(best = 0)

# Aggregate data
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
      reference = "best",
      worst = 1,
      good_values = "low"))

head(output$def_hours)

# ,
#   "crit_events" = qsimVis::critical_events(
#     dataFrame = df_pro,
#     deficiency_hours = 0.5,
#     separating_hours = 5 * 24,
#     threshold = 1.5,
#     recovery_value = NULL,
#     return_event_positions = FALSE),
#   "crit_periods" = qsimVis::critical_events(
#     dataFrame = df_pro,
#     deficiency_hours = 0.5,
#     separating_hours = 5 * 24,
#     threshold = 1.5,
#     recovery_value = NULL,
#     return_event_positions = TRUE))

output <- lapply(output, function(x){
  qsimVis::add_site_info(df_in = x, v_qsim_ids = rownames(x))
})
head(output$adv_deviation)

writexl::write_xlsx(x = output, path = file.path(path, "Viewer_Skript", "output_table.xlsx"))

#qsimVis::save_as_excel(
#  list_of_aggregated_data = output,
#  path = file.path(path, "Viewer_Skript", "output_table.xlsx"))




