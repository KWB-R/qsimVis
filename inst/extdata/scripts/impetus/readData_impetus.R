# Documentation of aggregation and visualization of Qsim results
path <- "C:/Users/mzamzo/Documents/impetus"

# load and prepare qsim data
df_in1 <- qsimVis::QSIM_prepare(
  qsim_output_file = file.path(path, "input/qsim_export",
    "S0 _Abwasseranteile_Alle_Stationen_Spandau.csv"
  ), parameter_name = "KONSSY"
)
df_in2 <- qsimVis::QSIM_prepare(
  qsim_output_file = file.path(path, "input/qsim_export",
                               "T1 S0_Abwasseranteile_Alle_Stationen_T1.csv"
  ), parameter_name = "KONSSY"
)
df_in3 <- qsimVis::QSIM_prepare(
  qsim_output_file = file.path(path, "input/qsim_export",
                               "T3 S0_Abwasseranteile_Alle_Stationen_T3.csv"
  ), parameter_name = "KONSSY"
)

df_pro <- Reduce(
  f = function(df1, df2){merge(df1, df2, by = "posixDateTime")},
  x = list(df_in1$para, df_in2$para, df_in3$para))

df_pro$best <- 0

# Aggregate data
output <- list(
  "def_hours" =
    qsimVis::deviating_hours(
    dataFrame = df_pro,
    thresholds = c(1, 2, 4, 8,16),
    dev_type = "egt"),
  "adv_deviation" =
    qsimVis::adverse_deviation_from_reference(
      dataFrame = df_pro,
      reference = "best",
      worst = 100,
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



qsimVis::save_as_excel(
  list_of_aggregated_data = output,
  path = file.path(path, "output", "output_table.xlsx"))




