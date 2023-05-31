# Documentation of aggregation and visualization of Qsim results

# load and prepare qsim data
df_in <- qsimVis::QSIM_prepare(
  qsim_output_file = system.file(
    package = "qsimVis", "extdata/qsim_data/qsim_example.csv"),
  parameter_name = "VO2"
)

# select one event
e_data <- qsimVis::load_timeframes(
  filename = system.file(package = "qsimVis", "extdata/event_data/events.csv")
)
event_time <- e_data[1,1:3]

# filter data
df_pro <- qsimVis::filter_parameter_data(
    dataFrame = df_in[["para"]],
    tBeg = event_time[,"tBeg"],
    tEnd = event_time[,"tEnd"],
    sites = "") # all sites are included

df_pro$test <- 0

# Aggregate data
output <- list(
  "def_hours" =
    qsimVis::deviating_hours(
    dataFrame = df_pro,
    thresholds = c(0.5, 1, 1.5, 2, 3),
    dev_type = "elt"),
  "adv_deviation" =
    qsimVis::adverse_deviation_from_reference(
      dataFrame = df_pro,
      reference = "SOW_S106.SOW_21.2",
      worst = 0),
  "crit_events" = qsimVis::critical_events(
    dataFrame = df_pro,
    deficiency_hours = 0.5,
    separating_hours = 5 * 24,
    threshold = 1.5,
    recovery_value = NULL,
    return_event_positions = FALSE),
  "crit_periods" = qsimVis::critical_events(
    dataFrame = df_pro,
    deficiency_hours = 0.5,
    separating_hours = 5 * 24,
    threshold = 1.5,
    recovery_value = NULL,
    return_event_positions = TRUE))


qsimVis::save_as_excel(
  list_of_aggregated_data = output,
  path = "output_table.xlsx") # saved in the curent working directory -> getwd()
getwd()



