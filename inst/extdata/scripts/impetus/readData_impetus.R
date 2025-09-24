# Documentation of aggregation and visualization of Qsim results in IMPETUS

# Select path and file names --------------------------------------------------
path <-
  # "C:/Users/dwicke/Documents/work/IMPETUS/Work-packages/WP4_Demonstration_KWB/CS-Berlin/04_Modelling/OGewaesser"
  "Y:/iGB/Projects/IMPETUS/Work-packages/WP4_Demonstration_KWB/CS-Berlin/04_Modelling/OGewaesser"
data_subpath <- "Daten/SenUMVK/Daten_Masterplan_Wasser/Daten_Schumacher"

file1 <-
  # "S0 _Abwasseranteile_Alle_Stationen_Spandau.csv"
  # "S3.25_Abwasseranteile_Alle_Stationen_Spandau.csv"
  "S3.50_Abwasseranteile_Alle_Stationen_Spandau.csv"

file2 <-
  "T1 S0_Abwasseranteile_Alle_Stationen_T1.csv"
  # "T1 S3.25_Abwasseranteile_Alle_Stationen_T1.csv"
  # "T1 S3.50_Abwasseranteile_Alle_Stationen_T1.csv"
  # "T1 S3.75_Abwasseranteile_Alle_Stationen_T1.csv"

file3 <-
  # "T3 S0_Abwasseranteile_Alle_Stationen_T3.csv"
  # "T3 S3.25_Abwasseranteile_Alle_Stationen_T3.csv"
  # "T3 S3.50_Abwasseranteile_Alle_Stationen_T3.csv"
  "T3 S3.75_Abwasseranteile_Alle_Stationen_T3.csv"

qsim_output_file_list <- list(
  file.path(path, data_subpath, "Teilmodell_T4-SH_Spandau", file1),
  file.path(path, data_subpath, "Teilmodell_T1-SH_Mühlendamm", file2),
  file.path(path, data_subpath, "Teilmodell_T3-SH_Brandenburg", file3)
)

# find out about column names --------------------------------------------------
colNames <- read.csv(
  file = file.path(path, data_subpath, "Teilmodell_T4-SH_Spandau", file1),
  header = FALSE, nrows = 1, sep = ";")
print(colNames)

# load and prepare qsim data ---------------------------------------------------
model_data <- qsimVis::QSIM_prepare_multiple(
  qsim_output_file_list = qsim_output_file_list,
  parameter_name = "KONSSY",
  date_column_name = "Datum",
  id_column_name = "GewaesserId",
  km_column_name = "Km",
  flow_column_name = "Q",
  section_column_name = "Strang")
gc()

# Aggregate data
df_para <- model_data$para
df_para$best <- 0

output <- list(
  "def_hours" =
    qsimVis::deviating_hours(
    dataFrame = df_para,
    thresholds = c(10, 20, 40, 60, 80)/100,
    #thresholds = c(1, 2, 4, 8, 16),
    dev_type = "egt"),
  "adv_deviation" =
    qsimVis::adverse_deviation_from_reference(
      dataFrame = df_para,
      reference = "best",
      worst = 1,
      good_values = "low"))

head(output$def_hours)

,
  "crit_events" = qsimVis::critical_events(
    dataFrame = df_pro,
    deficiency_hours = 0.5,
    separating_hours = 12,
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

output <- lapply(output, function(x){
  qsimVis::add_site_info(df_in = x, v_qsim_ids = rownames(x))
})
head(output$adv_deviation)

writexl::write_xlsx(x = output, path = file.path(path, "Viewer_Skript", "output_table.xlsx"))

#qsimVis::save_as_excel(
#  list_of_aggregated_data = output,
#  path = file.path(path, "Viewer_Skript", "output_table.xlsx"))




