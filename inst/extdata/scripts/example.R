# Documentation of aggregation and visualization of Qsim results

# load and prepare qsim data
df_in <- qsimVis::QSIM_prepare(
  qsim_output_file = system.file(
    package = "qsimVis", "extdata/qsim_data/qsim_example.csv"),
  parameter_name = "VO2"
)

# select one of the listed events
e_data <- qsimVis::load_events(
  filename = system.file(package = "qsimVis", "extdata/event_data/events.csv")
)
event_time <- e_data[1,1:3]

# 2. Filter Data per event
df_pro <- qsimVis::filter_parameter_data(
    dataFrame = df_in[["para"]],
    tBeg = event_time[,"tBeg"],
    tEnd = event_time[,"tEnd"],
    sites = "") # all sites are included

qsimVis::deficiency_hours(
  dataFrame = df_pro)


kwb.misa::misa_prepare_data(
  df_MiSa = df_pro,
  res = 15, # temporal resolution in minutes
  max_na_interpolation = 60/15
)

# Reference for neg_dev is "Oberhalb Abzweig LWK"
dl_misa <- lapply(data_comp_per_event, function(df_event){
  print(head(df_event))
  # 3. Manipulated Data
  dl <- kwb.misa::misa_prepare_data(
    df_MiSa = df_event,
    res = 15, # temporal resolution in minutes
    max_na_interpolation = 60/15) # 4 missing values a 15 mins  -> one hour max

  # 4. Assess Data
  list(
    "hours" = do.call(rbind, lapply(X = dl, kwb.misa::yearly_deficiency_time,
                                    max_missing = 100, thresholds = c(0.5, 1, 1.5, 2, 3))),
    "events" = do.call(rbind, lapply(X = dl, kwb.misa::yearly_crit_Events, max_missing = 100)),
    "neg_dev" = do.call(rbind, lapply(X = dl, kwb.misa::yearly_negative_deviation,
                                      oxygen_ref = dl[["SOW_S106.SOW_21.2"]]$d, max_missing = 100)))
})

# 5. Add site info to the misa assessment (based on row names)
dl_misa <- lapply(dl_misa, kwb.misa:::siteInfo_from_QsimName)

# 6. Aggregate events
df_aggr <- kwb.misa:::aggregate_eventSeries(dl_misa = dl_misa)

rm(list = setdiff(x = ls(), list("df_aggr", "dl_misa", "scenario", "scenario_path")))

save.image(file.path(
  scenario_path,
  "5_assessment_output",
  paste0("misa_tool_", scenario, ".RData"))
)

