save_table <- function(data, filename) {
  
  readr::write_csv(
    data,
    here::here(
      "outputs",
      "tables",
      paste0(filename, ".csv")
    )
  )
  
  saveRDS(
    data,
    here::here(
      "outputs",
      "tables",
      paste0(filename, ".rds")
    )
  )
}

save_table(
  holdout_point_global,
  "holdout_point_global"
)

save_table(
  holdout_point_series,
  "holdout_point_by_series"
)

save_table(
  holdout_pinball_global,
  "holdout_pinball_global"
)

save_table(
  holdout_interval_global,
  "holdout_probabilistic_intervals"
)

save_table(
  holdout_matched_policy,
  "holdout_decision_matched_policy"
)

