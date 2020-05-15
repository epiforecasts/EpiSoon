#' @importFrom stats median quantile rpois rt sd setNames

globalVariables(
  c("timeseries",
    "bottom",
    "cases",
    "calibration",
    "ci",
    "crps",
    "dss",
    "forecast_date",
    "hdi_50",
    "hdi_90",
    "horizon",
    "index",
    "infectiousness",
    "iqr",
    "logs",
    "lower",
    "model_name",
    "obs_sample",
    "row_id",
    "score",
    "time",
    "top",
    "upper",
    "time",
    "rts",
    "value",
    "y",
    "model")
)

check_suggests <- function(pkg_name){
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    msg <- sprintf("This function requires `%s` to work. Please install it.\n", pkg_name)
    stop(msg,
         call. = FALSE)
  }
}
