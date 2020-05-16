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
    "model",
    "sample_nr",
    "geography",
    "y_pred")
)

check_suggests <- function(pkg_name, dev_message = NULL){
  if (!requireNamespace(pkg_name, quietly = TRUE)) {
    msg <- sprintf("This function requires `%s` to work.",
                   pkg_name)

    if (!is.null(dev_message)) {
      msg <- paste(msg, dev_message)
    } else{
      msg <- paste(msg, "Please install it.\n")
    }
    stop(msg, call. = FALSE)
  }
}





