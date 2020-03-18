#' Iteratively fit a BSTS model to each timepoint
#'
#'
#' @return
#' @export
#' @inheritParams fit_model
#'
#' @importFrom purrr map_dfr safely
#' @importFrom dplyr filter
#' @examples
#'
#' rts <- data.frame(rt = 1:10,
#'                  date = as.Date("2020-01-01") + lubridate::days(1:10))
#'
#'
#' iterative_model_fit(rts, model = bsts::AddAutoAr, horizon = 7, samples = 1000)
iterative_model_fit <- function(rts,
                          model = NULL,
                          horizon = 7,
                          samples = 1000) {

  safe_fit <- purrr::safely(fit_model)

  samples <- purrr::map_dfr(rts$date, ~ safe_fit(dplyr::filter(rts, date <= .),
                                                     model = model,
                                                     samples = samples,
                                                     horizon = horizon)[[1]],
                        .id = "forecast_date")



  return(samples)
}







