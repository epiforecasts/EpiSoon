


iterative_case_forecast <- function(rts = NULL, cases = NULL,
                                    serial_interval = NULL, horizon = NULL) {

  predictions <- purrr::map_dfr(cases$date, function(target_date) {
    EpiSoon::predict_cases(cases, rts, serial_interval = serial_interval,
                           horizon = horizon, forecast_date = target_date) %>%
      dplyr::mutate(forecast_date = target_date) %>%
      dplyr::select(forecast_date, date, cases) %>%
      dplyr::mutate(horizon = as.numeric(date - forecast_date))})

  return(predictions)
}


score_case_forecast <- function(pred_cases, obs_cases) {
  pred_cases <- pred_cases %>%
    dplyr::rename(rt = cases)

  obs_cases <- obs_cases %>%
    dplyr::rename(rt = cases)

  scores <- EpiSoon::score_forecast(pred_cases, obs_cases)

  return(scores)
}


rts <- out %>%
  tidyr::unnest(R) %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(sample = 1:dplyr::n()) %>%
  dplyr::ungroup() %>%
  dplyr::rename(rt = R)

preds <- rts %>%
  dplyr::group_split(sample) %>%
  purrr::map_dfr(~iterative_case_forecast(rts = dplyr::select(., -sample),
                                          cases = cases,
                                          serial_interval = serial_intervals[, 1],
                                          horizon = 1),
                 .id = "sample") %>%
  dplyr::mutate(sample = as.numeric(sample)) %>%
  dplyr::select(forecast_date, date, cases, horizon, sample)


scores <- preds %>%
  dplyr::group_split(forecast_date) %>%
  setNames(unique(preds$forecast_date)) %>%
  purrr::map_dfr(~score_case_forecast(dplyr::select(., -forecast_date),
                                      cases), .id = "forecast_date")

summarised_score <- scores %>%
  dplyr::summarise(crps = mean(crps, na.rm = TRUE))
