## Example serial interval
mean_si <- 4.7
sd_si <- 2.9

mu_log <- log(mean_si) - 1 / 2 * log((sd_si / mean_si)^2 + 1)
sd_log <- sqrt(log((sd_si / mean_si)^2 + 1))


example_serial_interval <- rlnorm(1:100, mu_log, sd_log) %>%
  round(0) %>%
  table() %>%
  {
    . / sum(.)
  } %>%
  c(0, .)

usethis::use_data(example_serial_interval, overwrite = TRUE)
