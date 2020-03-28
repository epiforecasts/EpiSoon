
# packages ----------------------------------------------------------------

library(dplyr)
library(usethis)

# Add rts  ----------------------------------------------------------------


example_obs_rts <- readRDS("data-raw/obs_rts.rds") %>%
  dplyr::filter(timeseries %in% "austria") %>%
  dplyr::select(-timeseries) %>%
  dplyr::slice(-c(1:29))


usethis::use_data(example_obs_rts, overwrite = TRUE)



# Add cases data -------------------------------------------------------

example_obs_cases <- readRDS("data-raw/obs_cases.rds") %>%
  dplyr::filter(timeseries %in% "austria") %>%
  dplyr::select(-timeseries) %>%
  dplyr::slice(1:63)


usethis::use_data(example_obs_cases, overwrite = TRUE)



