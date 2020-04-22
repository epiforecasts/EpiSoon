# Preparation for all the tests

# draw_from_si_prob
# Cette fonction tire les résultats des jours demandés. Elle fait en fait
# simplement serial_interval[days_ago], mais en vérifiant d'abord que les
# days ago sont dans l'invervalle, sinon elle retourne 0.
# Il n'y a pas de jour 13 dans EpiSoon::example_serial_interval.
# Liste des choses à tester
## Dans la fonction
#   - Que days_ago est numérique
#   - Que serial interval est un vecteur de numériques
## Dans les tests
#   - Que la fonction renvoie le bon résultat
#   - Que le résultat a la bonne classe

exampleResult <- draw_from_si_prob(c(1, 2, 4, 10),
                                   EpiSoon::example_serial_interval)
test_that("The expected draw is obtained", {
  expect_equal(exampleResult, c(0, 0.03, 0.17, 0.03))
  expect_identical(class(exampleResult), "numeric")
})

# predict_cases
# Cette fonction a pour but de prédire les cas d'un seul échantillon de forecast
# Rt. Pour celà, il faut lui fournir une dataframe avec deux colonnes date et
# cases, une autre df contenant les colonnes rt et date, avec rt en numeric et
# date en format date, un vecteur de numériques décrivant la distribution des
# probabilités de l'intervale. Les autres me semblent facultatives, mais c'est
# à vérifier.
# Il manque un "s" à EpiNow::covid_serial_interval dans l'aide
# Ils demandent des df mais renvoient des tibles.
# Liste des choses à tester
## Dans la fonction
#   - Que cases est une df avec les colonnes date et cases
#   - Que rts est une df avec les colonnes rt (en numérique) et date (en date)
#   - Que serila_interval est un vecteur de numériques
#   - Que forecast_date est une date
#   - Que horizon est numérique
#   - Que rdist est une fonction comme demandée
## Dans les tests
#   - Que la fonction renvoie le bon résultat
#   - Que le résultat a la bonne classe
### J'ai un gros problème ici pour vérifier que j'obtiens ce que je suis sensé
### avoir, car apparemment on ne peut pas fixer la seed. Donc chaque prédiction
### change. Je peux alors mettre un threshold sur une corrélation, mais à
### quel niveau d'erreur on est ok ? Est-ce que je bootstrap ?
set.seed(1234)

forecast <- forecast_rt(EpiSoon::example_obs_rts[1:10, ],
                        model = function(...) {EpiSoon::bsts_model(model =
                                function(ss, y){
                                  bsts::AddSemilocalLinearTrend(ss, y = y)},
                                ...)},
                        horizon = 7, samples = 1)


predictedCases <- predict_cases(cases = EpiSoon::example_obs_cases,
                                rts = forecast,
                                forecast_date = as.Date("2020-03-10"),
                                serial_interval = example_serial_interval) # Manque Episoon::

expectedTable <- tibble::as_tibble(
  data.frame(
    date = as.Date(c("2020-03-11", "2020-03-12", "2020-03-13", "2020-03-14",
                     "2020-03-15", "2020-03-16", "2020-03-17")),
    cases = c(120, 204, 132, 131, 219, 151, 193)
  )
)

test_that("The expected Rt sample forecasts predict cases are obtained", {
  expect_equal(predictedCases$date, expectedTable$date)
  expect_gte(cor(predictedCases$cases, expectedTable$cases), .9)
})

# predict_current_cases
# Même chose que la fonction précédente, il faut que la fonction vérifie que
# chaque input est dans le bon format. Je vais ici tester seulement les outputs.
# Encore ici, on ne peut fixer la seed, donc on va devoir tester avec la
# corrélation.
predictedCurrentCases <- predict_current_cases(
  cases = EpiSoon::example_obs_cases,
  rts = EpiSoon::example_obs_rts,
  serial_interval = EpiSoon::example_serial_interval
)

expectedCurrentCases <- data.frame(
  rt = EpiSoon::example_obs_rts$rt,
  date = EpiSoon::example_obs_rts$date,
  infectiousness = c(14.145, 14.495, 14.925, 15.435, 16.635, 18.705, 22.805,
                     27.555, 41.465, 60.965, 74.735, 104.655, 151.405, 158.155,
                     158.155, 160.445, 162.955, 165.845, 172.575, 184.005,
                     209.880, 223.155),
  cases = c(30, 53, 36, 37, 33, 39, 54, 63, 90, 119, 152, 201, 302, 337, 318,
            282, 320, 296, 298, 311, 359, 349)
)

test_that("The expected Rts based on observed data predict cases are obtained", {
  expect_equal(predictedCurrentCases$rt, expectedCurrentCases$rt)
  expect_equal(predictedCurrentCases$date, expectedCurrentCases$date)
  expect_equal(predictedCurrentCases$infectiousness,
               expectedCurrentCases$infectiousness)
  expect_gte(cor(predictedCurrentCases$cases, expectedCurrentCases$cases), .9)
})
