library(here)
library(dplyr)
library(purrr)
library(tsibble)
library(urca)
library(fable)
library(fabletools)

source(here("R", "04_protocol.R"))

# Fonction de prévision pour un fold

forecast_stat_fold <- function(
    fold_id,
    train_end,
    test_start,
    test_end
) {
  
  train_data <- model_data |>
    dplyr::filter(
      date <= train_end
    ) |>
    dplyr::select(
      serie_id,
      date,
      sales
    ) |>
    tsibble::as_tsibble(
      key = serie_id,
      index = date
    )
  
  
  test_data <- model_data |>
    dplyr::filter(
      date >= test_start,
      date <= test_end
    ) |>
    dplyr::select(
      serie_id,
      date,
      actual = sales
    )
  
  
  fitted_models <- train_data |>
    fabletools::model(
      
      snaive = fable::SNAIVE(
        sales ~ lag("week")
      ),
      
      ets = fable::ETS(
        sales ~ season(
          period = 7
        )
      ),
      
      sarima = fable::ARIMA(
        sales ~
          pdq() +
          PDQ(period = 7),
        stepwise = TRUE,
        approximation = FALSE
      )
    )
  
  
  forecasts <- fitted_models |>
    fabletools::forecast(
      h = horizon
    ) |>
    dplyr::left_join(
      test_data,
      by = c(
        "serie_id",
        "date"
      )
    ) |>
    dplyr::mutate(
      fold_id = fold_id,
      train_end = train_end,
      test_start = test_start,
      test_end = test_end
    )
  
  
  forecasts
}

# Test sur un seul fold

test_fold <- validation_folds |>
  dplyr::slice(1)

test_forecasts <- purrr::pmap_dfr(
  test_fold,
  forecast_stat_fold
)


# 5. Contrôles
test_forecasts |>
  dplyr::count(
    .model,
    serie_id
  )

sum(is.na(test_forecasts$.mean))
sum(is.na(test_forecasts$actual))

# Validation complète sur les 17 folds

stat_cv <- purrr::pmap_dfr(
  validation_folds,
  forecast_stat_fold
)

# Contrôles de la validation complète

stat_cv |>
  dplyr::count(
    fold_id,
    .model
  )

stat_cv |>
  dplyr::count(
    fold_id,
    .model
  ) |>
  dplyr::filter(
    n != 4 * horizon
  )

sum(is.na(stat_cv$.mean))
sum(is.na(stat_cv$actual))

nrow(stat_cv)


# Sauvegarde des prévisions

saveRDS(
  stat_cv,
  here(
    "outputs",
    "tables",
    "statistical_cv_forecasts.rds"
  )
)

nrow(stat_cv)