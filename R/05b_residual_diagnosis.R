library(here)
library(dplyr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)

source(here("R", "04_protocol.R"))

# Données disponibles avant le holdout final

diag_train <- model_data |>
  dplyr::filter(
    date <= holdout$train_end
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

# Ajustement des modèles statistiques

diag_models <- diag_train |>
  fabletools::model(
    
    snaive = fable::SNAIVE(
      sales ~ lag("week")
    ),
    
    ets = fable::ETS(
      sales ~ season(period = 7)
    ),
    
    sarima = fable::ARIMA(
      sales ~
        pdq() +
        PDQ(period = 7),
      stepwise = TRUE,
      approximation = FALSE
    )
  )

diag_models

# Extraction des résidus / innovations

diag_residuals <- diag_models |>
  fabletools::augment()

diag_residuals

## Contenu de l'objet
names(diag_residuals)

# Diagnostics complet fitted values
## 1. Résidus centrés autour de zéro


diag_summary <- diag_residuals |>
  dplyr::as_tibble() |>
  dplyr::group_by(
    serie_id,
    .model
  ) |>
  dplyr::summarise(
    n = sum(!is.na(.innov)),
    moyenne_innov = mean(.innov, na.rm = TRUE),
    ecart_type_innov = sd(.innov, na.rm = TRUE),
    .groups = "drop"
  )

diag_summary

## 2. Tests portmanteau Ljung-Box

diag_ljung <- diag_residuals |>
  dplyr::filter(
    !is.na(.innov)
  ) |>
  features(
    .innov,
    ljung_box,
    lag = 28,
    dof = 0
  )

diag_ljung


## 3. ACF des innovations

diag_acf <- diag_residuals |>
  dplyr::filter(
    !is.na(.innov)
  ) |>
  feasts::ACF(
    .innov,
    lag_max = 28
  )

diag_acf

### Graph ACF
library(ggplot2)
library(ggtime)

ggtime::autoplot(diag_acf)

### Graph Normalite et homoscedasticite

diag_residuals |>
  dplyr::filter(!is.na(.innov)) |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = date,
      y = .innov
    )
  ) +
  ggplot2::geom_point(
    alpha = 0.25,
    size = 0.5
  ) +
  ggplot2::facet_grid(
    serie_id ~ .model,
    scales = "free_y"
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  ggplot2::labs(
    title = "Innovations des modèles statistiques",
    x = NULL,
    y = "Innovation"
  )