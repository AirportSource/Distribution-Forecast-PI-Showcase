library(here)
library(dplyr)
library(tidyr)
library(ggplot2)

source(here("R", "04_protocol.R"))

# Chargement des previsions

stat_cv <- readRDS(
  here("outputs",
    "tables",
    "statistical_cv_forecasts.rds")
)

lgb_point_cv <- readRDS(
  here("outputs",
       "tables",
       "lightgbm_point_cv_forecasts.rds")
)

stat_cv_point <- tibble::as_tibble(stat_cv) # conversion en tibble pour eviter crash

# Controle\

nrow(stat_cv)
nrow(lgb_point_cv)

# Format commun

point_stat <- stat_cv_point |>
  dplyr::transmute(
    fold_id,
    serie_id,
    date,
    horizon_step = as.integer(date - test_start) + 1L,
    model = .model,
    actual,
    prediction_raw = .mean
  )


point_lgb <- lgb_point_cv |>
  dplyr::transmute(
    fold_id,
    serie_id,
    date = target_date,
    horizon_step,
    model = "lightgbm",
    actual,
    prediction_raw
  )


point_cv <- dplyr::bind_rows(
  point_stat,
  point_lgb
)

point_cv |>
  dplyr::count(model)

nrow(point_cv)


# Verification des previsions negatives

point_cv |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    min_prediction = min(prediction_raw),
    n_negative = sum(prediction_raw < 0),
    .groups = "drop"
  )

## on les ramene a 0

point_cv <- point_cv |>
  dplyr::mutate(
    prediction = pmax(prediction_raw, 0)
  )


# Echelles MASE / RMSSE sur le train de chaque fold (metrique off de M5)
## Lag 7 car reference naive saisonniere et a la maille journaliere un cycle c est 7 jorus

scales_cv <- purrr::pmap_dfr(
  validation_folds,
  function(fold_id, train_end, test_start, test_end) {
    
    model_data |>
      dplyr::filter(date <= train_end) |>
      dplyr::arrange(serie_id, date) |>
      dplyr::group_by(serie_id) |>
      dplyr::summarise(
        
        mase_scale = mean(
          abs(sales - dplyr::lag(sales, 7)),
          na.rm = TRUE
        ),
        
        rmsse_scale = mean(
          (sales - dplyr::lag(sales, 7))^2,
          na.rm = TRUE
        ),
        
        .groups = "drop"
      ) |>
      dplyr::mutate(
        fold_id = fold_id
      )
  }
)

#Ajout echelle a chaque prevision pour MASE RMSSE

point_cv <- point_cv |>
  dplyr::left_join(
    scales_cv,
    by = c(
      "fold_id",
      "serie_id"
    )
  )

# Calcul des erreurs

point_cv <- point_cv |>
  dplyr::mutate(
    error = actual - prediction,
    abs_error = abs(error),
    squared_error = error^2
  )


# Métriques par fold / série / modèle

point_metrics_fold <- point_cv |>
  dplyr::group_by(
    fold_id,
    serie_id,
    model
  ) |>
  dplyr::summarise(
    
    n = dplyr::n(),
    
    # Biais : positif = sous-prévision
    bias = mean(error),
    
    # Erreurs classiques
    mae = mean(abs_error),
    
    rmse = sqrt(
      mean(squared_error)
    ),
    
    # Erreurs mises à l'échelle
    mase = mean(abs_error) /
      dplyr::first(mase_scale),
    
    rmsse = sqrt(
      mean(squared_error) /
        dplyr::first(rmsse_scale)
    ),
    
    .groups = "drop"
  )


# global output

point_results_global <- point_metrics_fold |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    bias = mean(bias),
    mae = mean(mae),
    rmse = mean(rmse),
    mase = mean(mase),
    rmsse = mean(rmsse),
    .groups = "drop"
  ) |>
  dplyr::arrange(rmsse)

point_results_global


# Métriques par série

point_results_series <- point_metrics_fold |>
  dplyr::group_by(
    serie_id,
    model
  ) |>
  dplyr::summarise(
    bias = mean(bias),
    mae = mean(mae),
    rmse = mean(rmse),
    mase = mean(mase),
    rmsse = mean(rmsse),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    serie_id,
    rmsse
  )

point_results_series

# Metriques par semaine / fold / serie / modèle


point_metrics_week <- point_cv |>
  dplyr::mutate(
    forecast_week = ceiling(horizon_step / 7)
  ) |>
  dplyr::group_by(
    fold_id,
    serie_id,
    model,
    forecast_week
  ) |>
  dplyr::summarise(
    
    bias = mean(error),
    
    mase = mean(abs_error) /
      dplyr::first(mase_scale),
    
    rmsse = sqrt(
      mean(squared_error) /
        dplyr::first(rmsse_scale)
    ),
    
    .groups = "drop"
  )


# Moyenne sur les 17 folds et les 4 series


point_results_week <- point_metrics_week |>
  dplyr::group_by(
    model,
    forecast_week
  ) |>
  dplyr::summarise(
    bias = mean(bias),
    mase = mean(mase),
    rmsse = mean(rmsse),
    .groups = "drop"
  )

point_results_week

ggplot(
  point_results_week,
  aes(
    x = forecast_week,
    y = rmsse,
    colour = model
  )
) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  scale_x_continuous(
    breaks = 1:4
  ) +
  labs(
    title = "Performance ponctuelle selon l'échéance de prévision",
    subtitle = "RMSSE par semaine de l'horizon de 28 jours",
    x = "Semaine de prévision",
    y = "RMSSE",
    colour = "Modèle"
  ) +
  theme_minimal()

## enregistrement resultats et graphs

saveRDS(
  point_results_global,
  here("point_results_global.rds")
)

saveRDS(
  point_results_series,
  here("point_results_series.rds")
)

saveRDS(
  point_results_week,
  here("point_results_week.rds")
)


