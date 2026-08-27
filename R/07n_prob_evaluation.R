library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(fabletools)
library(distributional)
library(ggplot2)

source(here("R", "04_protocol.R"))

# Chargement des prévisions probabilistes

stat_cv <- readRDS(
  here("outputs",
       "tables",
       "statistical_cv_forecasts.rds")
)

lgb_quantile_cv <- readRDS(
  here("outputs",
       "tables",
       "lightgbm_quantile_cv_forecasts.rds")
)

stat_prob <- tibble::as_tibble(stat_cv)

quantiles <- c(
  0.025,
  0.10,
  0.25,
  0.50,
  0.75,
  0.90,
  0.975
)


# Extraction des quantiles des modeles fable

stat_quantile_cv <- purrr::map_dfr(
  quantiles,
  function(q) {
    
    stat_prob |>
      dplyr::transmute(
        fold_id,
        serie_id,
        date,
        horizon_step =
          as.integer(date - test_start) + 1L,
        model = .model,
        actual,
        quantile = q,
        
        prediction_raw = as.numeric(
          quantile(
            sales,
            p = q
          )
        )
      )
  }
)

#Non negativite

stat_quantile_cv |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    min_raw = min(prediction_raw),
    n_negative_raw = sum(prediction_raw < 0),
    .groups = "drop"
  )

stat_quantile_cv <- stat_quantile_cv |>
  dplyr::mutate(
    prediction = pmax(
      prediction_raw,
      0
    )
  )

stat_quantile_cv

# Uniformisation des formats


stat_quantile_common <- stat_quantile_cv |>
  dplyr::select(
    fold_id,
    serie_id,
    date,
    horizon_step,
    model,
    actual,
    quantile,
    prediction_raw,
    prediction
  )

lgb_quantile_common <- lgb_quantile_cv |>
  dplyr::transmute(
    fold_id,
    serie_id,
    date = target_date,
    horizon_step,
    model = "lightgbm",
    actual,
    quantile,
    prediction_raw,
    prediction
  )

# Controles 

prob_cv <- dplyr::bind_rows(
  stat_quantile_common,
  lgb_quantile_common
)

prob_cv |>
  dplyr::count(model)

nrow(prob_cv)

sum(is.na(prob_cv$prediction))
sum(!is.finite(prob_cv$prediction))
sum(prob_cv$prediction < 0)


# 1. Pinball Loss observation par observation

prob_cv <- prob_cv |>
  dplyr::mutate(
    quantile_error = actual - prediction,
    
    pinball_loss = dplyr::if_else(
      quantile_error >= 0,
      quantile * quantile_error,
      (1 - quantile) * (-quantile_error)
    )
  )

# Echelle de référence sur le train

scales_prob <- purrr::pmap_dfr(
  validation_folds,
  function(fold_id, train_end, test_start, test_end) {
    
    model_data |>
      dplyr::filter(date <= train_end) |>
      dplyr::arrange(serie_id, date) |>
      dplyr::group_by(serie_id) |>
      dplyr::summarise(
        pinball_scale = mean(
          abs(sales - dplyr::lag(sales, 7)),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        fold_id = fold_id
      )
  }
)

nrow(scales_prob)
sum(is.na(scales_prob$pinball_scale))
sum(scales_prob$pinball_scale <= 0)


# Rattache echelle aux previsions

prob_cv <- prob_cv |>
  dplyr::left_join(
    scales_prob,
    by = c(
      "fold_id",
      "serie_id"
    )
  ) |>
  dplyr::mutate(
    scaled_pinball =
      pinball_loss / pinball_scale
  )

# Score par fold modele et quantile

pinball_fold <- prob_cv |>
  dplyr::group_by(
    fold_id,
    serie_id,
    model,
    quantile
  ) |>
  dplyr::summarise(
    pinball = mean(pinball_loss),
    scaled_pinball = mean(scaled_pinball),
    .groups = "drop"
  )

## Global

pinball_global <- pinball_fold |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    pinball = mean(pinball),
    scaled_pinball = mean(scaled_pinball),
    .groups = "drop"
  ) |>
  dplyr::arrange(scaled_pinball)

pinball_global

## Quantile 

pinball_by_quantile <- pinball_fold |>
  dplyr::group_by(
    model,
    quantile
  ) |>
  dplyr::summarise(
    scaled_pinball = mean(scaled_pinball),
    .groups = "drop"
  )

pinball_by_quantile


# Construction des intervalles predictifs

prob_intervals <- prob_cv |>
  dplyr::mutate(
    quantile_name = dplyr::case_when(
      quantile == 0.025 ~ "q025",
      quantile == 0.10  ~ "q10",
      quantile == 0.25  ~ "q25",
      quantile == 0.50  ~ "q50",
      quantile == 0.75  ~ "q75",
      quantile == 0.90  ~ "q90",
      quantile == 0.975 ~ "q975"
    )
  ) |>
  dplyr::select(
    fold_id,
    serie_id,
    date,
    horizon_step,
    model,
    actual,
    quantile_name,
    prediction
  ) |>
  tidyr::pivot_wider(
    names_from = quantile_name,
    values_from = prediction
  )

nrow(prob_intervals)

## Calcul couverture et largeur

prob_intervals <- prob_intervals |>
  dplyr::mutate(
    
    ### Intervalle 50 %
    covered_50 =
      actual >= q25 &
      actual <= q75,
    
    width_50 =
      q75 - q25,
    
    
    ### Intervalle 80 %
    covered_80 =
      actual >= q10 &
      actual <= q90,
    
    width_80 =
      q90 - q10,
    
    
    ### Intervalle 95 %
    covered_95 =
      actual >= q025 &
      actual <= q975,
    
    width_95 =
      q975 - q025
  )

## Largeurs mise a l echelle

prob_intervals <- prob_intervals |>
  dplyr::left_join(
    scales_prob,
    by = c(
      "fold_id",
      "serie_id"
    )
  ) |>
  dplyr::mutate(
    scaled_width_50 =
      width_50 / pinball_scale,
    
    scaled_width_80 =
      width_80 / pinball_scale,
    
    scaled_width_95 =
      width_95 / pinball_scale
  )

## Agreger par fold serie modele

interval_metrics_fold <- prob_intervals |>
  dplyr::group_by(
    fold_id,
    serie_id,
    model
  ) |>
  dplyr::summarise(
    
    coverage_50 = mean(covered_50),
    width_50 = mean(width_50),
    scaled_width_50 = mean(scaled_width_50),
    
    coverage_80 = mean(covered_80),
    width_80 = mean(width_80),
    scaled_width_80 = mean(scaled_width_80),
    
    coverage_95 = mean(covered_95),
    width_95 = mean(width_95),
    scaled_width_95 = mean(scaled_width_95),
    
    .groups = "drop"
  )

## Recap global

interval_results_global <- interval_metrics_fold |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    
    coverage_50 = mean(coverage_50),
    width_50 = mean(width_50),
    
    coverage_80 = mean(coverage_80),
    width_80 = mean(width_80),
    
    coverage_95 = mean(coverage_95),
    width_95 = mean(width_95),
    
    .groups = "drop"
  )

interval_results_global

## Recap scaled

interval_results_scaled <- interval_metrics_fold |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    coverage_50 = mean(coverage_50),
    scaled_width_50 = mean(scaled_width_50),
    
    coverage_80 = mean(coverage_80),
    scaled_width_80 = mean(scaled_width_80),
    
    coverage_95 = mean(coverage_95),
    scaled_width_95 = mean(scaled_width_95),
    
    .groups = "drop"
  )

interval_results_scaled

# Calcul erreur de calibration

interval_results_final <- interval_results_scaled |>
  dplyr::mutate(
    
    calibration_error_50 =
      abs(coverage_50 - 0.50),
    
    calibration_error_80 =
      abs(coverage_80 - 0.80),
    
    calibration_error_95 =
      abs(coverage_95 - 0.95),
    
    mean_calibration_error =
      (
        calibration_error_50 +
          calibration_error_80 +
          calibration_error_95
      ) / 3
  ) |>
  dplyr::arrange(
    mean_calibration_error
  )

interval_results_final

# Pour toutes les series

interval_results_series <- interval_metrics_fold |>
  dplyr::group_by(
    serie_id,
    model
  ) |>
  dplyr::summarise(
    
    coverage_50 = mean(coverage_50),
    scaled_width_50 = mean(scaled_width_50),
    
    coverage_80 = mean(coverage_80),
    scaled_width_80 = mean(scaled_width_80),
    
    coverage_95 = mean(coverage_95),
    scaled_width_95 = mean(scaled_width_95),
    
    .groups = "drop"
  )

interval_results_series

# Erreur de calibration par serie

interval_results_series <- interval_results_series |>
  dplyr::mutate(
    calibration_error_50 =
      abs(coverage_50 - 0.50),
    
    calibration_error_80 =
      abs(coverage_80 - 0.80),
    
    calibration_error_95 =
      abs(coverage_95 - 0.95),
    
    mean_calibration_error =
      (
        calibration_error_50 +
          calibration_error_80 +
          calibration_error_95
      ) / 3
  ) |>
  dplyr::arrange(
    serie_id,
    mean_calibration_error
  )

interval_results_series |>
  dplyr::select(
    serie_id,
    model,
    mean_calibration_error
  )

# Graphiques recap

## Calibration des intervalles predictifs

calibration_plot_data <- interval_results_global |>
  dplyr::select(
    model,
    coverage_50,
    coverage_80,
    coverage_95
  ) |>
  tidyr::pivot_longer(
    cols = dplyr::starts_with("coverage_"),
    names_to = "level",
    values_to = "empirical_coverage"
  ) |>
  dplyr::mutate(
    nominal_coverage = dplyr::case_when(
      level == "coverage_50" ~ 0.50,
      level == "coverage_80" ~ 0.80,
      level == "coverage_95" ~ 0.95
    )
  )

ggplot2::ggplot(
  calibration_plot_data,
  ggplot2::aes(
    x = nominal_coverage,
    y = empirical_coverage,
    colour = model
  )
) +
  ggplot2::geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed"
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::scale_x_continuous(
    breaks = c(0.50, 0.80, 0.95),
    labels = scales::percent
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent
  ) +
  ggplot2::labs(
    title = "Calibration des intervalles prédictifs",
    subtitle = "Couverture empirique comparée à la couverture nominale",
    x = "Couverture nominale",
    y = "Couverture empirique",
    colour = "Modèle"
  ) +
  ggplot2::theme_minimal()

## Calibration vs finesse intervalle 80

p_calibration_sharpness_80 <- ggplot2::ggplot(
  interval_results_scaled,
  ggplot2::aes(
    x = scaled_width_80,
    y = coverage_80,
    label = model
  )
) +
  ggplot2::geom_hline(
    yintercept = 0.80,
    linetype = "dashed"
  ) +
  ggplot2::geom_point(
    size = 3
  ) +
  ggplot2::geom_text(
    nudge_y = 0.008,
    check_overlap = TRUE
  ) +
  ggplot2::labs(
    title = "Calibration et finesse des intervalles à 80 %",
    subtitle = "Couverture empirique et largeur normalisée",
    x = "Largeur normalisée de l'intervalle",
    y = "Couverture empirique"
  ) +
  ggplot2::scale_y_continuous(
    labels = scales::percent
  ) +
  ggplot2::theme_minimal()

p_calibration_sharpness_80

