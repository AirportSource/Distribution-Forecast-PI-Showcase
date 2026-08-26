library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(lightgbm)

source(here("R", "04_protocol.R"))

# Paramètres LightGBM

quantiles <- c(
  0.025,
  0.10,
  0.25,
  0.50,
  0.75,
  0.90,
  0.975
)

horizon_lgb <- 28L

# Construction de la base LightGBM

## Specification et tabulation

lgb_base <- model_data |>
  dplyr::arrange(
    serie_id,
    date
  ) |>
  dplyr::group_by(serie_id) |>
  dplyr::mutate(
    lag_0  = sales,
    lag_1  = dplyr::lag(sales, 1),
    lag_7  = dplyr::lag(sales, 7),
    lag_14 = dplyr::lag(sales, 14),
    lag_28 = dplyr::lag(sales, 28),
    lag_56 = dplyr::lag(sales, 56)
  ) |>
  dplyr::ungroup() |>
  dplyr::transmute(
    serie_id,
    origin_date = date,
    lag_0,
    lag_1,
    lag_7,
    lag_14,
    lag_28,
    lag_56
  )

## Decouplage des 28 series

lgb_examples <- tidyr::crossing(
  lgb_base,
  horizon_step = 1:horizon_lgb
) |>
  dplyr::mutate(
    target_date = origin_date + horizon_step,
    
    target_wday = as.integer(
      format(target_date, "%u")
    ),
    
    target_month = as.integer(
      format(target_date, "%m")
    )
  ) |>
  dplyr::left_join(
    model_data |>
      dplyr::select(
        serie_id,
        target_date = date,
        target = sales
      ),
    by = c(
      "serie_id",
      "target_date"
    )
  )

# Controle construction multi horizon

lgb_examples |>
  dplyr::filter(
    serie_id == "FOODS_3_156_CA_3",
    origin_date == as.Date("2015-01-04")
  ) |>
  dplyr::select(
    serie_id,
    origin_date,
    horizon_step,
    target_date,
    lag_0,
    lag_1,
    lag_7,
    lag_14,
    lag_28,
    lag_56,
    target_wday,
    target_month,
    target
  ) |>
  print(n = 28)

# Test de construction du premier fold LightGBM

fold1 <- validation_folds |>
  dplyr::slice(1)


# Données d'apprentissage
lgb_train_01 <- lgb_examples |>
  dplyr::filter(
    target_date <= fold1$train_end,
    !is.na(target),
    !is.na(lag_0),
    !is.na(lag_1),
    !is.na(lag_7),
    !is.na(lag_14),
    !is.na(lag_28),
    !is.na(lag_56)
  )


# Données à prévoir
lgb_test_01 <- lgb_examples |>
  dplyr::filter(
    origin_date == fold1$train_end,
    target_date >= fold1$test_start,
    target_date <= fold1$test_end
  )

max(lgb_train_01$target_date)
fold1$train_end

# Features LightGBM

series_levels <- sort(
  unique(model_data$serie_id)
)

make_lgb_matrix <- function(data) {
  
  data_features <- data |>
    dplyr::mutate(
      serie_id = factor(
        serie_id,
        levels = series_levels
      ),
      target_wday = factor(
        target_wday,
        levels = 1:7
      ),
      target_month = factor(
        target_month,
        levels = 1:12
      )
    )
  
  x <- stats::model.matrix(
    ~ 0 +
      serie_id +
      lag_0 +
      lag_1 +
      lag_7 +
      lag_14 +
      lag_28 +
      lag_56 +
      horizon_step +
      target_wday +
      target_month,
    data = data_features
  )
  
  storage.mode(x) <- "double"
  
  x
}

# Matrices train / test CV_01


x_train_01 <- make_lgb_matrix(
  lgb_train_01
)

x_test_01 <- make_lgb_matrix(
  lgb_test_01
)

dim(x_train_01)
dim(x_test_01)

identical(
  colnames(x_train_01),
  colnames(x_test_01)
)

# LightGBM ponctuel - CV_01

set.seed(123)

dtrain_01 <- lightgbm::lgb.Dataset(
  data = x_train_01,
  label = lgb_train_01$target
)

params_point <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 31L,
  min_data_in_leaf = 20L,
  verbosity = -1L,
  seed = 123L
)

model_point_01 <- lightgbm::lgb.train(
  params = params_point,
  data = dtrain_01,
  nrounds = 300L
)

pred_point_01 <- predict(
  model_point_01,
  x_test_01
)

# LightGBM quantile - CV_01

fit_predict_quantile <- function(q) {
  
  set.seed(123)
  
  params_quantile <- list(
    objective = "quantile",
    metric = "quantile",
    alpha = q,
    learning_rate = 0.05,
    num_leaves = 31L,
    min_data_in_leaf = 20L,
    verbosity = -1L,
    seed = 123L
  )
  
  model_q <- lightgbm::lgb.train(
    params = params_quantile,
    data = dtrain_01,
    nrounds = 300L
  )
  
  pred_q <- predict(
    model_q,
    x_test_01
  )
  
  tibble::tibble(
    quantile = q,
    row_id = seq_along(pred_q),
    prediction = pred_q
  )
}

# Lancement des modeles quantiles

quantile_predictions_01 <- purrr::map_dfr(
  quantiles,
  fit_predict_quantile
)
quantile_predictions_01 |>
  dplyr::count(quantile)

# bind des previsions aux observartions

quantile_predictions_01 <- quantile_predictions_01 |>
  dplyr::left_join(
    lgb_test_01 |>
      dplyr::mutate(
        row_id = dplyr::row_number()
      ) |>
      dplyr::select(
        row_id,
        serie_id,
        target_date,
        horizon_step,
        actual = target
      ),
    by = "row_id"
  )
quantile_predictions_01

# Contrôle du quantile crossing - CV_01

crossing_check_01 <- quantile_predictions_01 |>
  dplyr::group_by(
    row_id,
    serie_id,
    target_date,
    horizon_step
  ) |>
  dplyr::arrange(
    quantile,
    .by_group = TRUE
  ) |>
  dplyr::summarise(
    crossing = any(diff(prediction) < 0),
    min_gap = min(diff(prediction)),
    .groups = "drop"
  )

sum(crossing_check_01$crossing)

# Post-traitement des quantiles

quantile_predictions_01_clean <- quantile_predictions_01 |>
  dplyr::group_by(
    row_id,
    serie_id,
    target_date,
    horizon_step
  ) |>
  dplyr::arrange(
    quantile,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    # Les ventes ne peuvent pas être négatives
    prediction = pmax(prediction, 0),
    
    # Réarrangement pour garantir l'ordre des quantiles
    prediction = sort(prediction)
  ) |>
  dplyr::ungroup()

## Controle correctif du cross quantile

crossing_check_01_clean <- quantile_predictions_01_clean |>
  dplyr::group_by(
    row_id,
    serie_id,
    target_date,
    horizon_step
  ) |>
  dplyr::arrange(
    quantile,
    .by_group = TRUE
  ) |>
  dplyr::summarise(
    crossing = any(diff(prediction) < 0),
    .groups = "drop"
  )

sum(crossing_check_01_clean$crossing)


# Fonction LightGBM complete pour un fold

run_lgb_fold <- function(
    fold_id,
    train_end,
    test_start,
    test_end
) {
  
  message("Début ", fold_id)
  

  # Train
  
  train_fold <- lgb_examples |>
    dplyr::filter(
      target_date <= train_end,
      !is.na(target),
      !is.na(lag_0),
      !is.na(lag_1),
      !is.na(lag_7),
      !is.na(lag_14),
      !is.na(lag_28),
      !is.na(lag_56)
    )
  
  
  # Test : 28 jours suivants
  
  test_fold <- lgb_examples |>
    dplyr::filter(
      origin_date == train_end,
      target_date >= test_start,
      target_date <= test_end
    )
  
  stopifnot(
    nrow(test_fold) == 4 * horizon_lgb
  )
  

  # Matrices LightGBM
  
  x_train <- make_lgb_matrix(train_fold)
  x_test  <- make_lgb_matrix(test_fold)
  
  stopifnot(
    identical(
      colnames(x_train),
      colnames(x_test)
    )
  )
  
  dtrain <- lightgbm::lgb.Dataset(
    data = x_train,
    label = train_fold$target
  )
  
  # 1. Prévision ponctuelle
  
  set.seed(123)
  
  model_point <- lightgbm::lgb.train(
    params = params_point,
    data = dtrain,
    nrounds = 300L
  )
  
  pred_point <- predict(
    model_point,
    x_test
  )
  
  point_results <- test_fold |>
    dplyr::transmute(
      fold_id = fold_id,
      serie_id,
      target_date,
      horizon_step,
      actual = target,
      prediction_raw = as.numeric(pred_point)
    )
  

  # 2. Previsions quantiles
  
  test_key <- test_fold |>
    dplyr::mutate(
      row_id = dplyr::row_number()
    ) |>
    dplyr::select(
      row_id,
      serie_id,
      target_date,
      horizon_step,
      actual = target
    )
  
  quantile_results <- purrr::map_dfr(
    quantiles,
    function(q) {
      
      set.seed(123)
      
      params_q <- list(
        objective = "quantile",
        metric = "quantile",
        alpha = q,
        learning_rate = 0.05,
        num_leaves = 31L,
        min_data_in_leaf = 20L,
        verbosity = -1L,
        seed = 123L
      )
      
      model_q <- lightgbm::lgb.train(
        params = params_q,
        data = dtrain,
        nrounds = 300L
      )
      
      pred_q <- predict(
        model_q,
        x_test
      )
      
      tibble::tibble(
        row_id = seq_along(pred_q),
        quantile = q,
        prediction_raw = as.numeric(pred_q)
      )
    }
  )
  
  # Association aux dates + post-traitement

  quantile_results <- quantile_results |>
    dplyr::left_join(
      test_key,
      by = "row_id"
    ) |>
    dplyr::group_by(
      row_id,
      serie_id,
      target_date,
      horizon_step
    ) |>
    dplyr::arrange(
      quantile,
      .by_group = TRUE
    ) |>
    dplyr::mutate(
      
      # Support des ventes : y >= 0
      prediction_nonneg = pmax(
        prediction_raw,
        0
      ),
      
      # Réarrangement monotone
      prediction = sort(
        prediction_nonneg
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      fold_id = fold_id
    )
  
  message("Fin ", fold_id)
  
  list(
    point = point_results,
    quantile = quantile_results
  )
}

lgb_cv_results <- purrr::pmap(
  validation_folds,
  run_lgb_fold
)


lgb_point_cv <- purrr::map_dfr(
  lgb_cv_results,
  "point"
)

lgb_quantile_cv <- purrr::map_dfr(
  lgb_cv_results,
  "quantile"
)

# Sauvegarde des prévisions LightGBM

saveRDS(
  lgb_point_cv,
  here("lightgbm_point_cv_forecasts.rds")
)

saveRDS(
  lgb_quantile_cv,
  here("lightgbm_quantile_cv_forecasts.rds")
)
