library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(lightgbm)
library(tsibble)
library(fable)
library(fabletools)
library(distributional)

# 1. Charger le protocole et les donnees

source(
  here::here(
    "R",
    "04_protocol.R"
  )
)

# Definition Holdout final 

holdout_train_end <- as.Date("2016-04-24")

holdout_test_start <- as.Date("2016-04-25")

holdout_test_end <- as.Date("2016-05-22")

holdout_horizon <- 28

# 3. Train / test holdout

holdout_train <- model_data |>
  dplyr::filter(
    date <= holdout_train_end
  )

holdout_train <- holdout_train |>
  tsibble::as_tsibble(
    key = serie_id,
    index = date
  )


holdout_test <- model_data |>
  dplyr::filter(
    date >= holdout_test_start,
    date <= holdout_test_end
  )

## controle

holdout_train |>
  dplyr::group_by(serie_id) |>
  dplyr::summarise(
    last_train_date = max(date),
    n_train = dplyr::n(),
    .groups = "drop"
  )

holdout_test |>
  dplyr::group_by(serie_id) |>
  dplyr::summarise(
    first_test_date = min(date),
    last_test_date = max(date),
    n_test = dplyr::n(),
    .groups = "drop"
  )

# 4. Modeles statistiques

holdout_stat_models <- holdout_train |>
  fabletools::model(
    
    snaive =
      fable::SNAIVE(
        sales ~ lag("week")
      ),
    
    ets =
      fable::ETS(
        sales ~ season(period = 7)
      ),
    
    sarima =
      fable::ARIMA(
        sales ~
          pdq() +
          PDQ(period = 7),
        
        stepwise = TRUE,
        approximation = FALSE
      )
  )

## horizon

holdout_stat_fc <- holdout_stat_models |>
  fabletools::forecast(
    h = holdout_horizon
  )
    
   
 holdout_stat <- holdout_stat_fc |>
      dplyr::left_join(
        holdout_test |>
          dplyr::select(
            serie_id,
            date,
            actual = sales
          ),
        by = c(
          "serie_id",
          "date"
        )
      )

holdout_stat <- tibble::as_tibble(
  holdout_stat
)

## controle

nrow(holdout_stat)

holdout_stat |>
  dplyr::count(
    .model
  )


range(holdout_stat$date)

#5. HOLDOUT LIGHTGBM

needed_lgb_objects <- c(
  "lgb_examples",
  "make_lgb_matrix",
  "params_point",
  "quantiles",
  "horizon_lgb",
  "run_lgb_fold"
)

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

# Encodage des features identique au script 06

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

params_point <- list(
  objective = "regression",
  metric = "rmse",
  learning_rate = 0.05,
  num_leaves = 31L,
  min_data_in_leaf = 20L,
  verbosity = -1L,
  seed = 123L
)

# Copie resultats LGBM

run_lgb_fold <- function(
    fold_id,
    train_end,
    test_start,
    test_end
) {
  
  message("Début ", fold_id)
  
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
  
  test_fold <- lgb_examples |>
    dplyr::filter(
      origin_date == train_end,
      target_date >= test_start,
      target_date <= test_end
    )
  
  stopifnot(
    nrow(test_fold) == 4 * horizon_lgb
  )
  
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
  
  # Point forecast
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
  
  
  # Quantile forecasts
  
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
  
  # Post-traitement identique au CV
  
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
      
      prediction_nonneg = pmax(
        prediction_raw,
        0
      ),
      
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

holdout_lgb <- run_lgb_fold(
  fold_id = "HOLDOUT",
  train_end = holdout_train_end,
  test_start = holdout_test_start,
  test_end = holdout_test_end
)

holdout_lgb_point <- holdout_lgb$point

holdout_lgb_quantile <- holdout_lgb$quantile

nrow(holdout_lgb_point)
nrow



# 6. EVALUATION PONCTUELLE DU HOLDOUT

## Previsions statistiques

holdout_point_stat <- holdout_stat |>
  dplyr::transmute(
    serie_id,
    date,
    
    horizon_step =
      as.integer(date - holdout_test_start) + 1L,
    
    model = .model,
    
    actual,
    
    prediction_raw = .mean,
    
    prediction = pmax(.mean, 0)
  )

# Previsions LightGBM

holdout_point_lgb <- holdout_lgb_point |>
  dplyr::transmute(
    serie_id,
    
    date = target_date,
    
    horizon_step,
    
    model = "lightgbm",
    
    actual,
    
    prediction_raw,
    
    prediction = pmax(prediction_raw, 0)
  )

holdout_point <- dplyr::bind_rows(
  holdout_point_stat,
  holdout_point_lgb
)

# Echelles MASE / RMSSE calculees uniquement sur le train

holdout_scales <- tibble::as_tibble(
  holdout_train
) |>
  dplyr::arrange(
    serie_id,
    date
  ) |>
  dplyr::group_by(
    serie_id
  ) |>
  dplyr::summarise(
    
    mase_scale =
      mean(
        abs(
          sales - dplyr::lag(sales, 7)
        ),
        na.rm = TRUE
      ),
    
    rmsse_scale =
      mean(
        (
          sales - dplyr::lag(sales, 7)
        )^2,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )

#Calcul errerirs

holdout_point <- holdout_point |>
  dplyr::left_join(
    holdout_scales,
    by = "serie_id"
  ) |>
  dplyr::mutate(
    
    # Même convention que dans 07a
    error = actual - prediction,
    
    abs_error = abs(error),
    
    squared_error = error^2
  )

# Resultats par serie

holdout_point_series <- holdout_point |>
  dplyr::group_by(
    serie_id,
    model
  ) |>
  dplyr::summarise(
    
    bias =
      mean(error),
    
    mae =
      mean(abs_error),
    
    rmse =
      sqrt(mean(squared_error)),
    
    mase =
      mean(abs_error) /
      dplyr::first(mase_scale),
    
    rmsse =
      sqrt(
        mean(squared_error) /
          dplyr::first(rmsse_scale)
      ),
    
    .groups = "drop"
  )

# RESULTATS GLOBAUX

holdout_point_global <- holdout_point_series |>
  dplyr::group_by(
    model
  ) |>
  dplyr::summarise(
    
    bias =
      mean(bias),
    
    mae =
      mean(mae),
    
    rmse =
      mean(rmse),
    
    mase =
      mean(mase),
    
    rmsse =
      mean(rmsse),
    
    .groups = "drop"
  ) |>
  dplyr::arrange(
    rmsse
  )

holdout_point_global

# 7. EVALUATION PROBABILISTE DU HOLDOUT

holdout_quantiles <- c(
  0.025,
  0.10,
  0.25,
  0.50,
  0.75,
  0.90,
  0.975
)

# Quantiles ETS / SARIMA / SNaive

holdout_stat_quantiles <- purrr::map_dfr(
  holdout_quantiles,
  
  function(q) {
    
    holdout_stat |>
      dplyr::transmute(
        serie_id,
        date,
        
        horizon_step =
          as.integer(date - holdout_test_start) + 1L,
        
        model = .model,
        
        actual,
        
        quantile = q,
        
        prediction_raw =
          as.numeric(
            quantile(
              sales,
              p = q
            )
          ),
        
        prediction =
          pmax(
            prediction_raw,
            0
          )
      )
  }
)


holdout_lgb_quantiles <- holdout_lgb_quantile |>
  dplyr::transmute(
    serie_id,
    
    date = target_date,
    
    horizon_step,
    
    model = "lightgbm",
    
    actual,
    
    quantile,
    
    prediction_raw,
    
    # Déjà clipping + réarrangement monotone
    prediction
  )

# Assemblage

holdout_prob <- dplyr::bind_rows(
  holdout_stat_quantiles,
  holdout_lgb_quantiles)
  

#Pinball loss holdout

holdout_prob <- holdout_prob |>
  dplyr::mutate(
    
    quantile_error =
      actual - prediction,
    
    pinball_loss =
      dplyr::if_else(
        quantile_error >= 0,
        
        quantile *
          quantile_error,
        
        (1 - quantile) *
          (-quantile_error)
      )
  )

#Echelle

holdout_prob <- holdout_prob |>
  dplyr::left_join(
    holdout_scales |>
      dplyr::select(
        serie_id,
        mase_scale
      ),
    by = "serie_id"
  ) |>
  dplyr::mutate(
    scaled_pinball =
      pinball_loss /
      mase_scale
  )
  
#Agregatiopnm par serie

holdout_pinball_series <- holdout_prob |>
  dplyr::group_by(
    serie_id,
    model,
    quantile
  ) |>
  dplyr::summarise(
    pinball =
      mean(pinball_loss),
    
    scaled_pinball =
      mean(scaled_pinball),
    
    .groups = "drop"
  )

# Resultat global pinball

holdout_pinball_global <- holdout_pinball_series |>
  dplyr::group_by(
    model
  ) |>
  dplyr::summarise(
    pinball =
      mean(pinball),
    
    scaled_pinball =
      mean(scaled_pinball),
    
    .groups = "drop"
  ) |>
  dplyr::arrange(
    scaled_pinball
  )

holdout_pinball_global

# Intervalles predictifs holdout

holdout_intervals <- holdout_prob |>
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

holdout_intervals <- holdout_intervals |>
  dplyr::mutate(
    
    covered_50 =
      actual >= q25 &
      actual <= q75,
    
    width_50 =
      q75 - q25,
    
    
    covered_80 =
      actual >= q10 &
      actual <= q90,
    
    width_80 =
      q90 - q10,
    
    
    covered_95 =
      actual >= q025 &
      actual <= q975,
    
    width_95 =
      q975 - q025
  )

holdout_intervals <- holdout_intervals |>
  dplyr::left_join(
    holdout_scales |>
      dplyr::select(
        serie_id,
        mase_scale
      ),
    by = "serie_id"
  ) |>
  dplyr::mutate(
    scaled_width_50 =
      width_50 / mase_scale,
    
    scaled_width_80 =
      width_80 / mase_scale,
    
    scaled_width_95 =
      width_95 / mase_scale
  )

holdout_interval_series <- holdout_intervals |>
  dplyr::group_by(
    serie_id,
    model
  ) |>
  dplyr::summarise(
    
    coverage_50 = mean(covered_50),
    scaled_width_50 = mean(scaled_width_50),
    
    coverage_80 = mean(covered_80),
    scaled_width_80 = mean(scaled_width_80),
    
    coverage_95 = mean(covered_95),
    scaled_width_95 = mean(scaled_width_95),
    
    .groups = "drop"
  )

# Global handout 

holdout_interval_global <- holdout_interval_series |>
  dplyr::group_by(model) |>
  dplyr::summarise(
    
    coverage_50 = mean(coverage_50),
    scaled_width_50 = mean(scaled_width_50),
    
    coverage_80 = mean(coverage_80),
    scaled_width_80 = mean(scaled_width_80),
    
    coverage_95 = mean(coverage_95),
    scaled_width_95 = mean(scaled_width_95),
    
    .groups = "drop"
  ) |>
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

holdout_interval_global

# 8. EVALUATION DECISIONNELLE DU HOLDOUT

## Prévisions ponctuelles

holdout_stock_point <- holdout_point |>
  dplyr::transmute(
    serie_id,
    date,
    model,
    actual,
    policy = "point",
    quantity = prediction
  )

holdout_stock_quantiles <- holdout_prob |>
  dplyr::filter(
    quantile %in% c(
      0.50,
      0.75,
      0.90
    )
  ) |>
  dplyr::transmute(
    serie_id,
    date,
    model,
    actual,
    
    policy = dplyr::case_when(
      quantile == 0.50 ~ "q50",
      quantile == 0.75 ~ "q75",
      quantile == 0.90 ~ "q90"
    ),
    
    quantity = prediction
  )

holdout_stock <- dplyr::bind_rows(
  holdout_stock_point,
  holdout_stock_quantiles
)

holdout_stock <- holdout_stock |>
  dplyr::mutate(
    understock =
      pmax(actual - quantity, 0),
    
    overstock =
      pmax(quantity - actual, 0),
    
    shortage_day =
      understock > 0
  )

holdout_cost_scenarios <- tibble::tibble(
  cost_scenario = c(
    "balanced",
    "shortage_x3",
    "shortage_x9"
  ),
  
  understock_cost = c(
    1,
    3,
    9
  ),
  
  overstock_cost = c(
    1,
    1,
    1
  )
)

holdout_stock_costs <- holdout_stock |>
  tidyr::crossing(
    holdout_cost_scenarios
  ) |>
  dplyr::mutate(
    decision_cost =
      understock * understock_cost +
      overstock * overstock_cost
  )

# Resultats decisionnels par serie

holdout_stock_cost_series <- holdout_stock_costs |>
  dplyr::group_by(
    serie_id,
    model,
    policy,
    cost_scenario
  ) |>
  dplyr::summarise(
    
    total_demand =
      sum(actual),
    
    total_cost =
      sum(decision_cost),
    
    avg_understock =
      mean(understock),
    
    avg_overstock =
      mean(overstock),
    
    understock_rate =
      sum(understock) /
      sum(actual),
    
    overstock_rate =
      sum(overstock) /
      sum(actual),
    
    cost_per_demand_unit =
      sum(decision_cost) /
      sum(actual),
    
    shortage_day_rate =
      mean(shortage_day),
    
    demand_coverage_rate =
      1 -
      sum(understock) /
      sum(actual),
    
    .groups = "drop"
  )

# Resultats globaux holdout

holdout_stock_cost_global <- holdout_stock_cost_series |>
  dplyr::group_by(
    model,
    policy,
    cost_scenario
  ) |>
  dplyr::summarise(
    
    cost_per_demand_unit =
      mean(cost_per_demand_unit),
    
    understock_rate =
      mean(understock_rate),
    
    overstock_rate =
      mean(overstock_rate),
    
    shortage_day_rate =
      mean(shortage_day_rate),
    
    demand_coverage_rate =
      mean(demand_coverage_rate),
    
    .groups = "drop"
  )


scenario_policy <- tibble::tibble(
  
  cost_scenario = c(
    "balanced",
    "shortage_x3",
    "shortage_x9"
  ),
  
  theoretical_policy = c(
    "q50",
    "q75",
    "q90"
  ),
  
  target_quantile = c(
    0.50,
    0.75,
    0.90
  )
)

# Tableau final holdout

holdout_matched_policy <- holdout_stock_cost_global |>
  dplyr::left_join(
    scenario_policy,
    by = "cost_scenario"
  ) |>
  dplyr::filter(
    policy == theoretical_policy
  ) |>
  dplyr::group_by(
    cost_scenario
  ) |>
  dplyr::arrange(
    cost_per_demand_unit,
    .by_group = TRUE
  ) |>
  dplyr::mutate(
    rank = dplyr::row_number()
  ) |>
  dplyr::ungroup() |>
  dplyr::select(
    cost_scenario,
    target_quantile,
    model,
    policy,
    cost_per_demand_unit,
    understock_rate,
    overstock_rate,
    shortage_day_rate,
    demand_coverage_rate,
    rank
  )

holdout_matched_policy |>
  print(
    n = 12,
    width = Inf
  )