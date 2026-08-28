library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)
library(distributional)

# 1. Chargement des prévisions out-of-sample

stat_cv <- readRDS(
  here::here("outputs",
             "tables","statistical_cv_forecasts.rds")
)

lgb_point_cv <- readRDS(
  here::here("outputs",
             "tables","lightgbm_point_cv_forecasts.rds")
)

lgb_quantile_cv <- readRDS(
  here::here("outputs",
             "tables","lightgbm_quantile_cv_forecasts.rds")
)

# Conversion objet fable en tableau tibble

stat_tbl <- tibble::as_tibble(stat_cv)

# 2. Politique basee sur prévision ponctuelle

point_stat <- stat_tbl |>
  dplyr::transmute(
    fold_id,
    serie_id,
    date,
    
    model = .model,
    
    actual,
    
    quantity = pmax(.mean, 0),
    
    policy = "point"
  )

point_lgb <- lgb_point_cv |>
  dplyr::transmute(
    fold_id,
    serie_id,
    
    date = target_date,
    
    model = "lightgbm",
    
    actual,
    
    quantity = pmax(prediction_raw, 0),
    
    policy = "point"
  )

point_decisions <- dplyr::bind_rows(
  point_stat,
  point_lgb
)

# 3. Politiques probabilistes

stock_quantiles <- c(
  0.50,
  0.75,
  0.90
)

## Modeles stats classiques

quantile_stat <- purrr::map_dfr(
  stock_quantiles,
  
  function(q) {
    
    stat_tbl |>
      dplyr::transmute(
        fold_id,
        serie_id,
        date,
        
        model = .model,
        
        actual,
        
        quantile = q,
        
        quantity = pmax(
          as.numeric(
            quantile(sales, p = q)
          ),
          0
        )
      )
  }
)

### Nommer les politiques

quantile_stat <- quantile_stat |>
  dplyr::mutate(
    policy = dplyr::case_when(
      quantile == 0.50 ~ "q50",
      quantile == 0.75 ~ "q75",
      quantile == 0.90 ~ "q90"
    )
  )

## LightGBM

quantile_lgb <- lgb_quantile_cv |>
  dplyr::filter(
    quantile %in% stock_quantiles
  ) |>
  dplyr::transmute(
    fold_id,
    serie_id,
    
    date = target_date,
    
    model = "lightgbm",
    
    actual,
    quantile,
    quantity = prediction,
    
    policy = dplyr::case_when(
      quantile == 0.50 ~ "q50",
      quantile == 0.75 ~ "q75",
      quantile == 0.90 ~ "q90"
    )
  )

## Assembler les decisions entre elles

stock_decisions <- dplyr::bind_rows(
  
  point_decisions,
  
  quantile_stat |>
    dplyr::select(
      fold_id,
      serie_id,
      date,
      model,
      actual,
      quantity,
      policy
    ),
  
  quantile_lgb |>
    dplyr::select(
      fold_id,
      serie_id,
      date,
      model,
      actual,
      quantity,
      policy
    )
  
) |>
  dplyr::arrange(
    serie_id,
    model,
    policy,
    date
  )


## Controles

nrow(stock_decisions)

stock_decisions |>
  dplyr::count(
    model,
    policy
  )

stock_decisions |>
  dplyr::count(
    serie_id,
    model,
    policy
  )

stock_decisions |>
  dplyr::summarise(
    first_date = min(date),
    last_date = max(date),
    n_dates = dplyr::n_distinct(date)
  )


# 5. Consequences de chaque decision

stock_outcomes <- stock_decisions |>
  dplyr::mutate(
    
    # Demande non couverte
    understock = pmax(
      actual - quantity,
      0
    ),
    
    # Quantite excedentaire
    overstock = pmax(
      quantity - actual,
      0
    ),
    
    # TRUE si la quantite choisie ne couvre pas toute la demande
    shortage_day = understock > 0
  )

summary(stock_outcomes$understock)
summary(stock_outcomes$overstock)

##

stock_results_series <- stock_outcomes |>
dplyr::group_by(
  serie_id,
  model,
  policy
) |>
  dplyr::summarise(
    
    n_days = dplyr::n(),
    
    total_demand = sum(actual),
    
    avg_quantity = mean(quantity),
    
    avg_understock = mean(understock),
    
    avg_overstock = mean(overstock),
    
    shortage_day_rate = mean(shortage_day),
    
    demand_coverage_rate =
      1 - sum(understock) / sum(actual),
    
    .groups = "drop"
  )

## Resultats globaux

stock_results_global <- stock_results_series |>
  dplyr::group_by(
    model,
    policy
  ) |>
  dplyr::summarise(
    
    avg_quantity =
      mean(avg_quantity),
    
    avg_understock =
      mean(avg_understock),
    
    avg_overstock =
      mean(avg_overstock),
    
    shortage_day_rate =
      mean(shortage_day_rate),
    
    demand_coverage_rate =
      mean(demand_coverage_rate),
    
    .groups = "drop"
  ) |>
  dplyr::arrange(
    model,
    policy
  )

stock_results_global

# 7. Scenarios de cout

cost_scenarios <- tibble::tibble(
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

stock_costs <- stock_outcomes |>
  tidyr::crossing(
    cost_scenarios
  ) |>
  dplyr::mutate(
    
    understock_cost_total =
      understock * understock_cost,
    
    overstock_cost_total =
      overstock * overstock_cost,
    
    decision_cost =
      understock_cost_total +
      overstock_cost_total
  )

## Agregaton par serie

stock_cost_results_series <- stock_costs |>
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
    
    avg_cost =
      mean(decision_cost),
    
    avg_understock =
      mean(understock),
    
    avg_overstock =
      mean(overstock),
    
    understock_rate =
      sum(understock) / sum(actual),
    
    overstock_rate =
      sum(overstock) / sum(actual),
    
    cost_per_demand_unit =
      sum(decision_cost) / sum(actual),
    
    shortage_day_rate =
      mean(shortage_day),
    
    demand_coverage_rate =
      1 - sum(understock) / sum(actual),
    
    .groups = "drop"
  )

## Moyenne equitable 

stock_cost_results_global <- stock_cost_results_series |>
  dplyr::group_by(
    model,
    policy,
    cost_scenario
  ) |>
  dplyr::summarise(
    
    avg_cost =
      mean(avg_cost),
    
    avg_understock =
      mean(avg_understock),
    
    avg_overstock =
      mean(avg_overstock),
    
    shortage_day_rate =
      mean(shortage_day_rate),
    
    demand_coverage_rate =
      mean(demand_coverage_rate),
    
    .groups = "drop"
  )

## Resultats globaux

stock_cost_results_global <- stock_cost_results_series |>
  dplyr::group_by(
    model,
    policy,
    cost_scenario
  ) |>
  dplyr::summarise(
    
    avg_cost =
      mean(avg_cost),
    
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


# 8. Calibration des quantiles utilises pour la decision

decision_calibration <- stock_results_global |>
  dplyr::filter(
    policy %in% c(
      "q50",
      "q75",
      "q90"
    )
  ) |>
  dplyr::mutate(
    
    nominal_quantile =
      dplyr::case_when(
        policy == "q50" ~ 0.50,
        policy == "q75" ~ 0.75,
        policy == "q90" ~ 0.90
      ),
    
    empirical_coverage =
      1 - shortage_day_rate,
    
    calibration_gap =
      empirical_coverage -
      nominal_quantile
  ) |>
  dplyr::select(
    model,
    policy,
    nominal_quantile,
    empirical_coverage,
    calibration_gap,
    avg_understock,
    avg_overstock
  ) |>
  dplyr::arrange(
    model,
    nominal_quantile
  )

decision_calibration

# 9. Politique theorique associee a chaque scenario

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

## Tableau principal 

matched_policy_results <- stock_cost_results_global |>
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

matched_policy_results


# 10. Graphique - cout decisionnel

cost_plot_data <- matched_policy_results |>
  dplyr::mutate(
    scenario = dplyr::case_when(
      cost_scenario == "balanced" ~ "1:1 - q50",
      cost_scenario == "shortage_x3" ~ "3:1 - q75",
      cost_scenario == "shortage_x9" ~ "9:1 - q90"
    )
  )

cost_plot_data_clean <- cost_plot_data |>
  dplyr::group_by(scenario) |>
  dplyr::mutate(
    rank = rank(
      cost_per_demand_unit,
      ties.method = "min"
    )
  ) |>
  dplyr::ungroup()

p_stock_cost <- ggplot2::ggplot(
  cost_plot_data_clean,
  ggplot2::aes(
    x = scenario,
    y = cost_per_demand_unit,
    fill = model
  )
) +
  
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.9),
    width = 0.75
  ) +
  
  ggplot2::geom_text(
    data = cost_plot_data_clean |>
      dplyr::filter(rank != 1),
    ggplot2::aes(
      label = sprintf("%.2f", cost_per_demand_unit)
    ),
    position = ggplot2::position_dodge(width = 0.9),
    vjust = -0.45,
    size = 3.2
  ) +
  
  ggplot2::geom_text(
    data = cost_plot_data_clean |>
      dplyr::filter(rank == 1),
    ggplot2::aes(
      label = sprintf("%.2f", cost_per_demand_unit)
    ),
    position = ggplot2::position_dodge(width = 0.9),
    vjust = -0.55,
    size = 4,
    fontface = "bold"
  ) +
  
  ggplot2::scale_y_continuous(
    expand = ggplot2::expansion(
      mult = c(0, 0.12)
    )
  ) +
  
  ggplot2::labs(
    title = "Coût décisionnel selon l'asymétrie des coûts",
    subtitle = "Politiques quantiles associées au fractile critique",
    x = "Ratio coût sous-stock / surstock",
    y = "Coût normalisé par unité de demande",
    fill = "Modèle"
  ) +
  
  ggplot2::theme_minimal(base_size = 11) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold",
      size = 15
    ),
    axis.title = ggplot2::element_text(
      face = "bold"
    ),
    legend.position = "bottom",
    legend.title = ggplot2::element_text(
      face = "bold"
    ),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank()
  )

p_stock_cost

# 11. Graphique - compromis sous-stock / surstock

p_stock_tradeoff <- ggplot2::ggplot(
  matched_policy_results,
  ggplot2::aes(
    x = overstock_rate,
    y = understock_rate,
    label = model
  )
) +
  ggplot2::geom_point(
    size = 3
  ) +
  ggplot2::geom_text(
    nudge_y = 0.015,
    check_overlap = TRUE
  ) +
  ggplot2::facet_wrap(
    ~ cost_scenario,
    scales = "free"
  ) +
  ggplot2::labs(
    title = "Compromis entre sous-stock et surstock",
    subtitle = "Résultats normalisés par la demande observée",
    x = "Surstock relatif",
    y = "Sous-stock relatif"
  ) +
  ggplot2::theme_minimal()

p_stock_tradeoff