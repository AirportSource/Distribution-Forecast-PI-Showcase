serie_example <- "FOODS_3_476_TX_2"


history_example <- model_data |>
  dplyr::filter(
    serie_id == serie_example,
    date >= holdout_train_end - 27,
    date <= holdout_train_end
  ) |>
  dplyr::select(
    serie_id,
    date,
    sales
  )

fan_holdout <- holdout_prob |>
  dplyr::filter(
    serie_id == serie_example
  ) |>
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
    model,
    actual,
    quantile_name,
    prediction
  ) |>
  tidyr::pivot_wider(
    names_from = quantile_name,
    values_from = prediction
  )


p_fan <- ggplot() +
  
  # Historique observé
  geom_line(
    data = history_example,
    aes(
      x = date,
      y = sales
    ),
    linewidth = 0.55,
    colour = "black"
  ) +
  
  # Intervalle 95 %
  geom_ribbon(
    data = fan_holdout,
    aes(
      x = date,
      ymin = q025,
      ymax = q975,
      fill = "95 %"
    ),
    alpha = 0.35
  ) +
  
  # Intervalle 80 %
  geom_ribbon(
    data = fan_holdout,
    aes(
      x = date,
      ymin = q10,
      ymax = q90,
      fill = "80 %"
    ),
    alpha = 0.55
  ) +
  
  # Intervalle 50 %
  geom_ribbon(
    data = fan_holdout,
    aes(
      x = date,
      ymin = q25,
      ymax = q75,
      fill = "50 %"
    ),
    alpha = 0.80
  ) +
  
  # Médiane prévisionnelle
  geom_line(
    data = fan_holdout,
    aes(
      x = date,
      y = q50
    ),
    linewidth = 0.8,
    colour = "black"
  ) +
  
  # Ventes réellement observées sur le holdout
  geom_line(
    data = fan_holdout,
    aes(
      x = date,
      y = actual
    ),
    linewidth = 0.65,
    linetype = "dashed",
    colour = "black"
  ) +
  
  # Séparation historique / prévision
  geom_vline(
    xintercept = holdout_train_end,
    linetype = "dotted",
    linewidth = 0.5
  ) +
  
  facet_wrap(
    ~ model,
    ncol = 2
  ) +
  
  # Niveaux de gris :
  # 50 % = foncé ; 95 % = clair
  scale_fill_manual(
    name = "Intervalle prédictif",
    breaks = c(
      "50 %",
      "80 %",
      "95 %"
    ),
    values = c(
      "50 %" = "grey35",
      "80 %" = "grey60",
      "95 %" = "grey82"
    )
  ) +
  
  scale_x_date(
    date_labels = "%d/%m",
    date_breaks = "2 weeks"
  ) +
  
  labs(
    title = "Prévisions probabilistes sur la période holdout",
    subtitle = paste0(
      "Série ",
      serie_example,
      " — intervalles prédictifs à 50 %, 80 % et 95 %"
    ),
    x = NULL,
    y = "Ventes quotidiennes",
    caption = paste(
      "Ligne continue : médiane prévisionnelle ;",
      "ligne pointillée : ventes observées.",
      "La verticale pointillée marque le début du holdout."
    )
  ) +
  
  theme_minimal(base_size = 11) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
    ),
    plot.title = element_text(
      face = "bold"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )

p_fan


# Figures finales

library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(readr)

source(here("R", "04_protocol.R"))

# Fonction de sauvegarde

save_figure <- function(plot, filename, width = 10, height = 6) {
  
  ggsave(
    here(
      "outputs",
      "figures",
      paste0(filename, ".png")
    ),
    plot = plot,
    width = width,
    height = height,
    dpi = 300
  )
  
  ggsave(
    here(
      "outputs",
      "figures",
      paste0(filename, ".pdf")
    ),
    plot = plot,
    width = width,
    height = height
  )
}

# FIGURE 1 - PROFIL DES SERIES SELECTIONNEES

p_series_profiles <- model_data |>
  ggplot(
    aes(
      x = date,
      y = sales
    )
  ) +
  
  geom_line(
    linewidth = 0.3
  ) +
  
  facet_wrap(
    ~ serie_id,
    ncol = 2,
    scales = "free_y"
  ) +
  
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  
  labs(
    title = "Profils des quatre séries de ventes sélectionnées",
    subtitle = paste(
      "Les séries présentent des niveaux, des variabilités",
      "et des dynamiques temporelles contrastés"
    ),
    x = NULL,
    y = "Ventes quotidiennes"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold"
    ),
    strip.text = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )

p_series_profiles


# PREPARATION DES RESULTATS CV POUR LES FIGURES 2 ET 3

stat_cv_fig <- readRDS(
  here(
    "outputs",
    "tables",
    "statistical_cv_forecasts.rds"
  )
) |>
  tibble::as_tibble()


lgb_point_cv_fig <- readRDS(
  here(
    "outputs",
    "tables",
    "lightgbm_point_cv_forecasts.rds"
  )
)

cv_scales_fig <- purrr::pmap_dfr(
  validation_folds,
  
  function(
    fold_id,
    train_end,
    test_start,
    test_end,
    ...
  ) {
    
    model_data |>
      dplyr::filter(
        date <= train_end
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
              sales -
                dplyr::lag(sales, 7)
            ),
            na.rm = TRUE
          ),
        
        rmsse_scale =
          mean(
            (
              sales -
                dplyr::lag(sales, 7)
            )^2,
            na.rm = TRUE
          ),
        
        .groups = "drop"
      ) |>
      dplyr::mutate(
        fold_id = fold_id
      )
  }
)

stat_point_fig <- stat_cv_fig |>
  dplyr::left_join(
    validation_folds |>
      dplyr::select(
        fold_id,
        fold_test_start = test_start
      ),
    by = "fold_id"
  ) |>
  dplyr::transmute(
    fold_id,
    serie_id,
    date,
    model = .model,
    actual,
    
    prediction = pmax(
      .mean,
      0
    ),
    
    horizon_step =
      as.integer(
        date - fold_test_start
      ) + 1L
  )

lgb_point_fig <- lgb_point_cv_fig |>
  dplyr::transmute(
    fold_id,
    serie_id,
    date = target_date,
    model = "lightgbm",
    actual,
    
    prediction = pmax(
      prediction_raw,
      0
    ),
    
    horizon_step
  )

point_cv_fig <- dplyr::bind_rows(
  stat_point_fig,
  lgb_point_fig
) |>
  dplyr::left_join(
    cv_scales_fig,
    by = c(
      "fold_id",
      "serie_id"
    )
  )

rmsse_fold_series <- point_cv_fig |>
  dplyr::group_by(
    fold_id,
    serie_id,
    model
  ) |>
  dplyr::summarise(
    rmsse =
      sqrt(
        mean(
          (actual - prediction)^2
        ) /
          dplyr::first(rmsse_scale)
      ),
    .groups = "drop"
  )


rmsse_by_series <- rmsse_fold_series |>
  dplyr::group_by(
    serie_id,
    model
  ) |>
  dplyr::summarise(
    rmsse = mean(rmsse),
    .groups = "drop"
  )

rmsse_by_series

p_rmsse_series <- rmsse_by_series |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = serie_id,
      y = rmsse,
      fill = model
    )
  ) +
  
  # Barres
  ggplot2::geom_col(
    position = ggplot2::position_dodge(width = 0.8),
    width = 0.7
  ) +
  
  # Valeur RMSSE au bout de chaque barre
  ggplot2::geom_text(
    ggplot2::aes(
      label = sprintf("%.2f", rmsse)
    ),
    position = ggplot2::position_dodge(width = 0.8),
    hjust = -0.15,
    size = 3
  ) +
  
  # Référence RMSSE = 1
  ggplot2::geom_hline(
    yintercept = 1,
    linetype = "dashed",
    linewidth = 0.5
  ) +
  
  # Un peu d'espace pour afficher les valeurs
  ggplot2::scale_y_continuous(
    limits = c(0, 1.08),
    breaks = seq(
      0,
      1,
      by = 0.25
    ),
    expand = ggplot2::expansion(
      mult = c(0, 0.02)
    )
  ) +
  
  # Barres horizontales
  ggplot2::coord_flip(
    clip = "off"
  ) +
  
  # Titres
  ggplot2::labs(
    title = "Précision ponctuelle selon la série",
    subtitle = "RMSSE moyen sur les 17 fenêtres de validation rolling-origin",
    x = NULL,
    y = "RMSSE",
    fill = "Modèle",
    caption = "La ligne pointillée correspond à RMSSE = 1."
  ) +
  
  # Thème
  ggplot2::theme_minimal(
    base_size = 11
  ) +
  
  ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold"
    ),
    
    strip.text = ggplot2::element_text(
      face = "bold"
    ),
    
    legend.position = "bottom",
    
    legend.title = ggplot2::element_text(
      face = "bold"
    ),
    
    panel.grid.minor = ggplot2::element_blank(),
    
    plot.margin = ggplot2::margin(
      t = 10,
      r = 25,
      b = 10,
      l = 10
    )
  )


p_rmsse_series