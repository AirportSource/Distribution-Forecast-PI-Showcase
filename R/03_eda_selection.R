library(DBI)
library(dplyr)
library(ggplot2)
library(tidyr)
library(here)


# Import des séries candidates depuis PostgreSQL avec corrections Integer64 nombre entiers

source(here("R", "00_connexion_db.R"))

candidates <- DBI::dbGetQuery(
  con,
  "
  SELECT *
  FROM series_candidates
  ORDER BY item_id, store_id, date
  "
) |>
  dplyr::mutate(
    date = as.Date(date),
    sales = as.numeric(sales),
    sell_price = as.numeric(sell_price),
    wday = as.integer(wday),
    serie_id = paste(item_id, store_id, sep = "_")
  )


# Vérification de l'import

candidates |>
  dplyr::summarise(
    n_series = dplyr::n_distinct(serie_id),
    n_rows = dplyr::n()
  )

candidates |>
  dplyr::count(serie_id)

sum(is.na(candidates$sales))

# Période et prix utilisée pour la sélection des séries

candidates_eda <- candidates |>
  dplyr::filter(
    date < as.Date("2015-01-01")
  ) |>
  dplyr::group_by(serie_id) |>
  dplyr::mutate(
    premiere_date_prix = min(
      date[!is.na(sell_price)]
    )
  ) |>
  dplyr::filter(
    date >= premiere_date_prix
  ) |>
  dplyr::ungroup()

#Verification dispersion 0

max_zero_run <- function(x) {
  r <- rle(x == 0)
  
  if (!any(r$values)) {
    return(0)
  }
  
  max(r$lengths[r$values])
}

zero_structure <- candidates_eda |>
  dplyr::group_by(
    profil,
    statut,
    serie_id
  ) |>
  dplyr::summarise(
    taux_zeros = mean(sales == 0),
    max_zero_run = max_zero_run(sales),
    .groups = "drop"
  )

zero_structure

# Calcul du SNAP comme variable explicative pour chaque Etat

candidates_eda <- candidates_eda |>
  dplyr::mutate(
    snap = dplyr::case_when(
      grepl("^CA_", store_id) ~ snap_CA,
      grepl("^TX_", store_id) ~ snap_TX,
      grepl("^WI_", store_id) ~ snap_WI
    )
  )

# Resume des donnees SNAP et Effets du SNAP
snap_summary <- candidates_eda |>
  dplyr::group_by(
    profil,
    statut,
    serie_id,
    snap
  ) |>
  dplyr::summarise(
    ventes_moyennes = mean(sales),
    n_jours = dplyr::n(),
    .groups = "drop"
  )

snap_summary

snap_effect <- snap_summary |>
  dplyr::select(
    profil,
    statut,
    serie_id,
    snap,
    ventes_moyennes
  ) |>
  tidyr::pivot_wider(
    id_cols = c(
      profil,
      statut,
      serie_id
    ),
    names_from = snap,
    values_from = ventes_moyennes,
    names_prefix = "snap_"
  ) |>
  dplyr::mutate(
    ecart_snap = snap_1 - snap_0,
    ratio_snap = snap_1 / snap_0
  )

snap_effect


# Caractéristiques des séries sur la période de sélection

eda_summary <- candidates_eda |>
  dplyr::group_by(
    profil,
    statut,
    serie_id
  ) |>
  dplyr::summarise(
    n_jours = dplyr::n(),
    ventes_moyennes = mean(sales),
    ecart_type = sd(sales),
    cv = ecart_type / ventes_moyennes,
    taux_zeros = mean(sales == 0),
    taux_prix_manquant = mean(is.na(sell_price)),
    .groups = "drop"
  )

eda_summary


# Évolution chronologique des séries candidates

p_series <- candidates_eda |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = date,
      y = sales
    )
  ) +
  ggplot2::geom_line(
    linewidth = 0.25
  ) +
  ggplot2::facet_wrap(
    ~ profil + serie_id,
    scales = "free_y",
    ncol = 3
  ) +
  ggplot2::labs(
    title = "Évolution des ventes des séries candidates avant 2015",
    x = NULL,
    y = "Ventes quotidiennes"
  ) +
  ggplot2::theme_minimal()

p_series


# Profil hebdomadaire moyen

weekday_profile <- candidates_eda |>
  dplyr::group_by(
    profil,
    serie_id,
    wday,
    weekday
  ) |>
  dplyr::summarise(
    ventes_moyennes = mean(sales),
    .groups = "drop"
  ) |>
  dplyr::arrange(
    serie_id,
    wday
  )


p_weekday <- weekday_profile |>
  ggplot2::ggplot(
    ggplot2::aes(
      x = wday,
      y = ventes_moyennes,
      group = 1
    )
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point() +
  ggplot2::facet_wrap(
    ~ profil + serie_id,
    scales = "free_y",
    ncol = 3
  ) +
  ggplot2::scale_x_continuous(
    breaks = 1:7,
    labels = c(
      "Sam",
      "Dim",
      "Lun",
      "Mar",
      "Mer",
      "Jeu",
      "Ven"
    )
  ) +
  ggplot2::labs(
    title = "Profil hebdomadaire moyen des séries candidates avant 2015",
    x = NULL,
    y = "Ventes quotidiennes moyennes"
  ) +
  ggplot2::theme_minimal()

p_weekday

# Calcul Seasonality and Trend using Loess STL saisonnalite 7 jours

library(tsibble)
library(feasts)

candidates_ts <- candidates_eda |>
  tsibble::as_tsibble(
    key = serie_id,
    index = date
  )

stl_features <- candidates_ts |>
  fabletools::features(
    sales,
    feasts::feat_stl,
    .period = 7
  )

stl_features

# Calcul de l'ACF Autocorrelation Function

acf_candidates <- candidates_ts |>
  feasts::ACF(
    sales,
    lag_max = 28
  )

p_acf <- acf_candidates |>
  ggplot2::autoplot() +
  ggplot2::facet_wrap(
    ~ serie_id,
    scales = "free_y",
    ncol = 3
  ) +
  ggplot2::labs(
    title = "Autocorrélation des séries candidates avant 2015",
    x = "Retard",
    y = "Autocorrélation"
  ) +
  ggplot2::theme_minimal()

p_acf

# Selection finale de 4 series

series_finales <- tibble::tribble(
  ~item_id,       ~store_id, ~profil,
  "FOODS_3_156",  "CA_3",    "faible_variabilite",
  "FOODS_3_150",  "CA_1",    "fort_volume",
  "FOODS_1_206",  "TX_2",    "forte_variabilite",
  "FOODS_3_476",  "TX_2",    "saisonnalite"
)
series_finales

# Enregistrement des graphs

ggsave(here("outputs", "figures", "01_series.png"), p_series, width = 10, height = 6, dpi = 300)
ggsave(here("outputs", "figures", "02_weekday.png"), p_weekday, width = 10, height = 6, dpi = 300)
ggsave(here("outputs", "figures", "03_acf.png"), p_acf, width = 10, height = 6, dpi = 300)