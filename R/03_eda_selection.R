library(DBI)
library(dplyr)
library(ggplot2)
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


# Période utilisée pour la sélection des séries

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