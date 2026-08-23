library(DBI)
library(dplyr)
library(purrr)
library(tibble)
library(here)

source(here("R", "00_connexion_db.R"))

# Séries finales retenues après l'analyse exploratoire

series_finales <- tibble::tribble(
  ~item_id,       ~store_id, ~role,
  "FOODS_3_156",  "CA_3",    "faible_variabilite",
  "FOODS_3_150",  "CA_1",    "fort_volume",
  "FOODS_1_206",  "TX_2",    "forte_variabilite",
  "FOODS_3_476",  "TX_2",    "saisonnalite"
) |>
  dplyr::mutate(
    serie_id = paste(item_id, store_id, sep = "_")
  )


# Import des séries finales

model_data <- DBI::dbGetQuery(
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
  ) |>
  dplyr::semi_join(
    series_finales,
    by = c("item_id", "store_id")
  ) |>
  dplyr::left_join(
    series_finales |>
      dplyr::select(serie_id, role),
    by = "serie_id"
  )

# Début de l'historique exploitable

model_data <- model_data |>
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

# Contrôles

model_data |>
  dplyr::group_by(
    serie_id,
    role
  ) |>
  dplyr::summarise(
    premiere_date = min(date),
    derniere_date = max(date),
    n_jours = dplyr::n(),
    .groups = "drop"
  )

# Paramètres du protocole

horizon <- 28L

cv_start <- as.Date("2015-01-05")

holdout_start <- as.Date("2016-04-25")
holdout_end   <- as.Date("2016-05-22")

# Construction des origines de validation

validation_folds <- tibble::tibble(
  test_start = seq.Date(
    from = cv_start,
    to = holdout_start - 1,
    by = paste(horizon, "days")
  )
) |>
  dplyr::mutate(
    test_end = test_start + (horizon - 1),
    train_end = test_start - 1
  ) |>
  dplyr::filter(
    test_end < holdout_start
  ) |>
  dplyr::mutate(
    fold_id = sprintf(
      "CV_%02d",
      dplyr::row_number()
    )
  ) |>
  dplyr::select(
    fold_id,
    train_end,
    test_start,
    test_end
  )


validation_folds

# Test final conservé hors validation

holdout <- tibble::tibble(
  fold_id = "HOLDOUT",
  train_end = holdout_start - 1,
  test_start = holdout_start,
  test_end = holdout_end
)

holdout

# Vérification du nombre d'observations par fold

fold_check <- purrr::map_dfr(
  seq_len(nrow(validation_folds)),
  function(i) {
    
    f <- validation_folds[i, ]
    
    model_data |>
      dplyr::group_by(
        serie_id
      ) |>
      dplyr::summarise(
        n_train = sum(
          date <= f$train_end
        ),
        n_test = sum(
          date >= f$test_start &
            date <= f$test_end
        ),
        .groups = "drop"
      ) |>
      dplyr::mutate(
        fold_id = f$fold_id
      )
  }
)


fold_check

# Vérification de l'horizon commun

stopifnot(
  all(fold_check$n_test == horizon)
)

# Quantiles utilisés pour l'évaluation probabiliste

quantile_probs <- c(
  0.025,
  0.10,
  0.25,
  0.50,
  0.75,
  0.90,
  0.975
)

interval_levels <- c(
  50,
  80,
  95
)


# Sauvegarde du protocole

dir.create(
  here("outputs", "tables"),
  recursive = TRUE,
  showWarnings = FALSE
)

utils::write.csv(
  validation_folds,
  here(
    "outputs",
    "tables",
    "validation_folds.csv"
  ),
  row.names = FALSE
)

saveRDS(
  model_data,
  here(
    "data",
    "processed",
    "model_data_final.rds"
  )
)
