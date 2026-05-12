library(DBI)
library(dbplyr)
library(tidyverse)
library(RPostgres)

# 00_connexion.R  ← ce fichier va sur GitHub, pas de problème
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("DB_HOST"),
  port     = as.integer(Sys.getenv("DB_PORT")),
  dbname   = Sys.getenv("DB_NAME"),
  user     = Sys.getenv("DB_USER"),
  password = Sys.getenv("DB_PASSWORD")
)

df <- dbGetQuery(con, "SELECT * FROM sales_train_evaluation LIMIT 100")
View(df)

con |>
  dbReadTable("sales_train_evaluation") |>
  as_tibble()