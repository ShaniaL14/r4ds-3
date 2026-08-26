#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(DBI)
library(duckdb)
library(dbplyr)
#
#
#
con <- dbConnect(duckdb(), "data/seda_2025.duckdb")
dbplyr_dist <- tbl(con, "district_scores")
#
#
#
#| cache: true
#| cache.extra: !expr file.mtime("data/seda_2025.duckdb")
score_change <- dbplyr_dist |>
  filter(year %in% c(2015, 2023), !is.na(rla_score)) |>
  select(district_id, year, rla_score) |>
  collect() |>
  tidyr::pivot_wider(names_from = year, values_from = rla_score, names_prefix = "rla_") |>
  filter(!is.na(rla_2015), !is.na(rla_2023)) |>
  mutate(score_change = rla_2023 - rla_2015) |>
  select(district_id, score_change)

state_change <- tbl(con, "state_scores") |>
  filter(year %in% c(2015, 2023), !is.na(rla_score)) |>
  select(stateabb, year, rla_score) |>
  collect() |>
  tidyr::pivot_wider(names_from = year, values_from = rla_score, names_prefix = "rla_") |>
  filter(!is.na(rla_2015), !is.na(rla_2023)) |>
  mutate(state_change = rla_2023 - rla_2015) |>
  select(stateabb, state_change)
#
#
#
#
