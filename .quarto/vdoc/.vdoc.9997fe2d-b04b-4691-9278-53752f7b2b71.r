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
  mutate(rla_change = rla_2023 - rla_2015) |>
  select(stateabb, rla_change) |>
  arrange(rla_change)
state_change
#
#
#
rla_2023 <- dbplyr_dist |>
  filter(year == 2023, !is.na(rla_score)) |>
  select(rla_score) |>
  collect()

ggplot(rla_2023, aes(x = rla_score)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of 2023 Reading Scores",
    subtitle = paste("District count:", nrow(rla_2023)),
    x = "Reading score",
    y = "Number of districts",
    caption = "Source: SEDA 2025 district scores database."
  )
#
#
#
rla_change <- score_change |>
  rename(rla_change = score_change)

ggplot(rla_change, aes(x = rla_change, fill = rla_change > 0)) +
  geom_histogram(bins = 30) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(
    title = "Distribution of District Reading Score Changes",
    subtitle = paste("District count:", nrow(rla_change)),
    x = "Reading score change",
    y = "Number of districts",
    fill = "Change is positive",
    caption = "Source: SEDA 2025 district scores database; change is 2023 minus 2015."
  )
#
#
#
state_scores_plot <- tbl(con, "state_scores") |>
  filter(year %in% c(2015, 2023), !is.na(rla_score)) |>
  select(stateabb, year, rla_score) |>
  collect() |>
  tidyr::pivot_wider(names_from = year, values_from = rla_score, names_prefix = "rla_") |>
  filter(!is.na(rla_2015), !is.na(rla_2023)) |>
  arrange(rla_2023) |>
  mutate(stateabb = factor(stateabb, levels = stateabb))

ggplot(state_scores_plot, aes(y = stateabb)) +
  geom_segment(
    aes(x = rla_2015, xend = rla_2023, yend = stateabb, color = rla_2023 > rla_2015),
    arrow = grid::arrow(length = grid::unit(0.12, "inches"), type = "closed")
  ) +
  geom_point(aes(x = rla_2015), color = "gray50") +
  scale_color_manual(
    values = c(`FALSE` = "firebrick", `TRUE` = "steelblue"),
    labels = c(`FALSE` = "Decline", `TRUE` = "Improvement"),
    name = "Direction"
  ) +
  labs(
    title = "State Reading Score Changes, 2015 to 2023",
    subtitle = "Arrows show each state's change in reading score",
    x = "Reading score",
    y = NULL,
    caption = "Source: SEDA 2025 state scores database. Gray dots mark 2015 scores."
  )
#
#
#
atus_con <- dbConnect(duckdb(), "data/atus.duckdb")
dbplyr_act <- tbl(atus_con, "activities")
dbplyr_resp <- tbl(atus_con, "respondents")
dbplyr_codes <- tbl(atus_con, "activity_codes")
dbplyr_resp
#
#
#
#
