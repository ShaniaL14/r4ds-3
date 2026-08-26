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
atus_row_counts <- tibble(
  table = c("activities", "respondents", "activity_codes"),
  rows = c(
    dbplyr_act |> count() |> collect() |> pull(n),
    dbplyr_resp |> count() |> collect() |> pull(n),
    dbplyr_codes |> count() |> collect() |> pull(n)
  )
)
activity_category_counts <- dbplyr_act |>
  inner_join(dbplyr_codes, by = "activity_code") |>
  count(major_name, name = "n") |>
  arrange(desc(n)) |>
  collect()
average_activity_length <- dbplyr_act |>
  inner_join(dbplyr_codes, by = "activity_code") |>
  filter(!is.na(duration_min)) |>
  group_by(major_name) |>
  summarise(average_duration_min = mean(duration_min)) |>
  arrange(desc(average_duration_min)) |>
  collect()
respondent_counts <- dbplyr_resp |>
  count(sex, employment_status, name = "n") |>
  collect()
respondent_counts_by_year <- dbplyr_resp |>
  count(year, name = "n") |>
  arrange(year) |>
  collect()
weekday_nlf_activity_minutes <- dbplyr_act |>
  inner_join(
    dbplyr_resp |>
      filter(day_type == "Weekday", employment_status == "Not in labor force") |>
      select(tucaseid, sex),
    by = "tucaseid"
  ) |>
  inner_join(
    dbplyr_codes |>
      select(activity_code, major_name),
    by = "activity_code"
  ) |>
  group_by(tucaseid, sex, major_name) |>
  summarise(minutes = sum(duration_min, na.rm = TRUE), .groups = "drop") |>
  collect()
activity_avg <- weekday_nlf_activity_minutes |>
  group_by(sex, major_name) |>
  summarise(mean_minutes = mean(minutes), .groups = "drop")
#
#
#
keep_cats <- c(
  "Personal Care Activities",
  "Household Activities",
  "Caring For & Helping Household (HH) Members",
  "Socializing, Relaxing, and Leisure"
)

hourly_avg <- dbplyr_act |>
  mutate(hour = start_hhmm %/% 100L) |>
  filter(hour >= 6L, hour <= 23L) |>
  left_join(dbplyr_codes |> select(activity_code, major_name), by = "activity_code") |>
  filter(major_name %in% keep_cats) |>
  inner_join(
    dbplyr_resp |>
      filter(str_starts(employment_status, "Not"), day_type == "Weekday") |>
      select(tucaseid, sex),
    by = "tucaseid"
  ) |>
  group_by(sex, hour, major_name) |>
  summarise(avg_min = mean(duration_min, na.rm = TRUE), .groups = "drop") |>
  collect()
#
#
#
activity_avg_plot <- activity_avg |>
  filter(major_name != "Data Codes") |>
  group_by(major_name) |>
  mutate(category_average = mean(mean_minutes)) |>
  ungroup() |>
  mutate(major_name = forcats::fct_reorder(major_name, category_average))

ggplot(activity_avg_plot, aes(x = mean_minutes, y = major_name, fill = sex)) +
  geom_col(position = position_dodge()) +
  labs(
    title = "Average Daily Minutes by Major Activity Category",
    subtitle = "Weekday averages for non-employed respondents, by sex",
    x = "Average daily minutes",
    y = "Major activity category",
    caption = "Source: ATUS database; Data Codes excluded as missing-data bookkeeping."
  )
#
#
#
ggplot(hourly_avg, aes(x = hour, y = avg_min, fill = major_name)) +
  geom_area() +
  facet_wrap(~sex) +
  scale_x_continuous(
    breaks = seq(6, 23, by = 3),
    labels = \(hour) sprintf("%02d:00", hour)
  ) +
  labs(
    title = "How the Day Fills Up",
    subtitle = "Average minutes spent in selected activities by hour and sex",
    x = "Hour of day",
    y = "Average minutes",
    fill = "Major activity category",
    caption = "Source: ATUS database; weekday non-employed respondents, hours 6:00 to 23:00."
  )
#
#
#
#
