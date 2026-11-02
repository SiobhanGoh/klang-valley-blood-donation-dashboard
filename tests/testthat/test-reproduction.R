# These tests confirm that the dashboard's recomputed results equal the outputs
# saved by the final FIT5145 Assignment 3 R Markdown analysis (reference/).

root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "analysis.R"))
d <- build_report_data(file.path(root, SNAPSHOT_PATH))
low25 <- filter(d$low_errors, evaluation_year == 2025L)
cut25 <- filter(d$low_cutoffs, evaluation_year == 2025L)
ref <- function(name) readr::read_csv(file.path(root, "reference", name),
                               show_col_types = FALSE)
tol <- 1e-8

join_and_compare <- function(ours, theirs, keys, cols) {
  merged <- inner_join(ours, theirs, by = keys, suffix = c("", ".ref"))
  expect_equal(nrow(merged), nrow(theirs))
  for (col in cols) {
    expect_true(all(abs(merged[[col]] - merged[[paste0(col, ".ref")]]) < tol),
                info = col)
  }
}

test_that("raw snapshot audit matches the report", {
  a <- d$audit
  expect_identical(a$checksum, SNAPSHOT_MD5)
  expect_equal(a$rows, 490425)
  expect_equal(a$columns, 4)
  expect_equal(a$distinct_dates, 7545)
  expect_equal(a$first_date, as.Date("2006-01-01"))
  expect_equal(a$last_date, as.Date("2026-08-28"))
  expect_equal(a$regions, 13)
  expect_equal(a$blood_type_values, 5)
  expect_equal(a$missing_values, 0)
  expect_equal(a$duplicate_rows, 0)
  expect_equal(a$duplicate_keys, 0)
  expect_equal(a$negative_values, 0)
  expect_equal(a$zero_values, 68749)
  expect_equal(a$all_rows_checked, 98085)
  expect_equal(a$all_mismatches, 280)
  expect_equal(a$all_max_abs_difference, 3)
})

test_that("Klang Valley panel equals the saved processed dataset", {
  expect_equal(d$panel_summary$rows, 29220)
  expect_equal(d$panel_summary$dates, 7305)
  expect_equal(d$panel_summary$zero_values, 123)
  saved <- ref("klang_valley_daily_abo.csv")
  expect_equal(nrow(saved), nrow(d$panel))
  expect_equal(saved$date, d$panel$date)
  expect_equal(saved$blood_type, d$panel$blood_type)
  expect_equal(saved$donations, d$panel$donations)
})

test_that("four-method, three-year pooled scores reproduce the Rmd", {
  join_and_compare(d$scores_overall, ref("repeated_year_overall_errors.csv"),
                   c("evaluation_year", "method"),
                   c("observations", "MAE", "RMSE"))
})

test_that("blood-group scores reproduce the Rmd", {
  join_and_compare(d$scores_by_group, ref("repeated_year_group_errors.csv"),
                   c("evaluation_year", "blood_type", "method"),
                   c("observations", "MAE", "RMSE"))
})

test_that("headline 2025 figures are as reported", {
  s <- d$scores_overall |> filter(evaluation_year == 2025L)
  get <- function(m, col) round(s[[col]][s$method == m], 2)
  expect_equal(get("Recent weekday average", "MAE"), 36.28)
  expect_equal(get("Recent weekday average", "RMSE"), 56.47)
  expect_equal(get("Linear regression", "MAE"), 38.82)
  expect_equal(get("Weekday average", "MAE"), 43.60)
  expect_equal(get("Seven-day baseline", "MAE"), 48.74)
  join_and_compare(s, ref("final_four_method_2025.csv"), "method",
                   c("observations", "MAE", "RMSE"))
})

test_that("low-collection diagnostics reproduce the Rmd", {
  join_and_compare(low25, ref("low_collection_day_errors.csv"),
                   c("blood_type", "method", "day_category"),
                   c("days", "mean_actual", "MAE",
                     "overprediction_percent", "mean_excess"))
  join_and_compare(low25, ref("final_four_method_low_O.csv"),
                   c("blood_type", "method", "day_category"),
                   c("days", "mean_actual", "MAE",
                     "overprediction_percent", "mean_excess"))
  cut <- setNames(cut25$low_cutoff, cut25$blood_type)
  expect_equal(unname(cut[c("A", "AB", "B", "O")]), c(82.75, 19, 91, 139))
})

test_that("regression coefficient explanation reproduces the Rmd", {
  join_and_compare(d$coefficients, ref("coefficient_explanation.csv"),
                   "blood_type", c("change_per_365_days", "sunday_minus_monday"))
})

test_that("no evaluation-year outcome is used for fitting", {
  p <- d$predictions
  expect_true(all(p$training_end < as.Date(paste0(p$evaluation_year, "-01-01"))))
  expect_equal(nrow(p), 17536)
  expect_false(anyNA(p$prediction))
})

test_that("claims written on the Overview tab hold in the data", {
  best <- d$scores_overall |> group_by(evaluation_year) |>
    summarise(mae = method[which.min(MAE)], rmse = method[which.min(RMSE)])
  expect_true(all(best$mae == "Recent weekday average"))
  expect_true(all(best$rmse == "Recent weekday average"))

  g <- d$scores_by_group |>
    select(evaluation_year, blood_type, method, MAE) |>
    pivot_wider(names_from = method, values_from = MAE)
  expect_true(all(g$`Linear regression` < g$`Seven-day baseline`))
  non_ab <- filter(g, blood_type != "AB")
  expect_true(all(non_ab$`Linear regression` < non_ab$`Weekday average`))
  ab <- filter(g, blood_type == "AB")
  expect_true(all(ab$`Weekday average` < ab$`Linear regression`))
  ab25 <- filter(ab, evaluation_year == 2025L)
  expect_true(ab25$`Weekday average` < ab25$`Recent weekday average`)

  low <- low25 |>
    select(blood_type, method, day_category, MAE) |>
    pivot_wider(names_from = day_category, values_from = MAE)
  lr <- filter(low, method %in% c("Linear regression", "Recent weekday average"))
  expect_true(all(lr$`Low collection` > lr$`Other days`))
  low_only <- filter(low25, day_category == "Low collection")
  best_low <- low_only |> group_by(blood_type) |>
    summarise(best = method[which.min(MAE)])
  expect_true(all(best_low$best[best_low$blood_type != "AB"] == "Weekday average"))
  expect_true(all(low_only$overprediction_percent[
    low_only$method == "Weekday average"] == 100))
})
