# 80% ranges, the quiet-day watch and the forecast log (R/ranges.R), on the frozen snapshot.

root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "analysis.R"))
raw <- load_snapshot(file.path(root, SNAPSHOT_PATH))
live <- build_dashboard_data(raw)
report <- build_report_data(file.path(root, SNAPSHOT_PATH))

test_that("conformal offsets use the finite-sample ranks", {
  # n = 19: lower rank floor(20 * 0.1) = 2, upper rank ceil(20 * 0.9) = 18.
  o <- conformal_offsets(c(-9:9))
  expect_equal(unname(o), c(-8, 8))
  expect_true(all(is.na(conformal_offsets(1:5))))
})

test_that("every next-day forecast has a range around it, from the latest complete year", {
  nd <- live$next_days
  expect_equal(nrow(nd), 7 * 4 * 4)
  expect_false(anyNA(nd$low) || anyNA(nd$high))
  expect_true(all(nd$low >= 0 & nd$low <= nd$prediction & nd$high >= nd$prediction))
  expect_equal(unique(live$range_offsets$calibration_year), 2025L)
  expect_true(all(live$range_offsets$calibration_days == 365L))
})

test_that("ranges built on one year hold about 80% of the next year's days", {
  rc <- report$range_coverage
  expect_equal(sort(unique(rc$tested_on)), c(2024L, 2025L))
  r <- rc[rc$tested_on == 2025L & rc$method == "Recent weekday average", ]
  expect_equal(r$calibrated_on, 2024L)
  expect_equal(r$days, 1460L)
  expect_equal(round(r$coverage, 4), 0.7890)
  # Every method and year, pinned to the numbers the README and the page report.
  got <- rc[order(rc$tested_on, factor(rc$method, levels = METHODS)), ]
  expect_equal(got$method, rep(METHODS, 2))
  expect_equal(round(got$coverage, 4),
               c(0.8374, 0.8484, 0.8490, 0.8176,   # calibrated on 2023, tested on 2024
                 0.8055, 0.7938, 0.7890, 0.7890))  # calibrated on 2024, tested on 2025
})

test_that("the quiet-day watch is scored on a year its ranges never saw", {
  wb <- report$watch_backtest[report$watch_backtest$tested_on == 2025L, ]
  expect_equal(wb$low_days, 212L)
  expect_equal(wb$flagged_and_low, 182L)
  expect_equal(round(wb$recall, 4), 0.8585)
  expect_equal(round(wb$precision, 4), 0.3176)  # many false alarms, which the page says
  w <- live$next_watch
  expect_true(all(w$watch %in% c("", "Could be quiet", "Likely quiet")))
  expect_true(all(w$watch[w$low > w$low_cutoff] == ""))
})

test_that("the forecast log only grows, and is scored once days have data", {
  log <- update_forecast_log(NULL, live$next_days, as.Date("2026-08-28"))
  expect_equal(nrow(log), 112L)
  again <- update_forecast_log(log, transform(live$next_days, prediction = 0), as.Date("2026-08-28"))
  expect_equal(again, log)  # same issue date: the first published values are kept
  later <- update_forecast_log(log, live$next_days, as.Date("2026-08-29"))
  expect_equal(nrow(later), 224L)
  expect_null(score_forecast_log(NULL, live$panel))
  # None of these days has data yet.
  expect_equal(score_forecast_log(log, live$panel)$scored, 0L)
  # The same forecasts moved a week back can be scored against recorded days.
  past <- transform(log, date = date - 7, issued = issued - 7)
  s <- score_forecast_log(past, live$panel)
  expect_equal(s$scored, 112L)
  expect_setequal(s$by_method$method, METHODS)
  tmp <- tempfile(fileext = ".csv")
  utils::write.csv(log, tmp, row.names = FALSE)
  expect_equal(read_forecast_log(tmp), log)
})
