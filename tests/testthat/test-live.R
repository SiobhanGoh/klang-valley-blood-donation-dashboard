# The live configuration runs the same methods on whatever date the data
# reaches. These tests use the snapshot (which runs to 28 Aug 2026) so they are
# deterministic and need no network.

root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "analysis.R"))
raw <- load_snapshot(file.path(root, SNAPSHOT_PATH))
live <- build_dashboard_data(raw)          # newest complete day in the snapshot
report <- build_report_data(file.path(root, SNAPSHOT_PATH))

test_that("live periods are the last three complete years plus the year to date", {
  expect_equal(live$panel_summary$last_date, as.Date("2026-08-28"))
  expect_equal(live$periods$evaluation_year, 2023:2026)
  expect_equal(live$periods$partial, c(FALSE, FALSE, FALSE, TRUE))
  expect_match(live$periods$label[4], "2026 to date \\(to 28 Aug\\)")
  days_2026 <- as.integer(as.Date("2026-08-28") - as.Date("2026-01-01")) + 1L
  expect_equal(sum(live$predictions$evaluation_year == 2026L), days_2026 * 16L)
  expect_false(anyNA(live$predictions$prediction))
})

test_that("complete years give the same scores whichever way the data is cut", {
  a <- filter(live$scores_overall, evaluation_year %in% 2023:2025)
  b <- report$scores_overall
  m <- inner_join(a, b, by = c("evaluation_year", "method"))
  expect_equal(nrow(m), 12)
  expect_true(all(abs(m$MAE.x - m$MAE.y) < 1e-8))
})

test_that("a year with fewer than four weeks of data is not evaluated yet", {
  d <- build_dashboard_data(raw, end_date = as.Date("2026-01-20"))
  expect_equal(d$periods$evaluation_year, 2023:2025)
})

test_that("next-seven-day forecasts follow the method definitions", {
  nd <- live$next_days
  expect_equal(nrow(nd), 7 * 4 * 4)
  expect_false(anyNA(nd$prediction))
  expect_equal(range(nd$date), as.Date(c("2026-08-29", "2026-09-04")))
  # Seven-day baseline equals the observed count a week earlier.
  lag_o <- nd |> filter(blood_type == "O", method == "Seven-day baseline", date == as.Date("2026-08-29"))
  obs <- live$panel |> filter(blood_type == "O", date == as.Date("2026-08-22"))
  expect_equal(lag_o$prediction, obs$donations)
  # Recent weekday average equals the 2023-2025 mean for that weekday.
  rc <- nd |> filter(blood_type == "A", method == "Recent weekday average", date == as.Date("2026-08-31"))
  hist <- live$model_data |> filter(blood_type == "A", year(date) %in% 2023:2025, weekday == "Monday")
  expect_equal(rc$prediction, mean(hist$donations))
})

test_that("latest summary and blood-type mix are consistent with the panel", {
  ls <- live$latest
  expect_equal(ls$end, as.Date("2026-08-28"))
  expect_equal(ls$latest_total, sum(live$panel$donations[live$panel$date == ls$end]))
  mix <- live$blood_mix |> group_by(year) |> summarise(s = sum(share))
  expect_true(all(abs(mix$s - 1) < 1e-12))
})

test_that("findings are generated from the results", {
  f <- report$findings
  expect_match(f[1], "recent weekday average is the most accurate method in every test period, 2023 to 2025")
  expect_true(any(grepl("Linear regression is more accurate than the seven-day baseline", f)))
  expect_true(any(grepl("AB is the exception", f)))
  expect_true(any(grepl("most accurate for A, B and O", f)))
  expect_true(length(live$findings) >= 2)
})

test_that("a failed download falls back to the snapshot", {
  expect_null(load_live(cache_dir = tempfile(), url = "https://invalid.example.invalid/x.csv",
                        timeout = 5))
})
