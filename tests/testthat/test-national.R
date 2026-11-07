# The "Every state" page reads DarahWatch's published files. These fixtures are a real DarahWatch
# run on the frozen snapshot (data to 28 Aug 2026).

root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "common.R"))
source(file.path(root, "R", "national.R"))
fixture <- file.path(root, "tests", "testthat", "fixtures", "darahwatch")

test_that("DarahWatch's files are read into plain tables", {
  nat <- read_darahwatch(fixture)
  expect_equal(nat$issued, as.Date("2026-08-28"))
  expect_equal(nat$method, "Gradient-boosted trees")
  expect_equal(nrow(nat$weeks), 13 * 4 * 2)
  expect_equal(nrow(nat$days), 13 * 4 * 14)
  expect_true(all(nat$weeks$low <= nat$weeks$forecast & nat$weeks$forecast <= nat$weeks$high))
  expect_true(all(nat$weeks$warning %in% c("", "Watch", "Likely low")))
  expect_equal(nrow(nat$by_state), 13)
  # The check is the latest complete year, never the year in progress.
  expect_equal(nat$check$year, 2025L)
  expect_false(nat$check$partial)
  expect_equal(round(nat$check$coverage, 3), 0.795)
  expect_true(nzchar(nat$bulletin$en) && nzchar(nat$bulletin$ms))
})

test_that("missing DarahWatch files give NULL, not an error", {
  expect_null(read_darahwatch(tempfile()))
})

test_that("the page renders with and without a national forecast", {
  old <- setwd(root); on.exit(setwd(old), add = TRUE)
  old_env <- Sys.getenv("KVBD_SOURCE", unset = NA)
  Sys.setenv(KVBD_SOURCE = "report")
  on.exit(if (is.na(old_env)) Sys.unsetenv("KVBD_SOURCE") else Sys.setenv(KVBD_SOURCE = old_env), add = TRUE)
  env <- new.env()
  sys.source(file.path(root, "app.R"), envir = env)
  soon <- as.character(htmltools::renderTags(env$states_page(NULL))$html)
  expect_match(soon, "coming soon", fixed = TRUE)
  nat <- read_darahwatch(fixture)
  html <- as.character(htmltools::renderTags(env$states_page(nat))$html)
  expect_match(html, "No state and blood group expects a low week ahead.", fixed = TRUE)
  expect_match(html, "W.P. Kuala Lumpur", fixed = TRUE)
  expect_match(html, "Bahasa Malaysia", fixed = TRUE)
  expect_match(html, "1,013", fixed = TRUE)  # KL group A, next 7 days
  nat$weeks$warning[1] <- "Likely low"
  html <- as.character(htmltools::renderTags(env$states_page(nat))$html)
  expect_match(html, "1 state and blood group pair could have a low week ahead.", fixed = TRUE)
  expect_match(html, "nat-cell nat-likely", fixed = TRUE)
})
