# The plain-words readings change their wording with the size of the result.
source(file.path("..", "..", "R", "explain.R"), local = TRUE)

test_that("compare wording follows the gap between the top two methods", {
  mk <- function(a, b) data.frame(method = c("A m", "B m"), MAE = c(a, b))
  g <- data.frame(blood_type = c("A", "A"), method = c("A m", "B m"), MAE = c(1, 2))
  expect_match(explain_compare(mk(80, 100), g, c("A m", "B m"), "2024"), "clearly the most accurate")
  expect_match(explain_compare(mk(90, 100), g, c("A m", "B m"), "2024"), "a little more accurate")
  expect_match(explain_compare(mk(98, 100), g, c("A m", "B m"), "2024"), "almost level")
  expect_match(explain_compare(mk(98, 100), g, "A m", "2024"), "at least two")
})

test_that("weekday wording follows how much busier the busiest day is", {
  mk <- function(hi) data.frame(blood_type = "A", weekday = c("Sunday", "Monday"), mean_donations = c(hi, 100))
  expect_match(explain_weekday(mk(250)), "more than twice as busy")
  expect_match(explain_weekday(mk(150)), "noticeably busier")
  expect_match(explain_weekday(mk(110)), "only a little busier")
})

test_that("region wording follows the leader's share", {
  mk <- function(a, b) data.frame(state = c("X", "Y"), last30 = c(a, b))
  expect_match(explain_region(mk(80, 20), "R"), "most of the region")
  expect_match(explain_region(mk(50, 50), "R"), "collects the most")
})
