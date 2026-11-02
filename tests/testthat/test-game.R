root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "analysis.R"))
source(file.path(root, "R", "game.R"))
d <- build_dashboard_data(file.path(root, SNAPSHOT_PATH))
pool <- game_pool(d$predictions, d$low_cutoffs_2025, game_hint_table(d$model_data))

test_that("the game pool is exactly the 2025 evaluation rows", {
  expect_equal(nrow(pool), 1460)
  expect_true(all(lubridate::year(pool$date) == 2025))
  expect_false(anyNA(pool[METHODS]))
  expect_equal(sum(pool$low_day), 40 + 105 + 39 + 28)
  expect_true(all(pool$typical_low <= pool$typical_high))
})

test_that("round predictions match the evaluation predictions", {
  r <- new_round(pool, 1L)
  p <- d$predictions |>
    filter(date == r$date, blood_type == r$blood_type)
  for (m in METHODS) {
    expect_equal(unname(r$predictions[m]), p$prediction[p$method == m])
  }
  expect_equal(r$actual, unique(p$donations))
})

test_that("scoring rewards beating methods and close guesses", {
  r <- list(actual = 100, low_day = FALSE,
            predictions = setNames(c(150, 130, 112, 95), METHODS))
  exact <- score_guess(r, 100)
  expect_setequal(exact$beaten, METHODS)
  expect_true(exact$close)
  expect_equal(exact$xp, 4 * GAME_XP_PER_METHOD + GAME_XP_CLOSE_BONUS)

  far <- score_guess(r, 300)
  expect_length(far$beaten, 0)
  expect_false(far$close)
  expect_equal(far$xp, 0)

  # Ties do not count as beating a method.
  tie <- score_guess(r, 105)
  expect_false("Recent weekday average" %in% tie$beaten)
  expect_error(score_guess(r, -1))
})

test_that("badges are awarded once and levels advance every 100 XP", {
  r <- list(actual = 50, low_day = TRUE,
            predictions = setNames(c(90, 80, 70, 60), METHODS))
  res <- score_guess(r, 50)
  b <- earned_badges(res, r, character(), regression_streak = 3L)
  expect_setequal(b, c("first_guess", "beat_lag", "beat_regression",
                       "beat_all", "streak_3", "low_day"))
  expect_length(earned_badges(res, r, b, 3L), 0)
  expect_equal(level_for(0)$level, 1)
  expect_equal(level_for(250)$level, 3)
  expect_equal(level_for(250)$progress, 0.5)
})
