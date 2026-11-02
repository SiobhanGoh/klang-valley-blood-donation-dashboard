# Server-level checks: the app builds, every output renders without error,
# navigation and the game update state, and headline values match the analysis.

root <- normalizePath(file.path("..", ".."))

test_that("all app outputs render and headline values match the analysis", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  methods <- c("Seven-day baseline", "Weekday average",
               "Linear regression", "Recent weekday average")
  app <- shiny::shinyAppFile(file.path(root, "app.R"))

  shiny::testServer(app, {
    session$setInputs(
      pattern_groups = c("A", "AB", "B", "O"), pattern_years = c(2006, 2025),
      cmp_year = "2025", cmp_methods = methods, cmp_group = "O",
      cmp_dates = as.Date(c("2025-01-01", "2025-01-31")),
      month_methods = methods
    )
    expect_equal(output$headline_recent, "36.28")
    expect_equal(output$headline_lm, "38.82")
    expect_equal(output$headline_lag, "48.74")

    for (id in c("overview_plot", "audit_table", "panel_table", "weekly_plot",
                 "weekday_plot", "annual_plot", "cmp_group_plot", "cmp_table",
                 "cmp_daily_plot", "cutoff_table", "low_plot", "low_table",
                 "min_pred_table", "coef_table", "bias_table", "monthly_plot",
                 "game_round", "game_result", "game_stats")) {
      expect_no_error(output[[id]])
    }
    expect_match(output$audit_table, "490,425")
    expect_match(output$audit_table, "280 of 98,085")
    expect_match(output$panel_table, "29,220")
    expect_match(output$cutoff_table, "82.75")

    # Earlier evaluation years switch cleanly.
    session$setInputs(cmp_year = "2023")
    expect_no_error(output$cmp_table)
    session$setInputs(cmp_dates = as.Date(c("2019-01-01", "2019-02-01")))
    expect_error(output$cmp_daily_plot)  # validate(): no evaluation days
  })
})

test_that("a game round scores a guess and awards XP and badges", {
  old <- setwd(root)
  on.exit(setwd(old), add = TRUE)
  app <- shiny::shinyAppFile(file.path(root, "app.R"))

  shiny::testServer(app, {
    session$flushReact()
    expect_false(is.null(st$round))
    expect_true(is.null(st$result))
    expect_match(as.character(output$game_round$html), "Typical for group")

    # Guess the exact recorded count: every method is beaten.
    session$setInputs(guess = st$round$actual, lock_guess = 1)
    expect_equal(st$rounds, 1L)
    expect_setequal(st$result$beaten, METHODS)
    expect_equal(st$xp, 4L * GAME_XP_PER_METHOD + GAME_XP_CLOSE_BONUS)
    expect_true(all(c("first_guess", "beat_all") %in% st$badges))
    expect_match(as.character(output$game_result$html), "Clean sweep")

    # Locking again before a new round changes nothing.
    session$setInputs(lock_guess = 2)
    expect_equal(st$rounds, 1L)

    # A new round clears the result.
    session$setInputs(new_round = 1)
    expect_true(is.null(st$result))

    # Visiting sections earns explorer XP once each, then the Explorer badge.
    xp_before <- st$xp
    for (sct in c("play", "compare", "low", "patterns", "explain", "audit", "methods")) {
      session$setInputs(pages = sct)
    }
    session$setInputs(pages = "compare")
    expect_equal(st$xp - xp_before, 7L * EXPLORER_XP)
    expect_true("explorer" %in% st$badges)
  })
})
