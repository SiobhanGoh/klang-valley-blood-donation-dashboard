# Server-level checks: the app builds, every output renders without error,
# navigation works, and headline values match the analysis.
# The app runs in the "report" configuration here so results are deterministic.

root <- normalizePath(file.path("..", ".."))
methods <- c("Seven-day baseline", "Weekday average",
             "Linear regression", "Recent weekday average")

with_report_app <- function(code) {
  old_wd <- setwd(root)
  old_env <- Sys.getenv("KVBD_SOURCE", unset = NA)
  Sys.setenv(KVBD_SOURCE = "report")
  on.exit({
    setwd(old_wd)
    if (is.na(old_env)) Sys.unsetenv("KVBD_SOURCE") else Sys.setenv(KVBD_SOURCE = old_env)
  }, add = TRUE)
  force(code)
}

test_that("all app outputs render and headline values match the report", {
  with_report_app({
    app <- shiny::shinyAppFile(file.path(root, "app.R"))
    shiny::testServer(app, {
      session$setInputs(
        pattern_groups = c("A", "AB", "B", "O"), pattern_years = c(2006, 2025),
        cmp_year = "2025", cmp_methods = methods, cmp_group = "O",
        cmp_dates = as.Date(c("2025-01-01", "2025-01-31")),
        low_year = "2025", month_year = "2025", month_methods = methods,
        live_group = "A", bt_pick = "A+", region_pick = "Central"
      )

      for (id in c("overview_plot", "live_plot", "next_table", "live_change7",
                   "live_change_ytd", "audit_table", "panel_table", "weekly_plot",
                   "weekday_plot", "annual_plot", "mix_plot", "cmp_group_plot",
                   "cmp_table", "cmp_daily_plot", "cutoff_table", "low_plot",
                   "low_table", "min_pred_table", "coef_table", "bias_table",
                   "monthly_plot", "compat_result", "region_result")) {
        expect_no_error(output[[id]])
      }
      expect_match(output$audit_table, "490,425")
      expect_match(output$audit_table, "280 of 98,085")
      expect_match(output$panel_table, "29,220")
      expect_match(output$cutoff_table, "82.75")
      expect_match(as.character(output$compat_result$html), "A+ receives red cells from O-, O+, A-, A+", fixed = TRUE)
      expect_match(as.character(output$region_result$html), "W.P. Kuala Lumpur", fixed = TRUE)

      # Earlier periods switch cleanly.
      session$setInputs(cmp_year = "2023", low_year = "2023")
      expect_no_error(output$cmp_table)
      expect_no_error(output$low_table)
      session$setInputs(cmp_dates = as.Date(c("2019-01-01", "2019-02-01")))
      expect_error(output$cmp_daily_plot)  # validate(): no evaluation days
    })
  })
})

test_that("the rendered page shows the report's headline numbers", {
  with_report_app({
    env <- new.env()
    sys.source(file.path(root, "app.R"), envir = env)
    expect_equal(env$LATEST, 2025L)
    expect_equal(vapply(env$SECTIONS, `[[`, "", "id"),
                 c("overview", "live", "compare", "low", "patterns", "blood101", "about"))
    html <- as.character(htmltools::renderTags(env$ui)$html)
    for (n in c("36.28", "38.82", "48.74")) expect_match(html, n, fixed = TRUE)
    expect_match(html, "Blood 101: Malaysia", fixed = TRUE)
    expect_match(html, "pdn.gov.my", fixed = TRUE)
  })
})
