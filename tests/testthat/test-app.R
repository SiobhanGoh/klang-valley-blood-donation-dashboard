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

      for (id in c("overview_plot", "live_plot", "next_table", "range_table", "watch_note", "track_record", "live_change7",
                   "live_change_ytd", "audit_table", "panel_table", "weekly_plot",
                   "weekday_plot", "annual_plot", "mix_plot", "cmp_group_plot",
                   "cmp_table", "cmp_daily_plot", "cutoff_table", "low_plot",
                   "low_table", "min_pred_table", "coef_table", "bias_table",
                   "monthly_plot", "compat_result", "region_result", "cmp_headline", "cmp_sub",
                   "cmp_stat", "cmp_explain", "daily_explain", "low_headline", "low_sub", "low_stat",
                   "low_explain", "live_explain", "weekday_explain", "annual_explain", "monthly_explain")) {
        expect_no_error(output[[id]])
      }
      expect_match(output$audit_table, "490,425")
      expect_match(output$audit_table, "280 of 98,085")
      expect_match(output$panel_table, "29,220")
      expect_match(output$cutoff_table, "82.75")
      expect_match(as.character(output$compat_result$html), "A+ receives red cells from O-, O+, A-, A+", fixed = TRUE)
      expect_match(as.character(output$region_result$html), "W.P. Kuala Lumpur", fixed = TRUE)

      # Each report renders in its own voice.
      session$setInputs(report_type = "summary", rep_period = "2025")
      expect_match(as.character(output$report_preview$html), "is the one to beat", fixed = TRUE)
      session$setInputs(report_type = "data", rep_group = "O",
                        rep_dates = as.Date(c("2025-01-01", "2025-01-31")), rep_extra = "forecasts")
      expect_match(as.character(output$report_preview$html), "31 in this extract", fixed = TRUE)
      session$setInputs(report_type = "card", card_type = "O-", card_region = "Northern",
                        card_day = "1", card_month = "10", card_year = "2025")
      card <- as.character(output$report_preview$html)
      expect_match(card, "universal red-cell donor", fixed = TRUE)
      expect_match(card, "1 January 2026", fixed = TRUE)

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
                 c("overview", "live", "compare", "low", "patterns", "blood101", "about", "export"))
    html <- as.character(htmltools::renderTags(env$ui)$html)
    for (n in c("36.3", "38.8", "48.7")) expect_match(html, n, fixed = TRUE)
    expect_match(html, "Donating blood", fixed = TRUE)
    expect_match(html, "pdn.gov.my", fixed = TRUE)
  })
})

test_that("the CSV extract carries the credit and every requested row", {
  with_report_app({
    env <- new.env()
    sys.source(file.path(root, "app.R"), envir = env)
    df <- env$data_extract("all", as.Date("2025-01-01"), as.Date("2025-01-07"), TRUE)
    expect_equal(nrow(df), 28)
    expect_true(all(c("recorded", "recent_weekday_average") %in% names(df)))
    txt <- env$csv_text(df)
    expect_match(txt, "CC BY 4.0", fixed = TRUE)
  })
})

test_that("reports take a group focus, chosen sections and personal card details", {
  with_report_app({
    app <- shiny::shinyAppFile(file.path(root, "app.R"))
    shiny::testServer(app, {
      session$setInputs(report_type = "summary", rep_period = "2025", rep_focus = "O",
                        rep_sections = c("stats", "blindspot"))
      html <- as.character(output$report_preview$html)
      expect_match(html, "for group O", fixed = TRUE)
      expect_match(html, 'data-sec="blindspot"', fixed = TRUE)
      expect_false(grepl('data-sec="scoreboard"', html, fixed = TRUE))
      expect_match(html, "Read with care", fixed = TRUE)

      session$setInputs(report_type = "card", card_type = "A-", card_region = "Central",
                        card_name = "Test Donor", card_state = "Selangor", card_centre = "3",
                        card_first = "first", card_age = "61-70", card_weight = "45-55")
      html <- as.character(output$report_preview$html)
      expect_match(html, "Test Donor", fixed = TRUE)
      expect_match(html, "Group A gave", fixed = TRUE)
      expect_match(html, '<li class="is-mine" data-centre="3"', fixed = TRUE)
      # The matching tailored lines are shown; the others are drawn but hidden.
      expect_match(html, '<li data-show="age=61-70;first=first">', fixed = TRUE)
      expect_match(html, '<li data-show="weight=45-55">', fixed = TRUE)
      expect_match(html, '<li data-show="weight=over55" hidden>', fixed = TRUE)
    })
  })
})
