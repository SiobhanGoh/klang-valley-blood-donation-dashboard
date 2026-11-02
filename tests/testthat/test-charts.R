# Every chart must build without warnings: R 4.6 (used by the published site
# in the browser) turns some warnings that R 4.4 tolerates into errors.
root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "analysis.R"))
source(file.path(root, "R", "charts.R"))
d <- build_report_data(file.path(root, SNAPSHOT_PATH))
y <- d$latest_year
end <- max(d$panel$date)

test_that("every chart builds cleanly", {
  charts <- list(
    pooled = chart_pooled_mae(d$scores_overall, setNames(d$periods$label, d$periods$evaluation_year)),
    group = chart_group_mae(filter(d$scores_by_group, evaluation_year == y), METHODS),
    low = chart_low_days(filter(d$low_errors, evaluation_year == y)),
    live = chart_live(d$panel, d$predictions, d$next_days, "O", end),
    daily = chart_daily(d$predictions, "O", METHODS, end - 30, end),
    months = chart_month_totals(d$panel, end),
    annual = chart_annual(annual_summary(d$panel)),
    mix = chart_mix(d$blood_mix, NULL),
    monthly = chart_monthly(filter(d$monthly, evaluation_year == y), METHODS)
  )
  for (nm in names(charts)) {
    expect_no_warning(plotly_build(charts[[nm]]), message = nm)
  }
})
