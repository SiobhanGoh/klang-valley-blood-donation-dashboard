# Every chart builds into valid plotly.js JSON with no warnings. (R 4.6 in the
# browser turns some warnings that R 4.4 tolerates into errors.)
root <- normalizePath(file.path("..", ".."))
source(file.path(root, "R", "analysis.R"))
source(file.path(root, "R", "charts.R"))
d <- build_report_data(file.path(root, SNAPSHOT_PATH))
y <- d$latest_year
end <- max(d$panel$date)

test_that("every chart builds cleanly into plotly.js JSON", {
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
    expect_no_warning(js <- chart_json(charts[[nm]]), message = nm)
    fig <- jsonlite::fromJSON(js, simplifyVector = FALSE)
    expect_true(length(fig$data) >= 1, info = nm)
    expect_true(all(vapply(fig$data, function(t) is.list(t$x) || length(t$x) >= 1, TRUE)), info = nm)
  }
  # The 2 x 2 grids put each group on its own axes.
  g <- jsonlite::fromJSON(chart_json(charts$group), simplifyVector = FALSE)
  expect_setequal(unique(vapply(g$data, `[[`, "", "xaxis")), c("x", "x2", "x3", "x4"))
})

test_that("a single-value series is still sent as an array", {
  fig <- figure(list(bar_trace("A", 1, "one", "#000", "tip")))
  expect_match(as.character(chart_json(fig)), '"x":["A"]', fixed = TRUE)
})
