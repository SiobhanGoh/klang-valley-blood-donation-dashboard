# Every state: DarahWatch's national forecasts, read from its published files.
#
# DarahWatch (github.com/SiobhanGoh/darahwatch) forecasts every state and blood group 14 days
# ahead each morning. The publish workflow downloads two of its files into app_data/darahwatch/:
#   forecast.json  the latest forecast, week totals, warnings, bulletin and scorecard
#   backtest.json  its honest backtests (ranges built on one year, checked on the next)
# This file turns them into plain data frames for the "Every state" page. If they are missing,
# the page says the national forecast is not available yet, and nothing else changes.

DARAHWATCH_URL <- "https://github.com/SiobhanGoh/darahwatch"

# A number from JSON, or NA when it is missing or null.
num <- function(x) if (is.null(x) || !length(x)) NA_real_ else as.numeric(x)

# A file that can't be read (half-downloaded, a format change) also gives NULL, with a warning,
# so the rest of the site still builds.
read_darahwatch <- function(dir = "app_data/darahwatch") {
  tryCatch(read_darahwatch_files(dir), error = function(e) {
    warning("Could not read DarahWatch's files: ", conditionMessage(e), call. = FALSE)
    NULL
  })
}

read_darahwatch_files <- function(dir) {
  f <- file.path(dir, "forecast.json")
  b <- file.path(dir, "backtest.json")
  if (!file.exists(f) || !file.exists(b)) return(NULL)
  fc <- jsonlite::fromJSON(f, simplifyVector = TRUE)
  # The scorecard stays as plain lists (simplifying would turn its list of methods into a table).
  fc$scorecard <- jsonlite::fromJSON(f, simplifyVector = FALSE)$scorecard
  bt <- jsonlite::fromJSON(b, simplifyVector = FALSE)
  years <- bt$years
  scored <- Filter(function(y) !is.null(y$by_state), years)
  latest <- scored[[length(scored)]]
  complete <- Filter(function(y) !isTRUE(y$partial), scored)
  check <- if (length(complete)) complete[[length(complete)]] else latest
  pick <- function(rows, method) Filter(function(r) identical(r$method, method), rows)[[1]]
  method <- fc$method
  by_state <- do.call(rbind, lapply(check$by_state, function(r) data.frame(
    state = r$state, coverage = r$coverage, MAE = r$MAE, MAE_simple = r$MAE_simple,
    weeks = r$weeks, really_low = r$really_low_weeks, warnings = r$warnings,
    hit_rate = num(r$warning_hit_rate), catch_rate = num(r$warning_catch_rate),
    stringsAsFactors = FALSE)))
  rule <- check$low_week_rule[[method]]
  regions <- length(unique(fc$weeks$state))
  mae <- function(m) pick(check$mae, m)
  list(
    issued = as.Date(fc$issued),
    method = method,
    bulletin = fc$bulletin,
    weeks = transform(as.data.frame(fc$weeks), first = as.Date(first), last = as.Date(last)),
    days = transform(as.data.frame(fc$days), date = as.Date(date)),
    holidays = if (length(fc$holidays)) as.data.frame(fc$holidays) else data.frame(),
    scorecard = fc$scorecard,
    check = list(year = check$year, partial = isTRUE(check$partial),
                 coverage = pick(check$daily_range, method)$coverage,
                 week_coverage = pick(check$weekly_range, method)$coverage,
                 mae = mae(method)$MAE, mae_simple = mae("Recent weekday average")$MAE,
                 holiday = mae(method)$by_day$`Public holiday`, holiday_simple = mae("Recent weekday average")$by_day$`Public holiday`,
                 ramadan = mae(method)$by_day$Ramadan, ramadan_simple = mae("Recent weekday average")$by_day$Ramadan,
                 base_rate = num(rule$base_rate), warnings = num(rule$any_warning$flagged),
                 hit_rate = num(rule$any_warning$hit_rate), catch_rate = num(rule$any_warning$catch_rate)),
    regions = regions,
    analysed_to = as.Date(bt$data$analysed_to),
    by_state = by_state[order(by_state$state), , drop = FALSE]
  )
}
