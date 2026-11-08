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

# --- Charts for the Every state page -------------------------------------------------
# Same plotly.js figures and palette as R/charts.R. Each colour is one trace, so the
# page's dark-theme colour swap applies to it.

NAT_COL <- c(none = "#F2C2CC", Watch = "#C45A72", "Likely low" = "#7D2440", all = "#E3909F")
NAT_GROUPS <- c("All groups" = "all", "A" = "A", "AB" = "AB", "B" = "B", "O" = "O")

nat_short <- function(s) sub("^W\\.P\\. ", "", s)

# Week totals by state for one blood group ("all" adds the four together) and one week.
nat_week_rows <- function(nat, group, week) {
  w <- nat$weeks[nat$weeks$week == week, ]
  if (group != "all") return(w[w$blood_type == group, ])
  agg <- aggregate(forecast ~ state, w, sum)
  flags <- aggregate(warning ~ state, w, function(x) sum(nzchar(x)))
  out <- merge(agg, flags, by = "state")
  names(out)[names(out) == "warning"] <- "n_warnings"
  out$first <- min(w$first); out$last <- max(w$last)
  out
}

chart_nat_states <- function(nat, group, week) {
  r <- nat_week_rows(nat, group, week)
  r <- r[order(r$forecast), ]
  y <- nat_short(r$state)
  days <- sprintf("%s to %s", format(r$first[1], "%e %b"), format(r$last[1], "%e %b"))
  if (group == "all") {
    hover <- sprintf("<b>%s</b><br>About %s donations, all four groups<br>%s%s", r$state, fmt0(r$forecast), days,
                     ifelse(r$n_warnings > 0, sprintf("<br>%d group%s with a low-week warning", r$n_warnings,
                                                      ifelse(r$n_warnings == 1, "", "s")), ""))
    traces <- list(c(bar_trace(r$forecast, y, "Expected donations", NAT_COL[["all"]], hover), list(orientation = "h")))
    return(figure(traces, xtitle = "Expected donations, all four blood groups", legend = FALSE,
                  yaxis = list(type = "category", categoryorder = "array", categoryarray = arr(y), showgrid = FALSE),
                  xaxis = list(showgrid = TRUE, gridcolor = GRID), margin = list(l = 8), bargap = 0.35))
  }
  lvl <- ifelse(nzchar(r$warning), r$warning, "none")
  traces <- list()
  for (k in c("none", "Watch", "Likely low")) {
    i <- which(lvl == k)
    if (!length(i)) next
    hover <- sprintf("<b>%s, group %s</b><br>About %s donations (80%% range %s to %s)<br>A normal week's low line: %s<br>%s%s",
                     r$state[i], group, fmt0(r$forecast[i]), fmt0(r$low[i]), fmt0(r$high[i]), fmt0(r$low_line[i]), days,
                     if (k == "none") "" else paste0("<br><b>", k, "</b>"))
    traces <- c(traces, list(c(
      bar_trace(r$forecast[i], y[i], c(none = "No warning", Watch = "Watch", "Likely low" = "Likely low")[[k]], NAT_COL[[k]], hover),
      list(orientation = "h",
           error_x = list(type = "data", symmetric = FALSE, array = arr(r$high[i] - r$forecast[i]),
                          arrayminus = arr(r$forecast[i] - r$low[i]), color = MUTED, thickness = 1.4, width = 3)))))
  }
  traces <- c(traces, list(list(
    type = "scatter", mode = "markers", x = arr(r$low_line), y = arr(y), name = "A normal week's low line",
    marker = list(symbol = "line-ns-open", size = 16, color = INK, line = list(width = 2, color = INK)),
    hovertemplate = "Low line: <b>%{x:,.0f}</b><extra></extra>")))
  figure(traces, xtitle = sprintf("Expected group %s donations, with the 80%% range", group),
         barmode = "overlay",
         yaxis = list(type = "category", categoryorder = "array", categoryarray = arr(y), showgrid = FALSE),
         xaxis = list(showgrid = TRUE, gridcolor = GRID, rangemode = "tozero"), margin = list(l = 8), bargap = 0.35)
}

explain_nat <- function(nat, group, week) {
  r <- nat_week_rows(nat, group, week)
  when <- if (week == 1) "next 7 days" else "7 days after that"
  top <- r[which.max(r$forecast), ]
  low <- r[which.min(r$forecast), ]
  if (group == "all") {
    flagged <- r[r$n_warnings > 0, ]
    return(sprintf("Over the %s, %s should collect the most (about %s) and %s the fewest (about %s). %s",
                   when, nat_short(top$state), fmt0(top$forecast), nat_short(low$state), fmt0(low$forecast),
                   if (nrow(flagged)) sprintf("%s ha%s a blood group with a low-week warning: pick a group to see it.",
                                              paste(nat_short(flagged$state), collapse = ", "),
                                              if (nrow(flagged) == 1) "s" else "ve")
                   else "No state has a low-week warning."))
  }
  flagged <- r[nzchar(r$warning), ]
  sprintf("Group %s, %s: %s leads with about %s. %s", group, when, nat_short(top$state), fmt0(top$forecast),
          if (nrow(flagged)) sprintf("%s %s below a normal week's line (the dark tick).",
                                     paste(nat_short(flagged$state), collapse = ", "),
                                     if (nrow(flagged) == 1) "is forecast" else "are forecast")
          else "Every state is above its normal-week line (the dark tick).")
}

# How often the real count fell inside the 80% range, per state, on the year checked.
chart_nat_coverage <- function(nat) {
  b <- nat$by_state[order(nat$by_state$coverage), ]
  y <- nat_short(b$state)
  hover <- sprintf("<b>%s</b><br>%s of days inside the 80%% range<br>Average miss %s (simple method %s)",
                   b$state, sprintf("%.1f%%", 100 * b$coverage), fmt2(b$MAE), fmt2(b$MAE_simple))
  figure(list(
    list(type = "scatter", mode = "lines", x = arr(c(80, 80)), y = arr(c(y[1], y[length(y)])), name = "The 80% aim",
         line = list(color = MUTED, width = 1.5, dash = "dash"), hoverinfo = "skip"),
    list(type = "scatter", mode = "markers", x = arr(100 * b$coverage), y = arr(y), name = "Days inside the range",
         marker = list(color = "#7D2440", size = 11, line = list(width = 0)), hovertext = arr(hover), hoverinfo = "text")),
    xtitle = "Days inside the 80% range (%)", legend = FALSE,
    yaxis = list(type = "category", categoryorder = "array", categoryarray = arr(y), showgrid = FALSE),
    xaxis = list(range = arr(c(60, 100)), showgrid = TRUE, gridcolor = GRID, ticksuffix = "%"), margin = list(l = 8))
}
