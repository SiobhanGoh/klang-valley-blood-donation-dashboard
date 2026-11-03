# Interactive charts. Each builder returns a plain plotly.js figure
# (list(data, layout, config)) that the browser draws with the small
# plotly.js "basic" bundle in www/. Building figures directly, instead of with
# the plotly R package, keeps the in-browser R download small: no ggplot2,
# data.table, httr or stringi. Every chart shares one style: transparent
# background, the blood-red ramp, generous margins and the glass tooltip.

INK <- "#4A1426"
MUTED <- "#8A6470"
GRID <- "#F3E1E5"
FONT <- "-apple-system, 'SF Pro Text', Inter, 'Segoe UI', system-ui, sans-serif"

# Methods run light to dark in the order they improve; the strongest red is
# the best method. Blood groups use the same ramp.
METHOD_COL <- c("Seven-day baseline" = "#F2C2CC",
                "Weekday average" = "#E3909F",
                "Linear regression" = "#C45A72",
                "Recent weekday average" = "#7D2440")
GROUP_COL <- c("A" = "#F2B3C0", "AB" = "#E07D93", "B" = "#B8445F", "O" = "#6E1D37")
# Line styles keep forecasts apart from each other and from the recorded line.
METHOD_DASH <- c("Seven-day baseline" = "dot", "Weekday average" = "dash",
                 "Linear regression" = "longdash", "Recent weekday average" = "solid")
SHORT_METHOD <- c("Seven-day baseline" = "7-day lag",
                  "Weekday average" = "Weekday mean",
                  "Linear regression" = "Regression",
                  "Recent weekday average" = "Recent mean")

fmt2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
fmt0 <- function(x) formatC(round(x), format = "d", big.mark = ",")

# Arrays must stay arrays in JSON even when they hold one value.
arr <- function(x) {
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
  I(unname(as.vector(x)))
}

axis_style <- function(title = NULL, grid = TRUE, ...) {
  utils::modifyList(list(
    title = if (is.null(title)) list(text = "") else
      list(text = title, standoff = 14, font = list(size = 12, color = MUTED)),
    showgrid = grid, gridcolor = GRID, zeroline = FALSE, showline = FALSE,
    tickfont = list(size = 11.5, color = MUTED), automargin = TRUE,
    fixedrange = TRUE
  ), list(...))
}

figure <- function(traces, ytitle = NULL, xtitle = NULL, hovermode = "closest", legend = TRUE,
                   xaxis = list(), yaxis = list(), margin = list(), ...) {
  list(
    data = unname(traces),
    layout = c(list(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = FONT, size = 12.5, color = INK),
      margin = utils::modifyList(list(l = 12, r = 12, t = 16, b = 12, pad = 6), margin),
      xaxis = utils::modifyList(axis_style(xtitle, grid = FALSE), xaxis),
      yaxis = utils::modifyList(axis_style(ytitle), yaxis),
      showlegend = legend,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.16,
                    yanchor = "top", font = list(size = 12), itemclick = "toggle",
                    bgcolor = "rgba(0,0,0,0)"),
      hovermode = hovermode,
      bargap = 0.32), list(...)),
    config = list(displayModeBar = FALSE, responsive = TRUE)
  )
}

bar_trace <- function(x, y, name, colour, hovertext, ...) {
  c(list(type = "bar", x = arr(x), y = arr(y), name = name,
         marker = list(color = colour), hovertext = arr(hovertext), hoverinfo = "text"), list(...))
}

line_trace <- function(x, y, name, line, hovertemplate, mode = "lines", ...) {
  c(list(type = "scatter", mode = mode, x = arr(x), y = arr(y), name = name,
         line = line, hovertemplate = hovertemplate), list(...))
}

# Serialise a figure for the chart output binding.
chart_json <- function(fig) {
  jsonlite::toJSON(fig, auto_unbox = TRUE, null = "null", na = "null", digits = NA)
}

# Grouped bars of the average error per test period.
chart_pooled_mae <- function(scores_overall, period_labels) {
  d <- scores_overall
  d$period <- period_labels[as.character(d$evaluation_year)]
  traces <- lapply(METHODS, function(m) {
    s <- d[d$method == m, ]
    bar_trace(s$period, s$MAE, m, METHOD_COL[[m]],
              sprintf("<b>%s</b><br>%s<br>Average error <b>%s</b> donations a day<br>Big-miss error (RMSE) %s",
                      m, s$period, fmt2(s$MAE), fmt2(s$RMSE)))
  })
  figure(traces, ytitle = "Average error (donations a day)", barmode = "group",
         xaxis = list(type = "category", categoryorder = "array", categoryarray = arr(unique(period_labels))),
         yaxis = list(rangemode = "tozero"), margin = list(t = 28))
}

# A 2 x 2 grid of small multiples, one per blood group, with titles.
# build_one(g, show_legend) returns that group's traces.
group_grid <- function(build_one, groups = ABO_GROUPS, ytitle = NULL, legend_from = groups[1]) {
  gap_x <- 0.09; gap_y <- 0.2
  w <- (1 - gap_x) / 2; h <- (1 - gap_y) / 2
  order <- arr(unname(SHORT_METHOD))
  traces <- list(); axes <- list(); ann <- list()
  for (i in seq_along(groups)) {
    col <- (i - 1) %% 2; row <- (i - 1) %/% 2
    xd <- c(col * (w + gap_x), col * (w + gap_x) + w)
    yd <- c(1 - row * (h + gap_y) - h, 1 - row * (h + gap_y))
    sfx <- if (i == 1) "" else i
    xref <- paste0("x", sfx); yref <- paste0("y", sfx)
    tr <- build_one(groups[i], show_legend = identical(groups[i], legend_from))
    tr <- lapply(tr, function(t) { t$xaxis <- xref; t$yaxis <- yref; t })
    traces <- c(traces, tr)
    axes[[paste0("xaxis", sfx)]] <- axis_style(grid = FALSE, domain = arr(xd), anchor = yref,
                                               type = "category", categoryorder = "array",
                                               categoryarray = order)
    axes[[paste0("yaxis", sfx)]] <- axis_style(if (i %% 2 == 1) ytitle else NULL, domain = arr(yd),
                                               anchor = xref, rangemode = "tozero")
    ann[[i]] <- list(text = paste0("<b>Group ", groups[i], "</b>"), showarrow = FALSE,
                     xref = "paper", yref = "paper", x = xd[1], y = yd[2] + 0.03,
                     xanchor = "left", yanchor = "bottom", font = list(size = 13.5, color = INK))
  }
  fig <- figure(traces, barmode = "group", margin = list(t = 40), annotations = ann)
  fig$layout <- utils::modifyList(fig$layout, axes)
  fig
}

chart_group_mae <- function(scores_by_group, methods) {
  d <- scores_by_group[scores_by_group$method %in% methods, ]
  group_grid(function(g, show_legend) {
    lapply(intersect(METHODS, methods), function(m) {
      s <- d[d$blood_type == g & d$method == m, ]
      bar_trace(SHORT_METHOD[[m]], s$MAE, m, METHOD_COL[[m]],
                sprintf("<b>%s</b><br>Group %s<br>Average error <b>%s</b> a day<br>Big-miss error (RMSE) %s<br>%s days scored",
                        m, g, fmt2(s$MAE), fmt2(s$RMSE), s$observations),
                legendgroup = m, showlegend = show_legend)
    })
  }, ytitle = "Average error")
}

chart_low_days <- function(low_errors) {
  group_grid(function(g, show_legend) {
    s <- low_errors[low_errors$blood_type == g, ]
    s <- s[order(match(s$method, METHODS)), ]
    lo <- s[s$day_category == "Low collection", ]
    ot <- s[s$day_category == "Other days", ]
    list(
      bar_trace(SHORT_METHOD[lo$method], lo$MAE, "Low-collection days", "#7D2440",
                sprintf("<b>%s</b><br>Group %s, %s low-collection days<br>Average error <b>%s</b> a day<br>Forecast too high on %s%% of them",
                        lo$method, g, lo$days, fmt2(lo$MAE), round(lo$overprediction_percent)),
                legendgroup = "low", showlegend = show_legend),
      bar_trace(SHORT_METHOD[ot$method], ot$MAE, "Other days", "#F2C2CC",
                sprintf("<b>%s</b><br>Group %s, %s other days<br>Average error <b>%s</b> a day",
                        ot$method, g, ot$days, fmt2(ot$MAE)),
                legendgroup = "other", showlegend = show_legend))
  }, ytitle = "Average error")
}

recorded_trace <- function(x, y) {
  line_trace(x, y, "Recorded", list(color = INK, width = 3),
             "Recorded: <b>%{y:,.0f}</b><extra></extra>", mode = "lines+markers",
             marker = list(color = "#FFFFFF", size = 6, line = list(color = INK, width = 2)))
}

# Daily lines: recorded donations (dark) and each method's forecast.
chart_daily <- function(predictions, group, methods, from, to) {
  d <- predictions[predictions$blood_type == group & predictions$method %in% methods &
                     predictions$date >= from & predictions$date <= to, ]
  if (!nrow(d)) return(NULL)
  actual <- unique(d[, c("date", "donations")])
  actual <- actual[order(actual$date), ]
  traces <- lapply(intersect(METHODS, methods), function(m) {
    s <- d[d$method == m, ]; s <- s[order(s$date), ]
    line_trace(s$date, s$prediction, m,
               list(color = METHOD_COL[[m]], width = 1.8, dash = METHOD_DASH[[m]]),
               paste0(m, ": <b>%{y:,.0f}</b><extra></extra>"))
  })
  figure(c(traces, list(recorded_trace(actual$date, actual$donations))),
         ytitle = paste("Group", group, "donations per day"), hovermode = "x unified",
         xaxis = list(type = "date", tickformat = "%d %b", hoverformat = "%a %d %b %Y"))
}

# Last 60 days plus the next seven, with the forecast window shaded.
chart_live <- function(panel, predictions, next_days, group, data_end) {
  from <- data_end - 59
  actual <- panel[panel$blood_type == group & panel$date >= from, ]
  actual <- actual[order(actual$date), ]
  past <- predictions[predictions$blood_type == group & predictions$date >= from, ]
  ahead <- next_days[next_days$blood_type == group, ]
  traces <- list()
  for (m in METHODS) {
    s <- past[past$method == m, ]; s <- s[order(s$date), ]
    a <- ahead[ahead$method == m, ]; a <- a[order(a$date), ]
    traces <- c(traces, list(
      line_trace(s$date, s$prediction, m,
                 list(color = METHOD_COL[[m]], width = 1.6, dash = METHOD_DASH[[m]]),
                 paste0(m, ": <b>%{y:,.0f}</b><extra></extra>"), legendgroup = m),
      line_trace(a$date, a$prediction, paste(m, "(next 7 days)"),
                 list(color = METHOD_COL[[m]], width = 2.4, dash = "dot"),
                 paste0(m, " forecast: <b>%{y:,.0f}</b><extra></extra>"), mode = "lines+markers",
                 marker = list(color = METHOD_COL[[m]], size = 6), legendgroup = m,
                 showlegend = FALSE)))
  }
  traces <- c(traces, list(recorded_trace(actual$date, actual$donations)))
  figure(traces, ytitle = paste("Group", group, "donations per day"), hovermode = "x unified",
         xaxis = list(type = "date", tickformat = "%d %b", hoverformat = "%a %d %b %Y"),
         shapes = list(list(type = "rect", xref = "x", yref = "paper",
                            x0 = format(data_end + 0.5), x1 = format(max(ahead$date) + 0.5),
                            y0 = 0, y1 = 1, fillcolor = "rgba(242,179,192,0.28)",
                            line = list(width = 0), layer = "below")),
         annotations = list(list(x = format(data_end + 4), y = 1, xref = "x",
                                 yref = "paper", yanchor = "bottom", showarrow = FALSE,
                                 text = "Next 7 days", font = list(size = 12, color = MUTED))))
}

# Monthly totals stacked by blood group. An unfinished final month is left out
# so the chart does not end in a false drop.
chart_month_totals <- function(panel, data_end) {
  d <- panel |>
    mutate(month = floor_date(date, "month")) |>
    group_by(month, blood_type) |>
    summarise(donations = sum(donations), .groups = "drop")
  if (data_end < ceiling_date(data_end, "month") - 1) d <- d[d$month < floor_date(data_end, "month"), ]
  traces <- lapply(intersect(c("O", "B", "A", "AB"), unique(d$blood_type)), function(g) {
    s <- d[d$blood_type == g, ]
    line_trace(s$month, s$donations, paste("Group", g), list(color = GROUP_COL[[g]], width = 0.6),
               paste0("Group ", g, ": <b>%{y:,.0f}</b><extra></extra>"),
               stackgroup = "one", fillcolor = GROUP_COL[[g]])
  })
  figure(traces, ytitle = "Donations per month", hovermode = "x unified",
         xaxis = list(type = "date", hoverformat = "%B %Y"))
}

chart_weekday <- function(weekday_means) {
  traces <- lapply(intersect(ABO_GROUPS, unique(weekday_means$blood_type)), function(g) {
    s <- weekday_means[weekday_means$blood_type == g, ]
    bar_trace(substr(as.character(s$weekday), 1, 3), s$mean_donations, paste("Group", g),
              GROUP_COL[[g]],
              sprintf("<b>Group %s, %s</b><br>Mean <b>%s</b> donations a day",
                      g, s$weekday, fmt0(s$mean_donations)))
  })
  figure(traces, ytitle = "Mean donations a day", barmode = "group",
         xaxis = list(type = "category", categoryorder = "array",
                      categoryarray = arr(substr(WEEKDAYS, 1, 3))))
}

chart_annual <- function(annual) {
  traces <- lapply(intersect(ABO_GROUPS, unique(annual$blood_type)), function(g) {
    s <- annual[annual$blood_type == g, ]
    line_trace(s$year, s$mean_daily, paste("Group", g), list(color = GROUP_COL[[g]], width = 2.2),
               paste0("Group ", g, ": <b>%{y:,.1f}</b> a day<extra></extra>"),
               mode = "lines+markers", marker = list(color = GROUP_COL[[g]], size = 6))
  })
  figure(traces, ytitle = "Mean donations a day", hovermode = "x unified", xaxis = list(dtick = 2))
}

chart_mix <- function(mix, partial_year = NULL) {
  py <- if (is.null(partial_year)) NA_integer_ else partial_year
  mix$label <- ifelse(!is.na(py) & mix$year == py, paste0(mix$year, " so far"), as.character(mix$year))
  traces <- lapply(c("O", "B", "A", "AB"), function(g) {
    s <- mix[mix$blood_type == g, ]
    bar_trace(s$label, s$share, paste("Group", g), GROUP_COL[[g]],
              sprintf("<b>%s, group %s</b><br><b>%.1f%%</b> of recorded donations<br>%s donations",
                      s$label, g, 100 * s$share, fmt0(s$donations)))
  })
  figure(traces, ytitle = "Share of recorded donations", barmode = "stack",
         yaxis = list(tickformat = ".0%", range = arr(c(0, 1))),
         xaxis = list(tickangle = -45, type = "category"))
}

chart_monthly <- function(monthly, methods) {
  d <- monthly[monthly$method %in% methods, ]
  d$month <- as.Date(paste0(d$month, "-01"))
  traces <- lapply(intersect(METHODS, methods), function(m) {
    s <- d[d$method == m, ]; s <- s[order(s$month), ]
    line_trace(s$month, s$MAE, m, list(color = METHOD_COL[[m]], width = 2.4),
               paste0(m, ": off by <b>%{y:.1f}</b> a day<extra></extra>"),
               mode = "lines+markers", marker = list(color = METHOD_COL[[m]], size = 7))
  })
  figure(traces, ytitle = "Average error (donations)", hovermode = "x unified",
         xaxis = list(type = "date", tickformat = "%b", dtick = "M1", hoverformat = "%B %Y"),
         yaxis = list(rangemode = "tozero"))
}
