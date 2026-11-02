# Interactive charts (plotly). Every chart shares one style: transparent
# background so the glass cards show through, the blood-red ramp, generous
# margins, and a wine tooltip that shows exact values on hover.

suppressPackageStartupMessages(library(plotly))

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

axis_style <- function(title = NULL, grid = TRUE, ...) {
  utils::modifyList(list(
    title = if (is.null(title)) list(text = "") else
      list(text = title, standoff = 14, font = list(size = 12.5, color = MUTED)),
    showgrid = grid, gridcolor = GRID, zeroline = FALSE, showline = FALSE,
    tickfont = list(size = 12, color = MUTED), automargin = TRUE,
    fixedrange = TRUE
  ), list(...))
}

glass_layout <- function(p, ytitle = NULL, xtitle = NULL, hovermode = "closest",
                         legend = TRUE, xaxis = list(), yaxis = list(), margin = list(), ...) {
  p |>
    layout(
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      font = list(family = FONT, size = 13, color = INK),
      margin = utils::modifyList(list(l = 12, r = 12, t = 16, b = 12, pad = 6), margin),
      xaxis = utils::modifyList(axis_style(xtitle, grid = FALSE), xaxis),
      yaxis = utils::modifyList(axis_style(ytitle), yaxis),
      showlegend = legend,
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.16,
                    yanchor = "top", font = list(size = 12.5), itemclick = "toggle",
                    bgcolor = "rgba(0,0,0,0)"),
      hovermode = hovermode,
      hoverlabel = list(bgcolor = INK, bordercolor = INK, align = "left",
                        font = list(family = FONT, size = 13, color = "#FFFFFF")),
      bargap = 0.32,
      ...
    ) |>
    config(displayModeBar = FALSE, responsive = TRUE)
}

fmt2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
fmt0 <- function(x) formatC(round(x), format = "d", big.mark = ",")

# Grouped bars of pooled MAE per evaluation period.
chart_pooled_mae <- function(scores_overall, period_labels) {
  d <- scores_overall |>
    mutate(period = factor(period_labels[as.character(evaluation_year)],
                           levels = unique(period_labels)),
           method = factor(method, levels = METHODS),
           tip = sprintf("<b>%s</b><br>%s<br>MAE <b>%s</b> donations<br>RMSE %s",
                         method, period, fmt2(MAE), fmt2(RMSE)))
  p <- plot_ly()
  for (m in METHODS) {
    s <- d[d$method == m, ]
    p <- p |> add_bars(data = s, x = ~period, y = ~MAE, name = m,
                       marker = list(color = METHOD_COL[[m]]),
                       hovertext = ~tip, hoverinfo = "text")
  }
  glass_layout(p, ytitle = "MAE, donations per group-day", barmode = "group",
               yaxis = list(rangemode = "tozero"), margin = list(t = 28))
}

# A 2 x 2 grid of small multiples, one per blood group, with titles.
group_grid <- function(build_one, groups = ABO_GROUPS, ytitle = NULL, legend_from = groups[1]) {
  plots <- lapply(groups, function(g) build_one(g, show_legend = identical(g, legend_from)))
  p <- subplot(plots, nrows = 2, shareX = FALSE, shareY = FALSE,
               margin = c(0.045, 0.045, 0.1, 0.1))
  p <- glass_layout(p, barmode = "group", margin = list(t = 40))
  p <- plotly_build(p)
  order <- unname(SHORT_METHOD)
  ann <- list()
  # Style every subplot axis the same way and title each panel above its plot.
  for (i in seq_along(groups)) {
    suffix <- if (i == 1) "" else i
    xa <- paste0("xaxis", suffix); ya <- paste0("yaxis", suffix)
    p$x$layout[[xa]] <- utils::modifyList(
      p$x$layout[[xa]] %||% list(),
      axis_style(grid = FALSE, categoryorder = "array", categoryarray = order))
    p$x$layout[[ya]] <- utils::modifyList(
      p$x$layout[[ya]] %||% list(),
      axis_style(if (i %% 2 == 1) ytitle else NULL, rangemode = "tozero"))
    xd <- p$x$layout[[xa]]$domain %||% c(0, 1)
    yd <- p$x$layout[[ya]]$domain %||% c(0, 1)
    ann[[i]] <- list(text = paste0("<b>Group ", groups[i], "</b>"), showarrow = FALSE,
                     xref = "paper", yref = "paper", x = xd[1], y = yd[2] + 0.03,
                     xanchor = "left", yanchor = "bottom", font = list(size = 14, color = INK))
  }
  p$x$layout$annotations <- ann
  p
}

chart_group_mae <- function(scores_by_group, methods) {
  d <- scores_by_group |> filter(method %in% methods)
  group_grid(function(g, show_legend) {
    p <- plot_ly()
    for (m in intersect(METHODS, methods)) {
      s <- d[d$blood_type == g & d$method == m, ]
      p <- p |> add_bars(data = s, x = SHORT_METHOD[[m]], y = ~MAE, name = m,
                         legendgroup = m, showlegend = show_legend,
                         marker = list(color = METHOD_COL[[m]]),
                         hovertext = sprintf("<b>%s</b><br>Group %s<br>MAE <b>%s</b><br>RMSE %s<br>%s days",
                                             m, g, fmt2(s$MAE), fmt2(s$RMSE), s$observations),
                         hoverinfo = "text")
    }
    p
  }, ytitle = "MAE")
}

chart_low_days <- function(low_errors) {
  group_grid(function(g, show_legend) {
    s <- low_errors |> filter(blood_type == g) |>
      mutate(method = factor(method, levels = METHODS)) |> arrange(method)
    lo <- s[s$day_category == "Low collection", ]
    ot <- s[s$day_category == "Other days", ]
    plot_ly() |>
      add_bars(data = lo, x = SHORT_METHOD[as.character(lo$method)], y = ~MAE,
               name = "Low-collection days", legendgroup = "low", showlegend = show_legend,
               marker = list(color = "#7D2440"),
               hovertext = sprintf("<b>%s</b><br>Group %s, low-collection days (%s)<br>MAE <b>%s</b><br>Overpredicted %s%% of days",
                                   lo$method, g, lo$days, fmt2(lo$MAE), round(lo$overprediction_percent)),
               hoverinfo = "text") |>
      add_bars(data = ot, x = SHORT_METHOD[as.character(ot$method)], y = ~MAE,
               name = "Other days", legendgroup = "other", showlegend = show_legend,
               marker = list(color = "#F2C2CC"),
               hovertext = sprintf("<b>%s</b><br>Group %s, other days (%s)<br>MAE <b>%s</b>",
                                   ot$method, g, ot$days, fmt2(ot$MAE)),
               hoverinfo = "text")
  }, ytitle = "MAE")
}

# Daily lines: recorded donations (dark) and each method's forecast.
chart_daily <- function(predictions, group, methods, from, to) {
  d <- predictions |> filter(blood_type == group, method %in% methods, date >= from, date <= to)
  if (!nrow(d)) return(NULL)
  actual <- distinct(d, date, donations)
  p <- plot_ly()
  for (m in intersect(METHODS, methods)) {
    s <- d[d$method == m, ]
    p <- p |> add_lines(data = s, x = ~date, y = ~prediction, name = m,
                        line = list(color = METHOD_COL[[m]], width = 1.8, dash = METHOD_DASH[[m]]),
                        hovertemplate = paste0(m, ": <b>%{y:,.0f}</b><extra></extra>"))
  }
  p |>
    add_trace(data = actual, x = ~date, y = ~donations, type = "scatter", mode = "lines+markers",
              name = "Recorded", line = list(color = INK, width = 3),
              marker = list(color = "#FFFFFF", size = 6, line = list(color = INK, width = 2)),
              hovertemplate = "Recorded: <b>%{y:,.0f}</b><extra></extra>") |>
    glass_layout(ytitle = paste("Group", group, "donations per day"), hovermode = "x unified",
                 xaxis = list(tickformat = "%d %b", hoverformat = "%a %d %b %Y"))
}

# Last 60 days plus the next seven, with the forecast window shaded.
chart_live <- function(panel, predictions, next_days, group, data_end) {
  from <- data_end - 59
  actual <- panel |> filter(blood_type == group, date >= from)
  past <- predictions |> filter(blood_type == group, date >= from)
  ahead <- next_days |> filter(blood_type == group)
  p <- plot_ly()
  for (m in METHODS) {
    s <- past[past$method == m, ]
    a <- ahead[ahead$method == m, ]
    p <- p |>
      add_lines(data = s, x = ~date, y = ~prediction, name = m, legendgroup = m,
                line = list(color = METHOD_COL[[m]], width = 1.6, dash = METHOD_DASH[[m]]),
                hovertemplate = paste0(m, ": <b>%{y:,.0f}</b><extra></extra>")) |>
      add_trace(data = a, x = ~date, y = ~prediction, type = "scatter", mode = "lines+markers",
                name = paste(m, "(next 7 days)"), legendgroup = m, showlegend = FALSE,
                line = list(color = METHOD_COL[[m]], width = 2.4, dash = "dot"),
                marker = list(color = METHOD_COL[[m]], size = 6),
                hovertemplate = paste0(m, " forecast: <b>%{y:,.0f}</b><extra></extra>"))
  }
  p |>
    add_trace(data = actual, x = ~date, y = ~donations, type = "scatter", mode = "lines+markers",
              name = "Recorded", line = list(color = INK, width = 3),
              marker = list(color = "#FFFFFF", size = 6, line = list(color = INK, width = 2)),
              hovertemplate = "Recorded: <b>%{y:,.0f}</b><extra></extra>") |>
    glass_layout(ytitle = paste("Group", group, "donations per day"), hovermode = "x unified",
                 xaxis = list(tickformat = "%d %b", hoverformat = "%a %d %b %Y"),
                 shapes = list(list(type = "rect", xref = "x", yref = "paper",
                                    x0 = as.character(data_end + 0.5),
                                    x1 = as.character(max(ahead$date) + 0.5),
                                    y0 = 0, y1 = 1, fillcolor = "rgba(242,179,192,0.28)",
                                    line = list(width = 0), layer = "below")),
                 annotations = list(list(x = as.character(data_end + 4), y = 1, xref = "x",
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
  p <- plot_ly()
  for (g in intersect(c("O", "B", "A", "AB"), unique(d$blood_type))) {
    s <- d[d$blood_type == g, ]
    p <- p |> add_trace(data = s, x = ~month, y = ~donations, type = "scatter", mode = "lines",
                        stackgroup = "one", name = paste("Group", g),
                        line = list(color = GROUP_COL[[g]], width = 0.6),
                        fillcolor = GROUP_COL[[g]],
                        hovertemplate = paste0("Group ", g, ": <b>%{y:,.0f}</b><extra></extra>"))
  }
  glass_layout(p, ytitle = "Donations per month", hovermode = "x unified",
               xaxis = list(hoverformat = "%B %Y"))
}

chart_weekly <- function(weekly) {
  p <- plot_ly()
  for (g in intersect(ABO_GROUPS, unique(weekly$blood_type))) {
    s <- weekly[weekly$blood_type == g, ]
    p <- p |> add_lines(data = s, x = ~week_start, y = ~donations, name = paste("Group", g),
                        line = list(color = GROUP_COL[[g]], width = 1.4),
                        hovertemplate = paste0("Group ", g, ": <b>%{y:,.0f}</b><extra></extra>"))
  }
  glass_layout(p, ytitle = "Donations per week", hovermode = "x unified",
               xaxis = list(hoverformat = "Week of %d %b %Y"))
}

chart_weekday <- function(weekday_means) {
  p <- plot_ly()
  for (g in intersect(ABO_GROUPS, unique(weekday_means$blood_type))) {
    s <- weekday_means[weekday_means$blood_type == g, ]
    p <- p |> add_bars(data = s, x = ~substr(as.character(weekday), 1, 3), y = ~mean_donations,
                       name = paste("Group", g), marker = list(color = GROUP_COL[[g]]),
                       hovertext = sprintf("<b>Group %s, %s</b><br>Mean <b>%s</b> donations a day",
                                           g, s$weekday, fmt0(s$mean_donations)),
                       hoverinfo = "text")
  }
  glass_layout(p, ytitle = "Mean donations a day", barmode = "group",
               xaxis = list(categoryorder = "array", categoryarray = substr(WEEKDAYS, 1, 3)))
}

chart_annual <- function(annual) {
  p <- plot_ly()
  for (g in intersect(ABO_GROUPS, unique(annual$blood_type))) {
    s <- annual[annual$blood_type == g, ]
    p <- p |> add_trace(data = s, x = ~year, y = ~mean_daily, type = "scatter",
                        mode = "lines+markers", name = paste("Group", g),
                        line = list(color = GROUP_COL[[g]], width = 2.2),
                        marker = list(color = GROUP_COL[[g]], size = 6),
                        hovertemplate = paste0("Group ", g, ": <b>%{y:,.1f}</b> a day<extra></extra>"))
  }
  glass_layout(p, ytitle = "Mean donations a day", hovermode = "x unified",
               xaxis = list(dtick = 2))
}

chart_mix <- function(mix, partial_year = NULL) {
  py <- if (is.null(partial_year)) NA_integer_ else partial_year
  mix <- mix |> mutate(label = ifelse(!is.na(py) & year == py,
                                      paste0(year, " so far"), as.character(year)))
  p <- plot_ly()
  for (g in c("O", "B", "A", "AB")) {
    s <- mix[mix$blood_type == g, ]
    p <- p |> add_bars(data = s, x = ~label, y = ~share, name = paste("Group", g),
                       marker = list(color = GROUP_COL[[g]]),
                       hovertext = sprintf("<b>%s, group %s</b><br><b>%.1f%%</b> of recorded donations<br>%s donations",
                                           s$label, g, 100 * s$share, fmt0(s$donations)),
                       hoverinfo = "text")
  }
  glass_layout(p, ytitle = "Share of recorded donations", barmode = "stack",
               yaxis = list(tickformat = ".0%", range = c(0, 1)),
               xaxis = list(tickangle = -45, type = "category"))
}

chart_monthly <- function(monthly, methods) {
  d <- monthly |> filter(method %in% methods) |> mutate(month = as.Date(paste0(month, "-01")))
  p <- plot_ly()
  for (m in intersect(METHODS, methods)) {
    s <- d[d$method == m, ]
    p <- p |> add_trace(data = s, x = ~month, y = ~MAE, type = "scatter", mode = "lines+markers",
                        name = m, line = list(color = METHOD_COL[[m]], width = 2.4),
                        marker = list(color = METHOD_COL[[m]], size = 7),
                        hovertemplate = paste0(m, ": off by <b>%{y:.1f}</b> a day<extra></extra>"))
  }
  glass_layout(p, ytitle = "Average error (donations)", hovermode = "x unified",
               xaxis = list(tickformat = "%b", dtick = "M1", hoverformat = "%B %Y"),
               yaxis = list(rangemode = "tozero"))
}
