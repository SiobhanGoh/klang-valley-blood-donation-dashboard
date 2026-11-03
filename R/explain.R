# Plain-words readings of each chart, rewritten for whatever the reader has
# selected. Each function returns one or two short sentences whose wording
# depends on how big the result is, so a small gap never reads like a big one.
# Base R only, so it also runs in the browser.

pct_gap <- function(better, worse) if (is.na(worse) || worse <= 0) NA_real_ else 100 * (worse - better) / worse
fmt_pct <- function(x) paste0(formatC(x, format = "f", digits = 0), "%")
fmt_n <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
lower_first <- function(s) paste0(tolower(substr(s, 1, 1)), substring(s, 2))

# Compare: who wins among the selected methods, by how much, and how often by group.
explain_compare <- function(overall, by_group, methods, period) {
  o <- overall[overall$method %in% methods, ]
  if (nrow(o) < 2) return("Pick at least two methods to compare them.")
  o <- o[order(o$MAE), ]
  best <- o$method[1]; gap <- pct_gap(o$MAE[1], o$MAE[2])
  lead <- if (gap >= 15) sprintf("the %s is clearly the most accurate, with %s less error than the next best (%s)",
                                 lower_first(best), fmt_pct(gap), lower_first(o$method[2]))
          else if (gap >= 5) sprintf("the %s is a little more accurate than the %s, by %s",
                                     lower_first(best), lower_first(o$method[2]), fmt_pct(gap))
          else sprintf("the %s and the %s are almost level, %s apart",
                       lower_first(best), lower_first(o$method[2]), fmt_pct(gap))
  g <- by_group[by_group$method %in% methods, ]
  winners <- vapply(split(g, g$blood_type), function(x) x$method[which.min(x$MAE)], "")
  same <- sum(winners == best)
  groups <- if (same == length(winners)) "It is the best for every blood group."
            else sprintf("It is the best for %d of %d blood groups; for %s the %s does better.",
                         same, length(winners), paste(names(winners)[winners != best], collapse = " and "),
                         lower_first(unique(winners[winners != best])[1]))
  paste0("In ", period, ", ", lead, ". ", groups)
}

# Low days: how much worse every method does when donations drop.
explain_low <- function(low, period) {
  lowd <- low[low$day_category == "Low collection", ]
  oth <- low[low$day_category == "Other days", ]
  w <- merge(lowd[c("blood_type", "method", "MAE")], oth[c("blood_type", "method", "MAE")],
             by = c("blood_type", "method"), suffixes = c("_low", "_other"))
  ratio <- mean(w$MAE_low / w$MAE_other, na.rm = TRUE)
  over <- sum(lowd$days * lowd$overprediction_percent, na.rm = TRUE) / sum(lowd$days, na.rm = TRUE)
  if (!is.finite(ratio) || !is.finite(over)) return(sprintf("No low-collection days to compare in %s.", period))
  tone <- if (ratio >= 1.5) sprintf("the forecasts struggle badly on quiet days: their error is %s times what it is on other days",
                                    fmt_n(ratio))
          else if (ratio >= 1.15) sprintf("the forecasts do noticeably worse on quiet days, with %s times the usual error",
                                          fmt_n(ratio))
          else "the forecasts cope about as well on quiet days as on other days"
  aim <- if (over >= 90) sprintf("On almost every quiet day (%s) they guessed too high.", fmt_pct(over))
         else if (over >= 60) sprintf("Most of the time (%s of quiet days) they guessed too high.", fmt_pct(over))
         else sprintf("They guessed too high on %s of quiet days, so misses go both ways.", fmt_pct(over))
  paste0("In ", period, ", ", tone, ". ", aim)
}

# Live: the latest recorded day against the best method's forecast for it.
explain_live <- function(predictions, group, data_end) {
  p <- predictions[predictions$blood_type == group & predictions$date == data_end &
                     predictions$method == "Recent weekday average", ]
  if (!nrow(p)) return(sprintf("Group %s: the latest day has no forecast to compare with yet.", group))
  ch <- 100 * (p$donations - p$prediction) / p$prediction
  day <- format(data_end, "%A")
  if (abs(ch) <= 10) sprintf("Group %s on %s: %s donations against %s expected. Right on track.",
                             group, day, fmt_n(p$donations, 0), fmt_n(p$prediction, 0))
  else if (ch > 0) sprintf("Group %s on %s: %s donations, %s more than the %s expected. A busier day than usual.",
                           group, day, fmt_n(p$donations, 0), fmt_pct(ch), fmt_n(p$prediction, 0))
  else sprintf("Group %s on %s: %s donations, %s fewer than the %s expected. A quieter day than usual.",
               group, day, fmt_n(p$donations, 0), fmt_pct(-ch), fmt_n(p$prediction, 0))
}

# Compare, day by day: which forecast tracked closest over the chosen dates.
explain_daily <- function(predictions, group, methods, from, to) {
  d <- predictions[predictions$blood_type == group & predictions$method %in% methods &
                     predictions$date >= from & predictions$date <= to, ]
  if (!nrow(d)) return("No evaluation days in the selected range.")
  mae <- sort(tapply(d$absolute_error, d$method, mean))
  r <- d[d$method == names(mae)[1], ]
  worst <- r[which.max(r$absolute_error), ]
  sprintf(paste("Over these %d days the %s tracked group %s most closely, off by %s donations a day on average.",
                "Its biggest miss was %s, when it expected %s and %s were recorded."),
          length(unique(d$date)), lower_first(names(mae)[1]), group, fmt_n(mae[[1]]),
          gsub("  ", " ", format(worst$date, "%a %e %b")), fmt_n(worst$prediction, 0), fmt_n(worst$donations, 0))
}

# Patterns: the weekly rhythm for the selected groups and years.
explain_weekday <- function(m) {
  tot <- tapply(m$mean_donations, m$weekday, sum)
  hi <- names(which.max(tot)); lo <- names(which.min(tot)); r <- max(tot) / min(tot)
  how <- if (r >= 2) sprintf("more than twice as busy as %s (%s times)", lo, fmt_n(r))
         else if (r >= 1.3) sprintf("noticeably busier than %s (%s times)", lo, fmt_n(r))
         else sprintf("only a little busier than %s", lo)
  sprintf("%s is the busiest day, %s.", hi, how)
}

explain_annual <- function(a) {
  tot <- tapply(a$mean_daily, a$year, sum)
  if (length(tot) < 2) return("Pick at least two years to see a trend.")
  y <- as.integer(names(tot)); ch <- 100 * (tot[[length(tot)]] - tot[[1]]) / tot[[1]]
  peak <- y[which.max(tot)]
  trend <- if (ch >= 10) sprintf("Donations a day grew by %s from %d to %d.", fmt_pct(ch), y[1], y[length(y)])
           else if (ch <= -10) sprintf("Donations a day fell by %s from %d to %d.", fmt_pct(-ch), y[1], y[length(y)])
           else sprintf("Donations a day stayed fairly steady from %d to %d (%s%s).", y[1], y[length(y)],
                        if (ch >= 0) "+" else "−", fmt_pct(abs(ch)))
  paste(trend, sprintf("The busiest year was %d.", peak))
}

# About: the month that was hardest to forecast.
explain_monthly <- function(monthly, methods, period) {
  d <- monthly[monthly$method %in% methods, ]
  if (!nrow(d)) return("Pick at least one method.")
  m <- tapply(d$MAE, d$month, mean)
  hard <- names(which.max(m)); easy <- names(which.min(m)); spread <- max(m) / min(m)
  name <- function(k) format(as.Date(paste0(k, "-01")), "%B")
  lead <- if (spread >= 1.5) sprintf("%s was by far the hardest month to forecast in %s", name(hard), period)
          else sprintf("%s was the hardest month to forecast in %s, though not by much", name(hard), period)
  sprintf("%s, and %s the easiest.", lead, name(easy))
}

# Blood 101: who collects the most in a region.
explain_region <- function(st, region) {
  st <- st[order(-st$last30), ]
  if (nrow(st) == 1) return(sprintf("%s recorded %s donations in the last 30 days.", st$state, fmt_n(st$last30, 0)))
  share <- 100 * st$last30[1] / sum(st$last30)
  lead <- if (share >= 60) "collects most of the region's blood" else if (share >= 40) "collects the most" else "is narrowly ahead"
  sprintf("In the %s region, %s %s: %s of donations in the last 30 days.", region, st$state[1], lead, fmt_pct(share))
}

# The Overview chart: does the same method win every period?
explain_overview <- function(overall, labels) {
  w <- vapply(split(overall, overall$evaluation_year), function(x) x$method[which.min(x$MAE)], "")
  if (length(unique(w)) == 1)
    sprintf("The %s had the shortest bars in all %d test periods, so its lead is consistent, not a one-off.",
            lower_first(w[[1]]), length(w))
  else sprintf("The winner changes between periods: the %s won in %s.", lower_first(w[[length(w)]]),
               labels[[names(w)[length(w)]]])
}
