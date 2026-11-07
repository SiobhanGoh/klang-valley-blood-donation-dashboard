# Forecast ranges, a quiet-day watch, and a running track record of published forecasts.
#
# 1. Ranges. Each method's backtest errors from one complete year give an 80% range for its
#    forecasts (split-conformal: the floor((n+1)*0.1)-th and ceil((n+1)*0.9)-th smallest
#    errors, added to the forecast). How well that works is checked honestly: ranges built
#    from year Y-1 are scored on year Y, which they never saw.
# 2. Quiet-day watch. A day is marked "could be quiet" when the low end of its range is at or
#    below the group's low-collection cutoff (the 25th percentile of earlier years). Scored the
#    same way, on a year the ranges never saw. It is a prompt to look closer, not a clinical
#    shortage alert.
# 3. Track record. Every forecast the site publishes is appended, never edited, to a log kept
#    on the repository's forecast-log branch; once the days have real counts, they are scored.

RANGE_LEVEL <- 0.8
WATCH_METHOD <- "Recent weekday average"

# Offsets to add to a forecast for an 80% range, from a vector of past errors (actual - forecast).
conformal_offsets <- function(errors, level = RANGE_LEVEL) {
  e <- sort(errors[is.finite(errors)])
  n <- length(e)
  if (n < 10) return(c(lo = NA_real_, hi = NA_real_))
  a <- (1 - level) / 2
  # Rounded first: (1 - 0.8) / 2 is not exactly 0.1 in floating point, which would shift a rank.
  lo_rank <- max(1L, floor(round((n + 1) * a, 8)))
  hi_rank <- min(n, ceiling(round((n + 1) * (1 - a), 8)))
  c(lo = e[lo_rank], hi = e[hi_rank])
}

# One pair of offsets per blood group and method, from one evaluation year's backtest errors.
range_offsets <- function(predictions, calibration_year) {
  p <- predictions[predictions$evaluation_year == calibration_year, ]
  keys <- unique(p[c("blood_type", "method")])
  rows <- lapply(seq_len(nrow(keys)), function(i) {
    s <- p[p$blood_type == keys$blood_type[i] & p$method == keys$method[i], ]
    o <- conformal_offsets(s$error)
    data.frame(blood_type = keys$blood_type[i], method = keys$method[i],
               lo_offset = unname(o["lo"]), hi_offset = unname(o["hi"]),
               calibration_year = as.integer(calibration_year), calibration_days = nrow(s),
               stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

add_ranges <- function(forecasts, offsets) {
  out <- merge(as.data.frame(forecasts), offsets[c("blood_type", "method", "lo_offset", "hi_offset")],
               by = c("blood_type", "method"), all.x = TRUE, sort = FALSE)
  out$low <- pmax(0, out$prediction + out$lo_offset)
  out$high <- pmax(out$low, out$prediction + out$hi_offset)
  out$lo_offset <- NULL
  out$hi_offset <- NULL
  out[order(out$date, out$blood_type, factor(out$method, levels = METHODS)), , drop = FALSE]
}

# Ranges from year Y-1 scored on year Y, for every consecutive pair of evaluation years.
range_coverage <- function(predictions, periods) {
  years <- sort(periods$evaluation_year)
  if (length(years) < 2) return(data.frame())
  rows <- lapply(years[-1], function(y) {
    off <- range_offsets(predictions, y - 1L)
    p <- add_ranges(predictions[predictions$evaluation_year == y, ], off)
    p$inside <- p$donations >= p$low & p$donations <= p$high
    s <- group_stat(p, "method", "inside", mean, "coverage")
    w <- group_stat(p, "method", "high", sum, "high_sum")
    l <- group_stat(p, "method", "low", sum, "low_sum")
    n <- group_stat(p, "method", "inside", length, "days")
    out <- Reduce(function(a, b) merge(a, b, by = "method"), list(s, w, l, n))
    out$mean_width <- (out$high_sum - out$low_sum) / out$days
    out$calibrated_on <- y - 1L
    out$tested_on <- y
    out$partial <- periods$partial[periods$evaluation_year == y]
    out[c("calibrated_on", "tested_on", "partial", "method", "days", "coverage", "mean_width")]
  })
  do.call(rbind, rows)
}

watch_flags <- function(forecasts, cutoffs) {
  f <- merge(as.data.frame(forecasts), as.data.frame(cutoffs)[c("blood_type", "low_cutoff")], by = "blood_type")
  f$watch <- ifelse(f$high <= f$low_cutoff, "Likely quiet",
                    ifelse(f$low <= f$low_cutoff, "Could be quiet", ""))
  f
}

# How the watch rule would have done on each year the ranges never saw: of the days it flagged,
# how many were really low (precision), and of the really low days, how many it flagged (recall).
watch_backtest <- function(predictions, model_data, periods, method = WATCH_METHOD) {
  years <- sort(periods$evaluation_year)
  if (length(years) < 2) return(data.frame())
  rows <- lapply(years[-1], function(y) {
    off <- range_offsets(predictions, y - 1L)
    p <- add_ranges(predictions[predictions$evaluation_year == y & predictions$method == method, ], off)
    p <- watch_flags(p, low_collection_cutoffs(model_data, y))
    flagged <- p$watch != ""
    low <- p$donations <= p$low_cutoff
    data.frame(calibrated_on = y - 1L, tested_on = y, partial = periods$partial[periods$evaluation_year == y],
               method = method, group_days = nrow(p), low_days = sum(low), flagged_days = sum(flagged),
               flagged_and_low = sum(flagged & low),
               precision = if (any(flagged)) mean(low[flagged]) else NA_real_,
               recall = if (any(low)) mean(flagged[low]) else NA_real_)
  })
  do.call(rbind, rows)
}

# --- Track record of published forecasts ------------------------------------------------

LOG_COLUMNS <- c("issued", "date", "blood_type", "method", "prediction", "low", "high")

read_forecast_log <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  log <- utils::read.csv(path, stringsAsFactors = FALSE)
  if (!nrow(log)) return(NULL)
  stopifnot(all(LOG_COLUMNS %in% names(log)))
  log$issued <- as.Date(log$issued)
  log$date <- as.Date(log$date)
  log[LOG_COLUMNS]
}

# Appends today's forecasts. A forecast already logged for the same issue date, day, group and
# method is never replaced, so the log only grows and past entries stay as first published.
update_forecast_log <- function(log, next_days, issued) {
  new <- as.data.frame(next_days)
  new$issued <- as.Date(issued)
  new <- new[LOG_COLUMNS]
  key <- function(d) paste(d$issued, d$date, d$blood_type, d$method)
  out <- if (is.null(log)) new else rbind(log, new[!key(new) %in% key(log), , drop = FALSE])
  rownames(out) <- NULL
  out
}

# Scores every logged forecast whose day now has a recorded count.
score_forecast_log <- function(log, panel) {
  if (is.null(log) || !nrow(log)) return(NULL)
  actual <- as.data.frame(panel)[c("date", "blood_type", "donations")]
  s <- merge(log, actual, by = c("date", "blood_type"))
  if (!nrow(s)) return(list(scored = 0L, issued_from = min(log$issued), logged = nrow(log), by_method = data.frame()))
  s$abs_error <- abs(s$donations - s$prediction)
  s$inside <- s$donations >= s$low & s$donations <= s$high
  by <- Reduce(function(a, b) merge(a, b, by = "method"),
               list(group_stat(s, "method", "abs_error", mean, "MAE"),
                    group_stat(s, "method", "inside", mean, "coverage"),
                    group_stat(s, "method", "inside", length, "scored")))
  list(scored = nrow(s), logged = nrow(log), issued_from = min(log$issued), issued_to = max(log$issued),
       days_from = min(s$date), days_to = max(s$date),
       by_method = by[order(factor(by$method, levels = METHODS)), , drop = FALSE])
}
