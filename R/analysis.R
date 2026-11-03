# Analysis functions for the Klang Valley blood donation dashboard.
#
# Every function here reproduces a step of the final FIT5145 Assignment 3
# R Markdown analysis (BloodReady_AI_Demonstration.Rmd, 15 October 2026).
#
# Two configurations use the same code:
#   * report: the frozen snapshot, analysed to 31 Dec 2025 with evaluation
#     years 2023-2025. The tests check this against the outputs saved by the
#     original Rmd (reference/), so the methods are proven to match the report.
#   * live: the latest official CSV, analysed to its newest complete day. The
#     evaluation covers the last three complete years plus the current year
#     to date, so every page reflects the newest data.

# Constants and the light helpers the app shares live in R/common.R.
.kvbd_dir <- local({
  d <- "R"
  for (i in rev(seq_len(sys.nframe()))) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) { d <- dirname(f); break }
  }
  d
})
source(file.path(.kvbd_dir, "common.R"), local = environment())

# readr and janitor are only needed when reading the raw CSV, so they are
# called with `::`.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# --- Data loading and audit --------------------------------------------------

read_donations_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE,
                  col_types = readr::cols(date = readr::col_date(),
                                          state = readr::col_character(),
                                          blood_type = readr::col_character(),
                                          donations = readr::col_double()))
}

# Decompress the snapshot, confirm it is byte-identical to the analysed CSV,
# and read it. Refuses to continue on a different data version.
load_snapshot <- function(path = SNAPSHOT_PATH) {
  if (!file.exists(path)) stop("Data snapshot not found: ", path)
  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)
  con_in <- gzfile(path, "rb")
  bytes <- readBin(con_in, "raw", n = 64e6)
  close(con_in)
  writeBin(bytes, tmp)
  checksum <- unname(tools::md5sum(tmp))
  if (!identical(checksum, SNAPSHOT_MD5)) {
    stop("Snapshot checksum ", checksum, " does not match ", SNAPSHOT_MD5)
  }
  raw <- read_donations_csv(tmp)
  attr(raw, "checksum") <- checksum
  attr(raw, "source") <- "Frozen snapshot used in the FIT5145 report"
  raw
}

# Download the latest official CSV. A cached copy younger than `max_age_hours`
# is reused; returns NULL if the download fails so callers can fall back.
load_live <- function(cache_dir = file.path(tempdir(), "kvbd-cache"),
                      max_age_hours = 6, url = LIVE_CSV_URL, timeout = 120) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  dest <- file.path(cache_dir, "blood_donations_state.csv")
  fresh <- file.exists(dest) &&
    difftime(Sys.time(), file.mtime(dest), units = "hours") < max_age_hours
  if (!fresh) {
    old_timeout <- getOption("timeout")
    on.exit(options(timeout = old_timeout), add = TRUE)
    options(timeout = max(timeout, old_timeout))
    tmp <- paste0(dest, ".part")
    ok <- tryCatch({
      utils::download.file(url, tmp, mode = "wb", quiet = TRUE)
      file.info(tmp)$size > 1e6
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(ok)) { unlink(tmp); if (!file.exists(dest)) return(NULL) }
    else file.rename(tmp, dest)
  }
  raw <- tryCatch(read_donations_csv(dest), error = function(e) NULL)
  if (is.null(raw) || !all(c("date", "state", "blood_type", "donations") %in% names(raw))) {
    return(NULL)
  }
  attr(raw, "checksum") <- unname(tools::md5sum(dest))
  attr(raw, "source") <- "Latest official CSV from data.gov.my"
  attr(raw, "downloaded") <- file.mtime(dest)
  raw
}

clean_donations <- function(raw) {
  raw |>
    janitor::clean_names() |>
    mutate(date = as.Date(date), blood_type = toupper(blood_type))
}

# Data-quality checks shown in the Rmd's "Data-quality assessment" section.
audit_raw <- function(raw, clean = clean_donations(raw)) {
  all_check <- clean |>
    select(date, state, blood_type, donations) |>
    pivot_wider(names_from = blood_type, values_from = donations) |>
    mutate(difference = ALL - (A + B + AB + O))
  list(
    checksum = attr(raw, "checksum"),
    source = attr(raw, "source") %||% "Unknown",
    downloaded = attr(raw, "downloaded"),
    rows = nrow(raw),
    columns = ncol(raw),
    first_date = min(clean$date),
    last_date = max(clean$date),
    distinct_dates = n_distinct(clean$date),
    regions = n_distinct(clean$state),
    blood_type_values = n_distinct(clean$blood_type),
    missing_values = sum(is.na(raw)),
    duplicate_rows = sum(duplicated(raw)),
    duplicate_keys = sum(duplicated(clean[c("date", "state", "blood_type")])),
    negative_values = sum(clean$donations < 0, na.rm = TRUE),
    zero_values = sum(clean$donations == 0, na.rm = TRUE),
    all_rows_checked = nrow(all_check),
    all_mismatches = sum(all_check$difference != 0),
    all_max_abs_difference = max(abs(all_check$difference))
  )
}

# The newest date on which both regions report all four ABO groups.
complete_data_end <- function(clean) {
  clean |>
    filter(state %in% KLANG_VALLEY_STATES, blood_type %in% ABO_GROUPS) |>
    count(date) |>
    filter(n == length(KLANG_VALLEY_STATES) * length(ABO_GROUPS)) |>
    summarise(end = max(date)) |>
    pull(end)
}

# Selangor + W.P. Kuala Lumpur, ABO groups only (ALL excluded to avoid
# double counting), from 2006 to `end_date`.
build_panel <- function(clean, end_date = complete_data_end(clean)) {
  clean |>
    filter(date >= ANALYSIS_START, date <= end_date,
           state %in% KLANG_VALLEY_STATES, blood_type %in% ABO_GROUPS) |>
    group_by(date, blood_type) |>
    summarise(donations = sum(donations), .groups = "drop") |>
    arrange(date, blood_type)
}

# Evaluation periods: the last three complete calendar years, plus the current
# year to date once it has at least MIN_PARTIAL_DAYS days of data.
evaluation_periods <- function(panel, n_complete = 3L) {
  end <- max(panel$date)
  end_year <- as.integer(year(end))
  year_complete <- end >= as.Date(paste0(end_year, "-12-31"))
  last_complete <- if (year_complete) end_year else end_year - 1L
  years <- seq(last_complete - n_complete + 1L, last_complete)
  partial_days <- as.integer(end - as.Date(paste0(end_year, "-01-01"))) + 1L
  partial <- !year_complete && partial_days >= MIN_PARTIAL_DAYS
  if (partial) years <- c(years, end_year)
  tibble(
    evaluation_year = years,
    partial = years == end_year & !year_complete,
    label = ifelse(years == end_year & !year_complete,
                   paste0(years, " to date (to ", format(end, "%d %b"), ")"),
                   as.character(years))
  )
}

# --- Modelling features -------------------------------------------------------

# Weekday as an unordered factor (Monday reference) and a day-count time index
# from the first training date, exactly as in the Rmd. The seven-day lag is
# computed per blood group over the continuous 2006–2025 calendar.
add_model_features <- function(panel) {
  model_start_date <- min(panel$date)
  panel |>
    mutate(
      blood_type = factor(blood_type, levels = ABO_GROUPS),
      weekday = factor(WEEKDAYS[wday(date, week_start = 1)],
                       levels = WEEKDAYS, ordered = FALSE),
      time_index = as.numeric(date - model_start_date)
    ) |>
    group_by(blood_type) |>
    arrange(date, .by_group = TRUE) |>
    mutate(seven_day_prediction = lag(donations, n = 7)) |>
    ungroup()
}

# Stop if any blood group's dates are not one continuous daily calendar,
# because a seven-row lag would then not be a seven-day lag.
check_calendar_continuity <- function(model_data) {
  gaps <- model_data |>
    group_by(blood_type) |>
    arrange(date, .by_group = TRUE) |>
    summarise(max_gap = max(diff(date)), .groups = "drop")
  stopifnot(all(gaps$max_gap == 1))
  invisible(TRUE)
}

# --- Forecasting (ported from predict_evaluation_year() in the Rmd) ----------

# Fits the four methods using only data before `evaluation_year`:
#   * Seven-day baseline: the observed count seven days earlier (rolling).
#   * Weekday average: full-history mean per blood group and weekday.
#   * Linear regression: lm(donations ~ weekday + time_index) per blood group.
#   * Recent weekday average: weekday mean over the preceding three years.
predict_evaluation_year <- function(data, evaluation_year, recent_years = 3L) {
  year_predictions <- vector("list", length(ABO_GROUPS))
  for (i in seq_along(ABO_GROUPS)) {
    group_name <- ABO_GROUPS[i]
    earlier_data <- data |>
      filter(blood_type == group_name, year(date) < evaluation_year)
    recent_data <- earlier_data |>
      filter(year(date) >= evaluation_year - recent_years)
    year_data <- data |>
      filter(blood_type == group_name, year(date) == evaluation_year)

    stopifnot(max(earlier_data$date) < min(year_data$date))
    stopifnot(n_distinct(year(recent_data$date)) == recent_years)

    annual_model <- lm(donations ~ weekday + time_index, data = earlier_data)
    year_data$regression_prediction <- unname(predict(annual_model, newdata = year_data))

    weekday_means <- earlier_data |>
      group_by(weekday) |>
      summarise(weekday_prediction = mean(donations), .groups = "drop")
    recent_means <- recent_data |>
      group_by(weekday) |>
      summarise(recent_prediction = mean(donations), .groups = "drop")
    year_data <- year_data |>
      left_join(weekday_means, by = "weekday") |>
      left_join(recent_means, by = "weekday")

    year_predictions[[i]] <- bind_rows(
      year_data |> transmute(date, blood_type, weekday, donations,
                             prediction = seven_day_prediction,
                             method = "Seven-day baseline"),
      year_data |> transmute(date, blood_type, weekday, donations,
                             prediction = weekday_prediction,
                             method = "Weekday average"),
      year_data |> transmute(date, blood_type, weekday, donations,
                             prediction = regression_prediction,
                             method = "Linear regression"),
      year_data |> transmute(date, blood_type, weekday, donations,
                             prediction = recent_prediction,
                             method = "Recent weekday average")
    ) |>
      mutate(evaluation_year = as.integer(evaluation_year),
             training_end = max(earlier_data$date))
  }
  out <- bind_rows(year_predictions)
  expected_days <- n_distinct(data$date[year(data$date) == evaluation_year])
  stopifnot(nrow(out) == expected_days * 4L * 4L,
            !anyNA(out$prediction),
            !anyDuplicated(out[c("date", "blood_type", "method")]))
  out |>
    mutate(blood_type = as.character(blood_type),
           error = donations - prediction,
           absolute_error = abs(error),
           squared_error = error^2)
}

# Same error definitions as score_forecasts() in the Rmd.
score_forecasts <- function(predictions, groups = c("evaluation_year", "method")) {
  stopifnot(all(is.finite(predictions$prediction)),
            all(is.finite(predictions$donations)))
  predictions |>
    group_by(across(all_of(groups))) |>
    summarise(observations = n(),
              MAE = mean(abs(donations - prediction)),
              RMSE = sqrt(mean((donations - prediction)^2)),
              mean_signed_error = mean(donations - prediction),
              .groups = "drop")
}

# --- Diagnostics --------------------------------------------------------------

# Low-collection days: actual count at or below the blood group's lower
# quartile over the training years (2006 to the year before evaluation).
# This is an after-the-fact error audit, not an alert.
low_collection_cutoffs <- function(model_data, evaluation_year) {
  model_data |>
    filter(year(date) < evaluation_year) |>
    group_by(blood_type) |>
    summarise(low_cutoff = as.numeric(quantile(donations, probs = 0.25)),
              .groups = "drop") |>
    mutate(blood_type = as.character(blood_type))
}

low_collection_errors <- function(predictions, cutoffs) {
  predictions |>
    left_join(cutoffs, by = "blood_type") |>
    mutate(day_category = ifelse(donations <= low_cutoff,
                                 "Low collection", "Other days"),
           overpredicted = prediction > donations,
           excess_prediction = ifelse(prediction > donations,
                                      prediction - donations, 0)) |>
    group_by(blood_type, method, day_category) |>
    summarise(days = n(), mean_actual = mean(donations),
              MAE = mean(absolute_error),
              overprediction_percent = 100 * mean(overpredicted),
              mean_excess = mean(excess_prediction), .groups = "drop")
}

minimum_prediction_check <- function(predictions, cutoffs) {
  predictions |>
    group_by(blood_type, method) |>
    summarise(minimum_prediction = min(prediction), .groups = "drop") |>
    left_join(cutoffs, by = "blood_type") |>
    mutate(all_predictions_above_cutoff = minimum_prediction > low_cutoff)
}

# Coefficient explanation from the 2006–2024 regression fits.
regression_coefficients <- function(model_data, training_end_year = 2024L) {
  bind_rows(lapply(ABO_GROUPS, function(g) {
    fit <- lm(donations ~ weekday + time_index,
              data = filter(model_data, blood_type == g,
                            year(date) <= training_end_year))
    cf <- coef(fit)
    tibble(blood_type = g,
           change_per_365_days = unname(cf["time_index"]) * 365,
           sunday_minus_monday = unname(cf["weekdaySunday"]),
           monday_intercept = unname(cf["(Intercept)"]),
           r_squared = summary(fit)$r.squared)
  }))
}

monthly_errors <- function(predictions) {
  predictions |>
    mutate(month = format(date, "%Y-%m")) |>
    group_by(month, method) |>
    summarise(observations = n(), MAE = mean(absolute_error), .groups = "drop")
}

# --- Descriptive summaries ----------------------------------------------------

weekday_summary <- function(model_data) {
  model_data |>
    group_by(blood_type, weekday) |>
    summarise(mean_donations = mean(donations),
              median_donations = median(donations), .groups = "drop")
}

# --- Live extras ---------------------------------------------------------------

# Forecasts for the next `horizon` days after the data ends, using exactly the
# four method definitions: each target date is predicted from years before its
# own year (the seven-day lag uses the observed count a week earlier, so the
# horizon is at most seven days).
next_days_forecast <- function(model_data, horizon = 7L) {
  stopifnot(horizon >= 1L, horizon <= 7L)
  last <- max(model_data$date)
  start <- min(model_data$date)
  targets <- tidyr::expand_grid(date = last + seq_len(horizon), blood_type = ABO_GROUPS) |>
    mutate(weekday = factor(WEEKDAYS[wday(date, week_start = 1)], levels = WEEKDAYS),
           time_index = as.numeric(date - start))
  rows <- lapply(split(targets, list(targets$blood_type, year(targets$date)), drop = TRUE), function(t) {
    g <- t$blood_type[1]; y <- year(t$date[1])
    hist <- filter(model_data, blood_type == g, year(date) < y)
    recent <- filter(hist, year(date) >= y - 3L)
    fit <- lm(donations ~ weekday + time_index, data = hist)
    wd <- hist |> group_by(weekday) |> summarise(v = mean(donations), .groups = "drop")
    rc <- recent |> group_by(weekday) |> summarise(v = mean(donations), .groups = "drop")
    lag7 <- filter(model_data, blood_type == g) |> select(date, donations)
    t |> mutate(
      `Seven-day baseline` = lag7$donations[match(date - 7, lag7$date)],
      `Weekday average` = wd$v[match(weekday, wd$weekday)],
      `Linear regression` = unname(predict(fit, newdata = t)),
      `Recent weekday average` = rc$v[match(weekday, rc$weekday)])
  })
  bind_rows(rows) |>
    tidyr::pivot_longer(all_of(METHODS), names_to = "method", values_to = "prediction") |>
    mutate(weekday = as.character(weekday)) |>
    arrange(date, blood_type, factor(method, levels = METHODS))
}

# Share of recorded donations by ABO group, per year.
blood_type_mix <- function(panel) {
  panel |>
    mutate(year = as.integer(year(date))) |>
    group_by(year, blood_type) |>
    summarise(donations = sum(donations), days = n_distinct(date), .groups = "drop") |>
    group_by(year) |>
    mutate(share = donations / sum(donations)) |>
    ungroup()
}

# Headline numbers for the live tracker. Comparisons use the same weekdays a
# year earlier (364 days back) so weekly patterns line up.
latest_summary <- function(panel) {
  daily <- panel |> group_by(date) |> summarise(total = sum(donations), .groups = "drop")
  end <- max(daily$date)
  win <- function(from, to) sum(daily$total[daily$date >= from & daily$date <= to])
  ytd_start <- as.Date(paste0(year(end), "-01-01"))
  list(
    end = end,
    latest_total = daily$total[daily$date == end],
    latest_by_group = panel |> filter(date == end) |> select(blood_type, donations),
    last7 = win(end - 6, end),
    last7_prev_year = win(end - 6 - 364, end - 364),
    ytd = win(ytd_start, end),
    ytd_prev_year = win(ytd_start - 364, end - 364),
    daily = daily
  )
}

# Recorded donations by state (all of Malaysia), for the "where to donate"
# region filter. Uses the ALL rows, which are each state's total.
state_summary <- function(clean, end_date) {
  totals <- clean |>
    filter(blood_type == "ALL", state != "Malaysia", date <= end_date)
  ytd_start <- as.Date(paste0(year(end_date), "-01-01"))
  totals |>
    group_by(state) |>
    summarise(last30 = sum(donations[date > end_date - 30]),
              ytd = sum(donations[date >= ytd_start]),
              .groups = "drop") |>
    mutate(region = vapply(state, function(s) {
      hit <- names(MALAYSIA_REGIONS)[vapply(MALAYSIA_REGIONS, function(r) s %in% r, TRUE)]
      if (length(hit)) hit[1] else NA_character_
    }, ""))
}

# Plain-language findings, computed from the results rather than written by
# hand, so they stay true as the data changes.
summarise_findings <- function(scores_overall, scores_by_group, low_errors, periods) {
  label <- setNames(periods$label, periods$evaluation_year)
  best <- scores_overall |> group_by(evaluation_year) |>
    summarise(mae = method[which.min(MAE)], rmse = method[which.min(RMSE)], .groups = "drop")
  out <- character()
  winners <- unique(best$mae)
  if (length(winners) == 1L && all(best$rmse == winners)) {
    span <- paste0(min(periods$evaluation_year), " to ", max(periods$evaluation_year),
                   if (any(periods$partial)) " so far" else "")
    out <- c(out, sprintf(paste("The %s is the most accurate method in every test period, %s:",
                                "the smallest average error, and the smallest big misses."),
                          tolower(winners), span))
  } else {
    out <- c(out, paste0("Most accurate method by test period: ",
                         paste(sprintf("%s, %s", label[as.character(best$evaluation_year)],
                                       tolower(best$mae)), collapse = "; "), "."))
  }
  g <- scores_by_group |> select(evaluation_year, blood_type, method, MAE) |>
    tidyr::pivot_wider(names_from = method, values_from = MAE)
  lr_lag <- all(g$`Linear regression` < g$`Seven-day baseline`)
  non_ab <- filter(g, blood_type != "AB")
  lr_wd <- all(non_ab$`Linear regression` < non_ab$`Weekday average`)
  if (lr_lag) {
    out <- c(out, paste0("Linear regression is more accurate than the seven-day lag for every blood group ",
                         "in every period",
                         if (lr_wd) ", and more accurate than the full-history weekday mean for A, B and O." else "."))
  }
  ab <- filter(g, blood_type == "AB")
  if (nrow(ab) && all(ab$`Weekday average` < ab$`Linear regression`)) {
    out <- c(out, "AB is the exception: there the full-history weekday mean is slightly more accurate than regression in every period.")
  }
  low <- low_errors |> filter(day_category == "Low collection")
  if (nrow(low)) {
    best_low <- low |> group_by(blood_type) |> summarise(best = method[which.min(MAE)], .groups = "drop")
    wd_groups <- best_low$blood_type[best_low$best == "Weekday average"]
    over_all <- all(low$overprediction_percent[low$method == "Weekday average"] == 100)
    if (length(wd_groups)) {
      out <- c(out, sprintf(paste0("On low-collection days in %s, the full-history weekday mean is the ",
                                   "most accurate for %s%s."),
                            label[as.character(max(low$evaluation_year))],
                            if (length(wd_groups) > 1)
                              paste(paste(head(wd_groups, -1), collapse = ", "), "and", tail(wd_groups, 1))
                            else wd_groups,
                            if (over_all) ", but it still forecasts too high on every one of those days" else ""))
    }
  }
  out
}

# --- One call that builds everything the dashboard needs ----------------------

build_dashboard_data <- function(raw, end_date = NULL, evaluation_years = NULL) {
  clean <- clean_donations(raw)
  audit <- audit_raw(raw, clean)
  if (is.null(end_date)) end_date <- complete_data_end(clean)
  panel <- build_panel(clean, end_date)
  model_data <- add_model_features(panel)
  check_calendar_continuity(model_data)

  periods <- evaluation_periods(panel)
  if (!is.null(evaluation_years)) {
    periods <- filter(periods, evaluation_year %in% evaluation_years)
  }
  latest_year <- max(periods$evaluation_year)

  predictions <- bind_rows(lapply(periods$evaluation_year, function(y)
    predict_evaluation_year(model_data, y)))

  low <- lapply(periods$evaluation_year, function(y) {
    cut <- low_collection_cutoffs(model_data, y)
    p <- filter(predictions, evaluation_year == y)
    list(cutoffs = mutate(cut, evaluation_year = y),
         errors = mutate(low_collection_errors(p, cut), evaluation_year = y),
         minimums = mutate(minimum_prediction_check(p, cut), evaluation_year = y))
  })
  low_cutoffs <- bind_rows(lapply(low, `[[`, "cutoffs"))
  low_errors <- bind_rows(lapply(low, `[[`, "errors"))
  min_predictions <- bind_rows(lapply(low, `[[`, "minimums"))
  scores_overall <- score_forecasts(predictions)
  scores_by_group <- score_forecasts(predictions, c("evaluation_year", "blood_type", "method"))

  list(
    built = Sys.time(),
    audit = audit,
    panel = panel,
    panel_summary = list(rows = nrow(panel),
                         dates = n_distinct(panel$date),
                         first_date = min(panel$date),
                         last_date = max(panel$date),
                         zero_values = sum(panel$donations == 0)),
    model_data = model_data,
    periods = periods,
    latest_year = latest_year,
    predictions = predictions,
    scores_overall = scores_overall,
    scores_by_group = scores_by_group,
    low_cutoffs = low_cutoffs,
    low_errors = low_errors,
    min_predictions = min_predictions,
    coefficients = regression_coefficients(model_data, latest_year - 1L),
    monthly = monthly_errors(predictions) |>
      mutate(evaluation_year = as.integer(substr(month, 1, 4))),
    weekly = weekly_totals(panel),
    weekday = weekday_summary(model_data),
    annual = annual_summary(panel),
    blood_mix = blood_type_mix(panel),
    latest = latest_summary(panel),
    states = state_summary(clean, end_date),
    next_days = next_days_forecast(model_data, 7L),
    findings = summarise_findings(scores_overall, scores_by_group,
                                  filter(low_errors, evaluation_year == latest_year), periods)
  )
}

# The configuration used in the FIT5145 report: frozen snapshot to 2025.
build_report_data <- function(path = SNAPSHOT_PATH) {
  build_dashboard_data(load_snapshot(path), end_date = REPORT_END_DATE,
                       evaluation_years = REPORT_EVALUATION_YEARS)
}

# The newest data, falling back to the frozen snapshot if the download fails.
build_live_data <- function(...) {
  raw <- load_live(...)
  if (is.null(raw)) {
    raw <- load_snapshot()
    attr(raw, "source") <- "Frozen snapshot (the latest download was unavailable)"
  }
  build_dashboard_data(raw)
}
