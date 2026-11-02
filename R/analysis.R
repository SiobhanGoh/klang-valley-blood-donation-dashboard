# Analysis functions for the Klang Valley blood donation dashboard.
#
# Every function here reproduces a step of the final FIT5145 Assignment 3
# R Markdown analysis (BloodReady_AI_Demonstration.Rmd, 15 October 2026).
# The dashboard recomputes all results from the frozen data snapshot; the
# saved Rmd outputs in reference/ are used only by the tests to confirm that
# the recomputed numbers match the original analysis.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(lubridate)
  library(janitor)
})

SNAPSHOT_MD5 <- "b5365310d6819b52d39ddeba125093e5"
SNAPSHOT_PATH <- "data/blood_donations_state_snapshot.csv.gz"
ANALYSIS_START <- as.Date("2006-01-01")
ANALYSIS_END_EXCLUSIVE <- as.Date("2026-01-01")
KLANG_VALLEY_STATES <- c("Selangor", "W.P. Kuala Lumpur")
ABO_GROUPS <- c("A", "AB", "B", "O")
WEEKDAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday",
              "Friday", "Saturday", "Sunday")
EVALUATION_YEARS <- c(2023L, 2024L, 2025L)
METHODS <- c("Seven-day baseline", "Weekday average",
             "Linear regression", "Recent weekday average")

# Colours taken from the final Rmd so charts match the submitted figures.
METHOD_COLOURS <- c("Seven-day baseline" = "#48000C",
                    "Weekday average" = "#737373",
                    "Linear regression" = "#4D7966",
                    "Recent weekday average" = "#B05C83")
GROUP_COLOURS <- c("A" = "#BF5B79", "AB" = "#8B6CAB",
                   "B" = "#398C99", "O" = "#416A51")

# --- Data loading and audit --------------------------------------------------

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
  raw <- read_csv(tmp, show_col_types = FALSE,
                  col_types = cols(date = col_date(), state = col_character(),
                                   blood_type = col_character(),
                                   donations = col_double()))
  attr(raw, "checksum") <- checksum
  raw
}

clean_donations <- function(raw) {
  raw |>
    clean_names() |>
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

# Selangor + W.P. Kuala Lumpur, ABO groups only (ALL excluded to avoid
# double counting), complete calendar years 2006–2025.
build_panel <- function(clean) {
  clean |>
    filter(date >= ANALYSIS_START, date < ANALYSIS_END_EXCLUSIVE,
           state %in% KLANG_VALLEY_STATES, blood_type %in% ABO_GROUPS) |>
    group_by(date, blood_type) |>
    summarise(donations = sum(donations), .groups = "drop") |>
    arrange(date, blood_type)
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
  expected_days <- as.integer(as.Date(paste0(evaluation_year + 1L, "-01-01")) -
                                as.Date(paste0(evaluation_year, "-01-01")))
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

weekly_totals <- function(panel) {
  panel |>
    mutate(week_start = floor_date(date, "week", week_start = 1)) |>
    group_by(week_start, blood_type) |>
    summarise(donations = sum(donations), days = n(), .groups = "drop")
}

weekday_summary <- function(model_data) {
  model_data |>
    group_by(blood_type, weekday) |>
    summarise(mean_donations = mean(donations),
              median_donations = median(donations), .groups = "drop")
}

annual_summary <- function(panel) {
  panel |>
    mutate(year = year(date)) |>
    group_by(year, blood_type) |>
    summarise(mean_daily = mean(donations), total = sum(donations),
              zero_days = sum(donations == 0), .groups = "drop")
}

# --- One call that builds everything the dashboard needs ----------------------

build_dashboard_data <- function(path = SNAPSHOT_PATH) {
  raw <- load_snapshot(path)
  clean <- clean_donations(raw)
  audit <- audit_raw(raw, clean)
  panel <- build_panel(clean)
  model_data <- add_model_features(panel)
  check_calendar_continuity(model_data)

  predictions <- bind_rows(lapply(EVALUATION_YEARS, function(y)
    predict_evaluation_year(model_data, y)))
  stopifnot(nrow(predictions) == 17536)

  cutoffs_2025 <- low_collection_cutoffs(model_data, 2025L)
  predictions_2025 <- filter(predictions, evaluation_year == 2025L)

  list(
    audit = audit,
    panel = panel,
    panel_summary = list(rows = nrow(panel),
                         dates = n_distinct(panel$date),
                         first_date = min(panel$date),
                         last_date = max(panel$date),
                         zero_values = sum(panel$donations == 0)),
    model_data = model_data,
    predictions = predictions,
    scores_overall = score_forecasts(predictions),
    scores_by_group = score_forecasts(predictions,
                                      c("evaluation_year", "blood_type", "method")),
    low_cutoffs_2025 = cutoffs_2025,
    low_errors_2025 = low_collection_errors(predictions_2025, cutoffs_2025),
    min_predictions_2025 = minimum_prediction_check(predictions_2025, cutoffs_2025),
    coefficients = regression_coefficients(model_data, 2024L),
    monthly_2025 = monthly_errors(predictions_2025),
    weekly = weekly_totals(panel),
    weekday = weekday_summary(model_data),
    annual = annual_summary(panel)
  )
}
