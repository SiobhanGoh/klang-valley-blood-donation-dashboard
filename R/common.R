# Shared constants and light helpers, in base R. This is all the dashboard
# needs to show precomputed results, so the published site loads no data
# packages at all; R/analysis.R adds dplyr, lubridate, readr, janitor and
# tidyr for the full pipeline.

year_of <- function(d) as.integer(format(d, "%Y"))
weekday_num <- function(d) as.integer(format(d, "%u"))          # 1 = Monday ... 7 = Sunday
month_start <- function(d) as.Date(format(d, "%Y-%m-01"))
week_start_monday <- function(d) d - (weekday_num(d) - 1L)

# Sum or average `value` within groups, returning a plain data frame.
group_stat <- function(df, by, value, fun, name) {
  out <- stats::aggregate(df[[value]], by = df[by], FUN = fun)
  names(out)[ncol(out)] <- name
  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

SNAPSHOT_MD5 <- "b5365310d6819b52d39ddeba125093e5"
SNAPSHOT_PATH <- "data/blood_donations_state_snapshot.csv.gz"
ANALYSIS_START <- as.Date("2006-01-01")
REPORT_END_DATE <- as.Date("2025-12-31")
LIVE_CSV_URL <- "https://storage.data.gov.my/healthcare/blood_donations_state.csv"
MIN_PARTIAL_DAYS <- 28L   # include the current year once it has four weeks of data
KLANG_VALLEY_STATES <- c("Selangor", "W.P. Kuala Lumpur")
ABO_GROUPS <- c("A", "AB", "B", "O")
WEEKDAYS <- c("Monday", "Tuesday", "Wednesday", "Thursday",
              "Friday", "Saturday", "Sunday")
REPORT_EVALUATION_YEARS <- c(2023L, 2024L, 2025L)
METHODS <- c("Seven-day baseline", "Weekday average",
             "Linear regression", "Recent weekday average")

# Colours taken from the final Rmd so charts match the submitted figures.
METHOD_COLOURS <- c("Seven-day baseline" = "#48000C",
                    "Weekday average" = "#737373",
                    "Linear regression" = "#4D7966",
                    "Recent weekday average" = "#B05C83")
GROUP_COLOURS <- c("A" = "#BF5B79", "AB" = "#8B6CAB",
                   "B" = "#398C99", "O" = "#416A51")

# Regions of Malaysia, for the "where blood is collected" filter.
MALAYSIA_REGIONS <- list(
  "Central" = c("W.P. Kuala Lumpur", "Selangor"),
  "Northern" = c("Perlis", "Kedah", "Pulau Pinang", "Perak"),
  "Southern" = c("Negeri Sembilan", "Melaka", "Johor"),
  "East Coast" = c("Pahang", "Terengganu", "Kelantan"),
  "Sabah & Sarawak" = c("Sabah", "Sarawak", "W.P. Labuan")
)

# --- Descriptive summaries used by the app ------------------------------------

weekly_totals <- function(panel) {
  panel$week_start <- week_start_monday(panel$date)
  out <- group_stat(panel, c("week_start", "blood_type"), "donations", sum, "donations")
  out$days <- group_stat(panel, c("week_start", "blood_type"), "donations", length, "days")$days
  out[order(out$week_start, out$blood_type), ]
}

annual_summary <- function(panel) {
  panel$year <- year_of(panel$date)
  by <- c("year", "blood_type")
  out <- group_stat(panel, by, "donations", mean, "mean_daily")
  out$total <- group_stat(panel, by, "donations", sum, "total")$total
  out$zero_days <- group_stat(panel, by, "donations", function(x) sum(x == 0), "zero_days")$zero_days
  out[order(out$year, out$blood_type), ]
}

