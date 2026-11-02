# Shared constants and light helpers. This is all the dashboard needs to show
# precomputed results, so the published site loads only dplyr and lubridate
# here; R/analysis.R adds readr, janitor and tidyr for the full pipeline.

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
})

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
  panel |>
    mutate(week_start = floor_date(date, "week", week_start = 1)) |>
    group_by(week_start, blood_type) |>
    summarise(donations = sum(donations), days = n(), .groups = "drop")
}

annual_summary <- function(panel) {
  panel |>
    mutate(year = year(date)) |>
    group_by(year, blood_type) |>
    summarise(mean_daily = mean(donations), total = sum(donations),
              zero_days = sum(donations == 0), .groups = "drop")
}

