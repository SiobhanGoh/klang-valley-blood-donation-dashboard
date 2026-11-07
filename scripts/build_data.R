# Build the dashboard's data from the latest official CSV and save it to
# app_data/dashboard.rds, which the app (and the published site) reads.
#
#   Rscript scripts/build_data.R             # download the latest data
#   Rscript scripts/build_data.R --snapshot  # use the bundled frozen snapshot
#
# Exits with an error if the download fails, so a scheduled run never
# replaces a good published site with stale or partial data.

source("R/analysis.R")

args <- commandArgs(trailingOnly = TRUE)
use_snapshot <- "--snapshot" %in% args

raw <- if (use_snapshot) load_snapshot() else load_live(max_age_hours = 0)
if (is.null(raw)) stop("Could not download the latest data from ", LIVE_CSV_URL)

d <- build_dashboard_data(raw)
dir.create("app_data", showWarnings = FALSE)

# Track record: append today's forecasts to the log (never editing earlier ones) and score every
# logged forecast whose day now has data. The publish workflow keeps the log on the
# forecast-log branch; locally it simply lives in app_data/.
log_path <- Sys.getenv("KVBD_FORECAST_LOG", "app_data/forecast_log.csv")
forecast_log <- update_forecast_log(read_forecast_log(log_path), d$next_days, d$panel_summary$last_date)
utils::write.csv(forecast_log, log_path, row.names = FALSE)
d$track_record <- score_forecast_log(forecast_log, d$panel)
# Plain data frames only: the in-browser app reads them without dplyr or tibble.
untibble <- function(x) {
  if (is.data.frame(x)) return(as.data.frame(x))
  if (is.list(x) && !is.object(x)) return(lapply(x, untibble))
  x
}
d <- untibble(d)
saveRDS(d, "app_data/dashboard.rds", compress = "xz")

cat(sprintf(paste0("Source: %s\nRows: %s\nData to: %s\nPeriods: %s\n",
                   "Latest-period MAE (recent weekday mean): %.2f\nSaved: app_data/dashboard.rds (%.0f KB)\n"),
            d$audit$source, format(d$audit$rows, big.mark = ","), d$panel_summary$last_date,
            paste(d$periods$label, collapse = ", "),
            d$scores_overall$MAE[d$scores_overall$evaluation_year == d$latest_year &
                                 d$scores_overall$method == "Recent weekday average"],
            file.size("app_data/dashboard.rds") / 1024))
cat(sprintf("Forecast log: %s rows, %s scored\n", format(nrow(forecast_log), big.mark = ","),
            format(if (is.null(d$track_record)) 0 else d$track_record$scored, big.mark = ",")))
