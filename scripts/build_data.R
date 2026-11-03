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
