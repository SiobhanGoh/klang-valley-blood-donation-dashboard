# Builds the dashboard as a static website in _site/.
#
# R does all the work here, once a day in GitHub Actions: it renders the page,
# fills in every view at its default settings, and precomputes every view that
# has a fixed set of choices (test periods, method mixes, blood groups,
# regions, reports). Views that take free dates or year ranges are drawn in the
# browser from a small daily data file. Visitors download plain HTML, CSS,
# JavaScript and JSON, so the page opens in a second or two instead of starting
# R in the browser.
#
# Usage: Rscript scripts/build_static.R [_site]

suppressPackageStartupMessages({ library(shiny); library(htmltools); library(jsonlite) })
args <- commandArgs(trailingOnly = TRUE)
site <- if (length(args)) args[1] else "_site"
unlink(site, recursive = TRUE)
dir.create(file.path(site, "data"), recursive = TRUE)

# --- 1. The page itself -----------------------------------------------------------
app_env <- new.env()
sys.source("app.R", envir = app_env)
index <- file.path(site, "index.html")
htmltools::save_html(app_env$ui, index, libdir = "lib")
for (f in list.files("www", full.names = TRUE)) file.copy(f, site, recursive = TRUE)
invisible(file.create(file.path(site, ".nojekyll")))

from_app <- function(name) get(name, envir = app_env, inherits = TRUE)
dash <- from_app("dash")
METHODS <- from_app("METHODS")
GROUPS <- from_app("ABO_GROUPS")
BLOOD_TYPES <- from_app("BLOOD_TYPES")
REGIONS <- names(from_app("MALAYSIA_REGIONS"))
YEARS <- as.character(dash$periods$evaluation_year)
LATEST <- as.character(from_app("LATEST"))
DATA_END <- from_app("DATA_END")
END_YEAR <- from_app("END_YEAR")

# Every non-empty mix of methods, keyed in the legend's order.
method_sets <- unlist(lapply(seq_along(METHODS), function(k) combn(METHODS, k, simplify = FALSE)),
                      recursive = FALSE)
set_key <- function(m) paste(m, collapse = ",")

# --- 2. Run the app's own server code for every fixed choice ------------------------
html_of <- function(x) {
  if (is.null(x)) return("")
  if (is.list(x) && !is.null(x$html)) return(as.character(x$html))
  as.character(x)
}
grab <- function(output, ids) {
  out <- list()
  for (id in ids) out[[id]] <- tryCatch(html_of(output[[id]]), error = function(e) {
    message("  ", id, ": ", conditionMessage(e), " in ", paste(deparse(conditionCall(e)), collapse = " ")); ""
  })
  out
}
chart_ids <- c("overview_plot", "live_plot", "cmp_group_plot", "cmp_daily_plot", "low_plot", "weekly_plot",
               "weekday_plot", "annual_plot", "mix_plot", "monthly_plot")

store <- new.env()
app <- shiny::shinyAppFile("app.R")
shiny::testServer(app, {
  default_dates <- c(max(DATA_END - 30, min(dash$predictions$date)), DATA_END)
  session$setInputs(live_group = "O", cmp_year = LATEST, cmp_methods = METHODS, cmp_group = "O",
                    cmp_dates = default_dates, low_year = LATEST, month_year = LATEST,
                    month_methods = c("Seven-day baseline", "Linear regression", "Recent weekday average"),
                    pattern_groups = GROUPS, pattern_years = c(2006, END_YEAR),
                    bt_pick = "O+", region_pick = "Central", report_type = "summary", rep_period = LATEST, rep_focus = "all", rep_sections = unname(from_app("SUMMARY_SECTIONS")),
                    rep_group = "all", rep_dates = c(DATA_END - 30, DATA_END), rep_extra = "forecasts",
                    card_type = "O+", card_region = "Central")

  # Everything at its default setting.
  ids <- c(setdiff(chart_ids, "cmp_daily_plot"), "next_table", "range_table", "watch_note", "track_record", "live_change7", "live_change_ytd", "live_explain", "cmp_table",
           "cmp_headline", "cmp_sub", "cmp_stat", "cmp_explain", "low_headline", "low_sub",
           "low_stat", "low_explain", "cutoff_table", "low_table", "min_pred_table", "weekday_explain",
           "annual_explain", "audit_table", "panel_table", "coef_table", "bias_table", "monthly_explain",
           "compat_result", "region_result", "report_blurb", "report_preview", "card_last_label")
  store$init <- grab(output, ids)

  store$live <- list()
  for (g in GROUPS) { session$setInputs(live_group = g); store$live[[g]] <- grab(output, c("live_plot", "live_explain")) }

  store$compare <- list()
  for (y in YEARS) for (m in method_sets) {
    session$setInputs(cmp_year = y, cmp_methods = m)
    store$compare[[paste(y, set_key(m), sep = "|")]] <-
      grab(output, c("cmp_group_plot", "cmp_table", "cmp_headline", "cmp_sub", "cmp_stat", "cmp_explain"))
  }

  store$low <- list()
  for (y in YEARS) {
    session$setInputs(low_year = y)
    store$low[[y]] <- grab(output, c("low_plot", "low_headline", "low_sub", "low_stat", "low_explain",
                                     "cutoff_table", "low_table", "min_pred_table"))
  }

  store$monthly <- list()
  for (y in YEARS) for (m in method_sets) {
    session$setInputs(month_year = y, month_methods = m)
    store$monthly[[paste(y, set_key(m), sep = "|")]] <- grab(output, c("monthly_plot", "monthly_explain"))
  }

  store$compat <- list()
  for (t in BLOOD_TYPES) { session$setInputs(bt_pick = t); store$compat[[t]] <- html_of(output$compat_result) }
  store$region <- list()
  for (r in REGIONS) { session$setInputs(region_pick = r); store$region[[r]] <- html_of(output$region_result) }

  store$reports <- list(blurb = list(), summary = list(), card = list())
  for (k in c("summary", "data", "card")) {
    session$setInputs(report_type = k); store$reports$blurb[[k]] <- html_of(output$report_blurb)
  }
  session$setInputs(report_type = "summary")
  for (y in YEARS) for (g in c("all", ABO_GROUPS)) {
    session$setInputs(rep_period = y, rep_focus = g)
    store$reports$summary[[paste(y, g, sep = "|")]] <- html_of(output$report_preview)
  }
  session$setInputs(report_type = "card")
  for (t in BLOOD_TYPES) for (r in REGIONS) {
    session$setInputs(card_type = t, card_region = r)
    store$reports$card[[paste(t, r, sep = "|")]] <- html_of(output$report_preview)
  }
})

# The day-by-day Compare chart at its default dates, drawn straight from the chart code.
dd <- c(max(DATA_END - 30, min(dash$predictions$date)), DATA_END)
store$init$cmp_daily_plot <- as.character(from_app("chart_json")(from_app("chart_daily")(dash$predictions, "O", METHODS, dd[1], dd[2])))
store$init$daily_explain <- from_app("explain_daily")(dash$predictions, "O", METHODS, dd[1], dd[2])

# --- 3. Daily data for the views that take free dates or year ranges ------------------
p <- dash$panel[order(dash$panel$date), ]
days <- seq(min(p$date), max(p$date), by = "day")
counts <- lapply(setNames(GROUPS, GROUPS), function(g) {
  s <- p[p$blood_type == g, ]; s$donations[match(days, s$date)]
})
pr <- dash$predictions
pdays <- seq(min(pr$date), max(pr$date), by = "day")
preds <- lapply(setNames(METHODS, METHODS), function(m) lapply(setNames(GROUPS, GROUPS), function(g) {
  s <- pr[pr$method == m & pr$blood_type == g, ]; round(s$prediction[match(pdays, s$date)], 2)
}))
daily <- list(start = format(min(days)), end = format(max(days)), counts = counts,
              pred_start = format(min(pdays)), preds = preds, methods = METHODS, groups = GROUPS,
              data_end = format(from_app("DATA_END")), data_credit = from_app("data_credit"),
              made = trimws(format(Sys.Date(), "%e %B %Y")),
              group_col = as.list(from_app("GROUP_COL")), method_col = as.list(from_app("METHOD_COL")),
              method_dash = as.list(from_app("METHOD_DASH")))

write_json_file <- function(x, name) {
  path <- file.path(site, "data", name)
  writeLines(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = 6), path, useBytes = TRUE)
  cat(sprintf("  %-14s %7.0f KB\n", name, file.size(path) / 1024))
}
# Chart outputs are already JSON text; parse them so each file is one JSON document.
parse_charts <- function(x) {
  for (n in intersect(names(x), chart_ids)) if (nzchar(x[[n]])) x[[n]] <- jsonlite::fromJSON(x[[n]], simplifyVector = FALSE)
  x
}
cat("Data files:\n")
write_json_file(parse_charts(store$init), "init.json")
write_json_file(lapply(store$live, parse_charts), "live.json")
write_json_file(lapply(store$compare, parse_charts), "compare.json")
write_json_file(lapply(store$low, parse_charts), "low.json")
write_json_file(lapply(store$monthly, parse_charts), "monthly.json")
write_json_file(list(compat = store$compat, region = store$region), "blood101.json")
write_json_file(store$reports, "reports.json")
write_json_file(daily, "daily.json")

# --- 4. Bake the default text and tables into the page, so it reads before any script runs ---
html <- paste(readLines(index, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
for (id in setdiff(names(store$init), chart_ids)) {
  pat <- sprintf('(<(div|span)\\b[^>]*\\bid="%s"[^>]*>)(</\\2>)', id)
  if (!grepl(pat, html, perl = TRUE)) { message("  no container for ", id); next }
  val <- gsub("\\\\", "\\\\\\\\", store$init[[id]])
  val <- gsub("$", "\\$", val, fixed = TRUE)
  html <- sub(pat, paste0("\\1", val, "\\3"), html, perl = TRUE)
}
head_extra <- paste(
  '<title>Klang Valley Blood Donation Forecasts</title>',
  '<meta name="description" content="Daily blood donation collections in the Klang Valley, Malaysia: live data, four forecasting methods compared, and general blood donation information.">',
  '<meta name="theme-color" content="#FBF5F3">',
  '<link rel="icon" type="image/svg+xml" href="favicon.svg">',
  '<link rel="preload" href="data/init.json" as="fetch" crossorigin>',
  sep = "\n")
html <- sub("<title>[^<]*</title>", "", html)
html <- sub("</head>", paste0(head_extra, "\n</head>"), html, fixed = TRUE)
writeLines(html, index, useBytes = TRUE)
cat("Built", index, sprintf("(%.0f KB)\n", file.size(index) / 1024))
