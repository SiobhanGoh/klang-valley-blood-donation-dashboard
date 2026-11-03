# Printable reports. Each one tells its story in its own voice:
#   summary  - editorial: what happened and what it means
#   data     - technical: exactly what the numbers are and where they come from
#   card     - personal: a donor's own one-page card
# They are built as HTML "paper" that the browser saves as PDF; nothing a
# visitor enters is sent or stored. Sourced by app.R after its helpers.

report_paper <- function(kind, ...) {
  div(class = paste("paper", paste0("paper-", kind)), ...)
}

report_masthead <- function(kicker, title, deck) {
  div(class = "rp-mast",
      div(class = "rp-brand",
          span(class = "rp-logo", icon("droplet")),
          span("Klang Valley Blood Donation Forecasts")),
      div(class = "rp-kicker", kicker),
      h1(class = "rp-title", title),
      p(class = "rp-deck", deck))
}

report_footer <- function(...) {
  div(class = "rp-foot",
      p(..., " Made ", format(Sys.Date(), "%e %B %Y") |> trimws(),
        " from https://siobhangoh.github.io/klang-valley-blood-donation-dashboard/"))
}

data_credit <- paste("Data: Government of Malaysia, Daily Blood Donations by Blood Group & State",
                     "(data.gov.my), CC BY 4.0.")

# Horizontal bars as inline SVG, so the PDF needs no chart library.
svg_bars <- function(labels, values, highlight = NULL, digits = 1, width = 520) {
  n <- length(values)
  row <- 34
  mx <- max(values, 1)
  lab_w <- 190
  bar_w <- width - lab_w - 60
  tags$svg(class = "rp-bars", viewBox = sprintf("0 0 %d %d", width, n * row), role = "img",
           `aria-label` = paste(labels, fmt(values, digits), collapse = "; "),
           lapply(seq_len(n), function(i) {
             y <- (i - 1) * row
             w <- bar_w * values[i] / mx
             on <- identical(labels[i], highlight)
             tags$g(
               tags$text(x = 0, y = y + 21, class = "rp-bar-label", labels[i]),
               tags$rect(x = lab_w, y = y + 8, width = bar_w, height = 16, rx = 8, class = "rp-bar-track"),
               tags$rect(x = lab_w, y = y + 8, width = max(w, 2), height = 16, rx = 8,
                         class = if (on) "rp-bar-fill is-key" else "rp-bar-fill"),
               tags$text(x = lab_w + bar_w + 10, y = y + 21, class = "rp-bar-val", fmt(values[i], digits)))
           }))
}

# --- 1. Summary report: editorial ---------------------------------------------------

report_summary <- function(year) {
  y <- as.integer(year)
  lab <- PERIOD_LABEL[[as.character(y)]]
  sc <- dash$scores_overall[dash$scores_overall$evaluation_year == y, ]
  sc <- sc[order(sc$MAE), ]
  best <- sc$method[1]
  lag <- sc$MAE[sc$method == "Seven-day baseline"]
  gain <- 100 * (lag - sc$MAE[1]) / lag
  low <- dash$low_errors[dash$low_errors$evaluation_year == y &
                           dash$low_errors$day_category == "Low collection" &
                           dash$low_errors$method == best, ]
  over <- sum(low$days * low$overprediction_percent) / sum(low$days)
  wk <- pct_change(dash$latest$last7, dash$latest$last7_prev_year)

  report_paper(
    "summary",
    report_masthead(
      paste("Summary report ·", lab),
      sprintf("The %s is the one to beat.", method_phrase(best)),
      sprintf(paste("Across %s, it forecast recorded blood donations in Selangor and W.P. Kuala",
                    "Lumpur with an average error of %s donations per blood group per day, %s",
                    "lower than simply repeating last week's count."),
              lab, fmt(sc$MAE[1]), pct_text(gain))),
    div(class = "rp-stats",
        div(class = "rp-stat",
            div(class = "rp-stat-num", fmt_int(dash$latest$latest_total)),
            div(class = "rp-stat-lab", paste("donations on", fmt_date(DATA_END)))),
        div(class = "rp-stat",
            div(class = "rp-stat-num", fmt_int(dash$latest$last7)),
            div(class = "rp-stat-lab",
                if (is.na(wk)) "in the last seven days"
                else sprintf("in the last seven days, %s%% %s than a year earlier",
                             fmt(abs(wk), 1), if (wk >= 0) "more" else "fewer"))),
        div(class = "rp-stat",
            div(class = "rp-stat-num", fmt_int(dash$latest$ytd)),
            div(class = "rp-stat-lab", paste("so far in", END_YEAR)))),
    h2(class = "rp-h2", "The scoreboard"),
    p(class = "rp-p", "Average daily error of each method, in donations per blood group per day. Shorter is better."),
    svg_bars(sc$method, sc$MAE, highlight = best, digits = 2),
    h2(class = "rp-h2", "What stands out"),
    tags$ol(class = "rp-list", lapply(dash$findings, function(f) tags$li(f))),
    h2(class = "rp-h2", "The blind spot"),
    p(class = "rp-p", sprintf(paste("Low-collection days are those at or below each group's lower",
                                    "quartile from earlier years. On the %d such days in %s, the %s",
                                    "forecast too high on %s of them. No method here gives an early warning",
                                    "of a low day."),
                              sum(low$days), lab, method_phrase(best), pct_text(over))),
    div(class = "rp-callout",
        strong("Read with care. "),
        "These are forecasts of donations collected, not of blood demand, stock or shortage. ",
        "The evaluation is retrospective, the periods share training data, and there are no ",
        "forecast intervals."),
    report_footer(data_credit))
}

# --- 2. Data appendix: technical ----------------------------------------------------

data_extract <- function(group, from, to, forecasts) {
  groups <- if (identical(group, "all")) ABO_GROUPS else group
  rec <- dash$panel[dash$panel$blood_type %in% groups &
                      dash$panel$date >= from & dash$panel$date <= to, c("date", "blood_type", "donations")]
  rec <- rec[order(rec$date, rec$blood_type), ]
  out <- data.frame(date = format(rec$date), blood_group = rec$blood_type,
                    recorded = rec$donations, stringsAsFactors = FALSE)
  if (isTRUE(forecasts)) {
    for (m in METHODS) {
      pr <- dash$predictions[dash$predictions$method == m, c("date", "blood_type", "prediction")]
      key <- paste(format(pr$date), pr$blood_type)
      out[[gsub("[^a-z]+", "_", tolower(m))]] <- round(pr$prediction[match(paste(out$date, out$blood_group), key)], 2)
    }
  }
  out
}

csv_text <- function(df) {
  con <- textConnection("csv_lines", "w", local = TRUE)
  on.exit(close(con))
  writeLines(paste("#", data_credit), con)
  writeLines("# Recorded donations are Selangor and W.P. Kuala Lumpur combined.", con)
  utils::write.csv(df, con, row.names = FALSE, na = "")
  paste(textConnectionValue(con), collapse = "\n")
}

report_data <- function(group, from, to, forecasts, max_rows = 62L) {
  df <- data_extract(group, from, to, forecasts)
  shown <- utils::head(df, max_rows)
  names_shown <- c(date = "Date", blood_group = "Group", recorded = "Recorded",
                   seven_day_baseline = "7-day baseline", weekday_average = "Weekday average",
                   linear_regression = "Regression", recent_weekday_average = "Recent average")
  report_paper(
    "data",
    report_masthead(
      "Data appendix",
      "Recorded blood donations, Klang Valley",
      sprintf(paste("Daily recorded donations for %s, %s to %s, from the official open data.",
                    "Figures are counts, not estimates."),
              if (identical(group, "all")) "all four ABO groups" else paste("group", group),
              fmt_date(from), fmt_date(to))),
    tags$table(class = "rp-meta",
      tags$tr(tags$th("Source"), tags$td("Government of Malaysia, Daily Blood Donations by Blood Group & State, data.gov.my")),
      tags$tr(tags$th("Licence"), tags$td("Creative Commons Attribution 4.0")),
      tags$tr(tags$th("Region"), tags$td("Selangor and W.P. Kuala Lumpur combined (KL includes Putrajaya)")),
      tags$tr(tags$th("Data to"), tags$td(fmt_date(DATA_END))),
      tags$tr(tags$th("Rows"), tags$td(sprintf("%s in this extract%s", fmt_int(nrow(df)),
                                                if (nrow(df) > max_rows) sprintf("; the first %d are shown, the CSV has all", max_rows) else ""))),
      if (isTRUE(forecasts))
        tags$tr(tags$th("Forecasts"), tags$td("Shown for test periods only; blank before them."))),
    tags$table(class = "rp-table",
      tags$thead(tags$tr(lapply(names(shown), function(n) tags$th(names_shown[[n]] %||% n)))),
      tags$tbody(lapply(seq_len(nrow(shown)), function(i)
        tags$tr(lapply(names(shown), function(n) {
          v <- shown[[n]][i]
          tags$td(if (is.numeric(v)) (if (is.na(v)) "" else if (n == "recorded") fmt_int(v) else fmt(v, 1)) else v)
        }))))),
    h2(class = "rp-h2", "Definitions"),
    tags$dl(class = "rp-defs",
      tags$dt("Recorded"), tags$dd("Donations recorded that day in the Blood Bank Information System for the group."),
      tags$dt("7-day baseline"), tags$dd("The count recorded seven days earlier."),
      tags$dt("Weekday average"), tags$dd("The group's mean for that weekday over all years before the evaluation year."),
      tags$dt("Regression"), tags$dd("lm(donations ~ weekday + time_index), fitted per group on all earlier years."),
      tags$dt("Recent average"), tags$dd("The group's mean for that weekday over the three complete years before.")),
    report_footer(data_credit))
}

# --- 3. Donor card: personal --------------------------------------------------------

report_card <- function(type, region, last_donation = NULL) {
  cmp <- compatibility_for(type)
  next_date <- if (!is.null(last_donation) && length(last_donation) && !is.na(last_donation))
    seq(as.Date(last_donation), by = "3 months", length.out = 2)[2] else NULL
  st <- dash$states[!is.na(dash$states$region) & dash$states$region == region, ]
  chips <- function(on) div(class = "rp-chips",
    lapply(BLOOD_TYPES, function(x) span(class = paste("rp-chip", if (x %in% on) "is-on"), x)))
  note <- if (type == "O-") "Your red cells can go to every type: you are a universal red-cell donor."
          else if (type == "AB+") "You can receive red cells from every type: a universal red-cell recipient."
          else if (endsWith(type, "-")) "As an RhD-negative type, you can only receive RhD-negative red cells."
          else sprintf("You can receive from %d types and give to %d.", length(cmp$receives_from), length(cmp$gives_to))
  report_paper(
    "card",
    div(class = "rp-card-hero",
        div(class = "rp-card-drop", span(type)),
        div(class = "rp-card-copy",
            div(class = "rp-kicker", "My donor card"),
            h1(class = "rp-title", sprintf("Blood type %s", type)),
            p(class = "rp-deck", note))),
    div(class = "rp-card-grid",
        div(class = "rp-box",
            h3("I can give red cells to"), chips(cmp$gives_to)),
        div(class = "rp-box",
            h3("I can receive red cells from"), chips(cmp$receives_from))),
    if (!is.null(next_date))
      div(class = "rp-next",
          div(class = "rp-next-lab", "Earliest next donation"),
          div(class = "rp-next-date", format(next_date, "%e %B %Y") |> trimws()),
          div(class = "rp-next-note", sprintf(paste("At least three months after your last donation on %s.",
                                                    "Staff confirm eligibility on the day."),
                                              fmt_date(as.Date(last_donation))))),
    h2(class = "rp-h2", "Before I donate"),
    tags$ul(class = "rp-check", lapply(ELIGIBILITY, function(e)
      tags$li(HTML(sprintf("<strong>%s</strong>: %s", htmltools::htmlEscape(e$title), htmltools::htmlEscape(e$value)))))),
    h2(class = "rp-h2", sprintf("Where to donate: %s", region)),
    if (region == "Central")
      tags$ul(class = "rp-list", lapply(DONATION_CENTRES, function(ct)
        tags$li(strong(ct$name), sprintf(" (%s). %s", ct$area, ct$hours))))
    else p(class = "rp-p", sprintf(paste("In %s, donations are collected at Ministry of Health blood",
                                         "collection sites and mobile campaigns. Check PDN or the",
                                         "nearest state hospital for times and locations."),
                                   paste(st$state, collapse = ", "))),
    h2(class = "rp-h2", "Afterwards"),
    tags$ul(class = "rp-list", lapply(AFTERCARE, tags$li)),
    report_footer("For general information only, not a medical document. Compatibility is a",
                  "simplified red-cell ABO and RhD guide; transfusions are matched in a laboratory.",
                  "Eligibility summarised from Pusat Darah Negara."))
}
