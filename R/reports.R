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

SUMMARY_SECTIONS <- c("The numbers" = "stats", "Scoreboard" = "scoreboard",
                      "What stands out" = "findings", "The blind spot" = "blindspot")

# Sums of one group's (or all groups') recorded donations between two dates.
panel_sum <- function(groups, from, to) {
  p <- dash$panel
  sum(p$donations[p$blood_type %in% groups & p$date >= from & p$date <= to])
}

# group: "all" or one ABO group, which retells the report for that group.
# sections: which optional sections to include; the headline and the "read with
# care" note are always there.
report_summary <- function(year, group = "all", sections = SUMMARY_SECTIONS) {
  y <- as.integer(year)
  lab <- PERIOD_LABEL[[as.character(y)]]
  one <- !identical(group, "all")
  groups <- if (one) group else ABO_GROUPS
  sc <- if (one) dash$scores_by_group[dash$scores_by_group$evaluation_year == y &
                                        dash$scores_by_group$blood_type == group, ]
        else dash$scores_overall[dash$scores_overall$evaluation_year == y, ]
  sc <- sc[order(sc$MAE), ]
  best <- sc$method[1]
  lag <- sc$MAE[sc$method == "Seven-day baseline"]
  gain <- 100 * (lag - sc$MAE[1]) / lag
  low <- dash$low_errors[dash$low_errors$evaluation_year == y &
                           dash$low_errors$day_category == "Low collection" &
                           dash$low_errors$method == best &
                           dash$low_errors$blood_type %in% groups, ]
  over <- sum(low$days * low$overprediction_percent) / sum(low$days)
  end <- DATA_END
  last7 <- panel_sum(groups, end - 6, end)
  prev7 <- panel_sum(groups, seq(end, by = "-1 year", length.out = 2)[2] - 6, seq(end, by = "-1 year", length.out = 2)[2])
  wk <- pct_change(last7, prev7)
  latest <- panel_sum(groups, end, end)
  ytd <- panel_sum(groups, as.Date(sprintf("%s-01-01", END_YEAR)), end)
  share <- if (one) 100 * panel_sum(group, end - 29, end) / max(panel_sum(ABO_GROUPS, end - 29, end), 1) else NA
  who <- if (one) sprintf("group %s donations", group) else "blood donations"
  sec <- function(id, ...) if (id %in% sections) div(class = "rp-sec", `data-sec` = id, ...)

  report_paper(
    "summary",
    report_masthead(
      paste("Summary report ·", lab, if (one) paste("· Group", group)),
      sprintf("The %s is the one to beat%s.", method_phrase(best), if (one) paste(" for group", group) else ""),
      sprintf(paste("Across %s, it forecast recorded %s in Selangor and W.P. Kuala",
                    "Lumpur with an average error of %s %s, %s",
                    "lower than simply repeating last week's count."),
              lab, who, fmt(sc$MAE[1]), if (one) "donations a day" else "donations per blood group per day",
              pct_text(gain))),
    sec("stats", div(class = "rp-stats",
        div(class = "rp-stat",
            div(class = "rp-stat-num", fmt_int(latest)),
            div(class = "rp-stat-lab", paste(if (one) paste("group", group, "donations on") else "donations on",
                                             fmt_date(end)))),
        div(class = "rp-stat",
            div(class = "rp-stat-num", fmt_int(last7)),
            div(class = "rp-stat-lab",
                if (is.na(wk)) "in the last seven days"
                else sprintf("in the last seven days, %s%% %s than a year earlier",
                             fmt(abs(wk), 1), if (wk >= 0) "more" else "fewer"))),
        div(class = "rp-stat",
            div(class = "rp-stat-num", if (one) paste0(fmt(share, 0), "%") else fmt_int(ytd)),
            div(class = "rp-stat-lab", if (one) sprintf("of all donations in the 30 days to %s", fmt_date(end))
                                       else paste("so far in", END_YEAR))))),
    sec("scoreboard",
        h2(class = "rp-h2", "The scoreboard"),
        p(class = "rp-p", if (one) sprintf("Average daily error of each method for group %s, in donations a day. Shorter is better.", group)
                          else "Average daily error of each method, in donations per blood group per day. Shorter is better."),
        svg_bars(sc$method, sc$MAE, highlight = best, digits = 2)),
    sec("findings",
        h2(class = "rp-h2", "What stands out"),
        if (one) p(class = "rp-p", "Across all four blood groups:"),
        tags$ol(class = "rp-list", lapply(dash$findings, function(f) tags$li(f)))),
    sec("blindspot",
        h2(class = "rp-h2", "The blind spot"),
        p(class = "rp-p", sprintf(paste("Low-collection days are those at or below %s lower",
                                        "quartile from earlier years. On the %d such days in %s, the %s",
                                        "forecast too high on %s of them. No point forecast warns of a low day; the",
                                        "range-based quiet-day watch on the Live page catches more of them, with false alarms."),
                                  if (one) sprintf("group %s's", group) else "each group's",
                                  sum(low$days), lab, method_phrase(best), pct_text(over)))),
    div(class = "rp-callout",
        strong("Read with care. "),
        "These are forecasts of donations collected, not of blood demand, stock or shortage. ",
        "The evaluation is retrospective and the periods share training data. The 80% ranges on the ",
        "Live page are built from one year's errors and checked on the next, and can still be missed ",
        "in an unusual week."),
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

# --- 3. Donor card: personal ---------------------------------------------------------

# Everything a visitor can add to their card is optional. Each tailored line is
# drawn with the answers it belongs to (data-show) and hidden unless they match,
# so the page can switch lines on as answers change without sending anything.
CARD_AGE <- c("Not set" = "", "17" = "17", "18 to 60" = "18-60", "61 to 70" = "61-70", "Over 70" = "over70")
CARD_WEIGHT <- c("Not set" = "", "Under 45 kg" = "under45", "45 to 55 kg" = "45-55", "Over 55 kg" = "over55")
CARD_FIRST <- c("Not set" = "", "This is my first time" = "first", "I've donated before" = "returning")
CENTRE_STATE <- c("W.P. Kuala Lumpur", "W.P. Kuala Lumpur", "Selangor", "W.P. Putrajaya")
CARD_NOTES <- list(
  list(show = "age=17", text = "At 17, you need written consent from a parent or guardian."),
  list(show = "age=18-60", text = "Your age is within PDN's range for first-time and regular donors."),
  list(show = "age=61-70;first=first", text = "PDN takes first-time donors up to age 60, so ask PDN before you go."),
  list(show = "age=61-70;first=returning", text = "As a regular donor aged 61 to 70, you need a yearly health screening."),
  list(show = "age=61-70;first=", text = "From 61 to 70, PDN takes regular donors, with a yearly health screening."),
  list(show = "age=over70", text = "PDN lists 70 as the upper age for donors. Ask PDN if you are unsure."),
  list(show = "weight=under45", text = "PDN's minimum body weight is 45 kg."),
  list(show = "weight=45-55", text = "At 45 to 55 kg, you would give 350 ml."),
  list(show = "weight=over55", text = "Above 55 kg, you would give 450 ml."),
  list(show = "first=first", text = "First time? Bring your IC, driving licence or passport."),
  list(show = "first=returning", text = "Bring your IC, driving licence or passport, or your donor book with a photo.")
)
# Does a data-show rule ("age=61-70;first=") match the answers in `me`?
show_match <- function(rule, me) {
  all(vapply(strsplit(rule, ";", fixed = TRUE)[[1]], function(p) {
    kv <- strsplit(p, "=", fixed = TRUE)[[1]]
    v <- me[[kv[1]]]; if (is.null(v)) v <- ""
    identical(as.character(v), if (length(kv) > 1) kv[2] else "")
  }, TRUE))
}
when <- function(rule, me, tag) {
  tag <- tagAppendAttributes(tag, `data-show` = rule)
  if (!show_match(rule, me)) tag <- tagAppendAttributes(tag, hidden = NA)
  tag
}

report_card <- function(type, region, last_donation = NULL, me = list()) {
  cmp <- compatibility_for(type)
  group <- sub("[+-]$", "", type)
  next_date <- if (!is.null(last_donation) && length(last_donation) && !is.na(last_donation))
    seq(as.Date(last_donation), by = "3 months", length.out = 2)[2] else NULL
  st <- dash$states[!is.na(dash$states$region) & dash$states$region == region, ]
  st <- st[order(-st$last30), ]
  share <- 100 * panel_sum(group, DATA_END - 29, DATA_END) / max(panel_sum(ABO_GROUPS, DATA_END - 29, DATA_END), 1)
  chips <- function(on) div(class = "rp-chips",
    lapply(BLOOD_TYPES, function(x) span(class = paste("rp-chip", if (x %in% on) "is-on"), x)))
  note <- if (type == "O-") "Your red cells can go to every type: you are a universal red-cell donor."
          else if (type == "AB+") "You can receive red cells from every type: a universal red-cell recipient."
          else if (endsWith(type, "-")) "As an RhD-negative type, you can only receive RhD-negative red cells."
          else sprintf("You can receive from %d types and give to %d.", length(cmp$receives_from), length(cmp$gives_to))
  name <- if (is.null(me$name)) "" else trimws(me$name)
  notes_on <- any(vapply(CARD_NOTES, function(n) show_match(n$show, me), TRUE))
  centre <- if (is.null(me$centre)) "" else as.character(me$centre)
  report_paper(
    "card",
    div(class = "rp-card-hero",
        div(class = "rp-card-drop", span(type)),
        div(class = "rp-card-copy",
            div(class = "rp-kicker", "My donor card"),
            div(class = "rp-card-owner", `data-slot` = "name", hidden = if (!nzchar(name)) NA, name),
            h1(class = "rp-title", sprintf("Blood type %s", type)),
            p(class = "rp-deck", note))),
    p(class = "rp-group-line", icon("droplet"),
      sprintf("Group %s gave %s of the Klang Valley's recorded donations in the 30 days to %s.",
              group, paste0(fmt(share, 0), "%"), fmt_date(DATA_END))),
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
    div(class = "rp-me", `data-slot` = "notes", hidden = if (!notes_on) NA,
        h2(class = "rp-h2", "For me"),
        tags$ul(class = "rp-list", lapply(CARD_NOTES, function(n) when(n$show, me, tags$li(n$text))))),
    h2(class = "rp-h2", "Before I donate"),
    tags$ul(class = "rp-check", lapply(ELIGIBILITY, function(e)
      tags$li(HTML(sprintf("<strong>%s</strong>: %s", htmltools::htmlEscape(e$title), htmltools::htmlEscape(e$value)))))),
    h2(class = "rp-h2", sprintf("Where to donate: %s", region)),
    lapply(seq_len(nrow(st)), function(i)
      when(paste0("state=", st$state[i]), me,
           p(class = "rp-p rp-state-line", sprintf("%s recorded %s donations in the 30 days to %s.",
                                                    st$state[i], fmt_int(st$last30[i]), fmt_date(DATA_END))))),
    if (region == "Central")
      tags$ul(class = "rp-list rp-centres", lapply(seq_along(DONATION_CENTRES), function(i) {
        ct <- DONATION_CENTRES[[i]]
        tags$li(class = if (identical(centre, as.character(i))) "is-mine", `data-centre` = i,
                `data-state` = CENTRE_STATE[i],
                span(class = "rp-mine-tag", "My centre"),
                strong(ct$name), sprintf(" (%s). %s", ct$area, ct$hours))
      }))
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
