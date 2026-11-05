# Klang Valley Blood Donation Forecasts
#
# An interactive dashboard built on the BloodReady AI FIT5145 analysis. It
# forecasts *recorded blood donations* (collections) for Selangor and
# W.P. Kuala Lumpur by ABO group, using the Government of Malaysia's open
# daily data. It does not model blood demand, inventory, shortage risk or
# clinical need.
#
# Data source (set KVBD_SOURCE):
#   auto   (default) use app_data/dashboard.rds if present, else download the
#          latest CSV, else fall back to the frozen snapshot
#   live   always download the latest CSV
#   report the frozen snapshot to 2025, exactly as in the FIT5145 report
#
# Run from this folder with:  shiny::runApp()

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})
source("R/common.R")
# The full pipeline (download, cleaning, forecasting) is only needed when no
# precomputed results exist. The published site ships without it, so the
# browser never downloads readr, janitor or tidyr.
if (file.exists("R/analysis.R")) source("R/analysis.R")
source("R/content.R")
source("R/charts.R")

# Charts are drawn in the browser by plotly.js (www/plotly-basic.min.js); R
# sends each figure as JSON through this small output binding.
chartOutput <- function(id, height = "320px") {
  div(id = id, class = "kvbd-chart", style = sprintf("width:100%%;height:%s;", height))
}
renderChart <- function(expr, env = parent.frame(), quoted = FALSE) {
  func <- shiny::exprToFunction(expr, env, quoted)
  shiny::createRenderFunction(func, function(value, session, name, ...) {
    if (is.null(value)) NULL else as.character(chart_json(value))
  }, chartOutput)
}

load_dashboard <- function(source = Sys.getenv("KVBD_SOURCE", "auto")) {
  if (identical(source, "auto") && file.exists("app_data/dashboard.rds")) {
    return(readRDS("app_data/dashboard.rds"))
  }
  if (identical(source, "report")) return(build_report_data())
  build_live_data()
}

dash <- load_dashboard()
# Findings written by older data builds used other names for the methods.
dash$findings <- Reduce(function(x, r) gsub(r[1], r[2], x, fixed = TRUE),
                        list(c("full-history weekday mean", "weekday average"),
                             c("recent weekday mean", "recent weekday average"),
                             c("seven-day lag", "seven-day baseline")), dash$findings)
LATEST <- dash$latest_year
PERIOD_LABEL <- setNames(dash$periods$label, dash$periods$evaluation_year)
PERIOD_CHOICES <- setNames(as.character(dash$periods$evaluation_year), dash$periods$label)
LATEST_LABEL <- PERIOD_LABEL[[as.character(LATEST)]]
LATEST_SHORT <- sub(" \\(.*", "", LATEST_LABEL)
DATA_END <- dash$panel_summary$last_date
END_YEAR <- as.integer(format(DATA_END, "%Y"))
PARTIAL_YEAR <- if (DATA_END < as.Date(paste0(END_YEAR, "-12-31"))) END_YEAR else NULL

# --- Formatting -------------------------------------------------------------------

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_int <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_date <- function(x) format(x, "%e %b %Y") |> trimws()
pct_change <- function(now, before) if (before > 0) 100 * (now - before) / before else NA_real_

# A plain HTML table that sorts by any column and pages long results, so the
# browser does not need DataTables (and the knitr/rmarkdown stack behind it).
sort_table <- function(df, digits_cols = character(), digits = 2, page_length = 8) {
  num <- vapply(df, is.numeric, TRUE)
  cell <- function(v, col) {
    if (!num[[col]]) return(tags$td(v))
    shown <- if (col %in% digits_cols) fmt(v, digits) else fmt_int(v)
    tags$td(class = "num", `data-v` = format(v, scientific = FALSE, trim = TRUE), shown)
  }
  div(class = "st-wrap", `data-page` = page_length,
      tags$table(class = "st-table",
        tags$thead(tags$tr(lapply(names(df), function(n)
          tags$th(class = if (num[[n]]) "num" else NULL, tabindex = "0", `aria-sort` = "none",
                  span(n), span(class = "st-arrow", `aria-hidden` = "true"))))),
        tags$tbody(lapply(seq_len(nrow(df)), function(i)
          tags$tr(lapply(names(df), function(n) cell(df[[n]][i], n)))))),
      div(class = "st-foot"))
}

source_link <- function(src) a(src$title, href = src$url, target = "_blank", rel = "noopener")

# --- The story, computed from the results ----------------------------------------------

latest_scores <- dash$scores_overall[dash$scores_overall$evaluation_year == LATEST, ]
BEST_METHOD <- latest_scores$method[which.min(latest_scores$MAE)]
BEST_MAE <- min(latest_scores$MAE)
LAG_MAE <- latest_scores$MAE[latest_scores$method == "Seven-day baseline"]
BEST_GAIN <- 100 * (LAG_MAE - BEST_MAE) / LAG_MAE

low_best <- dash$low_errors[dash$low_errors$evaluation_year == LATEST &
                              dash$low_errors$day_category == "Low collection" &
                              dash$low_errors$method == BEST_METHOD, ]
LOW_OVER_PCT <- sum(low_best$days * low_best$overprediction_percent) / sum(low_best$days)
LOW_DAYS <- sum(low_best$days)
other_best <- dash$low_errors[dash$low_errors$evaluation_year == LATEST &
                                dash$low_errors$day_category == "Other days" &
                                dash$low_errors$method == BEST_METHOD, ]
LOW_MAE <- sum(low_best$days * low_best$MAE) / sum(low_best$days)
OTHER_MAE <- sum(other_best$days * other_best$MAE) / sum(other_best$days)

daily_totals <- group_stat(dash$panel, "date", "donations", sum, "total")
daily_totals$weekday <- WEEKDAYS[weekday_num(daily_totals$date)]
weekday_totals <- group_stat(daily_totals, "weekday", "total", mean, "mean_total")
BUSIEST_DAY <- weekday_totals$weekday[which.max(weekday_totals$mean_total)]
QUIETEST_DAY <- weekday_totals$weekday[which.min(weekday_totals$mean_total)]
BUSY_RATIO <- max(weekday_totals$mean_total) / min(weekday_totals$mean_total)

mix_year <- if (is.null(PARTIAL_YEAR)) END_YEAR else END_YEAR - 1L
mix_latest <- dash$blood_mix[dash$blood_mix$year == mix_year, ]
O_SHARE <- 100 * mix_latest$share[mix_latest$blood_type == "O"]

pct_text <- function(x) paste0(fmt(x, 0), "%")
low_rows <- dash$low_errors[dash$low_errors$evaluation_year == LATEST &
                              dash$low_errors$day_category == "Low collection", ]
low_all <- data.frame(method = unique(low_rows$method))
low_all$over <- vapply(low_all$method, function(m) {
  r <- low_rows[low_rows$method == m, ]
  sum(r$days * r$overprediction_percent) / sum(r$days)
}, 0)
LOW_OVER_BARS <- setNames(low_all$over, c("Seven-day baseline" = "7-day", "Weekday average" = "Wkday",
                                           "Linear regression" = "Regr.", "Recent weekday average" = "Recent")[low_all$method])
LOW_HEADLINE <- if (all(low_all$over == 100)) {
  "On the quietest days, every forecast aims too high."
} else if (all(low_all$over > 50)) {
  "On the quietest days, the forecasts aim too high."
} else "The quietest days are the hardest to forecast."
mp <- dash$min_predictions[dash$min_predictions$evaluation_year == LATEST, ]
LOW_WHY <- sprintf(paste("Why? For %d of the %d method and blood-group pairs, even the smallest",
                         "forecast of the period sits above the low-day threshold, so a day that",
                         "quiet could never be predicted."),
                   sum(mp$all_predictions_above_cutoff), nrow(mp))
o_range <- range(100 * dash$blood_mix$share[dash$blood_mix$blood_type == "O"])
MIX_LINE <- if (diff(o_range) < 5) {
  sprintf("And who gives? The mix barely moves: group O has made up %s to %s of donations every year.",
          pct_text(o_range[1]), pct_text(o_range[2]))
} else "And who gives? The mix of blood groups shifts only slowly from year to year."

wk <- pct_change(dash$latest$last7, dash$latest$last7_prev_year)
# "yesterday" when the data is a day old (the usual case), otherwise the date.
DAY_PHRASE <- local({
  lag_days <- as.integer(Sys.Date() - DATA_END)
  if (identical(lag_days, 0L)) "today" else if (identical(lag_days, 1L)) "yesterday"
  else paste("on", gsub("  ", " ", format(DATA_END, "%A, %e %B")))
})
signed_span <- function(x, digits = 1, suffix = "%") {
  if (is.na(x)) return(span("n/a"))
  count_span(abs(x), digits, prefix = if (x >= 0) "+" else "\u2212", suffix = suffix)
}
WEEK_CHANGE <- if (is.na(wk)) "n/a" else paste0(if (wk >= 0) "+" else "\u2212", fmt(abs(wk), 1), "%")

LIMITS_MAP <- list(
  list(title = "Tested on the past", sub = "Scored on days that already happened; the methods were chosen using 2006–2025, so older years aren't a fresh test"),
  list(title = "Overlapping tests", sub = "Test years share their past data, so they aren't fully separate checks"),
  list(title = "3-year window untested", sub = "Three years for the recent weekday average was not compared with other window lengths"),
  list(title = "No outside factors", sub = "No weather, holidays, campaigns, blood stock or hospital demand, and no ranges of uncertainty"),
  list(title = "Zero days kept", sub = "Days with no donations may be closures or reporting gaps"),
  list(title = "Data can change", sub = "Official figures can be revised between daily refreshes"))

wk_line <- function(now, before, what) {
  ch <- pct_change(now, before)
  if (is.na(ch)) return(what)
  sprintf("%s%s%% on %s", if (ch >= 0) "+" else "\u2212", fmt(abs(ch), 1), what)
}
WEEK_LINE <- wk_line(dash$latest$last7, dash$latest$last7_prev_year, "last year")
YTD_LINE <- wk_line(dash$latest$ytd, dash$latest$ytd_prev_year, "last year")
last7 <- utils::tail(dash$latest$daily[order(dash$latest$daily$date), ], 7)
LAST7_BARS <- setNames(last7$total, format(last7$date, "%a"))
NEXT_TOTALS <- group_stat(dash$next_days[dash$next_days$method == "Recent weekday average", ],
                          "date", "prediction", sum, "total")
NEXT_TOTALS <- NEXT_TOTALS[order(NEXT_TOTALS$date), ]

method_phrase <- function(m) c("Seven-day baseline" = "seven-day baseline",
                               "Weekday average" = "weekday average",
                               "Linear regression" = "linear regression",
                               "Recent weekday average" = "recent weekday average")[[m]]
change_words <- function(now, before) {
  ch <- pct_change(now, before)
  if (is.na(ch)) return("")
  sprintf("%s%% %s than the same week last year", fmt(abs(ch), 1), if (ch >= 0) "more" else "fewer")
}

# --- Building blocks ----------------------------------------------------------------

# Numbers that count up when their page opens (see www/interactions.js).
# A number that counts up when it comes into view. `burst` adds a small
# splash of drops as it lands, for the headline numbers.
count_span <- function(x, digits = 0, prefix = "", suffix = "", burst = FALSE, delay = 0) {
  span(class = "countup", `data-count` = x, `data-decimals` = digits,
       `data-prefix` = if (nzchar(prefix)) prefix, `data-suffix` = if (nzchar(suffix)) suffix,
       `data-burst` = if (burst) "true", `data-delay` = if (delay > 0) delay,
       paste0(prefix, if (digits == 0) fmt_int(x) else fmt(x, digits), suffix), .noWS = "outside")
}
# A friendly line that types in word by word before the number it introduces.
lead_in <- function(...) p(class = "lead-in", ...)

# Highlight the terms a reader should remember: method names and key phrases.
KEY_TERMS <- c(METHODS, "recent weekday average", "linear regression", "seven-day baseline",
               "weekday average", "low-collection days", "too high on every one", "most accurate method")
emphasise <- function(text) {
  pattern <- paste0("(", paste(unique(KEY_TERMS), collapse = "|"), ")")
  parts <- regmatches(text, gregexpr(pattern, text, ignore.case = TRUE), invert = NA)[[1]]
  tagList(lapply(seq_along(parts), function(i) if (i %% 2 == 0) tags$mark(parts[i]) else parts[i]))
}

# A liquid wave that sits at the bottom of a card (decorative).
wave <- function(class = "") {
  tags$svg(class = paste("wave", class), viewBox = "0 0 1200 120", preserveAspectRatio = "none",
           `aria-hidden` = "true",
           tags$path(class = "wave-back",
                     d = "M0 60 C150 20 300 100 450 60 S750 20 900 60 S1150 100 1200 60 V120 H0 Z"),
           tags$path(class = "wave-front",
                     d = "M0 75 C200 40 350 110 600 75 S1000 40 1200 75 V120 H0 Z"))
}

# Seven small columns for the last seven days, busiest day labelled.
mini_cols <- function(values) {
  mx <- max(values, 1); top <- which.max(values)
  div(class = "mini-cols",
      lapply(seq_along(values), function(i)
        div(class = paste("mc", if (i == top) "is-top"), title = paste(names(values)[i], fmt_int(values[[i]])),
            style = sprintf("--h:%.0f%%;--i:%d", 100 * values[[i]] / mx, i - 1),
            span(class = "mc-val", if (i == top) fmt_int(values[[i]]) else ""),
            span(class = "mc-bar"),
            span(class = "mc-lab", substr(names(values)[i], 1, 2)))))
}

# Horizontal mini bars used on card backs.
mini_bars <- function(values, digits = 0, highlight = NULL) {
  mx <- max(values, 1)
  div(class = "mini-bars",
      lapply(names(values), function(g)
        div(class = paste("mini-bar", if (identical(g, highlight)) "is-key"),
            span(class = "mb-label", g),
            div(class = "mb-track", div(class = "mb-fill",
                                        style = sprintf("--w:%.1f%%", 100 * values[[g]] / mx))),
            span(class = "mb-val", if (digits == 0) fmt_int(values[[g]]) else fmt(values[[g]], digits)))))
}

# A stat card that flips (click, Enter or Space) to show what lies behind
# its number. The back face has its own content and a liquid wave.
flip_card <- function(label, value, unit, back_title, back_body, tone = "", badge = NULL) {
  div(
    class = paste("flip-card", tone), tabindex = "0", role = "button",
    `aria-pressed` = "false",
    `aria-label` = paste(label, "- press to see the breakdown"),
    div(class = "flip-inner",
        div(class = "flip-face flip-front",
            div(class = "flip-top",
                div(class = "flip-label", label),
                if (!is.null(badge)) span(class = "flip-badge", badge)),
            div(class = "flip-value", value),
            div(class = "flip-foot",
                span(class = "flip-unit", unit),
                span(class = "flip-hint", `aria-hidden` = "true", title = "Turn over", icon("rotate")))),
        div(class = "flip-face flip-back", `aria-hidden` = "true",
            div(class = "flip-back-title", back_title),
            back_body,
            wave("wave-card")))
  )
}

latest_mae <- function(m) {
  s <- dash$scores_overall
  s$MAE[s$evaluation_year == LATEST & s$method == m]
}
method_rank <- function(m) {
  s <- dash$scores_overall[dash$scores_overall$evaluation_year == LATEST, ]
  rank(s$MAE)[s$method == m]
}
group_mae <- function(m) {
  s <- dash$scores_by_group[dash$scores_by_group$evaluation_year == LATEST &
                              dash$scores_by_group$method == m, ]
  setNames(s$MAE, s$blood_type)[ABO_GROUPS]
}
rank_badge <- function(m) paste0("#", method_rank(m), " of 4")
method_back <- function(m, text) {
  tagList(
    p(class = "flip-back-text",
      paste0(sprintf("Ranked %s of 4 for %s. ", c("1st", "2nd", "3rd", "4th")[method_rank(m)],
                     LATEST_LABEL), text)),
    div(class = "flip-back-sub", "Average error by blood group"),
    mini_bars(group_mae(m), digits = 1))
}
latest_groups <- setNames(dash$latest$latest_by_group$donations,
                          dash$latest$latest_by_group$blood_type)[ABO_GROUPS]

# Segmented control (single choice) and toggle chips (multiple choice).
seg <- function(id, label, choices, selected, class = "") {
  div(class = paste("control", class),
      span(class = "control-label", label),
      radioButtons(id, NULL, choices, selected = selected, inline = TRUE) |>
        tagAppendAttributes(class = "seg"))
}
# Chips for methods and blood groups wear the colour (and, for methods, the line
# style) each one has in the charts, so the controls double as the legend.
CHART_DARK_COL <- c("#F2C2CC" = "#6B3A48", "#E3909F" = "#9C4A60", "#C45A72" = "#CE5F7B", "#7D2440" = "#F48AA2",
                    "#F2B3C0" = "#7A3F50", "#E07D93" = "#A84F66", "#B8445F" = "#D06580", "#6E1D37" = "#F4A3B6")
DASH_ARRAY <- c(dot = "1.5 3.5", dash = "5 3", longdash = "9 3", solid = "none")
chip_key <- function(value) {
  col <- if (value %in% names(METHOD_COL)) METHOD_COL[[value]] else if (value %in% names(GROUP_COL)) GROUP_COL[[value]] else NULL
  if (is.null(col)) return(NULL)
  style <- sprintf("--k:%s;--kd:%s;", col, CHART_DARK_COL[[col]])
  if (value %in% names(METHOD_DASH)) {
    span(class = "chip-key key-line", style = style, `aria-hidden` = "true",
         HTML(sprintf('<svg viewBox="0 0 24 10" width="24" height="10"><line x1="2" y1="5" x2="22" y2="5" stroke-dasharray="%s"/></svg>',
                      DASH_ARRAY[[METHOD_DASH[[value]]]])))
  } else {
    span(class = "chip-key key-drop", style = style, `aria-hidden` = "true",
         HTML('<svg viewBox="0 0 12 14" width="12" height="14"><path d="M6 1C6 1 1.5 6.4 1.5 9.1A4.5 4.5 0 0 0 10.5 9.1C10.5 6.4 6 1 6 1Z"/></svg>'))
  }
}
chips <- function(id, label, choices, selected) {
  values <- unname(choices)
  labels <- if (is.null(names(choices))) values else names(choices)
  keyed <- any(values %in% c(names(METHOD_COL), names(GROUP_COL)))
  # Only chips with a chart colour get the legend key; the rest stay plain labels.
  names_html <- if (keyed) lapply(seq_along(values), function(i)
    tagList(chip_key(values[i]), span(class = "chip-text", labels[i]))) else as.list(labels)
  div(class = "control",
      span(class = "control-label", label),
      checkboxGroupInput(id, NULL, choiceNames = names_html, choiceValues = values,
                         selected = selected, inline = TRUE) |>
        tagAppendAttributes(class = paste("chips", if (keyed) "chips-keyed")))
}

# Each page opens like a chapter: its number and name, then the finding as
# the headline (computed from the data), then what it means.
chapter_head <- function(id, headline, subtitle = NULL, stat = NULL) {
  sct <- SECTIONS[[match(id, vapply(SECTIONS, `[[`, "", "id"))]]
  div(class = "chapter-head stage-1",
      div(class = "chapter-copy",
          div(class = "chapter-eyebrow",
              span(class = "chapter-badge", icon(sct$icon)),
              span(class = "chapter-name", sct$label),
              if (id %in% LIVE_PAGES) status_pill),
          h2(class = "chapter-title", headline),
          if (!is.null(subtitle)) p(class = "chapter-sub", subtitle)),
      chapter_art(id),
      if (!is.null(stat)) div(class = "chapter-stat", stat))
}

# A soft aurora of drifting colour behind each chapter headline (decorative). It
# fades into the page canvas, so it reads as part of the background.
chapter_art <- function(id) {
  ribbon <- function(cls, y, amp) {
    d <- sprintf("M-200 %d C 0 %d, 200 %d, 400 %d S 800 %d, 1000 %d S 1400 %d, 1600 %d",
                 y, y - amp, y + amp, y, y - amp, y, y + amp, y)
    tags$path(class = cls, d = d)
  }
  div(class = paste("aurora", paste0("aurora-", id)), `aria-hidden` = "true",
      div(class = "au au1"), div(class = "au au2"), div(class = "au au3"), div(class = "au au4"),
      tags$svg(class = "silk", viewBox = "0 0 1200 300", preserveAspectRatio = "none",
               ribbon("s1", 150, 70), ribbon("s2", 170, 50), ribbon("s3", 130, 90),
               ribbon("s4", 190, 40)))
}
story_card <- function(target, title, value, text) {
  sct <- SECTIONS[[match(target, vapply(SECTIONS, `[[`, "", "id"))]]
  tags$button(class = "story-card", type = "button", role = "listitem", `data-target` = target,
              span(class = "story-icon", icon(sct$icon)),
              span(class = "story-title", title),
              span(class = "story-value", value),
              span(class = "story-text", text),
              span(class = "story-go", icon("arrow-right")))
}
# A headline number with its label and, where there is one, the comparison that
# says whether the number is good: a stakeholder reads the verdict, not just the figure.
# A headline number with what it is measured against: two short bars (this
# result and its reference) and a verdict coloured green when it is good news
# and amber when it is not.
big_stat <- function(value, label, bars = NULL, verdict = NULL, tone = "good") {
  top <- if (length(bars)) max(vapply(bars, function(b) b$value, 0), 1e-9) else 1
  tagList(
    div(class = "big-stat-value", value), div(class = "big-stat-label", label),
    if (length(bars)) div(class = "bs-bars", lapply(bars, function(b)
      div(class = paste("bs-bar", b$kind),
          span(class = "bs-name", b$name),
          span(class = "bs-track", span(class = "bs-fill", style = sprintf("--w:%.1f%%;", 100 * b$value / top))),
          span(class = "bs-val", b$text)))),
    if (!is.null(verdict)) div(class = paste("big-stat-compare", paste0("tone-", tone)),
                               icon(if (tone == "good") "circle-check" else "triangle-exclamation"), verdict))
}
# A word with a dotted underline that explains itself on hover, focus or tap.
term <- function(text, key) {
  g <- GLOSSARY[[key]]
  span(class = "term", tabindex = "0", role = "button", `data-term` = g[1], `data-def` = g[2], text,
       .noWS = "outside")
}

# A line of narration that carries the reader from one panel to the next.
narrate <- function(...) p(class = "narration stage-3", ...)

# Tabs: one tab bar, one pane showing at a time, so a page reads as a few
# short views instead of one long scroll.
tab <- function(label, ..., icon = NULL) list(label = label, icon = icon, content = tagList(...))
tab_set <- function(id, tabs, class = "") {
  div(class = paste("kv-tabset", class), id = id,
      div(class = "kv-tabs", role = "tablist",
          lapply(seq_along(tabs), function(i)
            tags$button(type = "button", class = paste("kv-tab", if (i == 1) "is-on"), role = "tab",
                        `aria-selected` = if (i == 1) "true" else "false", `data-tab` = i,
                        if (!is.null(tabs[[i]]$icon)) icon(tabs[[i]]$icon),
                        span(tabs[[i]]$label)))),
      lapply(seq_along(tabs), function(i)
        div(class = paste("kv-pane", if (i == 1) "is-on"), role = "tabpanel", `data-pane` = i,
            hidden = if (i != 1) NA, tabs[[i]]$content)))
}

# A fold: a quiet button that opens to show detail kept out of the way.
fold <- function(label, ..., open = FALSE) {
  tags$details(class = "fold", open = if (open) NA,
               tags$summary(span(class = "fold-label", label), span(class = "fold-chev", icon("chevron-down"))),
               div(class = "fold-body", ...))
}

# A scene that holds several tiles: the same white panel as every other scene,
# so each page reads as one storyboard of numbered scenes.
section <- function(title, ..., sub = NULL, class = "stage-3") {
  tags$section(class = paste("panel block", class),
               div(class = "panel-head",
                   div(class = "panel-titles",
                       h3(class = "panel-title", title),
                       if (!is.null(sub)) p(class = "panel-sub", sub))),
               div(class = "panel-body", ...))
}

# The end of each chapter points to the next one, so the pages read in order.
chapter_nav <- function(id) {
  ids <- vapply(SECTIONS, `[[`, "", "id")
  i <- match(id, ids)
  nxt <- if (i < length(SECTIONS)) SECTIONS[[i + 1]] else SECTIONS[[1]]
  div(class = "chapter-nav",
      tags$button(class = "link-btn chapter-sources", type = "button", `data-open` = "sources",
                  icon("book-open"), "Sources & notes"),
      tags$button(class = "chapter-next", type = "button", `data-target` = nxt$id,
                  span(class = "cn-label", if (i < length(SECTIONS)) "Next" else "Back to the start"),
                  span(class = "cn-name", nxt$label), icon("arrow-right")))
}

# A panel: white card with a proper title row and optional controls.
# "How to read this" as a small chip; the explanation opens on hover, focus or tap.
how_read <- function(text) {
  tags$details(class = "how-read",
               tags$summary(`aria-label` = "How to read this", span(class = "hr-icon", icon("circle-question")),
                            span(class = "hr-label", "How to read this")),
               div(class = "how-read-pop", role = "note", text))
}

panel <- function(title, ..., sub = NULL, controls = NULL, class = "", footer = NULL, read = NULL) {
  div(class = paste("panel", class),
      div(class = "panel-head",
          div(class = "panel-titles",
              div(class = "panel-title-row",
                  h3(class = "panel-title", title),
                  if (!is.null(read)) how_read(read)),
              if (!is.null(sub)) p(class = "panel-sub", sub)),
          if (!is.null(controls)) div(class = "panel-controls", controls)),
      div(class = "panel-body", ...),
      if (!is.null(footer)) div(class = "panel-foot", footer))
}

kpi <- function(label, value_ui, detail_ui = NULL, class = "") {
  div(class = paste("kpi", class),
      div(class = "kpi-label", label),
      div(class = "kpi-value", value_ui),
      if (!is.null(detail_ui)) div(class = "kpi-detail", detail_ui),
      if (grepl("kpi-hero", class)) wave("wave-hero"))
}

ascii_drop <- function(width = 30, height = 22) {
  ramp <- strsplit(" .:-=+*%#@", "")[[1]]
  rows <- character(height)
  for (y in seq_len(height)) {
    line <- character(width)
    for (x in seq_len(width)) {
      u <- (x - (width + 1) / 2) / (width / 2)
      v <- (y - 1) / (height - 1)
      # Teardrop: a round bottom that narrows to a point at the top.
      cy <- 0.52
      inside <- if (v >= cy) u^2 + ((v - cy) / (1.03 - cy))^2 <= 1
                else abs(u) <= (v / cy)^1.45
      if (!isTRUE(inside)) { line[x] <- " "; next }
      shade <- 0.55 + 0.45 * (-(u * 0.6) - (v - 0.6) * 0.7)
      idx <- max(2, min(length(ramp), round(shade * (length(ramp) - 1)) + 1))
      line[x] <- ramp[idx]
    }
    rows[y] <- paste(line, collapse = "")
  }
  tags$pre(class = "ascii-drop", `aria-hidden` = "true", paste(rows, collapse = "\n"))
}

# label: the full name (page headings, hover, screen readers). short: the word
# under the icon in the menu.
SECTIONS <- list(
  list(id = "overview", label = "Overview", short = "Overview", icon = "droplet"),
  list(id = "live", label = "Live tracker", short = "Live", icon = "tower-broadcast"),
  list(id = "compare", label = "Compare forecasts", short = "Compare", icon = "scale-balanced"),
  list(id = "low", label = "Quiet days", short = "Quiet days", icon = "arrow-trend-down"),
  list(id = "patterns", label = "Patterns", short = "Patterns", icon = "chart-line"),
  list(id = "blood101", label = "Donating blood", short = "Donate", icon = "hand-holding-medical"),
  list(id = "about", label = "About the data", short = "About", icon = "circle-info"),
  list(id = "export", label = "Reports", short = "Reports", icon = "file-lines")
)

theme_button <- function(class = "rail-item theme-toggle") {
  tags$button(class = class, type = "button", `aria-label` = "Switch light or dark theme",
              title = "Light or dark", `aria-pressed` = "false",
              span(class = "rail-icon", span(class = "ti-sun", icon("circle-half-stroke")), span(class = "ti-moon", icon("circle-half-stroke"))),
              span(class = "rail-label", span(class = "ti-text-light", "Dark theme"),
                   span(class = "ti-text-dark", "Light theme")))
}
rail <- tags$nav(
  class = "rail", id = "rail", `aria-label` = "Sections",
  div(class = "rail-items",
      div(class = "rail-indicator", `aria-hidden` = "true"),
      lapply(seq_along(SECTIONS), function(i) {
        sct <- SECTIONS[[i]]
        tags$button(
          class = paste("rail-item", if (i == 1) "is-active"), type = "button",
          `data-target` = sct$id, `aria-label` = sct$label, title = sct$label,
          `aria-current` = if (i == 1) "page" else NULL,
          span(class = "rail-icon", icon(sct$icon)),
          span(class = "rail-label", sct$short))
      })),
  theme_button()
)
# Phones: the rail tucks into a tab on the screen edge and slides out when tapped.
# The tab can be dragged up or down so it never sits over something you are reading.
rail_tab <- tagList(
  div(class = "rail-scrim", `aria-hidden` = "true"),
  tags$button(class = "rail-tab", type = "button", `aria-label` = "Open the menu",
              `aria-expanded` = "false", `aria-controls` = "rail",
              span(class = "rt-icon", `aria-hidden` = "true", icon(SECTIONS[[1]]$icon)),
              span(class = "rt-grip", `aria-hidden` = "true")))


# Where the data stands: a heartbeat for "live", the last recorded day on a
# little calendar tile, and when the site last refreshed. Shown on every page
# that is drawn from the daily counts.
status_pill <- div(
  class = "status-pill", role = "status",
  `aria-label` = paste0("Live data to ", fmt_date(DATA_END), ". Updated ",
                        trimws(format(dash$built, "%e %b"))),
  span(class = "sp-beat", `aria-hidden` = "true",
       HTML('<svg viewBox="0 0 36 18" width="30" height="15"><path class="sp-ecg-bg" d="M1 10H9L11.5 5L14.5 15L18 2L21 13L23 10H35"/><path class="sp-ecg" d="M1 10H9L11.5 5L14.5 15L18 2L21 13L23 10H35"/></svg>')),
  span(class = "sp-cal", `aria-hidden` = "true",
       span(class = "sp-mon", toupper(format(DATA_END, "%b"))),
       span(class = "sp-day", as.integer(format(DATA_END, "%d")))),
  span(class = "sp-copy", `aria-hidden` = "true",
       span(class = "sp-kicker", "Live data to"),
       span(class = "sp-date", fmt_date(DATA_END),
            span(class = "sp-ago", `data-day` = format(DATA_END, "%Y-%m-%d")))),
  span(class = "sp-fresh", `aria-hidden` = "true", icon("rotate"),
       paste0("Updated ", trimws(format(dash$built, "%e %b"))))
)
LIVE_PAGES <- c("overview", "live", "compare", "low", "patterns")


# --- Blood 101 views ----------------------------------------------------------------

# Red-cell flow for one blood type: donors it can receive from on the left,
# recipients it can give to on the right, joined through the chosen type.
compat_flow <- function(t) {
  cmp <- compatibility_for(t)
  W <- 720; H <- 400; cx <- W / 2; cy <- H / 2
  ys <- seq(36, H - 36, length.out = length(BLOOD_TYPES))
  node <- function(x, y, type, on, side) {
    tags$g(class = paste("cf-node", side, if (on) "is-on" else "is-off"),
           transform = sprintf("translate(%.1f %.1f)", x, y),
           tags$path(d = "M0 -17 C0 -17 -12 -3 -12 5 a12 12 0 0 0 24 0 C12 -3 0 -17 0 -17 Z"),
           tags$text(x = if (side == "left") -22 else 22, y = 5,
                     `text-anchor` = if (side == "left") "end" else "start", type))
  }
  link <- function(x1, y1, x2, y2, i, cls) {
    mx <- (x1 + x2) / 2
    tags$path(class = paste("cf-link", cls), style = sprintf("--d:%dms", 90 * i),
              d = sprintf("M%.1f %.1f C%.1f %.1f %.1f %.1f %.1f %.1f", x1, y1, mx, y1, mx, y2, x2, y2))
  }
  lx <- 120; rx <- W - 120
  ins <- which(BLOOD_TYPES %in% cmp$receives_from)
  outs <- which(BLOOD_TYPES %in% cmp$gives_to)
  note <- if (t == "O-") {
    "O- red cells can go to every type, which is why O- is called the universal red-cell donor."
  } else if (t == "AB+") {
    "AB+ can receive red cells from every type: the universal red-cell recipient."
  } else if (endsWith(t, "-")) {
    sprintf("RhD-negative types like %s can only receive RhD-negative red cells.", t)
  } else {
    sprintf("%s can receive from %d types and give to %d.", t, length(ins), length(outs))
  }
  div(class = "cf",
      tags$svg(class = "cf-svg", viewBox = sprintf("0 -6 %d %d", W, H + 56), role = "img",
               `aria-label` = sprintf("%s receives red cells from %s and gives to %s", t,
                                      paste(cmp$receives_from, collapse = ", "),
                                      paste(cmp$gives_to, collapse = ", ")),
               tags$text(class = "cf-head", x = lx, y = 14, `text-anchor` = "middle", "Receives from"),
               tags$text(class = "cf-head", x = rx, y = 14, `text-anchor` = "middle", "Gives to"),
               tags$text(class = "cf-count in", x = lx, y = H + 34, `text-anchor` = "middle",
                         tags$tspan(class = "cf-count-num", length(ins)), " of 8 types"),
               tags$text(class = "cf-count out", x = rx, y = H + 34, `text-anchor` = "middle",
                         tags$tspan(class = "cf-count-num", length(outs)), " of 8 types"),
               tags$path(class = "cf-stem", d = sprintf("M%.1f %.1f V%.1f", cx, cy + 46, H + 44)),
               lapply(seq_along(ins), function(k) link(lx + 14, ys[ins[k]], cx - 46, cy, k, "in")),
               lapply(seq_along(outs), function(k) link(cx + 46, cy, rx - 14, ys[outs[k]], k + length(ins), "out")),
               lapply(seq_along(BLOOD_TYPES), function(i)
                 node(lx, ys[i], BLOOD_TYPES[i], i %in% ins, "left")),
               lapply(seq_along(BLOOD_TYPES), function(i)
                 node(rx, ys[i], BLOOD_TYPES[i], i %in% outs, "right")),
               tags$g(class = "cf-centre", transform = sprintf("translate(%.1f %.1f)", cx, cy),
                      tags$circle(class = "cf-halo", r = 58),
                      tags$path(d = "M0 -48 C0 -48 -34 -8 -34 14 a34 34 0 0 0 68 0 C34 -8 0 -48 0 -48 Z"),
                      tags$text(y = 22, `text-anchor` = "middle", t))),
      div(class = "cf-phone",
          div(class = "cf-row", span(class = "cf-row-h", "Receives from"),
              div(class = "cf-chips", lapply(BLOOD_TYPES, function(b)
                span(class = paste("cf-chip", if (b %in% cmp$receives_from) "is-on"), b)))),
          div(class = "cf-row", span(class = "cf-row-h", "Gives to"),
              div(class = "cf-chips", lapply(BLOOD_TYPES, function(b)
                span(class = paste("cf-chip", if (b %in% cmp$gives_to) "is-on"), b))))),
      p(class = "cf-note", note))
}

# Hours as a bar across the day (6 am to 10 pm), for the centre cards.
hours_track <- function(h, label) {
  lo <- 6; hi <- 22
  span(class = "ht-row",
       span(class = "ht-label", label),
       span(class = "ht-track",
            span(class = "ht-open", style = sprintf("--a:%.2f%%;--b:%.2f%%;",
                                                    100 * (h[1] - lo) / (hi - lo), 100 * (h[2] - lo) / (hi - lo)))))
}
region_view <- function(r) {
  st <- dash$states[!is.na(dash$states$region) & dash$states$region == r, ]
  st <- st[order(-st$last30), ]
  tot <- max(sum(st$last30), 1)
  share <- 100 * st$last30 / tot
  ring <- function(p) HTML(sprintf(
    '<svg viewBox="0 0 44 44" width="56" height="56" aria-hidden="true"><circle class="ring-bg" cx="22" cy="22" r="18"/><circle class="ring-fg" cx="22" cy="22" r="18" pathLength="100" style="--p:%.1f"/></svg>', p))
  tagList(
    what_shows(text = explain_region(st, r)),
    div(class = "share-bar", role = "img",
        `aria-label` = paste(sprintf("%s %s", st$state, fmt_pct(share)), collapse = ", "),
        lapply(seq_len(nrow(st)), function(i)
          span(class = "share-seg", style = sprintf("--w:%.2f%%;--i:%d;", share[i], i - 1),
               if (share[i] >= 9) span(class = "share-lab", span(class = "share-pct", fmt_pct(share[i])),
                                       span(class = "share-name", st$state[i]))))),
    p(class = "share-cap", paste("Share of the region's donations in the 30 days to", fmt_date(DATA_END))),
    div(class = "state-grid",
        lapply(seq_len(nrow(st)), function(i)
          div(class = "state-card", style = sprintf("--i:%d", i - 1),
              div(class = "state-ring", ring(share[i]), span(class = "state-pct", fmt_pct(share[i]))),
              div(class = "state-copy",
                  div(class = "state-name", st$state[i]),
                  div(class = "state-num", fmt_int(st$last30[i]), span(class = "state-unit", "in 30 days")),
                  div(class = "state-ytd", sprintf("%s so far in %s", fmt_int(st$ytd[i]), END_YEAR)))))),
    if (r == "Central") tagList(
      p(class = "region-note", icon("circle-info"),
        paste("Counts are by the state where each donation was recorded, not where donors live.",
              "Kuala Lumpur is home to Pusat Darah Negara, which may explain part of its higher count.")),
      div(class = "centre-headrow",
          h4(class = "centre-head", "PDN's fixed donation centres"),
          span(class = "centre-key", span(class = "ck-open"), "Opening hours, 6 am to 10 pm")),
      div(class = "centre-grid",
          lapply(DONATION_CENTRES, function(ct)
            div(class = "centre", `data-wk` = paste(ct$wk, collapse = ","), `data-we` = paste(ct$we, collapse = ","),
                div(class = "centre-top",
                    span(class = "centre-icon", icon("location-dot")),
                    span(class = "centre-status", `aria-live` = "polite")),
                div(class = "centre-name", ct$name),
                div(class = "centre-area", ct$area),
                div(class = "centre-hours-vis",
                    hours_track(ct$wk, "Mon–Fri"),
                    hours_track(ct$we, "Sat–Sun"),
                    span(class = "ht-now", `aria-hidden` = "true")),
                div(class = "centre-hours", ct$hours),
                tags$a(class = "centre-map", target = "_blank", rel = "noopener",
                       href = paste0("https://www.google.com/maps/search/?api=1&query=",
                                     utils::URLencode(paste(ct$name, ct$area), reserved = TRUE)),
                       icon("map-location-dot"), "Open in Maps")))),
      p(class = "viz-note centre-foot", "Open or closed is worked out from Malaysia time and the hours above.",
        "Public holidays are not included, so check PDN before you go."))
    else p(class = "region-note", icon("circle-info"),
           paste("PDN's list of fixed donation centres covers the Klang Valley only. Elsewhere,",
                 "the donations above were collected at Ministry of Health blood collection sites",
                 "and mobile campaigns; check PDN or the nearest state hospital for times and",
                 "locations.")))
}

source("R/reports.R", local = TRUE)
source("R/visuals.R", local = TRUE)
source("R/explain.R", local = TRUE)

# "What this shows": one plain sentence under a chart that rewrites itself for the selection.
# Native range controls: the page is a static site, so no Shiny widgets.
date_range <- function(id, start, end, min, max) {
  div(class = "input-daterange", id = id, role = "group", `aria-label` = "Date range",
      tags$input(type = "date", class = "form-control", id = paste0(id, "_from"), value = format(start),
                 min = format(min), max = format(max), `aria-label` = "From"),
      span(class = "input-group-addon", "to"),
      tags$input(type = "date", class = "form-control", id = paste0(id, "_to"), value = format(end),
                 min = format(min), max = format(max), `aria-label` = "To"))
}
year_range <- function(id, from, to, min, max) {
  yrs <- as.character(seq(min, max))
  sel <- function(suffix, value, label)
    tags$select(class = "year-select", id = paste0(id, suffix), `aria-label` = label,
                lapply(yrs, function(y) tags$option(value = y, selected = if (y == as.character(value)) NA, y)))
  div(class = "year-range", id = id, role = "group", `aria-label` = "Years",
      sel("_from", from, "From year"), span(class = "input-group-addon", "to"), sel("_to", to, "To year"))
}

# A chart with its plain-words reading beside it (stacked on narrow screens).
chart_row <- function(note, chart)
  div(class = "chart-row", div(class = "chart-main", chart), note)

what_shows <- function(id = NULL, text = NULL) {
  div(class = "what-shows",
      span(class = "ws-tag", icon("lightbulb"), "What this shows"),
      if (!is.null(id)) uiOutput(id, inline = TRUE, class = "ws-text") else span(class = "ws-text", text))
}

# A first-visit card: three things a newcomer would not discover on their own.
# It floats over a softly blurred page, so it never reads as one more content card.
welcome_card <- div(
  class = "welcome-layer", id = "welcome", hidden = NA,
  div(class = "welcome-scrim", `data-welcome` = "close"),
  div(class = "welcome", role = "dialog", `aria-modal` = "true", `aria-labelledby` = "welcome-title",
      div(class = "welcome-hero", `aria-hidden` = "true",
          span(class = "wh-blob b1"), span(class = "wh-blob b2"), span(class = "wh-blob b3"),
          span(class = "wh-spark s1"), span(class = "wh-spark s2"), span(class = "wh-spark s3"),
          span(class = "wh-drop", icon("droplet"))),
      div(class = "welcome-body",
          div(class = "welcome-kicker", "Welcome"),
          h3(id = "welcome-title", "A quick tour, in three taps"),
          p(class = "welcome-lead", "Everything here can be explored. These are the three things people miss."),
          tags$ul(class = "welcome-tips",
                  tags$li(span(class = "wt-icon t1", icon("rotate")),
                          span(strong("Tap a card"), " with a turn icon to see what's behind the number.")),
                  tags$li(span(class = "wt-icon t2", icon("hand-pointer")),
                          span(strong("Hover or tap a chart"), " for the exact numbers on any bar or day.")),
                  tags$li(span(class = "wt-icon t3", icon("book-open")),
                          span(strong("Dotted words"), " explain themselves when you hover or tap them."))),
          div(class = "welcome-foot",
              tags$button(class = "welcome-go", type = "button", `data-welcome` = "close",
                          "Start exploring", icon("arrow-right")),
              tags$button(class = "welcome-alt", type = "button", `data-welcome` = "close",
                          `data-target` = "blood101", icon("hand-holding-droplet"), "I'm here to donate")))))

# Every source and footnote in one drawer, so they never interrupt the story.
sources_drawer <- div(
  class = "drawer", id = "sources-drawer", role = "dialog", `aria-modal` = "true",
  `aria-labelledby` = "drawer-title", `aria-hidden` = "true",
  div(class = "drawer-scrim", `data-close` = "sources"),
  div(class = "drawer-panel",
      div(class = "drawer-head",
          h2(id = "drawer-title", "Sources & notes"),
          tags$button(class = "drawer-close", type = "button", `data-close` = "sources",
                      `aria-label` = "Close", icon("xmark"))),
      div(class = "drawer-body",
          h3("What the words mean"),
          tags$dl(class = "glossary",
            lapply(GLOSSARY, function(g) tagList(tags$dt(g[1]), tags$dd(g[2])))),
          h3("The data"),
          p(HTML(paste0(
            "Government of Malaysia. <em>Daily Blood Donations by Blood Group &amp; State</em>. ",
            "data.gov.my. <a href=\"https://data.gov.my/data-catalogue/blood_donations_state\" ",
            "target=\"_blank\" rel=\"noopener\">Catalogue page</a>. Open data under the ",
            "<a href=\"https://creativecommons.org/licenses/by/4.0/\" target=\"_blank\" ",
            "rel=\"noopener\">Creative Commons Attribution 4.0 licence</a>. Refreshed daily."))),
          p("Records come from the Blood Bank Information System of 22 main Ministry of Health ",
            "collection sites, which cover most but not all national donations. W.P. Kuala Lumpur ",
            "includes Putrajaya and mobile campaigns around Selangor, so the two regions are combined."),
          h3("Blood 101"),
          p("Summarised in our own words; accessed 2 November 2026. Requirements can change, so ",
            "check Pusat Darah Negara before donating."),
          tags$ul(class = "source-list",
            tags$li(source_link(PDN_SOURCES$booklet)),
            tags$li(source_link(PDN_SOURCES$az)),
            tags$li(source_link(PDN_SOURCES$centres)),
            tags$li(source_link(COMPAT_SOURCES$nhsbt)),
            tags$li(source_link(COMPAT_SOURCES$hemaq))),
          h3("Notes"),
          tags$ul(class = "note-list",
            tags$li("The reasons to wait are examples only. PDN's A–Z list covers many more ",
                    "conditions and medicines."),
            tags$li("The compatibility explorer is a simplified educational guide to red-cell ABO and ",
                    "RhD compatibility. Real transfusions are matched in a laboratory and decided by ",
                    "clinicians. The donation data records ABO groups only, not RhD."),
            tags$li("A lower-quartile count is not a clinical shortage threshold, and nothing here ",
                    "forecasts blood demand, stock or shortage.")),
          h3("Where this comes from"),
          p("This dashboard extends the BloodReady AI analysis submitted for FIT5145 Foundations of ",
            "Data Science (Monash University, 2026). The repository's tests check that the methods ",
            "reproduce that report's results on its frozen snapshot."),
          p(a("Source code on GitHub", href = "https://github.com/SiobhanGoh/klang-valley-blood-donation-dashboard",
              target = "_blank", rel = "noopener"), " (MIT licence).")))
)

# --- UI -------------------------------------------------------------------------------

ui <- page(
  # The default Bootstrap theme ships precompiled, so the browser never has to
  # compile Sass; styles.css sets every colour and font.
  theme = bs_theme(version = 5),
  title = "Klang Valley Blood Donation Forecasts",
  tags$head(
    tags$meta(name = "description", content = paste(
      "Daily blood donation collections in the Klang Valley, Malaysia: live data,",
      "four forecasting methods compared, and general blood donation information.")),
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    # Apply the saved or system theme before the first paint, so nothing flashes.
    tags$script(HTML(paste0(
      "(function(){var t=null;try{t=localStorage.getItem('kvbd-theme')}catch(e){}",
      "if(t!=='light'&&t!=='dark'){t=window.matchMedia&&matchMedia('(prefers-color-scheme: dark)').matches?'dark':'light'}",
      "document.documentElement.setAttribute('data-theme',t)})();"))),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = paste0(
      "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght,SOFT@9..144,300..700,50",
      "&family=Inter:wght@300;400;600;700&display=swap")),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "plotly-basic.min.js"),
    tags$script(src = "static.js", defer = NA),
    tags$script(src = "interactions.js", defer = NA)
  ),
  div(class = "canvas-glow", `aria-hidden` = "true"),
  div(
    class = "app-shell",
    h1(class = "sr-only", "Klang Valley blood donation forecasts: daily donations in Selangor and Kuala Lumpur, and how well four methods predict them"),
    rail,
    tags$main(
      class = "stage",
      navset_hidden(
        id = "pages",

        # Overview ------------------------------------------------------------------
        nav_panel_hidden(
          "overview",
          div(
            class = "ov-layout",
            div(
              class = "ov-main",
              div(
                class = "ov-top",
                div(class = "ov-hero stage-1",
                    div(class = "ov-hero-top",
                        div(class = "chapter-eyebrow",
                            span(class = "chapter-badge", icon("droplet")),
                            span(class = "chapter-name", "Overview")),
                        status_pill),
                    div(class = "ov-reveal", `data-reveal-group` = "hero",
                        lead_in(paste0("Let's see how many donations were made ", DAY_PHRASE, "\u2026")),
                        div(class = "ov-big", count_span(dash$latest$latest_total, burst = TRUE)),
                        p(class = "ov-unit",
                          paste0("donations across Selangor and W.P. Kuala Lumpur",
                                 if (DAY_PHRASE %in% c("today", "yesterday"))
                                   paste0(" (", gsub("  ", " ", format(DATA_END, "%a %e %b")), ")") else "",
                                 ". Every one of them is someone who made time to help.")),
                        p(class = "ov-what", icon("circle-info"),
                          "This site tests four simple ways to forecast tomorrow's donations from official daily counts. It forecasts donations collected, not blood stock or demand."),
                        tags$button(class = "ov-cta", type = "button", `data-target` = "blood101",
                                    span(class = "ov-cta-icon", icon("hand-holding-droplet")),
                                    span("Thinking of donating? ", strong("See if you can", .noWS = "outside")),
                                    icon("arrow-right")))),
                div(class = "stat-grid ov-mini stage-2",
                    flip_card("This past week", count_span(dash$latest$last7, delay = 150), WEEK_LINE,
                              "Last seven days", mini_cols(LAST7_BARS)),
                    flip_card(paste("So far in", END_YEAR), count_span(dash$latest$ytd, delay = 250), YTD_LINE,
                              "Latest day, by group",
                              mini_bars(latest_groups, highlight = names(which.max(latest_groups)))),
                    flip_card("Our best forecast was off by", count_span(BEST_MAE, 1, delay = 350),
                              "donations a day, per blood group", "Error by blood group",
                              mini_bars(group_mae(BEST_METHOD), digits = 1), "tone-hero"),
                    flip_card("Quiet days we guessed too high", count_span(LOW_OVER_PCT, 0, suffix = "%", delay = 450),
                              sprintf("%d of %d quiet days. The hardest days to call", round(LOW_OVER_PCT * LOW_DAYS / 100), LOW_DAYS),
                              "Guessed too high, %",
                              mini_bars(LOW_OVER_BARS, digits = 0)))),
              panel("How far off each method was, year by year",
                    sub = tagList("Shorter bars mean a smaller ", term("average error", "error"), "."),
                    read = paste("Each bar is how many donations a method's forecast missed by, on an average",
                                 "day for one blood group, in one test period. All four blood groups are",
                                 "counted together. Hover a bar for the exact numbers."),
                    class = "stage-3 chart-panel ov-chart panel-ink",
                    chart_row(what_shows(text = explain_overview(dash$scores_overall, PERIOD_LABEL)),
                    chartOutput("overview_plot", height = "250px")),
                    footer = paste("Each period's averages and regression are fitted only on earlier",
                                   "years. Training windows overlap, so the periods are not independent tests.")),
              narrate("Here's the story in five parts. Dip in anywhere, or read them in order."),
              div(
                class = "story-strip stage-3", role = "list",
                story_card("live", "This week", signed_span(wk),
                           "donations in the past seven days, against the same week last year"),
                story_card("compare", "The winner", count_span(BEST_GAIN, 0, prefix = "\u2212", suffix = "%"),
                           sprintf("error with the %s, against repeating last week's count",
                                   method_phrase(BEST_METHOD))),
                story_card("low", "The blind spot", count_span(LOW_OVER_PCT, 0, suffix = "%"),
                           sprintf("of %d low-collection days forecast too high by the best method",
                                   LOW_DAYS)),
                story_card("patterns", "The rhythm", BUSIEST_DAY,
                           sprintf("is the busiest day, with %s times a %s's donations",
                                   fmt(BUSY_RATIO, 1), QUIETEST_DAY)),
                story_card("blood101", "Your part", tagList(count_span(3), " months"),
                           "the minimum gap between donations. Donors can be aged 17 to 70"))),
            tags$aside(
              class = "ov-side stage-2", `aria-label` = "Today's story",
              div(class = "ov-kicker", "Today's story"),
              h2(class = "ov-side-title",
                 sprintf("Our most accurate forecast: the %s.", method_phrase(BEST_METHOD))),
              p(class = "ov-side-lead",
                sprintf("It misses by %s less than simply repeating last week's count.", pct_text(BEST_GAIN))),
              h4(class = "ov-side-h", "What stands out"),
              tags$ul(class = "ov-story",
                      lapply(dash$findings, function(f) tags$li(span(class = "ov-dot"), span(emphasise(f))))),
              div(class = "ov-side-actions",
                  tags$button(class = "btn-pill btn-primary-solid", type = "button",
                              `data-target` = "live", "See the latest data", icon("arrow-right")),
                  tags$button(class = "btn-pill btn-quiet", type = "button",
                              `data-target` = "blood101", "Can I donate?")),
              div(class = "ov-more", `aria-hidden` = "true",
                  tags$button(class = "ov-more-btn", type = "button", tabindex = "-1",
                              title = "Scroll down", icon("chevron-down"))))),
          chapter_nav("overview")
        ),

        # Live tracker -------------------------------------------------------------
        nav_panel_hidden(
          "live",
          chapter_head("live",
                       tagList("Here's the latest: ", count_span(dash$latest$latest_total, burst = TRUE),
                               paste0(" donations ", DAY_PHRASE, ".")),
                       paste0("That makes ", fmt_int(dash$latest$last7), " this past week, ",
                              change_words(dash$latest$last7, dash$latest$last7_prev_year),
                              ". Scroll on for the last 60 days, and what each method expects next.")),
          div(class = "kpi-row stage-2",
              kpi("Latest day", count_span(dash$latest$latest_total),
                  paste("donations recorded on", fmt_date(DATA_END)), "kpi-hero"),
              kpi("Last 7 days", count_span(dash$latest$last7),
                  uiOutput("live_change7", inline = TRUE)),
              kpi(paste0(END_YEAR, " so far"), count_span(dash$latest$ytd),
                  uiOutput("live_change_ytd", inline = TRUE))),
          panel("The last 60 days and the next seven",
                sub = tagList("Bold line: what was recorded. Fainter lines: each ", term("forecast", "forecast"), "."),
                read = paste("The bold line with dots is what was really recorded. Where a fainter line sits close",
                             "to it, that method got the day right. The shaded strip on the right is the next",
                             "seven days, still to come. Hover any day to see every number."),
                controls = seg("live_group", "Blood group", ABO_GROUPS, "O"),
                class = "stage-3 chart-panel panel-ink",
                chart_row(what_shows("live_explain"),
                chartOutput("live_plot", height = "260px")),
                footer = "The seven-day baseline can only look one week ahead, so the forecast window is seven days."),
          narrate("Seven days out, the recent weekday average expects this. Each daily refresh ",
                  "replaces one forecast with the real count."),
          panel("Forecasts for the next seven days",
                sub = paste("Expected donations from the recent weekday average, the method with the",
                            "lowest error so far. Like every method, it overestimates unusually low days."),
                class = "stage-3 next-panel",
                tableOutput("next_table")),
          chapter_nav("live")
        ),

        # Compare ------------------------------------------------------------------
        nav_panel_hidden(
          "compare",
          chapter_head("compare", uiOutput("cmp_headline", inline = TRUE), uiOutput("cmp_sub", inline = TRUE),
                       stat = uiOutput("cmp_stat")),
          div(class = "control-bar stage-2",
              seg("cmp_year", "Year", PERIOD_CHOICES, as.character(LATEST)),
              chips("cmp_methods", "Methods", METHODS, METHODS)),
          panel("Error by blood group",
                read = paste("One small chart per blood group. Each bar is a method's average error for that",
                             "group: shorter is better. Hover a bar for the big-miss error and the number of days."),
                sub = tagList("The same ", term("average error", "error"), ", split by ",
                              term("ABO group", "abo"), ".", span(class = "phone-only", " Left to right in each group: 7-day baseline, weekday average, regression, recent average.")),
                class = "stage-3 chart-panel panel-ink", chart_row(what_shows("cmp_explain"),
                chartOutput("cmp_group_plot", height = "380px"))),
          panel("Scores", class = "stage-3",
                sub = tagList("Every number behind the charts. ", term("Big-miss error", "rmse"), " and ",
                              term("average bias", "bias"), " are explained when you hover them."),
                fold("Show every score", uiOutput("cmp_table"))),
          narrate("Averages hide the day-to-day story. Here is how each forecast tracked the ",
                  "real count, one day at a time."),
          panel("Recorded and forecast, day by day",
                read = paste("The bold white line with dots is the real count for each day; the fainter lines are the",
                             "forecasts. The closer a line hugs the white one, the better that method did."),
                controls = tagList(
                  seg("cmp_group", "Blood group", ABO_GROUPS, "O"),
                  div(class = "control",
                      span(class = "control-label", "Dates"),
                      date_range("cmp_dates", start = max(DATA_END - 30, min(dash$predictions$date)),
                                 end = DATA_END, min = min(dash$predictions$date), max = DATA_END))),
                class = "stage-3 chart-panel panel-ink",
                chart_row(what_shows("daily_explain"),
                chartOutput("cmp_daily_plot", height = "270px")),
                footer = paste("The seven-day baseline uses the count observed a week earlier, so",
                               "it is a rolling benchmark rather than a forecast issued at the start",
                               "of the period.")),
          chapter_nav("compare")
        ),

        # Low-collection days ------------------------------------------------------
        nav_panel_hidden(
          "low",
          chapter_head("low", uiOutput("low_headline", inline = TRUE),
                       tagList(term("Low-collection days", "low_day"),
                               " are the quietest quarter of days for each blood group. ",
                               uiOutput("low_sub", inline = TRUE)),
                       stat = uiOutput("low_stat")),
          div(class = "control-bar stage-2",
              seg("low_year", "Year", PERIOD_CHOICES, as.character(LATEST))),
          panel("Error on low-collection days and other days",
                read = paste("For each method, the bright bar is its average error on the quietest days and the",
                             "muted bar is its error on all other days. A bright bar much taller than its muted",
                             "bar means the method struggles when donations drop."),
                sub = tagList("Bright bars: ", term("low-collection days", "low_day"), ". Muted bars: all other days.", span(class = "phone-only", " Left to right in each group: 7-day baseline, weekday average, regression, recent average.")),
                class = "stage-3 chart-panel panel-ink", chart_row(what_shows("low_explain"),
                chartOutput("low_plot", height = "380px"))),
          narrate(LOW_WHY),
          panel("The numbers behind it", class = "stage-3 tabs-in-head",
                sub = "Three short tables. Pick one.",
                tab_set("low_tabs", list(
                  tab("Low-day lines", icon = "ruler-horizontal",
                      p(class = "tab-note", "For each blood group, a day counts as low at or below this count: the ",
                        term("lower quartile", "quartile"), " of earlier years."),
                      tableOutput("cutoff_table"),
                      p(class = "tab-foot", "A lower-quartile count is not a clinical shortage threshold.")),
                  tab("Why they aim too high", icon = "arrow-up",
                      p(class = "tab-note", "If a method's lowest ", term("forecast", "forecast"),
                        " of the whole period is still above the low-day line, it can never predict a day that quiet."),
                      uiOutput("min_pred_table")),
                  tab("Low-day errors", icon = "table",
                      p(class = "tab-note", "How often each method's forecast was ", term("too high", "overpredict"),
                        " on low days, and by how much."),
                      uiOutput("low_table"),
                      p(class = "tab-foot", "'Average overshoot' is how far above the real count the forecast was,",
                        " counting zero on days it was not too high."))))),
          chapter_nav("low")
        ),

        # Patterns -------------------------------------------------------------------
        nav_panel_hidden(
          "patterns",
          chapter_head("patterns",
                       paste0("Guess the busiest day? It's ", BUSIEST_DAY, "."),
                       sprintf(paste("On an average %s the Klang Valley records %s times as many donations",
                                     "as on a %s, and group O gives %s of them. Twenty years of recorded",
                                     "donations, 2006 to %s."),
                               BUSIEST_DAY, fmt(BUSY_RATIO, 1), QUIETEST_DAY, pct_text(O_SHARE),
                               fmt_date(DATA_END))),
          div(class = "control-bar stage-2",
              chips("pattern_groups", "Blood groups", ABO_GROUPS, ABO_GROUPS),
              div(class = "control control-wide",
                  span(class = "control-label", "Years"),
                  year_range("pattern_years", 2006, END_YEAR, 2006, END_YEAR))),
          panel("Recorded donations, month by month", sub = "Stacked by blood group. Hover a month for exact counts.",
                read = paste("The height of the whole shape is all donations that month; each colour band is",
                             "one blood group. Bumps show busy and quiet months."),
                class = "stage-3 chart-panel panel-ink", chartOutput("weekly_plot", height = "250px")),
          panel("Zoom in on the rhythm", class = "stage-3 chart-panel panel-ink tabs-in-head",
                sub = "Three ways to slice the same donations. Pick a view.",
                tab_set("pattern_tabs", list(
                  tab("By weekday", icon = "calendar-week",
                      how_read("Taller bars are busier weekdays. Each colour is one blood group."),
                      chart_row(what_shows("weekday_explain"),
                      chartOutput("weekday_plot", height = "250px"))),
                  tab("By year", icon = "chart-line",
                      how_read("Each line is one blood group; rising lines mean more donations a day than before."),
                      chart_row(what_shows("annual_explain"),
                      chartOutput("annual_plot", height = "250px"))),
                  tab("Blood-type mix", icon = "droplet",
                      how_read("Each column is one year and adds up to 100%. The bands show each group's share."),
                      chart_row(what_shows(text = paste(MIX_LINE, "This describes who donated, not how common each type",
                                                        "is in the population.")),
                                chartOutput("mix_plot", height = "250px")))))),
          chapter_nav("patterns")
        ),

        # Blood 101 -----------------------------------------------------------------
        nav_panel_hidden(
          "blood101",
          chapter_head("blood101",
                       "Thinking of giving blood? Every forecast here is someone's donation.",
                       paste("Here's what it takes to give blood in Malaysia, summarised from Pusat Darah",
                             "Negara (National Blood Centre). Staff confirm eligibility on the day;",
                             "check PDN for the latest requirements.")),
          tab_set("b101_tabs", class = "kv-tabset-page stage-2", list(
            tab("Can I donate?", icon = "circle-check",
          section(
            "Can I donate?",
            sub = "The basics PDN checks before every donation.",
            div(class = "donate-grid",
                div(class = "dg-side dg-need",
                    h4(class = "dg-head", icon("clipboard-check"), "What you need"),
                    tags$ul(class = "need-list", lapply(ELIGIBILITY, function(e)
                      tags$li(class = "need", title = e$detail,
                              span(class = "need-icon", icon(e$icon)),
                              div(class = "need-copy",
                                  div(class = "need-top", span(class = "need-title", e$title),
                                      span(class = "need-value", e$value)),
                                  p(class = "need-text", e$short)))))),
                div(class = "dg-map",
                    viz(mindmap(c("Can I", "donate?"),
                                lapply(ELIGIBILITY, function(e) list(title = e$map, sub = e$value, tip = e$detail)),
                                label = paste(vapply(ELIGIBILITY, function(e) paste0(e$title, ": ", e$value), ""),
                                              collapse = "; "),
                                width = 600, arm = 158, wrap = 17, hub_r = 58),
                        NULL)),
                div(class = "dg-side dg-wait",
                    h4(class = "dg-head", icon("hourglass-half"), "Some reasons to wait"),
                    tags$ul(class = "wait-list", lapply(DEFERRAL_EXAMPLES, function(d)
                      tags$li(class = "wait", title = paste0(d$what, ": ", d$wait),
                              span(class = "wait-icon", icon(d$icon)),
                              div(class = "wait-copy",
                                  div(class = "wait-top", span(class = "wait-title", sub("^After ", "", d$what)),
                                      span(class = "wait-tag", d$tag)),
                                  span(class = "wait-track", `aria-hidden` = "true",
                                       span(class = "wait-fill", style = sprintf("--w:%.0f%%;", 100 * d$months / 6))),
                                  if (nzchar(note <- sub("^[0-9]+ (weeks|months) ?", "", d$wait)))
                                    span(class = "wait-text", note))))),
                    p(class = "viz-note", "Examples only; staff decide on the day. ",
                      tags$button(class = "link-btn", type = "button", `data-open` = "sources",
                                  "Sources & notes")))))),
            tab("On the day", icon = "hand-holding-medical",
          section("Before you go",
                  sub = "Getting ready, from PDN's donor booklet.",
                  div(class = "prep-grid",
                      lapply(seq_along(PREP_TIPS), function(i) {
                        t <- PREP_TIPS[[i]]
                        div(class = "prep", style = sprintf("--i:%d;", i - 1),
                            span(class = "prep-icon", icon(t$icon)),
                            span(class = "prep-when", t$when),
                            div(class = "prep-title", t$title),
                            if (!is.null(t$text)) p(class = "prep-text", t$text))
                      }),
                      div(class = "prep prep-food", style = sprintf("--i:%d;", length(PREP_TIPS)),
                          span(class = "prep-icon", icon("bowl-food")),
                          span(class = "prep-when", "Food with iron"),
                          div(class = "prep-title", "Keep your haemoglobin up"),
                          p(class = "prep-text", "It is checked before you donate. PDN's examples of iron-rich food:"),
                          div(class = "food-chips", lapply(IRON_FOODS, function(f)
                            span(class = "food-chip", icon(f$icon), f$name))))),
                  p(class = "viz-note", "After donating, eating and drinking the refreshments provided is a must. ",
                    tags$button(class = "link-btn", type = "button", `data-open` = "sources", "Sources & notes"))),
          section("How a donation works",
                  viz(journey(Map(function(t, s) list(title = t, sub = s), DONATION_STEP_TITLES, DONATION_STEP_NOTES),
                              label = paste(DONATION_STEPS, collapse = " ")),
                      tags$ol(class = "steps", lapply(DONATION_STEPS, tags$li)))),
          section("After donating",
                  viz(mindmap(c("After", "you give"),
                              Map(function(t, s, k) list(title = t, sub = s, art = k),
                                  AFTERCARE_TITLES, AFTERCARE, AFTERCARE_ART),
                              label = paste(AFTERCARE, collapse = " ")),
                      tags$ul(class = "aftercare", lapply(AFTERCARE, tags$li))))),
            tab("Where to go", icon = "location-dot",
          section(
            "Where blood is collected",
            sub = paste("Pick a region. Each state shows the donations recorded there in the official",
                        "data; the Central region also lists PDN's fixed donation centres."),
            div(class = "region-wrap",
                div(class = "region-bar",
                    seg("region_pick", "Region", names(MALAYSIA_REGIONS), "Central")),
                uiOutput("region_result")))),
            tab("Who can receive", icon = "people-arrows",
          section(
            "Blood type compatibility explorer",
            sub = tagList("A blood type is the ", term("ABO group", "abo"), " plus ", term("RhD", "rhd"),
                          " (+ or −). Pick one to see whose ", term("red cells", "red_cells"),
                          " it can receive and whom it can give red cells to."),
            div(class = "type-grid", role = "radiogroup", `aria-label` = "Blood type",
                lapply(BLOOD_TYPES, function(t)
                  tags$button(class = paste("type-pick", if (t == "O+") "is-on"),
                              type = "button", `data-type` = t, role = "radio",
                              `aria-checked` = if (t == "O+") "true" else "false", t))),
            uiOutput("compat_result"),
            p(class = "viz-note", "A simplified guide, not medical advice. ",
              tags$button(class = "link-btn", type = "button", `data-open` = "sources",
                          "Sources & notes")))))),
          chapter_nav("blood101")
        ),

        # About ------------------------------------------------------------------------
        nav_panel_hidden(
          "about",
          chapter_head("about",
                       "Curious how it all works? Here's how the forecasts are tested.",
                       paste("The methods, what the regression learned, a check of the raw data,",
                             "and, just as important, what a look back like this can't tell you.")),
          tab_set("about_tabs", class = "kv-tabset-page stage-2", list(
            tab("The methods", icon = "diagram-project",
          section(
            "Four forecasting methods",
            sub = "Each one uses only data recorded before the day it predicts.",
            viz(method_flow(latest_scores, LATEST_LABEL),
            div(class = "method-grid",
                    div(class = "method", span(class = "method-tag", "Benchmark"), h4("Seven-day baseline"),
                        p("The count recorded for the same blood group seven calendar days earlier.")),
                    div(class = "method", span(class = "method-tag", "All history"), h4("Weekday average"),
                        p("Mean for the blood group and weekday over all years before the evaluation year.")),
                    div(class = "method", span(class = "method-tag", "Trend"), h4("Linear regression"),
                        p("A separate lm(donations ~ weekday + time_index) per blood group, fitted on all earlier years.")),
                    div(class = "method", span(class = "method-tag", "Last 3 years"), h4("Recent weekday average"),
                        p("Mean for the blood group and weekday over the three complete years before the evaluation year.")))))),
            tab("What it learned", icon = "brain",
          p(class = "tab-note", sprintf("What did the regression learn from 2006 to %d? Associations, not causes.",
                          LATEST - 1L)),
          div(class = "two-up stage-3",
              panel("What the regression learned", tableOutput("coef_table"),
                    sub = tagList("Each ", term("coefficient", "coefficient"), " in plain words: how much donations",
                                  " grow each year, and how much busier a Sunday is than a Monday. ",
                                  term("Fit (R²)", "r2"), " shows how much of the pattern it captures."),
                    footer = "The Sunday contrast holds the time index fixed."),
              panel("Too high or too low on average?", uiOutput("bias_table"),
                    sub = tagList(term("Average bias", "bias"), ": the real count minus the forecast. ",
                                  "Positive means the forecasts ran low; negative means they ran high."),
                    footer = paste("Near-zero averages mean positive and negative errors cancel;",
                                   "they do not mean the daily forecasts are accurate."))),
          panel("How the error moves through the year",
                sub = paste("Each point is a method's average forecast error for one month: how many",
                            "donations per blood group per day the forecast was off by. Higher means",
                            "a harder month to predict. Hover a month to compare methods."),
                controls = tagList(
                  seg("month_year", "Year", PERIOD_CHOICES, as.character(LATEST)),
                  chips("month_methods", "Methods", METHODS,
                        c("Seven-day baseline", "Linear regression", "Recent weekday average"))),
                class = "stage-3 chart-panel panel-ink",
                chart_row(what_shows("monthly_explain"),
                chartOutput("monthly_plot", height = "250px")),
                footer = "Describes when errors occurred; it cannot explain why.")),
            tab("Data checks", icon = "clipboard-check",
          div(class = "two-up stage-3",
              panel("Raw data checks", tableOutput("audit_table"),
                    sub = "Basic checks on the official file before anything is calculated: size, dates, gaps and duplicates."),
              panel("What goes into the forecasts", tableOutput("panel_table"),
                    sub = "The daily counts used: one row per blood group per day, for the Klang Valley.",
                    footer = "The ALL category is excluded from modelling to avoid double counting."))),
            tab("Limits", icon = "triangle-exclamation",
          section(
            "Validation limits",
            viz(mindmap(c("What this", "can't show"), LIMITS_MAP,
                        label = paste(vapply(LIMITS_MAP, function(b) paste0(b$title, ": ", b$sub), ""),
                                      collapse = "; ")),
            tags$ul(class = "limits",
                  tags$li("Results are retrospective. The method choices were developed on the ",
                          "2006–2025 data, so earlier evaluation years are not untouched holdouts."),
                  tags$li("Evaluation periods share overlapping training data and are not ",
                          "independent experiments. No significance testing is done."),
                  tags$li("The three-year window for the recent weekday average was not tuned or ",
                          "independently validated."),
                  tags$li("No forecast intervals, weather, holiday, campaign, inventory or ",
                          "hospital-demand data are used."),
                  tags$li("Zero counts are kept; they may be closures or reporting gaps."),
                  tags$li("The official data can be revised, so past numbers may change slightly ",
                          "between daily refreshes."))))))),
          chapter_nav("about")
        ),

        # Export ------------------------------------------------------------------------
        nav_panel_hidden(
          "export",
          chapter_head("export",
                       "Take the story with you.",
                       paste("Three reports, each told its own way. Pick one, set it up, check the",
                             "preview, then save it as a PDF. Everything is made in your browser;",
                             "nothing you enter is sent or stored.")),
          div(class = "control-bar stage-2",
              seg("report_type", "Report",
                  c("Summary report" = "summary", "Data appendix" = "data", "My donor card" = "card"),
                  "summary")),
          div(class = "report-studio stage-3",
              div(class = "panel report-setup",
                  div(class = "panel-head", div(class = "panel-titles",
                      h3(class = "panel-title", "Set up"),
                      p(class = "panel-sub", uiOutput("report_blurb", inline = TRUE)))),
                  div(class = "panel-body",
                      conditionalPanel("input.report_type == 'summary'",
                        seg("rep_period", "Year", PERIOD_CHOICES, as.character(LATEST))),
                      conditionalPanel("input.report_type == 'data'",
                        seg("rep_group", "Blood group", c("All" = "all", setNames(ABO_GROUPS, ABO_GROUPS)), "all"),
                        div(class = "control",
                            span(class = "control-label", "Dates"),
                            date_range("rep_dates", start = DATA_END - 30, end = DATA_END,
                                       min = min(dash$panel$date), max = DATA_END)),
                        chips("rep_extra", "Include", c("Forecasts from each method" = "forecasts"),
                              "forecasts")),
                      conditionalPanel("input.report_type == 'card'",
                        seg("card_type", "My blood type", BLOOD_TYPES, "O+"),
                        seg("card_region", "My region", names(MALAYSIA_REGIONS), "Central"),
                        tags$details(class = "date-pick",
                            tags$summary(span(class = "control-label", "Last donation (optional)"),
                                         uiOutput("card_last_label", inline = TRUE)),
                            div(class = "date-pick-row",
                                selectInput("card_day", "Day", c("Day" = "", 1:31), selectize = FALSE),
                                selectInput("card_month", "Month",
                                            c("Month" = "", setNames(1:12, month.name)), selectize = FALSE),
                                selectInput("card_year", "Year",
                                            c("Year" = "", as.character(seq(as.integer(format(Sys.Date(), "%Y")),
                                                                              by = -1, length.out = 6))),
                                            selectize = FALSE)),
                            div(class = "date-pick-foot",
                                uiOutput("card_last_check", inline = TRUE),
                                actionLink("card_clear", "Clear")))),
                      div(class = "report-actions",
                          tags$button(class = "btn-pill btn-primary-solid js-print", type = "button",
                                      icon("file-pdf"), "Save as PDF"),
                          conditionalPanel("input.report_type == 'data'",
                            actionButton("csv_go", tagList(icon("table"), "Download CSV"),
                                         class = "btn-pill btn-quiet"))))),
              div(class = "paper-stage",
                  div(class = "paper-label", "Preview"),
                  uiOutput("report_preview"))),
          chapter_nav("export")
        )
      )
    ),
    rail_tab,
    tags$button(class = "help-fab", type = "button", `data-welcome` = "open", `aria-label` = "Show the quick tips",
                span(class = "fab-q", icon("question")), span(class = "fab-text", "Quick tips")),
    sources_drawer,
    welcome_card
  )
)

# --- Server ---------------------------------------------------------------------------

server <- function(input, output, session) {

  period_lab <- function(y) PERIOD_LABEL[[as.character(y)]]

  observeEvent(input$rail_go, {
    req(input$rail_go %in% vapply(SECTIONS, `[[`, "", "id"))
    nav_select("pages", input$rail_go)
  })

  # Overview ---------------------------------------------------------------------------
  output$overview_plot <- renderChart(chart_pooled_mae(dash$scores_overall, PERIOD_LABEL))

  # Live tracker -------------------------------------------------------------------------
  change_ui <- function(now, before, what) {
    ch <- pct_change(now, before)
    if (is.na(ch)) return(span(what))
    span(class = if (ch >= 0) "trend up" else "trend down",
         icon(if (ch >= 0) "arrow-up" else "arrow-down"),
         sprintf("%s%% vs %s", fmt(abs(ch), 1), what))
  }
  output$live_change7 <- renderUI(change_ui(dash$latest$last7, dash$latest$last7_prev_year,
                                            "the same week last year"))
  output$live_change_ytd <- renderUI(change_ui(dash$latest$ytd, dash$latest$ytd_prev_year,
                                               "the same days last year"))

  output$live_plot <- renderChart({
    g <- if (is.null(input$live_group)) "O" else input$live_group
    chart_live(dash$panel, dash$predictions, dash$next_days, g, DATA_END)
  })

  output$live_explain <- renderUI(explain_live(dash$predictions,
                                               if (is.null(input$live_group)) "O" else input$live_group, DATA_END))

  output$next_table <- renderTable({
    d <- dash$next_days[dash$next_days$method == "Recent weekday average", ]
    days <- sort(unique(d$date))
    val <- function(g) vapply(days, function(x) d$prediction[d$date == x & d$blood_type == g], 0)
    v <- lapply(setNames(ABO_GROUPS, ABO_GROUPS), val)
    data.frame(Day = format(days, "%a %e %b"),
               A = fmt_int(v$A), AB = fmt_int(v$AB), B = fmt_int(v$B), O = fmt_int(v$O),
               Total = fmt_int(v$A + v$AB + v$B + v$O), check.names = FALSE)
  }, align = "lrrrrr", striped = FALSE, width = "100%")

  # Compatibility explorer -------------------------------------------------------------
  output$compat_result <- renderUI({
    t <- if (is.null(input$bt_pick) || !input$bt_pick %in% BLOOD_TYPES) "O+" else input$bt_pick
    compat_flow(t)
  })

  output$region_result <- renderUI({
    r <- if (is.null(input$region_pick) || !input$region_pick %in% names(MALAYSIA_REGIONS)) "Central"
         else input$region_pick
    region_view(r)
  })

  # Reports ------------------------------------------------------------------------------------
  report_kind <- reactive(if (is.null(input$report_type)) "summary" else input$report_type)
  output$report_blurb <- renderUI(switch(report_kind(),
    summary = "An editorial summary: the winner, the scoreboard, the findings and the blind spot.",
    data = "A technical appendix: the exact daily records, their source and definitions. Also as CSV.",
    card = "A personal one-page card: who you can give to and receive from, and where to donate."))
  rep_dates <- reactive({
    d <- input$rep_dates
    if (is.null(d) || any(is.na(d))) c(DATA_END - 30, DATA_END) else sort(as.Date(d))
  })
  output$report_preview <- renderUI({
    switch(report_kind(),
      summary = report_summary(if (is.null(input$rep_period)) LATEST else input$rep_period),
      data = report_data(if (is.null(input$rep_group)) "all" else input$rep_group,
                         rep_dates()[1], rep_dates()[2], "forecasts" %in% input$rep_extra),
      card = {
        t <- if (is.null(input$card_type) || !input$card_type %in% BLOOD_TYPES) "O+" else input$card_type
        r <- if (is.null(input$card_region) || !input$card_region %in% names(MALAYSIA_REGIONS)) "Central"
             else input$card_region
        last <- card_last()
        report_card(t, r, if (!is.null(last) && !is.na(last)) last else NULL)
      })
  })
  card_last <- reactive({
    d <- suppressWarnings(as.integer(input$card_day)); m <- suppressWarnings(as.integer(input$card_month))
    y <- suppressWarnings(as.integer(input$card_year))
    if (any(is.na(c(d, m, y))) || !length(c(d, m, y)) ) return(NULL)
    dt <- suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, m, d)))
    if (is.na(dt) || dt > Sys.Date()) return(NA)
    dt
  })
  output$card_last_label <- renderUI({
    v <- card_last()
    span(class = "date-pick-value",
         if (is.null(v)) "Not set" else if (is.na(v)) "Check the date" else format(v, "%a %e %b %Y"))
  })
  output$card_last_check <- renderUI({
    v <- card_last()
    if (!is.null(v) && is.na(v)) span(class = "date-pick-warn", icon("circle-exclamation"),
                                     "That date doesn't exist or is in the future.")
  })
  observeEvent(input$card_clear, {
    for (id in c("card_day", "card_month", "card_year")) updateSelectInput(session, id, selected = "")
  })

  observeEvent(input$csv_go, {
    d <- rep_dates()
    df <- data_extract(if (is.null(input$rep_group)) "all" else input$rep_group, d[1], d[2],
                       "forecasts" %in% input$rep_extra)
    session$sendCustomMessage("download_csv", list(
      name = sprintf("klang-valley-donations_%s_%s.csv", format(d[1]), format(d[2])),
      text = csv_text(df)))
  })

  # Compare ----------------------------------------------------------------------------------
  cmp_methods <- reactive({
    m <- input$cmp_methods
    if (is.null(m) || !length(m)) METHODS else m
  })
  cmp_year <- reactive(as.integer(if (is.null(input$cmp_year)) LATEST else input$cmp_year))

  output$cmp_group_plot <- renderChart({
    chart_group_mae(dash$scores_by_group[dash$scores_by_group$evaluation_year == cmp_year(), ], cmp_methods())
  })

  output$cmp_table <- renderUI({
    y <- cmp_year(); m <- cmp_methods()
    pooled <- dash$scores_overall[dash$scores_overall$evaluation_year == y & dash$scores_overall$method %in% m, ]
    pooled$blood_type <- "All groups"
    grp <- dash$scores_by_group[dash$scores_by_group$evaluation_year == y & dash$scores_by_group$method %in% m, ]
    cols <- c("blood_type", "method", "observations", "MAE", "RMSE", "mean_signed_error")
    d <- rbind(as.data.frame(pooled)[cols], as.data.frame(grp)[cols])
    d <- d[order(d$blood_type != "All groups", d$blood_type, match(d$method, METHODS)), ]
    sort_table(data.frame(`Blood group` = d$blood_type, Method = d$method, `Days scored` = d$observations,
                          `Average error (MAE)` = d$MAE, `Big-miss error (RMSE)` = d$RMSE,
                          `Average bias` = d$mean_signed_error, check.names = FALSE),
               c("Average error (MAE)", "Big-miss error (RMSE)", "Average bias"))
  })

  output$cmp_daily_plot <- renderChart({
    req(input$cmp_dates)
    g <- if (is.null(input$cmp_group)) "O" else input$cmp_group
    p <- chart_daily(dash$predictions, g, cmp_methods(), input$cmp_dates[1], input$cmp_dates[2])
    validate(need(!is.null(p), "No evaluation days in the selected range."))
    p
  })

  # The headline, its sentence and the big number follow the chosen period and methods.
  cmp_story <- reactive({
    y <- cmp_year(); m <- cmp_methods()
    o <- dash$scores_overall[dash$scores_overall$evaluation_year == y & dash$scores_overall$method %in% m, ]
    o <- o[order(o$MAE), ]
    ref <- if ("Seven-day baseline" %in% o$method && o$method[1] != "Seven-day baseline") "Seven-day baseline"
           else if (nrow(o) > 1) o$method[2] else NA
    list(y = y, best = o$method[1], mae = o$MAE[1], ref = ref,
         ref_mae = if (is.na(ref)) NA else o$MAE[o$method == ref],
         label = PERIOD_LABEL[[as.character(y)]])
  })
  output$cmp_headline <- renderUI({
    st <- cmp_story()
    if (is.na(st$ref)) return(paste0("Showing the ", method_phrase(st$best), " on its own."))
    tagList(paste0("So, which forecast did best? The ", method_phrase(st$best), ", with "),
            count_span(100 * (st$ref_mae - st$mae) / st$ref_mae, 0, suffix = "%"), " less error.")
  })
  output$cmp_sub <- renderUI({
    st <- cmp_story()
    if (is.na(st$ref)) return(sprintf("In %s its forecasts were off by %s donations per blood group per day.",
                                      st$label, fmt(st$mae)))
    sprintf("In %s its forecasts were off by %s donations per blood group per day, against %s for %s.",
            st$label, fmt(st$mae), fmt(st$ref_mae),
            if (st$ref == "Seven-day baseline") "simply repeating last week's count"
            else paste("the", method_phrase(st$ref)))
  })
  output$cmp_stat <- renderUI({
    st <- cmp_story()
    name <- sub(" \\(.*", "", st$label)
    if (is.na(st$ref)) return(big_stat(count_span(st$mae, 1, burst = TRUE),
                                       paste("donations a day off, on average,", name)))
    gain <- 100 * (st$ref_mae - st$mae) / st$ref_mae
    big_stat(count_span(st$mae, 1, burst = TRUE),
             paste("donations a day off, on average,", name),
             bars = list(list(kind = "is-this", name = st$best, value = st$mae, text = fmt(st$mae, 1)),
                         list(kind = "is-ref", name = if (st$ref == "Seven-day baseline") "Repeating last week" else st$ref,
                              value = st$ref_mae, text = fmt(st$ref_mae, 1))),
             verdict = sprintf("%s%% less error", fmt(gain, 0)), tone = if (gain > 0) "good" else "bad")
  })
  output$cmp_explain <- renderUI({
    y <- cmp_year()
    explain_compare(dash$scores_overall[dash$scores_overall$evaluation_year == y, ],
                    dash$scores_by_group[dash$scores_by_group$evaluation_year == y, ],
                    cmp_methods(), PERIOD_LABEL[[as.character(y)]])
  })
  output$daily_explain <- renderUI({
    req(input$cmp_dates)
    explain_daily(dash$predictions, if (is.null(input$cmp_group)) "O" else input$cmp_group,
                  cmp_methods(), input$cmp_dates[1], input$cmp_dates[2])
  })

  # Low-collection days ------------------------------------------------------------------
  low_y <- reactive(as.integer(if (is.null(input$low_year)) LATEST else input$low_year))
  low_story <- reactive({
    y <- low_y()
    o <- dash$scores_overall[dash$scores_overall$evaluation_year == y, ]
    best <- o$method[which.min(o$MAE)]
    le <- dash$low_errors[dash$low_errors$evaluation_year == y, ]
    lo <- le[le$day_category == "Low collection" & le$method == best, ]
    ot <- le[le$day_category == "Other days" & le$method == best, ]
    rows <- le[le$day_category == "Low collection", ]
    over_all <- vapply(split(rows, rows$method), function(r) sum(r$days * r$overprediction_percent) / sum(r$days), 0)
    list(best = best, days = sum(lo$days), label = PERIOD_LABEL[[as.character(y)]],
         over = sum(lo$days * lo$overprediction_percent) / sum(lo$days), over_all = over_all,
         low_mae = sum(lo$days * lo$MAE) / sum(lo$days), other_mae = sum(ot$days * ot$MAE) / sum(ot$days),
         other_over = sum(ot$days * ot$overprediction_percent) / sum(ot$days))
  })
  output$low_headline <- renderUI({
    a <- low_story()$over_all
    if (all(a == 100)) "On the quietest days, every forecast aims too high."
    else if (all(a > 50)) "On the quietest days, the forecasts aim too high."
    else "The quietest days are the hardest to forecast."
  })
  output$low_sub <- renderUI({
    st <- low_story()
    sprintf(paste("In %s, the %s was off by %s donations a day on them, against %s on other days.",
                  "This looks back at what happened; it is not an early warning."),
            st$label, method_phrase(st$best), fmt(st$low_mae, 1), fmt(st$other_mae, 1))
  })
  output$low_stat <- renderUI({
    st <- low_story()
    worse <- st$over > st$other_over
    big_stat(count_span(st$over, 0, suffix = "%", burst = TRUE),
             sprintf("of %d quiet days forecast too high, %s", st$days, sub(" \\(.*", "", st$label)),
             bars = list(list(kind = "is-this", name = "Quiet days", value = st$over, text = paste0(fmt(st$over, 0), "%")),
                         list(kind = "is-ref", name = "Other days", value = st$other_over, text = paste0(fmt(st$other_over, 0), "%"))),
             verdict = if (worse) "Aims too high on quiet days" else "No more than on other days",
             tone = if (worse) "bad" else "good")
  })
  output$low_explain <- renderUI(explain_low(dash$low_errors[dash$low_errors$evaluation_year == low_y(), ],
                                             PERIOD_LABEL[[as.character(low_y())]]))

  output$low_plot <- renderChart(chart_low_days(dash$low_errors[dash$low_errors$evaluation_year == low_y(), ]))

  output$cutoff_table <- renderTable({
    y <- low_y()
    low <- dash$low_errors[dash$low_errors$evaluation_year == y & dash$low_errors$day_category == "Low collection", ]
    low <- unique(as.data.frame(low)[c("blood_type", "days", "mean_actual")])
    cut <- as.data.frame(dash$low_cutoffs[dash$low_cutoffs$evaluation_year == y, ])
    k <- match(cut$blood_type, low$blood_type)
    data.frame(`Blood group` = cut$blood_type,
               `Low-day line (≤)` = sub("\\.?0+$", "", fmt(cut$low_cutoff)),
               `Low days` = ifelse(is.na(k), 0L, low$days[k]),
               `Average on low days` = ifelse(is.na(k), "–", fmt(low$mean_actual[k], 1)),
               check.names = FALSE)
  }, width = "100%")

  output$low_table <- renderUI({
    d <- dash$low_errors[dash$low_errors$evaluation_year == low_y() & dash$low_errors$day_category == "Low collection", ]
    d <- d[order(d$blood_type, match(d$method, METHODS)), ]
    sort_table(data.frame(`Blood group` = d$blood_type, Method = d$method, `Low days` = d$days,
                          `Average error` = d$MAE, `Days forecast too high (%)` = d$overprediction_percent,
                          `Average overshoot` = d$mean_excess, check.names = FALSE),
               c("Average error", "Days forecast too high (%)", "Average overshoot"))
  })

  output$min_pred_table <- renderUI({
    d <- dash$min_predictions[dash$min_predictions$evaluation_year == low_y(), ]
    d <- d[order(d$blood_type, match(d$method, METHODS)), ]
    sort_table(data.frame(Group = d$blood_type, Method = d$method,
                          `Lowest forecast` = d$minimum_prediction, `Low-day line` = d$low_cutoff,
                          `Can never reach it?` = ifelse(d$all_predictions_above_cutoff, "Yes", "No"),
                          check.names = FALSE),
               c("Lowest forecast", "Low-day line"))
  })

  # Patterns ---------------------------------------------------------------------------------
  pattern_panel <- reactive({
    g <- if (is.null(input$pattern_groups) || !length(input$pattern_groups)) ABO_GROUPS
         else input$pattern_groups
    yrs <- if (is.null(input$pattern_years)) c(2006, END_YEAR) else input$pattern_years
    yr <- year_of(dash$panel$date)
    dash$panel[dash$panel$blood_type %in% g & yr >= yrs[1] & yr <= yrs[2], ]
  })

  output$weekly_plot <- renderChart(chart_month_totals(pattern_panel(), DATA_END))
  output$weekday_plot <- renderChart({
    d <- pattern_panel()
    d$weekday <- WEEKDAYS[weekday_num(d$date)]
    m <- group_stat(d, c("blood_type", "weekday"), "donations", mean, "mean_donations")
    m <- m[order(match(m$weekday, WEEKDAYS)), ]
    chart_weekday(m)
  })
  output$annual_plot <- renderChart(chart_annual(annual_summary(pattern_panel())))
  output$weekday_explain <- renderUI({
    d <- pattern_panel(); d$weekday <- WEEKDAYS[weekday_num(d$date)]
    explain_weekday(group_stat(d, c("blood_type", "weekday"), "donations", mean, "mean_donations"))
  })
  output$annual_explain <- renderUI(explain_annual(annual_summary(pattern_panel())))
  output$mix_plot <- renderChart(chart_mix(dash$blood_mix, PARTIAL_YEAR))

  # About ------------------------------------------------------------------------------------------
  output$audit_table <- renderTable({
    a <- dash$audit
    data.frame(
      Check = c("Source", "MD5 checksum", "Rows × columns", "Date range", "Distinct dates",
                "Regions", "Blood-type values (incl. ALL)", "Missing values",
                "Duplicate rows", "Duplicate date-state-type keys", "Negative counts",
                "Zero counts", "ALL vs A+B+AB+O mismatches", "Largest ALL mismatch"),
      Result = c(a$source, a$checksum, paste(fmt_int(a$rows), "×", a$columns),
                 paste(a$first_date, "to", a$last_date), fmt_int(a$distinct_dates),
                 a$regions, a$blood_type_values, a$missing_values, a$duplicate_rows,
                 a$duplicate_keys, a$negative_values,
                 sprintf("%s (%.2f%%)", fmt_int(a$zero_values), 100 * a$zero_values / a$rows),
                 sprintf("%s of %s date-state totals", fmt_int(a$all_mismatches),
                         fmt_int(a$all_rows_checked)),
                 a$all_max_abs_difference)
    )
  }, width = "100%")

  output$panel_table <- renderTable({
    p <- dash$panel_summary
    data.frame(
      Item = c("Regions combined", "Blood groups", "Period", "Group-days",
               "Distinct dates", "Zero group-days"),
      Value = c("Selangor + W.P. Kuala Lumpur", "A, AB, B, O",
                paste(p$first_date, "to", p$last_date), fmt_int(p$rows), fmt_int(p$dates),
                sprintf("%d (%.2f%%)", p$zero_values, 100 * p$zero_values / p$rows))
    )
  }, width = "100%")

  output$coef_table <- renderTable({
    d <- dash$coefficients
    data.frame(`Blood group` = d$blood_type,
               `Change per year` = fmt(d$change_per_365_days),
               `Sunday vs Monday` = ifelse(d$sunday_minus_monday >= 0, paste0("+", fmt(d$sunday_minus_monday)),
                                           fmt(d$sunday_minus_monday)),
               `Fit (R²)` = fmt(d$r_squared, 3), check.names = FALSE)
  }, width = "100%")

  output$bias_table <- renderUI({
    d <- dash$scores_by_group[dash$scores_by_group$evaluation_year == LATEST, ]
    d <- d[order(d$blood_type, match(d$method, METHODS)), ]
    sort_table(data.frame(Group = d$blood_type, Method = d$method, `Average bias` = d$mean_signed_error,
                          `Average error` = d$MAE, check.names = FALSE),
               c("Average bias", "Average error"))
  })

  output$monthly_explain <- renderUI({
    y <- as.integer(if (is.null(input$month_year)) LATEST else input$month_year)
    m <- if (is.null(input$month_methods) || !length(input$month_methods)) METHODS else input$month_methods
    explain_monthly(dash$monthly[dash$monthly$evaluation_year == y, ], m, PERIOD_LABEL[[as.character(y)]])
  })
  output$monthly_plot <- renderChart({
    y <- as.integer(if (is.null(input$month_year)) LATEST else input$month_year)
    m <- if (is.null(input$month_methods) || !length(input$month_methods)) METHODS
         else input$month_methods
    chart_monthly(dash$monthly[dash$monthly$evaluation_year == y, ], m)
  })
}

shinyApp(ui, server)
