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
  library(DT)
})
source("R/common.R")
# The full pipeline (download, cleaning, forecasting) is only needed when no
# precomputed results exist. The published site ships without it, so the
# browser never downloads readr, janitor or tidyr.
if (file.exists("R/analysis.R")) source("R/analysis.R")
source("R/content.R")
source("R/charts.R")

load_dashboard <- function(source = Sys.getenv("KVBD_SOURCE", "auto")) {
  if (identical(source, "auto") && file.exists("app_data/dashboard.rds")) {
    return(readRDS("app_data/dashboard.rds"))
  }
  if (identical(source, "report")) return(build_report_data())
  build_live_data()
}

dash <- load_dashboard()
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

dt_table <- function(df, digits_cols = NULL, digits = 2, page_length = 10) {
  out <- datatable(df, rownames = FALSE, class = "display compact",
                   options = list(pageLength = page_length, dom = "tip", scrollX = TRUE,
                                  autoWidth = FALSE))
  if (length(digits_cols)) out <- formatRound(out, digits_cols, digits)
  out
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

weekday_totals <- dash$panel |>
  group_by(date) |> summarise(total = sum(donations), .groups = "drop") |>
  mutate(weekday = WEEKDAYS[wday(date, week_start = 1)]) |>
  group_by(weekday) |> summarise(mean_total = mean(total), .groups = "drop")
BUSIEST_DAY <- weekday_totals$weekday[which.max(weekday_totals$mean_total)]
QUIETEST_DAY <- weekday_totals$weekday[which.min(weekday_totals$mean_total)]
BUSY_RATIO <- max(weekday_totals$mean_total) / min(weekday_totals$mean_total)

mix_year <- if (is.null(PARTIAL_YEAR)) END_YEAR else END_YEAR - 1L
mix_latest <- dash$blood_mix[dash$blood_mix$year == mix_year, ]
O_SHARE <- 100 * mix_latest$share[mix_latest$blood_type == "O"]

pct_text <- function(x) paste0(fmt(x, 0), "%")
low_all <- dash$low_errors[dash$low_errors$evaluation_year == LATEST &
                             dash$low_errors$day_category == "Low collection", ] |>
  group_by(method) |>
  summarise(over = sum(days * overprediction_percent) / sum(days), .groups = "drop")
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
WEEK_CHANGE <- if (is.na(wk)) "n/a" else paste0(if (wk >= 0) "+" else "\u2212", fmt(abs(wk), 1), "%")

LIMITS_MAP <- list(
  list(title = "Tested on the past", sub = "Scored on days that already happened; the methods were chosen using 2006–2025, so older years aren't a fresh test"),
  list(title = "Overlapping tests", sub = "Test years share their past data, so they aren't fully separate checks"),
  list(title = "3-year window untested", sub = "Three years for the recent mean was not compared with other window lengths"),
  list(title = "No outside factors", sub = "No weather, holidays, campaigns, blood stock or hospital demand, and no ranges of uncertainty"),
  list(title = "Zero days kept", sub = "Days with no donations may be closures or reporting gaps"),
  list(title = "Data can change", sub = "Official figures can be revised between daily refreshes"))

method_phrase <- function(m) c("Seven-day baseline" = "seven-day lag",
                               "Weekday average" = "full-history weekday mean",
                               "Linear regression" = "linear regression",
                               "Recent weekday average" = "recent weekday mean")[[m]]
change_words <- function(now, before) {
  ch <- pct_change(now, before)
  if (is.na(ch)) return("")
  sprintf("%s%% %s than the same week last year", fmt(abs(ch), 1), if (ch >= 0) "more" else "fewer")
}

# --- Building blocks ----------------------------------------------------------------

# Numbers that count up when their page opens (see www/interactions.js).
count_span <- function(x, digits = 0) {
  span(class = "countup", `data-count` = x, `data-decimals` = digits,
       if (digits == 0) fmt_int(x) else fmt(x, digits))
}

# Highlight the terms a reader should remember: method names and key phrases.
KEY_TERMS <- c(METHODS, "recent weekday average", "linear regression", "seven-day lag",
               "full-history weekday mean", "low-collection days", "too high on every one", "most accurate method")
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
                span(class = "flip-hint", icon("rotate"), "Details"))),
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
chips <- function(id, label, choices, selected) {
  div(class = "control",
      span(class = "control-label", label),
      checkboxGroupInput(id, NULL, choices, selected = selected, inline = TRUE) |>
        tagAppendAttributes(class = "chips"))
}

# Each page opens like a chapter: its number and name, then the finding as
# the headline (computed from the data), then what it means.
chapter_head <- function(id, headline, subtitle = NULL, stat = NULL) {
  sct <- SECTIONS[[match(id, vapply(SECTIONS, `[[`, "", "id"))]]
  div(class = "chapter-head stage-1",
      div(class = "chapter-copy",
          div(class = "chapter-eyebrow",
              span(class = "chapter-badge", icon(sct$icon)),
              span(class = "chapter-name", sct$label)),
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
big_stat <- function(value, label) {
  tagList(div(class = "big-stat-value", value), div(class = "big-stat-label", label))
}
# A word with a dotted underline that explains itself on hover, focus or tap.
term <- function(text, key) {
  g <- GLOSSARY[[key]]
  span(class = "term", tabindex = "0", role = "button", `data-term` = g[1], `data-def` = g[2], text,
       .noWS = "outside")
}

# A line of narration that carries the reader from one panel to the next.
narrate <- function(...) p(class = "narration stage-3", ...)

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
panel <- function(title, ..., sub = NULL, controls = NULL, class = "", footer = NULL, read = NULL) {
  div(class = paste("panel", class),
      div(class = "panel-head",
          div(class = "panel-titles",
              h3(class = "panel-title", title),
              if (!is.null(sub)) p(class = "panel-sub", sub)),
          if (!is.null(controls)) div(class = "panel-controls", controls)),
      if (!is.null(read)) div(class = "how-read", span(class = "how-read-tag", "How to read this"), span(read)),
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

SECTIONS <- list(
  list(id = "overview", label = "Overview", icon = "droplet"),
  list(id = "live", label = "Live tracker", icon = "tower-broadcast"),
  list(id = "compare", label = "Compare methods", icon = "scale-balanced"),
  list(id = "low", label = "Low-collection days", icon = "arrow-trend-down"),
  list(id = "patterns", label = "Patterns", icon = "chart-line"),
  list(id = "blood101", label = "Blood 101: Malaysia", icon = "hand-holding-medical"),
  list(id = "about", label = "About the data", icon = "circle-info"),
  list(id = "export", label = "Export reports", icon = "file-export")
)

rail <- tags$nav(
  class = "rail", `aria-label` = "Sections",
  div(class = "rail-brand",
      span(class = "rail-logo", span(icon("droplet"))),
      span(class = "rail-label rail-title", "Klang Valley", tags$br(), "Donations")),
  div(class = "rail-items",
      div(class = "rail-indicator", `aria-hidden` = "true"),
      lapply(seq_along(SECTIONS), function(i) {
        sct <- SECTIONS[[i]]
        tags$button(
          class = paste("rail-item", if (i == 1) "is-active"), type = "button",
          `data-target` = sct$id, `aria-label` = sct$label, title = sct$label,
          `aria-current` = if (i == 1) "page" else NULL,
          span(class = "rail-icon", icon(sct$icon)),
          span(class = "rail-label", sct$label))
      }))
)

quick_actions <- div(
  class = "quick-actions",
  tags$button(class = "qa-toggle", type = "button",
              `aria-expanded` = "false", `aria-controls` = "qa-menu",
              `aria-label` = "Quick actions", icon("bolt")),
  div(class = "qa-menu", id = "qa-menu", role = "menu",
      tags$button(class = "qa-item", type = "button", role = "menuitem",
                  `data-target` = "live", icon("tower-broadcast"), span("Latest data")),
      tags$button(class = "qa-item", type = "button", role = "menuitem",
                  `data-target` = "compare", icon("scale-balanced"), span("Compare methods")),
      tags$button(class = "qa-item", type = "button", role = "menuitem",
                  `data-target` = "blood101", icon("hand-holding-medical"),
                  span("Can I donate?")),
      tags$button(class = "qa-item", type = "button", role = "menuitem",
                  `data-open` = "sources", icon("book-open"), span("Sources & notes")),
      tags$button(class = "qa-item", type = "button", role = "menuitem",
                  `data-target` = "overview", icon("droplet"), span("Back to overview")))
)

status_pill <- div(
  class = "status-pill",
  span(class = "live-dot", `aria-hidden` = "true"),
  span(strong("Live data"), paste0(" to ", fmt_date(DATA_END))),
  span(class = "status-sep", `aria-hidden` = "true"),
  span(class = "status-quiet", paste0("Updated ", format(dash$built, "%e %b") |> trimws()))
)


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
      p(class = "cf-note", note))
}

region_view <- function(r) {
  st <- dash$states[!is.na(dash$states$region) & dash$states$region == r, ]
  st <- st[order(-st$last30), ]
  mx <- max(dash$states$last30, 1)
  tagList(
    div(class = "state-grid",
        lapply(seq_len(nrow(st)), function(i)
          div(class = "state-card", style = sprintf("--i:%d", i - 1),
              div(class = "state-name", st$state[i]),
              div(class = "state-num", fmt_int(st$last30[i])),
              div(class = "state-label", paste("donations in the 30 days to", fmt_date(DATA_END))),
              div(class = "state-track",
                  div(class = "state-fill", style = sprintf("--w:%.1f%%", 100 * st$last30[i] / mx))),
              div(class = "state-ytd", sprintf("%s so far in %s", fmt_int(st$ytd[i]), END_YEAR))))),
    if (r == "Central") tagList(
      h4(class = "centre-head", "PDN's fixed donation centres"),
      div(class = "centre-grid",
          lapply(DONATION_CENTRES, function(ct)
            div(class = "centre",
                span(class = "centre-icon", icon("location-dot")),
                div(class = "centre-name", ct$name),
                div(class = "centre-area", ct$area),
                div(class = "centre-hours", ct$hours)))))
    else p(class = "region-note", icon("circle-info"),
           paste("PDN's list of fixed donation centres covers the Klang Valley only. Elsewhere,",
                 "the donations above were collected at Ministry of Health blood collection sites",
                 "and mobile campaigns; check PDN or the nearest state hospital for times and",
                 "locations.")))
}

source("R/reports.R", local = TRUE)
source("R/visuals.R", local = TRUE)

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
  theme = bs_theme(version = 5, bg = "#FBF5F3", fg = "#2A1219", primary = "#A3213F",
                   base_font = font_collection("-apple-system", "SF Pro Text", "Inter",
                                               "Segoe UI", "system-ui", "sans-serif"),
                   heading_font = font_collection("-apple-system", "SF Pro Display", "Inter",
                                                  "Segoe UI", "system-ui", "sans-serif")),
  title = "Klang Valley Blood Donation Forecasts",
  tags$head(
    tags$meta(name = "description", content = paste(
      "Daily blood donation collections in the Klang Valley, Malaysia: live data,",
      "four forecasting methods compared, and general blood donation information.")),
    tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = paste0(
      "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght,SOFT@9..144,300..700,50",
      "&family=Inter:wght@300;400;600;700&display=swap")),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "interactions.js", defer = NA)
  ),
  div(class = "canvas-glow", `aria-hidden` = "true"),
  div(
    class = "app-shell",
    rail,
    tags$main(
      class = "stage",
      navset_hidden(
        id = "pages",

        # Overview ------------------------------------------------------------------
        nav_panel_hidden(
          "overview",
          div(
            class = "hero-grid",
            div(class = "intro stage-1",
                div(class = "intro-copy",
                    div(class = "intro-top",
                        div(class = "chapter-eyebrow",
                            span(class = "chapter-badge", icon("droplet")),
                            span(class = "chapter-name", "Overview")),
                        status_pill),
                    h1("How well can past collections predict tomorrow's donations?"),
                    p(class = "intro-lead",
                      "Four forecasting methods, tested every day on recorded blood donations ",
                      "in Selangor and W.P. Kuala Lumpur."),
                    p(class = "intro-scope", icon("circle-info"),
                      "Forecasts of donations collected, not of blood demand, stock or shortage."),
                    div(class = "intro-actions",
                        tags$button(class = "btn-pill btn-primary-solid", type = "button",
                                    `data-target` = "live", "See the latest data",
                                    icon("arrow-right")),
                        tags$button(class = "btn-pill btn-quiet", type = "button",
                                    `data-target` = "blood101", "Can I donate?"))),
                ascii_drop(),
                tags$svg(class = "ecg", viewBox = "0 0 600 60", preserveAspectRatio = "none",
                         `aria-hidden` = "true",
                         tags$path(d = paste("M0 34 H210 l12 0 l10 -22 l12 44 l10 -30 l8 8",
                                             "H330 l8 -6 l8 6 H600")))),
            div(class = "stat-grid stage-2",
                flip_card(
                  paste0("Latest day, ", format(DATA_END, "%e %b") |> trimws()),
                  count_span(dash$latest$latest_total), "donations recorded",
                  "By blood group",
                  tagList(p(class = "flip-back-text",
                            paste0("Selangor and W.P. Kuala Lumpur combined, on ", fmt_date(DATA_END), ".")),
                          mini_bars(latest_groups, highlight = names(which.max(latest_groups)))),
                  "tone-hero"),
                flip_card(
                  "Recent weekday mean", count_span(latest_mae("Recent weekday average"), 2),
                  paste("average error,", LATEST_SHORT), "Recent weekday mean",
                  method_back("Recent weekday average",
                              "Each day predicted from the same weekday over the last three complete years."),
                  "tone-best", badge = rank_badge("Recent weekday average")),
                flip_card(
                  "Linear regression", count_span(latest_mae("Linear regression"), 2),
                  paste("average error,", LATEST_SHORT), "Linear regression",
                  method_back("Linear regression", "Weekday plus a straight-line trend, per blood group."),
                  badge = rank_badge("Linear regression")),
                flip_card(
                  "Seven-day lag", count_span(latest_mae("Seven-day baseline"), 2),
                  paste("average error,", LATEST_SHORT), "Seven-day lag",
                  method_back("Seven-day baseline", "The count from one week earlier."),
                  badge = rank_badge("Seven-day baseline")))
          ),
          narrate("The story in five parts. Start anywhere, or read them in order."),
          div(
            class = "story-strip stage-3", role = "list",
            story_card("live", "This week", WEEK_CHANGE,
                       "donations in the past seven days, against the same week last year"),
            story_card("compare", "The winner", paste0("\u2212", pct_text(BEST_GAIN)),
                       sprintf("error with the %s, against repeating last week's count",
                               method_phrase(BEST_METHOD))),
            story_card("low", "The blind spot", pct_text(LOW_OVER_PCT),
                       sprintf("of %d low-collection days forecast too high by the best method",
                               LOW_DAYS)),
            story_card("patterns", "The rhythm", BUSIEST_DAY,
                       sprintf("is the busiest day, with %s times a %s's donations",
                               fmt(BUSY_RATIO, 1), QUIETEST_DAY)),
            story_card("blood101", "Your part", "3 months",
                       "the minimum gap between donations. Donors can be aged 17 to 70")),
          narrate("First, the scoreboard: how far off each method has been, period by period."),
          div(
            class = "overview-grid",
            panel("How far off each method was, year by year",
                  sub = tagList("The ", term("average error", "error"), " of each method in each ",
                                term("test period", "test_period"), ", with all four blood groups together."),
                  read = paste("Each bar is how many donations a method's forecast missed by, on an average",
                               "day for one blood group. Shorter is better. Hover a bar for the exact numbers."),
                  class = "stage-3 chart-panel",
                  plotlyOutput("overview_plot", height = "400px"),
                  footer = paste("Each period's averages and regression are fitted only on earlier",
                                 "years. Training windows overlap, so the periods are not independent tests.")),
            div(class = "findings-panel stage-3",
                h3(class = "panel-title", "What the data shows"),
                tags$ul(class = "findings",
                        lapply(seq_along(dash$findings), function(i)
                          tags$li(style = sprintf("--i:%d", i - 1), emphasise(dash$findings[[i]])))),
                wave("wave-findings"))),
          chapter_nav("overview")
        ),

        # Live tracker -------------------------------------------------------------
        nav_panel_hidden(
          "live",
          chapter_head("live",
                       paste0(fmt_int(dash$latest$latest_total), " donations on ",
                              format(DATA_END, "%e %B") |> trimws(), "."),
                       paste0("The past week brought ", fmt_int(dash$latest$last7), " donations, ",
                              change_words(dash$latest$last7, dash$latest$last7_prev_year),
                              ". Below: the last 60 days, and what each method expects next.")),
          div(class = "kpi-row stage-2",
              kpi("Latest day", count_span(dash$latest$latest_total),
                  paste("donations recorded on", fmt_date(DATA_END)), "kpi-hero"),
              kpi("Last 7 days", count_span(dash$latest$last7),
                  uiOutput("live_change7", inline = TRUE)),
              kpi(paste0(END_YEAR, " so far"), count_span(dash$latest$ytd),
                  uiOutput("live_change_ytd", inline = TRUE))),
          panel("The last 60 days and the next seven",
                sub = tagList("The dark line is what was really recorded; the paler lines are each method's ",
                              term("forecast", "forecast"), "."),
                read = paste("Where a pale line sits close to the dark line, that method got the day right.",
                             "The shaded strip on the right is the next seven days, still to come. Hover any day",
                             "to see every number."),
                controls = seg("live_group", "Blood group", ABO_GROUPS, "O"),
                class = "stage-3 chart-panel",
                plotlyOutput("live_plot", height = "440px"),
                footer = "The seven-day lag can only look one week ahead, so the forecast window is seven days."),
          narrate("Seven days out, the recent weekday mean expects this. Each daily refresh ",
                  "replaces one forecast with the real count."),
          panel("Forecasts for the next seven days",
                sub = paste("Expected donations from the recent weekday mean, the method with the",
                            "lowest error so far. Like every method, it overestimates unusually low days."),
                class = "stage-3 next-panel",
                tableOutput("next_table")),
          chapter_nav("live")
        ),

        # Compare ------------------------------------------------------------------
        nav_panel_hidden(
          "compare",
          chapter_head("compare",
                       sprintf("The %s wins, by %s.", method_phrase(BEST_METHOD), pct_text(BEST_GAIN)),
                       sprintf(paste("In %s its forecasts were off by %s donations per blood group",
                                     "per day, against %s for simply repeating last week's count."),
                               LATEST_LABEL, fmt(BEST_MAE), fmt(LAG_MAE)),
                       stat = big_stat(fmt(BEST_MAE, 1), "average daily error, best method")),
          div(class = "control-bar stage-2",
              seg("cmp_year", "Test period", PERIOD_CHOICES, as.character(LATEST)),
              chips("cmp_methods", "Methods", METHODS, METHODS)),
          panel("Error by blood group",
                read = paste("One small chart per blood group. Each bar is a method's average error for that",
                             "group: shorter is better. Hover a bar for the big-miss error and the number of days."),
                sub = tagList("The same ", term("average error", "error"), ", split by ",
                              term("ABO group", "abo"), ".", span(class = "phone-only", " Left to right in each group: 7-day lag, weekday mean, regression, recent mean.")),
                class = "stage-3 chart-panel", plotlyOutput("cmp_group_plot", height = "620px")),
          panel("Scores", class = "stage-3",
                sub = tagList("Every number behind the charts. ", term("Big-miss error", "rmse"), " and ",
                              term("average bias", "bias"), " are explained when you hover them."),
                DTOutput("cmp_table")),
          narrate("Averages hide the day-to-day story. Here is how each forecast tracked the ",
                  "real count, one day at a time."),
          panel("Recorded and forecast, day by day",
                read = paste("The dark line with dots is the real count for each day; the paler lines are the",
                             "forecasts. The closer a line hugs the dark one, the better that method did."),
                controls = tagList(
                  seg("cmp_group", "Blood group", ABO_GROUPS, "O"),
                  div(class = "control",
                      span(class = "control-label", "Dates"),
                      dateRangeInput("cmp_dates", NULL,
                                     start = max(DATA_END - 30, min(dash$predictions$date)),
                                     end = DATA_END,
                                     min = min(dash$predictions$date), max = DATA_END))),
                class = "stage-3 chart-panel",
                plotlyOutput("cmp_daily_plot", height = "420px"),
                footer = paste("The seven-day baseline uses the count observed a week earlier, so",
                               "it is a rolling benchmark rather than a forecast issued at the start",
                               "of the period.")),
          chapter_nav("compare")
        ),

        # Low-collection days ------------------------------------------------------
        nav_panel_hidden(
          "low",
          chapter_head("low",
                       LOW_HEADLINE,
                       tagList(term("Low-collection days", "low_day"),
                               " are the quietest quarter of days for each blood group. ",
                               sprintf(paste("On them, the %s was off by %s donations a day, against %s on",
                                             "other days. This looks back at what happened; it is not an",
                                             "early warning."),
                                       method_phrase(BEST_METHOD), fmt(LOW_MAE, 1), fmt(OTHER_MAE, 1))),
                       stat = big_stat(pct_text(LOW_OVER_PCT),
                                       sprintf("of %d low days forecast too high, %s", LOW_DAYS, LATEST_SHORT))),
          div(class = "control-bar stage-2",
              seg("low_year", "Test period", PERIOD_CHOICES, as.character(LATEST))),
          panel("Error on low-collection days and other days",
                read = paste("For each method, the dark bar is its average error on the quietest days and the",
                             "light bar is its error on all other days. A dark bar much taller than its light",
                             "bar means the method struggles when donations drop."),
                sub = tagList("Dark bars: ", term("low-collection days", "low_day"), ". Light bars: all other days.", span(class = "phone-only", " Left to right in each group: 7-day lag, weekday mean, regression, recent mean.")),
                class = "stage-3 chart-panel", plotlyOutput("low_plot", height = "620px")),
          narrate(LOW_WHY),
          div(class = "two-up stage-3",
              panel("Low-day lines", tableOutput("cutoff_table"),
                    sub = tagList("For each blood group, a day counts as low at or below this count: the ",
                                  term("lower quartile", "quartile"), " of earlier years."),
                    footer = "A lower-quartile count is not a clinical shortage threshold."),
              panel("Why the forecasts aim too high", DTOutput("min_pred_table"),
                    sub = tagList("If a method's lowest ", term("forecast", "forecast"),
                                  " of the whole period is still above the low-day line, it can never predict a day that quiet."))),
          panel("Low-day errors", class = "stage-3", DTOutput("low_table"),
                sub = tagList("How often each method's forecast was ", term("too high", "overpredict"),
                              " on low days, and by how much."),
                footer = paste("'Average overshoot' is how far above the real count the forecast was,",
                               "counting zero on days it was not too high.")),
          chapter_nav("low")
        ),

        # Patterns -------------------------------------------------------------------
        nav_panel_hidden(
          "patterns",
          chapter_head("patterns",
                       sprintf("%s is the busiest day. Group O gives %s of donations.",
                               BUSIEST_DAY, pct_text(O_SHARE)),
                       sprintf(paste("Twenty years of recorded donations, 2006 to %s. On an average %s",
                                     "the Klang Valley records %s times as many donations as on a %s."),
                               fmt_date(DATA_END), BUSIEST_DAY, fmt(BUSY_RATIO, 1), QUIETEST_DAY)),
          div(class = "control-bar stage-2",
              chips("pattern_groups", "Blood groups", ABO_GROUPS, ABO_GROUPS),
              div(class = "control control-wide",
                  span(class = "control-label", "Years"),
                  sliderInput("pattern_years", NULL, min = 2006, max = END_YEAR,
                              value = c(2006, END_YEAR), sep = "", step = 1, ticks = FALSE))),
          panel("Recorded donations, month by month", sub = "Stacked by blood group. Hover a month for exact counts.",
                read = paste("The height of the whole shape is all donations that month; each colour band is",
                             "one blood group. Bumps show busy and quiet months."),
                class = "stage-3 chart-panel", plotlyOutput("weekly_plot", height = "400px")),
          narrate("Zoom in on a single week and the rhythm is clear."),
          div(class = "two-up stage-3",
              panel("Mean donations by weekday", class = "chart-panel",
                    read = "Taller bars are busier weekdays. Each colour is one blood group.",
                    plotlyOutput("weekday_plot", height = "360px")),
              panel("Mean donations a day, by year", class = "chart-panel",
                    read = "Each line is one blood group; rising lines mean more donations a day than before.",
                    plotlyOutput("annual_plot", height = "360px"))),
          narrate(MIX_LINE),
          panel("Blood-type mix of donations",
                read = "Each column is one year and adds up to 100%. The bands show each group's share.",
                sub = paste("Share of recorded Klang Valley donations by ABO group each year. This",
                            "describes who donated, not how common each type is in the population."),
                class = "stage-3 chart-panel", plotlyOutput("mix_plot", height = "380px")),
          chapter_nav("patterns")
        ),

        # Blood 101 -----------------------------------------------------------------
        nav_panel_hidden(
          "blood101",
          chapter_head("blood101",
                       "Every forecast on this site is someone's donation.",
                       paste("What it takes to give blood in Malaysia, summarised from Pusat Darah",
                             "Negara (National Blood Centre). Staff confirm eligibility on the day;",
                             "check PDN for the latest requirements.")),
          section(
            "Can I donate?",
            sub = "The basics PDN checks before every donation.",
            viz(mindmap(c("Can I", "donate?"),
                        lapply(ELIGIBILITY, function(e) list(title = e$title, sub = e$value, tip = e$detail)),
                        label = paste(vapply(ELIGIBILITY, function(e) paste0(e$title, ": ", e$value), ""),
                                      collapse = "; ")),
                div(class = "fact-grid",
                    lapply(ELIGIBILITY, function(e)
                      div(class = "fact",
                          span(class = "fact-icon", icon(e$icon)),
                          div(class = "fact-title", e$title),
                          div(class = "fact-value", e$value),
                          p(class = "fact-detail", e$detail))))),
            tags$details(class = "fact-details",
                tags$summary("Details for each requirement"),
                lapply(ELIGIBILITY, function(e)
                  div(class = "fact-line", strong(e$title), span(e$detail)))),
            div(class = "deferral",
                h4("Some reasons to wait"),
                tags$ul(lapply(DEFERRAL_EXAMPLES, function(d)
                  tags$li(strong(d$what), span(d$wait)))),
                p(class = "viz-note", "Examples only. ",
                  tags$button(class = "link-btn", type = "button", `data-open` = "sources",
                              "Sources & notes")))),
          narrate("On the day, a donation follows the same six steps."),
          section("How a donation works",
                  viz(journey(Map(function(t, s) list(title = t, sub = s), DONATION_STEP_TITLES, DONATION_STEP_NOTES),
                              label = paste(DONATION_STEPS, collapse = " ")),
                      tags$ol(class = "steps", lapply(DONATION_STEPS, tags$li)))),
          section("After donating",
                  viz(mindmap(c("After", "you give"),
                              Map(function(t, s, k) list(title = t, sub = s, art = k),
                                  AFTERCARE_TITLES, AFTERCARE, AFTERCARE_ART),
                              label = paste(AFTERCARE, collapse = " ")),
                      tags$ul(class = "aftercare", lapply(AFTERCARE, tags$li)))),
          narrate("Where to go, wherever you are in Malaysia."),
          section(
            "Where blood is collected",
            sub = paste("Pick a region. Each state shows the donations recorded there in the official",
                        "data; the Central region also lists PDN's fixed donation centres."),
            div(class = "region-bar",
                seg("region_pick", "Region", names(MALAYSIA_REGIONS), "Central")),
            uiOutput("region_result")),
          narrate("And once it is given, whose blood can help whom?"),
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
                          "Sources & notes"))),
          chapter_nav("blood101")
        ),

        # About ------------------------------------------------------------------------
        nav_panel_hidden(
          "about",
          chapter_head("about",
                       "How the forecasts are tested, and what they cannot tell you.",
                       paste("The methods, what the regression learned, a full audit of the raw data",
                             "and the limits of a retrospective evaluation.")),
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
                        p("Mean for the blood group and weekday over the three complete years before the evaluation year."))))),
          narrate(sprintf("What did the regression learn from 2006 to %d? Associations, not causes.",
                          LATEST - 1L)),
          div(class = "two-up stage-3",
              panel("What the regression learned", tableOutput("coef_table"),
                    sub = tagList("Each ", term("coefficient", "coefficient"), " in plain words: how much donations",
                                  " grow each year, and how much busier a Sunday is than a Monday. ",
                                  term("Fit (R²)", "r2"), " shows how much of the pattern it captures."),
                    footer = "The Sunday contrast holds the time index fixed."),
              panel("Too high or too low on average?", DTOutput("bias_table"),
                    sub = tagList(term("Average bias", "bias"), ": the real count minus the forecast. ",
                                  "Positive means the forecasts ran low; negative means they ran high."),
                    footer = paste("Near-zero averages mean positive and negative errors cancel;",
                                   "they do not mean the daily forecasts are accurate."))),
          panel("How the error moves through the year",
                sub = paste("Each point is a method's average forecast error for one month: how many",
                            "donations per blood group per day the forecast was off by. Higher means",
                            "a harder month to predict. Hover a month to compare methods."),
                controls = tagList(
                  seg("month_year", "Test period", PERIOD_CHOICES, as.character(LATEST)),
                  chips("month_methods", "Methods", METHODS,
                        c("Seven-day baseline", "Linear regression", "Recent weekday average"))),
                class = "stage-3 chart-panel",
                plotlyOutput("monthly_plot", height = "380px"),
                footer = "Describes when errors occurred; it cannot explain why."),
          narrate("Before trusting any of it, check the data itself."),
          div(class = "two-up stage-3",
              panel("Raw data checks", tableOutput("audit_table"),
                    sub = "Basic checks on the official file before anything is calculated: size, dates, gaps and duplicates."),
              panel("What goes into the forecasts", tableOutput("panel_table"),
                    sub = "The daily counts used: one row per blood group per day, for the Klang Valley.",
                    footer = "The ALL category is excluded from modelling to avoid double counting.")),
          narrate("And be clear about what this cannot show."),
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
                  tags$li("The three-year window for the recent weekday mean was not tuned or ",
                          "independently validated."),
                  tags$li("No forecast intervals, weather, holiday, campaign, inventory or ",
                          "hospital-demand data are used."),
                  tags$li("Zero counts are kept; they may be closures or reporting gaps."),
                  tags$li("The official data can be revised, so past numbers may change slightly ",
                          "between daily refreshes.")))),
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
                        seg("rep_period", "Test period", PERIOD_CHOICES, as.character(LATEST))),
                      conditionalPanel("input.report_type == 'data'",
                        seg("rep_group", "Blood group", c("All" = "all", setNames(ABO_GROUPS, ABO_GROUPS)), "all"),
                        div(class = "control",
                            span(class = "control-label", "Dates"),
                            dateRangeInput("rep_dates", NULL, start = DATA_END - 30, end = DATA_END,
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
    quick_actions,
    sources_drawer
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
  output$overview_plot <- renderPlotly(chart_pooled_mae(dash$scores_overall, PERIOD_LABEL))

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

  output$live_plot <- renderPlotly({
    g <- if (is.null(input$live_group)) "O" else input$live_group
    chart_live(dash$panel, dash$predictions, dash$next_days, g, DATA_END)
  })

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

  output$cmp_group_plot <- renderPlotly({
    chart_group_mae(filter(dash$scores_by_group, evaluation_year == cmp_year()), cmp_methods())
  })

  output$cmp_table <- renderDT({
    y <- cmp_year(); m <- cmp_methods()
    pooled <- dash$scores_overall |>
      filter(evaluation_year == y, method %in% m) |>
      mutate(blood_type = "All groups")
    bind_rows(pooled, filter(dash$scores_by_group, evaluation_year == y, method %in% m)) |>
      arrange(blood_type != "All groups", blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method, `Days scored` = observations,
                `Average error (MAE)` = MAE, `Big-miss error (RMSE)` = RMSE,
                `Average bias` = mean_signed_error) |>
      dt_table(c("Average error (MAE)", "Big-miss error (RMSE)", "Average bias"), page_length = 8)
  })

  output$cmp_daily_plot <- renderPlotly({
    req(input$cmp_dates)
    g <- if (is.null(input$cmp_group)) "O" else input$cmp_group
    p <- chart_daily(dash$predictions, g, cmp_methods(), input$cmp_dates[1], input$cmp_dates[2])
    validate(need(!is.null(p), "No evaluation days in the selected range."))
    p
  })

  # Low-collection days ------------------------------------------------------------------
  low_y <- reactive(as.integer(if (is.null(input$low_year)) LATEST else input$low_year))

  output$low_plot <- renderPlotly(chart_low_days(filter(dash$low_errors, evaluation_year == low_y())))

  output$cutoff_table <- renderTable({
    y <- low_y()
    low_days <- dash$low_errors |>
      filter(evaluation_year == y, day_category == "Low collection") |>
      distinct(blood_type, days, mean_actual)
    dash$low_cutoffs |>
      filter(evaluation_year == y) |>
      left_join(low_days, by = "blood_type") |>
      transmute(`Blood group` = blood_type,
                `Low-day line (≤)` = fmt(low_cutoff),
                `Low days` = ifelse(is.na(days), 0L, days),
                `Average on low days` = ifelse(is.na(mean_actual), "–", fmt(mean_actual)))
  }, width = "100%")

  output$low_table <- renderDT({
    dash$low_errors |>
      filter(evaluation_year == low_y(), day_category == "Low collection") |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method, `Low days` = days,
                `Average error` = MAE,
                `Days forecast too high (%)` = overprediction_percent,
                `Average overshoot` = mean_excess) |>
      dt_table(c("Average error", "Days forecast too high (%)", "Average overshoot"), page_length = 8)
  })

  output$min_pred_table <- renderDT({
    dash$min_predictions |>
      filter(evaluation_year == low_y()) |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Group` = blood_type, Method = method,
                `Lowest forecast` = minimum_prediction,
                `Low-day line` = low_cutoff,
                `Can never reach it?` = ifelse(all_predictions_above_cutoff, "Yes", "No")) |>
      dt_table(c("Lowest forecast", "Low-day line"), page_length = 8)
  })

  # Patterns ---------------------------------------------------------------------------------
  pattern_panel <- reactive({
    g <- if (is.null(input$pattern_groups) || !length(input$pattern_groups)) ABO_GROUPS
         else input$pattern_groups
    yrs <- if (is.null(input$pattern_years)) c(2006, END_YEAR) else input$pattern_years
    dash$panel |> filter(blood_type %in% g, year(date) >= yrs[1], year(date) <= yrs[2])
  })

  output$weekly_plot <- renderPlotly(chart_month_totals(pattern_panel(), DATA_END))
  output$weekday_plot <- renderPlotly({
    pattern_panel() |>
      mutate(weekday = factor(WEEKDAYS[wday(date, week_start = 1)], levels = WEEKDAYS)) |>
      group_by(blood_type, weekday) |>
      summarise(mean_donations = mean(donations), .groups = "drop") |>
      chart_weekday()
  })
  output$annual_plot <- renderPlotly(chart_annual(annual_summary(pattern_panel())))
  output$mix_plot <- renderPlotly(chart_mix(dash$blood_mix, PARTIAL_YEAR))

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
    dash$coefficients |>
      transmute(`Blood group` = blood_type,
                `Change per year` = fmt(change_per_365_days),
                `Sunday vs Monday` = ifelse(sunday_minus_monday >= 0, paste0("+", fmt(sunday_minus_monday)), fmt(sunday_minus_monday)),
                `Fit (R²)` = fmt(r_squared, 3))
  }, width = "100%")

  output$bias_table <- renderDT({
    dash$scores_by_group |>
      filter(evaluation_year == LATEST) |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Group` = blood_type, Method = method,
                `Average bias` = mean_signed_error, `Average error` = MAE) |>
      dt_table(c("Average bias", "Average error"), page_length = 8)
  })

  output$monthly_plot <- renderPlotly({
    y <- as.integer(if (is.null(input$month_year)) LATEST else input$month_year)
    m <- if (is.null(input$month_methods) || !length(input$month_methods)) METHODS
         else input$month_methods
    chart_monthly(filter(dash$monthly, evaluation_year == y), m)
  })
}

shinyApp(ui, server)
