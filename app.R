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
  library(ggplot2)
  library(DT)
})
source("R/analysis.R")
source("R/game.R")
source("R/content.R")

load_dashboard <- function(source = Sys.getenv("KVBD_SOURCE", "auto")) {
  if (identical(source, "report")) return(build_report_data())
  if (identical(source, "auto") && file.exists("app_data/dashboard.rds")) {
    return(readRDS("app_data/dashboard.rds"))
  }
  build_live_data()
}

dash <- load_dashboard()
LATEST <- dash$latest_year
PERIOD_LABEL <- setNames(dash$periods$label, dash$periods$evaluation_year)
PERIOD_CHOICES <- setNames(dash$periods$evaluation_year, dash$periods$label)
LATEST_LABEL <- PERIOD_LABEL[[as.character(LATEST)]]
DATA_END <- dash$panel_summary$last_date
game_rows <- game_pool(dash$predictions, dash$low_cutoffs,
                       game_hint_table(dash$model_data, LATEST), LATEST)
period_label_vec <- function(years) unname(PERIOD_LABEL[as.character(years)])

# --- Formatting and theme -----------------------------------------------------

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_int <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_date <- function(x) format(x, "%d %b %Y")
pct_change <- function(now, before) if (before > 0) 100 * (now - before) / before else NA_real_

# One colour family: blood reds from blush to wine. Methods run light to
# dark in the order they improve, so the strongest red is the best method.
INK <- "#4A1426"
PASTEL_METHOD <- c("Seven-day baseline" = "#F2C2CC",
                   "Weekday average" = "#E3909F",
                   "Linear regression" = "#C45A72",
                   "Recent weekday average" = "#7D2440")
PASTEL_GROUP <- c("A" = "#F2B3C0", "AB" = "#E07D93", "B" = "#B8445F", "O" = "#6E1D37")

base_theme <- theme_minimal(base_size = 13, base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#F0DCE1", linewidth = 0.4),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(colour = INK, face = "bold", size = 12),
        axis.text = element_text(colour = "#8A6470"),
        axis.title = element_text(colour = INK),
        plot.subtitle = element_text(colour = "#8A6470"),
        text = element_text(colour = INK),
        plot.title.position = "plot",
        legend.position = "bottom")

# All plots render on a transparent background so the glass cards show through.
glass_plot <- function(expr) {
  renderPlot(substitute(expr), env = parent.frame(), quoted = TRUE,
             bg = "transparent", res = 110)
}

dt_table <- function(df, digits_cols = NULL, digits = 2, page_length = 10) {
  out <- datatable(df, rownames = FALSE,
                   options = list(pageLength = page_length, dom = "tip", scrollX = TRUE))
  if (length(digits_cols)) out <- formatRound(out, digits_cols, digits)
  out
}

source_link <- function(src) a(src$title, href = src$url, target = "_blank", rel = "noopener")

# --- UI building blocks ---------------------------------------------------------

scope_note <- div(
  class = "scope-note", role = "note",
  strong("Scope: "),
  "these forecasts estimate recorded donation collections. They are not ",
  "predictions of blood demand, hospital inventory, shortage risk or clinical need."
)

# A headline card that tilts under the pointer and flips (click, Enter or
# Space) to explain what its number means.
flip_card <- function(label, value, unit, back_title, back_text, tone) {
  div(
    class = paste("flip-card", tone), tabindex = "0", role = "button",
    `aria-pressed` = "false",
    `aria-label` = paste(label, "- press to see what this number means"),
    div(class = "flip-tilt",
        div(class = "flip-inner",
            div(class = "flip-face flip-front glass",
                div(class = "flip-glare"),
                div(class = "flip-label", label),
                div(class = "flip-value", value),
                div(class = "flip-unit", unit),
                div(class = "flip-hint", "Flip for details")),
            div(class = "flip-face flip-back glass", `aria-hidden` = "true",
                h3(back_title),
                p(back_text))))
  )
}

# Numbers that count up when their page opens (see www/interactions.js).
count_span <- function(x, digits = 0) {
  span(class = "countup", `data-count` = x, `data-decimals` = digits,
       if (digits == 0) fmt_int(x) else fmt(x, digits))
}

# Highlight the terms a reader should remember: method names and key phrases.
KEY_TERMS <- c(METHODS, "recent weekday average", "linear regression", "seven-day lag",
               "full-history weekday mean", "low-collection days", "overpredicts every one")
emphasise <- function(text) {
  pattern <- paste0("(", paste(unique(KEY_TERMS), collapse = "|"), ")")
  parts <- regmatches(text, gregexpr(pattern, text, ignore.case = TRUE), invert = NA)[[1]]
  tagList(lapply(seq_along(parts), function(i) if (i %% 2 == 0) tags$mark(parts[i]) else parts[i]))
}

finding <- function(colour, text) tags$li(style = paste0("--dot:", colour), emphasise(text))

# ASCII-art blood drop. Characters get denser towards the lit side of the
# drop; CSS fills them with a pastel mesh gradient.
ascii_drop <- function(width = 32, height = 24) {
  ramp <- strsplit(" .:-=+*%#@", "")[[1]]
  rows <- character(height)
  for (y in seq_len(height)) {
    line <- character(width)
    for (x in seq_len(width)) {
      u <- (x - (width + 1) / 2) / (width / 2)
      v <- (y - 1) / (height - 1)
      # Teardrop: a round bottom (ellipse centred at v = 0.52) that narrows
      # to a point at the top.
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
  list(id = "play", label = "Beat the forecast", icon = "gamepad"),
  list(id = "compare", label = "Compare methods", icon = "scale-balanced"),
  list(id = "low", label = "Low-collection days", icon = "arrow-trend-down"),
  list(id = "patterns", label = "Patterns", icon = "chart-line"),
  list(id = "blood101", label = "Blood 101: Malaysia", icon = "hand-holding-medical"),
  list(id = "about", label = "About the data", icon = "circle-info")
)

rail <- tags$nav(
  class = "rail glass", `aria-label` = "Sections",
  div(class = "rail-brand",
      span(class = "rail-logo", icon("droplet")),
      span(class = "rail-label rail-title", "Klang Valley", tags$br(), "Donations")),
  div(class = "rail-items",
      div(class = "rail-indicator", `aria-hidden` = "true"),
      lapply(seq_along(SECTIONS), function(i) {
        sct <- SECTIONS[[i]]
        tags$button(
          class = paste("rail-item", if (i == 1) "is-active"), type = "button",
          `data-target` = sct$id, `aria-label` = sct$label,
          `aria-current` = if (i == 1) "page" else NULL,
          span(class = "rail-icon", icon(sct$icon)),
          span(class = "rail-label", sct$label))
      }))
)

# The level indicator is a blood drop that fills as XP builds towards the
# next level; the liquid surface is a gentle wave.
DROP_PATH <- "M22 3 C22 3 7 20 7 30 a15 15 0 0 0 30 0 C37 20 22 3 22 3 Z"
level_ring <- tags$svg(
  class = "level-drop", viewBox = "0 0 44 48", `aria-hidden` = "true",
  tags$defs(tags$clipPath(id = "drop-clip", tags$path(d = DROP_PATH))),
  tags$path(class = "drop-shell", d = DROP_PATH),
  tags$g(`clip-path` = "url(#drop-clip)",
         tags$g(class = "drop-liquid",
                tags$path(class = "drop-wave",
                          d = "M-44 6 q11 -5 22 0 t22 0 t22 0 t22 0 t22 0 V60 H-44 Z"))),
  tags$path(class = "drop-gloss", d = "M15 24 q-2 6 1 11")
)

hud <- div(
  class = "hud glass", role = "status", `aria-live` = "polite",
  div(class = "hud-level", level_ring,
      span(class = "hud-level-num", id = "hud-level", "1")),
  div(class = "hud-stat", span(class = "hud-num", id = "hud-xp", "0"),
      span(class = "hud-cap", "XP")),
  div(class = "hud-stat", span(class = "hud-num", id = "hud-streak", "0"),
      span(class = "hud-cap", icon("fire"), "streak")),
  div(class = "hud-stat", span(class = "hud-num", id = "hud-badges", "0"),
      span(class = "hud-cap", "badges"))
)

quick_actions <- div(
  class = "quick-actions",
  tags$button(class = "qa-toggle glass", type = "button",
              `aria-expanded` = "false", `aria-controls` = "qa-menu",
              `aria-label` = "Quick actions", icon("bolt")),
  div(class = "qa-menu", id = "qa-menu", role = "menu",
      tags$button(class = "qa-item glass", type = "button", role = "menuitem",
                  `data-action` = "play", icon("gamepad"), span("Play a round")),
      tags$button(class = "qa-item glass", type = "button", role = "menuitem",
                  `data-target` = "live", icon("tower-broadcast"), span("Latest data")),
      tags$button(class = "qa-item glass", type = "button", role = "menuitem",
                  `data-target` = "blood101", icon("hand-holding-medical"),
                  span("Can I donate?")),
      tags$button(class = "qa-item glass", type = "button", role = "menuitem",
                  `data-target` = "overview", icon("droplet"), span("Back to overview")))
)

page_head <- function(title, subtitle = NULL) {
  div(class = "page-head stage-1",
      h2(class = "page-title", title),
      if (!is.null(subtitle)) p(class = "page-sub", subtitle))
}

# A titled block of content with consistent vertical rhythm.
section <- function(title, ..., sub = NULL, class = "stage-2") {
  tags$section(class = paste("block", class),
               div(class = "block-head",
                   h3(class = "block-title", title),
                   if (!is.null(sub)) p(class = "block-sub", sub)),
               ...)
}

kpi <- function(label, value_ui, detail_ui = NULL, tone = "") {
  div(class = paste("kpi glass", tone),
      div(class = "kpi-label", label),
      div(class = "kpi-value", value_ui),
      if (!is.null(detail_ui)) div(class = "kpi-detail", detail_ui))
}

badge_tile <- function(id) {
  b <- BADGES[[id]]
  div(class = "badge-tile", id = paste0("badge-", id), `data-badge` = id,
      span(class = "badge-icon", icon(b$icon)),
      span(class = "badge-name", b$name),
      span(class = "badge-text", b$text))
}

latest_mae <- function(m) {
  s <- dash$scores_overall
  s$MAE[s$evaluation_year == LATEST & s$method == m]
}

# --- UI -------------------------------------------------------------------------

ui <- page(
  theme = bs_theme(version = 5, bg = "#F5F0FA", fg = INK, primary = "#7E5AA8",
                   secondary = "#B8B3C9",
                   base_font = font_collection("-apple-system", "SF Pro Text", "Inter",
                                               "Segoe UI", "system-ui", "sans-serif"),
                   heading_font = font_collection("-apple-system", "SF Pro Display", "Inter",
                                                  "Segoe UI", "system-ui", "sans-serif")),
  title = "Klang Valley Blood Donation Forecasts",
  tags$head(
    tags$meta(name = "description", content = paste(
      "Daily blood donation collections in the Klang Valley, Malaysia: live data,",
      "four forecasting methods compared, and general blood donation information.")),
    tags$link(rel = "icon", href = "data:,"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(rel = "stylesheet", href = paste0(
      "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght,SOFT@9..144,500..800,100&family=Inter:wght@400;500;600;700;800&display=swap")),
    tags$link(rel = "stylesheet", href = "styles.css"),
    tags$script(src = "interactions.js", defer = NA)
  ),
  div(class = "mesh", `aria-hidden` = "true"),
  div(
    class = "app-shell",
    rail,
    tags$main(
      class = "stage",
      div(class = "topbar",
          div(class = "topbar-meta",
              span(class = "live-dot", `aria-hidden` = "true"),
              paste0("Data to ", fmt_date(DATA_END), ". Refreshed ",
                     format(dash$built, "%d %b"), " from data.gov.my")),
          hud),
      navset_hidden(
        id = "pages",

        # Overview ----------------------------------------------------------------
        nav_panel_hidden(
          "overview",
          div(
            class = "bento",
            div(class = "glass intro span-2 row-2 stage-1",
                div(class = "intro-copy",
                    h1("How well can past collections predict tomorrow's donations?"),
                    p("Four simple forecasting methods compared on daily recorded donations ",
                      "in Selangor and W.P. Kuala Lumpur, by ABO blood group, updated with ",
                      "the newest official data."),
                    scope_note,
                    div(class = "intro-actions",
                        tags$button(class = "btn-pill btn-primary-glass", type = "button",
                                    `data-action` = "play", icon("gamepad"),
                                    "Try to beat the forecast"),
                        tags$button(class = "btn-pill btn-ghost-glass", type = "button",
                                    `data-target` = "live", icon("tower-broadcast"),
                                    "See the latest data"))),
                ascii_drop(),
                tags$svg(class = "ecg", viewBox = "0 0 600 60", preserveAspectRatio = "none",
                         `aria-hidden` = "true",
                         tags$path(d = paste("M0 34 H210 l12 0 l10 -22 l12 44 l10 -30 l8 8",
                                             "H330 l8 -6 l8 6 H600")))),
            div(class = "stage-2 drop-card",
                style = sprintf("--fill:%.2f", min(1, dash$latest$latest_total /
                  max(tail(dash$latest$daily$total, 60)))),
                flip_card(
              paste0("Latest day, ", format(DATA_END, "%d %b")),
              count_span(dash$latest$latest_total), "donations recorded",
              "What this counts",
              paste("All four ABO groups, Selangor and W.P. Kuala Lumpur combined, on",
                    fmt_date(DATA_END), "- the newest day in the official data."),
              "tone-hero")),
            div(class = "stage-2", flip_card(
              "Recent weekday mean, MAE",
              count_span(latest_mae("Recent weekday average"), 2),
              paste("donations per group-day,", LATEST_LABEL),
              "Recent weekday mean",
              paste("Predicts each day as the average for that blood group and weekday over",
                    "the three complete years before it."), "tone-best")),
            div(class = "stage-2", flip_card(
              "Linear regression, MAE",
              count_span(latest_mae("Linear regression"), 2),
              paste("donations per group-day,", LATEST_LABEL),
              "Linear regression",
              paste("One model per blood group using weekday and a straight-line time trend,",
                    "fitted on all earlier years."), "tone-glass")),
            div(class = "stage-2", flip_card(
              "Seven-day lag, MAE",
              count_span(latest_mae("Seven-day baseline"), 2),
              paste("donations per group-day,", LATEST_LABEL),
              "Seven-day lag",
              paste("Uses the count recorded one week earlier: the simplest benchmark the",
                    "other methods have to beat."), "tone-glass")),
            card(class = "span-3 row-2 stage-3",
                 card_header("Pooled error by evaluation period (lower is better)"),
                 plotOutput("overview_plot", height = 380),
                 card_footer(
                   "Each period's averages and regression are fitted only on earlier years. ",
                   "Training windows overlap, so the periods are not independent tests.")),
            card(class = "row-2 stage-3",
                 card_header("What the data shows"),
                 tags$ul(class = "findings",
                         Map(finding,
                             rep(unname(PASTEL_METHOD), length.out = length(dash$findings)),
                             dash$findings)))
          )
        ),

        # Live tracker --------------------------------------------------------------
        nav_panel_hidden(
          "live",
          page_head("Live tracker",
                    paste0("The newest official data, to ", fmt_date(DATA_END),
                           ", with forecasts for the next seven days.")),
          div(class = "kpi-row stage-2",
              kpi("Latest day", count_span(dash$latest$latest_total),
                  paste("donations recorded on", fmt_date(DATA_END)), "kpi-hero"),
              kpi("Last 7 days", count_span(dash$latest$last7),
                  uiOutput("live_change7", inline = TRUE)),
              kpi(paste0(format(DATA_END, "%Y"), " so far"), count_span(dash$latest$ytd),
                  uiOutput("live_change_ytd", inline = TRUE))),
          section(
            "Recent days and the next seven",
            sub = paste("Recorded donations for the last 60 days, each method's forecast for",
                        "those days, and its forecast for the coming week (shaded)."),
            div(class = "chip-row", role = "radiogroup", `aria-label` = "Blood group",
                lapply(ABO_GROUPS, function(g)
                  tags$button(class = paste("group-pick", if (g == "O") "is-on"),
                              type = "button", `data-group` = g, role = "radio",
                              `aria-checked` = if (g == "O") "true" else "false",
                              span(class = paste0("dot group-", g)), paste("Group", g)))),
            card(plotOutput("live_plot", height = 400),
                 card_footer("The seven-day lag can only look one week ahead, so the ",
                             "forecast window is seven days."))),
          section(
            "Forecasts for the next seven days",
            sub = paste("Expected donations by the recent weekday mean, the method with the",
                        "lowest error so far. Like every method here, it overestimates",
                        "unusually low days. The chart above shows all four methods."),
            card(class = "next-card", tableOutput("next_table")))
        ),

        # Game -----------------------------------------------------------------------
        nav_panel_hidden(
          "play",
          page_head("Beat the forecast",
                    paste("Each round is a real recorded day from", LATEST_LABEL,
                          "- guess how many donations that blood group recorded, then see",
                          "whether you beat the four forecasting methods.")),
          div(
            class = "play-grid",
            div(class = "glass round-card stage-2",
                uiOutput("game_round"),
                div(class = "guess-block",
                    div(class = "guess-readout",
                        span(class = "guess-num", id = "guess-num", "0"),
                        span(class = "guess-cap", "donations")),
                    sliderInput("guess", NULL, min = 0, max = 400, value = 0,
                                step = 1, ticks = FALSE, width = "100%")),
                div(class = "round-actions",
                    actionButton("lock_guess", "Lock in guess", icon = icon("lock"),
                                 class = "btn-pill btn-primary-glass"),
                    actionButton("new_round", "New day", icon = icon("shuffle"),
                                 class = "btn-pill btn-ghost-glass"))),
            div(class = "glass result-card stage-3", uiOutput("game_result")),
            div(class = "glass badges-card stage-3",
                h3(class = "block-title", "Badges"),
                div(class = "badge-grid", lapply(names(BADGES), badge_tile)),
                uiOutput("game_stats"))
          ),
          p(class = "fine-print",
            "Typical ranges come from the three years before each round's year, the window ",
            "the recent weekday mean uses. Rounds replay past days.")
        ),

        # Compare ------------------------------------------------------------------
        nav_panel_hidden(
          "compare",
          page_head("Compare methods",
                    "Errors by period and blood group, and the daily forecasts behind them."),
          layout_sidebar(
            class = "stage-2",
            sidebar = sidebar(
              width = 300,
              selectInput("cmp_year", "Evaluation period", PERIOD_CHOICES, selected = LATEST),
              checkboxGroupInput("cmp_methods", "Methods", METHODS, selected = METHODS),
              hr(),
              h6("Daily detail"),
              selectInput("cmp_group", "Blood group", ABO_GROUPS, selected = "O"),
              dateRangeInput("cmp_dates", "Dates",
                             start = max(DATA_END - 30, min(dash$predictions$date)),
                             end = DATA_END,
                             min = min(dash$predictions$date), max = DATA_END)
            ),
            card(card_header("MAE by blood group"), plotOutput("cmp_group_plot", height = 380)),
            card(card_header("Scores"), DTOutput("cmp_table")),
            card(card_header("Actual versus predicted daily donations"),
                 plotOutput("cmp_daily_plot", height = 380),
                 card_footer("Dark line: recorded donations. The seven-day baseline uses the ",
                             "count observed a week earlier, so it is a rolling benchmark ",
                             "rather than a forecast issued at the start of the period."))
          )
        ),

        # Low-collection days -------------------------------------------------------
        nav_panel_hidden(
          "low",
          page_head("Low-collection days",
                    paste("Days at or below each group's lower quartile over the years before",
                          "the period. An after-the-fact error audit, not an early warning.")),
          div(class = "toolbar stage-2",
              selectInput("low_year", "Evaluation period", PERIOD_CHOICES,
                          selected = LATEST, width = "260px")),
          layout_columns(
            class = "stage-2",
            col_widths = c(4, 8),
            card(card_header("Thresholds"), tableOutput("cutoff_table"),
                 card_footer("A lower-quartile count is not a clinical shortage threshold.")),
            card(card_header("MAE on low-collection days and other days"),
                 plotOutput("low_plot", height = 340))
          ),
          card(class = "stage-3", card_header("Low-day errors"), DTOutput("low_table"),
               card_footer("'Mean excess forecast' averages only the amount by which the ",
                           "forecast exceeded the actual count, counting zero when it did not.")),
          card(class = "stage-3", card_header("Why overprediction is unavoidable here"),
               DTOutput("min_pred_table"),
               card_footer("When a method's smallest prediction for a group is above that ",
                           "group's threshold, it must overpredict every low day."))
        ),

        # Patterns -----------------------------------------------------------------
        nav_panel_hidden(
          "patterns",
          page_head("Patterns",
                    paste0("Recorded donations from 2006 to ", fmt_date(DATA_END), ".")),
          layout_sidebar(
            class = "stage-2",
            sidebar = sidebar(
              width = 300,
              checkboxGroupInput("pattern_groups", "Blood groups", ABO_GROUPS,
                                 selected = ABO_GROUPS),
              sliderInput("pattern_years", "Years", min = 2006,
                          max = as.integer(format(DATA_END, "%Y")),
                          value = c(2006, as.integer(format(DATA_END, "%Y"))),
                          sep = "", step = 1)
            ),
            card(card_header("Weekly recorded donations"),
                 plotOutput("weekly_plot", height = 360),
                 card_footer("Weekly totals; partial first and last weeks contain fewer days.")),
            layout_columns(
              col_widths = c(6, 6),
              card(card_header("Mean daily donations by weekday"),
                   plotOutput("weekday_plot", height = 320)),
              card(card_header("Mean daily donations by year"),
                   plotOutput("annual_plot", height = 320)))
          ),
          section(
            "Blood-type mix of donations",
            sub = paste("Share of recorded Klang Valley donations by ABO group each year.",
                        "This describes who donated, not how common each blood type is in",
                        "the population."),
            class = "stage-3",
            card(plotOutput("mix_plot", height = 340)))
        ),

        # Blood 101 ------------------------------------------------------------------
        nav_panel_hidden(
          "blood101",
          page_head("Blood 101: Malaysia",
                    paste("General information about donating blood in Malaysia, summarised",
                          "from Pusat Darah Negara (National Blood Centre). Staff confirm",
                          "eligibility on the day; check PDN for the latest requirements.")),
          section(
            "Can I donate?",
            div(class = "fact-grid masonry",
                lapply(ELIGIBILITY, function(e)
                  div(class = "fact glass",
                      span(class = "fact-icon", icon(e$icon)),
                      div(class = "fact-title", e$title),
                      div(class = "fact-value", e$value),
                      p(class = "fact-detail", e$detail)))),
            div(class = "glass deferral",
                h4("Some reasons to wait"),
                tags$ul(lapply(DEFERRAL_EXAMPLES, function(d)
                  tags$li(strong(d$what), span(d$wait)))),
                p(class = "fine-print",
                  "Examples only. PDN's A–Z list covers many more conditions and medicines."))),
          layout_columns(
            class = "stage-3",
            col_widths = c(6, 6),
            section("How a donation works", class = "",
                    tags$ol(class = "steps glass", lapply(DONATION_STEPS, tags$li))),
            section("After donating", class = "",
                    tags$ul(class = "aftercare glass", lapply(AFTERCARE, tags$li)))
          ),
          section(
            "Where to donate in the Klang Valley",
            sub = paste("PDN's fixed donation centres. Mobile blood drives run across the",
                        "region; PDN publishes their locations."),
            class = "stage-3",
            div(class = "centre-grid masonry",
                lapply(DONATION_CENTRES, function(ct)
                  div(class = "centre glass",
                      span(class = "centre-icon", icon("location-dot")),
                      div(class = "centre-name", ct$name),
                      div(class = "centre-area", ct$area),
                      div(class = "centre-hours", ct$hours))))),
          section(
            "Blood type compatibility explorer",
            sub = paste("Pick a blood type to see whose red cells it can receive and whom it",
                        "can give red cells to."),
            class = "stage-3",
            div(class = "glass compat",
                div(class = "type-grid", role = "radiogroup", `aria-label` = "Blood type",
                    lapply(BLOOD_TYPES, function(t)
                      tags$button(class = paste("type-pick", if (t == "O+") "is-on"),
                                  type = "button", `data-type` = t, role = "radio",
                                  `aria-checked` = if (t == "O+") "true" else "false", t))),
                uiOutput("compat_result"),
                p(class = "fine-print",
                  "A simplified educational guide to red-cell ABO and RhD compatibility. ",
                  "Real transfusions are matched in a laboratory and decided by clinicians. ",
                  "The donation data records ABO groups only, not RhD."))),
          div(class = "sources glass stage-3",
              h4("Sources"),
              tags$ul(
                tags$li(source_link(PDN_SOURCES$booklet)),
                tags$li(source_link(PDN_SOURCES$az)),
                tags$li(source_link(PDN_SOURCES$centres)),
                tags$li(source_link(COMPAT_SOURCES$nhsbt)),
                tags$li(source_link(COMPAT_SOURCES$hemaq))),
              p(class = "fine-print", "Summarised in our own words; accessed 2 November 2026."))
        ),

        # About -----------------------------------------------------------------------
        nav_panel_hidden(
          "about",
          page_head("About the data and methods"),
          section(
            "Forecasting methods",
            card(tags$dl(class = "methods",
              tags$dt("Seven-day baseline"),
              tags$dd("The count recorded for the same blood group seven calendar days earlier."),
              tags$dt("Weekday average (full history)"),
              tags$dd("Mean for the blood group and weekday over all years before the evaluation year."),
              tags$dt("Linear regression"),
              tags$dd("A separate lm(donations ~ weekday + time_index) for each blood group, fitted on all earlier years."),
              tags$dt("Recent weekday average"),
              tags$dd("Mean for the blood group and weekday over the three complete years before the evaluation year.")))),
          section(
            "What the regression learned",
            sub = paste0("Fitted on 2006 to ", LATEST - 1L, ". Associations in historical data, ",
                         "not causal effects."),
            layout_columns(
              col_widths = c(6, 6),
              card(card_header("Coefficients"), tableOutput("coef_table"),
                   card_footer("The Sunday contrast holds the time index fixed.")),
              card(card_header("Signed error (actual minus predicted)"), DTOutput("bias_table"),
                   card_footer("Near-zero averages mean positive and negative errors cancel; ",
                               "they do not mean the daily forecasts are accurate."))),
            card(card_header("Monthly MAE"),
                 div(class = "toolbar",
                     selectInput("month_year", "Period", PERIOD_CHOICES, selected = LATEST,
                                 width = "240px"),
                     selectInput("month_methods", "Methods", METHODS, multiple = TRUE,
                                 selected = c("Seven-day baseline", "Linear regression",
                                              "Recent weekday average"))),
                 plotOutput("monthly_plot", height = 340))),
          section(
            "Data audit",
            layout_columns(
              col_widths = c(6, 6),
              card(card_header("Raw data checks"), tableOutput("audit_table")),
              card(card_header("Analysis panel"), tableOutput("panel_table"),
                   card_footer("The ALL category is excluded from modelling to avoid ",
                               "double counting.")))),
          section(
            "Validation limits",
            card(tags$ul(class = "limits",
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
          section(
            "Source, licence and origin",
            card(
              p("Government of Malaysia. ", em("Daily Blood Donations by Blood Group & State"),
                ". data.gov.my. ",
                a("Catalogue page", href = "https://data.gov.my/data-catalogue/blood_donations_state",
                  target = "_blank", rel = "noopener"),
                ". Open data under the ",
                a("Creative Commons Attribution 4.0 licence",
                  href = "https://creativecommons.org/licenses/by/4.0/",
                  target = "_blank", rel = "noopener"), "."),
              p("Records come from the Blood Bank Information System of 22 main Ministry of ",
                "Health collection sites, which cover most but not all national donations. ",
                "W.P. Kuala Lumpur includes Putrajaya and mobile campaigns around Selangor, ",
                "so the two regions are combined."),
              p("This dashboard extends the BloodReady AI analysis submitted for FIT5145 ",
                "Foundations of Data Science (Monash University, 2026). The repository's tests ",
                "check that the methods reproduce that report's results on its frozen snapshot.")))
        )
      )
    ),
    quick_actions
  )
)

# --- Server ---------------------------------------------------------------------

server <- function(input, output, session) {

  # Charts adapt to their rendered width: on narrow screens the method legend
  # stacks in one column and crowded value labels are hidden.
  narrow <- function(id) {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    !is.null(w) && w < 640
  }
  method_legend <- function(id) {
    if (narrow(id)) guides(fill = guide_legend(ncol = 1), colour = guide_legend(ncol = 1))
    else guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))
  }
  theme_for <- function(id) {
    if (!narrow(id)) return(base_theme)
    base_theme + theme(legend.text = element_text(size = 8.5),
                       axis.title.y = element_text(size = 8),
                       legend.key.size = unit(0.8, "lines"))
  }
  period_lab <- function(y) PERIOD_LABEL[[as.character(y)]]

  # --- Navigation ---------------------------------------------------------------
  observeEvent(input$rail_go, {
    req(input$rail_go %in% vapply(SECTIONS, `[[`, "", "id"))
    nav_select("pages", input$rail_go)
  })

  # --- Game state ---------------------------------------------------------------
  st <- reactiveValues(xp = 0L, rounds = 0L, streak = 0L, best_streak = 0L,
                       beaten_total = 0L, badges = character(),
                       visited = "overview", round = NULL, result = NULL)

  push_hud <- function(gained = 0L, new_badges = character(), source = NULL) {
    lv <- level_for(st$xp)
    session$sendCustomMessage("hud", list(
      xp = st$xp, level = lv$level, progress = lv$progress,
      streak = st$streak, badges = length(st$badges),
      held = as.list(st$badges), gained = gained, source = source,
      new_badges = lapply(new_badges, function(id) list(id = id, name = BADGES[[id]]$name))
    ))
  }

  award_badges <- function(ids) {
    ids <- setdiff(ids, st$badges)
    if (length(ids)) st$badges <- c(st$badges, ids)
    ids
  }

  observeEvent(input$pages, {
    if (input$pages %in% st$visited) return()
    st$visited <- c(st$visited, input$pages)
    st$xp <- st$xp + EXPLORER_XP
    new <- if (length(st$visited) == length(SECTIONS)) award_badges("explorer") else character()
    push_hud(EXPLORER_XP, new, source = "rail")
  }, ignoreInit = TRUE)

  start_round <- function() {
    st$round <- new_round(game_rows)
    st$result <- NULL
    r <- st$round
    hi <- max(10, ceiling(r$typical_high * 2.5 / 10) * 10)
    updateSliderInput(session, "guess", min = 0, max = hi,
                      value = round((r$typical_low + r$typical_high) / 2))
  }
  observeEvent(TRUE, start_round(), once = TRUE)
  observeEvent(input$new_round, start_round())
  observeEvent(input$qa_play, { if (!is.null(st$result)) start_round() })

  observeEvent(input$lock_guess, {
    req(st$round, is.null(st$result), is.numeric(input$guess))
    r <- st$round
    res <- score_guess(r, input$guess)
    st$result <- res
    st$rounds <- st$rounds + 1L
    st$beaten_total <- st$beaten_total + length(res$beaten)
    st$streak <- if ("Linear regression" %in% res$beaten) st$streak + 1L else 0L
    st$best_streak <- max(st$best_streak, st$streak)
    st$xp <- st$xp + as.integer(res$xp)
    new <- award_badges(earned_badges(res, r, st$badges, st$streak))
    push_hud(as.integer(res$xp), new, source = "result")
  })

  output$game_round <- renderUI({
    r <- st$round
    req(r)
    yr <- as.integer(format(r$date, "%Y"))
    div(class = "round-head",
        span(class = paste0("group-chip group-", r$blood_type), span(r$blood_type)),
        div(class = "round-meta",
            div(class = "round-date", format(r$date, "%A %d %B %Y")),
            div(class = "round-hint",
                sprintf("Typical for group %s on a %s in %d–%d: %s–%s donations",
                        r$blood_type, r$weekday, yr - 3L, yr - 1L,
                        fmt(r$typical_low, 0), fmt(r$typical_high, 0)))))
  })

  output$game_result <- renderUI({
    res <- st$result
    r <- st$round
    if (is.null(res)) {
      return(div(class = "result-empty",
                 icon("hand-pointer"),
                 p("Set your guess and lock it in. The recorded count and every ",
                   "method's forecast appear here.")))
    }
    errs <- c("You" = res$player_error, res$method_error)
    scale_max <- max(errs, 1)
    rows <- lapply(names(errs), function(nm) {
      is_you <- nm == "You"
      beaten <- nm %in% res$beaten
      pred <- if (is_you) res$guess else r$predictions[[nm]]
      div(class = paste("race-row", if (is_you) "is-you", if (beaten) "is-beaten"),
          style = paste0("--w:", round(100 * errs[[nm]] / scale_max, 1), "%;",
                         "--c:", if (is_you) INK else PASTEL_METHOD[[nm]]),
          span(class = "race-name", nm),
          div(class = "race-track", div(class = "race-bar")),
          span(class = "race-val", sprintf("guess %s, off by %s", fmt(pred, 0), fmt(errs[[nm]], 0))),
          span(class = "race-mark", if (!is_you) (if (beaten) icon("check") else icon("xmark"))))
    })
    tagList(
      div(class = "result-head",
          div(class = "actual",
              span(class = "actual-cap", "Recorded"),
              span(class = "actual-num", `data-count` = r$actual, "0"),
              span(class = "actual-unit", "donations")),
          if (res$xp > 0) div(class = "xp-chip", id = "xp-chip",
                              icon("star"), sprintf("+%d XP", as.integer(res$xp)))
          else div(class = "xp-chip is-empty", icon("rotate-right"), "Try another day")),
      p(class = "result-line",
        if (length(res$beaten) == 0) "No methods beaten this time; the forecasts were closer."
        else if (length(res$beaten) == length(METHODS)) "Clean sweep: you beat all four methods."
        else sprintf("You beat %d of 4 methods%s.", length(res$beaten),
                     if (res$close) ", and landed within 10%" else ""),
        if (r$low_day) span(class = "low-flag", icon("arrow-trend-down"), "low-collection day")),
      div(class = "race", rows),
      p(class = "race-cap", "Bars show absolute error (shorter is better).")
    )
  })

  output$game_stats <- renderUI({
    div(class = "stats-row",
        div(class = "stat", span(class = "stat-num", st$rounds), span("rounds")),
        div(class = "stat", span(class = "stat-num", st$beaten_total), span("methods beaten")),
        div(class = "stat", span(class = "stat-num", st$best_streak), span("best streak")))
  })

  # --- Overview -----------------------------------------------------------------
  output$overview_plot <- glass_plot({
    dash$scores_overall |>
      mutate(period = factor(period_label_vec(evaluation_year), levels = dash$periods$label)) |>
      ggplot(aes(period, MAE, fill = factor(method, levels = METHODS))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      { if (!narrow("overview_plot"))
          geom_text(aes(label = fmt(MAE)), position = position_dodge(width = 0.8),
                    vjust = -0.4, size = 3.2) } +
      method_legend("overview_plot") +
      scale_fill_manual(values = PASTEL_METHOD, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = NULL, y = "MAE (donations per group-day)") +
      theme_for("overview_plot")
  })

  # --- Live tracker -------------------------------------------------------------
  change_ui <- function(now, before, what) {
    ch <- pct_change(now, before)
    if (is.na(ch)) return(span(what))
    span(class = if (ch >= 0) "up" else "down",
         icon(if (ch >= 0) "arrow-up" else "arrow-down"),
         sprintf("%s%% vs %s", fmt(abs(ch), 1), what))
  }
  output$live_change7 <- renderUI(change_ui(dash$latest$last7, dash$latest$last7_prev_year,
                                            "same week last year"))
  output$live_change_ytd <- renderUI(change_ui(dash$latest$ytd, dash$latest$ytd_prev_year,
                                               "same days last year"))

  live_group <- reactive(if (is.null(input$live_group)) "O" else input$live_group)

  output$live_plot <- glass_plot({
    g <- live_group()
    from <- DATA_END - 59
    actual <- dash$panel |> filter(blood_type == g, date >= from)
    past <- dash$predictions |> filter(blood_type == g, date >= from)
    ahead <- dash$next_days |> filter(blood_type == g)
    ggplot() +
      annotate("rect", xmin = DATA_END + 0.5, xmax = max(ahead$date) + 0.5,
               ymin = -Inf, ymax = Inf, fill = "#FBE1E6", alpha = 0.6) +
      geom_line(data = past, aes(date, prediction, colour = factor(method, levels = METHODS)),
                linewidth = 0.6, alpha = 0.85) +
      geom_line(data = ahead, aes(date, prediction, colour = factor(method, levels = METHODS)),
                linewidth = 0.9, linetype = "22") +
      geom_point(data = ahead, aes(date, prediction, colour = factor(method, levels = METHODS)),
                 size = 1.4) +
      geom_line(data = actual, aes(date, donations), colour = INK, linewidth = 0.9) +
      geom_point(data = actual, aes(date, donations), colour = INK, size = 1.1) +
      method_legend("live_plot") +
      scale_colour_manual(values = PASTEL_METHOD, name = NULL) +
      scale_x_date(date_labels = "%d %b") +
      labs(x = NULL, y = "Donations per day",
           subtitle = paste0("Blood group ", g, ". Dark line: recorded. Shaded: next 7 days.")) +
      theme_for("live_plot")
  })

  output$next_table <- renderTable({
    dash$next_days |>
      filter(method == "Recent weekday average") |>
      select(date, blood_type, prediction) |>
      tidyr::pivot_wider(names_from = blood_type, values_from = prediction) |>
      mutate(Total = A + AB + B + O) |>
      transmute(Day = format(date, "%a %d %b"),
                A = fmt_int(A), AB = fmt_int(AB), B = fmt_int(B), O = fmt_int(O),
                Total = fmt_int(Total))
  }, align = "lrrrrr", striped = FALSE, width = "100%")

  # --- Compatibility explorer ---------------------------------------------------
  output$compat_result <- renderUI({
    t <- if (is.null(input$bt_pick) || !input$bt_pick %in% BLOOD_TYPES) "O+" else input$bt_pick
    cmp <- compatibility_for(t)
    chips <- function(on) div(class = "type-chips",
      lapply(BLOOD_TYPES, function(x) span(class = paste("type-chip", if (x %in% on) "is-on"), x)))
    div(class = "compat-result",
        div(class = "compat-col",
            h4(sprintf("%s can receive red cells from", t)),
            chips(cmp$receives_from),
            p(class = "compat-note", sprintf("%d of 8 types", length(cmp$receives_from)))),
        div(class = "compat-col",
            h4(sprintf("%s can give red cells to", t)),
            chips(cmp$gives_to),
            p(class = "compat-note", sprintf("%d of 8 types", length(cmp$gives_to)))))
  })

  # --- Compare --------------------------------------------------------------------
  cmp_scores <- reactive({
    req(input$cmp_methods, input$cmp_year)
    dash$scores_by_group |>
      filter(evaluation_year == as.integer(input$cmp_year), method %in% input$cmp_methods)
  })

  output$cmp_group_plot <- glass_plot({
    ggplot(cmp_scores(), aes(factor(method, levels = METHODS), MAE,
                             fill = factor(method, levels = METHODS))) +
      geom_col(width = 0.7) +
      geom_text(aes(label = fmt(MAE, 1)), vjust = -0.4, size = 3.4) +
      facet_wrap(~ blood_type, ncol = 4, scales = "free_y") +
      method_legend("cmp_group_plot") +
      scale_fill_manual(values = PASTEL_METHOD, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "MAE (donations per day)",
           subtitle = paste0(period_lab(input$cmp_year), "; y-axes differ by group")) +
      theme_for("cmp_group_plot") + theme(axis.text.x = element_blank())
  })

  output$cmp_table <- renderDT({
    req(input$cmp_methods)
    pooled <- dash$scores_overall |>
      filter(evaluation_year == as.integer(input$cmp_year), method %in% input$cmp_methods) |>
      mutate(blood_type = "All (pooled)")
    bind_rows(pooled, cmp_scores()) |>
      arrange(blood_type != "All (pooled)", blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method, Days = observations, MAE, RMSE,
                `Mean signed error` = mean_signed_error) |>
      dt_table(c("MAE", "RMSE", "Mean signed error"), page_length = 20)
  })

  output$cmp_daily_plot <- glass_plot({
    req(input$cmp_methods, input$cmp_dates)
    rng <- input$cmp_dates
    shown <- dash$predictions |>
      filter(blood_type == input$cmp_group, method %in% input$cmp_methods,
             date >= rng[1], date <= rng[2])
    validate(need(nrow(shown) > 0, "No evaluation days in the selected range."))
    actual <- distinct(shown, date, donations)
    ggplot(shown, aes(date, prediction, colour = factor(method, levels = METHODS))) +
      geom_line(linewidth = 0.7) +
      geom_line(data = actual, aes(date, donations), inherit.aes = FALSE,
                colour = INK, linewidth = 0.9) +
      geom_point(data = actual, aes(date, donations), inherit.aes = FALSE,
                 colour = INK, size = 1.2) +
      method_legend("cmp_daily_plot") +
      scale_colour_manual(values = PASTEL_METHOD, name = NULL) +
      labs(x = NULL, y = "Donations per day", subtitle = paste("Blood group", input$cmp_group)) +
      theme_for("cmp_daily_plot")
  })

  # --- Low-collection days --------------------------------------------------------
  low_y <- reactive(as.integer(if (is.null(input$low_year)) LATEST else input$low_year))

  output$cutoff_table <- renderTable({
    y <- low_y()
    low_days <- dash$low_errors |>
      filter(evaluation_year == y, day_category == "Low collection") |>
      distinct(blood_type, days, mean_actual)
    dash$low_cutoffs |>
      filter(evaluation_year == y) |>
      left_join(low_days, by = "blood_type") |>
      transmute(`Blood group` = blood_type,
                `Threshold (≤)` = fmt(low_cutoff),
                `Low days` = ifelse(is.na(days), 0L, days),
                `Mean actual on low days` = ifelse(is.na(mean_actual), "–", fmt(mean_actual)))
  })

  output$low_plot <- glass_plot({
    dash$low_errors |>
      filter(evaluation_year == low_y()) |>
      ggplot(aes(factor(method, levels = METHODS), MAE,
                 fill = factor(method, levels = METHODS), alpha = day_category)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      facet_wrap(~ blood_type, ncol = 4, scales = "free_y") +
      method_legend("low_plot") +
      scale_fill_manual(values = PASTEL_METHOD, name = NULL) +
      scale_alpha_manual(values = c("Low collection" = 1, "Other days" = 0.35), name = NULL) +
      guides(alpha = "none") +
      labs(x = NULL, y = "MAE (donations per day)",
           subtitle = "Solid bars: low-collection days. Faded bars: other days.") +
      theme_for("low_plot") + theme(axis.text.x = element_blank())
  })

  output$low_table <- renderDT({
    dash$low_errors |>
      filter(evaluation_year == low_y(), day_category == "Low collection") |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method, `Low days` = days, MAE,
                `Overpredicted (%)` = overprediction_percent,
                `Mean excess forecast` = mean_excess) |>
      dt_table(c("MAE", "Overpredicted (%)", "Mean excess forecast"), page_length = 16)
  })

  output$min_pred_table <- renderDT({
    dash$min_predictions |>
      filter(evaluation_year == low_y()) |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method,
                `Smallest prediction` = minimum_prediction,
                `Low-day threshold` = low_cutoff,
                `All predictions above threshold` = ifelse(all_predictions_above_cutoff, "Yes", "No")) |>
      dt_table(c("Smallest prediction", "Low-day threshold"), page_length = 16)
  })

  # --- Patterns ---------------------------------------------------------------------
  pattern_panel <- reactive({
    req(input$pattern_groups)
    yrs <- input$pattern_years
    dash$panel |>
      filter(blood_type %in% input$pattern_groups, year(date) >= yrs[1], year(date) <= yrs[2])
  })

  output$weekly_plot <- glass_plot({
    weekly_totals(pattern_panel()) |>
      ggplot(aes(week_start, donations, colour = blood_type)) +
      geom_line(linewidth = 0.4) +
      scale_colour_manual(values = PASTEL_GROUP, name = "Blood group") +
      labs(x = NULL, y = "Donations per week") + base_theme
  })

  output$weekday_plot <- glass_plot({
    pattern_panel() |>
      mutate(weekday = factor(WEEKDAYS[wday(date, week_start = 1)], levels = WEEKDAYS)) |>
      group_by(blood_type, weekday) |>
      summarise(mean_donations = mean(donations), .groups = "drop") |>
      ggplot(aes(weekday, mean_donations, fill = blood_type)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = PASTEL_GROUP, name = "Blood group") +
      scale_x_discrete(labels = substr(WEEKDAYS, 1, 3)) +
      labs(x = NULL, y = "Mean daily donations") + base_theme
  })

  output$annual_plot <- glass_plot({
    annual_summary(pattern_panel()) |>
      ggplot(aes(year, mean_daily, colour = blood_type)) +
      geom_line() + geom_point(size = 1.5) +
      scale_colour_manual(values = PASTEL_GROUP, name = "Blood group") +
      labs(x = NULL, y = "Mean daily donations") + base_theme
  })

  output$mix_plot <- glass_plot({
    end_year <- as.integer(format(DATA_END, "%Y"))
    partial <- DATA_END < as.Date(paste0(end_year, "-12-31"))
    dash$blood_mix |>
      mutate(blood_type = factor(blood_type, levels = c("O", "B", "A", "AB")),
             year_lab = ifelse(year == end_year & partial, paste0(year, "*"), as.character(year))) |>
      ggplot(aes(factor(year_lab, levels = unique(year_lab)), share, fill = blood_type)) +
      geom_col(width = 0.8) +
      scale_fill_manual(values = PASTEL_GROUP, name = "Blood group") +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%"),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "Share of recorded donations",
           subtitle = if (partial) paste0("* ", end_year, " to ", fmt_date(DATA_END)) else NULL) +
      base_theme + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 9))
  })

  # --- About ------------------------------------------------------------------------
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
  })

  output$panel_table <- renderTable({
    p <- dash$panel_summary
    data.frame(
      Item = c("Regions combined", "Blood groups", "Period", "Group-days",
               "Distinct dates", "Zero group-days"),
      Value = c("Selangor + W.P. Kuala Lumpur", "A, AB, B, O",
                paste(p$first_date, "to", p$last_date), fmt_int(p$rows), fmt_int(p$dates),
                sprintf("%d (%.2f%%)", p$zero_values, 100 * p$zero_values / p$rows))
    )
  })

  output$coef_table <- renderTable({
    dash$coefficients |>
      transmute(`Blood group` = blood_type,
                `Trend per 365 days` = fmt(change_per_365_days),
                `Sunday − Monday` = fmt(sunday_minus_monday),
                `R²` = fmt(r_squared, 3))
  })

  output$bias_table <- renderDT({
    dash$scores_by_group |>
      filter(evaluation_year == LATEST) |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method,
                `Mean signed error` = mean_signed_error, MAE) |>
      dt_table(c("Mean signed error", "MAE"), page_length = 16)
  })

  output$monthly_plot <- glass_plot({
    req(input$month_methods)
    y <- as.integer(if (is.null(input$month_year)) LATEST else input$month_year)
    dash$monthly |>
      filter(evaluation_year == y, method %in% input$month_methods) |>
      mutate(month = as.Date(paste0(month, "-01"))) |>
      ggplot(aes(month, MAE, colour = factor(method, levels = METHODS))) +
      geom_line() + geom_point() +
      method_legend("monthly_plot") +
      scale_colour_manual(values = PASTEL_METHOD, name = NULL) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(x = NULL, y = "Pooled MAE (donations per group-day)",
           subtitle = "Describes when errors occurred; it cannot explain why") +
      theme_for("monthly_plot")
  })
}

shinyApp(ui, server)
