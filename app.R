# Klang Valley Blood Donation Forecast Dashboard
#
# An interactive view of the retrospective analysis from the BloodReady AI
# FIT5145 project. It forecasts *recorded blood donations* (collections) for
# Selangor + W.P. Kuala Lumpur by ABO group. It does not model blood demand,
# inventory, shortage risk or clinical need.
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

dash <- build_dashboard_data()
game_rows <- game_pool(dash$predictions, dash$low_cutoffs_2025,
                       game_hint_table(dash$model_data))

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")

# Pastel palette for the dashboard (the original Rmd colours live in R/analysis.R).
INK <- "#3B2F45"
PASTEL_METHOD <- c("Seven-day baseline" = "#E8A0AE",
                   "Weekday average" = "#B8B3C9",
                   "Linear regression" = "#86C5B0",
                   "Recent weekday average" = "#B79BE0")
PASTEL_GROUP <- c("A" = "#EE9AB4", "AB" = "#B9A2E3", "B" = "#8CC6DA", "O" = "#93CDA2")

base_theme <- theme_minimal(base_size = 13, base_family = "sans") +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#E4DAEC", linewidth = 0.4),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        strip.text = element_text(colour = INK, face = "bold", size = 12),
        axis.text = element_text(colour = "#6C5F78"),
        axis.title = element_text(colour = INK),
        plot.subtitle = element_text(colour = "#6C5F78"),
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
                   options = list(pageLength = page_length, dom = "tip",
                                  scrollX = TRUE))
  if (length(digits_cols)) out <- formatRound(out, digits_cols, digits)
  out
}

scope_note <- div(
  class = "scope-note", role = "note",
  strong("Scope: "),
  "these forecasts estimate recorded donation collections. They are not ",
  "predictions of blood demand, hospital inventory, shortage risk or ",
  "clinical need, and the evaluation is retrospective."
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

finding <- function(colour, ...) tags$li(style = paste0("--dot:", colour), ...)

# ASCII-art blood drop. Characters get denser towards the lit side of the
# drop; CSS fills them with a pastel mesh gradient.
ascii_drop <- function(width = 32, height = 24) {
  ramp <- strsplit(" .:-=+*%#@", "")[[1]]
  rows <- character(height)
  for (y in seq_len(height)) {
    line <- character(width)
    for (x in seq_len(width)) {
      u <- (x - (width + 1) / 2) / (width / 2)      # -1..1 across
      v <- (y - 1) / (height - 1)                    #  0..1 down
      # Teardrop: a round bottom (ellipse centred at v = 0.66) that narrows
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
  list(id = "play", label = "Beat the forecast", icon = "gamepad"),
  list(id = "compare", label = "Compare methods", icon = "scale-balanced"),
  list(id = "low", label = "Low-collection days", icon = "arrow-trend-down"),
  list(id = "patterns", label = "Patterns", icon = "chart-line"),
  list(id = "explain", label = "Model explanation", icon = "lightbulb"),
  list(id = "audit", label = "Data audit", icon = "clipboard-check"),
  list(id = "methods", label = "Methods & limits", icon = "book-open")
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

level_ring <- tags$svg(
  class = "level-ring", viewBox = "0 0 44 44", `aria-hidden` = "true",
  tags$circle(class = "ring-track", cx = "22", cy = "22", r = "18"),
  tags$circle(class = "ring-fill", cx = "22", cy = "22", r = "18",
              `stroke-dasharray` = "113.1", `stroke-dashoffset` = "113.1")
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
                  `data-target` = "compare", icon("scale-balanced"),
                  span("Compare 2025 results")),
      tags$button(class = "qa-item glass", type = "button", role = "menuitem",
                  `data-target` = "low", icon("arrow-trend-down"),
                  span("Low-collection days")),
      tags$button(class = "qa-item glass", type = "button", role = "menuitem",
                  `data-target` = "overview", icon("droplet"),
                  span("Back to overview")))
)

page_head <- function(title, subtitle = NULL) {
  div(class = "page-head stage-1",
      h2(class = "page-title", title),
      if (!is.null(subtitle)) p(class = "page-sub", subtitle))
}

badge_tile <- function(id) {
  b <- BADGES[[id]]
  div(class = "badge-tile", id = paste0("badge-", id), `data-badge` = id,
      span(class = "badge-icon", icon(b$icon)),
      span(class = "badge-name", b$name),
      span(class = "badge-text", b$text))
}

# --- UI -----------------------------------------------------------------------

ui <- page(
  theme = bs_theme(version = 5, bg = "#F5F0FA", fg = INK, primary = "#7E5AA8",
                   secondary = "#B8B3C9",
                   base_font = font_collection("-apple-system", "SF Pro Text",
                                               "Inter", "Segoe UI", "system-ui",
                                               "sans-serif"),
                   heading_font = font_collection("-apple-system", "SF Pro Display",
                                                  "Inter", "Segoe UI", "system-ui",
                                                  "sans-serif")),
  title = "Klang Valley Blood Donation Forecasts",
  tags$head(
    tags$link(rel = "icon", href = "data:,"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com",
              crossorigin = NA),
    tags$link(rel = "stylesheet", href = paste0(
      "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800",
      "&display=swap")),
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
              paste0("Analysis 2006–2025, data to ",
                     format(dash$audit$last_date, "%d %b %Y"))),
          hud),
      navset_hidden(
        id = "pages",

        nav_panel_hidden(
          "overview",
          div(
            class = "bento",
            div(class = "glass intro span-2 row-2 stage-1",
                div(class = "intro-copy",
                    h1("How well can past collections predict tomorrow's donations?"),
                    p("Four simple forecasting methods compared on daily recorded ",
                      "donations in Selangor and W.P. Kuala Lumpur, by ABO blood group, ",
                      "for 2023, 2024 and 2025."),
                    scope_note,
                    tags$button(class = "btn-pill btn-primary-glass", type = "button",
                                `data-action` = "play", icon("gamepad"),
                                "Try to beat the forecast")),
                ascii_drop()),
            div(class = "stage-2", flip_card(
              "Recent weekday mean, 2025 MAE",
              textOutput("headline_recent", inline = TRUE),
              "donations per group-day", "Recent weekday mean",
              paste("Predicts each day as the average for that blood group and",
                    "weekday over 2022–2024. Lowest pooled error in all three",
                    "evaluation years."), "tone-lilac")),
            div(class = "stage-2", flip_card(
              "Linear regression, 2025 MAE",
              textOutput("headline_lm", inline = TRUE),
              "donations per group-day", "Linear regression",
              paste("One model per blood group using weekday and a straight-line",
                    "time trend, fitted on 2006–2024."), "tone-mint")),
            div(class = "stage-2", flip_card(
              "Seven-day lag, 2025 MAE",
              textOutput("headline_lag", inline = TRUE),
              "donations per group-day", "Seven-day lag",
              paste("Uses the count recorded one week earlier. The simplest",
                    "benchmark: every other method has lower pooled error in every year."),
              "tone-rose")),
            div(class = "stage-2", flip_card(
              "Study panel", fmt_int(dash$panel_summary$rows),
              "group-days, 2006–2025", "What a group-day is",
              paste("One blood group on one date: 7,305 dates × 4 groups,",
                    "summed over the two regions. The ALL category is excluded."),
              "tone-peach")),
            card(class = "span-3 row-2 stage-3",
                 card_header("Pooled error by evaluation year (lower is better)"),
                 plotOutput("overview_plot", height = 380),
                 card_footer(
                   "Each year's averages and regression are fitted only on earlier ",
                   "years. Training windows overlap, so the three years are not ",
                   "independent tests.")),
            card(class = "row-2 stage-3",
                 card_header("What the analysis shows"),
                 tags$ul(
                   class = "findings",
                   finding(PASTEL_METHOD[["Recent weekday average"]],
                           "The recent weekday mean has the lowest pooled MAE and RMSE ",
                           "in each of 2023, 2024 and 2025."),
                   finding(PASTEL_METHOD[["Linear regression"]],
                           "Linear regression beats the seven-day lag for every blood ",
                           "group in all three years, and the full-history weekday mean ",
                           "for A, B and O."),
                   finding(PASTEL_GROUP[["AB"]],
                           "AB is the exception: the full-history weekday mean is ",
                           "slightly better than regression in every year, and in 2025 ",
                           "marginally better than the recent mean too."),
                   finding(PASTEL_METHOD[["Seven-day baseline"]],
                           "On 2025 low-collection days, regression and the recent mean ",
                           "make much larger errors than on other days. The full-history ",
                           "mean has the smallest low-day errors for A, B and O, but it ",
                           "still overpredicts every one of those days.")
                 ))
          )
        ),

        nav_panel_hidden(
          "play",
          page_head("Beat the forecast",
                    paste("Each round is a real 2025 day. Guess how many donations",
                          "that blood group recorded, then see whether you beat the",
                          "four forecasting methods.")),
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
                    actionButton("lock_guess", "Lock in guess",
                                 icon = icon("lock"), class = "btn-pill btn-primary-glass"),
                    actionButton("new_round", "New day", icon = icon("shuffle"),
                                 class = "btn-pill btn-ghost-glass"))),
            div(class = "glass result-card stage-3", uiOutput("game_result")),
            div(class = "glass badges-card stage-3",
                h3(class = "card-title", "Badges"),
                div(class = "badge-grid", lapply(names(BADGES), badge_tile)),
                uiOutput("game_stats"))
          ),
          p(class = "fine-print",
            "Typical ranges come from 2022–2024, the window the recent weekday mean uses. ",
            "Rounds replay past days; nothing here forecasts future collections.")
        ),

        nav_panel_hidden(
          "compare",
          page_head("Compare methods",
                    "Errors by year and blood group, and the daily forecasts behind them."),
          layout_sidebar(
            class = "stage-2",
            sidebar = sidebar(width = 300,
              selectInput("cmp_year", "Evaluation year", EVALUATION_YEARS,
                          selected = 2025),
              checkboxGroupInput("cmp_methods", "Methods", METHODS, selected = METHODS),
              hr(),
              h6("Daily detail"),
              selectInput("cmp_group", "Blood group", ABO_GROUPS, selected = "O"),
              dateRangeInput("cmp_dates", "Dates",
                             start = as.Date("2025-01-01"), end = as.Date("2025-01-31"),
                             min = as.Date("2023-01-01"), max = as.Date("2025-12-31"))
            ),
            card(card_header("MAE by blood group"),
                 plotOutput("cmp_group_plot", height = 380)),
            card(card_header("Scores"), DTOutput("cmp_table")),
            card(card_header("Actual versus predicted daily donations"),
                 plotOutput("cmp_daily_plot", height = 380),
                 card_footer("Dark line: recorded donations. The seven-day baseline ",
                             "uses the count observed a week earlier, so it is a ",
                             "rolling benchmark rather than a fixed forecast issued ",
                             "at the start of the year."))
          )
        ),

        nav_panel_hidden(
          "low",
          page_head("Low-collection days",
                    paste("2025 days at or below each group's 2006–2024 lower quartile.",
                          "An after-the-fact error audit, not an early warning.")),
          div(class = "stage-2", scope_note),
          layout_columns(
            class = "stage-2",
            col_widths = c(4, 8),
            card(card_header("Training-period thresholds"), tableOutput("cutoff_table"),
                 card_footer("A lower-quartile count is not a clinical shortage threshold.")),
            card(card_header("Low-day MAE by method"),
                 plotOutput("low_plot", height = 340))
          ),
          card(class = "stage-3", card_header("Low-day errors"), DTOutput("low_table"),
               card_footer("'Mean excess forecast' averages only the amount by which ",
                           "the forecast exceeded the actual count, counting zero ",
                           "when it did not.")),
          card(class = "stage-3", card_header("Why overprediction is unavoidable here"),
               DTOutput("min_pred_table"),
               card_footer("When a method's smallest prediction for a group is above ",
                           "that group's threshold, it must overpredict every low day."))
        ),

        nav_panel_hidden(
          "patterns",
          page_head("Patterns", "Weekly, weekday and annual recorded donations, 2006–2025."),
          layout_sidebar(
            class = "stage-2",
            sidebar = sidebar(width = 300,
              checkboxGroupInput("pattern_groups", "Blood groups", ABO_GROUPS,
                                 selected = ABO_GROUPS),
              sliderInput("pattern_years", "Years", min = 2006, max = 2025,
                          value = c(2006, 2025), sep = "", step = 1)
            ),
            card(card_header("Weekly recorded donations"),
                 plotOutput("weekly_plot", height = 360),
                 card_footer("Weekly totals of daily counts; partial first/last ",
                             "weeks of the range contain fewer days.")),
            layout_columns(
              col_widths = c(6, 6),
              card(card_header("Mean daily donations by weekday"),
                   plotOutput("weekday_plot", height = 320)),
              card(card_header("Mean daily donations by year"),
                   plotOutput("annual_plot", height = 320))
            )
          )
        ),

        nav_panel_hidden(
          "explain",
          page_head("Model explanation", "What the 2006–2024 regression learned, and where 2025 errors fell."),
          layout_columns(
            class = "stage-2",
            col_widths = c(6, 6),
            card(card_header("Linear regression coefficients (fitted 2006–2024)"),
                 tableOutput("coef_table"),
                 card_footer("Associations in historical data, not causal effects. ",
                             "The Sunday contrast holds the time index fixed.")),
            card(card_header("Signed error in 2025 (actual − predicted)"),
                 DTOutput("bias_table"),
                 card_footer("Near-zero averages mean positive and negative errors ",
                             "cancel; they do not mean the daily forecasts are accurate."))
          ),
          card(class = "stage-3", card_header("Monthly MAE in 2025"),
               selectInput("month_methods", NULL, METHODS, multiple = TRUE,
                           selected = c("Seven-day baseline", "Linear regression",
                                        "Recent weekday average")),
               plotOutput("monthly_plot", height = 340))
        ),

        nav_panel_hidden(
          "audit",
          page_head("Data audit", "Checks on the frozen public snapshot before any analysis."),
          layout_columns(
            class = "stage-2",
            col_widths = c(6, 6),
            card(card_header("Raw snapshot checks"), tableOutput("audit_table")),
            card(card_header("Analysis panel"), tableOutput("panel_table"),
                 card_footer("The ALL category is excluded from modelling to avoid ",
                             "double counting. 2026 rows are excluded so that only ",
                             "complete calendar years are analysed."))
          ),
          card(
            class = "stage-3",
            card_header("Source and licence"),
            p("Government of Malaysia (2026). ",
              em("Daily Blood Donations by Blood Group & State"), ". data.gov.my. ",
              a("Catalogue page",
                href = "https://data.gov.my/data-catalogue/blood_donations_state",
                target = "_blank"),
              ". Published under the ",
              a("Creative Commons Attribution 4.0 International licence",
                href = "https://creativecommons.org/licenses/by/4.0/", target = "_blank"),
              ". The repository includes the unchanged snapshot (gzip-compressed) ",
              "used in the original analysis; the app checks its MD5 before use."),
            p("Official metadata: records come from the Blood Bank Information System ",
              "of 22 main Ministry of Health collection sites, which cover most but not ",
              "all national donations. The W.P. Kuala Lumpur category includes ",
              "Putrajaya and mobile campaigns around Selangor, which is why the two ",
              "regions are combined.")
          )
        ),

        nav_panel_hidden(
          "methods",
          page_head("Methods & limits"),
          card(
            class = "stage-2",
            card_header("Forecasting methods"),
            tags$dl(class = "methods",
              tags$dt("Seven-day baseline"),
              tags$dd("The count recorded for the same blood group seven calendar days earlier."),
              tags$dt("Weekday average (full history)"),
              tags$dd("Mean for the blood group and weekday over all years before the evaluation year."),
              tags$dt("Linear regression"),
              tags$dd("A separate lm(donations ~ weekday + time_index) for each blood group, fitted on all earlier years."),
              tags$dt("Recent weekday average"),
              tags$dd("Mean for the blood group and weekday over the three complete years before the evaluation year.")
            )
          ),
          card(
            class = "stage-3",
            card_header("Validation limits"),
            tags$ul(
              tags$li("Results are retrospective. The evaluation years were inspected ",
                      "during exploration, so they are not untouched confirmatory holdouts."),
              tags$li("The three evaluation years use overlapping training data and ",
                      "are not independent experiments. No significance testing was done."),
              tags$li("The recent three-year window is a reasoned choice, not a tuned ",
                      "or independently validated setting."),
              tags$li("No forecast intervals, weather, holiday, campaign, inventory or ",
                      "hospital-demand data are used."),
              tags$li("Zero counts are kept; they may be closures or reporting gaps ",
                      "rather than genuine zero collections."),
              tags$li("Nothing here measures operational or clinical benefit. A prospective, ",
                      "governed pilot would be needed before any planning use.")
            )
          ),
          card(
            class = "stage-3",
            card_header("Origin"),
            p("This dashboard is an interactive companion to the BloodReady AI analysis ",
              "submitted for FIT5145 Foundations of Data Science (Monash University, ",
              "2026). All numbers are recomputed in R from the frozen snapshot, and the ",
              "repository's tests check them against the outputs saved by the original ",
              "R Markdown analysis.")
          )
        )
      )
    ),
    quick_actions
  )
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  # Charts adapt to their rendered width: on narrow screens the method legend
  # stacks in one column and crowded value labels are hidden.
  narrow <- function(id) {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    !is.null(w) && w < 640
  }
  method_legend <- function(id) {
    if (narrow(id)) {
      guides(fill = guide_legend(ncol = 1), colour = guide_legend(ncol = 1))
    } else {
      guides(fill = guide_legend(nrow = 1), colour = guide_legend(nrow = 1))
    }
  }
  # base_theme is a complete theme, so narrow-screen tweaks are added after it.
  theme_for <- function(id) {
    if (!narrow(id)) return(base_theme)
    base_theme + theme(legend.text = element_text(size = 8.5),
                       axis.title.y = element_text(size = 8),
                       legend.key.size = unit(0.8, "lines"))
  }


  # --- Navigation -----------------------------------------------------------
  # The rail and quick actions send the section id; the hidden tabset switches.
  observeEvent(input$rail_go, {
    req(input$rail_go %in% vapply(SECTIONS, `[[`, "", "id"))
    nav_select("pages", input$rail_go)
  })

  # --- Game state -----------------------------------------------------------
  st <- reactiveValues(xp = 0L, rounds = 0L, streak = 0L, best_streak = 0L,
                       beaten_total = 0L, badges = character(),
                       visited = "overview", round = NULL, result = NULL)

  push_hud <- function(gained = 0L, new_badges = character(), source = NULL) {
    lv <- level_for(st$xp)
    session$sendCustomMessage("hud", list(
      xp = st$xp, level = lv$level, progress = lv$progress,
      streak = st$streak, badges = length(st$badges),
      held = as.list(st$badges), gained = gained, source = source,
      new_badges = lapply(new_badges, function(id)
        list(id = id, name = BADGES[[id]]$name))
    ))
  }

  award_badges <- function(ids) {
    ids <- setdiff(ids, st$badges)
    if (length(ids)) st$badges <- c(st$badges, ids)
    ids
  }

  # Visiting every section earns XP once per section and the Explorer badge.
  observeEvent(input$pages, {
    if (input$pages %in% st$visited) return()
    st$visited <- c(st$visited, input$pages)
    st$xp <- st$xp + EXPLORER_XP
    new <- if (length(st$visited) == length(SECTIONS)) award_badges("explorer")
           else character()
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
  observeEvent(input$qa_play, {
    if (!is.null(st$result)) start_round()
  })

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
    div(class = "round-head",
        span(class = paste0("group-chip group-", r$blood_type), r$blood_type),
        div(class = "round-meta",
            div(class = "round-date", format(r$date, "%A %d %B %Y")),
            div(class = "round-hint",
                sprintf("Typical for group %s on a %s in 2022–2024: %s–%s donations",
                        r$blood_type, r$weekday,
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
          span(class = "race-val",
               sprintf("guess %s, off by %s", fmt(pred, 0), fmt(errs[[nm]], 0))),
          span(class = "race-mark",
               if (!is_you) (if (beaten) icon("check") else icon("xmark"))))
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
        if (r$low_day) span(class = "low-flag", icon("arrow-trend-down"),
                            "low-collection day")),
      div(class = "race", rows),
      p(class = "race-cap", "Bars show absolute error (shorter is better).")
    )
  })

  output$game_stats <- renderUI({
    div(class = "stats-row",
        div(class = "stat", span(class = "stat-num", st$rounds), span("rounds")),
        div(class = "stat", span(class = "stat-num", st$beaten_total),
            span("methods beaten")),
        div(class = "stat", span(class = "stat-num", st$best_streak),
            span("best streak")))
  })

  headline <- function(m) {
    s <- dash$scores_overall
    fmt(s$MAE[s$evaluation_year == 2025L & s$method == m])
  }
  output$headline_recent <- renderText(headline("Recent weekday average"))
  output$headline_lm <- renderText(headline("Linear regression"))
  output$headline_lag <- renderText(headline("Seven-day baseline"))

  output$overview_plot <- glass_plot({
    ggplot(dash$scores_overall,
           aes(factor(evaluation_year), MAE,
               fill = factor(method, levels = METHODS))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      { if (!narrow("overview_plot"))
          geom_text(aes(label = fmt(MAE)), position = position_dodge(width = 0.8),
                    vjust = -0.4, size = 3.4) } +
      method_legend("overview_plot") +
      scale_fill_manual(values = PASTEL_METHOD, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Evaluation year", y = "MAE (donations per group-day)") +
      theme_for("overview_plot")
  })

  # Data audit
  output$audit_table <- renderTable({
    a <- dash$audit
    data.frame(
      Check = c("MD5 checksum", "Rows × columns", "Date range", "Distinct dates",
                "Regions", "Blood-type values (incl. ALL)", "Missing values",
                "Duplicate rows", "Duplicate date-state-type keys",
                "Negative counts", "Zero counts",
                "ALL vs A+B+AB+O mismatches", "Largest ALL mismatch"),
      Result = c(a$checksum, paste(fmt_int(a$rows), "×", a$columns),
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
                paste(p$first_date, "to", p$last_date), fmt_int(p$rows),
                fmt_int(p$dates),
                sprintf("%d (%.2f%%)", p$zero_values, 100 * p$zero_values / p$rows))
    )
  })

  # Patterns
  pattern_panel <- reactive({
    req(input$pattern_groups)
    yrs <- input$pattern_years
    dash$panel |>
      filter(blood_type %in% input$pattern_groups,
             year(date) >= yrs[1], year(date) <= yrs[2])
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
      mutate(weekday = factor(WEEKDAYS[wday(date, week_start = 1)],
                              levels = WEEKDAYS)) |>
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

  # Forecast comparison
  cmp_scores <- reactive({
    req(input$cmp_methods)
    dash$scores_by_group |>
      filter(evaluation_year == as.integer(input$cmp_year),
             method %in% input$cmp_methods)
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
           subtitle = paste0("Evaluation year ", input$cmp_year,
                             "; y-axes differ by group")) +
      theme_for("cmp_group_plot") + theme(axis.text.x = element_blank())
  })

  output$cmp_table <- renderDT({
    req(input$cmp_methods)
    pooled <- dash$scores_overall |>
      filter(evaluation_year == as.integer(input$cmp_year),
             method %in% input$cmp_methods) |>
      mutate(blood_type = "All (pooled)")
    bind_rows(pooled, cmp_scores()) |>
      arrange(blood_type != "All (pooled)", blood_type,
              factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method,
                Days = observations, MAE, RMSE,
                `Mean signed error` = mean_signed_error) |>
      dt_table(c("MAE", "RMSE", "Mean signed error"), page_length = 20)
  })

  output$cmp_daily_plot <- glass_plot({
    req(input$cmp_methods, input$cmp_dates)
    rng <- input$cmp_dates
    shown <- dash$predictions |>
      filter(blood_type == input$cmp_group, method %in% input$cmp_methods,
             date >= rng[1], date <= rng[2])
    validate(need(nrow(shown) > 0, "No evaluation days in the selected range (2023–2025 only)."))
    actual <- distinct(shown, date, donations)
    ggplot(shown, aes(date, prediction, colour = factor(method, levels = METHODS))) +
      geom_line(linewidth = 0.7) +
      geom_line(data = actual, aes(date, donations), inherit.aes = FALSE,
                colour = INK, linewidth = 0.9) +
      geom_point(data = actual, aes(date, donations), inherit.aes = FALSE,
                 colour = INK, size = 1.2) +
      method_legend("cmp_daily_plot") +
      scale_colour_manual(values = PASTEL_METHOD, name = NULL) +
      labs(x = NULL, y = "Donations per day",
           subtitle = paste("Blood group", input$cmp_group)) + theme_for("cmp_daily_plot")
  })

  # Low-collection days
  output$cutoff_table <- renderTable({
    low_days <- dash$low_errors_2025 |>
      filter(day_category == "Low collection") |>
      distinct(blood_type, days, mean_actual)
    dash$low_cutoffs_2025 |>
      left_join(low_days, by = "blood_type") |>
      transmute(`Blood group` = blood_type,
                `Threshold (≤)` = fmt(low_cutoff),
                `Low days in 2025` = days,
                `Mean actual on low days` = fmt(mean_actual))
  })

  output$low_plot <- glass_plot({
    dash$low_errors_2025 |>
      ggplot(aes(factor(method, levels = METHODS), MAE,
                 fill = factor(method, levels = METHODS),
                 alpha = day_category)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      facet_wrap(~ blood_type, ncol = 4, scales = "free_y") +
      method_legend("low_plot") +
      scale_fill_manual(values = PASTEL_METHOD, name = NULL) +
      scale_alpha_manual(values = c("Low collection" = 1, "Other days" = 0.35),
                         name = NULL) +
      guides(alpha = "none") +
      labs(x = NULL, y = "MAE (donations per day)",
           subtitle = "Solid bars: low-collection days. Faded bars: other days.") +
      theme_for("low_plot") + theme(axis.text.x = element_blank())
  })

  output$low_table <- renderDT({
    dash$low_errors_2025 |>
      filter(day_category == "Low collection") |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method, `Low days` = days,
                MAE, `Overpredicted (%)` = overprediction_percent,
                `Mean excess forecast` = mean_excess) |>
      dt_table(c("MAE", "Overpredicted (%)", "Mean excess forecast"),
               page_length = 16)
  })

  output$min_pred_table <- renderDT({
    dash$min_predictions_2025 |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method,
                `Smallest 2025 prediction` = minimum_prediction,
                `Low-day threshold` = low_cutoff,
                `All predictions above threshold` =
                  ifelse(all_predictions_above_cutoff, "Yes", "No")) |>
      dt_table(c("Smallest 2025 prediction", "Low-day threshold"),
               page_length = 16)
  })

  # Model explanation
  output$coef_table <- renderTable({
    dash$coefficients |>
      transmute(`Blood group` = blood_type,
                `Trend per 365 days` = fmt(change_per_365_days),
                `Sunday − Monday` = fmt(sunday_minus_monday),
                `R²` = fmt(r_squared, 3))
  })

  output$bias_table <- renderDT({
    dash$scores_by_group |>
      filter(evaluation_year == 2025L) |>
      arrange(blood_type, factor(method, levels = METHODS)) |>
      transmute(`Blood group` = blood_type, Method = method,
                `Mean signed error` = mean_signed_error, MAE) |>
      dt_table(c("Mean signed error", "MAE"), page_length = 16)
  })

  output$monthly_plot <- glass_plot({
    req(input$month_methods)
    dash$monthly_2025 |>
      filter(method %in% input$month_methods) |>
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
