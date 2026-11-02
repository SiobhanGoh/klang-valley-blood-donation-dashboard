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

dash <- build_dashboard_data()

fmt <- function(x, digits = 2) formatC(x, format = "f", digits = digits, big.mark = ",")
fmt_int <- function(x) formatC(x, format = "d", big.mark = ",")

base_theme <- theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")

dt_table <- function(df, digits_cols = NULL, digits = 2, page_length = 10) {
  out <- datatable(df, rownames = FALSE,
                   options = list(pageLength = page_length, dom = "tip",
                                  scrollX = TRUE))
  if (length(digits_cols)) out <- formatRound(out, digits_cols, digits)
  out
}

scope_note <- div(
  class = "alert alert-warning", role = "note",
  strong("Scope: "),
  "these forecasts estimate recorded donation collections. They are not ",
  "predictions of blood demand, hospital inventory, shortage risk or ",
  "clinical need, and the evaluation is retrospective."
)

# --- UI -----------------------------------------------------------------------

ui <- page_navbar(
  title = "Klang Valley Blood Donation Forecasts",
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   primary = "#4D7966") |>
    bs_add_rules(paste(
      ".navbar .nav-link.active { color: #fff !important; font-weight: 600;",
      "  text-decoration: underline; text-underline-offset: 6px; }",
      ".navbar .nav-link { color: rgba(255,255,255,0.85) !important; }")),
  fillable = FALSE,
  header = tags$head(tags$link(rel = "icon", href = "data:,")),

  nav_panel(
    "Overview",
    scope_note,
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("Study panel", fmt_int(dash$panel_summary$rows),
                p("group-days, 2006–2025"), theme = "primary"),
      value_box("Recent weekday mean, 2025 MAE",
                textOutput("headline_recent", inline = TRUE),
                p("donations per group-day"), theme = "secondary"),
      value_box("Linear regression, 2025 MAE",
                textOutput("headline_lm", inline = TRUE),
                p("donations per group-day")),
      value_box("Seven-day lag, 2025 MAE",
                textOutput("headline_lag", inline = TRUE),
                p("donations per group-day"))
    ),
    card(
      card_header("Pooled error by evaluation year (lower is better)"),
      plotOutput("overview_plot", height = 360),
      card_footer(
        "Each year's averages and regression are fitted only on earlier years. ",
        "The recent weekday mean uses the preceding three complete years. ",
        "Training windows overlap, so the three years are not independent tests."
      )
    ),
    card(
      card_header("What the analysis shows"),
      tags$ul(
        tags$li("The recent three-year weekday mean has the lowest pooled MAE ",
                "and RMSE in each of 2023, 2024 and 2025."),
        tags$li("Linear regression (weekday + time trend) beats the seven-day ",
                "lag for every blood group in all three years, and the ",
                "full-history weekday mean for A, B and O."),
        tags$li("AB is the exception: there the full-history weekday mean is ",
                "slightly better than regression in every year, and in 2025 ",
                "marginally better than the recent mean too."),
        tags$li("On 2025 low-collection days, regression and the recent mean ",
                "make much larger errors than on other days. The full-history ",
                "mean has the smallest low-day errors for A, B and O, but it ",
                "still overpredicts every one of those days.")
      )
    )
  ),

  nav_panel(
    "Data audit",
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Raw snapshot checks"), tableOutput("audit_table")),
      card(card_header("Analysis panel"), tableOutput("panel_table"),
           card_footer("The ALL category is excluded from modelling to avoid ",
                       "double counting. 2026 rows are excluded so that only ",
                       "complete calendar years are analysed."))
    ),
    card(
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

  nav_panel(
    "Patterns",
    layout_sidebar(
      sidebar = sidebar(
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

  nav_panel(
    "Forecast comparison",
    layout_sidebar(
      sidebar = sidebar(
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
           card_footer("Black line: recorded donations. The seven-day baseline ",
                       "uses the count observed a week earlier, so it is a ",
                       "rolling benchmark rather than a fixed forecast issued ",
                       "at the start of the year."))
    )
  ),

  nav_panel(
    "Low-collection days",
    scope_note,
    p("A low-collection day is a 2025 day whose recorded count is at or below ",
      "the blood group's lower quartile over 2006–2024. The rule uses outcomes ",
      "known only after collection, so this is an error audit, not an early ",
      "warning. A lower-quartile count is not a clinical shortage threshold."),
    layout_columns(
      col_widths = c(4, 8),
      card(card_header("Training-period thresholds"), tableOutput("cutoff_table")),
      card(card_header("Low-day MAE by method"),
           plotOutput("low_plot", height = 340))
    ),
    card(card_header("Low-day errors"), DTOutput("low_table"),
         card_footer("'Mean excess forecast' averages only the amount by which ",
                     "the forecast exceeded the actual count, counting zero ",
                     "when it did not.")),
    card(card_header("Why overprediction is unavoidable here"),
         DTOutput("min_pred_table"),
         card_footer("When a method's smallest prediction for a group is above ",
                     "that group's threshold, it must overpredict every low day."))
  ),

  nav_panel(
    "Model explanation",
    layout_columns(
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
    card(card_header("Monthly MAE in 2025"),
         selectInput("month_methods", NULL, METHODS, multiple = TRUE,
                     selected = c("Seven-day baseline", "Linear regression",
                                  "Recent weekday average")),
         plotOutput("monthly_plot", height = 340))
  ),

  nav_panel(
    "Methods & limits",
    card(
      card_header("Forecasting methods"),
      tags$dl(
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
      card_header("Origin"),
      p("This dashboard is an interactive companion to the BloodReady AI analysis ",
        "submitted for FIT5145 Foundations of Data Science (Monash University, ",
        "2026). All numbers are recomputed in R from the frozen snapshot, and the ",
        "repository's tests check them against the outputs saved by the original ",
        "R Markdown analysis.")
    )
  ),
  nav_spacer(),
  nav_item(tags$small(style = "color: rgba(255,255,255,0.8);",
                      paste("Data snapshot to", format(dash$audit$last_date, "%d %b %Y"),
                            "· analysis 2006–2025")))
)

# --- Server -------------------------------------------------------------------

server <- function(input, output, session) {

  headline <- function(m) {
    s <- dash$scores_overall
    fmt(s$MAE[s$evaluation_year == 2025L & s$method == m])
  }
  output$headline_recent <- renderText(headline("Recent weekday average"))
  output$headline_lm <- renderText(headline("Linear regression"))
  output$headline_lag <- renderText(headline("Seven-day baseline"))

  output$overview_plot <- renderPlot({
    ggplot(dash$scores_overall,
           aes(factor(evaluation_year), MAE,
               fill = factor(method, levels = METHODS))) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      geom_text(aes(label = fmt(MAE)), position = position_dodge(width = 0.8),
                vjust = -0.4, size = 3.4) +
      scale_fill_manual(values = METHOD_COLOURS, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(x = "Evaluation year", y = "MAE (donations per group-day)") +
      base_theme
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

  output$weekly_plot <- renderPlot({
    weekly_totals(pattern_panel()) |>
      ggplot(aes(week_start, donations, colour = blood_type)) +
      geom_line(linewidth = 0.4) +
      scale_colour_manual(values = GROUP_COLOURS, name = "Blood group") +
      labs(x = NULL, y = "Donations per week") + base_theme
  })

  output$weekday_plot <- renderPlot({
    pattern_panel() |>
      mutate(weekday = factor(WEEKDAYS[wday(date, week_start = 1)],
                              levels = WEEKDAYS)) |>
      group_by(blood_type, weekday) |>
      summarise(mean_donations = mean(donations), .groups = "drop") |>
      ggplot(aes(weekday, mean_donations, fill = blood_type)) +
      geom_col(position = "dodge") +
      scale_fill_manual(values = GROUP_COLOURS, name = "Blood group") +
      scale_x_discrete(labels = substr(WEEKDAYS, 1, 3)) +
      labs(x = NULL, y = "Mean daily donations") + base_theme
  })

  output$annual_plot <- renderPlot({
    annual_summary(pattern_panel()) |>
      ggplot(aes(year, mean_daily, colour = blood_type)) +
      geom_line() + geom_point(size = 1.5) +
      scale_colour_manual(values = GROUP_COLOURS, name = "Blood group") +
      labs(x = NULL, y = "Mean daily donations") + base_theme
  })

  # Forecast comparison
  cmp_scores <- reactive({
    req(input$cmp_methods)
    dash$scores_by_group |>
      filter(evaluation_year == as.integer(input$cmp_year),
             method %in% input$cmp_methods)
  })

  output$cmp_group_plot <- renderPlot({
    ggplot(cmp_scores(), aes(factor(method, levels = METHODS), MAE,
                             fill = factor(method, levels = METHODS))) +
      geom_col(width = 0.7) +
      geom_text(aes(label = fmt(MAE, 1)), vjust = -0.4, size = 3.4) +
      facet_wrap(~ blood_type, ncol = 4, scales = "free_y") +
      scale_fill_manual(values = METHOD_COLOURS, name = NULL) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(x = NULL, y = "MAE (donations per day)",
           subtitle = paste("Evaluation year", input$cmp_year,
                            "· y-axes differ by group")) +
      base_theme + theme(axis.text.x = element_blank())
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

  output$cmp_daily_plot <- renderPlot({
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
                colour = "black", linewidth = 0.9) +
      geom_point(data = actual, aes(date, donations), inherit.aes = FALSE,
                 colour = "black", size = 1.2) +
      scale_colour_manual(values = METHOD_COLOURS, name = NULL) +
      labs(x = NULL, y = "Donations per day",
           subtitle = paste("Blood group", input$cmp_group)) + base_theme
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

  output$low_plot <- renderPlot({
    dash$low_errors_2025 |>
      ggplot(aes(factor(method, levels = METHODS), MAE,
                 fill = factor(method, levels = METHODS),
                 alpha = day_category)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.75) +
      facet_wrap(~ blood_type, ncol = 4, scales = "free_y") +
      scale_fill_manual(values = METHOD_COLOURS, name = NULL) +
      scale_alpha_manual(values = c("Low collection" = 1, "Other days" = 0.35),
                         name = NULL) +
      labs(x = NULL, y = "MAE (donations per day)",
           subtitle = "Solid: low-collection days · Faded: other days") +
      base_theme + theme(axis.text.x = element_blank())
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

  output$monthly_plot <- renderPlot({
    req(input$month_methods)
    dash$monthly_2025 |>
      filter(method %in% input$month_methods) |>
      mutate(month = as.Date(paste0(month, "-01"))) |>
      ggplot(aes(month, MAE, colour = factor(method, levels = METHODS))) +
      geom_line() + geom_point() +
      scale_colour_manual(values = METHOD_COLOURS, name = NULL) +
      scale_x_date(date_labels = "%b", date_breaks = "1 month") +
      labs(x = NULL, y = "Pooled MAE (donations per group-day)",
           subtitle = "Describes when errors occurred; it cannot explain why") +
      base_theme
  })
}

shinyApp(ui, server)
