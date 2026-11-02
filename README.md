# Klang Valley Blood Donation Forecast Dashboard

An R Shiny dashboard for exploring a retrospective comparison of daily blood-donation
**collection** forecasts for the Klang Valley (Selangor + W.P. Kuala Lumpur), by ABO
blood group, using public data from the Government of Malaysia.

It is an interactive companion to *BloodReady AI*, an R Markdown analysis submitted
for FIT5145 Foundations of Data Science (Monash University, 2026). The app recomputes
every result from the frozen data snapshot. Automated tests then check those results
against the outputs saved by the original analysis.

> **Scope.** The forecasts estimate *recorded donations*. They do not predict blood
> demand, hospital inventory, shortage risk or clinical need. All evaluation is
> retrospective; nothing here measures operational benefit.

## What the dashboard shows

A left rail (a bottom tab bar on phones) switches between sections:

| Section | Contents |
|---|---|
| Overview | Headline errors as flip cards, and pooled MAE for 2023–2025 across four methods |
| Beat the forecast | A game: guess a real 2025 day's recorded donations and try to beat the four methods |
| Compare methods | MAE/RMSE by year and blood group; actual vs predicted daily counts |
| Low-collection days | 2025 errors on days at or below each group's training-period lower quartile |
| Patterns | Weekly, weekday and annual donation patterns, filterable by blood group and year |
| Model explanation | Regression coefficients, signed error and monthly error in 2025 |
| Data audit | Snapshot checksum, coverage, missing/duplicate/negative/zero checks, ALL-category reconciliation |
| Methods & limits | Method definitions and validation limits |

### Beat the forecast

Each round shows a real 2025 date and blood group. It also gives the typical range for
that group and weekday over 2022–2024, the window the recent weekday mean uses. You
guess the recorded count, and the game compares your error with each method's:
- 10 XP for each method you beat;
- 15 XP more for landing within 10% of the actual count;
- levels every 100 XP;
- badges such as *Clean sweep* (beat all four) and *Low-day detective* (beat regression
  on a low-collection day);
- XP for exploring each section.

Progress lasts for the browser session. Rounds replay past days; the game does not
forecast future collections.

### Interface

The interface uses frosted-glass cards on a pastel mesh gradient. The navigation rail
expands on hover, and each section springs out of its icon when opened. A quick-action
button gives shortcuts. The headline cards tilt and flip to explain each number. Motion is
switched off for anyone who has "reduce motion" set on their device.

## Methods

For each evaluation year (2023, 2024, 2025), every method uses only data from earlier years:

1. **Seven-day baseline** – the count recorded seven calendar days earlier (a rolling benchmark).
2. **Weekday average** – the blood group's weekday mean over all earlier years (from 2006).
3. **Linear regression** – `lm(donations ~ weekday + time_index)` fitted separately per blood group.
4. **Recent weekday average** – the weekday mean over the three complete preceding years.

Errors are the mean absolute error (MAE) and root mean squared error (RMSE) in donations per
blood group per day, pooled over the four groups.

## Results (retrospective)

| Method | 2023 MAE | 2024 MAE | 2025 MAE | 2025 RMSE |
|---|---:|---:|---:|---:|
| Seven-day baseline | 51.28 | 46.95 | 48.74 | 77.55 |
| Weekday average (full history) | 47.92 | 44.39 | 43.60 | 68.44 |
| Linear regression | 44.06 | 39.63 | 38.82 | 59.35 |
| Recent weekday average | 41.27 | 37.10 | 36.28 | 56.47 |

- The recent weekday average has the lowest pooled MAE and RMSE in every year.
- For AB, the full-history weekday average is slightly better than regression in every year.
- On 2025 low-collection days, regression and the recent average make much larger errors than
  on other days. The full-history average has the smallest low-day errors for A, B and O, but
  still overpredicts every one of those days.

**Limitations.**
- The evaluation years were inspected during the original exploration, so they are not
  untouched holdouts.
- The three years share overlapping training data, and no significance testing was done.
- The three-year window was not tuned or independently validated.
- No forecast intervals, weather, holiday, campaign, inventory or demand data are used.
- Low-collection days are identified from outcomes after the fact. This is an error audit,
  not an alert system.

## Run it locally

Requires R (≥ 4.3). Install the packages once:

```r
install.packages(c("shiny", "bslib", "DT", "ggplot2", "dplyr", "tidyr",
                   "readr", "lubridate", "janitor", "testthat"))
```

Then, from the repository folder:

```r
shiny::runApp()
```

Startup takes a few seconds while the app rebuilds the panel and refits the models from
the snapshot.

## Tests

```r
testthat::test_dir("tests/testthat")
```

The tests check that the app's recomputed results match the original analysis's saved
outputs (in `reference/`) to within 1e-8. This covers:
- the data-audit counts
- the 29,220 group-day panel
- pooled and per-group MAE/RMSE for all four methods and three years
- low-collection diagnostics and regression coefficients

The tests also check:
- that the claims written on the Overview tab hold;
- the game's scoring, badges and levels;
- a full simulated game round;
- that every app output renders. They run on each push via GitHub Actions.

## Repository layout

```
app.R                    Shiny UI and server
R/analysis.R             Data preparation, forecasting and scoring (ported from the Rmd)
R/game.R                 "Beat the forecast" rounds, scoring, badges and levels
www/                     Styles and interaction scripts
data/                    Frozen public data snapshot (gzip-compressed, MD5-checked)
reference/               Outputs saved by the original R Markdown analysis (test oracle)
tests/testthat/          Reproduction and app tests
```

## Data source and licence

Government of Malaysia (2026). *Daily Blood Donations by Blood Group & State* [Data set].
data.gov.my. <https://data.gov.my/data-catalogue/blood_donations_state>. Licensed under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

`data/blood_donations_state_snapshot.csv.gz` is the unmodified CSV downloaded for the
analysis (coverage 1 January 2006 – 28 August 2026), compressed with gzip. The decompressed
file has MD5 `b5365310d6819b52d39ddeba125093e5`, and the app refuses to run on any other
version. The analysis uses complete years 2006–2025 only. Later downloads may contain
revised values.

The source code is released under the MIT licence (see `LICENSE`). The data remains under
CC BY 4.0 as above.
