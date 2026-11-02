# Klang Valley Blood Donation Forecasts

An R Shiny dashboard of daily blood-donation **collections** in the Klang Valley
(Selangor + W.P. Kuala Lumpur), by ABO blood group. It uses the Government of
Malaysia's open daily data and refreshes every day.

It grew out of *BloodReady AI*, an R Markdown analysis submitted for FIT5145
Foundations of Data Science (Monash University, 2026). That analysis compared four
forecasting methods on a frozen snapshot up to 2025. The dashboard applies the same
methods to the newest data, and its tests confirm the methods still reproduce the
report's results on that snapshot.

> **Scope.** The forecasts estimate *recorded donations*. They do not predict blood
> demand, hospital inventory, shortage risk or clinical need. Evaluation is
> retrospective; nothing here measures operational benefit.

## Live data

A scheduled GitHub Action runs every day at 06:17 Malaysia time, after
data.gov.my updates the CSV. It:
1. runs the tests;
2. downloads the latest CSV;
3. recomputes every result;
4. publishes the dashboard to GitHub Pages.

Evaluation covers the last three complete years plus the current year to date,
once it has four weeks of data. If the download fails, the run stops and the
previous site stays up.

The published site uses [Shinylive](https://posit-dev.github.io/r-shinylive/), which
runs the R app in the visitor's browser. Hosting is therefore free, has no usage-hour
limits and needs no server. The first visit downloads the R runtime, so it takes a
little longer to open.

## What the dashboard shows

A left rail (a bottom tab bar on phones) switches between sections:

| Section | Contents |
|---|---|
| Overview | Latest day, headline errors as flip cards, pooled MAE by period and findings generated from the results |
| Live tracker | Latest day, last 7 days and year to date against a year earlier; the last 60 days and a 7-day forecast |
| Beat the forecast | A game: guess a real recent day's recorded donations and try to beat the four methods |
| Compare methods | MAE/RMSE by period and blood group; actual vs predicted daily counts |
| Low-collection days | Errors on days at or below each group's lower quartile from earlier years |
| Patterns | Weekly, weekday and annual patterns, and the blood-type mix of donations by year |
| Blood 101: Malaysia | Who can donate, the donation steps, aftercare, Klang Valley donation centres and a compatibility explorer, all sourced from Pusat Darah Negara and two national blood services |
| About the data | Methods, what the regression learned, data audit, limits, source and licence |

### Beat the forecast

Each round shows a real date and blood group from the latest evaluation period. It also
gives the typical range for that group and weekday over the previous three years, the
window the recent weekday mean uses. You
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

## Results in the FIT5145 report (frozen snapshot)

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
shiny::runApp()                         # downloads today's data (falls back to the snapshot offline)
Sys.setenv(KVBD_SOURCE = "report"); shiny::runApp()   # the FIT5145 report configuration
```

Or precompute once with `Rscript scripts/build_data.R`; the app then loads
`app_data/dashboard.rds` instantly.

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
- that the live pipeline handles a part-way year;
- that the next-7-day forecasts follow the method definitions;
- that a failed download falls back safely;
- that the claims written on the Overview tab hold;
- the game's scoring, badges and levels;
- a full simulated game round;
- that every app output renders. They run on each push via GitHub Actions.

## Repository layout

```
app.R                    Shiny UI and server
R/analysis.R             Data loading (live or snapshot), forecasting and scoring (ported from the Rmd)
R/game.R                 "Beat the forecast" rounds, scoring, badges and levels
R/content.R              Blood 101 facts (with sources) and the compatibility rule
scripts/build_data.R     Downloads the latest CSV and precomputes results for the site
.github/workflows/       Tests on every push; daily data refresh and Pages deployment
www/                     Styles and interaction scripts
data/                    Frozen public data snapshot (gzip-compressed, MD5-checked)
reference/               Outputs saved by the original R Markdown analysis (test oracle)
tests/testthat/          Reproduction and app tests
```

## Data source and licence

Government of Malaysia (2026). *Daily Blood Donations by Blood Group & State* [Data set].
data.gov.my. <https://data.gov.my/data-catalogue/blood_donations_state>. Licensed under
[CC BY 4.0](https://creativecommons.org/licenses/by/4.0/).

The live site downloads this CSV daily. `data/blood_donations_state_snapshot.csv.gz`
is the unmodified CSV downloaded for the original analysis (coverage 1 January 2006 – 28 August 2026), compressed with gzip. The decompressed
file has MD5 `b5365310d6819b52d39ddeba125093e5`, and the app refuses to run on any other
version in the report configuration.

Blood 101 content is summarised in our own words from public pages. Sources:
- Pusat Darah Negara: the
  [donation information booklet](https://pdn.gov.my/v2/images/dokumen/Buku_Informasi_Pendermaan_Darah.pdf),
  the [eligibility A–Z](https://pdn.gov.my/v2/index.php/informasi/bahan-pendidikan-2) and the
  [static donation centres](https://pdn.gov.my/v2/index.php/hubungi-kami/pusat-pendermaan-darah-statik);
- [NHS Blood and Transplant](https://www.blood.co.uk/why-give-blood/blood-types/whats-your-type/);
- [Héma-Québec](https://www.hemaquebec.ca/en/learn/blood/blood-types).

All were accessed on 2 November 2026. Requirements can change, so check PDN before
donating.

The source code is released under the MIT licence (see `LICENSE`). The data remains under
CC BY 4.0 as above.
