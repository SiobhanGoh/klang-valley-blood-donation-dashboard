# "Beat the forecast" game logic. Kept separate from the Shiny UI so it can be
# tested directly. Every round is a real 2025 group-day from the evaluation:
# the player guesses the recorded count and is compared with the four methods.

GAME_XP_PER_METHOD <- 10L
GAME_XP_CLOSE_BONUS <- 15L     # guess within 10% of the actual count
GAME_XP_PER_LEVEL <- 100L
EXPLORER_XP <- 5L

BADGES <- list(
  first_guess = list(name = "First guess", icon = "seedling",
                     text = "Played your first round."),
  beat_lag = list(name = "Faster than last week", icon = "forward",
                  text = "Beat the seven-day baseline."),
  beat_regression = list(name = "Trend breaker", icon = "chart-line",
                         text = "Beat linear regression."),
  beat_all = list(name = "Clean sweep", icon = "crown",
                  text = "Beat all four methods in one round."),
  streak_3 = list(name = "Hot streak", icon = "fire",
                  text = "Beat regression three rounds in a row."),
  low_day = list(name = "Low-day detective", icon = "magnifying-glass",
                 text = "Beat regression on a low-collection day."),
  explorer = list(name = "Explorer", icon = "compass",
                  text = "Visited every section of the dashboard.")
)

# Typical range (25th–75th percentile) for a blood group and weekday over the
# three years before 2025 — the same window the recent weekday mean uses.
game_hint_table <- function(model_data) {
  model_data |>
    filter(year(date) >= 2022L, year(date) <= 2024L) |>
    mutate(blood_type = as.character(blood_type),
           weekday = as.character(weekday)) |>
    group_by(blood_type, weekday) |>
    summarise(typical_low = as.numeric(quantile(donations, 0.25)),
              typical_high = as.numeric(quantile(donations, 0.75)),
              .groups = "drop")
}

# One row per 2025 group-day with the actual count and all four predictions.
game_pool <- function(predictions, low_cutoffs, hints) {
  predictions |>
    filter(evaluation_year == 2025L) |>
    mutate(weekday = as.character(weekday)) |>
    select(date, blood_type, weekday, donations, method, prediction) |>
    tidyr::pivot_wider(names_from = method, values_from = prediction) |>
    left_join(low_cutoffs, by = "blood_type") |>
    left_join(hints, by = c("blood_type", "weekday")) |>
    mutate(low_day = donations <= low_cutoff) |>
    arrange(date, blood_type)
}

new_round <- function(pool, row = sample.int(nrow(pool), 1L)) {
  r <- pool[row, ]
  list(
    date = r$date,
    blood_type = r$blood_type,
    weekday = r$weekday,
    actual = r$donations,
    low_day = isTRUE(r$low_day),
    typical_low = r$typical_low,
    typical_high = r$typical_high,
    predictions = vapply(METHODS, function(m) r[[m]], numeric(1))
  )
}

# Score a guess against the four methods. A method is "beaten" when the
# player's absolute error is strictly smaller than the method's.
score_guess <- function(round, guess) {
  stopifnot(is.numeric(guess), length(guess) == 1L, is.finite(guess), guess >= 0)
  player_error <- abs(round$actual - guess)
  method_error <- abs(round$actual - round$predictions)
  beaten <- names(method_error)[player_error < method_error]
  close <- round$actual > 0 && player_error <= 0.10 * round$actual
  list(
    guess = guess,
    player_error = player_error,
    method_error = method_error,
    beaten = beaten,
    close = close,
    xp = GAME_XP_PER_METHOD * length(beaten) + if (close) GAME_XP_CLOSE_BONUS else 0L
  )
}

# Badges newly earned by this result, given the badges already held and the
# streak of consecutive rounds that beat regression (after this round).
earned_badges <- function(result, round, held, regression_streak) {
  new <- character()
  add <- function(id, cond) if (cond && !(id %in% held)) new <<- c(new, id)
  add("first_guess", TRUE)
  add("beat_lag", "Seven-day baseline" %in% result$beaten)
  add("beat_regression", "Linear regression" %in% result$beaten)
  add("beat_all", length(result$beaten) == length(METHODS))
  add("streak_3", regression_streak >= 3L)
  add("low_day", round$low_day && "Linear regression" %in% result$beaten)
  new
}

level_for <- function(xp) {
  list(level = xp %/% GAME_XP_PER_LEVEL + 1L,
       progress = (xp %% GAME_XP_PER_LEVEL) / GAME_XP_PER_LEVEL)
}
