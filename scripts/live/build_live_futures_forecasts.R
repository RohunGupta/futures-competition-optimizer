# ==============================================================================
# build_live_futures_forecasts.R
#
# Stage 4 of the live competition pipeline.
#
# Purpose:
#   1. consume the validated Stage 2 live competition clock;
#   2. consume the validated Stage 3 live futures model history;
#   3. refit the already-selected futures mean and variance specifications;
#   4. forecast conditional log-return means and variances from current_t
#      through t = 146;
#   5. combine conditional standard deviations with the frozen Notebook 42
#      futures innovation correlation matrix;
#   6. convert Gaussian log-return moments to arithmetic-return moments;
#   7. construct full-horizon optimizer-compatible mu and Sigma objects;
#   8. leave past rows inert rather than replacing them with realized returns.
#
# Important:
#   - No model family is re-selected here.
#   - SR3 keeps the frozen "arma_aic" specification. Its internal AIC fitting
#     procedure is part of fitting that already-selected specification.
#   - The Notebook 42 futures innovation correlation matrix remains frozen.
#   - Futures models are fitted in log-return space.
#   - Optimizer moments are arithmetic-return moments.
#   - Global optimizer indexing remains t_original = 0, ..., 146.
# ==============================================================================

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(purrr)
  library(stringr)
  library(lubridate)
})

source(
  here(
    "scripts",
    "futures_models.R"
  )
)

source(
  here(
    "scripts",
    "futures_forecasting.R"
  )
)

# ------------------------------------------------------------------------------
# 1. Constants
# ------------------------------------------------------------------------------

LIVE_FUTURES_SYMBOL_ORDER <- c(
  "CL",
  "GC",
  "SR3",
  "ZN",
  "KE",
  "HO",
  "ZW",
  "NQ"
)

LIVE_FUTURES_OPTIMIZER_NAMES <- paste0(
  "Future_",
  seq_along(LIVE_FUTURES_SYMBOL_ORDER)
)

LIVE_EXPECTED_HORIZON <- 147L
LIVE_FINAL_T <- 146L
LIVE_TIME_ZONE <- "America/Chicago"

EXPECTED_LIVE_MODEL_PAIRS <-
  tibble::tribble(
    ~Symbol, ~MeanModel, ~VarianceModel,
    "CL", "hurdle_seasonal_persistence", "harmonic_interval",
    "GC", "linear_trend", "garch11_normal",
    "SR3", "arma_aic", "seasonal_lag_squared",
    "ZN", "ma2", "seasonal_lag_squared",
    "KE", "interval", "garch11_normal",
    "HO", "ar2", "harmonic_interval",
    "ZW", "linear_trend", "clock_factor",
    "NQ", "lag1", "clock_factor"
  )

# ------------------------------------------------------------------------------
# 2. Paths
# ------------------------------------------------------------------------------

live_model_history_path <-
  here(
    "data",
    "processed",
    "live",
    "live_futures_model_history.csv"
  )

final_futures_model_selection_path <-
  here(
    "data",
    "processed",
    "futures",
    "model_selection",
    "final_futures_model_selection.rds"
  )

production_futures_innovation_correlation_path <-
  here(
    "data",
    "processed",
    "futures",
    "diagnostics",
    "production_futures_innovation_correlation.rds"
  )

optimizer_input_dir <-
  here("outputs", "optimizer_inputs")

dir.create(
  optimizer_input_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

live_futures_output_dir <-
  here(
    "data",
    "processed",
    "live"
  )

dir.create(
  live_futures_output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------------------------
# 3. Require validated Stage 2 clock
# ------------------------------------------------------------------------------

if (
  !exists(
    "live_competition_clock",
    envir = .GlobalEnv
  )
) {
  stage2_script <-
    here(
      "scripts",
      "live",
      "build_live_competition_clock.R"
    )
  
  stopifnot(
    file.exists(stage2_script)
  )
  
  source(
    stage2_script,
    local = .GlobalEnv
  )
}

stopifnot(
  exists(
    "live_competition_clock",
    envir = .GlobalEnv
  )
)

stopifnot(
  is.list(
    live_competition_clock
  )
)

stopifnot(
  "current_t" %in%
    names(
      live_competition_clock
    )
)

current_t <-
  as.integer(
    live_competition_clock[["current_t"]]
  )

stopifnot(
  length(current_t) == 1L
)

stopifnot(
  is.finite(current_t)
)

stopifnot(
  current_t >= 0L
)

stopifnot(
  current_t <= LIVE_FINAL_T
)

competition_clock_to_minutes_live <- function(
    clock_value
) {
  clock_value <-
    as.character(
      clock_value
    )
  
  clock_parts <-
    strsplit(
      clock_value,
      ":",
      fixed = TRUE
    )
  
  vapply(
    clock_parts,
    function(
    parts
    ) {
      stopifnot(
        length(parts) >= 2L
      )
      
      hour_value <-
        as.numeric(
          parts[[1]]
        )
      
      minute_value <-
        as.numeric(
          parts[[2]]
        )
      
      stopifnot(
        is.finite(hour_value)
      )
      
      stopifnot(
        is.finite(minute_value)
      )
      
      hour_value *
        60 +
        minute_value
    },
    numeric(1)
  )
}

# ------------------------------------------------------------------------------
# 4. Extract authoritative full 147-row schedule from Stage 2
# ------------------------------------------------------------------------------

stopifnot(
  "competition_clock" %in%
    names(
      live_competition_clock
    )
)

live_full_schedule <-
  live_competition_clock[["competition_clock"]]

stopifnot(
  is.data.frame(
    live_full_schedule
  )
)

stopifnot(
  nrow(
    live_full_schedule
  ) ==
    LIVE_EXPECTED_HORIZON
)

required_stage2_clock_columns <- c(
  "t",
  "start_day",
  "start_time_ct",
  "end_day",
  "end_time_ct",
  "interval_minutes"
)

missing_stage2_clock_columns <-
  setdiff(
    required_stage2_clock_columns,
    names(
      live_full_schedule
    )
  )

if (
  length(missing_stage2_clock_columns) > 0L
) {
  stop(
    paste0(
      "Stage 2 competition_clock is missing required columns: ",
      paste(
        missing_stage2_clock_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

stopifnot(
  identical(
    as.integer(
      live_full_schedule[["t"]]
    ),
    0:LIVE_FINAL_T
  )
)

if (
  !exists(
    "LIVE_COMPETITION_SUNDAY",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  LIVE_COMPETITION_SUNDAY <- as.Date("2026-09-13")
}

live_competition_start_date <-
  as.Date(
    LIVE_COMPETITION_SUNDAY
  )

live_schedule_day_offset <-
  c(
    Sunday = 0L,
    Monday = 1L,
    Tuesday = 2L,
    Wednesday = 3L,
    Thursday = 4L,
    Friday = 5L
  )

live_schedule_model_day <-
  ifelse(
    live_full_schedule[["end_time_ct"]] == "00:00",
    as.character(
      live_full_schedule[["start_day"]]
    ),
    as.character(
      live_full_schedule[["end_day"]]
    )
  )

live_schedule_model_clock <-
  ifelse(
    live_full_schedule[["end_time_ct"]] == "00:00",
    "24:00",
    as.character(
      live_full_schedule[["end_time_ct"]]
    )
  )

live_schedule_date <-
  live_competition_start_date +
  unname(
    live_schedule_day_offset[
      live_schedule_model_day
    ]
  )

live_schedule_timestamp_date <-
  live_schedule_date

live_schedule_timestamp_clock <-
  live_schedule_model_clock

live_schedule_midnight <-
  live_schedule_model_clock == "24:00"

live_schedule_timestamp_date[live_schedule_midnight] <-
  live_schedule_timestamp_date[live_schedule_midnight] +
  1L

live_schedule_timestamp_clock[live_schedule_midnight] <-
  "00:00"

live_schedule_timestamp_text <-
  paste(
    format(
      live_schedule_timestamp_date,
      "%Y-%m-%d"
    ),
    live_schedule_timestamp_clock
  )

live_schedule_timestamp <-
  as.POSIXct(
    live_schedule_timestamp_text,
    format = "%Y-%m-%d %H:%M",
    tz = LIVE_TIME_ZONE
  )

stopifnot(
  all(
    !is.na(
      live_schedule_date
    )
  )
)

stopifnot(
  all(
    !is.na(
      live_schedule_timestamp
    )
  )
)

live_full_schedule <-
  live_full_schedule |>
  transmute(
    t_original =
      as.integer(
        t
      ),
    CompetitionTimestampCT =
      live_schedule_timestamp,
    ScheduleDate =
      live_schedule_date,
    ScheduleDay =
      live_schedule_model_day,
    CompetitionMinuteOffset =
      vapply(
        live_schedule_model_clock,
        competition_clock_to_minutes_live,
        numeric(1)
      ),
    CompetitionClockCT =
      live_schedule_model_clock,
    CompetitionIntervalMinutes =
      as.numeric(
        interval_minutes
      ),
    StartDay =
      start_day,
    StartClockCT =
      start_time_ct
  ) |>
  arrange(
    t_original
  )

stopifnot(
  identical(
    as.integer(
      live_full_schedule[["t_original"]]
    ),
    0:LIVE_FINAL_T
  )
)

stopifnot(
  all(
    live_full_schedule[["CompetitionIntervalMinutes"]] %in%
      c(
        30,
        90,
        480
      )
  )
)

live_remaining_schedule <-
  live_full_schedule |>
  filter(
    t_original >= current_t
  )

remaining_horizon <-
  nrow(
    live_remaining_schedule
  )

stopifnot(
  remaining_horizon ==
    LIVE_EXPECTED_HORIZON -
    current_t
)

stopifnot(
  identical(
    as.integer(
      live_remaining_schedule[["t_original"]]
    ),
    current_t:LIVE_FINAL_T
  )
)

cat(
  "Stage 4 current_t:",
  current_t,
  "\n"
)

cat(
  "Stage 4 remaining forecast intervals:",
  remaining_horizon,
  "\n"
)

# ------------------------------------------------------------------------------
# 5. Load validated Stage 3 live futures model history
# ------------------------------------------------------------------------------

stopifnot(
  file.exists(
    live_model_history_path
  )
)

live_futures_model_history <-
  read.csv(
    live_model_history_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

stopifnot(
  is.data.frame(
    live_futures_model_history
  )
)

required_history_columns <- c(
  "Symbol",
  "CompetitionTimestampCT",
  "CandidateLogReturn"
)

missing_history_columns <-
  setdiff(
    required_history_columns,
    names(
      live_futures_model_history
    )
  )

if (
  length(missing_history_columns) > 0L
) {
  stop(
    paste0(
      "Live futures model history is missing required columns: ",
      paste(
        missing_history_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

live_futures_model_history[["Symbol"]] <-
  as.character(
    live_futures_model_history[["Symbol"]]
  )

history_schedule_date <-
  as.Date(
    live_futures_model_history[["ScheduleDate"]]
  )

history_clock_minutes <-
  vapply(
    live_futures_model_history[["CompetitionClockCT"]],
    competition_clock_to_minutes_live,
    numeric(1)
  )

history_midnight_rows <-
  history_clock_minutes == 1440

history_timestamp_date <-
  history_schedule_date

history_timestamp_date[history_midnight_rows] <-
  history_timestamp_date[history_midnight_rows] +
  1L

history_timestamp_minutes <-
  history_clock_minutes

history_timestamp_minutes[history_midnight_rows] <-
  0

history_timestamp_hour <-
  history_timestamp_minutes %/% 60

history_timestamp_minute <-
  history_timestamp_minutes %% 60

history_timestamp_text <-
  sprintf(
    "%s %02d:%02d:00",
    format(
      history_timestamp_date,
      "%Y-%m-%d"
    ),
    history_timestamp_hour,
    history_timestamp_minute
  )

live_futures_model_history[["CompetitionTimestampCT"]] <-
  as.POSIXct(
    history_timestamp_text,
    format = "%Y-%m-%d %H:%M:%S",
    tz = LIVE_TIME_ZONE
  )

stopifnot(
  all(
    !is.na(
      live_futures_model_history[["CompetitionTimestampCT"]]
    )
  )
)

live_futures_model_history[["CandidateLogReturn"]] <-
  as.numeric(
    live_futures_model_history[["CandidateLogReturn"]]
  )

stopifnot(
  setequal(
    unique(
      live_futures_model_history[["Symbol"]]
    ),
    LIVE_FUTURES_SYMBOL_ORDER
  )
)

stopifnot(
  all(
    is.finite(
      live_futures_model_history[["CandidateLogReturn"]]
    )
  )
)

live_futures_model_history <-
  live_futures_model_history |>
  arrange(
    Symbol,
    CompetitionTimestampCT
  )

duplicate_history_rows <-
  live_futures_model_history |>
  count(
    Symbol,
    CompetitionTimestampCT,
    name = "N"
  ) |>
  filter(
    N > 1L
  )

stopifnot(
  nrow(
    duplicate_history_rows
  ) ==
    0L
)

# ------------------------------------------------------------------------------
# 6. Load and validate frozen model selections
# ------------------------------------------------------------------------------

stopifnot(
  file.exists(
    final_futures_model_selection_path
  )
)

final_futures_model_selection <-
  readRDS(
    final_futures_model_selection_path
  )

stopifnot(
  is.data.frame(
    final_futures_model_selection
  )
)

actual_live_model_pairs <-
  final_futures_model_selection |>
  select(
    Symbol,
    MeanModel,
    VarianceModel
  ) |>
  arrange(
    Symbol
  )

expected_live_model_pairs <-
  EXPECTED_LIVE_MODEL_PAIRS |>
  arrange(
    Symbol
  )

stopifnot(
  identical(
    actual_live_model_pairs,
    expected_live_model_pairs
  )
)

stopifnot(
  setequal(
    actual_live_model_pairs[["Symbol"]],
    LIVE_FUTURES_SYMBOL_ORDER
  )
)

cat(
  "Frozen futures model selections loaded and validated.\n"
)

# ------------------------------------------------------------------------------
# 7. Load and validate frozen Notebook 42 futures correlation matrix
# ------------------------------------------------------------------------------

stopifnot(
  file.exists(
    production_futures_innovation_correlation_path
  )
)

live_futures_innovation_correlation <-
  readRDS(
    production_futures_innovation_correlation_path
  )

stopifnot(
  is.matrix(
    live_futures_innovation_correlation
  )
)

stopifnot(
  all(
    dim(
      live_futures_innovation_correlation
    ) ==
      c(
        8L,
        8L
      )
  )
)

stopifnot(
  identical(
    rownames(
      live_futures_innovation_correlation
    ),
    LIVE_FUTURES_SYMBOL_ORDER
  )
)

stopifnot(
  identical(
    colnames(
      live_futures_innovation_correlation
    ),
    LIVE_FUTURES_SYMBOL_ORDER
  )
)

stopifnot(
  all(
    is.finite(
      live_futures_innovation_correlation
    )
  )
)

stopifnot(
  max(
    abs(
      live_futures_innovation_correlation -
        t(
          live_futures_innovation_correlation
        )
    )
  ) <
    1e-10
)

live_correlation_eigenvalues <-
  eigen(
    live_futures_innovation_correlation,
    symmetric = TRUE,
    only.values = TRUE
  )[["values"]]

stopifnot(
  min(
    live_correlation_eigenvalues
  ) >
    0
)

cat(
  "Frozen Notebook 42 futures correlation loaded and validated.\n"
)

# ------------------------------------------------------------------------------
# 8. Competition-clock helper
# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------
# 9. Build future feature row
# ------------------------------------------------------------------------------

build_live_futures_feature_row <- function(
    symbol,
    schedule_row,
    training_observations,
    horizon_index,
    lag_return_1,
    lag_return_2,
    previous_nonzero_return,
    lag_squared_residual_1 = NA_real_,
    lag_squared_residual_2 = NA_real_,
    lag_absolute_residual_1 = NA_real_
) {
  clock_minutes <-
    competition_clock_to_minutes_live(
      schedule_row[["CompetitionClockCT"]][[1]]
    )
  
  clock_fraction <-
    clock_minutes /
    1440
  
  trend_value <-
    if (
      training_observations >
      1L
    ) {
      (
        training_observations +
          horizon_index -
          1L
      ) /
        (
          training_observations -
            1L
        )
    } else {
      0
    }
  
  tibble(
    Symbol =
      symbol,
    CompetitionTimestampCT =
      schedule_row[["CompetitionTimestampCT"]][[1]],
    ScheduleDate =
      schedule_row[["ScheduleDate"]][[1]],
    ScheduleDay =
      schedule_row[["ScheduleDay"]][[1]],
    CompetitionMinuteOffset =
      if (
        "CompetitionMinuteOffset" %in%
        names(
          schedule_row
        )
      ) {
        schedule_row[["CompetitionMinuteOffset"]][[1]]
      } else {
        NA_real_
      },
    CompetitionClockCT =
      schedule_row[["CompetitionClockCT"]][[1]],
    CompetitionIntervalMinutes =
      schedule_row[["CompetitionIntervalMinutes"]][[1]],
    ObservationWithinSymbol =
      training_observations +
      horizon_index,
    TotalObservationsWithinSymbol =
      training_observations,
    Trend01 =
      trend_value,
    ReturnValue =
      NA_real_,
    LagReturn1 =
      lag_return_1,
    LagReturn2 =
      lag_return_2,
    LagAbsoluteReturn1 =
      abs(
        lag_return_1
      ),
    LagSquaredReturn1 =
      lag_return_1^2,
    IsZeroReturn =
      NA,
    PreviousZeroReturn =
      !as.logical(
        previous_nonzero_return
      ),
    PreviousNonzeroReturn =
      as.logical(
        previous_nonzero_return
      ),
    DayFactor =
      factor(
        schedule_row[["ScheduleDay"]][[1]],
        levels =
          FUTURES_COMPETITION_DAY_LEVELS
      ),
    ClockFactor =
      factor(
        schedule_row[["CompetitionClockCT"]][[1]],
        levels =
          FUTURES_COMPETITION_CLOCK_LEVELS
      ),
    IntervalFactor =
      factor(
        as.character(
          schedule_row[["CompetitionIntervalMinutes"]][[1]]
        ),
        levels =
          FUTURES_COMPETITION_INTERVAL_LEVELS
      ),
    IsThirtyMinute =
      schedule_row[["CompetitionIntervalMinutes"]][[1]] ==
      30,
    IsNinetyMinute =
      schedule_row[["CompetitionIntervalMinutes"]][[1]] ==
      90,
    IsOvernight =
      schedule_row[["CompetitionIntervalMinutes"]][[1]] ==
      480,
    ClockMinutes =
      clock_minutes,
    ClockFraction =
      clock_fraction,
    SinTime =
      sin(
        2 *
          pi *
          clock_fraction
      ),
    CosTime =
      cos(
        2 *
          pi *
          clock_fraction
      ),
    SinTime2 =
      sin(
        4 *
          pi *
          clock_fraction
      ),
    CosTime2 =
      cos(
        4 *
          pi *
          clock_fraction
      ),
    LogIntervalMinutes =
      log(
        schedule_row[["CompetitionIntervalMinutes"]][[1]]
      ),
    SqrtIntervalMinutes =
      sqrt(
        schedule_row[["CompetitionIntervalMinutes"]][[1]]
      ),
    IsMonday =
      schedule_row[["ScheduleDay"]][[1]] ==
      "Monday",
    IsTuesday =
      schedule_row[["ScheduleDay"]][[1]] ==
      "Tuesday",
    IsWednesday =
      schedule_row[["ScheduleDay"]][[1]] ==
      "Wednesday",
    IsThursday =
      schedule_row[["ScheduleDay"]][[1]] ==
      "Thursday",
    IsFriday =
      schedule_row[["ScheduleDay"]][[1]] ==
      "Friday",
    IsSunday =
      schedule_row[["ScheduleDay"]][[1]] ==
      "Sunday",
    LagAbsoluteResidual1 =
      lag_absolute_residual_1,
    LagSquaredResidual1 =
      lag_squared_residual_1,
    LagSquaredResidual2 =
      lag_squared_residual_2
  )
}

# ------------------------------------------------------------------------------
# 10. Refit frozen selected mean specifications on expanded live history
# ------------------------------------------------------------------------------

live_mean_model_fits <-
  list()

for (
  symbol in
  LIVE_FUTURES_SYMBOL_ORDER
) {
  symbol_model_row <-
    final_futures_model_selection |>
    filter(
      Symbol ==
        symbol
    )
  
  stopifnot(
    nrow(
      symbol_model_row
    ) ==
      1L
  )
  
  mean_model_name <-
    symbol_model_row[["MeanModel"]][[1]]
  
  symbol_history <-
    live_futures_model_history |>
    filter(
      Symbol ==
        symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  stopifnot(
    nrow(
      symbol_history
    ) >=
      2L
  )
  
  live_mean_model_fits[[symbol]] <-
    fit_any_futures_mean_model(
      data = symbol_history,
      model_name = mean_model_name
    )
  
  cat(
    "Live mean model fitted:",
    symbol,
    mean_model_name,
    "\n"
  )
}

stopifnot(
  length(
    live_mean_model_fits
  ) ==
    8L
)

# ------------------------------------------------------------------------------
# 11. Build live selected-mean residual histories
# ------------------------------------------------------------------------------

live_mean_residual_history <-
  list()

for (
  symbol in
  LIVE_FUTURES_SYMBOL_ORDER
) {
  symbol_history <-
    live_futures_model_history |>
    filter(
      Symbol ==
        symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  fitted_mean_model <-
    live_mean_model_fits[[symbol]]
  
  residual_history <-
    attach_any_futures_mean_residuals(
      data = symbol_history,
      model = fitted_mean_model
    )
  
  residual_history <-
    add_futures_model_features(
      data = residual_history
    )
  
  required_residual_columns <- c(
    "MeanResidual",
    "DayFactor",
    "IntervalFactor",
    "SinTime",
    "CosTime",
    "ClockFactor"
  )
  
  stopifnot(
    all(
      required_residual_columns %in%
        names(
          residual_history
        )
    )
  )
  
  live_mean_residual_history[[symbol]] <-
    residual_history
}

cat(
  "Live selected-mean residual histories constructed.\n"
)

# ------------------------------------------------------------------------------
# 12. Refit frozen selected variance specifications
# ------------------------------------------------------------------------------

live_variance_model_fits <-
  list()

for (
  symbol in
  LIVE_FUTURES_SYMBOL_ORDER
) {
  symbol_model_row <-
    final_futures_model_selection |>
    filter(
      Symbol ==
        symbol
    )
  
  variance_model_name <-
    symbol_model_row[["VarianceModel"]][[1]]
  
  residual_history <-
    live_mean_residual_history[[symbol]]
  
  if (
    variance_model_name %in%
    get_extended_futures_variance_model_names(
      include_garch = TRUE
    )
  ) {
    fitted_variance_model <-
      fit_extended_futures_variance_model(
        data = residual_history,
        model_name = variance_model_name
      )
  } else {
    fitted_variance_model <-
      fit_futures_variance_model(
        data = residual_history,
        model_name = variance_model_name
      )
  }
  
  live_variance_model_fits[[symbol]] <-
    fitted_variance_model
  
  cat(
    "Live variance model fitted:",
    symbol,
    variance_model_name,
    "\n"
  )
}

stopifnot(
  length(
    live_variance_model_fits
  ) ==
    8L
)

cat(
  "All frozen selected variance specifications refitted successfully.\n"
)

# ------------------------------------------------------------------------------
# 13. Extended ARMA multi-step forecast helper
# ------------------------------------------------------------------------------

forecast_live_extended_arma_mean_path <- function(
    fitted_model,
    horizon
) {
  stopifnot(
    inherits(
      fitted_model,
      "extended_futures_mean_model"
    )
  )
  
  stopifnot(
    fitted_model[["ModelType"]] %in%
      c(
        "arma_aic",
        "fixed_arma"
      )
  )
  
  forecast_object <-
    stats::predict(
      fitted_model[["Fit"]],
      n.ahead = horizon
    )
  
  forecast_values <-
    as.numeric(
      forecast_object[["pred"]]
    )
  
  stopifnot(
    length(
      forecast_values
    ) ==
      horizon
  )
  
  stopifnot(
    all(
      is.finite(
        forecast_values
      )
    )
  )
  
  forecast_values
}

# ------------------------------------------------------------------------------
# 14. Forecast remaining conditional log-return mean paths
# ------------------------------------------------------------------------------

live_log_mean_rows <-
  list()

live_mean_probability_paths <-
  list()

for (
  symbol in
  LIVE_FUTURES_SYMBOL_ORDER
) {
  symbol_model_row <-
    final_futures_model_selection |>
    filter(
      Symbol ==
        symbol
    )
  
  mean_model_name <-
    symbol_model_row[["MeanModel"]][[1]]
  
  fitted_mean_model <-
    live_mean_model_fits[[symbol]]
  
  symbol_history <-
    live_futures_model_history |>
    filter(
      Symbol ==
        symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  training_observations <-
    nrow(
      symbol_history
    )
  
  historical_returns <-
    symbol_history[["CandidateLogReturn"]]
  
  stopifnot(
    length(
      historical_returns
    ) >=
      2L
  )
  
  mean_path <-
    rep(
      NA_real_,
      remaining_horizon
    )
  
  probability_path <-
    rep(
      NA_real_,
      remaining_horizon
    )
  
  if (
    inherits(
      fitted_mean_model,
      "extended_futures_mean_model"
    )
  ) {
    mean_path <-
      forecast_live_extended_arma_mean_path(
        fitted_model = fitted_mean_model,
        horizon = remaining_horizon
      )
  } else {
    lag_return_1 <-
      tail(
        historical_returns,
        1L
      )[[1]]
    
    lag_return_2 <-
      tail(
        historical_returns,
        2L
      )[[1]]
    
    previous_nonzero_probability <-
      as.numeric(
        lag_return_1 !=
          0
      )
    
    for (
      horizon_index in
      seq_len(
        remaining_horizon
      )
    ) {
      schedule_row <-
        live_remaining_schedule[
          horizon_index,
          ,
          drop = FALSE
        ]
      
      feature_row <-
        build_live_futures_feature_row(
          symbol = symbol,
          schedule_row = schedule_row,
          training_observations = training_observations,
          horizon_index = horizon_index,
          lag_return_1 = lag_return_1,
          lag_return_2 = lag_return_2,
          previous_nonzero_return =
            previous_nonzero_probability >=
            0.5
        )
      
      if (
        fitted_mean_model[["ModelType"]] ==
        "hurdle"
      ) {
        feature_false <-
          feature_row
        
        feature_true <-
          feature_row
        
        feature_false[["PreviousNonzeroReturn"]] <-
          FALSE
        
        feature_true[["PreviousNonzeroReturn"]] <-
          TRUE
        
        feature_false[["PreviousZeroReturn"]] <-
          TRUE
        
        feature_true[["PreviousZeroReturn"]] <-
          FALSE
        
        probability_if_previous_zero <-
          as.numeric(
            stats::predict(
              fitted_mean_model[["ProbabilityFit"]],
              newdata = feature_false,
              type = "response"
            )[[1]]
          )
        
        probability_if_previous_nonzero <-
          as.numeric(
            stats::predict(
              fitted_mean_model[["ProbabilityFit"]],
              newdata = feature_true,
              type = "response"
            )[[1]]
          )
        
        current_nonzero_probability <-
          (
            1 -
              previous_nonzero_probability
          ) *
          probability_if_previous_zero +
          previous_nonzero_probability *
          probability_if_previous_nonzero
        
        conditional_nonzero_mean <-
          as.numeric(
            stats::predict(
              fitted_mean_model[["ConditionalMeanFit"]],
              newdata = feature_row
            )[[1]]
          )
        
        current_mean <-
          current_nonzero_probability *
          conditional_nonzero_mean
        
        probability_path[[horizon_index]] <-
          current_nonzero_probability
        
        previous_nonzero_probability <-
          current_nonzero_probability
      } else {
        current_mean <-
          as.numeric(
            predict_any_futures_mean_model(
              model = fitted_mean_model,
              new_data = feature_row
            )[[1]]
          )
      }
      
      stopifnot(
        is.finite(
          current_mean
        )
      )
      
      mean_path[[horizon_index]] <-
        current_mean
      
      lag_return_2 <-
        lag_return_1
      
      lag_return_1 <-
        current_mean
    }
  }
  
  stopifnot(
    all(
      is.finite(
        mean_path
      )
    )
  )
  
  live_mean_probability_paths[[symbol]] <-
    probability_path
  
  live_log_mean_rows[[symbol]] <-
    live_remaining_schedule |>
    select(
      t_original,
      CompetitionTimestampCT,
      ScheduleDate,
      ScheduleDay,
      CompetitionClockCT,
      CompetitionIntervalMinutes
    ) |>
    mutate(
      Symbol = symbol,
      MeanModel = mean_model_name,
      LogMeanForecast = mean_path
    )
}

live_log_mean_forecasts <-
  bind_rows(
    live_log_mean_rows
  ) |>
  arrange(
    t_original,
    match(
      Symbol,
      LIVE_FUTURES_SYMBOL_ORDER
    )
  )

stopifnot(
  nrow(
    live_log_mean_forecasts
  ) ==
    remaining_horizon *
    8L
)

stopifnot(
  all(
    is.finite(
      live_log_mean_forecasts[["LogMeanForecast"]]
    )
  )
)

cat(
  "Remaining conditional log-return mean paths constructed.\n"
)

# ------------------------------------------------------------------------------
# 15. GARCH multi-step variance helper
# ------------------------------------------------------------------------------

forecast_live_garch_variance_path <- function(
    fitted_variance_model,
    horizon
) {
  stopifnot(
    inherits(
      fitted_variance_model,
      "extended_futures_variance_model"
    )
  )
  
  stopifnot(
    fitted_variance_model[["ModelType"]] ==
      "garch11"
  )
  
  stopifnot(
    requireNamespace(
      "rugarch",
      quietly = TRUE
    )
  )
  
  forecast_object <-
    rugarch::ugarchforecast(
      fitted_variance_model[["Fit"]],
      n.ahead = horizon
    )
  
  sigma_path <-
    as.numeric(
      rugarch::sigma(
        forecast_object
      )
    )
  
  variance_path <-
    sigma_path^2
  
  stopifnot(
    length(
      variance_path
    ) ==
      horizon
  )
  
  stopifnot(
    all(
      is.finite(
        variance_path
      )
    )
  )
  
  stopifnot(
    all(
      variance_path >
        0
    )
  )
  
  variance_path
}

# ------------------------------------------------------------------------------
# 16. Map unseen factor levels exactly as production Notebook 43
# ------------------------------------------------------------------------------

map_live_unseen_factor_level <- function(
    requested_value,
    fitted_levels,
    factor_name
) {
  requested_value <-
    as.character(
      requested_value
    )
  
  fitted_levels <-
    as.character(
      fitted_levels
    )
  
  if (
    requested_value %in%
    fitted_levels
  ) {
    return(
      requested_value
    )
  }
  
  if (
    factor_name ==
    "ClockFactor"
  ) {
    requested_minutes <-
      competition_clock_to_minutes_live(
        requested_value
      )
    
    fitted_minutes <-
      competition_clock_to_minutes_live(
        fitted_levels
      )
    
    valid_index <-
      is.finite(
        fitted_minutes
      )
    
    stopifnot(
      any(
        valid_index
      )
    )
    
    fitted_levels_valid <-
      fitted_levels[
        valid_index
      ]
    
    fitted_minutes_valid <-
      fitted_minutes[
        valid_index
      ]
    
    nearest_index <-
      which.min(
        abs(
          fitted_minutes_valid -
            requested_minutes
        )
      )
    
    mapped_value <-
      fitted_levels_valid[[nearest_index]]
    
    cat(
      "Live factor mapping:",
      factor_name,
      requested_value,
      "->",
      mapped_value,
      "\n"
    )
    
    return(
      mapped_value
    )
  }
  
  stop(
    paste0(
      "Unsupported live factor level for ",
      factor_name,
      ": ",
      requested_value
    ),
    call. = FALSE
  )
}

# ------------------------------------------------------------------------------
# 17. Forecast original regression variance path
# ------------------------------------------------------------------------------

forecast_live_original_variance_path <- function(
    symbol,
    fitted_variance_model,
    residual_history,
    forecast_schedule,
    training_observations
) {
  horizon <-
    nrow(
      forecast_schedule
    )
  
  variance_path <-
    rep(
      NA_real_,
      horizon
    )
  
  residual_values <-
    residual_history[["MeanResidual"]]
  
  residual_values <-
    residual_values[
      is.finite(
        residual_values
      )
    ]
  
  stopifnot(
    length(
      residual_values
    ) >=
      2L
  )
  
  lag_squared_residual_1 <-
    tail(
      residual_values,
      1L
    )[[1]]^2
  
  lag_squared_residual_2 <-
    tail(
      residual_values,
      2L
    )[[1]]^2
  
  lag_absolute_residual_1 <-
    abs(
      tail(
        residual_values,
        1L
      )[[1]]
    )
  
  symbol_history <-
    live_futures_model_history |>
    filter(
      Symbol ==
        symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  historical_returns <-
    symbol_history[["CandidateLogReturn"]]
  
  lag_return_1 <-
    tail(
      historical_returns,
      1L
    )[[1]]
  
  lag_return_2 <-
    tail(
      historical_returns,
      2L
    )[[1]]
  
  for (
    horizon_index in
    seq_len(
      horizon
    )
  ) {
    schedule_row <-
      forecast_schedule[
        horizon_index,
        ,
        drop = FALSE
      ]
    
    feature_row <-
      build_live_futures_feature_row(
        symbol = symbol,
        schedule_row = schedule_row,
        training_observations = training_observations,
        horizon_index = horizon_index,
        lag_return_1 = lag_return_1,
        lag_return_2 = lag_return_2,
        previous_nonzero_return = TRUE,
        lag_squared_residual_1 = lag_squared_residual_1,
        lag_squared_residual_2 = lag_squared_residual_2,
        lag_absolute_residual_1 = lag_absolute_residual_1
      )
    
    variance_fit <-
      fitted_variance_model[["Fit"]]
    
    if (
      "ClockFactor" %in%
      names(
        variance_fit[["xlevels"]]
      )
    ) {
      requested_clock <-
        as.character(
          feature_row[["ClockFactor"]][[1]]
        )
      
      fitted_clock_levels <-
        variance_fit[["xlevels"]][["ClockFactor"]]
      
      mapped_clock <-
        map_live_unseen_factor_level(
          requested_value = requested_clock,
          fitted_levels = fitted_clock_levels,
          factor_name = "ClockFactor"
        )
      
      feature_row[["ClockFactor"]] <-
        factor(
          mapped_clock,
          levels = fitted_clock_levels
        )
    }
    
    if (
      "DayFactor" %in%
      names(
        variance_fit[["xlevels"]]
      )
    ) {
      requested_day <-
        as.character(
          feature_row[["DayFactor"]][[1]]
        )
      
      fitted_day_levels <-
        variance_fit[["xlevels"]][["DayFactor"]]
      
      if (
        !requested_day %in%
        fitted_day_levels
      ) {
        stop(
          paste0(
            "Unseen live DayFactor level: ",
            requested_day
          ),
          call. = FALSE
        )
      }
      
      feature_row[["DayFactor"]] <-
        factor(
          requested_day,
          levels = fitted_day_levels
        )
    }
    
    if (
      "IntervalFactor" %in%
      names(
        variance_fit[["xlevels"]]
      )
    ) {
      requested_interval <-
        as.character(
          feature_row[["IntervalFactor"]][[1]]
        )
      
      fitted_interval_levels <-
        variance_fit[["xlevels"]][["IntervalFactor"]]
      
      if (
        !requested_interval %in%
        fitted_interval_levels
      ) {
        stop(
          paste0(
            "Unseen live IntervalFactor level: ",
            requested_interval
          ),
          call. = FALSE
        )
      }
      
      feature_row[["IntervalFactor"]] <-
        factor(
          requested_interval,
          levels = fitted_interval_levels
        )
    }
    
    current_variance <-
      as.numeric(
        stats::predict(
          variance_fit,
          newdata = feature_row,
          type = "response"
        )[[1]]
      )
    
    stopifnot(
      is.finite(
        current_variance
      )
    )
    
    stopifnot(
      current_variance >
        0
    )
    
    variance_path[[horizon_index]] <-
      current_variance
    
    lag_squared_residual_2 <-
      lag_squared_residual_1
    
    lag_squared_residual_1 <-
      current_variance
    
    lag_absolute_residual_1 <-
      sqrt(
        current_variance
      )
  }
  
  variance_path
}

# ------------------------------------------------------------------------------
# 18. Forecast remaining conditional log-return variance paths
# ------------------------------------------------------------------------------

live_log_variance_rows <-
  list()

for (
  symbol in
  LIVE_FUTURES_SYMBOL_ORDER
) {
  symbol_model_row <-
    final_futures_model_selection |>
    filter(
      Symbol ==
        symbol
    )
  
  variance_model_name <-
    symbol_model_row[["VarianceModel"]][[1]]
  
  fitted_variance_model <-
    live_variance_model_fits[[symbol]]
  
  residual_history <-
    live_mean_residual_history[[symbol]]
  
  training_observations <-
    nrow(
      live_futures_model_history |>
        filter(
          Symbol ==
            symbol
        )
    )
  
  if (
    inherits(
      fitted_variance_model,
      "extended_futures_variance_model"
    ) &&
    fitted_variance_model[["ModelType"]] ==
    "garch11"
  ) {
    variance_path <-
      forecast_live_garch_variance_path(
        fitted_variance_model = fitted_variance_model,
        horizon = remaining_horizon
      )
  } else if (
    inherits(
      fitted_variance_model,
      "futures_variance_model"
    )
  ) {
    variance_path <-
      forecast_live_original_variance_path(
        symbol = symbol,
        fitted_variance_model = fitted_variance_model,
        residual_history = residual_history,
        forecast_schedule = live_remaining_schedule,
        training_observations = training_observations
      )
  } else {
    stop(
      paste0(
        "Unsupported selected live variance model for ",
        symbol,
        ": ",
        variance_model_name
      ),
      call. = FALSE
    )
  }
  
  stopifnot(
    length(
      variance_path
    ) ==
      remaining_horizon
  )
  
  stopifnot(
    all(
      is.finite(
        variance_path
      )
    )
  )
  
  stopifnot(
    all(
      variance_path >
        0
    )
  )
  
  live_log_variance_rows[[symbol]] <-
    live_remaining_schedule |>
    select(
      t_original,
      CompetitionTimestampCT,
      ScheduleDate,
      ScheduleDay,
      CompetitionClockCT,
      CompetitionIntervalMinutes
    ) |>
    mutate(
      Symbol = symbol,
      VarianceModel = variance_model_name,
      LogVarianceForecast = variance_path,
      LogSDForecast = sqrt(
        variance_path
      )
    )
  
  cat(
    "Live variance path forecast:",
    symbol,
    variance_model_name,
    "\n"
  )
}

live_log_variance_forecasts <-
  bind_rows(
    live_log_variance_rows
  ) |>
  arrange(
    t_original,
    match(
      Symbol,
      LIVE_FUTURES_SYMBOL_ORDER
    )
  )

stopifnot(
  nrow(
    live_log_variance_forecasts
  ) ==
    remaining_horizon *
    8L
)

stopifnot(
  all(
    is.finite(
      live_log_variance_forecasts[["LogVarianceForecast"]]
    )
  )
)

stopifnot(
  all(
    live_log_variance_forecasts[["LogVarianceForecast"]] >
      0
  )
)

cat(
  "Remaining conditional log-return variance paths constructed.\n"
)

# ------------------------------------------------------------------------------
# 19. Build remaining log-return mean matrix
# ------------------------------------------------------------------------------

live_log_mean_matrix <-
  live_log_mean_forecasts |>
  select(
    t_original,
    Symbol,
    LogMeanForecast
  ) |>
  mutate(
    Symbol =
      factor(
        Symbol,
        levels = LIVE_FUTURES_SYMBOL_ORDER
      )
  ) |>
  arrange(
    t_original,
    Symbol
  ) |>
  pivot_wider(
    names_from = Symbol,
    values_from = LogMeanForecast
  ) |>
  arrange(
    t_original
  )

stopifnot(
  identical(
    names(
      live_log_mean_matrix
    ),
    c(
      "t_original",
      LIVE_FUTURES_SYMBOL_ORDER
    )
  )
)

live_log_mean_array <-
  as.matrix(
    live_log_mean_matrix[
      LIVE_FUTURES_SYMBOL_ORDER
    ]
  )

stopifnot(
  all(
    dim(
      live_log_mean_array
    ) ==
      c(
        remaining_horizon,
        8L
      )
  )
)

# ------------------------------------------------------------------------------
# 20. Build remaining conditional log-return SD matrix
# ------------------------------------------------------------------------------

live_log_sd_matrix <-
  live_log_variance_forecasts |>
  select(
    t_original,
    Symbol,
    LogSDForecast
  ) |>
  mutate(
    Symbol =
      factor(
        Symbol,
        levels = LIVE_FUTURES_SYMBOL_ORDER
      )
  ) |>
  arrange(
    t_original,
    Symbol
  ) |>
  pivot_wider(
    names_from = Symbol,
    values_from = LogSDForecast
  ) |>
  arrange(
    t_original
  )

live_log_sd_array <-
  as.matrix(
    live_log_sd_matrix[
      LIVE_FUTURES_SYMBOL_ORDER
    ]
  )

stopifnot(
  all(
    dim(
      live_log_sd_array
    ) ==
      c(
        remaining_horizon,
        8L
      )
  )
)

stopifnot(
  all(
    live_log_sd_array >
      0
  )
)

# ------------------------------------------------------------------------------
# 21. Construct remaining conditional log-return covariance matrices
# ------------------------------------------------------------------------------

live_log_covariance_array <-
  array(
    NA_real_,
    dim = c(
      remaining_horizon,
      8L,
      8L
    ),
    dimnames = list(
      t_original =
        as.character(
          current_t:LIVE_FINAL_T
        ),
      future_1 =
        LIVE_FUTURES_SYMBOL_ORDER,
      future_2 =
        LIVE_FUTURES_SYMBOL_ORDER
    )
  )

for (
  time_index in
  seq_len(
    remaining_horizon
  )
) {
  current_sd <-
    live_log_sd_array[
      time_index,
      ,
      drop = TRUE
    ]
  
  current_d <-
    diag(
      current_sd,
      nrow = 8L,
      ncol = 8L
    )
  
  live_log_covariance_array[
    time_index,
    ,
  ] <-
    current_d %*%
    live_futures_innovation_correlation %*%
    current_d
}

stopifnot(
  all(
    is.finite(
      live_log_covariance_array
    )
  )
)

# ------------------------------------------------------------------------------
# 22. Validate remaining log-return covariance matrices
# ------------------------------------------------------------------------------

live_log_covariance_validation <-
  map_dfr(
    seq_len(
      remaining_horizon
    ),
    function(
    time_index
    ) {
      current_covariance <-
        live_log_covariance_array[
          time_index,
          ,
        ]
      
      current_eigenvalues <-
        eigen(
          current_covariance,
          symmetric = TRUE,
          only.values = TRUE
        )[["values"]]
      
      tibble(
        t_original =
          current_t +
          time_index -
          1L,
        MaximumAsymmetry =
          max(
            abs(
              current_covariance -
                t(
                  current_covariance
                )
            )
          ),
        MinimumDiagonal =
          min(
            diag(
              current_covariance
            )
          ),
        MinimumEigenvalue =
          min(
            current_eigenvalues
          )
      )
    }
  )

stopifnot(
  all(
    live_log_covariance_validation[["MaximumAsymmetry"]] <
      1e-12
  )
)

stopifnot(
  all(
    live_log_covariance_validation[["MinimumDiagonal"]] >
      0
  )
)

stopifnot(
  all(
    live_log_covariance_validation[["MinimumEigenvalue"]] >
      0
  )
)

cat(
  "All remaining conditional log-return covariance matrices validated.\n"
)

# ------------------------------------------------------------------------------
# 23. Exact Gaussian log-to-arithmetic moment conversion
# ------------------------------------------------------------------------------

convert_live_log_moments_to_arithmetic <- function(
    log_mean,
    log_covariance
) {
  log_mean <-
    as.numeric(
      log_mean
    )
  
  log_covariance <-
    as.matrix(
      log_covariance
    )
  
  k <-
    length(
      log_mean
    )
  
  stopifnot(
    all(
      dim(
        log_covariance
      ) ==
        c(
          k,
          k
        )
    )
  )
  
  log_variances <-
    diag(
      log_covariance
    )
  
  arithmetic_mean <-
    exp(
      log_mean +
        0.5 *
        log_variances
    ) -
    1
  
  arithmetic_covariance <-
    matrix(
      NA_real_,
      nrow = k,
      ncol = k
    )
  
  for (
    i in
    seq_len(
      k
    )
  ) {
    for (
      j in
      seq_len(
        k
      )
    ) {
      arithmetic_covariance[
        i,
        j
      ] <-
        exp(
          log_mean[[i]] +
            log_mean[[j]] +
            0.5 *
            log_variances[[i]] +
            0.5 *
            log_variances[[j]]
        ) *
        (
          exp(
            log_covariance[
              i,
              j
            ]
          ) -
            1
        )
    }
  }
  
  list(
    Mean = arithmetic_mean,
    Covariance = arithmetic_covariance
  )
}

# ------------------------------------------------------------------------------
# 24. Convert remaining forecast moments to arithmetic-return moments
# ------------------------------------------------------------------------------

live_arithmetic_mean_array <-
  matrix(
    NA_real_,
    nrow = remaining_horizon,
    ncol = 8L,
    dimnames = list(
      t_original =
        as.character(
          current_t:LIVE_FINAL_T
        ),
      Symbol =
        LIVE_FUTURES_SYMBOL_ORDER
    )
  )

live_arithmetic_covariance_array <-
  array(
    NA_real_,
    dim = c(
      remaining_horizon,
      8L,
      8L
    ),
    dimnames = list(
      t_original =
        as.character(
          current_t:LIVE_FINAL_T
        ),
      future_1 =
        LIVE_FUTURES_SYMBOL_ORDER,
      future_2 =
        LIVE_FUTURES_SYMBOL_ORDER
    )
  )

for (
  time_index in
  seq_len(
    remaining_horizon
  )
) {
  current_conversion <-
    convert_live_log_moments_to_arithmetic(
      log_mean =
        live_log_mean_array[
          time_index,
          ,
          drop = TRUE
        ],
      log_covariance =
        live_log_covariance_array[
          time_index,
          ,
        ]
    )
  
  live_arithmetic_mean_array[
    time_index,
  ] <-
    current_conversion[["Mean"]]
  
  live_arithmetic_covariance_array[
    time_index,
    ,
  ] <-
    current_conversion[["Covariance"]]
}

stopifnot(
  all(
    is.finite(
      live_arithmetic_mean_array
    )
  )
)

stopifnot(
  all(
    is.finite(
      live_arithmetic_covariance_array
    )
  )
)

# ------------------------------------------------------------------------------
# 25. Validate remaining arithmetic covariance matrices
# ------------------------------------------------------------------------------

live_arithmetic_covariance_validation <-
  map_dfr(
    seq_len(
      remaining_horizon
    ),
    function(
    time_index
    ) {
      current_covariance <-
        live_arithmetic_covariance_array[
          time_index,
          ,
        ]
      
      current_eigenvalues <-
        eigen(
          current_covariance,
          symmetric = TRUE,
          only.values = TRUE
        )[["values"]]
      
      tibble(
        t_original =
          current_t +
          time_index -
          1L,
        MaximumAsymmetry =
          max(
            abs(
              current_covariance -
                t(
                  current_covariance
                )
            )
          ),
        MinimumDiagonal =
          min(
            diag(
              current_covariance
            )
          ),
        MinimumEigenvalue =
          min(
            current_eigenvalues
          )
      )
    }
  )

stopifnot(
  all(
    live_arithmetic_covariance_validation[["MaximumAsymmetry"]] <
      1e-12
  )
)

stopifnot(
  all(
    live_arithmetic_covariance_validation[["MinimumDiagonal"]] >
      0
  )
)

stopifnot(
  all(
    live_arithmetic_covariance_validation[["MinimumEigenvalue"]] >
      0
  )
)

cat(
  "All remaining arithmetic-return covariance matrices validated.\n"
)

# ------------------------------------------------------------------------------
# 26. Build full 147-row mu object
#
# Past rows are deliberately inert NA rows.
# The optimizer must use only rows current_t:146.
# ------------------------------------------------------------------------------

mu_csv <-
  tibble(
    t_original =
      0:LIVE_FINAL_T
  )

for (
  optimizer_name in
  LIVE_FUTURES_OPTIMIZER_NAMES
) {
  mu_csv[[optimizer_name]] <-
    NA_real_
}

for (
  remaining_index in
  seq_len(
    remaining_horizon
  )
) {
  global_t <-
    current_t +
    remaining_index -
    1L
  
  mu_csv[
    global_t +
      1L,
    LIVE_FUTURES_OPTIMIZER_NAMES
  ] <-
    as.list(
      live_arithmetic_mean_array[
        remaining_index,
        ,
        drop = TRUE
      ]
    )
}

stopifnot(
  nrow(
    mu_csv
  ) ==
    LIVE_EXPECTED_HORIZON
)

stopifnot(
  identical(
    as.integer(
      mu_csv[["t_original"]]
    ),
    0:LIVE_FINAL_T
  )
)

if (
  current_t >
  0L
) {
  stopifnot(
    all(
      is.na(
        as.matrix(
          mu_csv[
            seq_len(
              current_t
            ),
            LIVE_FUTURES_OPTIMIZER_NAMES,
            drop = FALSE
          ]
        )
      )
    )
  )
}

stopifnot(
  all(
    is.finite(
      as.matrix(
        mu_csv[
          (current_t + 1L):LIVE_EXPECTED_HORIZON,
          LIVE_FUTURES_OPTIMIZER_NAMES,
          drop = FALSE
        ]
      )
    )
  )
)

# ------------------------------------------------------------------------------
# 27. Build full 147 x 8 x 8 Sigma object in optimizer long form
#
# Past covariance rows are deliberately inert NA rows.
# ------------------------------------------------------------------------------

sigma_csv_rows <-
  vector(
    mode = "list",
    length =
      LIVE_EXPECTED_HORIZON *
      64L
  )

sigma_row_index <-
  1L

for (
  global_t in
  0:LIVE_FINAL_T
) {
  for (
    future_1_index in
    seq_len(
      8L
    )
  ) {
    for (
      future_2_index in
      seq_len(
        8L
      )
    ) {
      if (
        global_t <
        current_t
      ) {
        covariance_value <-
          NA_real_
      } else {
        remaining_index <-
          global_t -
          current_t +
          1L
        
        covariance_value <-
          live_arithmetic_covariance_array[
            remaining_index,
            future_1_index,
            future_2_index
          ]
      }
      
      sigma_csv_rows[[sigma_row_index]] <-
        tibble(
          t_original =
            global_t,
          future_1 =
            LIVE_FUTURES_OPTIMIZER_NAMES[[future_1_index]],
          future_2 =
            LIVE_FUTURES_OPTIMIZER_NAMES[[future_2_index]],
          covariance =
            covariance_value
        )
      
      sigma_row_index <-
        sigma_row_index +
        1L
    }
  }
}

Sigma_csv <-
  bind_rows(
    sigma_csv_rows
  )

stopifnot(
  nrow(
    Sigma_csv
  ) ==
    LIVE_EXPECTED_HORIZON *
    64L
)

sigma_counts <-
  Sigma_csv |>
  count(
    t_original,
    name = "CovarianceEntries"
  )

stopifnot(
  nrow(
    sigma_counts
  ) ==
    LIVE_EXPECTED_HORIZON
)

stopifnot(
  all(
    sigma_counts[["CovarianceEntries"]] ==
      64L
  )
)

if (
  current_t >
  0L
) {
  stopifnot(
    all(
      is.na(
        Sigma_csv[["covariance"]][
          Sigma_csv[["t_original"]] <
            current_t
        ]
      )
    )
  )
}

stopifnot(
  all(
    is.finite(
      Sigma_csv[["covariance"]][
        Sigma_csv[["t_original"]] >=
          current_t
      ]
    )
  )
)

# ------------------------------------------------------------------------------
# 28. Build audit summaries
# ------------------------------------------------------------------------------

live_futures_forecast_summary <-
  tibble(
    Symbol =
      LIVE_FUTURES_SYMBOL_ORDER,
    OptimizerFuture =
      LIVE_FUTURES_OPTIMIZER_NAMES,
    MeanModel =
      vapply(
        LIVE_FUTURES_SYMBOL_ORDER,
        function(
    symbol
        ) {
          final_futures_model_selection |>
            filter(
              Symbol ==
                symbol
            ) |>
            pull(
              MeanModel
            ) |>
            as.character()
        },
    character(1)
      ),
    VarianceModel =
      vapply(
        LIVE_FUTURES_SYMBOL_ORDER,
        function(
    symbol
        ) {
          final_futures_model_selection |>
            filter(
              Symbol ==
                symbol
            ) |>
            pull(
              VarianceModel
            ) |>
            as.character()
        },
    character(1)
      ),
    HistoricalObservations =
      vapply(
        LIVE_FUTURES_SYMBOL_ORDER,
        function(
    symbol
        ) {
          sum(
            live_futures_model_history[["Symbol"]] ==
              symbol
          )
        },
    integer(1)
      ),
    CurrentT =
      current_t,
    RemainingIntervals =
      remaining_horizon,
    MinimumArithmeticMean =
      apply(
        live_arithmetic_mean_array,
        2L,
        min
      ),
    MeanArithmeticMean =
      colMeans(
        live_arithmetic_mean_array
      ),
    MaximumArithmeticMean =
      apply(
        live_arithmetic_mean_array,
        2L,
        max
      )
  )

# ------------------------------------------------------------------------------
# 29. Save Stage 4 audit outputs
#
# These are NOT yet the final transactional optimizer-input writes.
# The final orchestrator will write the complete 11-file package together.
# ------------------------------------------------------------------------------

write.csv(
  live_log_mean_forecasts,
  file =
    file.path(
      live_futures_output_dir,
      "live_futures_log_mean_forecasts.csv"
    ),
  row.names = FALSE,
  na = ""
)

write.csv(
  live_log_variance_forecasts,
  file =
    file.path(
      live_futures_output_dir,
      "live_futures_log_variance_forecasts.csv"
    ),
  row.names = FALSE,
  na = ""
)

write.csv(
  live_log_covariance_validation,
  file =
    file.path(
      live_futures_output_dir,
      "live_futures_log_covariance_validation.csv"
    ),
  row.names = FALSE,
  na = ""
)

write.csv(
  live_arithmetic_covariance_validation,
  file =
    file.path(
      live_futures_output_dir,
      "live_futures_arithmetic_covariance_validation.csv"
    ),
  row.names = FALSE,
  na = ""
)

write.csv(
  live_futures_forecast_summary,
  file =
    file.path(
      live_futures_output_dir,
      "live_futures_forecast_summary.csv"
    ),
  row.names = FALSE,
  na = ""
)

# ------------------------------------------------------------------------------
# 30. Expose canonical Stage 4 object
# ------------------------------------------------------------------------------

live_futures_forecast_system <-
  list(
    CurrentT =
      current_t,
    FinalT =
      LIVE_FINAL_T,
    FullHorizon =
      LIVE_EXPECTED_HORIZON,
    RemainingHorizon =
      remaining_horizon,
    FullSchedule =
      live_full_schedule,
    RemainingSchedule =
      live_remaining_schedule,
    ModelHistory =
      live_futures_model_history,
    MeanModelFits =
      live_mean_model_fits,
    MeanResidualHistory =
      live_mean_residual_history,
    VarianceModelFits =
      live_variance_model_fits,
    FrozenInnovationCorrelation =
      live_futures_innovation_correlation,
    LogMeanForecasts =
      live_log_mean_forecasts,
    LogVarianceForecasts =
      live_log_variance_forecasts,
    LogMeanArray =
      live_log_mean_array,
    LogSDForecastArray =
      live_log_sd_array,
    LogCovarianceArray =
      live_log_covariance_array,
    ArithmeticMeanArray =
      live_arithmetic_mean_array,
    ArithmeticCovarianceArray =
      live_arithmetic_covariance_array,
    MuCSV =
      mu_csv,
    SigmaCSV =
      Sigma_csv,
    ForecastSummary =
      live_futures_forecast_summary
  )

# ------------------------------------------------------------------------------
# 31. Final Stage 4 validation
# ------------------------------------------------------------------------------

stopifnot(
  live_futures_forecast_system[["CurrentT"]] ==
    current_t
)

stopifnot(
  live_futures_forecast_system[["FullHorizon"]] ==
    147L
)

stopifnot(
  live_futures_forecast_system[["RemainingHorizon"]] ==
    147L -
    current_t
)

stopifnot(
  nrow(
    live_futures_forecast_system[["MuCSV"]]
  ) ==
    147L
)

stopifnot(
  nrow(
    live_futures_forecast_system[["SigmaCSV"]]
  ) ==
    147L *
    64L
)

stopifnot(
  identical(
    live_futures_forecast_system[["MuCSV"]][["t_original"]],
    0:146
  )
)

stopifnot(
  setequal(
    unique(
      live_futures_forecast_system[["SigmaCSV"]][["t_original"]]
    ),
    0:146
  )
)

LIVE_FUTURES_FORECASTS_VALIDATED <-
  TRUE

# ------------------------------------------------------------------------------
# 32. Write validated Stage 4 optimizer inputs
# ------------------------------------------------------------------------------

mu_output_path <-
  file.path(
    optimizer_input_dir,
    "mu.csv"
  )

sigma_output_path <-
  file.path(
    optimizer_input_dir,
    "Sigma.csv"
  )

write.csv(
  mu_csv,
  file = mu_output_path,
  row.names = FALSE,
  na = ""
)

write.csv(
  Sigma_csv,
  file = sigma_output_path,
  row.names = FALSE,
  na = ""
)

mu_roundtrip <-
  read.csv(
    mu_output_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

sigma_roundtrip <-
  read.csv(
    sigma_output_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

stopifnot(
  identical(
    names(mu_roundtrip),
    names(mu_csv)
  )
)

stopifnot(
  identical(
    names(sigma_roundtrip),
    names(Sigma_csv)
  )
)

stopifnot(
  nrow(mu_roundtrip) ==
    LIVE_EXPECTED_HORIZON
)

stopifnot(
  nrow(sigma_roundtrip) ==
    LIVE_EXPECTED_HORIZON *
    64L
)

stopifnot(
  identical(
    as.integer(
      mu_roundtrip[["t_original"]]
    ),
    0:LIVE_FINAL_T
  )
)

stopifnot(
  setequal(
    unique(
      as.integer(
        sigma_roundtrip[["t_original"]]
      )
    ),
    0:LIVE_FINAL_T
  )
)

if (
  current_t >
  0L
) {
  stopifnot(
    all(
      is.na(
        as.matrix(
          mu_roundtrip[
            seq_len(current_t),
            LIVE_FUTURES_OPTIMIZER_NAMES,
            drop = FALSE
          ]
        )
      )
    )
  )
  
  stopifnot(
    all(
      is.na(
        sigma_roundtrip[["covariance"]][
          sigma_roundtrip[["t_original"]] <
            current_t
        ]
      )
    )
  )
}

stopifnot(
  all(
    is.finite(
      as.matrix(
        mu_roundtrip[
          (current_t + 1L):LIVE_EXPECTED_HORIZON,
          LIVE_FUTURES_OPTIMIZER_NAMES,
          drop = FALSE
        ]
      )
    )
  )
)

stopifnot(
  all(
    is.finite(
      sigma_roundtrip[["covariance"]][
        sigma_roundtrip[["t_original"]] >=
          current_t
      ]
    )
  )
)

cat(
  "Validated optimizer input written:",
  mu_output_path,
  "\n"
)

cat(
  "Validated optimizer input written:",
  sigma_output_path,
  "\n"
)

cat(
  "\n"
)

cat(
  "============================================================\n"
)

cat(
  "LIVE FUTURES FORECAST STAGE COMPLETE\n"
)

cat(
  "============================================================\n"
)

cat(
  "Current t:",
  current_t,
  "\n"
)

cat(
  "Remaining forecast intervals:",
  remaining_horizon,
  "\n"
)

cat(
  "Full mu rows:",
  nrow(
    mu_csv
  ),
  "\n"
)

cat(
  "Full Sigma rows:",
  nrow(
    Sigma_csv
  ),
  "\n"
)

cat(
  "Frozen futures correlation minimum eigenvalue:",
  min(
    live_correlation_eigenvalues
  ),
  "\n"
)

cat(
  "Minimum live log covariance eigenvalue:",
  min(
    live_log_covariance_validation[["MinimumEigenvalue"]]
  ),
  "\n"
)

cat(
  "Minimum live arithmetic covariance eigenvalue:",
  min(
    live_arithmetic_covariance_validation[["MinimumEigenvalue"]]
  ),
  "\n"
)

cat(
  "LIVE_FUTURES_FORECASTS_VALIDATED = TRUE\n"
)

cat(
  "============================================================\n"
)

print(
  live_futures_forecast_summary
)
