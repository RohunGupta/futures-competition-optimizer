# ============================================================
# build_live_competitor_forecasts.R
# ============================================================
#
# Purpose:
#
# Build the live competitor forecasting system for the
# 147-interval CME competition optimizer.
#
# Inputs:
#
#   data/live_inputs/current_leaderboard.csv
#   data/live_inputs/previous_leaderboard.csv
#
#   scripts/live/build_live_competition_clock.R
#
# Historical frozen-model inputs:
#
#   data/processed/competitors/aug_final_place_rts.csv
#
#   Historical daily competitor data are loaded through the
#   frozen daily competitor forecasting infrastructure.
#
# Outputs:
#
#   state_mean_competition.csv
#   state_cov_competition.csv
#
# Live objects exposed for later stages:
#
#   live_current_final_place_balances
#   live_previous_final_place_balances
#   live_current_daily_place_pnl
#   live_previous_daily_place_pnl
#   live_latest_final_rank_returns
#   live_latest_daily_rank_increments
#   live_current_competitor_state
#   live_state_mean_competition_csv
#   live_state_cov_competition_csv
#
# Important production rules:
#
#   1. Final-rank models remain in log-return units.
#   2. Daily models remain in dollar PnL units.
#   3. Rank positions are modeled, not persistent people.
#   4. DisplayName is NOT used as a unique identifier.
#   5. Frozen model families are NOT reselected.
#   6. Historical selected models are refitted on their
#      authoritative historical modeling datasets.
#   7. The current/previous live snapshot pair initializes
#      current state and latest observed transition/persistence.
#   8. Past t rows are inert.
#   9. Forecast-relevant rows are current_t:146.
#  10. Sunday and Friday daily moments are exactly zero.
#
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

required_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "readr",
  "stringr",
  "lubridate",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    logical(1),
    quietly = TRUE
  )
]

if (
  length(
    missing_packages
  ) > 0L
) {
  stop(
    paste0(
      "Missing required R packages: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
})


# ============================================================
# 2. Project Root
# ============================================================

PROJECT_ROOT <- here::here()

cat(
  "Project root: ",
  PROJECT_ROOT,
  "\n",
  sep = ""
)


# ============================================================
# 3. Paths
# ============================================================

CURRENT_LEADERBOARD_PATH <-
  file.path(
    PROJECT_ROOT,
    "data",
    "live_inputs",
    "current_leaderboard.csv"
  )

PREVIOUS_LEADERBOARD_PATH <-
  file.path(
    PROJECT_ROOT,
    "data",
    "live_inputs",
    "previous_leaderboard.csv"
  )

DAILY_PRIZE_WINNERS_PATH <-
  file.path(
    PROJECT_ROOT,
    "data",
    "live_inputs",
    "daily_prize_winners.csv"
  )

FINAL_RANK_DATA_PATH <-
  file.path(
    PROJECT_ROOT,
    "data",
    "processed",
    "competitors",
    "aug_final_place_rts.csv"
  )

LIVE_CLOCK_SCRIPT_PATH <-
  file.path(
    PROJECT_ROOT,
    "scripts",
    "live",
    "build_live_competition_clock.R"
  )

COMPETITOR_DATA_SCRIPT_PATH <-
  file.path(
    PROJECT_ROOT,
    "scripts",
    "competitor_data.R"
  )

COMPETITOR_MODELS_SCRIPT_PATH <-
  file.path(
    PROJECT_ROOT,
    "scripts",
    "competitor_models.R"
  )

COMPETITOR_FORECASTING_SCRIPT_PATH <-
  file.path(
    PROJECT_ROOT,
    "scripts",
    "competitor_forecasting.R"
  )

DAILY_COMPETITOR_DATA_SCRIPT_PATH <-
  file.path(
    PROJECT_ROOT,
    "scripts",
    "daily_competitor_data.R"
  )

DAILY_COMPETITOR_MODELS_SCRIPT_PATH <-
  file.path(
    PROJECT_ROOT,
    "scripts",
    "daily_competitor_models.R"
  )

DAILY_COMPETITOR_FORECASTING_SCRIPT_PATH <-
  file.path(
    PROJECT_ROOT,
    "scripts",
    "daily_competitor_forecasting.R"
  )

NOTEBOOK_32_CANDIDATES <-
  c(
    file.path(
      PROJECT_ROOT,
      "notebooks",
      "32_JointCompetition_Integration.Rmd"
    ),
    file.path(
      PROJECT_ROOT,
      "32_JointCompetition_Integration.Rmd"
    )
  )

OPTIMIZER_INPUT_DIRECTORY <-
  file.path(PROJECT_ROOT, "outputs", "optimizer_inputs")

LIVE_AUDIT_DIRECTORY <-
  file.path(
    PROJECT_ROOT,
    "data",
    "processed",
    "live"
  )

STATE_MEAN_OUTPUT_PATH <-
  file.path(
    OPTIMIZER_INPUT_DIRECTORY,
    "state_mean_competition.csv"
  )

STATE_COV_OUTPUT_PATH <-
  file.path(
    OPTIMIZER_INPUT_DIRECTORY,
    "state_cov_competition.csv"
  )

CURRENT_STATE_AUDIT_PATH <-
  file.path(
    LIVE_AUDIT_DIRECTORY,
    "live_competitor_current_state.csv"
  )

TRANSITION_AUDIT_PATH <-
  file.path(
    LIVE_AUDIT_DIRECTORY,
    "live_competitor_latest_transition.csv"
  )

required_paths <-
  c(
    DAILY_PRIZE_WINNERS_PATH,
    FINAL_RANK_DATA_PATH,
    LIVE_CLOCK_SCRIPT_PATH,
    COMPETITOR_DATA_SCRIPT_PATH,
    COMPETITOR_MODELS_SCRIPT_PATH,
    COMPETITOR_FORECASTING_SCRIPT_PATH,
    DAILY_COMPETITOR_DATA_SCRIPT_PATH,
    DAILY_COMPETITOR_MODELS_SCRIPT_PATH,
    DAILY_COMPETITOR_FORECASTING_SCRIPT_PATH
  )

missing_paths <-
  required_paths[
    !file.exists(
      required_paths
    )
  ]

if (
  length(
    missing_paths
  ) > 0L
) {
  stop(
    paste0(
      "Missing required Stage 5 input files:\n",
      paste(
        missing_paths,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

existing_notebook_32_candidates <-
  NOTEBOOK_32_CANDIDATES[
    file.exists(
      NOTEBOOK_32_CANDIDATES
    )
  ]

if (
  length(
    existing_notebook_32_candidates
  ) == 0L
) {
  stop(
    paste0(
      "Could not locate 32_JointCompetition_Integration.Rmd.\n",
      "Expected one of:\n",
      paste(
        NOTEBOOK_32_CANDIDATES,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

NOTEBOOK_32_PATH <-
  existing_notebook_32_candidates[[1]]

dir.create(
  OPTIMIZER_INPUT_DIRECTORY,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  LIVE_AUDIT_DIRECTORY,
  recursive = TRUE,
  showWarnings = FALSE
)

cat(
  "Notebook 32 path: ",
  NOTEBOOK_32_PATH,
  "\n",
  sep = ""
)


# ============================================================
# 4. Source Frozen Competitor Infrastructure
# ============================================================

source(
  COMPETITOR_DATA_SCRIPT_PATH
)

source(
  COMPETITOR_MODELS_SCRIPT_PATH
)

source(
  COMPETITOR_FORECASTING_SCRIPT_PATH
)

source(
  DAILY_COMPETITOR_DATA_SCRIPT_PATH
)

source(
  DAILY_COMPETITOR_MODELS_SCRIPT_PATH
)

source(
  DAILY_COMPETITOR_FORECASTING_SCRIPT_PATH
)

cat(
  "Frozen competitor forecasting infrastructure loaded.\n"
)


# ============================================================
# 5. Load Validated Stage 2 Clock
# ============================================================

source(
  LIVE_CLOCK_SCRIPT_PATH
)

if (
  !exists(
    "LIVE_COMPETITION_CLOCK_VALIDATED",
    inherits = TRUE
  ) ||
  !isTRUE(
    LIVE_COMPETITION_CLOCK_VALIDATED
  )
) {
  stop(
    "Stage 2 live competition clock is not validated.",
    call. = FALSE
  )
}

if (
  !exists(
    "live_competition_clock",
    inherits = TRUE
  )
) {
  stop(
    "Stage 2 did not expose live_competition_clock.",
    call. = FALSE
  )
}

LIVE_EXPECTED_HORIZON <-
  as.integer(
    live_competition_clock[["T"]]
  )

live_current_t <-
  as.integer(
    live_competition_clock[["current_t"]]
  )

live_current_day <-
  as.character(
    live_competition_clock[["current_day"]]
  )

live_current_time_ct <-
  as.character(
    live_competition_clock[["current_time_ct"]]
  )

if (
  LIVE_EXPECTED_HORIZON != 147L
) {
  stop(
    paste0(
      "Stage 5 expected T = 147 but Stage 2 supplied T = ",
      LIVE_EXPECTED_HORIZON,
      "."
    ),
    call. = FALSE
  )
}

if (
  live_current_t < 0L ||
  live_current_t >= LIVE_EXPECTED_HORIZON
) {
  stop(
    "Stage 2 current_t is outside the valid 0:146 range.",
    call. = FALSE
  )
}

cat(
  "Stage 2 clock validated for Stage 5.\n"
)

cat(
  "current_t = ",
  live_current_t,
  "\n",
  sep = ""
)

cat(
  "Current decision clock = ",
  live_current_day,
  " ",
  live_current_time_ct,
  " CT\n",
  sep = ""
)


# ============================================================
# Time-dependent live leaderboard requirements
# ============================================================
#
# t = 0:
#   No leaderboard snapshot is required. The competition starts
#   from the known deterministic state of $25,000 for everyone.
#
# t = 1:
#   Only the current leaderboard is required. The previous
#   competitor state is the known $25,000 starting state.
#
# t >= 2:
#   Both current and previous leaderboard snapshots are required.
#
# ============================================================

required_live_leaderboard_paths <- character(0)

if (
  live_current_t >= 1L
) {
  required_live_leaderboard_paths <- c(
    required_live_leaderboard_paths,
    CURRENT_LEADERBOARD_PATH
  )
}

if (
  live_current_t >= 2L
) {
  required_live_leaderboard_paths <- c(
    required_live_leaderboard_paths,
    PREVIOUS_LEADERBOARD_PATH
  )
}

missing_live_leaderboard_paths <-
  required_live_leaderboard_paths[
    !file.exists(
      required_live_leaderboard_paths
    )
  ]

if (
  length(
    missing_live_leaderboard_paths
  ) > 0L
) {
  stop(
    paste0(
      "Missing leaderboard file(s) required at current_t = ",
      live_current_t,
      ":\n",
      paste(
        missing_live_leaderboard_paths,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

cat(
  "Leaderboard requirement for current_t = ",
  live_current_t,
  ": ",
  if (
    live_current_t == 0L
  ) {
    "none"
  } else if (
    live_current_t == 1L
  ) {
    "current only"
  } else {
    "current + previous"
  },
  "\n",
  sep = ""
)


# ============================================================
# 6. Build Live 147-Interval Competitor Schedule
# ============================================================

live_stage2_schedule <-
  live_competition_clock[["competition_clock"]]

required_stage2_schedule_columns <-
  c(
    "t",
    "start_day",
    "start_time_ct",
    "end_day",
    "end_time_ct",
    "interval_minutes"
  )

missing_stage2_schedule_columns <-
  setdiff(
    required_stage2_schedule_columns,
    names(
      live_stage2_schedule
    )
  )

if (
  length(
    missing_stage2_schedule_columns
  ) > 0L
) {
  stop(
    paste0(
      "Stage 2 competition schedule is missing columns: ",
      paste(
        missing_stage2_schedule_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (
  nrow(
    live_stage2_schedule
  ) != LIVE_EXPECTED_HORIZON
) {
  stop(
    "Stage 2 competition schedule does not contain 147 rows.",
    call. = FALSE
  )
}

if (
  !identical(
    as.integer(
      live_stage2_schedule[["t"]]
    ),
    0:146
  )
) {
  stop(
    "Stage 2 competition schedule does not use t = 0:146.",
    call. = FALSE
  )
}

if (
  !exists(
    "LIVE_COMPETITION_SUNDAY",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  LIVE_COMPETITION_SUNDAY <- as.Date("2026-09-13")
}

LIVE_COMPETITION_START_DATE <-
  as.Date(
    LIVE_COMPETITION_SUNDAY
  )

live_day_offset <-
  c(
    Sunday = 0L,
    Monday = 1L,
    Tuesday = 2L,
    Wednesday = 3L,
    Thursday = 4L,
    Friday = 5L
  )

live_model_schedule_day <-
  ifelse(
    live_stage2_schedule[["end_time_ct"]] == "00:00",
    as.character(
      live_stage2_schedule[["start_day"]]
    ),
    as.character(
      live_stage2_schedule[["end_day"]]
    )
  )

live_model_schedule_clock <-
  ifelse(
    live_stage2_schedule[["end_time_ct"]] == "00:00",
    "24:00",
    as.character(
      live_stage2_schedule[["end_time_ct"]]
    )
  )

if (
  any(
    !live_model_schedule_day %in%
    names(
      live_day_offset
    )
  )
) {
  stop(
    "Live competitor schedule contains an unsupported day.",
    call. = FALSE
  )
}

live_schedule_date <-
  LIVE_COMPETITION_START_DATE +
  unname(
    live_day_offset[
      live_model_schedule_day
    ]
  )

live_timestamp_date <-
  live_schedule_date

live_timestamp_clock <-
  live_model_schedule_clock

live_midnight_rows <-
  live_model_schedule_clock == "24:00"

live_timestamp_date[live_midnight_rows] <-
  live_timestamp_date[live_midnight_rows] +
  1L

live_timestamp_clock[live_midnight_rows] <-
  "00:00"

live_timestamp_text <-
  paste(
    format(
      live_timestamp_date,
      "%Y-%m-%d"
    ),
    live_timestamp_clock
  )

live_competition_timestamp <-
  as.POSIXct(
    live_timestamp_text,
    format = "%Y-%m-%d %H:%M",
    tz = "America/Chicago"
  )

if (
  any(
    is.na(
      live_competition_timestamp
    )
  )
) {
  stop(
    "Live competitor schedule timestamp construction failed.",
    call. = FALSE
  )
}

live_competitor_schedule <-
  tibble(
    t_original =
      as.integer(
        live_stage2_schedule[["t"]]
      ),
    ScheduleDate =
      live_schedule_date,
    ScheduleDay =
      live_model_schedule_day,
    CompetitionTimestampCT =
      live_competition_timestamp,
    CompetitionClockCT =
      live_model_schedule_clock,
    CompetitionIntervalMinutes =
      as.numeric(
        live_stage2_schedule[["interval_minutes"]]
      ),
    StartDay =
      as.character(
        live_stage2_schedule[["start_day"]]
      ),
    StartClockCT =
      as.character(
        live_stage2_schedule[["start_time_ct"]]
      )
  )

if (
  anyDuplicated(
    live_competitor_schedule[["t_original"]]
  ) > 0L
) {
  stop(
    "Live competitor schedule contains duplicate t_original values.",
    call. = FALSE
  )
}

if (
  anyDuplicated(
    live_competitor_schedule[["CompetitionTimestampCT"]]
  ) > 0L
) {
  stop(
    "Live competitor schedule contains duplicate timestamps.",
    call. = FALSE
  )
}

cat(
  "Live 147-interval competitor schedule constructed.\n"
)


# ============================================================
# 7. Frozen Production Definitions
# ============================================================

FINAL_TOTAL_COMPETITION_OBSERVATIONS <-
  147

FINAL_RANK_NAMES <-
  c(
    "Rank1",
    "Rank2",
    "Rank3",
    "Rank4",
    "Rank15",
    "Rank16"
  )

FINAL_RANK_NUMBERS <-
  c(
    1L,
    2L,
    3L,
    4L,
    15L,
    16L
  )

FINAL_RAW_NAMES <-
  c(
    "r1",
    "r2",
    "r3",
    "r4",
    "r15",
    "r16"
  )

DAILY_RAW_NAMES <-
  c(
    "h1",
    "h2"
  )

JOINT_RAW_NAMES_PRODUCTION <-
  c(
    FINAL_RAW_NAMES,
    DAILY_RAW_NAMES
  )

STATE_NAMES <-
  c(
    "Delta_Final_X1",
    "Delta_Final_D12",
    "Delta_Final_D23",
    "Delta_Final_D34",
    "Delta_Final_D4_15",
    "Delta_Final_D15_16",
    "Delta_Daily_H1",
    "Delta_Daily_D12"
  )

FINAL_SELECTED_MEAN_MODELS <-
  c(
    Rank1 = "hurdle_time_duration",
    Rank2 = "linear_time",
    Rank3 = "hurdle_time_duration",
    Rank4 = "linear_time",
    Rank15 = "quadratic_time",
    Rank16 = "linear_time"
  )

FINAL_SELECTED_VARIANCE_MODELS <-
  c(
    Rank1 = "long_interval",
    Rank2 = "elapsed_linear",
    Rank3 = "elapsed_log",
    Rank4 = "elapsed_linear",
    Rank15 = "elapsed_linear",
    Rank16 = "constant"
  )

ACTIVE_DAILY_PRIZE_DAYS <-
  c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday"
  )

COVARIANCE_SYMMETRY_TOLERANCE <-
  1e-8

PSD_EIGENVALUE_TOLERANCE <-
  1e-8

cat(
  "Frozen Stage 5 model specification loaded.\n"
)


# ============================================================
# 7A. Load and Validate Prior Daily Prize Winners
# ============================================================

daily_prize_winners <-
  readr::read_csv(
    DAILY_PRIZE_WINNERS_PATH,
    show_col_types = FALSE
  )

required_daily_prize_winner_columns <-
  c(
    "Day",
    "DisplayName"
  )

missing_daily_prize_winner_columns <-
  setdiff(
    required_daily_prize_winner_columns,
    names(
      daily_prize_winners
    )
  )

if (
  length(
    missing_daily_prize_winner_columns
  ) > 0L
) {
  stop(
    paste0(
      "daily_prize_winners.csv is missing columns: ",
      paste(
        missing_daily_prize_winner_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

daily_prize_winners <-
  daily_prize_winners |>
  transmute(
    Day = trimws(
      as.character(
        Day
      )
    ),
    DisplayName = trimws(
      as.character(
        DisplayName
      )
    )
  )

if (
  any(
    is.na(
      daily_prize_winners[["Day"]]
    ) |
    daily_prize_winners[["Day"]] == ""
  )
) {
  stop(
    "daily_prize_winners.csv contains missing or blank Day values.",
    call. = FALSE
  )
}

if (
  any(
    is.na(
      daily_prize_winners[["DisplayName"]]
    ) |
    daily_prize_winners[["DisplayName"]] == ""
  )
) {
  stop(
    "daily_prize_winners.csv contains missing or blank DisplayName values.",
    call. = FALSE
  )
}

valid_daily_prize_days <-
  c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday"
  )

if (
  any(
    !daily_prize_winners[["Day"]] %in%
    valid_daily_prize_days
  )
) {
  stop(
    "daily_prize_winners.csv contains an invalid Day value.",
    call. = FALSE
  )
}

if (
  anyDuplicated(
    daily_prize_winners[["Day"]]
  ) > 0L
) {
  stop(
    "daily_prize_winners.csv contains more than one winner for the same Day.",
    call. = FALSE
  )
}

if (
  anyDuplicated(
    daily_prize_winners[["DisplayName"]]
  ) > 0L
) {
  stop(
    "daily_prize_winners.csv contains the same DisplayName more than once.",
    call. = FALSE
  )
}

daily_prize_winner_names <-
  daily_prize_winners[["DisplayName"]]

cat(
  "Prior daily prize winners loaded: ",
  length(
    daily_prize_winner_names
  ),
  "\n",
  sep = ""
)


# ============================================================
# 8. Load and Validate Live Leaderboard Snapshots
# ============================================================

live_required_leaderboard_columns <-
  c(
    "Rank",
    "DisplayName",
    "Balance",
    "Trades",
    "ProductsTraded",
    "DailyPnl",
    "TotalPnl"
  )

read_live_leaderboard_snapshot <-
  function(
    path,
    snapshot_name
  ) {
    snapshot <-
      readr::read_csv(
        path,
        show_col_types = FALSE
      )
    
    missing_columns <-
      setdiff(
        live_required_leaderboard_columns,
        names(
          snapshot
        )
      )
    
    if (
      length(
        missing_columns
      ) > 0L
    ) {
      stop(
        paste0(
          snapshot_name,
          " is missing columns: ",
          paste(
            missing_columns,
            collapse = ", "
          )
        ),
        call. = FALSE
      )
    }
    
    if (
      nrow(
        snapshot
      ) == 0L
    ) {
      stop(
        paste0(
          snapshot_name,
          " contains zero rows."
        ),
        call. = FALSE
      )
    }
    
    snapshot <-
      snapshot |>
      mutate(
        Rank =
          as.integer(
            Rank
          ),
        Balance =
          as.numeric(
            Balance
          ),
        DailyPnl =
          as.numeric(
            DailyPnl
          ),
        TotalPnl =
          as.numeric(
            TotalPnl
          )
      )
    
    if (
      any(
        !is.finite(
          snapshot[["Rank"]]
        )
      )
    ) {
      stop(
        paste0(
          snapshot_name,
          " contains invalid Rank values."
        ),
        call. = FALSE
      )
    }
    
    if (
      any(
        !is.finite(
          snapshot[["DailyPnl"]]
        )
      )
    ) {
      stop(
        paste0(
          snapshot_name,
          " contains invalid DailyPnl values."
        ),
        call. = FALSE
      )
    }
    
    if (
      anyDuplicated(
        snapshot[["Rank"]]
      ) > 0L
    ) {
      stop(
        paste0(
          snapshot_name,
          " contains duplicate Rank values."
        ),
        call. = FALSE
      )
    }
    
    snapshot <-
      snapshot |>
      arrange(
        Rank
      )
    
    snapshot
  }

if (
  live_current_t == 0L
) {
  current_leaderboard <- NULL
  previous_leaderboard <- NULL
  
  cat(
    "current_t = 0: no live leaderboard snapshots loaded.\n"
  )
  
} else if (
  live_current_t == 1L
) {
  current_leaderboard <-
    read_live_leaderboard_snapshot(
      CURRENT_LEADERBOARD_PATH,
      "current_leaderboard.csv"
    )
  
  previous_leaderboard <- NULL
  
  cat(
    "Current leaderboard rows: ",
    nrow(
      current_leaderboard
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "current_t = 1: previous leaderboard is the known starting state.\n"
  )
  
} else {
  current_leaderboard <-
    read_live_leaderboard_snapshot(
      CURRENT_LEADERBOARD_PATH,
      "current_leaderboard.csv"
    )
  
  previous_leaderboard <-
    read_live_leaderboard_snapshot(
      PREVIOUS_LEADERBOARD_PATH,
      "previous_leaderboard.csv"
    )
  
  cat(
    "Current leaderboard rows: ",
    nrow(
      current_leaderboard
    ),
    "\n",
    sep = ""
  )
  
  cat(
    "Previous leaderboard rows: ",
    nrow(
      previous_leaderboard
    ),
    "\n",
    sep = ""
  )
}

current_duplicate_display_names <-
  if (
    is.null(
      current_leaderboard
    )
  ) {
    tibble(
      DisplayName = character(0),
      Count = integer(0)
    )
  } else {
    current_leaderboard |>
      count(
        DisplayName,
        name = "Count"
      ) |>
      filter(
        Count > 1L
      )
  }

previous_duplicate_display_names <-
  if (
    is.null(
      previous_leaderboard
    )
  ) {
    tibble(
      DisplayName = character(0),
      Count = integer(0)
    )
  } else {
    previous_leaderboard |>
      count(
        DisplayName,
        name = "Count"
      ) |>
      filter(
        Count > 1L
      )
  }

cat(
  "Current duplicated DisplayName values: ",
  nrow(
    current_duplicate_display_names
  ),
  "\n",
  sep = ""
)

cat(
  "Previous duplicated DisplayName values: ",
  nrow(
    previous_duplicate_display_names
  ),
  "\n",
  sep = ""
)

cat(
  "DisplayName uniqueness is NOT assumed.\n"
)


# ============================================================
# 9. Extract Final-Rank Position Levels
# ============================================================

extract_final_place_balances <-
  function(
    snapshot,
    snapshot_name
  ) {
    selected_rows <-
      snapshot |>
      filter(
        Rank %in%
          FINAL_RANK_NUMBERS
      ) |>
      arrange(
        match(
          Rank,
          FINAL_RANK_NUMBERS
        )
      )
    
    if (
      nrow(
        selected_rows
      ) !=
      length(
        FINAL_RANK_NUMBERS
      )
    ) {
      stop(
        paste0(
          snapshot_name,
          " does not contain every required final rank."
        ),
        call. = FALSE
      )
    }
    
    if (
      !identical(
        as.integer(
          selected_rows[["Rank"]]
        ),
        FINAL_RANK_NUMBERS
      )
    ) {
      stop(
        paste0(
          snapshot_name,
          " final-rank extraction order is invalid."
        ),
        call. = FALSE
      )
    }
    
    balances <-
      as.numeric(
        selected_rows[["Balance"]]
      )
    
    names(
      balances
    ) <-
      FINAL_RANK_NAMES
    
    if (
      any(
        !is.finite(
          balances
        )
      ) ||
      any(
        balances <= 0
      )
    ) {
      stop(
        paste0(
          snapshot_name,
          " required final balances are invalid."
        ),
        call. = FALSE
      )
    }
    
    if (
      !all(
        balances[-length(balances)] >=
        balances[-1]
      )
    ) {
      stop(
        paste0(
          snapshot_name,
          " required final balances are not rank ordered."
        ),
        call. = FALSE
      )
    }
    
    list(
      balances = balances,
      rows = selected_rows
    )
  }

STARTING_BALANCE <- 25000

starting_final_place_balances <-
  setNames(
    rep(
      STARTING_BALANCE,
      length(
        FINAL_RANK_NAMES
      )
    ),
    FINAL_RANK_NAMES
  )

if (
  live_current_t == 0L
) {
  live_current_final_place_balances <-
    starting_final_place_balances
  
  live_previous_final_place_balances <-
    starting_final_place_balances
  
} else if (
  live_current_t == 1L
) {
  current_final_extraction <-
    extract_final_place_balances(
      current_leaderboard,
      "current_leaderboard.csv"
    )
  
  live_current_final_place_balances <-
    current_final_extraction[["balances"]]
  
  live_previous_final_place_balances <-
    starting_final_place_balances
  
} else {
  current_final_extraction <-
    extract_final_place_balances(
      current_leaderboard,
      "current_leaderboard.csv"
    )
  
  previous_final_extraction <-
    extract_final_place_balances(
      previous_leaderboard,
      "previous_leaderboard.csv"
    )
  
  live_current_final_place_balances <-
    current_final_extraction[["balances"]]
  
  live_previous_final_place_balances <-
    previous_final_extraction[["balances"]]
}

cat(
  "Current final-rank position balances constructed.\n"
)

print(
  live_current_final_place_balances
)

cat(
  "Previous final-rank position balances constructed.\n"
)

print(
  live_previous_final_place_balances
)


# ============================================================
# 10. Extract Daily Top-Two Position Levels
# ============================================================

extract_daily_place_pnl <-
  function(
    snapshot,
    snapshot_name
  ) {
    winner_match_counts <-
      vapply(
        daily_prize_winner_names,
        function(
    winner_name
        ) {
          sum(
            snapshot[["DisplayName"]] ==
              winner_name,
            na.rm = TRUE
          )
        },
    integer(1)
      )
    
    if (
      any(
        winner_match_counts > 1L
      )
    ) {
      ambiguous_winners <-
        names(
          winner_match_counts[
            winner_match_counts > 1L
          ]
        )
      
      stop(
        paste0(
          snapshot_name,
          " contains multiple rows matching prior daily prize winner(s): ",
          paste(
            ambiguous_winners,
            collapse = ", "
          ),
          ". Daily prize eligibility cannot be identified unambiguously."
        ),
        call. = FALSE
      )
    }
    
    daily_sorted <-
      snapshot |>
      filter(
        !DisplayName %in%
          daily_prize_winner_names
      ) |>
      arrange(
        desc(
          DailyPnl
        ),
        Rank
      )
    
    if (
      nrow(
        daily_sorted
      ) < 2L
    ) {
      stop(
        paste0(
          snapshot_name,
          " contains fewer than two daily leaderboard rows."
        ),
        call. = FALSE
      )
    }
    
    selected_rows <-
      daily_sorted |>
      slice_head(
        n = 2L
      )
    
    daily_values <-
      as.numeric(
        selected_rows[["DailyPnl"]]
      )
    
    names(
      daily_values
    ) <-
      c(
        "DailyRank1",
        "DailyRank2"
      )
    
    if (
      any(
        !is.finite(
          daily_values
        )
      )
    ) {
      stop(
        paste0(
          snapshot_name,
          " top-two DailyPnl values are invalid."
        ),
        call. = FALSE
      )
    }
    
    if (
      daily_values[[1]] <
      daily_values[[2]]
    ) {
      stop(
        paste0(
          snapshot_name,
          " daily leaderboard ordering is invalid."
        ),
        call. = FALSE
      )
    }
    
    list(
      pnl = daily_values,
      rows = selected_rows
    )
  }

starting_daily_place_pnl <-
  c(
    DailyRank1 = 0,
    DailyRank2 = 0
  )

if (
  live_current_t == 0L
) {
  live_current_daily_place_pnl <-
    starting_daily_place_pnl
  
  live_previous_daily_place_pnl <-
    starting_daily_place_pnl
  
} else if (
  live_current_t == 1L
) {
  current_daily_extraction <-
    extract_daily_place_pnl(
      current_leaderboard,
      "current_leaderboard.csv"
    )
  
  live_current_daily_place_pnl <-
    current_daily_extraction[["pnl"]]
  
  live_previous_daily_place_pnl <-
    starting_daily_place_pnl
  
} else {
  current_daily_extraction <-
    extract_daily_place_pnl(
      current_leaderboard,
      "current_leaderboard.csv"
    )
  
  previous_daily_extraction <-
    extract_daily_place_pnl(
      previous_leaderboard,
      "previous_leaderboard.csv"
    )
  
  live_current_daily_place_pnl <-
    current_daily_extraction[["pnl"]]
  
  live_previous_daily_place_pnl <-
    previous_daily_extraction[["pnl"]]
}

cat(
  "Current daily top-two position PnL constructed.\n"
)

print(
  live_current_daily_place_pnl
)

cat(
  "Previous daily top-two position PnL constructed.\n"
)

print(
  live_previous_daily_place_pnl
)


# ============================================================
# 11. Construct Current 8D Competitor State
# ============================================================

live_current_log_levels <-
  log(
    live_current_final_place_balances
  )

live_current_competitor_state <-
  c(
    Delta_Final_X1 =
      live_current_log_levels[["Rank1"]],
    Delta_Final_D12 =
      live_current_log_levels[["Rank1"]] -
      live_current_log_levels[["Rank2"]],
    Delta_Final_D23 =
      live_current_log_levels[["Rank2"]] -
      live_current_log_levels[["Rank3"]],
    Delta_Final_D34 =
      live_current_log_levels[["Rank3"]] -
      live_current_log_levels[["Rank4"]],
    Delta_Final_D4_15 =
      live_current_log_levels[["Rank4"]] -
      live_current_log_levels[["Rank15"]],
    Delta_Final_D15_16 =
      live_current_log_levels[["Rank15"]] -
      live_current_log_levels[["Rank16"]],
    Delta_Daily_H1 =
      live_current_daily_place_pnl[["DailyRank1"]],
    Delta_Daily_D12 =
      live_current_daily_place_pnl[["DailyRank1"]] -
      live_current_daily_place_pnl[["DailyRank2"]]
  )

if (
  !identical(
    names(
      live_current_competitor_state
    ),
    STATE_NAMES
  )
) {
  stop(
    "Current live competitor state ordering is invalid.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      live_current_competitor_state
    )
  )
) {
  stop(
    "Current live competitor state contains non-finite values.",
    call. = FALSE
  )
}

if (
  any(
    live_current_competitor_state[
      c(
        "Delta_Final_D12",
        "Delta_Final_D23",
        "Delta_Final_D34",
        "Delta_Final_D4_15",
        "Delta_Final_D15_16",
        "Delta_Daily_D12"
      )
    ] <
    -1e-12
  )
) {
  stop(
    "Current live competitor gap state violates ordering.",
    call. = FALSE
  )
}

cat(
  "Current 8D competitor state constructed and validated.\n"
)

print(
  live_current_competitor_state
)


# ============================================================
# 12. Construct Latest Observed Final-Rank Transition
# ============================================================

if (
  live_current_t == 0L
) {
  live_latest_final_rank_returns <-
    setNames(
      rep(
        0,
        length(
          FINAL_RAW_NAMES
        )
      ),
      FINAL_RAW_NAMES
    )
  
  live_latest_observed_t <-
    NA_integer_
  
  live_latest_elapsed_minutes <-
    NA_real_
} else {
  live_latest_final_rank_returns <-
    log(
      live_current_final_place_balances /
        live_previous_final_place_balances
    )
  
  names(
    live_latest_final_rank_returns
  ) <-
    FINAL_RAW_NAMES
  
  live_latest_observed_t <-
    live_current_t -
    1L
  
  live_latest_elapsed_minutes <-
    as.numeric(
      live_competitor_schedule[["CompetitionIntervalMinutes"]][
        live_competitor_schedule[["t_original"]] ==
          live_latest_observed_t
      ]
    )
}

if (
  any(
    !is.finite(
      live_latest_final_rank_returns
    )
  )
) {
  stop(
    "Latest final-rank live transition contains non-finite values.",
    call. = FALSE
  )
}

cat(
  "Latest final-rank position transition constructed.\n"
)

print(
  live_latest_final_rank_returns
)


# ============================================================
# 13. Build Daily Session Features
# ============================================================

live_daily_schedule_features <-
  live_competitor_schedule |>
  group_by(
    ScheduleDate,
    ScheduleDay
  ) |>
  mutate(
    DailyActive =
      ScheduleDay %in%
      ACTIVE_DAILY_PRIZE_DAYS,
    ActiveSessionObservation =
      if_else(
        DailyActive,
        row_number(),
        NA_integer_
      ),
    ActiveSessionObservations =
      if_else(
        DailyActive,
        n(),
        NA_integer_
      ),
    SessionProgress =
      if_else(
        DailyActive &
          ActiveSessionObservations > 1L,
        (
          ActiveSessionObservation -
            1L
        ) /
          (
            ActiveSessionObservations -
              1L
          ),
        0
      ),
    SessionProgressSquared =
      SessionProgress^2,
    SessionStart =
      as.numeric(
        DailyActive &
          ActiveSessionObservation == 1L
      ),
    ElapsedMinutes =
      as.numeric(
        CompetitionIntervalMinutes
      ),
    LogElapsedMinutes =
      log1p(
        ElapsedMinutes
      )
  ) |>
  ungroup()

active_day_counts <-
  live_daily_schedule_features |>
  filter(
    DailyActive
  ) |>
  count(
    ScheduleDay,
    name = "Intervals"
  )

if (
  !setequal(
    active_day_counts[["ScheduleDay"]],
    ACTIVE_DAILY_PRIZE_DAYS
  )
) {
  stop(
    "Live daily schedule is missing an active daily-prize day.",
    call. = FALSE
  )
}

if (
  any(
    active_day_counts[["Intervals"]] != 31L
  )
) {
  stop(
    "Each active daily-prize session must contain 31 intervals.",
    call. = FALSE
  )
}

cat(
  "Live daily session structure validated.\n"
)


# ============================================================
# 14. Construct Latest Observed Daily Transition
# ============================================================

live_latest_daily_rank_increments <-
  c(
    h1 = 0,
    h2 = 0
  )

live_latest_daily_session_start <-
  FALSE

if (
  live_current_t > 0L
) {
  latest_daily_feature_row <-
    live_daily_schedule_features |>
    filter(
      t_original ==
        live_latest_observed_t
    )
  
  if (
    nrow(
      latest_daily_feature_row
    ) != 1L
  ) {
    stop(
      "Could not identify the latest observed daily feature row.",
      call. = FALSE
    )
  }
  
  latest_daily_active <-
    isTRUE(
      latest_daily_feature_row[["DailyActive"]][[1]]
    )
  
  live_latest_daily_session_start <-
    isTRUE(
      latest_daily_feature_row[["SessionStart"]][[1]] == 1
    )
  
  if (
    latest_daily_active
  ) {
    if (
      live_latest_daily_session_start
    ) {
      live_latest_daily_rank_increments <-
        c(
          h1 =
            live_current_daily_place_pnl[["DailyRank1"]],
          h2 =
            live_current_daily_place_pnl[["DailyRank2"]]
        )
    } else {
      live_latest_daily_rank_increments <-
        c(
          h1 =
            live_current_daily_place_pnl[["DailyRank1"]] -
            live_previous_daily_place_pnl[["DailyRank1"]],
          h2 =
            live_current_daily_place_pnl[["DailyRank2"]] -
            live_previous_daily_place_pnl[["DailyRank2"]]
        )
    }
  }
}

if (
  any(
    !is.finite(
      live_latest_daily_rank_increments
    )
  )
) {
  stop(
    "Latest daily live transition contains non-finite values.",
    call. = FALSE
  )
}

live_latest_rank1_nonzero <-
  as.numeric(
    live_latest_daily_rank_increments[["h1"]] != 0
  )

live_latest_rank2_nonzero <-
  as.numeric(
    live_latest_daily_rank_increments[["h2"]] != 0
  )

cat(
  "Latest daily transition constructed.\n"
)

cat(
  "Latest daily transition is session start: ",
  live_latest_daily_session_start,
  "\n",
  sep = ""
)

print(
  live_latest_daily_rank_increments
)


# ============================================================
# 15. Helper to Source Notebook 32 Into an Environment
# ============================================================

extract_r_code_from_rmd <-
  function(
    rmd_path
  ) {
    rmd_lines <-
      readLines(
        rmd_path,
        warn = FALSE
      )
    
    extracted_code <-
      character()
    
    inside_r_chunk <-
      FALSE
    
    for (
      current_line in
      rmd_lines
    ) {
      if (
        grepl(
          "^```\\{r",
          current_line
        )
      ) {
        inside_r_chunk <-
          TRUE
        next
      }
      
      if (
        inside_r_chunk &&
        grepl(
          "^```\\s*$",
          current_line
        )
      ) {
        inside_r_chunk <-
          FALSE
        extracted_code <-
          c(
            extracted_code,
            ""
          )
        next
      }
      
      if (
        inside_r_chunk
      ) {
        extracted_code <-
          c(
            extracted_code,
            current_line
          )
      }
    }
    
    extracted_code
  }

source_rmd_into_environment <-
  function(
    rmd_path,
    target_environment
  ) {
    original_working_directory <-
      getwd()
    
    on.exit(
      setwd(
        original_working_directory
      ),
      add = TRUE
    )
    
    extracted_code <-
      extract_r_code_from_rmd(
        rmd_path
      )
    
    temporary_script <-
      tempfile(
        fileext = ".R"
      )
    
    writeLines(
      extracted_code,
      temporary_script
    )
    
    on.exit(
      unlink(
        temporary_script
      ),
      add = TRUE
    )
    
    invisible(
      capture.output(
        source(
          temporary_script,
          local = target_environment
        )
      )
    )
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 16. Reconstruct Exact Frozen 8D Competitor Dependence
# ============================================================

joint_notebook_environment <-
  new.env(
    parent = globalenv()
  )

source_rmd_into_environment(
  rmd_path = NOTEBOOK_32_PATH,
  target_environment = joint_notebook_environment
)

if (
  !exists(
    "joint_residual_correlation",
    envir = joint_notebook_environment,
    inherits = FALSE
  )
) {
  stop(
    "Notebook 32 did not create joint_residual_correlation.",
    call. = FALSE
  )
}

if (
  !exists(
    "JOINT_STATE_TRANSFORM",
    envir = joint_notebook_environment,
    inherits = TRUE
  )
) {
  stop(
    "Notebook 32 did not expose JOINT_STATE_TRANSFORM.",
    call. = FALSE
  )
}

live_joint_residual_correlation <-
  get(
    "joint_residual_correlation",
    envir = joint_notebook_environment,
    inherits = FALSE
  )

live_state_transform <-
  get(
    "JOINT_STATE_TRANSFORM",
    envir = joint_notebook_environment,
    inherits = TRUE
  )

rownames(
  live_joint_residual_correlation
) <-
  JOINT_RAW_NAMES_PRODUCTION

colnames(
  live_joint_residual_correlation
) <-
  JOINT_RAW_NAMES_PRODUCTION

rownames(
  live_state_transform
) <-
  STATE_NAMES

colnames(
  live_state_transform
) <-
  JOINT_RAW_NAMES_PRODUCTION

if (
  !all(
    dim(
      live_joint_residual_correlation
    ) ==
    c(
      8L,
      8L
    )
  )
) {
  stop(
    "Frozen joint residual correlation is not 8x8.",
    call. = FALSE
  )
}

if (
  !all(
    dim(
      live_state_transform
    ) ==
    c(
      8L,
      8L
    )
  )
) {
  stop(
    "Frozen state transform is not 8x8.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      live_joint_residual_correlation
    )
  )
) {
  stop(
    "Frozen joint residual correlation contains non-finite values.",
    call. = FALSE
  )
}

if (
  max(
    abs(
      live_joint_residual_correlation -
      t(
        live_joint_residual_correlation
      )
    )
  ) >
  COVARIANCE_SYMMETRY_TOLERANCE
) {
  stop(
    "Frozen joint residual correlation is not symmetric.",
    call. = FALSE
  )
}

live_joint_correlation_eigenvalues <-
  eigen(
    live_joint_residual_correlation,
    symmetric = TRUE,
    only.values = TRUE
  )[["values"]]

if (
  any(
    live_joint_correlation_eigenvalues <= 0
  )
) {
  stop(
    "Frozen joint residual correlation is not positive definite.",
    call. = FALSE
  )
}

cat(
  "Exact frozen Notebook 32 competitor dependence validated.\n"
)

cat(
  "Minimum frozen R8 eigenvalue: ",
  min(
    live_joint_correlation_eigenvalues
  ),
  "\n",
  sep = ""
)


# ============================================================
# 17. Build Historical Final-Rank Training Data
# ============================================================

final_rank_object <-
  prepare_final_rank_data(
    path = FINAL_RANK_DATA_PATH
  )

live_final_rank_training_data <-
  final_rank_object[["data"]]

if (
  "Date" %in%
  names(
    live_final_rank_training_data
  )
) {
  live_final_rank_training_data <-
    live_final_rank_training_data |>
    mutate(
      Timestamp =
        as.POSIXct(
          paste0(
            "2026-",
            stringr::str_replace(
              Date,
              "T",
              " "
            )
          ),
          format = "%Y-%m-%d %H-%M-%S",
          tz = "America/Chicago"
        )
    ) |>
    arrange(
      Timestamp
    ) |>
    mutate(
      Observation =
        row_number(),
      ElapsedMinutes =
        as.numeric(
          difftime(
            Timestamp,
            lag(
              Timestamp
            ),
            units = "mins"
          )
        )
    )
}

required_final_training_columns <-
  c(
    FINAL_RANK_NAMES,
    "Observation",
    "ElapsedMinutes"
  )

missing_final_training_columns <-
  setdiff(
    required_final_training_columns,
    names(
      live_final_rank_training_data
    )
  )

if (
  length(
    missing_final_training_columns
  ) > 0L
) {
  stop(
    paste0(
      "Final-rank training data is missing columns: ",
      paste(
        missing_final_training_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

cat(
  "Historical final-rank training observations: ",
  nrow(
    live_final_rank_training_data
  ),
  "\n",
  sep = ""
)


# ============================================================
# 18. Validate Frozen Final-Rank Model Choices
# ============================================================

expected_final_mean_models <-
  c(
    Rank1 = "hurdle_time_duration",
    Rank2 = "linear_time",
    Rank3 = "hurdle_time_duration",
    Rank4 = "linear_time",
    Rank15 = "quadratic_time",
    Rank16 = "linear_time"
  )

expected_final_variance_models <-
  c(
    Rank1 = "long_interval",
    Rank2 = "elapsed_linear",
    Rank3 = "elapsed_log",
    Rank4 = "elapsed_linear",
    Rank15 = "elapsed_linear",
    Rank16 = "constant"
  )

if (
  !identical(
    FINAL_SELECTED_MEAN_MODELS,
    expected_final_mean_models
  )
) {
  stop(
    "Frozen final-rank mean specification changed unexpectedly.",
    call. = FALSE
  )
}

if (
  !identical(
    FINAL_SELECTED_VARIANCE_MODELS,
    expected_final_variance_models
  )
) {
  stop(
    "Frozen final-rank variance specification changed unexpectedly.",
    call. = FALSE
  )
}

cat(
  "Frozen final-rank model specification validated.\n"
)


# ============================================================
# 19. Fit Frozen Final-Rank Mean Models
# ============================================================

live_final_mean_models <-
  list()

for (
  rank_name in
  FINAL_RANK_NAMES
) {
  model_type <-
    FINAL_SELECTED_MEAN_MODELS[[rank_name]]
  
  cat(
    "Fitting final mean model: ",
    rank_name,
    " | ",
    model_type,
    "\n",
    sep = ""
  )
  
  fitted_model <-
    fit_competitor_mean_model(
      data = live_final_rank_training_data,
      rank_name = rank_name,
      model_type = model_type,
      total_observations = FINAL_TOTAL_COMPETITION_OBSERVATIONS
    )
  
  if (
    is.null(
      fitted_model
    ) ||
    is.null(
      fitted_model[["fit"]]
    )
  ) {
    stop(
      paste0(
        "Final mean model fit failed for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  live_final_mean_models[[rank_name]] <-
    fitted_model
}

cat(
  "All frozen final-rank mean models fitted.\n"
)


# ============================================================
# 20. Construct Final-Rank Residual Histories
# ============================================================

live_final_residual_history <-
  list()

for (
  rank_name in
  FINAL_RANK_NAMES
) {
  residual_data <-
    construct_competitor_mean_residual_data(
      data = live_final_rank_training_data,
      rank_name = rank_name,
      mean_model_type = FINAL_SELECTED_MEAN_MODELS[[rank_name]],
      total_observations = FINAL_TOTAL_COMPETITION_OBSERVATIONS
    )
  
  if (
    is.null(
      residual_data
    )
  ) {
    stop(
      paste0(
        "Could not construct final-rank residual history for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  usable_residual_data <-
    residual_data |>
    filter(
      is.finite(
        Residual
      ),
      is.finite(
        SquaredResidual
      )
    )
  
  if (
    nrow(
      usable_residual_data
    ) == 0L
  ) {
    stop(
      paste0(
        "No usable residual observations for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  live_final_residual_history[[rank_name]] <-
    usable_residual_data
}

cat(
  "Final-rank residual histories constructed.\n"
)


# ============================================================
# 21. Fit Frozen Final-Rank Variance Models
# ============================================================

live_final_variance_models <-
  list()

for (
  rank_name in
  FINAL_RANK_NAMES
) {
  variance_model_type <-
    FINAL_SELECTED_VARIANCE_MODELS[[rank_name]]
  
  cat(
    "Fitting final variance model: ",
    rank_name,
    " | ",
    variance_model_type,
    "\n",
    sep = ""
  )
  
  fitted_variance_model <-
    fit_competitor_variance_model(
      residual_data = live_final_residual_history[[rank_name]],
      variance_model_type = variance_model_type
    )
  
  if (
    is.null(
      fitted_variance_model
    ) ||
    is.null(
      fitted_variance_model[["fit"]]
    )
  ) {
    stop(
      paste0(
        "Final variance model fit failed for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  live_final_variance_models[[rank_name]] <-
    fitted_variance_model
}

cat(
  "All frozen final-rank variance models fitted.\n"
)


# ============================================================
# 22. Build Final-Rank Live Forecast Features
# ============================================================

live_final_production_features <-
  live_competitor_schedule |>
  mutate(
    ProductionProgress =
      t_original /
      (
        LIVE_EXPECTED_HORIZON -
          1L
      ),
    Observation =
      1 +
      ProductionProgress *
      (
        FINAL_TOTAL_COMPETITION_OBSERVATIONS -
          1
      ),
    ElapsedMinutes =
      as.numeric(
        CompetitionIntervalMinutes
      )
  )

if (
  abs(
    live_final_production_features[["ProductionProgress"]][[1]]
  ) >
  1e-12
) {
  stop(
    "Final-rank production progress does not begin at zero.",
    call. = FALSE
  )
}

if (
  abs(
    tail(
      live_final_production_features[["ProductionProgress"]],
      1
    ) -
    1
  ) >
  1e-12
) {
  stop(
    "Final-rank production progress does not end at one.",
    call. = FALSE
  )
}

cat(
  "Final-rank live forecast features constructed.\n"
)


# ============================================================
# 23. Forecast Final-Rank Conditional Means
# ============================================================

live_final_mean_matrix <-
  matrix(
    0,
    nrow = LIVE_EXPECTED_HORIZON,
    ncol = length(
      FINAL_RANK_NAMES
    ),
    dimnames = list(
      NULL,
      FINAL_RAW_NAMES
    )
  )

live_active_rows <-
  which(
    live_final_production_features[["t_original"]] >=
      live_current_t
  )

live_active_final_features <-
  live_final_production_features[
    live_active_rows,
    ,
    drop = FALSE
  ]

for (
  rank_index in
  seq_along(
    FINAL_RANK_NAMES
  )
) {
  rank_name <-
    FINAL_RANK_NAMES[[rank_index]]
  
  forecast_values <-
    forecast_competitor_mean(
      model_object = live_final_mean_models[[rank_name]],
      future_data = live_active_final_features
    )
  
  forecast_values <-
    as.numeric(
      forecast_values
    )
  
  if (
    length(
      forecast_values
    ) !=
    length(
      live_active_rows
    )
  ) {
    stop(
      paste0(
        "Unexpected final mean forecast length for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        forecast_values
      )
    )
  ) {
    stop(
      paste0(
        "Non-finite final mean forecast for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  live_final_mean_matrix[
    live_active_rows,
    FINAL_RAW_NAMES[[rank_index]]
  ] <-
    forecast_values
}

cat(
  "Final-rank conditional mean path constructed.\n"
)


# ============================================================
# 24. Forecast Final-Rank Conditional Variances
# ============================================================

live_final_variance_matrix <-
  matrix(
    0,
    nrow = LIVE_EXPECTED_HORIZON,
    ncol = length(
      FINAL_RANK_NAMES
    ),
    dimnames = list(
      NULL,
      FINAL_RAW_NAMES
    )
  )

for (
  rank_index in
  seq_along(
    FINAL_RANK_NAMES
  )
) {
  rank_name <-
    FINAL_RANK_NAMES[[rank_index]]
  
  forecast_values <-
    forecast_competitor_variance(
      variance_model_object = live_final_variance_models[[rank_name]],
      future_data = live_active_final_features
    )
  
  forecast_values <-
    as.numeric(
      forecast_values
    )
  
  if (
    length(
      forecast_values
    ) !=
    length(
      live_active_rows
    )
  ) {
    stop(
      paste0(
        "Unexpected final variance forecast length for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        forecast_values
      )
    ) ||
    any(
      forecast_values <= 0
    )
  ) {
    stop(
      paste0(
        "Invalid final variance forecast for ",
        rank_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  live_final_variance_matrix[
    live_active_rows,
    FINAL_RAW_NAMES[[rank_index]]
  ] <-
    forecast_values
}

cat(
  "Final-rank conditional variance path constructed.\n"
)


# ============================================================
# 25. Fit Frozen Daily Production Models
# ============================================================

live_daily_production_system <-
  build_daily_competitor_forecasting_system()

validate_daily_competitor_forecasting_system(
  live_daily_production_system
)

if (
  !identical(
    live_daily_production_system[["selected_mean_models"]][["Rank1"]],
    "hurdle_persistence"
  )
) {
  stop(
    "Frozen daily Rank1 mean model changed unexpectedly.",
    call. = FALSE
  )
}

if (
  !identical(
    live_daily_production_system[["selected_mean_models"]][["Rank2"]],
    "hurdle_persistence"
  )
) {
  stop(
    "Frozen daily Rank2 mean model changed unexpectedly.",
    call. = FALSE
  )
}

if (
  !identical(
    live_daily_production_system[["selected_variance_models"]][["Rank1"]],
    "constant"
  )
) {
  stop(
    "Frozen daily Rank1 variance model changed unexpectedly.",
    call. = FALSE
  )
}

if (
  !identical(
    live_daily_production_system[["selected_variance_models"]][["Rank2"]],
    "quadratic_progress"
  )
) {
  stop(
    "Frozen daily Rank2 variance model changed unexpectedly.",
    call. = FALSE
  )
}

cat(
  "Frozen daily production models fitted and validated.\n"
)


# ============================================================
# 26. Daily Hurdle Expected-Mean Helper
# ============================================================

forecast_live_daily_hurdle_expected_mean <-
  function(
    fitted_mean_model,
    newdata
  ) {
    probability_model <-
      fitted_mean_model[["probability_fit"]]
    
    magnitude_model <-
      fitted_mean_model[["magnitude_fit"]]
    
    if (
      is.null(
        probability_model
      ) ||
      is.null(
        magnitude_model
      )
    ) {
      stop(
        "Daily hurdle mean model is incomplete.",
        call. = FALSE
      )
    }
    
    probability_nonzero <-
      as.numeric(
        predict(
          probability_model,
          newdata = newdata,
          type = "response"
        )
      )
    
    conditional_nonzero_mean <-
      as.numeric(
        predict(
          magnitude_model,
          newdata = newdata
        )
      )
    
    if (
      length(
        probability_nonzero
      ) != 1L ||
      length(
        conditional_nonzero_mean
      ) != 1L ||
      !is.finite(
        probability_nonzero
      ) ||
      !is.finite(
        conditional_nonzero_mean
      )
    ) {
      stop(
        "Daily hurdle mean prediction is invalid.",
        call. = FALSE
      )
    }
    
    probability_nonzero <-
      min(
        max(
          probability_nonzero,
          0
        ),
        1
      )
    
    unconditional_mean <-
      probability_nonzero *
      conditional_nonzero_mean
    
    c(
      ProbabilityNonzero =
        probability_nonzero,
      ConditionalNonzeroMean =
        conditional_nonzero_mean,
      UnconditionalMean =
        unconditional_mean
    )
  }


# ============================================================
# 27. Forecast Live Daily Means and Variances Recursively
# ============================================================

live_daily_mean_matrix <-
  matrix(
    0,
    nrow = LIVE_EXPECTED_HORIZON,
    ncol = 2L,
    dimnames = list(
      NULL,
      DAILY_RAW_NAMES
    )
  )

live_daily_variance_matrix <-
  matrix(
    0,
    nrow = LIVE_EXPECTED_HORIZON,
    ncol = 2L,
    dimnames = list(
      NULL,
      DAILY_RAW_NAMES
    )
  )

live_daily_probability_matrix <-
  matrix(
    NA_real_,
    nrow = LIVE_EXPECTED_HORIZON,
    ncol = 2L,
    dimnames = list(
      NULL,
      c(
        "Rank1",
        "Rank2"
      )
    )
  )

live_daily_conditional_nonzero_mean_matrix <-
  matrix(
    NA_real_,
    nrow = LIVE_EXPECTED_HORIZON,
    ncol = 2L,
    dimnames = list(
      NULL,
      c(
        "Rank1",
        "Rank2"
      )
    )
  )

daily_rank1_mean_model <-
  live_daily_production_system[["fitted_mean_models"]][["Rank1"]]

daily_rank2_mean_model <-
  live_daily_production_system[["fitted_mean_models"]][["Rank2"]]

daily_rank1_variance_model <-
  live_daily_production_system[["fitted_variance_models"]][["Rank1"]]

daily_rank2_variance_model <-
  live_daily_production_system[["fitted_variance_models"]][["Rank2"]]

for (
  current_day in
  ACTIVE_DAILY_PRIZE_DAYS
) {
  day_rows <-
    which(
      live_daily_schedule_features[["ScheduleDay"]] ==
        current_day
    )
  
  if (
    length(
      day_rows
    ) != 31L
  ) {
    stop(
      paste0(
        "Expected 31 live intervals for ",
        current_day,
        "."
      ),
      call. = FALSE
    )
  }
  
  future_day_rows <-
    day_rows[
      live_daily_schedule_features[["t_original"]][day_rows] >=
        live_current_t
    ]
  
  if (
    length(
      future_day_rows
    ) == 0L
  ) {
    next
  }
  
  first_future_row <-
    future_day_rows[[1]]
  
  first_future_t <-
    live_daily_schedule_features[["t_original"]][[first_future_row]]
  
  first_future_session_start <-
    live_daily_schedule_features[["SessionStart"]][[first_future_row]] == 1
  
  if (
    current_day == live_current_day &&
    first_future_t == live_current_t &&
    !first_future_session_start &&
    live_current_t > 0L
  ) {
    previous_rank1_nonzero_probability <-
      live_latest_rank1_nonzero
    
    previous_rank2_nonzero_probability <-
      live_latest_rank2_nonzero
  } else {
    previous_rank1_nonzero_probability <-
      0
    
    previous_rank2_nonzero_probability <-
      0
  }
  
  for (
    row_index in
    future_day_rows
  ) {
    current_features <-
      live_daily_schedule_features[
        row_index,
        ,
        drop = FALSE
      ] |>
      mutate(
        PreviousRank1Nonzero =
          previous_rank1_nonzero_probability,
        PreviousRank2Nonzero =
          previous_rank2_nonzero_probability
      )
    
    rank1_mean_forecast <-
      forecast_live_daily_hurdle_expected_mean(
        fitted_mean_model = daily_rank1_mean_model,
        newdata = current_features
      )
    
    rank2_mean_forecast <-
      forecast_live_daily_hurdle_expected_mean(
        fitted_mean_model = daily_rank2_mean_model,
        newdata = current_features
      )
    
    rank1_variance_forecast <-
      predict_daily_marginal_variance(
        fitted_variance_model = daily_rank1_variance_model,
        newdata = current_features
      )
    
    rank2_variance_forecast <-
      predict_daily_marginal_variance(
        fitted_variance_model = daily_rank2_variance_model,
        newdata = current_features
      )
    
    live_daily_mean_matrix[
      row_index,
      "h1"
    ] <-
      rank1_mean_forecast[["UnconditionalMean"]]
    
    live_daily_mean_matrix[
      row_index,
      "h2"
    ] <-
      rank2_mean_forecast[["UnconditionalMean"]]
    
    live_daily_variance_matrix[
      row_index,
      "h1"
    ] <-
      rank1_variance_forecast
    
    live_daily_variance_matrix[
      row_index,
      "h2"
    ] <-
      rank2_variance_forecast
    
    live_daily_probability_matrix[
      row_index,
      "Rank1"
    ] <-
      rank1_mean_forecast[["ProbabilityNonzero"]]
    
    live_daily_probability_matrix[
      row_index,
      "Rank2"
    ] <-
      rank2_mean_forecast[["ProbabilityNonzero"]]
    
    live_daily_conditional_nonzero_mean_matrix[
      row_index,
      "Rank1"
    ] <-
      rank1_mean_forecast[["ConditionalNonzeroMean"]]
    
    live_daily_conditional_nonzero_mean_matrix[
      row_index,
      "Rank2"
    ] <-
      rank2_mean_forecast[["ConditionalNonzeroMean"]]
    
    previous_rank1_nonzero_probability <-
      rank1_mean_forecast[["ProbabilityNonzero"]]
    
    previous_rank2_nonzero_probability <-
      rank2_mean_forecast[["ProbabilityNonzero"]]
  }
}

cat(
  "Recursive live daily forecasts completed.\n"
)


# ============================================================
# 28. Validate Live Daily Forecasts
# ============================================================

live_future_rows <-
  which(
    live_daily_schedule_features[["t_original"]] >=
      live_current_t
  )

live_future_daily_active_rows <-
  live_future_rows[
    live_daily_schedule_features[["DailyActive"]][live_future_rows]
  ]

live_future_daily_inactive_rows <-
  live_future_rows[
    !live_daily_schedule_features[["DailyActive"]][live_future_rows]
  ]

if (
  length(
    live_future_daily_active_rows
  ) > 0L
) {
  if (
    any(
      !is.finite(
        live_daily_mean_matrix[
          live_future_daily_active_rows,
          ,
          drop = FALSE
        ]
      )
    )
  ) {
    stop(
      "Active future daily mean forecasts contain non-finite values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        live_daily_variance_matrix[
          live_future_daily_active_rows,
          ,
          drop = FALSE
        ]
      )
    ) ||
    any(
      live_daily_variance_matrix[
        live_future_daily_active_rows,
        ,
        drop = FALSE
      ] <= 0
    )
  ) {
    stop(
      "Active future daily variance forecasts are invalid.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        live_daily_probability_matrix[
          live_future_daily_active_rows,
          ,
          drop = FALSE
        ]
      )
    ) ||
    any(
      live_daily_probability_matrix[
        live_future_daily_active_rows,
        ,
        drop = FALSE
      ] < 0
    ) ||
    any(
      live_daily_probability_matrix[
        live_future_daily_active_rows,
        ,
        drop = FALSE
      ] > 1
    )
  ) {
    stop(
      "Active future daily nonzero probabilities are invalid.",
      call. = FALSE
    )
  }
}

if (
  length(
    live_future_daily_inactive_rows
  ) > 0L
) {
  if (
    any(
      live_daily_mean_matrix[
        live_future_daily_inactive_rows,
        ,
        drop = FALSE
      ] != 0
    )
  ) {
    stop(
      "Inactive future daily means are not exactly zero.",
      call. = FALSE
    )
  }
  
  if (
    any(
      live_daily_variance_matrix[
        live_future_daily_inactive_rows,
        ,
        drop = FALSE
      ] != 0
    )
  ) {
    stop(
      "Inactive future daily variances are not exactly zero.",
      call. = FALSE
    )
  }
}

cat(
  "Live daily forecast validation passed.\n"
)


# ============================================================
# 29. Assemble Raw 8D Mean and Variance Paths
# ============================================================

live_raw_mean_matrix <-
  cbind(
    live_final_mean_matrix,
    live_daily_mean_matrix
  )

colnames(
  live_raw_mean_matrix
) <-
  JOINT_RAW_NAMES_PRODUCTION

live_raw_variance_matrix <-
  cbind(
    live_final_variance_matrix,
    live_daily_variance_matrix
  )

colnames(
  live_raw_variance_matrix
) <-
  JOINT_RAW_NAMES_PRODUCTION

if (
  !all(
    dim(
      live_raw_mean_matrix
    ) ==
    c(
      147L,
      8L
    )
  )
) {
  stop(
    "Raw live competitor mean matrix has invalid dimensions.",
    call. = FALSE
  )
}

if (
  !all(
    dim(
      live_raw_variance_matrix
    ) ==
    c(
      147L,
      8L
    )
  )
) {
  stop(
    "Raw live competitor variance matrix has invalid dimensions.",
    call. = FALSE
  )
}

if (
  live_current_t > 0L
) {
  live_past_rows <-
    seq_len(
      live_current_t
    )
  
  if (
    any(
      live_raw_mean_matrix[
        live_past_rows,
        ,
        drop = FALSE
      ] != 0
    )
  ) {
    stop(
      "Past raw competitor mean rows are not inert.",
      call. = FALSE
    )
  }
  
  if (
    any(
      live_raw_variance_matrix[
        live_past_rows,
        ,
        drop = FALSE
      ] != 0
    )
  ) {
    stop(
      "Past raw competitor variance rows are not inert.",
      call. = FALSE
    )
  }
}

cat(
  "Raw 8D live competitor paths assembled.\n"
)


# ============================================================
# 30. Construct Raw 8D Covariance Path
# ============================================================

live_raw_covariance_array <-
  array(
    0,
    dim = c(
      LIVE_EXPECTED_HORIZON,
      8L,
      8L
    ),
    dimnames = list(
      NULL,
      JOINT_RAW_NAMES_PRODUCTION,
      JOINT_RAW_NAMES_PRODUCTION
    )
  )

for (
  time_index in
  seq_len(
    LIVE_EXPECTED_HORIZON
  )
) {
  t_value <-
    time_index -
    1L
  
  if (
    t_value <
    live_current_t
  ) {
    next
  }
  
  marginal_variance <-
    live_raw_variance_matrix[
      time_index,
      ,
      drop = TRUE
    ]
  
  if (
    any(
      marginal_variance < 0
    ) ||
    any(
      !is.finite(
        marginal_variance
      )
    )
  ) {
    stop(
      paste0(
        "Invalid raw marginal variance at t = ",
        t_value,
        "."
      ),
      call. = FALSE
    )
  }
  
  marginal_sd <-
    sqrt(
      pmax(
        marginal_variance,
        0
      )
    )
  
  D_t <-
    diag(
      marginal_sd,
      nrow = 8L,
      ncol = 8L
    )
  
  raw_covariance <-
    D_t %*%
    live_joint_residual_correlation %*%
    D_t
  
  raw_covariance <-
    (
      raw_covariance +
        t(
          raw_covariance
        )
    ) /
    2
  
  rownames(
    raw_covariance
  ) <-
    JOINT_RAW_NAMES_PRODUCTION
  
  colnames(
    raw_covariance
  ) <-
    JOINT_RAW_NAMES_PRODUCTION
  
  live_raw_covariance_array[
    time_index,
    ,
  ] <-
    raw_covariance
}

cat(
  "Raw 8D live covariance path constructed.\n"
)


# ============================================================
# 31. Transform Live Means Into State Space
# ============================================================

live_state_mean_matrix <-
  matrix(
    0,
    nrow = LIVE_EXPECTED_HORIZON,
    ncol = 8L,
    dimnames = list(
      NULL,
      STATE_NAMES
    )
  )

for (
  time_index in
  seq_len(
    LIVE_EXPECTED_HORIZON
  )
) {
  live_state_mean_matrix[
    time_index,
    
  ] <-
    as.numeric(
      live_state_transform %*%
        live_raw_mean_matrix[
          time_index,
          ,
          drop = TRUE
        ]
    )
}

cat(
  "Live competitor state mean path constructed.\n"
)


# ============================================================
# 32. Transform Live Covariances Into State Space
# ============================================================

live_state_covariance_array <-
  array(
    0,
    dim = c(
      LIVE_EXPECTED_HORIZON,
      8L,
      8L
    ),
    dimnames = list(
      NULL,
      STATE_NAMES,
      STATE_NAMES
    )
  )

for (
  time_index in
  seq_len(
    LIVE_EXPECTED_HORIZON
  )
) {
  raw_covariance <-
    live_raw_covariance_array[
      time_index,
      ,
      ,
      drop = TRUE
    ]
  
  state_covariance <-
    live_state_transform %*%
    raw_covariance %*%
    t(
      live_state_transform
    )
  
  state_covariance <-
    (
      state_covariance +
        t(
          state_covariance
        )
    ) /
    2
  
  rownames(
    state_covariance
  ) <-
    STATE_NAMES
  
  colnames(
    state_covariance
  ) <-
    STATE_NAMES
  
  live_state_covariance_array[
    time_index,
    ,
  ] <-
    state_covariance
}

cat(
  "Live competitor state covariance path constructed.\n"
)


# ============================================================
# 33. Numerical Validation of Future Covariance Matrices
# ============================================================

live_covariance_validation <-
  vector(
    mode = "list",
    length = length(
      live_future_rows
    )
  )

for (
  validation_index in
  seq_along(
    live_future_rows
  )
) {
  time_index <-
    live_future_rows[[validation_index]]
  
  t_value <-
    time_index -
    1L
  
  covariance_matrix <-
    live_state_covariance_array[
      time_index,
      ,
      ,
      drop = TRUE
    ]
  
  symmetry_error <-
    max(
      abs(
        covariance_matrix -
          t(
            covariance_matrix
          )
      )
    )
  
  eigenvalues <-
    eigen(
      covariance_matrix,
      symmetric = TRUE,
      only.values = TRUE
    )[["values"]]
  
  minimum_eigenvalue <-
    min(
      eigenvalues
    )
  
  minimum_diagonal <-
    min(
      diag(
        covariance_matrix
      )
    )
  
  if (
    symmetry_error >
    COVARIANCE_SYMMETRY_TOLERANCE
  ) {
    stop(
      paste0(
        "State covariance symmetry validation failed at t = ",
        t_value,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    minimum_eigenvalue <
    -PSD_EIGENVALUE_TOLERANCE
  ) {
    stop(
      paste0(
        "State covariance PSD validation failed at t = ",
        t_value,
        ". Minimum eigenvalue = ",
        minimum_eigenvalue,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    minimum_diagonal <
    -PSD_EIGENVALUE_TOLERANCE
  ) {
    stop(
      paste0(
        "State covariance has negative variance at t = ",
        t_value,
        "."
      ),
      call. = FALSE
    )
  }
  
  live_covariance_validation[[validation_index]] <-
    tibble(
      t_original = t_value,
      MinimumEigenvalue = minimum_eigenvalue,
      MinimumDiagonal = minimum_diagonal,
      MaximumSymmetryError = symmetry_error
    )
}

live_covariance_validation <-
  bind_rows(
    live_covariance_validation
  )

cat(
  "All future live state covariance matrices validated.\n"
)

cat(
  "Minimum future state covariance eigenvalue: ",
  min(
    live_covariance_validation[["MinimumEigenvalue"]]
  ),
  "\n",
  sep = ""
)


# ============================================================
# 34. Validate State Transform Explicitly
# ============================================================

maximum_state_mean_transformation_error <-
  0

maximum_state_covariance_transformation_error <-
  0

for (
  time_index in
  seq_len(
    LIVE_EXPECTED_HORIZON
  )
) {
  expected_state_mean <-
    as.numeric(
      live_state_transform %*%
        live_raw_mean_matrix[
          time_index,
          ,
          drop = TRUE
        ]
    )
  
  actual_state_mean <-
    live_state_mean_matrix[
      time_index,
      ,
      drop = TRUE
    ]
  
  current_mean_error <-
    max(
      abs(
        expected_state_mean -
          actual_state_mean
      )
    )
  
  maximum_state_mean_transformation_error <-
    max(
      maximum_state_mean_transformation_error,
      current_mean_error
    )
  
  expected_state_covariance <-
    live_state_transform %*%
    live_raw_covariance_array[
      time_index,
      ,
      ,
      drop = TRUE
    ] %*%
    t(
      live_state_transform
    )
  
  actual_state_covariance <-
    live_state_covariance_array[
      time_index,
      ,
      ,
      drop = TRUE
    ]
  
  current_covariance_error <-
    max(
      abs(
        expected_state_covariance -
          actual_state_covariance
      )
    )
  
  maximum_state_covariance_transformation_error <-
    max(
      maximum_state_covariance_transformation_error,
      current_covariance_error
    )
}

if (
  maximum_state_mean_transformation_error >
  1e-12
) {
  stop(
    "State mean transformation validation failed.",
    call. = FALSE
  )
}

if (
  maximum_state_covariance_transformation_error >
  1e-10
) {
  stop(
    "State covariance transformation validation failed.",
    call. = FALSE
  )
}

cat(
  "State transformation reproduced exactly within tolerance.\n"
)


# ============================================================
# 35. Validate Inactive Daily State
# ============================================================

inactive_future_rows <-
  which(
    live_competitor_schedule[["t_original"]] >=
      live_current_t &
      !live_competitor_schedule[["ScheduleDay"]] %in%
      ACTIVE_DAILY_PRIZE_DAYS
  )

inactive_daily_state_mean_max <-
  0

inactive_daily_state_covariance_max <-
  0

if (
  length(
    inactive_future_rows
  ) > 0L
) {
  inactive_daily_state_mean_max <-
    max(
      abs(
        live_state_mean_matrix[
          inactive_future_rows,
          c(
            "Delta_Daily_H1",
            "Delta_Daily_D12"
          ),
          drop = FALSE
        ]
      )
    )
  
  inactive_daily_state_covariance_max <-
    max(
      abs(
        live_state_covariance_array[
          inactive_future_rows,
          c(
            "Delta_Daily_H1",
            "Delta_Daily_D12"
          ),
          ,
          drop = FALSE
        ]
      ),
      abs(
        live_state_covariance_array[
          inactive_future_rows,
          ,
          c(
            "Delta_Daily_H1",
            "Delta_Daily_D12"
          ),
          drop = FALSE
        ]
      )
    )
}

if (
  inactive_daily_state_mean_max >
  1e-12
) {
  stop(
    "Inactive-day daily state means are not exactly zero.",
    call. = FALSE
  )
}

if (
  inactive_daily_state_covariance_max >
  1e-12
) {
  stop(
    "Inactive-day daily state covariance is not exactly zero.",
    call. = FALSE
  )
}

cat(
  "Sunday and Friday future daily-state moments are exactly zero.\n"
)


# ============================================================
# 36. Validate Past Rows Are Inert
# ============================================================

if (
  live_current_t > 0L
) {
  live_past_matrix_rows <-
    seq_len(
      live_current_t
    )
  
  if (
    any(
      live_state_mean_matrix[
        live_past_matrix_rows,
        ,
        drop = FALSE
      ] != 0
    )
  ) {
    stop(
      "Past state mean rows are not inert.",
      call. = FALSE
    )
  }
  
  if (
    any(
      live_state_covariance_array[
        live_past_matrix_rows,
        ,
        ,
        drop = FALSE
      ] != 0
    )
  ) {
    stop(
      "Past state covariance rows are not inert.",
      call. = FALSE
    )
  }
}

cat(
  "Past Stage 5 rows are inert.\n"
)


# ============================================================
# 37. Build Optimizer-Compatible state_mean_competition.csv
# ============================================================

live_state_mean_competition_csv <-
  bind_cols(
    tibble(
      t_original =
        live_competitor_schedule[["t_original"]]
    ),
    as_tibble(
      live_state_mean_matrix
    )
  )

expected_state_mean_columns <-
  c(
    "t_original",
    STATE_NAMES
  )

if (
  !identical(
    names(
      live_state_mean_competition_csv
    ),
    expected_state_mean_columns
  )
) {
  stop(
    "state_mean_competition.csv column order is invalid.",
    call. = FALSE
  )
}

if (
  nrow(
    live_state_mean_competition_csv
  ) != 147L
) {
  stop(
    "state_mean_competition.csv must contain 147 rows.",
    call. = FALSE
  )
}

if (
  !identical(
    as.integer(
      live_state_mean_competition_csv[["t_original"]]
    ),
    0:146
  )
) {
  stop(
    "state_mean_competition.csv t_original is invalid.",
    call. = FALSE
  )
}

cat(
  "Optimizer-compatible state_mean_competition.csv constructed.\n"
)


# ============================================================
# 38. Build Optimizer-Compatible state_cov_competition.csv
# ============================================================

live_state_cov_competition_rows <-
  vector(
    mode = "list",
    length = LIVE_EXPECTED_HORIZON
  )

for (
  time_index in
  seq_len(
    LIVE_EXPECTED_HORIZON
  )
) {
  covariance_matrix <-
    live_state_covariance_array[
      time_index,
      ,
      ,
      drop = TRUE
    ]
  
  live_state_cov_competition_rows[[time_index]] <-
    tidyr::expand_grid(
      state_1 = STATE_NAMES,
      state_2 = STATE_NAMES
    ) |>
    mutate(
      t_original =
        time_index -
        1L,
      covariance =
        as.numeric(
          covariance_matrix[
            cbind(
              match(
                state_1,
                STATE_NAMES
              ),
              match(
                state_2,
                STATE_NAMES
              )
            )
          ]
        )
    ) |>
    select(
      t_original,
      state_1,
      state_2,
      covariance
    )
}

live_state_cov_competition_csv <-
  bind_rows(
    live_state_cov_competition_rows
  )

if (
  nrow(
    live_state_cov_competition_csv
  ) !=
  147L *
  8L *
  8L
) {
  stop(
    "state_cov_competition.csv must contain 9408 rows.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      live_state_cov_competition_csv[["covariance"]]
    )
  )
) {
  stop(
    "state_cov_competition.csv contains non-finite covariance values.",
    call. = FALSE
  )
}

expected_covariance_rows_per_t <-
  live_state_cov_competition_csv |>
  count(
    t_original,
    name = "Rows"
  )

if (
  nrow(
    expected_covariance_rows_per_t
  ) != 147L ||
  any(
    expected_covariance_rows_per_t[["Rows"]] != 64L
  )
) {
  stop(
    "state_cov_competition.csv does not contain 64 entries per t.",
    call. = FALSE
  )
}

cat(
  "Optimizer-compatible state_cov_competition.csv constructed.\n"
)


# ============================================================
# 39. Build Stage 7 Current-State Audit Object
# ============================================================

live_competitor_current_state_audit <-
  tibble(
    current_t =
      live_current_t,
    Day =
      live_current_day,
    TimeCT =
      live_current_time_ct,
    FinalRank1Balance =
      live_current_final_place_balances[["Rank1"]],
    FinalRank2Balance =
      live_current_final_place_balances[["Rank2"]],
    FinalRank3Balance =
      live_current_final_place_balances[["Rank3"]],
    FinalRank4Balance =
      live_current_final_place_balances[["Rank4"]],
    FinalRank15Balance =
      live_current_final_place_balances[["Rank15"]],
    FinalRank16Balance =
      live_current_final_place_balances[["Rank16"]],
    DailyRank1Pnl =
      live_current_daily_place_pnl[["DailyRank1"]],
    DailyRank2Pnl =
      live_current_daily_place_pnl[["DailyRank2"]],
    Delta_Final_X1 =
      live_current_competitor_state[["Delta_Final_X1"]],
    Delta_Final_D12 =
      live_current_competitor_state[["Delta_Final_D12"]],
    Delta_Final_D23 =
      live_current_competitor_state[["Delta_Final_D23"]],
    Delta_Final_D34 =
      live_current_competitor_state[["Delta_Final_D34"]],
    Delta_Final_D4_15 =
      live_current_competitor_state[["Delta_Final_D4_15"]],
    Delta_Final_D15_16 =
      live_current_competitor_state[["Delta_Final_D15_16"]],
    Delta_Daily_H1 =
      live_current_competitor_state[["Delta_Daily_H1"]],
    Delta_Daily_D12 =
      live_current_competitor_state[["Delta_Daily_D12"]]
  )


# ============================================================
# 40. Build Latest Transition Audit Object
# ============================================================

live_competitor_latest_transition_audit <-
  tibble(
    current_t =
      live_current_t,
    latest_observed_t =
      live_latest_observed_t,
    latest_elapsed_minutes =
      live_latest_elapsed_minutes,
    daily_session_start =
      live_latest_daily_session_start,
    r1 =
      live_latest_final_rank_returns[["r1"]],
    r2 =
      live_latest_final_rank_returns[["r2"]],
    r3 =
      live_latest_final_rank_returns[["r3"]],
    r4 =
      live_latest_final_rank_returns[["r4"]],
    r15 =
      live_latest_final_rank_returns[["r15"]],
    r16 =
      live_latest_final_rank_returns[["r16"]],
    h1 =
      live_latest_daily_rank_increments[["h1"]],
    h2 =
      live_latest_daily_rank_increments[["h2"]],
    Rank1Nonzero =
      live_latest_rank1_nonzero,
    Rank2Nonzero =
      live_latest_rank2_nonzero
  )


# ============================================================
# 41. Save Stage 5 CSV Outputs
# ============================================================

readr::write_csv(
  live_state_mean_competition_csv,
  STATE_MEAN_OUTPUT_PATH
)

readr::write_csv(
  live_state_cov_competition_csv,
  STATE_COV_OUTPUT_PATH
)

readr::write_csv(
  live_competitor_current_state_audit,
  CURRENT_STATE_AUDIT_PATH
)

readr::write_csv(
  live_competitor_latest_transition_audit,
  TRANSITION_AUDIT_PATH
)

cat(
  "Saved: ",
  STATE_MEAN_OUTPUT_PATH,
  "\n",
  sep = ""
)

cat(
  "Saved: ",
  STATE_COV_OUTPUT_PATH,
  "\n",
  sep = ""
)


# ============================================================
# 42. Reload and Validate Serialization
# ============================================================

reloaded_state_mean <-
  readr::read_csv(
    STATE_MEAN_OUTPUT_PATH,
    show_col_types = FALSE
  )

reloaded_state_cov <-
  readr::read_csv(
    STATE_COV_OUTPUT_PATH,
    show_col_types = FALSE
  )

if (
  nrow(
    reloaded_state_mean
  ) != 147L
) {
  stop(
    "Reloaded state_mean_competition.csv has invalid row count.",
    call. = FALSE
  )
}

if (
  nrow(
    reloaded_state_cov
  ) != 9408L
) {
  stop(
    "Reloaded state_cov_competition.csv has invalid row count.",
    call. = FALSE
  )
}

if (
  !identical(
    names(
      reloaded_state_mean
    ),
    expected_state_mean_columns
  )
) {
  stop(
    "Reloaded state_mean_competition.csv has invalid columns.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      as.matrix(
        reloaded_state_mean[
          STATE_NAMES
        ]
      )
    )
  )
) {
  stop(
    "Reloaded state_mean_competition.csv contains non-finite values.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      reloaded_state_cov[["covariance"]]
    )
  )
) {
  stop(
    "Reloaded state_cov_competition.csv contains non-finite values.",
    call. = FALSE
  )
}

cat(
  "Stage 5 CSV serialization validated.\n"
)


# ============================================================
# 43. Final Stage 5 Validation
# ============================================================

LIVE_COMPETITOR_FORECASTS_VALIDATED <-
  all(
    c(
      LIVE_COMPETITION_CLOCK_VALIDATED,
      LIVE_EXPECTED_HORIZON == 147L,
      live_current_t >= 0L,
      live_current_t <= 146L,
      nrow(
        current_leaderboard
      ) > 0L,
      nrow(
        previous_leaderboard
      ) > 0L,
      length(
        live_current_final_place_balances
      ) == 6L,
      length(
        live_current_daily_place_pnl
      ) == 2L,
      length(
        live_current_competitor_state
      ) == 8L,
      all(
        is.finite(
          live_current_competitor_state
        )
      ),
      all(
        dim(
          live_final_mean_matrix
        ) ==
          c(
            147L,
            6L
          )
      ),
      all(
        dim(
          live_final_variance_matrix
        ) ==
          c(
            147L,
            6L
          )
      ),
      all(
        dim(
          live_daily_mean_matrix
        ) ==
          c(
            147L,
            2L
          )
      ),
      all(
        dim(
          live_daily_variance_matrix
        ) ==
          c(
            147L,
            2L
          )
      ),
      all(
        dim(
          live_state_mean_matrix
        ) ==
          c(
            147L,
            8L
          )
      ),
      all(
        dim(
          live_state_covariance_array
        ) ==
          c(
            147L,
            8L,
            8L
          )
      ),
      nrow(
        live_state_mean_competition_csv
      ) == 147L,
      nrow(
        live_state_cov_competition_csv
      ) == 9408L,
      maximum_state_mean_transformation_error <
        1e-12,
      maximum_state_covariance_transformation_error <
        1e-10,
      inactive_daily_state_mean_max <
        1e-12,
      inactive_daily_state_covariance_max <
        1e-12,
      file.exists(
        STATE_MEAN_OUTPUT_PATH
      ),
      file.exists(
        STATE_COV_OUTPUT_PATH
      )
    )
  )

cat(
  "\n"
)

cat(
  "============================================================\n"
)

cat(
  "LIVE COMPETITOR FORECAST STAGE SUMMARY\n"
)

cat(
  "============================================================\n"
)

cat(
  "T: ",
  LIVE_EXPECTED_HORIZON,
  "\n",
  sep = ""
)

cat(
  "current_t: ",
  live_current_t,
  "\n",
  sep = ""
)

cat(
  "Remaining intervals: ",
  LIVE_EXPECTED_HORIZON -
    live_current_t,
  "\n",
  sep = ""
)

cat(
  "Current clock: ",
  live_current_day,
  " ",
  live_current_time_ct,
  " CT\n",
  sep = ""
)

cat(
  "Latest observed t: ",
  ifelse(
    is.na(
      live_latest_observed_t
    ),
    "none",
    as.character(
      live_latest_observed_t
    )
  ),
  "\n",
  sep = ""
)

cat(
  "Current duplicated DisplayName values: ",
  nrow(
    current_duplicate_display_names
  ),
  "\n",
  sep = ""
)

cat(
  "Previous duplicated DisplayName values: ",
  nrow(
    previous_duplicate_display_names
  ),
  "\n",
  sep = ""
)

cat(
  "state_mean rows: ",
  nrow(
    live_state_mean_competition_csv
  ),
  "\n",
  sep = ""
)

cat(
  "state_cov rows: ",
  nrow(
    live_state_cov_competition_csv
  ),
  "\n",
  sep = ""
)

cat(
  "Maximum state mean transform error: ",
  maximum_state_mean_transformation_error,
  "\n",
  sep = ""
)

cat(
  "Maximum state covariance transform error: ",
  maximum_state_covariance_transformation_error,
  "\n",
  sep = ""
)

cat(
  "Maximum inactive daily mean magnitude: ",
  inactive_daily_state_mean_max,
  "\n",
  sep = ""
)

cat(
  "Maximum inactive daily covariance magnitude: ",
  inactive_daily_state_covariance_max,
  "\n",
  sep = ""
)

cat(
  "Minimum future covariance eigenvalue: ",
  min(
    live_covariance_validation[["MinimumEigenvalue"]]
  ),
  "\n",
  sep = ""
)

cat(
  "LIVE_COMPETITOR_FORECASTS_VALIDATED = ",
  LIVE_COMPETITOR_FORECASTS_VALIDATED,
  "\n",
  sep = ""
)

cat(
  "============================================================\n"
)

if (
  !LIVE_COMPETITOR_FORECASTS_VALIDATED
) {
  stop(
    "Stage 5 final validation failed.",
    call. = FALSE
  )
}

cat(
  "\n"
)

cat(
  "LIVE COMPETITOR FORECAST STAGE COMPLETE\n"
)

cat(
  "\n"
)
