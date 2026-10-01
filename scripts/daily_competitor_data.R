# ============================================================
# daily_competitor_data.R
# ============================================================
#
# Purpose:
#   Load, validate, transform, and prepare the daily-leaderboard
#   competitor P&L data used in the trading competition
#   forecasting pipeline.
#
# Expected input:
#   data/processed/competitors/
#   aug_daily_cumulative_and_differenced.csv
#
# Expected raw columns:
#   Daily Pnl
#   1st Cumulative Daily
#   2nd Cumulative Daily
#   1st Pnl Differenced
#   2nd Pnl Differenced
#
# Interpretation:
#
#   H1_t = cumulative daily P&L of daily first place
#   H2_t = cumulative daily P&L of daily second place
#
#   h1_t = interval P&L earned by daily first place
#   h2_t = interval P&L earned by daily second place
#
# Within a daily session:
#
#   h1_t = H1_t - H1_{t-1}
#   h2_t = H2_t - H2_{t-1}
#
# At the FIRST observed interval after the daily reset:
#
#   h1_t = H1_t - 0 = H1_t
#   h2_t = H2_t - 0 = H2_t
#
# Therefore daily-session resets must NOT be inferred merely
# from a calendar-date change or by differencing consecutive
# rows across sessions.
#
# Daily ordered state:
#
#   H1_t
#   D12_t = H1_t - H2_t >= 0
#
# State increments:
#
#   Delta_H1_t  = h1_t
#   Delta_D12_t = h1_t - h2_t
#
# Daily P&L is measured in DOLLARS.
# Do NOT take logs or log returns.
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lubridate)
})


# ============================================================
# 2. Constants
# ============================================================

DEFAULT_DAILY_COMPETITOR_PATH <- here(
  "data",
  "processed",
  "competitors",
  "aug_daily_cumulative_and_differenced.csv"
)


DAILY_RAW_COLUMNS <- c(
  "Daily Pnl",
  "1st Cumulative Daily",
  "2nd Cumulative Daily",
  "1st Pnl Differenced",
  "2nd Pnl Differenced"
)


DAILY_NUMERIC_RAW_COLUMNS <- c(
  "1st Cumulative Daily",
  "2nd Cumulative Daily",
  "1st Pnl Differenced",
  "2nd Pnl Differenced"
)


DAILY_INTERVAL_COLUMNS <- c(
  "DailyRank1Pnl",
  "DailyRank2Pnl"
)


DAILY_CUMULATIVE_COLUMNS <- c(
  "DailyRank1Cumulative",
  "DailyRank2Cumulative"
)


DAILY_STATE_NAMES <- c(
  "Delta_Daily_H1",
  "Delta_Daily_D12"
)


DAILY_RAW_PROCESS_NAMES <- c(
  "DailyRank1Pnl",
  "DailyRank2Pnl"
)


DAILY_STATE_TRANSFORMATION_MATRIX <-
  matrix(
    c(
      1,  0,
      1, -1
    ),
    nrow = 2,
    byrow = TRUE,
    dimnames = list(
      DAILY_STATE_NAMES,
      DAILY_RAW_PROCESS_NAMES
    )
  )


# ============================================================
# 3. Load Raw Daily Competitor Data
# ============================================================

load_daily_competitor_data <- function(
    path = DEFAULT_DAILY_COMPETITOR_PATH
) {
  
  if (!file.exists(path)) {
    stop(
      paste0(
        "Daily competitor data file does not exist:\n",
        path
      ),
      call. = FALSE
    )
  }
  
  data <- readr::read_csv(
    path,
    show_col_types = FALSE
  )
  
  if (nrow(data) == 0) {
    stop(
      "Daily competitor data file contains zero rows.",
      call. = FALSE
    )
  }
  
  if (ncol(data) == 0) {
    stop(
      "Daily competitor data file contains zero columns.",
      call. = FALSE
    )
  }
  
  data
}


# ============================================================
# 4. Check Required Raw Columns
# ============================================================

check_daily_competitor_columns <- function(
    data,
    required_columns = DAILY_RAW_COLUMNS
) {
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Missing required daily competitor columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 5. Check Numeric Columns
# ============================================================

check_daily_competitor_numeric <- function(
    data,
    numeric_columns = DAILY_NUMERIC_RAW_COLUMNS
) {
  
  check_daily_competitor_columns(
    data = data
  )
  
  numeric_check <- vapply(
    data[numeric_columns],
    is.numeric,
    logical(1)
  )
  
  if (!all(numeric_check)) {
    
    bad_columns <- names(
      numeric_check
    )[!numeric_check]
    
    stop(
      paste0(
        "The following daily competitor columns are not numeric: ",
        paste(
          bad_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  numeric_check
}


# ============================================================
# 6. Missing-Value Summary
# ============================================================

summarize_daily_competitor_missing <- function(
    data,
    columns = DAILY_RAW_COLUMNS
) {
  
  check_daily_competitor_columns(
    data = data
  )
  
  missing_counts <- vapply(
    data[columns],
    function(x) {
      sum(is.na(x))
    },
    numeric(1)
  )
  
  tibble(
    Variable = names(missing_counts),
    MissingValues = as.integer(
      missing_counts
    )
  )
}


# ============================================================
# 7. Validate Missing Values
# ============================================================

check_daily_competitor_missing <- function(
    data,
    columns = DAILY_RAW_COLUMNS
) {
  
  missing_summary <-
    summarize_daily_competitor_missing(
      data = data,
      columns = columns
    )
  
  if (any(missing_summary$MissingValues > 0)) {
    
    bad_variables <-
      missing_summary |>
      filter(
        MissingValues > 0
      )
    
    stop(
      paste0(
        "Daily competitor data contain missing values in: ",
        paste(
          bad_variables$Variable,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  missing_summary
}


# ============================================================
# 8. Validate Finite Numeric Values
# ============================================================

check_daily_competitor_finite <- function(
    data,
    numeric_columns = DAILY_NUMERIC_RAW_COLUMNS
) {
  
  check_daily_competitor_numeric(
    data = data,
    numeric_columns = numeric_columns
  )
  
  finite_counts <- vapply(
    data[numeric_columns],
    function(x) {
      sum(!is.finite(x))
    },
    numeric(1)
  )
  
  finite_summary <- tibble(
    Variable = names(finite_counts),
    NonFiniteValues = as.integer(
      finite_counts
    )
  )
  
  if (any(finite_summary$NonFiniteValues > 0)) {
    
    bad_variables <-
      finite_summary |>
      filter(
        NonFiniteValues > 0
      )
    
    stop(
      paste0(
        "Daily competitor data contain non-finite values in: ",
        paste(
          bad_variables$Variable,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  finite_summary
}


# ============================================================
# 9. Standardize Column Names
# ============================================================

standardize_daily_competitor_names <- function(
    data
) {
  
  check_daily_competitor_columns(
    data = data
  )
  
  data |>
    rename(
      Date = `Daily Pnl`,
      DailyRank1Cumulative =
        `1st Cumulative Daily`,
      DailyRank2Cumulative =
        `2nd Cumulative Daily`,
      DailyRank1Pnl =
        `1st Pnl Differenced`,
      DailyRank2Pnl =
        `2nd Pnl Differenced`
    )
}


# ============================================================
# 10. Parse Timestamps
# ============================================================

add_daily_competitor_timestamp <- function(
    data,
    competition_year = 2026L,
    timezone = "America/Chicago"
) {
  
  if (!"Date" %in% names(data)) {
    stop(
      "Data must contain a 'Date' column before timestamps can be parsed.",
      call. = FALSE
    )
  }
  
  timestamp <-
    as.POSIXct(
      paste0(
        competition_year,
        "-",
        stringr::str_replace(
          data$Date,
          "T",
          " "
        )
      ),
      format = "%Y-%m-%d %H-%M-%S",
      tz = timezone
    )
  
  if (any(is.na(timestamp))) {
    stop(
      "At least one daily competitor timestamp could not be parsed.",
      call. = FALSE
    )
  }
  
  data |>
    mutate(
      Timestamp = timestamp
    ) |>
    arrange(
      Timestamp
    )
}


# ============================================================
# 11. Check Duplicate Timestamps
# ============================================================

check_daily_competitor_duplicate_timestamps <- function(
    data
) {
  
  if (!"Timestamp" %in% names(data)) {
    stop(
      "Data must contain 'Timestamp'.",
      call. = FALSE
    )
  }
  
  duplicate_rows <-
    data |>
    count(
      Timestamp,
      name = "Count"
    ) |>
    filter(
      Count > 1
    )
  
  if (nrow(duplicate_rows) > 0) {
    stop(
      paste0(
        "Duplicate daily competitor timestamps detected. ",
        "Number of duplicated timestamps: ",
        nrow(duplicate_rows)
      ),
      call. = FALSE
    )
  }
  
  duplicate_rows
}


# ============================================================
# 12. Detect Daily Session Resets
# ============================================================
#
# A row is treated as the first observed interval after a daily
# reset when BOTH cumulative P&Ls equal their corresponding
# interval P&Ls.
#
# This works because at the reset:
#
#   H_{i,0} = 0
#
# so at the first subsequent observation:
#
#   h_{i,1} = H_{i,1} - 0 = H_{i,1}.
#
# This intentionally does NOT assume that a reset occurs simply
# because the calendar date changed.
# ============================================================

detect_daily_session_resets <- function(
    data,
    tolerance = 1e-8
) {
  
  required_columns <- c(
    "DailyRank1Cumulative",
    "DailyRank2Cumulative",
    "DailyRank1Pnl",
    "DailyRank2Pnl"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot detect daily resets. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  reset_rank1 <-
    abs(
      data$DailyRank1Cumulative -
        data$DailyRank1Pnl
    ) <= tolerance
  
  reset_rank2 <-
    abs(
      data$DailyRank2Cumulative -
        data$DailyRank2Pnl
    ) <= tolerance
  
  data |>
    mutate(
      DailyReset = reset_rank1 & reset_rank2
    )
}


# ============================================================
# 13. Assign Daily Sessions
# ============================================================

assign_daily_sessions <- function(
    data
) {
  
  if (!"DailyReset" %in% names(data)) {
    stop(
      "Data must contain 'DailyReset' before assigning sessions.",
      call. = FALSE
    )
  }
  
  if (nrow(data) > 0 && !isTRUE(data$DailyReset[1])) {
    stop(
      paste0(
        "The first observation is not identified as a daily reset. ",
        "The dataset should begin with the first observed interval ",
        "of a daily competition session."
      ),
      call. = FALSE
    )
  }
  
  session_id <- cumsum(
    as.integer(
      data$DailyReset
    )
  )
  
  if (any(session_id <= 0)) {
    stop(
      "Daily session IDs could not be constructed correctly.",
      call. = FALSE
    )
  }
  
  data |>
    mutate(
      DailySession = session_id
    ) |>
    group_by(
      DailySession
    ) |>
    mutate(
      DailySessionObservation = row_number(),
      DailySessionObservations = n()
    ) |>
    ungroup()
}


# ============================================================
# 14. Add Daily Session Labels
# ============================================================

add_daily_session_labels <- function(
    data
) {
  
  required_columns <- c(
    "Timestamp",
    "DailySession"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot add daily session labels. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  session_labels <-
    data |>
    group_by(
      DailySession
    ) |>
    summarise(
      SessionStartTimestamp =
        first(Timestamp),
      SessionDate =
        as.Date(
          first(Timestamp)
        ),
      CompetitionDay =
        weekdays(
          as.Date(
            first(Timestamp)
          )
        ),
      .groups = "drop"
    )
  
  data |>
    left_join(
      session_labels,
      by = "DailySession"
    )
}


# ============================================================
# 15. Add Elapsed-Time Variables
# ============================================================
#
# For observations after the first row within a daily session:
#
#   ElapsedMinutes =
#     current timestamp - previous observed timestamp
#
# For the first observation after a reset:
#
#   the previous state is known to be the daily reset at 00:00,
#   so elapsed time is measured from midnight to the first
#   observed timestamp.
# ============================================================

add_daily_elapsed_time <- function(
    data
) {
  
  required_columns <- c(
    "Timestamp",
    "DailySession",
    "DailySessionObservation"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot construct elapsed time. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  data |>
    group_by(
      DailySession
    ) |>
    arrange(
      Timestamp,
      .by_group = TRUE
    ) |>
    mutate(
      SessionResetTimestamp =
        lubridate::floor_date(
          first(Timestamp),
          unit = "day"
        ),
      
      PreviousEffectiveTimestamp =
        if_else(
          DailySessionObservation == 1L,
          SessionResetTimestamp,
          lag(Timestamp)
        ),
      
      ElapsedMinutes =
        as.numeric(
          difftime(
            Timestamp,
            PreviousEffectiveTimestamp,
            units = "mins"
          )
        ),
      
      LogElapsedMinutes =
        log1p(
          ElapsedMinutes
        ),
      
      LongInterval =
        as.numeric(
          ElapsedMinutes > 60
        )
    ) |>
    ungroup()
}


# ============================================================
# 16. Add Global Observation Index
# ============================================================

add_daily_competitor_index <- function(
    data
) {
  
  data |>
    arrange(
      Timestamp
    ) |>
    mutate(
      Observation = row_number()
    )
}


# ============================================================
# 17. Validate Historical Daily Ordering
# ============================================================
#
# Daily leaderboard ordering requires:
#
#   H1_t >= H2_t
#
# at every observed timestamp.
# ============================================================

validate_daily_ordering <- function(
    data,
    tolerance = 1e-8,
    stop_on_violation = TRUE
) {
  
  required_columns <- c(
    "DailyRank1Cumulative",
    "DailyRank2Cumulative"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot validate daily ordering. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  violation <-
    data$DailyRank1Cumulative + tolerance <
    data$DailyRank2Cumulative
  
  result <- tibble(
    TotalObservations = nrow(data),
    OrderingViolations = sum(violation),
    OrderingValid = !any(violation)
  )
  
  if (
    stop_on_violation &&
    any(violation)
  ) {
    
    violation_rows <- which(
      violation
    )
    
    stop(
      paste0(
        "Daily leaderboard ordering violation detected at row(s): ",
        paste(
          violation_rows,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  result
}


# ============================================================
# 18. Validate Differenced P&L Against Cumulative P&L
# ============================================================
#
# Within each session:
#
#   expected h_i,t = H_i,t - H_i,t-1
#
# At the first observation after a reset:
#
#   expected h_i,t = H_i,t - 0 = H_i,t
# ============================================================

validate_daily_differences <- function(
    data,
    tolerance = 1e-6,
    stop_on_violation = TRUE
) {
  
  required_columns <- c(
    "DailySession",
    "DailySessionObservation",
    "DailyRank1Cumulative",
    "DailyRank2Cumulative",
    "DailyRank1Pnl",
    "DailyRank2Pnl"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot validate daily differences. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  check_data <-
    data |>
    group_by(
      DailySession
    ) |>
    mutate(
      ExpectedRank1Pnl =
        if_else(
          DailySessionObservation == 1L,
          DailyRank1Cumulative,
          DailyRank1Cumulative -
            lag(DailyRank1Cumulative)
        ),
      
      ExpectedRank2Pnl =
        if_else(
          DailySessionObservation == 1L,
          DailyRank2Cumulative,
          DailyRank2Cumulative -
            lag(DailyRank2Cumulative)
        ),
      
      Rank1DifferenceError =
        DailyRank1Pnl -
        ExpectedRank1Pnl,
      
      Rank2DifferenceError =
        DailyRank2Pnl -
        ExpectedRank2Pnl,
      
      Rank1DifferenceValid =
        abs(
          Rank1DifferenceError
        ) <= tolerance,
      
      Rank2DifferenceValid =
        abs(
          Rank2DifferenceError
        ) <= tolerance
    ) |>
    ungroup()
  
  invalid_rows <-
    check_data |>
    filter(
      !Rank1DifferenceValid |
        !Rank2DifferenceValid
    )
  
  result <- list(
    valid =
      nrow(invalid_rows) == 0,
    
    invalid_rows =
      invalid_rows,
    
    maximum_rank1_error =
      max(
        abs(
          check_data$Rank1DifferenceError
        ),
        na.rm = TRUE
      ),
    
    maximum_rank2_error =
      max(
        abs(
          check_data$Rank2DifferenceError
        ),
        na.rm = TRUE
      )
  )
  
  if (
    stop_on_violation &&
    !result$valid
  ) {
    
    stop(
      paste0(
        "Differenced daily P&L does not reconstruct from ",
        "cumulative daily P&L for ",
        nrow(invalid_rows),
        " row(s)."
      ),
      call. = FALSE
    )
  }
  
  result
}


# ============================================================
# 19. Construct Daily Ordered State
# ============================================================
#
# Raw daily processes:
#
#   q_t =
#     [h1_t,
#      h2_t]'
#
# Daily state increments:
#
#   Delta S_t =
#     [Delta H1_t,
#      Delta D12_t]'
#
# where:
#
#   Delta H1_t  = h1_t
#   Delta D12_t = h1_t - h2_t
#
# and cumulative daily gap:
#
#   D12_t = H1_t - H2_t >= 0
# ============================================================

add_daily_state_variables <- function(
    data
) {
  
  required_columns <- c(
    "DailyRank1Cumulative",
    "DailyRank2Cumulative",
    "DailyRank1Pnl",
    "DailyRank2Pnl"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot construct daily state variables. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  data |>
    mutate(
      DailyGap12 =
        DailyRank1Cumulative -
        DailyRank2Cumulative,
      
      Delta_Daily_H1 =
        DailyRank1Pnl,
      
      Delta_Daily_D12 =
        DailyRank1Pnl -
        DailyRank2Pnl
    )
}


# ============================================================
# 20. Validate Daily State Transformation
# ============================================================

validate_daily_state_transformation <- function(
    data,
    tolerance = 1e-10
) {
  
  required_columns <- c(
    "DailyRank1Pnl",
    "DailyRank2Pnl",
    "Delta_Daily_H1",
    "Delta_Daily_D12"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot validate daily state transformation. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  raw_matrix <-
    as.matrix(
      data[
        c(
          "DailyRank1Pnl",
          "DailyRank2Pnl"
        )
      ]
    )
  
  transformed_matrix <-
    raw_matrix %*%
    t(
      DAILY_STATE_TRANSFORMATION_MATRIX
    )
  
  direct_matrix <-
    as.matrix(
      data[
        c(
          "Delta_Daily_H1",
          "Delta_Daily_D12"
        )
      ]
    )
  
  max_error <-
    max(
      abs(
        transformed_matrix -
          direct_matrix
      )
    )
  
  if (
    !is.finite(max_error) ||
    max_error > tolerance
  ) {
    stop(
      paste0(
        "Daily-state transformation validation failed. ",
        "Maximum absolute error = ",
        max_error
      ),
      call. = FALSE
    )
  }
  
  tibble(
    MaximumAbsoluteError = max_error,
    TransformationValid =
      max_error <= tolerance
  )
}


# ============================================================
# 21. Create Interval P&L Long Format
# ============================================================

create_daily_interval_long <- function(
    data
) {
  
  required_columns <- c(
    "Observation",
    "Timestamp",
    "DailySession",
    "DailySessionObservation",
    "CompetitionDay",
    "ElapsedMinutes",
    "DailyRank1Pnl",
    "DailyRank2Pnl"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot create daily interval long data. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  data |>
    select(
      all_of(
        required_columns
      )
    ) |>
    pivot_longer(
      cols = c(
        DailyRank1Pnl,
        DailyRank2Pnl
      ),
      names_to = "Rank",
      values_to = "Pnl"
    )
}


# ============================================================
# 22. Create Cumulative Daily P&L Long Format
# ============================================================

create_daily_cumulative_long <- function(
    data
) {
  
  required_columns <- c(
    "Observation",
    "Timestamp",
    "DailySession",
    "DailySessionObservation",
    "CompetitionDay",
    "DailyRank1Cumulative",
    "DailyRank2Cumulative"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot create cumulative daily long data. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  data |>
    select(
      all_of(
        required_columns
      )
    ) |>
    pivot_longer(
      cols = c(
        DailyRank1Cumulative,
        DailyRank2Cumulative
      ),
      names_to = "Rank",
      values_to = "CumulativePnl"
    )
}


# ============================================================
# 23. Create Daily State Data
# ============================================================

create_daily_state_data <- function(
    data
) {
  
  required_columns <- c(
    "Observation",
    "Timestamp",
    "DailySession",
    "DailySessionObservation",
    "CompetitionDay",
    "DailyReset",
    "ElapsedMinutes",
    "DailyRank1Cumulative",
    "DailyRank2Cumulative",
    "DailyGap12",
    "Delta_Daily_H1",
    "Delta_Daily_D12"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot create daily state data. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  data |>
    select(
      all_of(
        required_columns
      )
    )
}


# ============================================================
# 24. Daily Interval Summary Statistics
# ============================================================

summarize_daily_interval_data <- function(
    data
) {
  
  required_columns <- c(
    "DailyRank1Pnl",
    "DailyRank2Pnl"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot summarize daily interval data. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  map_dfr(
    required_columns,
    function(variable) {
      
      x <- data[[variable]]
      
      tibble(
        Rank = variable,
        Observations = length(x),
        Mean = mean(x),
        StandardDeviation = sd(x),
        Minimum = min(x),
        FirstQuartile =
          as.numeric(
            quantile(
              x,
              0.25
            )
          ),
        Median = median(x),
        ThirdQuartile =
          as.numeric(
            quantile(
              x,
              0.75
            )
          ),
        Maximum = max(x),
        ZeroCount = sum(x == 0),
        ZeroRate = mean(x == 0)
      )
    }
  )
}


# ============================================================
# 25. Daily Session Summary
# ============================================================

summarize_daily_sessions <- function(
    data
) {
  
  required_columns <- c(
    "DailySession",
    "SessionDate",
    "CompetitionDay",
    "Timestamp",
    "DailyRank1Cumulative",
    "DailyRank2Cumulative"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Cannot summarize daily sessions. Missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  data |>
    group_by(
      DailySession,
      SessionDate,
      CompetitionDay
    ) |>
    summarise(
      Observations = n(),
      FirstTimestamp = first(Timestamp),
      LastTimestamp = last(Timestamp),
      FinalRank1Cumulative =
        last(DailyRank1Cumulative),
      FinalRank2Cumulative =
        last(DailyRank2Cumulative),
      FinalGap =
        last(
          DailyRank1Cumulative -
            DailyRank2Cumulative
        ),
      .groups = "drop"
    )
}


# ============================================================
# 26. Full Validation
# ============================================================

validate_daily_competitor_data <- function(
    data,
    difference_tolerance = 1e-6,
    ordering_tolerance = 1e-8,
    stop_on_difference_violation = TRUE,
    stop_on_ordering_violation = TRUE
) {
  
  required_prepared_columns <- c(
    "Timestamp",
    "DailyRank1Cumulative",
    "DailyRank2Cumulative",
    "DailyRank1Pnl",
    "DailyRank2Pnl",
    "DailySession",
    "DailySessionObservation",
    "DailyReset",
    "ElapsedMinutes",
    "Delta_Daily_H1",
    "Delta_Daily_D12"
  )
  
  missing_prepared_columns <- setdiff(
    required_prepared_columns,
    names(data)
  )
  
  if (length(missing_prepared_columns) > 0) {
    stop(
      paste0(
        "Prepared daily competitor data are missing columns: ",
        paste(
          missing_prepared_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  numeric_columns <- c(
    "DailyRank1Cumulative",
    "DailyRank2Cumulative",
    "DailyRank1Pnl",
    "DailyRank2Pnl",
    "ElapsedMinutes",
    "Delta_Daily_H1",
    "Delta_Daily_D12"
  )
  
  numeric_check <- vapply(
    data[numeric_columns],
    is.numeric,
    logical(1)
  )
  
  if (!all(numeric_check)) {
    stop(
      paste0(
        "Prepared daily competitor numeric columns are invalid: ",
        paste(
          names(numeric_check)[!numeric_check],
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  missing_summary <- tibble(
    Variable = names(data),
    MissingValues = vapply(
      data,
      function(x) sum(is.na(x)),
      numeric(1)
    )
  )
  
  finite_summary <- tibble(
    Variable = numeric_columns,
    NonFiniteValues = vapply(
      data[numeric_columns],
      function(x) sum(!is.finite(x)),
      numeric(1)
    )
  )
  
  duplicate_timestamps <-
    check_daily_competitor_duplicate_timestamps(
      data = data
    )
  
  ordering_validation <-
    validate_daily_ordering(
      data = data,
      tolerance = ordering_tolerance,
      stop_on_violation =
        stop_on_ordering_violation
    )
  
  difference_validation <-
    validate_daily_differences(
      data = data,
      tolerance = difference_tolerance,
      stop_on_violation =
        stop_on_difference_violation
    )
  
  state_validation <-
    validate_daily_state_transformation(
      data = data
    )
  
  list(
    numeric_check =
      numeric_check,
    
    missing_summary =
      missing_summary,
    
    finite_summary =
      finite_summary,
    
    duplicate_timestamps =
      duplicate_timestamps,
    
    ordering_validation =
      ordering_validation,
    
    difference_validation =
      difference_validation,
    
    state_validation =
      state_validation
  )
}


# ============================================================
# 27. Prepare Daily Competitor Dataset
# ============================================================
#
# This is the main function that later notebooks should call.
#
# Example:
#
#   daily_competitor <- prepare_daily_competitor_data()
#
# Main prepared dataset:
#
#   daily_competitor$data
#
# ============================================================

prepare_daily_competitor_data <- function(
    path = DEFAULT_DAILY_COMPETITOR_PATH,
    competition_year = 2026L,
    timezone = "America/Chicago",
    reset_tolerance = 1e-8,
    difference_tolerance = 1e-6,
    ordering_tolerance = 1e-8,
    stop_on_difference_violation = TRUE,
    stop_on_ordering_violation = TRUE
) {
  
  data <-
    load_daily_competitor_data(
      path = path
    )
  
  check_daily_competitor_columns(
    data = data
  )
  
  check_daily_competitor_numeric(
    data = data
  )
  
  check_daily_competitor_missing(
    data = data
  )
  
  check_daily_competitor_finite(
    data = data
  )
  
  data <-
    standardize_daily_competitor_names(
      data = data
    )
  
  data <-
    add_daily_competitor_timestamp(
      data = data,
      competition_year =
        competition_year,
      timezone = timezone
    )
  
  check_daily_competitor_duplicate_timestamps(
    data = data
  )
  
  data <-
    detect_daily_session_resets(
      data = data,
      tolerance = reset_tolerance
    )
  
  data <-
    assign_daily_sessions(
      data = data
    )
  
  data <-
    add_daily_session_labels(
      data = data
    )
  
  data <-
    add_daily_elapsed_time(
      data = data
    )
  
  data <-
    add_daily_competitor_index(
      data = data
    )
  
  data <-
    add_daily_state_variables(
      data = data
    )
  
  validation <-
    validate_daily_competitor_data(
      data = data,
      difference_tolerance =
        difference_tolerance,
      ordering_tolerance =
        ordering_tolerance,
      stop_on_difference_violation =
        stop_on_difference_violation,
      stop_on_ordering_violation =
        stop_on_ordering_violation
    )
  
  interval_long <-
    create_daily_interval_long(
      data = data
    )
  
  cumulative_long <-
    create_daily_cumulative_long(
      data = data
    )
  
  state_data <-
    create_daily_state_data(
      data = data
    )
  
  summary_statistics <-
    summarize_daily_interval_data(
      data = data
    )
  
  session_summary <-
    summarize_daily_sessions(
      data = data
    )
  
  list(
    data = data,
    interval_long = interval_long,
    cumulative_long = cumulative_long,
    state_data = state_data,
    summary_statistics =
      summary_statistics,
    session_summary =
      session_summary,
    validation = validation
  )
}


# ============================================================
# 28. Convenience Transformation Functions
# ============================================================

transform_daily_pnl_to_state <- function(
    daily_rank1_pnl,
    daily_rank2_pnl
) {
  
  if (
    length(daily_rank1_pnl) !=
    length(daily_rank2_pnl)
  ) {
    stop(
      "Daily Rank1 and Rank2 P&L vectors must have equal length.",
      call. = FALSE
    )
  }
  
  raw_matrix <-
    cbind(
      DailyRank1Pnl =
        daily_rank1_pnl,
      DailyRank2Pnl =
        daily_rank2_pnl
    )
  
  transformed <-
    raw_matrix %*%
    t(
      DAILY_STATE_TRANSFORMATION_MATRIX
    )
  
  as.data.frame(
    transformed
  )
}


transform_daily_mean_to_state <- function(
    mean_vector
) {
  
  if (length(mean_vector) != 2L) {
    stop(
      "Daily raw-process mean vector must have length 2.",
      call. = FALSE
    )
  }
  
  result <-
    as.numeric(
      DAILY_STATE_TRANSFORMATION_MATRIX %*%
        as.numeric(mean_vector)
    )
  
  names(result) <-
    DAILY_STATE_NAMES
  
  result
}


transform_daily_covariance_to_state <- function(
    covariance_matrix
) {
  
  covariance_matrix <-
    as.matrix(
      covariance_matrix
    )
  
  if (
    !all(
      dim(covariance_matrix) ==
      c(2L, 2L)
    )
  ) {
    stop(
      "Daily raw-process covariance matrix must be 2 x 2.",
      call. = FALSE
    )
  }
  
  result <-
    DAILY_STATE_TRANSFORMATION_MATRIX %*%
    covariance_matrix %*%
    t(
      DAILY_STATE_TRANSFORMATION_MATRIX
    )
  
  dimnames(result) <- list(
    DAILY_STATE_NAMES,
    DAILY_STATE_NAMES
  )
  
  result
}


# ============================================================
# 29. Completion Message
# ============================================================

message(
  paste0(
    "daily_competitor_data.R loaded successfully. ",
    "Use prepare_daily_competitor_data() ",
    "to prepare the daily leaderboard dataset."
  )
)


# ============================================================
# End of daily_competitor_data.R
# ============================================================
