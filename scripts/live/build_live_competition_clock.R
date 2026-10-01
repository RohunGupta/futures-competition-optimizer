# ============================================================
# BUILD LIVE COMPETITION CLOCK
#
# Purpose:
#   Define the canonical 147-return competition clock and map
#   the live decision timestamp in current_account.csv to the
#   corresponding global optimizer index current_t.
#
# Clock convention:
#   - t is always the GLOBAL competition return index.
#   - t runs from 0 through 146.
#   - current_account.csv Day / Time (CT) identify the START
#     of the next unknown return.
#   - Therefore current_t is the interval whose start matches
#     the current live decision timestamp.
#   - The global 147-period indexing is never shortened or
#     reindexed during the competition.
#
# Examples:
#   Sunday 17:00 -> current_t = 0
#   Sunday 17:30 -> current_t = 1
#   Sunday 18:00 -> current_t = 2
#
# The competition contains:
#   Sunday:       14 returns
#   Monday:       31 returns
#   Tuesday:      31 returns
#   Wednesday:    31 returns
#   Thursday:     31 returns
#   Friday:        9 returns
#   Total:       147 returns
#
# IMPORTANT:
#   The listed market-clock times are RETURN ENDPOINTS.
#   Sunday 17:00 CT is the initial price/state before t = 0.
# ============================================================


# ============================================================
# SETTINGS
# ============================================================

PROJECT_ROOT <- here::here()

LIVE_INPUT_DIR <- file.path(
  PROJECT_ROOT,
  "data",
  "live_inputs"
)

CURRENT_ACCOUNT_PATH <- file.path(
  LIVE_INPUT_DIR,
  "current_account.csv"
)

EXPECTED_FUTURES <- c(
  "CL",
  "GC",
  "SR3",
  "ZN",
  "KE",
  "HO",
  "ZW",
  "NQ"
)

EXPECTED_T <- 147L


# ============================================================
# HELPERS
# ============================================================

normalize_day <- function(x) {
  
  x <- trimws(as.character(x))
  
  valid_days <- c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday"
  )
  
  matched <- match(
    tolower(x),
    tolower(valid_days)
  )
  
  if (any(is.na(matched))) {
    
    bad_values <- unique(
      x[is.na(matched)]
    )
    
    stop(
      paste0(
        "Invalid Day value(s) in current_account.csv: ",
        paste(
          bad_values,
          collapse = ", "
        )
      )
    )
  }
  
  valid_days[matched]
}


normalize_time_ct <- function(x) {
  
  x <- trimws(as.character(x))
  
  if (any(is.na(x) | x == "")) {
    stop(
      "Time (CT) contains missing or blank values."
    )
  }
  
  parse_one <- function(value) {
    
    # --------------------------------------------------------
    # Accept canonical 24-hour HH:MM / H:MM.
    # Examples:
    #   0:30
    #   08:30
    #   17:00
    # --------------------------------------------------------
    
    if (
      grepl(
        "^[0-9]{1,2}:[0-9]{2}$",
        value
      )
    ) {
      
      pieces <- strsplit(
        value,
        ":",
        fixed = TRUE
      )[[1]]
      
      hour <- suppressWarnings(
        as.integer(pieces[1])
      )
      
      minute <- suppressWarnings(
        as.integer(pieces[2])
      )
      
      if (
        is.na(hour) ||
        is.na(minute) ||
        hour < 0L ||
        hour > 23L ||
        minute < 0L ||
        minute > 59L
      ) {
        stop(
          paste0(
            "Invalid 24-hour Time (CT) value: ",
            value
          )
        )
      }
      
      return(
        sprintf(
          "%02d:%02d",
          hour,
          minute
        )
      )
    }
    
    
    # --------------------------------------------------------
    # Also accept explicit AM/PM values if entered manually.
    # Examples:
    #   5:00 PM
    #   12:30 AM
    # --------------------------------------------------------
    
    parsed <- suppressWarnings(
      as.POSIXct(
        value,
        format = "%I:%M %p",
        tz = "America/Chicago"
      )
    )
    
    if (!is.na(parsed)) {
      
      return(
        format(
          parsed,
          "%H:%M",
          tz = "America/Chicago"
        )
      )
    }
    
    
    stop(
      paste0(
        "Could not parse Time (CT) value: ",
        value,
        ". Use H:MM, HH:MM, or H:MM AM/PM."
      )
    )
  }
  
  vapply(
    x,
    parse_one,
    character(1)
  )
}


time_to_minutes <- function(x) {
  
  pieces <- strsplit(
    x,
    ":",
    fixed = TRUE
  )[[1]]
  
  hour <- as.integer(
    pieces[1]
  )
  
  minute <- as.integer(
    pieces[2]
  )
  
  hour * 60L + minute
}


minutes_to_time <- function(minutes) {
  
  minutes <- as.integer(minutes)
  
  if (
    minutes < 0L ||
    minutes > 1440L
  ) {
    stop(
      "minutes_to_time() received an invalid minute value."
    )
  }
  
  if (minutes == 1440L) {
    return("24:00")
  }
  
  hour <- minutes %/% 60L
  
  minute <- minutes %% 60L
  
  sprintf(
    "%02d:%02d",
    hour,
    minute
  )
}


make_sequence_minutes <- function(
    start_time,
    end_time,
    by_minutes = 30L
) {
  
  start_minutes <- time_to_minutes(
    start_time
  )
  
  end_minutes <- time_to_minutes(
    end_time
  )
  
  if (end_minutes < start_minutes) {
    stop(
      "End time precedes start time."
    )
  }
  
  seq(
    from = start_minutes,
    to = end_minutes,
    by = by_minutes
  )
}


# ============================================================
# BUILD CANONICAL RETURN ENDPOINTS
#
# Explicit 24-hour interpretation:
#
# Sunday:
#   17:30 through 24:00 every 30 minutes
#
# Monday-Thursday:
#   00:30
#   08:30 through 16:00 every 30 minutes
#   17:30 through 24:00 every 30 minutes
#
# Friday:
#   00:30
#   08:30 through 12:00 every 30 minutes
#
# These are RETURN ENDPOINTS, not decision timestamps.
# ============================================================

sunday_endpoint_minutes <- seq(
  from = 17L * 60L + 30L,
  to = 24L * 60L,
  by = 30L
)

weekday_endpoint_minutes <- c(
  30L,
  seq(
    from = 8L * 60L + 30L,
    to = 16L * 60L,
    by = 30L
  ),
  seq(
    from = 17L * 60L + 30L,
    to = 24L * 60L,
    by = 30L
  )
)

friday_endpoint_minutes <- c(
  30L,
  seq(
    from = 8L * 60L + 30L,
    to = 12L * 60L,
    by = 30L
  )
)


# ============================================================
# VALIDATE DAY COUNTS
# ============================================================

stopifnot(
  length(sunday_endpoint_minutes) == 14L
)

stopifnot(
  length(weekday_endpoint_minutes) == 31L
)

stopifnot(
  length(friday_endpoint_minutes) == 9L
)

stopifnot(
  (
    length(sunday_endpoint_minutes) +
      4L * length(weekday_endpoint_minutes) +
      length(friday_endpoint_minutes)
  ) == EXPECTED_T
)


# ============================================================
# BUILD ABSOLUTE COMPETITION MINUTE CLOCK
#
# Absolute minute 0:
#   Sunday 00:00 CT
#
# This avoids ambiguity around midnight.
#
# Sunday initial price:
#   17:00 = absolute minute 1020
#
# A Monday endpoint at 00:30 therefore becomes:
#   1440 + 30
#
# Tuesday:
#   2 * 1440 + ...
#
# etc.
# ============================================================

DAY_OFFSETS <- c(
  Sunday = 0L,
  Monday = 1L,
  Tuesday = 2L,
  Wednesday = 3L,
  Thursday = 4L,
  Friday = 5L
)

endpoint_day <- c(
  rep(
    "Sunday",
    length(sunday_endpoint_minutes)
  ),
  rep(
    "Monday",
    length(weekday_endpoint_minutes)
  ),
  rep(
    "Tuesday",
    length(weekday_endpoint_minutes)
  ),
  rep(
    "Wednesday",
    length(weekday_endpoint_minutes)
  ),
  rep(
    "Thursday",
    length(weekday_endpoint_minutes)
  ),
  rep(
    "Friday",
    length(friday_endpoint_minutes)
  )
)

endpoint_minute_of_day <- c(
  sunday_endpoint_minutes,
  weekday_endpoint_minutes,
  weekday_endpoint_minutes,
  weekday_endpoint_minutes,
  weekday_endpoint_minutes,
  friday_endpoint_minutes
)

endpoint_absolute_minute <- (
  unname(
    DAY_OFFSETS[endpoint_day]
  ) * 1440L
  +
    endpoint_minute_of_day
)


# ============================================================
# BUILD START TIMES
#
# Start of t = 0:
#   Sunday 17:00 CT
#
# Every later interval starts exactly at the preceding return
# endpoint.
#
# Therefore:
#
#   t = 0:
#       Sunday 17:00 -> Sunday 17:30
#
#   t = 1:
#       Sunday 17:30 -> Sunday 18:00
#
#   ...
#
# Long competition gaps are preserved automatically:
#
#   Monday 00:30 -> Monday 08:30 = 480 minutes
#
#   Monday 16:00 -> Monday 17:30 = 90 minutes
# ============================================================

initial_absolute_minute <- (
  17L * 60L
)

start_absolute_minute <- c(
  initial_absolute_minute,
  head(
    endpoint_absolute_minute,
    -1L
  )
)

interval_minutes <- (
  endpoint_absolute_minute
  -
    start_absolute_minute
)


# ============================================================
# ABSOLUTE MINUTE -> DAY / TIME
# ============================================================

absolute_to_day <- function(x) {
  
  day_number <- x %/% 1440L
  
  day_names <- names(
    DAY_OFFSETS
  )
  
  if (
    any(
      day_number < 0L |
      day_number > 5L
    )
  ) {
    stop(
      "Absolute competition minute maps outside Sunday-Friday."
    )
  }
  
  day_names[
    day_number + 1L
  ]
}


absolute_to_time <- function(x) {
  
  minute_of_day <- x %% 1440L
  
  sprintf(
    "%02d:%02d",
    minute_of_day %/% 60L,
    minute_of_day %% 60L
  )
}


start_day <- absolute_to_day(
  start_absolute_minute
)

start_time_ct <- absolute_to_time(
  start_absolute_minute
)

end_day <- absolute_to_day(
  endpoint_absolute_minute
)

end_time_ct <- absolute_to_time(
  endpoint_absolute_minute
)


# ============================================================
# CANONICAL 147-RETURN SCHEDULE
# ============================================================

competition_clock <- data.frame(
  t = 0:(EXPECTED_T - 1L),
  start_day = start_day,
  start_time_ct = start_time_ct,
  end_day = end_day,
  end_time_ct = end_time_ct,
  interval_minutes = interval_minutes,
  stringsAsFactors = FALSE
)


# ============================================================
# STRUCTURAL CLOCK VALIDATION
# ============================================================

if (
  nrow(competition_clock) != EXPECTED_T
) {
  stop(
    paste0(
      "Competition clock must contain exactly ",
      EXPECTED_T,
      " rows."
    )
  )
}

if (
  !identical(
    competition_clock$t,
    0:(EXPECTED_T - 1L)
  )
) {
  stop(
    "Competition clock t index is not exactly 0:146."
  )
}

expected_interval_lengths <- c(
  30L,
  90L,
  480L
)

if (
  any(
    !competition_clock$interval_minutes %in%
    expected_interval_lengths
  )
) {
  
  bad_intervals <- competition_clock[
    !competition_clock$interval_minutes %in%
      expected_interval_lengths,
    ,
    drop = FALSE
  ]
  
  print(
    bad_intervals
  )
  
  stop(
    paste0(
      "Competition clock contains interval lengths other than ",
      "30, 90, or 480 minutes."
    )
  )
}

if (
  any(
    diff(
      endpoint_absolute_minute
    ) <= 0L
  )
) {
  stop(
    "Competition return endpoints are not strictly increasing."
  )
}


# ============================================================
# VALIDATE EXACT DAY COUNTS AGAINST PYTHON OPTIMIZER CLOCK
#
# These are counts by RETURN ENDPOINT day, matching the
# canonical Python day_labels construction:
#
#   Sunday 14
#   Monday 31
#   Tuesday 31
#   Wednesday 31
#   Thursday 31
#   Friday 9
# ============================================================

expected_day_counts <- c(
  Sunday = 14L,
  Monday = 31L,
  Tuesday = 31L,
  Wednesday = 31L,
  Thursday = 31L,
  Friday = 9L
)

actual_day_counts <- table(
  factor(
    endpoint_day,
    levels = names(
      expected_day_counts
    )
  )
)

if (
  !identical(
    as.integer(
      actual_day_counts
    ),
    as.integer(
      expected_day_counts
    )
  )
) {
  stop(
    "Competition clock day counts do not match the canonical 147-period clock."
  )
}


# ============================================================
# READ CURRENT ACCOUNT
# ============================================================

if (
  !file.exists(
    CURRENT_ACCOUNT_PATH
  )
) {
  stop(
    paste0(
      "Missing current_account.csv: ",
      CURRENT_ACCOUNT_PATH
    )
  )
}

current_account <- read.csv(
  CURRENT_ACCOUNT_PATH,
  stringsAsFactors = FALSE,
  check.names = FALSE
)


# ============================================================
# VALIDATE CURRENT ACCOUNT SCHEMA
# ============================================================

required_columns <- c(
  "DisplayName",
  "Symbol",
  "ContractsHeld",
  "MarginPerContract",
  "Day",
  "Time (CT)"
)

missing_columns <- setdiff(
  required_columns,
  names(
    current_account
  )
)

if (
  length(
    missing_columns
  ) > 0L
) {
  stop(
    paste0(
      "current_account.csv is missing required columns: ",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

if (
  nrow(
    current_account
  ) != length(
    EXPECTED_FUTURES
  )
) {
  stop(
    paste0(
      "current_account.csv must contain exactly ",
      length(
        EXPECTED_FUTURES
      ),
      " rows, one for each production future."
    )
  )
}


# ============================================================
# VALIDATE FUTURE SYMBOLS
# ============================================================

symbols <- trimws(
  as.character(
    current_account$Symbol
  )
)

if (
  anyDuplicated(
    symbols
  ) > 0L
) {
  stop(
    "current_account.csv contains duplicate Symbol values."
  )
}

if (
  !setequal(
    symbols,
    EXPECTED_FUTURES
  )
) {
  stop(
    paste0(
      "current_account.csv Symbol values must be exactly: ",
      paste(
        EXPECTED_FUTURES,
        collapse = ", "
      )
    )
  )
}


# ============================================================
# NORMALIZE LIVE DAY / TIME
# ============================================================

live_days <- normalize_day(
  current_account$Day
)

live_times <- normalize_time_ct(
  current_account[["Time (CT)"]]
)


# ============================================================
# ALL 8 ROWS MUST REPRESENT THE SAME DECISION TIMESTAMP
# ============================================================

unique_live_days <- unique(
  live_days
)

unique_live_times <- unique(
  live_times
)

if (
  length(
    unique_live_days
  ) != 1L
) {
  stop(
    paste0(
      "All 8 rows of current_account.csv must have the same Day. ",
      "Found: ",
      paste(
        unique_live_days,
        collapse = ", "
      )
    )
  )
}

if (
  length(
    unique_live_times
  ) != 1L
) {
  stop(
    paste0(
      "All 8 rows of current_account.csv must have the same Time (CT). ",
      "Found: ",
      paste(
        unique_live_times,
        collapse = ", "
      )
    )
  )
}

current_day <- unique_live_days[1]

current_time_ct <- unique_live_times[1]


# ============================================================
# MATCH LIVE DECISION TIMESTAMP TO GLOBAL current_t
#
# IMPORTANT:
#   Match against interval START time, not return endpoint.
# ============================================================

matching_rows <- which(
  competition_clock$start_day == current_day &
    competition_clock$start_time_ct == current_time_ct
)

if (
  length(
    matching_rows
  ) == 0L
) {
  stop(
    paste0(
      "The live decision timestamp ",
      current_day,
      " ",
      current_time_ct,
      " CT does not match any valid competition interval start."
    )
  )
}

if (
  length(
    matching_rows
  ) != 1L
) {
  stop(
    paste0(
      "The live decision timestamp ",
      current_day,
      " ",
      current_time_ct,
      " CT matched more than one competition interval."
    )
  )
}

current_row <- matching_rows[1]

current_t <- competition_clock$t[
  current_row
]


# ============================================================
# BUILD LIVE CLOCK METADATA
# ============================================================

past_t <- if (
  current_t == 0L
) {
  integer(0)
} else {
  0:(current_t - 1L)
}

active_t <- current_t:(EXPECTED_T - 1L)

n_past <- length(
  past_t
)

n_remaining <- length(
  active_t
)

current_interval <- competition_clock[
  current_row,
  ,
  drop = FALSE
]

live_competition_clock <- list(
  
  T = EXPECTED_T,
  
  current_t = current_t,
  
  current_day = current_day,
  
  current_time_ct = current_time_ct,
  
  current_interval = current_interval,
  
  past_t = past_t,
  
  active_t = active_t,
  
  n_past = n_past,
  
  n_remaining = n_remaining,
  
  competition_clock = competition_clock
)


# ============================================================
# TARGETED INDEXING TESTS
# ============================================================

assert_clock_mapping <- function(
    day,
    time_ct,
    expected_t
) {
  
  matches <- which(
    competition_clock$start_day == day &
      competition_clock$start_time_ct == time_ct
  )
  
  if (
    length(
      matches
    ) != 1L
  ) {
    stop(
      paste0(
        "Clock self-test could not uniquely match ",
        day,
        " ",
        time_ct,
        "."
      )
    )
  }
  
  actual_t <- competition_clock$t[
    matches
  ]
  
  if (
    actual_t != expected_t
  ) {
    stop(
      paste0(
        "Clock self-test failed for ",
        day,
        " ",
        time_ct,
        ": expected t=",
        expected_t,
        ", got t=",
        actual_t,
        "."
      )
    )
  }
  
  invisible(
    TRUE
  )
}


# Sunday opening tests
assert_clock_mapping(
  "Sunday",
  "17:00",
  0L
)

assert_clock_mapping(
  "Sunday",
  "17:30",
  1L
)

assert_clock_mapping(
  "Sunday",
  "18:00",
  2L
)


# Sunday -> Monday midnight transition
assert_clock_mapping(
  "Sunday",
  "23:30",
  13L
)

assert_clock_mapping(
  "Monday",
  "00:00",
  14L
)

assert_clock_mapping(
  "Monday",
  "00:30",
  15L
)


# Monday long-gap transition
assert_clock_mapping(
  "Monday",
  "08:30",
  16L
)


# ============================================================
# FINAL VALIDATIONS
# ============================================================

if (
  n_past + n_remaining != EXPECTED_T
) {
  stop(
    "n_past + n_remaining does not equal 147."
  )
}

if (
  length(
    active_t
  ) < 1L
) {
  stop(
    "No active competition periods remain."
  )
}

if (
  active_t[1] != current_t
) {
  stop(
    "active_t does not begin at current_t."
  )
}

if (
  tail(
    active_t,
    1L
  ) != EXPECTED_T - 1L
) {
  stop(
    "active_t does not end at t=146."
  )
}


# ============================================================
# PRINT LIVE CLOCK SUMMARY
# ============================================================

cat(
  "\n"
)

cat(
  "============================================================\n"
)

cat(
  "LIVE COMPETITION CLOCK\n"
)

cat(
  "============================================================\n"
)

cat(
  "Canonical total returns: ",
  EXPECTED_T,
  "\n",
  sep = ""
)

cat(
  "Current decision timestamp: ",
  current_day,
  " ",
  current_time_ct,
  " CT\n",
  sep = ""
)

cat(
  "Current global t: ",
  current_t,
  "\n",
  sep = ""
)

cat(
  "Current interval: ",
  current_interval$start_day,
  " ",
  current_interval$start_time_ct,
  " -> ",
  current_interval$end_day,
  " ",
  current_interval$end_time_ct,
  " CT\n",
  sep = ""
)

cat(
  "Current interval duration: ",
  current_interval$interval_minutes,
  " minutes\n",
  sep = ""
)

cat(
  "Completed periods: ",
  n_past,
  "\n",
  sep = ""
)

cat(
  "Remaining periods including current_t: ",
  n_remaining,
  "\n",
  sep = ""
)

cat(
  "Active global t range: ",
  current_t,
  ":",
  EXPECTED_T - 1L,
  "\n",
  sep = ""
)

cat(
  "Full CSV horizon remains: 0:146\n"
)

cat(
  "============================================================\n"
)

cat(
  "\n"
)


# ============================================================
# STAGE 2 COMPLETION FLAG
# ============================================================

LIVE_COMPETITION_CLOCK_VALIDATED <- TRUE