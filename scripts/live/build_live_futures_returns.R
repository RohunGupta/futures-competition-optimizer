# ============================================================
# scripts/live/build_live_futures_returns.R
# ============================================================
#
# Purpose:
#
#   Build the live futures-return history used by the rolling
#   production forecasting pipeline.
#
# Inputs:
#
#   data/live_inputs/futures_history.csv
#   data/live_inputs/futures_roll_history.csv
#
# Stage 2 dependency:
#
#   scripts/live/build_live_competition_clock.R
#
# Output object:
#
#   live_futures_returns
#
# Modeling object:
#
#   live_futures_model_history
#
# Audit output:
#
#   data/processed/live/live_futures_returns.csv
#
# Frozen return-construction rules:
#
#   1. Use Bloomberg Close price levels.
#   2. Use latest-prior matching only.
#   3. Never use a price after the competition timestamp.
#   4. Maximum accepted price staleness = 120 minutes.
#   5. Both current and previous endpoint prices must be usable.
#   6. Both endpoint prices must be positive.
#   7. Both endpoint underlying-contract identities must exist.
#   8. Exclude roll-affected returns.
#   9. Roll risk includes:
#        - actual roll date;
#        - preceding calendar date;
#        - observed endpoint contract transition.
#  10. Log return:
#        log(P_t / P_previous)
#  11. Arithmetic return:
#        P_t / P_previous - 1
#  12. Valid competition return lengths:
#        30, 90, 480 minutes.
#  13. Never create a Friday-to-Sunday weekend return.
#
# Historical/live schedule policy:
#
#   The frozen historical models were estimated from the
#   historical competition schedule that existed in Notebook 35.
#   That historical schedule included Friday through 14:00 CT.
#
#   We therefore preserve that historical schedule through the
#   end of the original historical sample.
#
#   Beginning with the live competition week, we use the
#   corrected 147-return competition schedule:
#
#     Sunday:       14 returns
#     Monday:       31 returns
#     Tuesday:      31 returns
#     Wednesday:    31 returns
#     Thursday:     31 returns
#     Friday:        9 returns
#
#   The live competition begins:
#
#     Sunday 2026-09-13 17:00 CT
#
#   and the final live endpoint is:
#
#     Friday 2026-09-18 12:00 CT
#
# IMPORTANT:
#
#   This script does NOT fit or select any forecasting model.
#   It only constructs the return history that later stages use.
#
# ============================================================


# ============================================================
# 1. Required packages
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
  library(lubridate)
})


# ============================================================
# 2. Canonical constants
# ============================================================

COMPETITION_TIME_ZONE <- "America/Chicago"

BLOOMBERG_TIME_ZONE <- "America/New_York"

LIVE_FUTURES_SYMBOLS <- c(
  "CL",
  "GC",
  "SR3",
  "ZN",
  "KE",
  "HO",
  "ZW",
  "NQ"
)

# ============================================================
# Stage 6 futures-data source mode
# ============================================================
#
# "BLOOMBERG"
#   Preserve the frozen production behavior exactly.
#
# "YAHOO"
#   Use the Stage 4 Yahoo shadow canonical history and
#   timestamp-level Yahoo contract identity.
#
# This may be set by run_live_pipeline.R before this script is
# sourced. Standalone execution defaults to BLOOMBERG.
#
# ============================================================

if (
  !exists(
    "LIVE_FUTURES_DATA_SOURCE",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  LIVE_FUTURES_DATA_SOURCE <- "BLOOMBERG"
}

LIVE_FUTURES_DATA_SOURCE <- toupper(
  trimws(
    as.character(
      LIVE_FUTURES_DATA_SOURCE
    )
  )
)

if (
  length(LIVE_FUTURES_DATA_SOURCE) != 1L ||
  !LIVE_FUTURES_DATA_SOURCE %in%
  c(
    "BLOOMBERG",
    "YAHOO"
  )
) {
  stop(
    paste0(
      "LIVE_FUTURES_DATA_SOURCE must be exactly ",
      "\"BLOOMBERG\" or \"YAHOO\"."
    ),
    call. = FALSE
  )
}

FINAL_MAXIMUM_STALENESS_MINUTES <- 120

VALID_RETURN_INTERVAL_MINUTES <- c(
  30,
  90,
  480
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

if (
  !exists(
    "LIVE_COMPETITION_FRIDAY",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  LIVE_COMPETITION_FRIDAY <- LIVE_COMPETITION_SUNDAY + 5L
}

HISTORICAL_SCHEDULE_END_DATE <- as.Date(
  "2026-09-04"
)


# ============================================================
# 3. Paths
# ============================================================

LIVE_INPUT_DIR <- here(
  "data",
  "live_inputs"
)

FUTURES_HISTORY_PATH <- file.path(
  LIVE_INPUT_DIR,
  "futures_history.csv"
)

FUTURES_ROLL_HISTORY_PATH <- file.path(
  LIVE_INPUT_DIR,
  "futures_roll_history.csv"
)

YAHOO_DATA_DIR <- here(
  "data",
  "yahoo"
)

YAHOO_SHADOW_HISTORY_PATH <- file.path(
  YAHOO_DATA_DIR,
  "yahoo_shadow_futures_history.csv"
)

YAHOO_SHADOW_IDENTITY_PATH <- file.path(
  YAHOO_DATA_DIR,
  "yahoo_shadow_contract_identity.csv"
)

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "BLOOMBERG"
) {
  
  ACTIVE_FUTURES_HISTORY_PATH <-
    FUTURES_HISTORY_PATH
  
} else {
  
  ACTIVE_FUTURES_HISTORY_PATH <-
    YAHOO_SHADOW_HISTORY_PATH
}

LIVE_PROCESSED_DIR <- here(
  "data",
  "processed",
  "live"
)

LIVE_RETURNS_OUTPUT_PATH <- file.path(
  LIVE_PROCESSED_DIR,
  "live_futures_returns.csv"
)

LIVE_MODEL_HISTORY_OUTPUT_PATH <- file.path(
  LIVE_PROCESSED_DIR,
  "live_futures_model_history.csv"
)


# ============================================================
# 4. Validate required files
# ============================================================

if (!file.exists(ACTIVE_FUTURES_HISTORY_PATH)) {
  stop(
    paste0(
      "Missing active futures history:\n",
      ACTIVE_FUTURES_HISTORY_PATH
    ),
    call. = FALSE
  )
}

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "YAHOO" &&
  !file.exists(
    YAHOO_SHADOW_IDENTITY_PATH
  )
) {
  stop(
    paste0(
      "Missing Yahoo timestamp-level identity file:\n",
      YAHOO_SHADOW_IDENTITY_PATH
    ),
    call. = FALSE
  )
}

if (!file.exists(FUTURES_ROLL_HISTORY_PATH)) {
  stop(
    paste0(
      "Missing futures_roll_history.csv:\n",
      FUTURES_ROLL_HISTORY_PATH
    ),
    call. = FALSE
  )
}

if (!dir.exists(LIVE_PROCESSED_DIR)) {
  dir.create(
    LIVE_PROCESSED_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 5. Read canonical futures price history
# ============================================================

futures_history <- readr::read_csv(
  ACTIVE_FUTURES_HISTORY_PATH,
  show_col_types = FALSE
)


# ============================================================
# 6. Validate futures price-history schema
# ============================================================

required_history_columns <- c(
  "Symbol",
  "BloombergSecurity",
  "SourceType",
  "SourceFile",
  "RawDate",
  "RawTimeInterval",
  "EndTimestampET",
  "EndTimestampCT",
  "Close"
)

missing_history_columns <- setdiff(
  required_history_columns,
  names(futures_history)
)

if (length(missing_history_columns) > 0L) {
  stop(
    paste0(
      "futures_history.csv is missing required columns: ",
      paste(
        missing_history_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

futures_history <- futures_history |>
  mutate(
    Symbol = as.character(Symbol),
    BloombergSecurity = as.character(BloombergSecurity),
    SourceType = as.character(SourceType),
    SourceFile = as.character(SourceFile),
    RawDate = as.Date(RawDate),
    RawTimeInterval = as.character(RawTimeInterval),
    EndTimestampET = as.POSIXct(
      EndTimestampET,
      tz = BLOOMBERG_TIME_ZONE
    ),
    EndTimestampCT = as.POSIXct(
      EndTimestampCT,
      tz = COMPETITION_TIME_ZONE
    ),
    Close = as.numeric(Close)
  )


# ============================================================
# 7. Validate canonical futures price history
# ============================================================

missing_price_symbols <- setdiff(
  LIVE_FUTURES_SYMBOLS,
  unique(futures_history$Symbol)
)

if (length(missing_price_symbols) > 0L) {
  stop(
    paste0(
      "futures_history.csv is missing symbols: ",
      paste(
        missing_price_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

extra_price_symbols <- setdiff(
  unique(futures_history$Symbol),
  LIVE_FUTURES_SYMBOLS
)

if (length(extra_price_symbols) > 0L) {
  stop(
    paste0(
      "futures_history.csv contains unsupported symbols: ",
      paste(
        extra_price_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (
  any(is.na(futures_history$EndTimestampCT)) ||
  any(is.na(futures_history$EndTimestampET))
) {
  stop(
    "futures_history.csv contains missing timestamps.",
    call. = FALSE
  )
}

if (
  any(is.na(futures_history$Close)) ||
  any(!is.finite(futures_history$Close)) ||
  any(futures_history$Close <= 0)
) {
  stop(
    "futures_history.csv contains invalid Close values.",
    call. = FALSE
  )
}

price_duplicates <- futures_history |>
  count(
    Symbol,
    EndTimestampCT,
    name = "Occurrences"
  ) |>
  filter(
    Occurrences > 1L
  )

if (nrow(price_duplicates) > 0L) {
  print(price_duplicates)
  
  stop(
    paste0(
      "futures_history.csv contains duplicate Symbol / ",
      "EndTimestampCT observations."
    ),
    call. = FALSE
  )
}

futures_history <- futures_history |>
  mutate(
    Symbol = factor(
      Symbol,
      levels = LIVE_FUTURES_SYMBOLS
    )
  ) |>
  arrange(
    Symbol,
    EndTimestampCT
  ) |>
  mutate(
    Symbol = as.character(Symbol)
  )


# ============================================================
# 8. Read canonical roll history
# ============================================================

futures_roll_history <- readr::read_csv(
  FUTURES_ROLL_HISTORY_PATH,
  show_col_types = FALSE
)


# ============================================================
# 9. Validate roll-history schema
# ============================================================

required_roll_columns <- c(
  "Symbol",
  "Date",
  "UnderlyingContract"
)

missing_roll_columns <- setdiff(
  required_roll_columns,
  names(futures_roll_history)
)

if (length(missing_roll_columns) > 0L) {
  stop(
    paste0(
      "futures_roll_history.csv is missing required columns: ",
      paste(
        missing_roll_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

futures_roll_history <- futures_roll_history |>
  mutate(
    Symbol = as.character(Symbol),
    Date = as.Date(Date),
    UnderlyingContract = trimws(
      as.character(UnderlyingContract)
    )
  )

missing_roll_symbols <- setdiff(
  LIVE_FUTURES_SYMBOLS,
  unique(futures_roll_history$Symbol)
)

if (length(missing_roll_symbols) > 0L) {
  stop(
    paste0(
      "futures_roll_history.csv is missing symbols: ",
      paste(
        missing_roll_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

extra_roll_symbols <- setdiff(
  unique(futures_roll_history$Symbol),
  LIVE_FUTURES_SYMBOLS
)

if (length(extra_roll_symbols) > 0L) {
  stop(
    paste0(
      "futures_roll_history.csv contains unsupported symbols: ",
      paste(
        extra_roll_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (any(is.na(futures_roll_history$Date))) {
  stop(
    "futures_roll_history.csv contains missing Date values.",
    call. = FALSE
  )
}

if (
  any(is.na(futures_roll_history$UnderlyingContract)) ||
  any(futures_roll_history$UnderlyingContract == "")
) {
  stop(
    "futures_roll_history.csv contains invalid contract identities.",
    call. = FALSE
  )
}

roll_duplicates <- futures_roll_history |>
  count(
    Symbol,
    Date,
    name = "Occurrences"
  ) |>
  filter(
    Occurrences > 1L
  )

if (nrow(roll_duplicates) > 0L) {
  print(roll_duplicates)
  
  stop(
    paste0(
      "futures_roll_history.csv contains duplicate ",
      "Symbol / Date observations."
    ),
    call. = FALSE
  )
}

futures_roll_history <- futures_roll_history |>
  mutate(
    Symbol = factor(
      Symbol,
      levels = LIVE_FUTURES_SYMBOLS
    )
  ) |>
  arrange(
    Symbol,
    Date
  ) |>
  mutate(
    Symbol = as.character(Symbol)
  )


# ============================================================
# Stage 6 Yahoo timestamp-level contract identity
# ============================================================

yahoo_contract_identity <- NULL

normalize_yahoo_contract <- function(
    symbol,
    ticker
) {
  
  ticker <- toupper(
    trimws(
      as.character(
        ticker
      )
    )
  )
  
  if (
    is.na(ticker) ||
    ticker == ""
  ) {
    return(
      NA_character_
    )
  }
  
  ticker_without_exchange <- sub(
    "\\..*$",
    "",
    ticker
  )
  
  pattern <- switch(
    symbol,
    "CL" = "^CL([FGHJKMNQUVXZ])([0-9]{2})$",
    "GC" = "^GC([FGHJKMNQUVXZ])([0-9]{2})$",
    "SR3" = "^SR3([FGHJKMNQUVXZ])([0-9]{2})$",
    "ZN" = "^ZN([FGHJKMNQUVXZ])([0-9]{2})$",
    "KE" = "^KE([FGHJKMNQUVXZ])([0-9]{2})$",
    "HO" = "^HO([FGHJKMNQUVXZ])([0-9]{2})$",
    "ZW" = "^ZW([FGHJKMNQUVXZ])([0-9]{2})$",
    "NQ" = "^NQ([FGHJKMNQUVXZ])([0-9]{2})$",
    stop(
      paste0(
        "Unsupported Yahoo identity symbol: ",
        symbol
      ),
      call. = FALSE
    )
  )
  
  matched <- regexec(
    pattern,
    ticker_without_exchange
  )
  
  pieces <- regmatches(
    ticker_without_exchange,
    matched
  )[[1]]
  
  if (length(pieces) != 3L) {
    stop(
      paste0(
        "Unexpected Yahoo dated ticker for ",
        symbol,
        ": ",
        ticker
      ),
      call. = FALSE
    )
  }
  
  month_code <- pieces[[2]]
  
  year_two_digit <- pieces[[3]]
  
  year_one_digit <- substring(
    year_two_digit,
    2L,
    2L
  )
  
  output_root <- switch(
    symbol,
    "SR3" = "SFR",
    "ZN" = "TY",
    "ZW" = "W",
    symbol
  )
  
  paste0(
    output_root,
    month_code,
    year_one_digit
  )
}

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "YAHOO"
) {
  
  yahoo_contract_identity <-
    readr::read_csv(
      YAHOO_SHADOW_IDENTITY_PATH,
      show_col_types = FALSE
    )
  
  required_yahoo_identity_columns <- c(
    "Symbol",
    "EndTimestampCT",
    "YahooDatedTicker"
  )
  
  missing_yahoo_identity_columns <-
    setdiff(
      required_yahoo_identity_columns,
      names(
        yahoo_contract_identity
      )
    )
  
  if (
    length(
      missing_yahoo_identity_columns
    ) > 0L
  ) {
    stop(
      paste0(
        "Yahoo timestamp identity is missing columns: ",
        paste(
          missing_yahoo_identity_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  yahoo_contract_identity <-
    yahoo_contract_identity |>
    mutate(
      Symbol = as.character(
        Symbol
      ),
      EndTimestampCT = as.POSIXct(
        EndTimestampCT,
        tz = COMPETITION_TIME_ZONE
      )
    ) |>
    rowwise() |>
    mutate(
      NormalizedUnderlyingContract =
        normalize_yahoo_contract(
          Symbol,
          YahooDatedTicker
        )
    ) |>
    ungroup() |>
    select(
      Symbol,
      EndTimestampCT,
      YahooDatedTicker,
      NormalizedUnderlyingContract
    ) |>
    arrange(
      Symbol,
      EndTimestampCT
    )
  
  yahoo_identity_duplicates <-
    yahoo_contract_identity |>
    count(
      Symbol,
      EndTimestampCT,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    )
  
  if (
    nrow(
      yahoo_identity_duplicates
    ) > 0L
  ) {
    print(
      yahoo_identity_duplicates
    )
    
    stop(
      paste0(
        "Yahoo timestamp identity contains duplicate ",
        "Symbol / EndTimestampCT observations."
      ),
      call. = FALSE
    )
  }
}


# ============================================================
# 10. Detect actual contract changes
# ============================================================

roll_changes <- futures_roll_history |>
  arrange(
    Symbol,
    Date
  ) |>
  group_by(
    Symbol
  ) |>
  mutate(
    PreviousUnderlyingContract = lag(
      UnderlyingContract
    ),
    ContractChanged = !is.na(
      PreviousUnderlyingContract
    ) &
      UnderlyingContract != PreviousUnderlyingContract
  ) |>
  ungroup() |>
  filter(
    ContractChanged
  ) |>
  select(
    Symbol,
    Date,
    PreviousUnderlyingContract,
    UnderlyingContract
  )


# ============================================================
# 11. Schedule helpers
# ============================================================

format_minute_offset <- function(minute_offset) {
  
  minute_offset <- as.integer(minute_offset)
  
  if (minute_offset == 1440L) {
    return("24:00")
  }
  
  hour_value <- minute_offset %/% 60L
  
  minute_value <- minute_offset %% 60L
  
  sprintf(
    "%02d:%02d",
    hour_value,
    minute_value
  )
}


make_minute_sequence <- function(
    start_hour,
    start_minute,
    end_hour,
    end_minute,
    step_minutes = 30L
) {
  
  start_value <- as.integer(
    start_hour * 60L +
      start_minute
  )
  
  end_value <- as.integer(
    end_hour * 60L +
      end_minute
  )
  
  seq(
    from = start_value,
    to = end_value,
    by = step_minutes
  )
}


build_competition_timestamp <- function(
    schedule_date,
    minute_offset
) {
  
  schedule_date <- as.Date(schedule_date)
  
  minute_offset <- as.integer(minute_offset)
  
  if (minute_offset == 1440L) {
    
    timestamp_text <- paste(
      schedule_date + 1L,
      "00:00"
    )
    
  } else {
    
    timestamp_text <- paste(
      schedule_date,
      format_minute_offset(
        minute_offset
      )
    )
  }
  
  lubridate::ymd_hm(
    timestamp_text,
    tz = COMPETITION_TIME_ZONE,
    quiet = TRUE
  )
}


# ============================================================
# 12. Frozen historical schedule offsets
# ============================================================
#
# These reproduce the historical Notebook 35 schedule used by
# the model-estimation pipeline.
#
# We preserve this schedule for historical dates through
# 2026-09-04 so that the training-return construction is not
# silently changed.
#
# ============================================================

HISTORICAL_SUNDAY_OFFSETS <- make_minute_sequence(
  start_hour = 17L,
  start_minute = 0L,
  end_hour = 24L,
  end_minute = 0L
)

HISTORICAL_MONDAY_THURSDAY_OFFSETS <- c(
  30L,
  make_minute_sequence(
    start_hour = 8L,
    start_minute = 30L,
    end_hour = 16L,
    end_minute = 0L
  ),
  make_minute_sequence(
    start_hour = 17L,
    start_minute = 30L,
    end_hour = 24L,
    end_minute = 0L
  )
)

HISTORICAL_FRIDAY_OFFSETS <- c(
  30L,
  make_minute_sequence(
    start_hour = 8L,
    start_minute = 30L,
    end_hour = 14L,
    end_minute = 0L
  )
)

stopifnot(
  length(HISTORICAL_SUNDAY_OFFSETS) == 15L
)

stopifnot(
  length(HISTORICAL_MONDAY_THURSDAY_OFFSETS) == 31L
)

stopifnot(
  length(HISTORICAL_FRIDAY_OFFSETS) == 13L
)

stopifnot(
  length(HISTORICAL_SUNDAY_OFFSETS) +
    4L * length(HISTORICAL_MONDAY_THURSDAY_OFFSETS) +
    length(HISTORICAL_FRIDAY_OFFSETS) ==
    152L
)


# ============================================================
# 13. Corrected live competition schedule offsets
# ============================================================
#
# Live competition:
#
# Sunday price observations:
#   17:00 through 24:00
#
# Monday-Thursday price endpoints:
#   00:30
#   08:30 through 16:00
#   17:30 through 24:00
#
# Friday price endpoints:
#   00:30
#   08:30 through 12:00
#
# The Sunday 17:00 price is the initial price, not a return.
#
# Therefore:
#
#   148 live price observations
#   147 live return intervals
#
# ============================================================

LIVE_SUNDAY_OFFSETS <- make_minute_sequence(
  start_hour = 17L,
  start_minute = 0L,
  end_hour = 24L,
  end_minute = 0L
)

LIVE_MONDAY_THURSDAY_OFFSETS <- c(
  30L,
  make_minute_sequence(
    start_hour = 8L,
    start_minute = 30L,
    end_hour = 16L,
    end_minute = 0L
  ),
  make_minute_sequence(
    start_hour = 17L,
    start_minute = 30L,
    end_hour = 24L,
    end_minute = 0L
  )
)

LIVE_FRIDAY_OFFSETS <- c(
  30L,
  make_minute_sequence(
    start_hour = 8L,
    start_minute = 30L,
    end_hour = 12L,
    end_minute = 0L
  )
)

stopifnot(
  length(LIVE_SUNDAY_OFFSETS) == 15L
)

stopifnot(
  length(LIVE_MONDAY_THURSDAY_OFFSETS) == 31L
)

stopifnot(
  length(LIVE_FRIDAY_OFFSETS) == 9L
)

stopifnot(
  length(LIVE_SUNDAY_OFFSETS) +
    4L * length(LIVE_MONDAY_THURSDAY_OFFSETS) +
    length(LIVE_FRIDAY_OFFSETS) ==
    148L
)


# ============================================================
# 14. Return schedule offsets for one date
# ============================================================

get_schedule_offsets_for_date <- function(schedule_date) {
  
  schedule_date <- as.Date(schedule_date)
  
  day_number <- lubridate::wday(
    schedule_date,
    week_start = 1L
  )
  
  if (schedule_date <= HISTORICAL_SCHEDULE_END_DATE) {
    
    if (day_number == 7L) {
      return(HISTORICAL_SUNDAY_OFFSETS)
    }
    
    if (day_number %in% 1L:4L) {
      return(HISTORICAL_MONDAY_THURSDAY_OFFSETS)
    }
    
    if (day_number == 5L) {
      return(HISTORICAL_FRIDAY_OFFSETS)
    }
    
    return(integer(0))
  }
  
  if (
    schedule_date >= LIVE_COMPETITION_SUNDAY &&
    schedule_date <= LIVE_COMPETITION_FRIDAY
  ) {
    
    if (day_number == 7L) {
      return(LIVE_SUNDAY_OFFSETS)
    }
    
    if (day_number %in% 1L:4L) {
      return(LIVE_MONDAY_THURSDAY_OFFSETS)
    }
    
    if (day_number == 5L) {
      return(LIVE_FRIDAY_OFFSETS)
    }
    
    return(integer(0))
  }
  
  integer(0)
}


# ============================================================
# 15. Build historical plus live price schedule
# ============================================================

raw_first_end_ct <- min(
  futures_history$EndTimestampCT,
  na.rm = TRUE
)

raw_last_end_ct <- max(
  futures_history$EndTimestampCT,
  na.rm = TRUE
)

schedule_first_date <- as.Date(
  raw_first_end_ct,
  tz = COMPETITION_TIME_ZONE
)

schedule_last_available_date <- as.Date(
  raw_last_end_ct,
  tz = COMPETITION_TIME_ZONE
)

schedule_last_date <- min(
  schedule_last_available_date,
  LIVE_COMPETITION_FRIDAY
)

if (schedule_last_date < schedule_first_date) {
  stop(
    "The futures price history does not overlap the competition schedule.",
    call. = FALSE
  )
}

schedule_dates <- seq.Date(
  from = schedule_first_date,
  to = schedule_last_date,
  by = "day"
)

schedule_list <- lapply(
  schedule_dates,
  function(current_date) {
    
    offsets <- get_schedule_offsets_for_date(
      current_date
    )
    
    if (length(offsets) == 0L) {
      return(NULL)
    }
    
    timestamps <- vapply(
      offsets,
      function(current_offset) {
        as.numeric(
          build_competition_timestamp(
            schedule_date = current_date,
            minute_offset = current_offset
          )
        )
      },
      numeric(1)
    )
    
    tibble(
      ScheduleDate = rep(
        current_date,
        length(offsets)
      ),
      ScheduleDay = rep(
        weekdays(current_date),
        length(offsets)
      ),
      CompetitionMinuteOffset = offsets,
      CompetitionClockCT = vapply(
        offsets,
        format_minute_offset,
        character(1)
      ),
      CompetitionTimestampCT = as.POSIXct(
        timestamps,
        origin = "1970-01-01",
        tz = COMPETITION_TIME_ZONE
      )
    )
  }
)

competition_schedule <- bind_rows(
  schedule_list
) |>
  arrange(
    CompetitionTimestampCT
  ) |>
  mutate(
    CompetitionObservation = row_number()
  ) |>
  select(
    CompetitionObservation,
    everything()
  )


# ============================================================
# 16. Identify Sunday week starts / return endpoints
# ============================================================

competition_schedule <- competition_schedule |>
  mutate(
    IsSundayWeekStart =
      ScheduleDay == "Sunday" &
      CompetitionClockCT == "17:00",
    IsReturnEndpoint =
      !IsSundayWeekStart
  )


# ============================================================
# 17. Validate live 147-return schedule
# ============================================================

live_price_schedule <- competition_schedule |>
  filter(
    ScheduleDate >= LIVE_COMPETITION_SUNDAY,
    ScheduleDate <= LIVE_COMPETITION_FRIDAY
  )

live_return_schedule <- live_price_schedule |>
  filter(
    IsReturnEndpoint
  )

if (
  schedule_last_date >= LIVE_COMPETITION_FRIDAY &&
  nrow(live_price_schedule) != 148L
) {
  stop(
    paste0(
      "Complete live competition price schedule must contain ",
      "exactly 148 price observations."
    ),
    call. = FALSE
  )
}

if (
  schedule_last_date >= LIVE_COMPETITION_FRIDAY &&
  nrow(live_return_schedule) != 147L
) {
  stop(
    paste0(
      "Complete live competition return schedule must contain ",
      "exactly 147 return endpoints."
    ),
    call. = FALSE
  )
}


# ============================================================
# 18. Build full symbol-by-price target grid
# ============================================================

competition_target_grid <- tidyr::crossing(
  Symbol = LIVE_FUTURES_SYMBOLS,
  competition_schedule |>
    select(
      CompetitionObservation,
      ScheduleDate,
      ScheduleDay,
      CompetitionMinuteOffset,
      CompetitionClockCT,
      CompetitionTimestampCT,
      IsSundayWeekStart,
      IsReturnEndpoint
    )
) |>
  arrange(
    Symbol,
    CompetitionTimestampCT
  )


# ============================================================
# 19. Prepare canonical Close data
# ============================================================

futures_close_data <- futures_history |>
  select(
    Symbol,
    BloombergSecurity,
    SourceType,
    SourceFile,
    RawDate,
    RawTimeInterval,
    EndTimestampET,
    EndTimestampCT,
    Close
  ) |>
  arrange(
    Symbol,
    EndTimestampCT
  )


# ============================================================
# 20. Latest-prior matching helper
# ============================================================

match_latest_prior_for_symbol <- function(
    target_data,
    close_data,
    symbol
) {
  
  symbol_targets <- target_data |>
    filter(
      Symbol == symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  symbol_closes <- close_data |>
    filter(
      Symbol == symbol
    ) |>
    arrange(
      EndTimestampCT
    )
  
  if (nrow(symbol_closes) == 0L) {
    stop(
      paste0(
        "No futures Close observations available for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  target_numeric <- as.numeric(
    symbol_targets$CompetitionTimestampCT
  )
  
  close_numeric <- as.numeric(
    symbol_closes$EndTimestampCT
  )
  
  matched_index <- findInterval(
    target_numeric,
    close_numeric
  )
  
  valid_match <- matched_index > 0L
  
  matched_close <- rep(
    NA_real_,
    nrow(symbol_targets)
  )
  
  matched_timestamp_ct_numeric <- rep(
    NA_real_,
    nrow(symbol_targets)
  )
  
  matched_timestamp_et_numeric <- rep(
    NA_real_,
    nrow(symbol_targets)
  )
  
  matched_raw_date <- rep(
    as.Date(NA),
    nrow(symbol_targets)
  )
  
  matched_raw_interval <- rep(
    NA_character_,
    nrow(symbol_targets)
  )
  
  matched_source_type <- rep(
    NA_character_,
    nrow(symbol_targets)
  )
  
  matched_source_file <- rep(
    NA_character_,
    nrow(symbol_targets)
  )
  
  matched_bloomberg_security <- rep(
    NA_character_,
    nrow(symbol_targets)
  )
  
  if (any(valid_match)) {
    
    valid_indices <- matched_index[valid_match]
    
    matched_close[valid_match] <-
      symbol_closes$Close[valid_indices]
    
    matched_timestamp_ct_numeric[valid_match] <-
      as.numeric(
        symbol_closes$EndTimestampCT[valid_indices]
      )
    
    matched_timestamp_et_numeric[valid_match] <-
      as.numeric(
        symbol_closes$EndTimestampET[valid_indices]
      )
    
    matched_raw_date[valid_match] <-
      symbol_closes$RawDate[valid_indices]
    
    matched_raw_interval[valid_match] <-
      symbol_closes$RawTimeInterval[valid_indices]
    
    matched_source_type[valid_match] <-
      symbol_closes$SourceType[valid_indices]
    
    matched_source_file[valid_match] <-
      symbol_closes$SourceFile[valid_indices]
    
    matched_bloomberg_security[valid_match] <-
      symbol_closes$BloombergSecurity[valid_indices]
  }
  
  matched_timestamp_ct <- as.POSIXct(
    matched_timestamp_ct_numeric,
    origin = "1970-01-01",
    tz = COMPETITION_TIME_ZONE
  )
  
  matched_timestamp_et <- as.POSIXct(
    matched_timestamp_et_numeric,
    origin = "1970-01-01",
    tz = BLOOMBERG_TIME_ZONE
  )
  
  symbol_targets |>
    mutate(
      BloombergSecurity = matched_bloomberg_security,
      SourceType = matched_source_type,
      SourceFile = matched_source_file,
      MatchedRawDate = matched_raw_date,
      MatchedRawTimeInterval = matched_raw_interval,
      MatchedEndTimestampET = matched_timestamp_et,
      MatchedEndTimestampCT = matched_timestamp_ct,
      MatchedClose = matched_close,
      StalenessMinutes = as.numeric(
        difftime(
          CompetitionTimestampCT,
          MatchedEndTimestampCT,
          units = "mins"
        )
      ),
      ExactMatch =
        !is.na(StalenessMinutes) &
        StalenessMinutes == 0
    )
}


# ============================================================
# 21. Match prices for all eight futures
# ============================================================

latest_prior_matches <- bind_rows(
  lapply(
    LIVE_FUTURES_SYMBOLS,
    function(current_symbol) {
      match_latest_prior_for_symbol(
        target_data = competition_target_grid,
        close_data = futures_close_data,
        symbol = current_symbol
      )
    }
  )
) |>
  arrange(
    Symbol,
    CompetitionTimestampCT
  )


# ============================================================
# 22. Validate no look-ahead price matching
# ============================================================

future_looking_prices <- latest_prior_matches |>
  filter(
    !is.na(MatchedEndTimestampCT),
    MatchedEndTimestampCT > CompetitionTimestampCT
  )

if (nrow(future_looking_prices) > 0L) {
  print(future_looking_prices)
  
  stop(
    "Latest-prior matching used future price information.",
    call. = FALSE
  )
}

negative_staleness <- latest_prior_matches |>
  filter(
    !is.na(StalenessMinutes),
    StalenessMinutes < -1e-10
  )

if (nrow(negative_staleness) > 0L) {
  print(negative_staleness)
  
  stop(
    "Negative price staleness detected.",
    call. = FALSE
  )
}


# ============================================================
# 23. Attach roll mapping to each matched price
# ============================================================

attach_roll_mapping_for_symbol <- function(
    matched_data,
    roll_mapping_data,
    roll_changes_data,
    symbol
) {
  
  symbol_matches <- matched_data |>
    filter(
      Symbol == symbol
    ) |>
    arrange(
      CompetitionTimestampCT
    )
  
  symbol_mapping <- roll_mapping_data |>
    filter(
      Symbol == symbol
    ) |>
    arrange(
      Date
    )
  
  symbol_changes <- roll_changes_data |>
    filter(
      Symbol == symbol
    ) |>
    arrange(
      Date
    )
  
  if (nrow(symbol_mapping) == 0L) {
    stop(
      paste0(
        "No roll mapping observations available for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  target_dates <- as.numeric(
    symbol_matches$MatchedRawDate
  )
  
  mapping_dates <- as.numeric(
    symbol_mapping$Date
  )
  
  matched_mapping_index <- findInterval(
    target_dates,
    mapping_dates
  )
  
  valid_mapping <-
    !is.na(matched_mapping_index) &
    matched_mapping_index > 0L
  
  mapped_contract <- rep(
    NA_character_,
    nrow(symbol_matches)
  )
  
  mapped_contract_date <- rep(
    as.Date(NA),
    nrow(symbol_matches)
  )
  
  if (any(valid_mapping)) {
    
    valid_indices <- matched_mapping_index[valid_mapping]
    
    mapped_contract[valid_mapping] <-
      symbol_mapping$UnderlyingContract[valid_indices]
    
    mapped_contract_date[valid_mapping] <-
      symbol_mapping$Date[valid_indices]
  }
  
  roll_risk_dates <- sort(
    unique(
      c(
        symbol_changes$Date,
        symbol_changes$Date - 1L
      )
    )
  )
  
  symbol_matches |>
    mutate(
      RollMappingDate = mapped_contract_date,
      UnderlyingContract = mapped_contract,
      RollRiskDate =
        !is.na(MatchedRawDate) &
        MatchedRawDate %in% roll_risk_dates
    )
}


latest_prior_matches <- bind_rows(
  lapply(
    LIVE_FUTURES_SYMBOLS,
    function(current_symbol) {
      
      attach_roll_mapping_for_symbol(
        matched_data =
          latest_prior_matches,
        roll_mapping_data =
          futures_roll_history,
        roll_changes_data =
          roll_changes,
        symbol =
          current_symbol
      )
    }
  )
) |>
  arrange(
    Symbol,
    CompetitionTimestampCT
  )

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "YAHOO"
) {
  
  yahoo_identity_lookup <-
    yahoo_contract_identity |>
    rename(
      YahooIdentityTimestampCT =
        EndTimestampCT
    )
  
  latest_prior_matches <-
    latest_prior_matches |>
    left_join(
      yahoo_identity_lookup,
      by = c(
        "Symbol",
        "MatchedEndTimestampCT" =
          "YahooIdentityTimestampCT"
      )
    ) |>
    mutate(
      UnderlyingContract = if_else(
        SourceType ==
          "YahooFinance",
        NormalizedUnderlyingContract,
        UnderlyingContract
      ),
      RollMappingDate = if_else(
        SourceType ==
          "YahooFinance",
        as.Date(
          MatchedEndTimestampCT,
          tz =
            COMPETITION_TIME_ZONE
        ),
        RollMappingDate
      ),
      RollRiskDate = if_else(
        SourceType ==
          "YahooFinance",
        FALSE,
        RollRiskDate
      )
    )
  
  missing_yahoo_contract_identity <-
    latest_prior_matches |>
    filter(
      SourceType ==
        "YahooFinance",
      is.na(
        NormalizedUnderlyingContract
      )
    )
  
  if (
    nrow(
      missing_yahoo_contract_identity
    ) > 0L
  ) {
    print(
      missing_yahoo_contract_identity |>
        select(
          Symbol,
          CompetitionTimestampCT,
          MatchedEndTimestampCT,
          MatchedClose
        )
    )
    
    stop(
      paste0(
        "A matched Yahoo price lacks timestamp-level ",
        "contract identity."
      ),
      call. = FALSE
    )
  }
}


# ============================================================
# Stage 6 Yahoo timestamp-level switch detection
# ============================================================

yahoo_contract_switches <- tibble(
  Symbol = character(),
  SwitchTimestampCT = as.POSIXct(
    character(),
    tz = COMPETITION_TIME_ZONE
  )
)

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "YAHOO"
) {
  
  yahoo_contract_switches <-
    yahoo_contract_identity |>
    group_by(
      Symbol
    ) |>
    arrange(
      EndTimestampCT,
      .by_group = TRUE
    ) |>
    mutate(
      PreviousNormalizedUnderlyingContract =
        lag(
          NormalizedUnderlyingContract
        ),
      ContractChanged =
        !is.na(
          PreviousNormalizedUnderlyingContract
        ) &
        PreviousNormalizedUnderlyingContract !=
        NormalizedUnderlyingContract
    ) |>
    ungroup() |>
    filter(
      ContractChanged
    ) |>
    transmute(
      Symbol,
      SwitchTimestampCT =
        EndTimestampCT
    )
}


# ============================================================
# 24. Attach previous endpoint information
# ============================================================

return_price_pairs <- latest_prior_matches |>
  group_by(
    Symbol
  ) |>
  arrange(
    CompetitionTimestampCT,
    .by_group = TRUE
  ) |>
  mutate(
    PreviousCompetitionObservation = lag(
      CompetitionObservation
    ),
    PreviousScheduleDate = lag(
      ScheduleDate
    ),
    PreviousScheduleDay = lag(
      ScheduleDay
    ),
    PreviousCompetitionClockCT = lag(
      CompetitionClockCT
    ),
    PreviousCompetitionTimestampCT = lag(
      CompetitionTimestampCT
    ),
    PreviousMatchedClose = lag(
      MatchedClose
    ),
    PreviousMatchedEndTimestampCT = lag(
      MatchedEndTimestampCT
    ),
    PreviousStalenessMinutes = lag(
      StalenessMinutes
    ),
    PreviousExactMatch = lag(
      ExactMatch
    ),
    PreviousRollMappingDate = lag(
      RollMappingDate
    ),
    PreviousUnderlyingContract = lag(
      UnderlyingContract
    ),
    PreviousRollRiskDate = lag(
      RollRiskDate
    ),
    ObservedContractTransition =
      !is.na(PreviousUnderlyingContract) &
      !is.na(UnderlyingContract) &
      PreviousUnderlyingContract != UnderlyingContract,
    RollAffected =
      RollRiskDate |
      dplyr::coalesce(
        PreviousRollRiskDate,
        FALSE
      ) |
      ObservedContractTransition,
    CompetitionIntervalMinutes = as.numeric(
      difftime(
        CompetitionTimestampCT,
        PreviousCompetitionTimestampCT,
        units = "mins"
      )
    )
  ) |>
  ungroup()

return_price_pairs <-
  return_price_pairs |>
  mutate(
    YahooSwitchInInterval = FALSE
  )

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "YAHOO" &&
  nrow(
    yahoo_contract_switches
  ) > 0L
) {
  
  for (
    row_index in seq_len(
      nrow(
        return_price_pairs
      )
    )
  ) {
    
    interval_start <-
      return_price_pairs$
      PreviousCompetitionTimestampCT[
        row_index
      ]
    
    interval_end <-
      return_price_pairs$
      CompetitionTimestampCT[
        row_index
      ]
    
    if (
      is.na(interval_start) ||
      is.na(interval_end)
    ) {
      next
    }
    
    current_symbol <-
      return_price_pairs$
      Symbol[
        row_index
      ]
    
    switch_found <-
      yahoo_contract_switches |>
      filter(
        Symbol ==
          current_symbol,
        SwitchTimestampCT >
          interval_start,
        SwitchTimestampCT <=
          interval_end
      )
    
    if (
      nrow(
        switch_found
      ) > 0L
    ) {
      return_price_pairs$
        YahooSwitchInInterval[
          row_index
        ] <- TRUE
    }
  }
  
  return_price_pairs <-
    return_price_pairs |>
    mutate(
      RollAffected =
        RollAffected |
        YahooSwitchInInterval
    )
}

# ============================================================
# 25. Construct final 120-minute candidate returns
# ============================================================

live_futures_returns <- return_price_pairs |>
  mutate(
    MaximumStalenessMinutes =
      FINAL_MAXIMUM_STALENESS_MINUTES,
    CurrentPriceUsable =
      !is.na(MatchedClose) &
      !is.na(StalenessMinutes) &
      StalenessMinutes <=
      FINAL_MAXIMUM_STALENESS_MINUTES &
      MatchedClose > 0,
    PreviousPriceUsable =
      !is.na(PreviousMatchedClose) &
      !is.na(PreviousStalenessMinutes) &
      PreviousStalenessMinutes <=
      FINAL_MAXIMUM_STALENESS_MINUTES &
      PreviousMatchedClose > 0,
    ContractIdentityAvailable =
      !is.na(UnderlyingContract) &
      !is.na(PreviousUnderlyingContract),
    CandidateReturnUsable =
      IsReturnEndpoint &
      !is.na(PreviousCompetitionTimestampCT) &
      CompetitionIntervalMinutes %in%
      VALID_RETURN_INTERVAL_MINUTES &
      CurrentPriceUsable &
      PreviousPriceUsable &
      ContractIdentityAvailable &
      !RollAffected,
    CandidateLogReturn = if_else(
      CandidateReturnUsable,
      log(
        MatchedClose /
          PreviousMatchedClose
      ),
      NA_real_
    ),
    CandidateArithmeticReturn = if_else(
      CandidateReturnUsable,
      MatchedClose /
        PreviousMatchedClose -
        1,
      NA_real_
    )
  ) |>
  arrange(
    Symbol,
    CompetitionTimestampCT
  )


# ============================================================
# 26. Explicitly prevent weekend returns
# ============================================================

weekend_returns <- live_futures_returns |>
  filter(
    CandidateReturnUsable,
    CompetitionIntervalMinutes > 480
  )

if (nrow(weekend_returns) > 0L) {
  print(weekend_returns)
  
  stop(
    "A weekend return was incorrectly classified as usable.",
    call. = FALSE
  )
}


# ============================================================
# 27. Validate return interval lengths
# ============================================================

usable_interval_lengths <- sort(
  unique(
    live_futures_returns$CompetitionIntervalMinutes[
      live_futures_returns$CandidateReturnUsable
    ]
  )
)

if (
  any(
    !usable_interval_lengths %in%
    VALID_RETURN_INTERVAL_MINUTES
  )
) {
  stop(
    paste0(
      "Usable futures returns contain an invalid competition ",
      "interval length."
    ),
    call. = FALSE
  )
}


# ============================================================
# 28. Validate return formulas
# ============================================================

return_formula_check <- live_futures_returns |>
  filter(
    CandidateReturnUsable
  ) |>
  mutate(
    ReconstructedArithmeticReturn =
      exp(CandidateLogReturn) - 1,
    ArithmeticDifference =
      CandidateArithmeticReturn -
      ReconstructedArithmeticReturn,
    DirectLogReturn =
      log(
        MatchedClose /
          PreviousMatchedClose
      ),
    LogDifference =
      CandidateLogReturn -
      DirectLogReturn
  )

if (
  any(
    abs(
      return_formula_check$ArithmeticDifference
    ) >= 1e-12
  )
) {
  stop(
    "Arithmetic and log futures returns are inconsistent.",
    call. = FALSE
  )
}

if (
  any(
    abs(
      return_formula_check$LogDifference
    ) >= 1e-12
  )
) {
  stop(
    "CandidateLogReturn does not equal the direct Close-price log return.",
    call. = FALSE
  )
}


# ============================================================
# 29. Validate finite usable returns
# ============================================================

invalid_usable_returns <- live_futures_returns |>
  filter(
    CandidateReturnUsable,
    !is.finite(CandidateLogReturn) |
      !is.finite(CandidateArithmeticReturn)
  )

if (nrow(invalid_usable_returns) > 0L) {
  print(invalid_usable_returns)
  
  stop(
    "Non-finite usable futures returns detected.",
    call. = FALSE
  )
}


# ============================================================
# 30. Validate roll exclusion
# ============================================================

usable_roll_affected_returns <- live_futures_returns |>
  filter(
    CandidateReturnUsable,
    RollAffected
  )

if (nrow(usable_roll_affected_returns) > 0L) {
  print(usable_roll_affected_returns)
  
  stop(
    "A roll-affected return was incorrectly classified as usable.",
    call. = FALSE
  )
}


usable_yahoo_switch_returns <-
  live_futures_returns |>
  filter(
    CandidateReturnUsable,
    YahooSwitchInInterval
  )

if (
  nrow(
    usable_yahoo_switch_returns
  ) > 0L
) {
  print(
    usable_yahoo_switch_returns
  )
  
  stop(
    paste0(
      "A return spanning a Yahoo contract switch ",
      "was incorrectly classified as usable."
    ),
    call. = FALSE
  )
}


# ============================================================
# 31. Build individually usable model history
# ============================================================
#
# This is the object required for the selected univariate
# futures models.
#
# We intentionally do NOT require simultaneous availability
# across all eight futures here.
#
# ============================================================

live_futures_model_history <- live_futures_returns |>
  filter(
    CandidateReturnUsable,
    !RollAffected,
    is.finite(CandidateLogReturn)
  ) |>
  arrange(
    Symbol,
    CompetitionTimestampCT
  )


# ============================================================
# 32. Validate all eight futures remain represented
# ============================================================

missing_model_symbols <- setdiff(
  LIVE_FUTURES_SYMBOLS,
  unique(live_futures_model_history$Symbol)
)

if (length(missing_model_symbols) > 0L) {
  stop(
    paste0(
      "Usable futures model history is missing symbols: ",
      paste(
        missing_model_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 33. Live-week return diagnostics
# ============================================================

live_week_returns <- live_futures_returns |>
  filter(
    ScheduleDate >= LIVE_COMPETITION_SUNDAY,
    ScheduleDate <= LIVE_COMPETITION_FRIDAY,
    IsReturnEndpoint
  )

live_week_usable_returns <- live_week_returns |>
  filter(
    CandidateReturnUsable
  )

live_week_coverage <- live_week_returns |>
  group_by(
    Symbol
  ) |>
  summarise(
    ReturnTargets = n(),
    UsableReturns = sum(
      CandidateReturnUsable
    ),
    MissingReturns =
      ReturnTargets -
      UsableReturns,
    CoverageRate = mean(
      CandidateReturnUsable
    ),
    RollAffectedReturns = sum(
      RollAffected,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ============================================================
# 34. Full model-history diagnostics
# ============================================================

model_history_summary <- live_futures_model_history |>
  group_by(
    Symbol
  ) |>
  summarise(
    Observations = n(),
    FirstReturnTimestampCT = min(
      CompetitionTimestampCT
    ),
    LastReturnTimestampCT = max(
      CompetitionTimestampCT
    ),
    MeanLogReturn = mean(
      CandidateLogReturn
    ),
    SDLogReturn = sd(
      CandidateLogReturn
    ),
    .groups = "drop"
  )


# ============================================================
# 35. Roll-event diagnostics
# ============================================================

roll_event_summary <- roll_changes |>
  group_by(
    Symbol
  ) |>
  summarise(
    RollEvents = n(),
    FirstRollDate = min(
      Date
    ),
    LastRollDate = max(
      Date
    ),
    .groups = "drop"
  )


# ============================================================
# 36. Check live competition current progress if Stage 2 exists
# ============================================================

if (
  exists(
    "live_competition_clock",
    envir = .GlobalEnv
  )
) {
  
  stage2_current_t <-
    live_competition_clock[["current_t"]]
  
  stage2_current_day <-
    live_competition_clock[["current_day"]]
  
  stage2_current_time_ct <-
    live_competition_clock[["current_time_ct"]]
  
  cat(
    "\nStage 2 clock detected.\n"
  )
  
  cat(
    "Stage 2 current_t: ",
    stage2_current_t,
    "\n",
    sep = ""
  )
  
  cat(
    "Stage 2 decision timestamp: ",
    stage2_current_day,
    " ",
    stage2_current_time_ct,
    " CT\n",
    sep = ""
  )
}


# ============================================================
# 37. Validate that no live return extends beyond available data
# ============================================================
#
# A scheduled endpoint can exist after the last available
# Bloomberg observation, but it must not become a usable return
# merely by carrying an old price indefinitely.
#
# The 120-minute staleness rule prevents this.
#
# ============================================================

usable_after_last_raw_price <- live_futures_returns |>
  group_by(
    Symbol
  ) |>
  mutate(
    LastRawPriceTimestampCT = max(
      futures_history$EndTimestampCT[
        futures_history$Symbol == first(Symbol)
      ]
    )
  ) |>
  ungroup() |>
  filter(
    CandidateReturnUsable,
    MatchedEndTimestampCT >
      LastRawPriceTimestampCT
  )

if (nrow(usable_after_last_raw_price) > 0L) {
  print(usable_after_last_raw_price)
  
  stop(
    "A usable return references a price after the available raw history.",
    call. = FALSE
  )
}


# ============================================================
# 38. Write audit return panel
# ============================================================

readr::write_csv(
  live_futures_returns,
  LIVE_RETURNS_OUTPUT_PATH,
  na = ""
)


# ============================================================
# 39. Write usable model-history panel
# ============================================================

readr::write_csv(
  live_futures_model_history,
  LIVE_MODEL_HISTORY_OUTPUT_PATH,
  na = ""
)


# ============================================================
# 40. Reload written outputs
# ============================================================

written_live_returns <- readr::read_csv(
  LIVE_RETURNS_OUTPUT_PATH,
  show_col_types = FALSE
)

written_model_history <- readr::read_csv(
  LIVE_MODEL_HISTORY_OUTPUT_PATH,
  show_col_types = FALSE
)

if (
  nrow(written_live_returns) !=
  nrow(live_futures_returns)
) {
  stop(
    paste0(
      "Written live_futures_returns.csv row count does not ",
      "match the in-memory object."
    ),
    call. = FALSE
  )
}

if (
  nrow(written_model_history) !=
  nrow(live_futures_model_history)
) {
  stop(
    paste0(
      "Written live_futures_model_history.csv row count does ",
      "not match the in-memory object."
    ),
    call. = FALSE
  )
}


# ============================================================
# 41. Final validation flag
# ============================================================

LIVE_FUTURES_RETURNS_VALIDATED <-
  nrow(future_looking_prices) == 0L &&
  nrow(negative_staleness) == 0L &&
  nrow(weekend_returns) == 0L &&
  nrow(invalid_usable_returns) == 0L &&
  nrow(usable_roll_affected_returns) == 0L &&
  nrow(usable_yahoo_switch_returns) == 0L &&
  length(missing_model_symbols) == 0L &&
  all(
    usable_interval_lengths %in%
      VALID_RETURN_INTERVAL_MINUTES
  ) &&
  all(
    abs(
      return_formula_check$ArithmeticDifference
    ) < 1e-12
  ) &&
  all(
    abs(
      return_formula_check$LogDifference
    ) < 1e-12
  )

if (!isTRUE(LIVE_FUTURES_RETURNS_VALIDATED)) {
  stop(
    "Live futures-return validation failed.",
    call. = FALSE
  )
}


# ============================================================
# 42. Final diagnostics
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "LIVE FUTURES RETURNS BUILD VALIDATION\n"
)

cat(
  "============================================================\n\n"
)

cat(
  "Futures data source: ",
  LIVE_FUTURES_DATA_SOURCE,
  "\n",
  sep = ""
)

cat(
  "Active futures history: ",
  ACTIVE_FUTURES_HISTORY_PATH,
  "\n\n",
  sep = ""
)

cat(
  "Maximum staleness: ",
  FINAL_MAXIMUM_STALENESS_MINUTES,
  " minutes\n",
  sep = ""
)

cat(
  "Valid return intervals: ",
  paste(
    VALID_RETURN_INTERVAL_MINUTES,
    collapse = ", "
  ),
  " minutes\n\n",
  sep = ""
)

cat(
  "Price-history rows: ",
  nrow(futures_history),
  "\n",
  sep = ""
)

cat(
  "Roll-history rows: ",
  nrow(futures_roll_history),
  "\n",
  sep = ""
)

cat(
  "Detected roll events: ",
  nrow(roll_changes),
  "\n",
  sep = ""
)

cat(
  "Return-panel rows: ",
  nrow(live_futures_returns),
  "\n",
  sep = ""
)

cat(
  "Individually usable model-history rows: ",
  nrow(live_futures_model_history),
  "\n\n",
  sep = ""
)

cat(
  "Usable interval lengths: ",
  paste(
    usable_interval_lengths,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "Future-looking matches: ",
  nrow(future_looking_prices),
  "\n",
  sep = ""
)

cat(
  "Invalid usable returns: ",
  nrow(invalid_usable_returns),
  "\n",
  sep = ""
)

cat(
  "Usable roll-affected returns: ",
  nrow(usable_roll_affected_returns),
  "\n\n",
  sep = ""
)

cat(
  "Model history by future:\n\n"
)

print(
  model_history_summary,
  n = Inf
)

cat(
  "\nLive competition return coverage so far / available schedule:\n\n"
)

print(
  live_week_coverage,
  n = Inf
)

cat(
  "\nDetected roll events by future:\n\n"
)

print(
  roll_event_summary,
  n = Inf
)

cat(
  "\nAudit output:\n",
  LIVE_RETURNS_OUTPUT_PATH,
  "\n",
  sep = ""
)

cat(
  "\nModel-history output:\n",
  LIVE_MODEL_HISTORY_OUTPUT_PATH,
  "\n",
  sep = ""
)

cat(
  "\n============================================================\n"
)

cat(
  "LIVE_FUTURES_RETURNS_VALIDATED = TRUE\n"
)

cat(
  "============================================================\n"
)
