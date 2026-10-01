# ============================================================
# scripts/futures_data.R
# ============================================================
#
# Purpose:
#
#   Load, parse, validate, and prepare the eight raw Bloomberg
#   GIT intraday futures files used in the trading competition
#   forecasting pipeline.
#
# This script handles:
#
#   1. Mapping competition symbols to raw Bloomberg CSV files.
#   2. Reading Bloomberg GIT exports.
#   3. Identifying Summary, date-marker, and interval rows.
#   4. Parsing Bloomberg date markers.
#   5. Parsing 30-minute interval labels.
#   6. Constructing interval start and end timestamps.
#   7. Interpreting Bloomberg timestamps in America/New_York.
#   8. Converting timestamps to America/Chicago.
#   9. Preserving OHLC, Tick Count, and Volume data.
#  10. Validating parsed data.
#  11. Loading all eight futures into one aligned raw-data object.
#  12. Producing coverage, missingness, and spacing summaries.
#  13. Loading historical generic-futures underlying-contract mappings.
#  14. Identifying daily generic-contract change dates.
#
# This script DOES NOT:
#
#   - construct the competition observation schedule;
#   - select competition timestamps;
#   - fill missing bars;
#   - calculate returns;
#   - attach roll mappings to competition return intervals;
#   - remove futures roll returns;
#   - fit forecasting models;
#   - estimate covariance matrices;
#   - write production optimizer files.
#
# Those tasks belong in later notebooks and scripts.
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
# 2. Time-zone constants
# ============================================================

BLOOMBERG_TIME_ZONE <- "America/New_York"

COMPETITION_TIME_ZONE <- "America/Chicago"


# ============================================================
# 3. Raw-data directory
# ============================================================

DEFAULT_FUTURES_RAW_DIR <- here(
  "data",
  "raw"
)


# ============================================================
# 4. Competition symbols
# ============================================================

FUTURES_SYMBOLS <- c(
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
# 5. Raw Bloomberg file mapping
# ============================================================

FUTURES_RAW_FILE_MAP <- c(
  CL  = "CL1_GIT_test.csv",
  GC  = "GC1_GIT_test.csv",
  SR3 = "SFR1_GIT_test.csv",
  ZN  = "TY1_GIT_test.csv",
  KE  = "KE1_GIT_test.csv",
  HO  = "HO1_GIT_test.csv",
  ZW  = "W_GIT_test.csv",
  NQ  = "NQ1_GIT_test.csv"
)


# ============================================================
# 6. Historical generic-futures roll file mapping
# ============================================================

FUTURES_ROLL_FILE_MAP <- c(
  CL  = "CL_roll_mapping.csv",
  GC  = "GC_roll_mapping.csv",
  SR3 = "SR3_roll_mapping.csv",
  ZN  = "ZN_roll_mapping.csv",
  KE  = "KE_roll_mapping.csv",
  HO  = "HO_roll_mapping.csv",
  ZW  = "ZW_roll_mapping.csv",
  NQ  = "NQ_roll_mapping.csv"
)


# ============================================================
# 6. Bloomberg security mapping
# ============================================================

FUTURES_BLOOMBERG_SECURITY_MAP <- c(
  CL  = "CL1 Comdty",
  GC  = "GC1 Comdty",
  SR3 = "SFR1 Comdty",
  ZN  = "TY1 Comdty",
  KE  = "KE1 Comdty",
  HO  = "HO1 Comdty",
  ZW  = "W1 Comdty",
  NQ  = "NQ1 Index"
)


# ============================================================
# 7. Expected Bloomberg GIT columns
# ============================================================

BLOOMBERG_GIT_EXPECTED_COLUMNS <- c(
  "Time Interval",
  "Close",
  "Net Chg",
  "Open",
  "High",
  "Low",
  "Tick Count",
  "Volume"
)


# ============================================================
# 8. Parsed output columns
# ============================================================

FUTURES_PARSED_REQUIRED_COLUMNS <- c(
  "Symbol",
  "BloombergSecurity",
  "SourceFile",
  "SourceRow",
  "RawDateMarker",
  "RawDate",
  "RawTimeInterval",
  "StartTime",
  "EndTime",
  "StartTimestampET",
  "EndTimestampET",
  "StartTimestampCT",
  "EndTimestampCT",
  "StartDateCT",
  "StartTimeCT",
  "EndDateCT",
  "EndTimeCT",
  "NominalIntervalMinutes",
  "ActualIntervalMinutes",
  "Close",
  "NetChg",
  "Open",
  "High",
  "Low",
  "TickCount",
  "Volume"
)


# ============================================================
# 9. Validate symbol
# ============================================================

validate_futures_symbol <- function(
    symbol
) {
  
  if (
    length(symbol) != 1L ||
    !is.character(symbol) ||
    is.na(symbol)
  ) {
    stop(
      "symbol must be one non-missing character value.",
      call. = FALSE
    )
  }
  
  if (!symbol %in% FUTURES_SYMBOLS) {
    stop(
      paste0(
        "Unsupported futures symbol: ",
        symbol,
        ". Supported symbols are: ",
        paste(
          FUTURES_SYMBOLS,
          collapse = ", "
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 10. Resolve raw file paths
# ============================================================

resolve_futures_raw_paths <- function(
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  raw_dir <- as.character(
    raw_dir
  )
  
  tibble(
    Symbol = FUTURES_SYMBOLS,
    BloombergSecurity =
      unname(
        FUTURES_BLOOMBERG_SECURITY_MAP[
          FUTURES_SYMBOLS
        ]
      ),
    FileName =
      unname(
        FUTURES_RAW_FILE_MAP[
          FUTURES_SYMBOLS
        ]
      ),
    Path =
      file.path(
        raw_dir,
        unname(
          FUTURES_RAW_FILE_MAP[
            FUTURES_SYMBOLS
          ]
        )
      )
  )
}


# ============================================================
# 11. Check that all raw files exist
# ============================================================

check_futures_raw_files_exist <- function(
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  path_table <- resolve_futures_raw_paths(
    raw_dir = raw_dir
  )
  
  path_table <- path_table |>
    mutate(
      Exists = file.exists(
        Path
      )
    )
  
  if (!all(path_table$Exists)) {
    
    missing_files <- path_table |>
      filter(
        !Exists
      )
    
    stop(
      paste0(
        "The following raw futures files are missing:\n",
        paste(
          paste0(
            missing_files$Symbol,
            ": ",
            missing_files$Path
          ),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }
  
  path_table
}


# ============================================================
# 12. Clean column names
# ============================================================

clean_git_column_names <- function(
    data
) {
  
  names(data) <- trimws(
    names(data)
  )
  
  data
}


# ============================================================
# 13. Validate Bloomberg GIT columns
# ============================================================

check_bloomberg_git_columns <- function(
    data
) {
  
  missing_columns <- setdiff(
    BLOOMBERG_GIT_EXPECTED_COLUMNS,
    names(data)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Bloomberg GIT file is missing required columns: ",
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
# 14. Read one raw Bloomberg GIT CSV
# ============================================================

read_bloomberg_git_raw <- function(
    path
) {
  
  if (
    length(path) != 1L ||
    !is.character(path)
  ) {
    stop(
      "path must be one character value.",
      call. = FALSE
    )
  }
  
  if (!file.exists(path)) {
    stop(
      paste0(
        "Raw Bloomberg GIT file does not exist:\n",
        path
      ),
      call. = FALSE
    )
  }
  
  data <- readr::read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "minimal",
    trim_ws = TRUE
  )
  
  data <- clean_git_column_names(
    data
  )
  
  check_bloomberg_git_columns(
    data
  )
  
  if (nrow(data) == 0L) {
    stop(
      paste0(
        "Bloomberg GIT file contains zero rows:\n",
        path
      ),
      call. = FALSE
    )
  }
  
  data
}


# ============================================================
# 15. Detect Summary rows
# ============================================================

is_git_summary_row <- function(
    x
) {
  
  x <- trimws(
    as.character(x)
  )
  
  !is.na(x) &
    x == "Summary"
}


# ============================================================
# 16. Detect Bloomberg date-marker rows
# ============================================================

is_git_date_marker <- function(
    x
) {
  
  x <- trimws(
    as.character(x)
  )
  
  format_one <-
    grepl(
      "^[0-9]{2}[A-Za-z]{3}[0-9]{4}_",
      x
    )
  
  format_two <-
    grepl(
      "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}",
      x
    )
  
  !is.na(x) &
    (
      format_one |
        format_two
    )
}


# ============================================================
# 17. Detect interval rows
# ============================================================

is_git_interval_row <- function(
    x
) {
  
  x <- trimws(
    as.character(x)
  )
  
  !is.na(x) &
    grepl(
      "^[0-9]{1,2}:[0-9]{2}[[:space:]]*-[[:space:]]*[0-9]{1,2}:[0-9]{2}$",
      x
    )
}


# ============================================================
# 18. Detect blank rows
# ============================================================

is_git_blank_row <- function(
    x
) {
  
  x <- as.character(
    x
  )
  
  is.na(x) |
    trimws(x) == ""
}


# ============================================================
# 19. Classify Bloomberg GIT rows
# ============================================================

classify_git_rows <- function(
    data
) {
  
  check_bloomberg_git_columns(
    data
  )
  
  interval_text <- data[[
    "Time Interval"
  ]]
  
  row_type <- case_when(
    is_git_summary_row(
      interval_text
    ) ~ "Summary",
    
    is_git_date_marker(
      interval_text
    ) ~ "DateMarker",
    
    is_git_interval_row(
      interval_text
    ) ~ "Interval",
    
    is_git_blank_row(
      interval_text
    ) ~ "Blank",
    
    TRUE ~ "Other"
  )
  
  data |>
    mutate(
      SourceRow = row_number(),
      RowType = row_type
    )
}


# ============================================================
# 20. Month-number helper
# ============================================================

bloomberg_month_number <- function(
    month_text
) {
  
  month_lookup <- c(
    JAN = 1L,
    FEB = 2L,
    MAR = 3L,
    APR = 4L,
    MAY = 5L,
    JUN = 6L,
    JUL = 7L,
    AUG = 8L,
    SEP = 9L,
    OCT = 10L,
    NOV = 11L,
    DEC = 12L
  )
  
  month_text <- toupper(
    month_text
  )
  
  result <- unname(
    month_lookup[
      month_text
    ]
  )
  
  as.integer(
    result
  )
}


# ============================================================
# 21. Parse Bloomberg date markers
# ============================================================

parse_bloomberg_date_marker <- function(
    x
) {
  
  x <- trimws(
    as.character(x)
  )
  
  result <- rep(
    as.Date(NA),
    length(x)
  )
  
  for (i in seq_along(x)) {
    
    value <- x[[i]]
    
    if (
      is.na(value) ||
      value == ""
    ) {
      next
    }
    
    if (
      grepl(
        "^[0-9]{2}[A-Za-z]{3}[0-9]{4}_",
        value
      )
    ) {
      
      date_text <- substr(
        value,
        1L,
        9L
      )
      
      day_value <- suppressWarnings(
        as.integer(
          substr(
            date_text,
            1L,
            2L
          )
        )
      )
      
      month_text <- substr(
        date_text,
        3L,
        5L
      )
      
      month_value <- bloomberg_month_number(
        month_text
      )
      
      year_value <- suppressWarnings(
        as.integer(
          substr(
            date_text,
            6L,
            9L
          )
        )
      )
      
      if (
        is.na(day_value) ||
        is.na(month_value) ||
        is.na(year_value)
      ) {
        next
      }
      
      iso_date <- sprintf(
        "%04d-%02d-%02d",
        year_value,
        month_value,
        day_value
      )
      
      result[[i]] <- as.Date(
        iso_date
      )
      
      next
    }
    
    if (
      grepl(
        "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}",
        value
      )
    ) {
      
      date_text <- stringr::str_extract(
        value,
        "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}"
      )
      
      parsed_date <- suppressWarnings(
        lubridate::mdy(
          date_text,
          quiet = TRUE
        )
      )
      
      if (!is.na(parsed_date)) {
        result[[i]] <- as.Date(
          parsed_date
        )
      }
    }
  }
  
  result
}


# ============================================================
# 22. Standardize clock times
# ============================================================

standardize_clock_time <- function(
    x
) {
  
  x <- trimws(
    as.character(x)
  )
  
  hour_text <- stringr::str_extract(
    x,
    "^[0-9]{1,2}"
  )
  
  minute_text <- stringr::str_extract(
    x,
    "(?<=:)[0-9]{2}$"
  )
  
  hour_value <- suppressWarnings(
    as.integer(
      hour_text
    )
  )
  
  minute_value <- suppressWarnings(
    as.integer(
      minute_text
    )
  )
  
  valid <-
    !is.na(hour_value) &
    !is.na(minute_value) &
    hour_value >= 0L &
    hour_value <= 23L &
    minute_value >= 0L &
    minute_value <= 59L
  
  result <- rep(
    NA_character_,
    length(x)
  )
  
  result[valid] <- sprintf(
    "%02d:%02d",
    hour_value[valid],
    minute_value[valid]
  )
  
  result
}


# ============================================================
# 23. Split interval labels
# ============================================================

parse_git_interval_label <- function(
    x
) {
  
  x <- trimws(
    as.character(x)
  )
  
  matches <- stringr::str_match(
    x,
    "^([0-9]{1,2}:[0-9]{2})[[:space:]]*-[[:space:]]*([0-9]{1,2}:[0-9]{2})$"
  )
  
  tibble(
    StartTime =
      standardize_clock_time(
        matches[, 2L]
      ),
    EndTime =
      standardize_clock_time(
        matches[, 3L]
      )
  )
}


# ============================================================
# 24. Convert clock time to minutes after midnight
# ============================================================

clock_time_to_minutes <- function(
    x
) {
  
  x <- standardize_clock_time(
    x
  )
  
  hour_value <- suppressWarnings(
    as.integer(
      substr(
        x,
        1L,
        2L
      )
    )
  )
  
  minute_value <- suppressWarnings(
    as.integer(
      substr(
        x,
        4L,
        5L
      )
    )
  )
  
  hour_value * 60 +
    minute_value
}


# ============================================================
# 25. Parse numeric Bloomberg values
# ============================================================

parse_git_numeric <- function(
    x
) {
  
  if (is.numeric(x)) {
    return(
      as.numeric(x)
    )
  }
  
  suppressWarnings(
    readr::parse_number(
      as.character(x),
      na = c(
        "",
        "NA",
        "N/A",
        "-"
      )
    )
  )
}


# ============================================================
# 26. Build POSIXct timestamp
# ============================================================

build_local_timestamp <- function(
    date_value,
    time_value,
    time_zone
) {
  
  datetime_text <- paste(
    format(
      date_value,
      "%Y-%m-%d"
    ),
    time_value
  )
  
  as.POSIXct(
    datetime_text,
    format = "%Y-%m-%d %H:%M",
    tz = time_zone
  )
}


# ============================================================
# 27. Parse one Bloomberg GIT dataset
# ============================================================

parse_bloomberg_git_data <- function(
    raw_data,
    symbol,
    source_file
) {
  
  validate_futures_symbol(
    symbol
  )
  
  check_bloomberg_git_columns(
    raw_data
  )
  
  classified <- classify_git_rows(
    raw_data
  )
  
  summary_rows <- sum(
    classified$RowType == "Summary"
  )
  
  date_marker_rows <- sum(
    classified$RowType == "DateMarker"
  )
  
  interval_rows <- sum(
    classified$RowType == "Interval"
  )
  
  blank_rows <- sum(
    classified$RowType == "Blank"
  )
  
  other_rows <- sum(
    classified$RowType == "Other"
  )
  
  if (date_marker_rows == 0L) {
    stop(
      paste0(
        "No Bloomberg date-marker rows were found for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (interval_rows == 0L) {
    stop(
      paste0(
        "No Bloomberg intraday interval rows were found for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  classified <- classified |>
    mutate(
      RawDateMarker =
        if_else(
          RowType == "DateMarker",
          as.character(
            .data[[
              "Time Interval"
            ]]
          ),
          NA_character_
        )
    ) |>
    tidyr::fill(
      RawDateMarker,
      .direction = "down"
    )
  
  interval_data <- classified |>
    filter(
      RowType == "Interval"
    )
  
  if (
    any(
      is.na(
        interval_data$RawDateMarker
      )
    )
  ) {
    stop(
      paste0(
        "At least one interval row occurs before a date marker for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  parsed_dates <- parse_bloomberg_date_marker(
    interval_data$RawDateMarker
  )
  
  if (any(is.na(parsed_dates))) {
    
    bad_markers <- unique(
      interval_data$RawDateMarker[
        is.na(parsed_dates)
      ]
    )
    
    stop(
      paste0(
        "Could not parse Bloomberg date markers for ",
        symbol,
        ": ",
        paste(
          bad_markers,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  interval_parts <- parse_git_interval_label(
    interval_data[[
      "Time Interval"
    ]]
  )
  
  if (
    any(
      is.na(
        interval_parts$StartTime
      )
    ) ||
    any(
      is.na(
        interval_parts$EndTime
      )
    )
  ) {
    stop(
      paste0(
        "Could not parse one or more interval labels for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  start_minutes <- clock_time_to_minutes(
    interval_parts$StartTime
  )
  
  end_minutes <- clock_time_to_minutes(
    interval_parts$EndTime
  )
  
  nominal_interval_minutes <-
    end_minutes -
    start_minutes
  
  nominal_interval_minutes[
    nominal_interval_minutes < 0
  ] <-
    nominal_interval_minutes[
      nominal_interval_minutes < 0
    ] +
    24 * 60
  
  end_date <- parsed_dates
  
  crosses_midnight <-
    end_minutes <
    start_minutes
  
  end_date[
    crosses_midnight
  ] <-
    end_date[
      crosses_midnight
    ] +
    1L
  
  start_timestamp_et <- build_local_timestamp(
    date_value = parsed_dates,
    time_value = interval_parts$StartTime,
    time_zone = BLOOMBERG_TIME_ZONE
  )
  
  end_timestamp_et <- build_local_timestamp(
    date_value = end_date,
    time_value = interval_parts$EndTime,
    time_zone = BLOOMBERG_TIME_ZONE
  )
  
  if (
    any(
      is.na(
        start_timestamp_et
      )
    ) ||
    any(
      is.na(
        end_timestamp_et
      )
    )
  ) {
    stop(
      paste0(
        "Could not construct one or more Eastern Time timestamps for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  start_timestamp_ct <- lubridate::with_tz(
    start_timestamp_et,
    tzone = COMPETITION_TIME_ZONE
  )
  
  end_timestamp_ct <- lubridate::with_tz(
    end_timestamp_et,
    tzone = COMPETITION_TIME_ZONE
  )
  
  actual_interval_minutes <- as.numeric(
    difftime(
      end_timestamp_et,
      start_timestamp_et,
      units = "mins"
    )
  )
  
  parsed <- tibble(
    Symbol = symbol,
    BloombergSecurity =
      unname(
        FUTURES_BLOOMBERG_SECURITY_MAP[[
          symbol
        ]]
      ),
    SourceFile = basename(
      source_file
    ),
    SourceRow =
      interval_data$SourceRow,
    RawDateMarker =
      interval_data$RawDateMarker,
    RawDate =
      parsed_dates,
    RawTimeInterval =
      as.character(
        interval_data[[
          "Time Interval"
        ]]
      ),
    StartTime =
      interval_parts$StartTime,
    EndTime =
      interval_parts$EndTime,
    StartTimestampET =
      start_timestamp_et,
    EndTimestampET =
      end_timestamp_et,
    StartTimestampCT =
      start_timestamp_ct,
    EndTimestampCT =
      end_timestamp_ct,
    StartDateCT =
      as.Date(
        start_timestamp_ct,
        tz = COMPETITION_TIME_ZONE
      ),
    StartTimeCT =
      format(
        start_timestamp_ct,
        "%H:%M",
        tz = COMPETITION_TIME_ZONE
      ),
    EndDateCT =
      as.Date(
        end_timestamp_ct,
        tz = COMPETITION_TIME_ZONE
      ),
    EndTimeCT =
      format(
        end_timestamp_ct,
        "%H:%M",
        tz = COMPETITION_TIME_ZONE
      ),
    NominalIntervalMinutes =
      nominal_interval_minutes,
    ActualIntervalMinutes =
      actual_interval_minutes,
    Close =
      parse_git_numeric(
        interval_data[[
          "Close"
        ]]
      ),
    NetChg =
      parse_git_numeric(
        interval_data[[
          "Net Chg"
        ]]
      ),
    Open =
      parse_git_numeric(
        interval_data[[
          "Open"
        ]]
      ),
    High =
      parse_git_numeric(
        interval_data[[
          "High"
        ]]
      ),
    Low =
      parse_git_numeric(
        interval_data[[
          "Low"
        ]]
      ),
    TickCount =
      parse_git_numeric(
        interval_data[[
          "Tick Count"
        ]]
      ),
    Volume =
      parse_git_numeric(
        interval_data[[
          "Volume"
        ]]
      )
  )
  
  metadata <- list(
    Symbol = symbol,
    BloombergSecurity =
      unname(
        FUTURES_BLOOMBERG_SECURITY_MAP[[
          symbol
        ]]
      ),
    SourceFile = basename(
      source_file
    ),
    TotalSourceRows = nrow(
      classified
    ),
    SummaryRows = summary_rows,
    DateMarkerRows = date_marker_rows,
    IntervalRows = interval_rows,
    BlankRows = blank_rows,
    OtherRows = other_rows
  )
  
  attr(
    parsed,
    "git_metadata"
  ) <- metadata
  
  parsed
}


# ============================================================
# 28. Validate one parsed futures dataset
# ============================================================

validate_parsed_futures_data <- function(
    data,
    symbol = NULL
) {
  
  missing_columns <- setdiff(
    FUTURES_PARSED_REQUIRED_COLUMNS,
    names(data)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Parsed futures data are missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (nrow(data) == 0L) {
    stop(
      "Parsed futures data contain zero rows.",
      call. = FALSE
    )
  }
  
  if (!is.null(symbol)) {
    
    validate_futures_symbol(
      symbol
    )
    
    observed_symbols <- unique(
      data$Symbol
    )
    
    if (
      length(observed_symbols) != 1L ||
      observed_symbols[[1]] != symbol
    ) {
      stop(
        paste0(
          "Parsed dataset does not contain only symbol ",
          symbol,
          "."
        ),
        call. = FALSE
      )
    }
  }
  
  if (
    any(
      is.na(
        data$StartTimestampET
      )
    )
  ) {
    stop(
      "StartTimestampET contains missing values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(
        data$EndTimestampET
      )
    )
  ) {
    stop(
      "EndTimestampET contains missing values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(
        data$StartTimestampCT
      )
    )
  ) {
    stop(
      "StartTimestampCT contains missing values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(
        data$EndTimestampCT
      )
    )
  ) {
    stop(
      "EndTimestampCT contains missing values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      data$NominalIntervalMinutes <= 0,
      na.rm = TRUE
    )
  ) {
    stop(
      "At least one nominal Bloomberg interval has nonpositive duration.",
      call. = FALSE
    )
  }
  
  if (
    any(
      data$ActualIntervalMinutes <= 0,
      na.rm = TRUE
    )
  ) {
    warning(
      paste0(
        "At least one parsed interval has nonpositive actual duration. ",
        "Inspect possible daylight-saving-time ambiguity."
      ),
      call. = FALSE
    )
  }
  
  numeric_columns <- c(
    "Close",
    "NetChg",
    "Open",
    "High",
    "Low",
    "TickCount",
    "Volume",
    "NominalIntervalMinutes",
    "ActualIntervalMinutes"
  )
  
  bad_numeric_columns <- numeric_columns[
    !vapply(
      data[
        numeric_columns
      ],
      is.numeric,
      logical(1)
    )
  ]
  
  if (length(bad_numeric_columns) > 0L) {
    stop(
      paste0(
        "The following parsed columns are not numeric: ",
        paste(
          bad_numeric_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 29. Load one futures file
# ============================================================

load_futures_git <- function(
    symbol,
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  validate_futures_symbol(
    symbol
  )
  
  file_name <- unname(
    FUTURES_RAW_FILE_MAP[[
      symbol
    ]]
  )
  
  path <- file.path(
    as.character(
      raw_dir
    ),
    file_name
  )
  
  raw_data <- read_bloomberg_git_raw(
    path = path
  )
  
  parsed_data <- parse_bloomberg_git_data(
    raw_data = raw_data,
    symbol = symbol,
    source_file = path
  )
  
  validate_parsed_futures_data(
    data = parsed_data,
    symbol = symbol
  )
  
  parsed_data
}


# ============================================================
# 30. Load all eight futures files
# ============================================================

load_all_futures_git <- function(
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  check_futures_raw_files_exist(
    raw_dir = raw_dir
  )
  
  result <- vector(
    mode = "list",
    length = length(
      FUTURES_SYMBOLS
    )
  )
  
  names(result) <- FUTURES_SYMBOLS
  
  for (symbol in FUTURES_SYMBOLS) {
    
    result[[symbol]] <- load_futures_git(
      symbol = symbol,
      raw_dir = raw_dir
    )
  }
  
  result
}


# ============================================================
# 31. Combine all futures into one data frame
# ============================================================

bind_all_futures_git <- function(
    futures_list
) {
  
  if (!is.list(futures_list)) {
    stop(
      "futures_list must be a list.",
      call. = FALSE
    )
  }
  
  missing_symbols <- setdiff(
    FUTURES_SYMBOLS,
    names(futures_list)
  )
  
  if (length(missing_symbols) > 0L) {
    stop(
      paste0(
        "futures_list is missing symbols: ",
        paste(
          missing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  combined <- bind_rows(
    futures_list[
      FUTURES_SYMBOLS
    ]
  )
  
  combined <- combined |>
    mutate(
      Symbol = factor(
        Symbol,
        levels = FUTURES_SYMBOLS
      )
    ) |>
    arrange(
      StartTimestampET,
      Symbol,
      SourceRow
    ) |>
    mutate(
      Symbol = as.character(
        Symbol
      )
    )
  
  combined
}


# ============================================================
# 32. Extract metadata from one parsed dataset
# ============================================================

get_git_metadata <- function(
    data
) {
  
  metadata <- attr(
    data,
    "git_metadata"
  )
  
  if (is.null(metadata)) {
    stop(
      "No git_metadata attribute was found.",
      call. = FALSE
    )
  }
  
  metadata
}


# ============================================================
# 33. Summarize source-file structure
# ============================================================

summarize_futures_file_structure <- function(
    futures_list
) {
  
  rows <- lapply(
    FUTURES_SYMBOLS,
    function(symbol) {
      
      metadata <- get_git_metadata(
        futures_list[[
          symbol
        ]]
      )
      
      tibble(
        Symbol =
          metadata$Symbol,
        BloombergSecurity =
          metadata$BloombergSecurity,
        SourceFile =
          metadata$SourceFile,
        TotalSourceRows =
          metadata$TotalSourceRows,
        SummaryRows =
          metadata$SummaryRows,
        DateMarkerRows =
          metadata$DateMarkerRows,
        IntervalRows =
          metadata$IntervalRows,
        BlankRows =
          metadata$BlankRows,
        OtherRows =
          metadata$OtherRows
      )
    }
  )
  
  bind_rows(
    rows
  )
}


# ============================================================
# 34. Summarize data coverage
# ============================================================

summarize_futures_coverage <- function(
    futures_data
) {
  
  futures_data |>
    group_by(
      Symbol
    ) |>
    summarise(
      BloombergSecurity =
        first(
          BloombergSecurity
        ),
      SourceFile =
        first(
          SourceFile
        ),
      Observations =
        n(),
      FirstTimestampET =
        min(
          StartTimestampET,
          na.rm = TRUE
        ),
      LastTimestampET =
        max(
          EndTimestampET,
          na.rm = TRUE
        ),
      FirstTimestampCT =
        min(
          StartTimestampCT,
          na.rm = TRUE
        ),
      LastTimestampCT =
        max(
          EndTimestampCT,
          na.rm = TRUE
        ),
      CalendarDays =
        as.integer(
          as.Date(
            max(
              EndTimestampET,
              na.rm = TRUE
            ),
            tz = BLOOMBERG_TIME_ZONE
          ) -
            as.Date(
              min(
                StartTimestampET,
                na.rm = TRUE
              ),
              tz = BLOOMBERG_TIME_ZONE
            )
        ) +
        1L,
      .groups = "drop"
    )
}


# ============================================================
# 35. Summarize missing values
# ============================================================

summarize_futures_missing <- function(
    futures_data
) {
  
  futures_data |>
    group_by(
      Symbol
    ) |>
    summarise(
      Observations = n(),
      MissingClose =
        sum(
          is.na(
            Close
          )
        ),
      MissingOpen =
        sum(
          is.na(
            Open
          )
        ),
      MissingHigh =
        sum(
          is.na(
            High
          )
        ),
      MissingLow =
        sum(
          is.na(
            Low
          )
        ),
      MissingNetChg =
        sum(
          is.na(
            NetChg
          )
        ),
      MissingTickCount =
        sum(
          is.na(
            TickCount
          )
        ),
      MissingVolume =
        sum(
          is.na(
            Volume
          )
        ),
      .groups = "drop"
    )
}


# ============================================================
# 36. Summarize nominal interval lengths
# ============================================================

summarize_futures_interval_lengths <- function(
    futures_data
) {
  
  futures_data |>
    count(
      Symbol,
      NominalIntervalMinutes,
      name = "Observations"
    ) |>
    arrange(
      Symbol,
      NominalIntervalMinutes
    )
}


# ============================================================
# 37. Add gap diagnostics
# ============================================================

add_futures_gap_diagnostics <- function(
    futures_data
) {
  
  futures_data |>
    arrange(
      Symbol,
      StartTimestampET,
      SourceRow
    ) |>
    group_by(
      Symbol
    ) |>
    mutate(
      PreviousStartTimestampET =
        lag(
          StartTimestampET
        ),
      PreviousEndTimestampET =
        lag(
          EndTimestampET
        ),
      MinutesSincePreviousStart =
        as.numeric(
          difftime(
            StartTimestampET,
            PreviousStartTimestampET,
            units = "mins"
          )
        ),
      MinutesSincePreviousEnd =
        as.numeric(
          difftime(
            StartTimestampET,
            PreviousEndTimestampET,
            units = "mins"
          )
        )
    ) |>
    ungroup()
}


# ============================================================
# 38. Summarize gaps between available Bloomberg bars
# ============================================================

summarize_futures_gaps <- function(
    futures_data
) {
  
  gap_data <- add_futures_gap_diagnostics(
    futures_data
  )
  
  gap_data |>
    group_by(
      Symbol
    ) |>
    summarise(
      Observations =
        n(),
      MedianMinutesBetweenStarts =
        median(
          MinutesSincePreviousStart,
          na.rm = TRUE
        ),
      MeanMinutesBetweenStarts =
        mean(
          MinutesSincePreviousStart,
          na.rm = TRUE
        ),
      MaximumMinutesBetweenStarts =
        max(
          MinutesSincePreviousStart,
          na.rm = TRUE
        ),
      GapsOver30Minutes =
        sum(
          MinutesSincePreviousStart > 30,
          na.rm = TRUE
        ),
      GapsOver60Minutes =
        sum(
          MinutesSincePreviousStart > 60,
          na.rm = TRUE
        ),
      .groups = "drop"
    )
}


# ============================================================
# 39. Identify duplicate local interval starts
# ============================================================

find_futures_duplicate_local_starts <- function(
    futures_data
) {
  
  futures_data |>
    mutate(
      LocalStartKey =
        paste(
          RawDate,
          StartTime
        )
    ) |>
    count(
      Symbol,
      LocalStartKey,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    ) |>
    arrange(
      Symbol,
      LocalStartKey
    )
}


# ============================================================
# 40. Identify duplicate parsed ET timestamps
# ============================================================

find_futures_duplicate_et_timestamps <- function(
    futures_data
) {
  
  futures_data |>
    count(
      Symbol,
      StartTimestampET,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    ) |>
    arrange(
      Symbol,
      StartTimestampET
    )
}


# ============================================================
# 41. Identify abnormal nominal interval lengths
# ============================================================

find_abnormal_futures_intervals <- function(
    futures_data,
    expected_minutes = 30
) {
  
  futures_data |>
    filter(
      is.na(
        NominalIntervalMinutes
      ) |
        NominalIntervalMinutes != expected_minutes
    ) |>
    select(
      Symbol,
      SourceFile,
      SourceRow,
      RawDate,
      RawTimeInterval,
      NominalIntervalMinutes,
      ActualIntervalMinutes,
      StartTimestampET,
      EndTimestampET,
      StartTimestampCT,
      EndTimestampCT
    ) |>
    arrange(
      Symbol,
      StartTimestampET
    )
}


# ============================================================
# 42. Identify missing Close values
# ============================================================

find_missing_futures_closes <- function(
    futures_data
) {
  
  futures_data |>
    filter(
      is.na(
        Close
      )
    ) |>
    select(
      Symbol,
      SourceFile,
      SourceRow,
      RawDate,
      RawTimeInterval,
      StartTimestampET,
      StartTimestampCT,
      Close
    ) |>
    arrange(
      Symbol,
      StartTimestampET
    )
}


# ============================================================
# 43. Check OHLC consistency
# ============================================================

find_futures_ohlc_inconsistencies <- function(
    futures_data,
    tolerance = 1e-10
) {
  
  futures_data |>
    mutate(
      HighBelowLow =
        !is.na(
          High
        ) &
        !is.na(
          Low
        ) &
        High <
        Low -
        tolerance,
      
      OpenAboveHigh =
        !is.na(
          Open
        ) &
        !is.na(
          High
        ) &
        Open >
        High +
        tolerance,
      
      OpenBelowLow =
        !is.na(
          Open
        ) &
        !is.na(
          Low
        ) &
        Open <
        Low -
        tolerance,
      
      CloseAboveHigh =
        !is.na(
          Close
        ) &
        !is.na(
          High
        ) &
        Close >
        High +
        tolerance,
      
      CloseBelowLow =
        !is.na(
          Close
        ) &
        !is.na(
          Low
        ) &
        Close <
        Low -
        tolerance
    ) |>
    filter(
      HighBelowLow |
        OpenAboveHigh |
        OpenBelowLow |
        CloseAboveHigh |
        CloseBelowLow
    ) |>
    select(
      Symbol,
      SourceFile,
      SourceRow,
      StartTimestampET,
      Open,
      High,
      Low,
      Close,
      HighBelowLow,
      OpenAboveHigh,
      OpenBelowLow,
      CloseAboveHigh,
      CloseBelowLow
    ) |>
    arrange(
      Symbol,
      StartTimestampET
    )
}


# ============================================================
# 44. Validate all parsed futures
# ============================================================

validate_all_futures_git <- function(
    futures_list
) {
  
  if (!is.list(futures_list)) {
    stop(
      "futures_list must be a list.",
      call. = FALSE
    )
  }
  
  missing_symbols <- setdiff(
    FUTURES_SYMBOLS,
    names(futures_list)
  )
  
  if (length(missing_symbols) > 0L) {
    stop(
      paste0(
        "futures_list is missing symbols: ",
        paste(
          missing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  for (symbol in FUTURES_SYMBOLS) {
    
    validate_parsed_futures_data(
      data = futures_list[[
        symbol
      ]],
      symbol = symbol
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# Historical generic-futures roll mapping helpers
# ============================================================

resolve_futures_roll_paths <- function(
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  raw_dir <- as.character(
    raw_dir
  )
  
  tibble(
    Symbol = FUTURES_SYMBOLS,
    FileName =
      unname(
        FUTURES_ROLL_FILE_MAP[
          FUTURES_SYMBOLS
        ]
      ),
    Path =
      file.path(
        raw_dir,
        unname(
          FUTURES_ROLL_FILE_MAP[
            FUTURES_SYMBOLS
          ]
        )
      )
  )
}


check_futures_roll_files_exist <- function(
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  path_table <- resolve_futures_roll_paths(
    raw_dir = raw_dir
  )
  
  path_table <- path_table |>
    mutate(
      Exists = file.exists(
        Path
      )
    )
  
  if (!all(path_table$Exists)) {
    
    missing_files <- path_table |>
      filter(
        !Exists
      )
    
    stop(
      paste0(
        "The following futures roll-mapping files are missing:\n",
        paste(
          paste0(
            missing_files$Symbol,
            ": ",
            missing_files$Path
          ),
          collapse = "\n"
        )
      ),
      call. = FALSE
    )
  }
  
  path_table
}


read_futures_roll_mapping <- function(
    symbol,
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  validate_futures_symbol(
    symbol
  )
  
  file_name <- unname(
    FUTURES_ROLL_FILE_MAP[[symbol]]
  )
  
  path <- file.path(
    as.character(
      raw_dir
    ),
    file_name
  )
  
  if (!file.exists(path)) {
    stop(
      paste0(
        "Futures roll-mapping file does not exist:\n",
        path
      ),
      call. = FALSE
    )
  }
  
  roll_data <- readr::read_csv(
    path,
    show_col_types = FALSE,
    name_repair = "minimal",
    trim_ws = TRUE
  )
  
  names(
    roll_data
  ) <- trimws(
    names(
      roll_data
    )
  )
  
  required_columns <- c(
    "Date",
    "UnderlyingContract"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(
      roll_data
    )
  )
  
  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      paste0(
        "Roll-mapping file for ",
        symbol,
        " is missing required columns: ",
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
      roll_data
    ) == 0L
  ) {
    stop(
      paste0(
        "Roll-mapping file for ",
        symbol,
        " contains zero rows."
      ),
      call. = FALSE
    )
  }
  
  parsed_dates <- suppressWarnings(
    lubridate::mdy(
      as.character(
        roll_data[["Date"]]
      ),
      quiet = TRUE
    )
  )
  
  if (
    any(
      is.na(
        parsed_dates
      )
    )
  ) {
    
    bad_dates <- unique(
      as.character(
        roll_data[["Date"]][
          is.na(
            parsed_dates
          )
        ]
      )
    )
    
    stop(
      paste0(
        "Could not parse one or more roll-mapping dates for ",
        symbol,
        ": ",
        paste(
          bad_dates,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  underlying_contract <- trimws(
    as.character(
      roll_data[["UnderlyingContract"]]
    )
  )
  
  underlying_contract[
    underlying_contract == ""
  ] <- NA_character_
  
  if (
    any(
      is.na(
        underlying_contract
      )
    )
  ) {
    stop(
      paste0(
        "UnderlyingContract contains missing or blank values for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  result <- tibble(
    Symbol = symbol,
    RollMappingSourceFile = basename(
      path
    ),
    Date = as.Date(
      parsed_dates
    ),
    UnderlyingContract =
      underlying_contract
  ) |>
    arrange(
      Date
    )
  
  duplicate_dates <- result |>
    count(
      Date,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    )
  
  if (
    nrow(
      duplicate_dates
    ) > 0L
  ) {
    stop(
      paste0(
        "Duplicate roll-mapping dates were found for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  result <- result |>
    mutate(
      PreviousUnderlyingContract =
        lag(
          UnderlyingContract
        ),
      ContractChanged =
        !is.na(
          PreviousUnderlyingContract
        ) &
        UnderlyingContract !=
        PreviousUnderlyingContract
    )
  
  result
}


load_all_futures_roll_mappings <- function(
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  check_futures_roll_files_exist(
    raw_dir = raw_dir
  )
  
  result <- vector(
    mode = "list",
    length = length(
      FUTURES_SYMBOLS
    )
  )
  
  names(
    result
  ) <- FUTURES_SYMBOLS
  
  for (symbol in FUTURES_SYMBOLS) {
    
    result[[symbol]] <- read_futures_roll_mapping(
      symbol = symbol,
      raw_dir = raw_dir
    )
  }
  
  result
}


bind_all_futures_roll_mappings <- function(
    roll_mapping_list
) {
  
  if (
    !is.list(
      roll_mapping_list
    )
  ) {
    stop(
      "roll_mapping_list must be a list.",
      call. = FALSE
    )
  }
  
  missing_symbols <- setdiff(
    FUTURES_SYMBOLS,
    names(
      roll_mapping_list
    )
  )
  
  if (
    length(
      missing_symbols
    ) > 0L
  ) {
    stop(
      paste0(
        "roll_mapping_list is missing symbols: ",
        paste(
          missing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  bind_rows(
    roll_mapping_list[
      FUTURES_SYMBOLS
    ]
  ) |>
    mutate(
      Symbol = factor(
        Symbol,
        levels = FUTURES_SYMBOLS
      )
    ) |>
    arrange(
      Date,
      Symbol
    ) |>
    mutate(
      Symbol = as.character(
        Symbol
      )
    )
}


summarize_futures_roll_mappings <- function(
    roll_mapping_data
) {
  
  roll_mapping_data |>
    group_by(
      Symbol
    ) |>
    summarise(
      Observations = n(),
      FirstMappingDate = min(
        Date
      ),
      LastMappingDate = max(
        Date
      ),
      UniqueContracts = n_distinct(
        UnderlyingContract
      ),
      ContractChanges = sum(
        ContractChanged
      ),
      .groups = "drop"
    )
}


find_futures_roll_changes <- function(
    roll_mapping_data
) {
  
  roll_mapping_data |>
    filter(
      ContractChanged
    ) |>
    select(
      Symbol,
      Date,
      PreviousUnderlyingContract,
      UnderlyingContract,
      RollMappingSourceFile
    ) |>
    arrange(
      Date,
      Symbol
    )
}


validate_futures_roll_mappings <- function(
    roll_mapping_data
) {
  
  required_columns <- c(
    "Symbol",
    "RollMappingSourceFile",
    "Date",
    "UnderlyingContract",
    "PreviousUnderlyingContract",
    "ContractChanged"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(
      roll_mapping_data
    )
  )
  
  if (
    length(
      missing_columns
    ) > 0L
  ) {
    stop(
      paste0(
        "Combined roll mapping is missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  missing_symbols <- setdiff(
    FUTURES_SYMBOLS,
    unique(
      roll_mapping_data$Symbol
    )
  )
  
  if (
    length(
      missing_symbols
    ) > 0L
  ) {
    stop(
      paste0(
        "Combined roll mapping is missing symbols: ",
        paste(
          missing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(
        roll_mapping_data$Date
      )
    )
  ) {
    stop(
      "Combined roll mapping contains missing Date values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      is.na(
        roll_mapping_data$UnderlyingContract
      )
    )
  ) {
    stop(
      "Combined roll mapping contains missing UnderlyingContract values.",
      call. = FALSE
    )
  }
  
  duplicate_dates <- roll_mapping_data |>
    count(
      Symbol,
      Date,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    )
  
  if (
    nrow(
      duplicate_dates
    ) > 0L
  ) {
    stop(
      "Combined roll mapping contains duplicate Symbol-Date rows.",
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 45. Prepare the complete raw futures data object
# ============================================================

prepare_futures_raw_data <- function(
    raw_dir = DEFAULT_FUTURES_RAW_DIR
) {
  
  file_table <- check_futures_raw_files_exist(
    raw_dir = raw_dir
  )
  
  futures_list <- load_all_futures_git(
    raw_dir = raw_dir
  )
  
  validate_all_futures_git(
    futures_list
  )
  
  combined_data <- bind_all_futures_git(
    futures_list
  )
  
  file_structure <- summarize_futures_file_structure(
    futures_list
  )
  
  coverage <- summarize_futures_coverage(
    combined_data
  )
  
  missing <- summarize_futures_missing(
    combined_data
  )
  
  interval_lengths <- summarize_futures_interval_lengths(
    combined_data
  )
  
  gaps <- summarize_futures_gaps(
    combined_data
  )
  
  duplicate_local_starts <-
    find_futures_duplicate_local_starts(
      combined_data
    )
  
  duplicate_et_timestamps <-
    find_futures_duplicate_et_timestamps(
      combined_data
    )
  
  abnormal_intervals <-
    find_abnormal_futures_intervals(
      combined_data
    )
  
  missing_closes <-
    find_missing_futures_closes(
      combined_data
    )
  
  ohlc_inconsistencies <-
    find_futures_ohlc_inconsistencies(
      combined_data
    )
  
  roll_file_table <- check_futures_roll_files_exist(
    raw_dir = raw_dir
  )
  
  roll_mapping_list <- load_all_futures_roll_mappings(
    raw_dir = raw_dir
  )
  
  roll_mapping <- bind_all_futures_roll_mappings(
    roll_mapping_list
  )
  
  validate_futures_roll_mappings(
    roll_mapping
  )
  
  roll_summary <- summarize_futures_roll_mappings(
    roll_mapping
  )
  
  roll_changes <- find_futures_roll_changes(
    roll_mapping
  )
  
  list(
    files = file_table,
    assets = futures_list,
    data = combined_data,
    file_structure = file_structure,
    coverage = coverage,
    missing = missing,
    interval_lengths = interval_lengths,
    gaps = gaps,
    duplicate_local_starts =
      duplicate_local_starts,
    duplicate_et_timestamps =
      duplicate_et_timestamps,
    abnormal_intervals =
      abnormal_intervals,
    missing_closes =
      missing_closes,
    ohlc_inconsistencies =
      ohlc_inconsistencies,
    roll_files =
      roll_file_table,
    roll_assets =
      roll_mapping_list,
    roll_mapping =
      roll_mapping,
    roll_summary =
      roll_summary,
    roll_changes =
      roll_changes
  )
} 


# ============================================================
# 46. End of futures_data.R
# ============================================================