# ============================================================
# scripts/live/build_futures_history.R
# ============================================================
#
# Purpose:
#
#   Build the canonical, append-only Bloomberg futures price
#   history used by the live production pipeline.
#
#   The output is:
#
#     data/live_inputs/futures_history.csv
#
#   The file contains price-level observations for the eight
#   optimizer futures:
#
#     CL, GC, SR3, ZN, KE, HO, ZW, NQ
#
#   This script combines:
#
#     1. The original historical Bloomberg GIT files.
#     2. New Bloomberg GIT update files.
#
#   It reuses the frozen Bloomberg parsing conventions in:
#
#     scripts/futures_data.R
#
#   In particular:
#
#     - Bloomberg timestamps are interpreted in America/New_York.
#     - Competition timestamps are converted to America/Chicago.
#     - Bloomberg Close is stored as the authoritative price.
#     - No returns are calculated here.
#     - No missing bars are fabricated.
#     - No generic-contract roll returns are calculated here.
#
#   The builder is idempotent:
#
#     Re-running it with the same source files must not create
#     duplicate canonical observations.
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
# 2. Source frozen Bloomberg parsing utilities
# ============================================================

source(
  here(
    "scripts",
    "futures_data.R"
  )
)


# ============================================================
# 3. Canonical symbols
# ============================================================

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


if (!identical(LIVE_FUTURES_SYMBOLS, FUTURES_SYMBOLS)) {
  stop(
    paste0(
      "LIVE_FUTURES_SYMBOLS does not match FUTURES_SYMBOLS ",
      "from scripts/futures_data.R."
    ),
    call. = FALSE
  )
}


# ============================================================
# 4. Directories
# ============================================================

RAW_DIR <- here(
  "data",
  "raw"
)

LIVE_INPUT_DIR <- here(
  "data",
  "live_inputs"
)

OUTPUT_PATH <- file.path(
  LIVE_INPUT_DIR,
  "futures_history.csv"
)

EXISTING_HISTORY_EXISTS <- file.exists(
  OUTPUT_PATH
)


if (!dir.exists(RAW_DIR)) {
  stop(
    paste0(
      "Raw-data directory does not exist:\n",
      RAW_DIR
    ),
    call. = FALSE
  )
}


if (!dir.exists(LIVE_INPUT_DIR)) {
  dir.create(
    LIVE_INPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 5. Original historical Bloomberg files
# ============================================================
#
# These are the frozen historical files already used by the
# futures modeling pipeline.
#
# Do not replace these mappings with the names of the new
# Bloomberg update files.
#
# ============================================================

HISTORICAL_FILE_MAP <- c(
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
# 6. New Bloomberg update files
# ============================================================
#
# These are the Bloomberg GIT exports obtained for the missing
# period after the original historical sample.
#
# IMPORTANT:
#
#   Future_5 remains named KE internally because KE is the
#   optimizer's canonical KC HRW Wheat symbol.
#
#   The new Bloomberg source file is KW_missing_data.csv.
#   This is intentional.
#
# If your actual local filenames differ, change ONLY the
# filename strings below.
#
# ============================================================

UPDATE_FILE_MAP <- c(
  CL  = "CL_missing_data.csv",
  GC  = "GC_missing_data.csv",
  SR3 = "SFR_missing_data.csv",
  ZN  = "TY_missing_data.csv",
  KE  = "KW_missing_data.csv",
  HO  = "HO_missing_data.csv",
  ZW  = "W_missing_data.csv",
  NQ  = "NQ_missing_data.csv"
)


# ============================================================
# 7. Bloomberg security labels for provenance
# ============================================================
#
# These labels describe the source represented by each update.
#
# They are provenance fields only. They do not determine the
# optimizer symbol or calculate returns.
#
# ============================================================

LIVE_BLOOMBERG_SECURITY_MAP <- c(
  CL  = "CL1 Comdty",
  GC  = "GC1 Comdty",
  SR3 = "SFR1 Comdty",
  ZN  = "TY1 Comdty",
  KE  = "KW1 COMB Comdty",
  HO  = "HO1 Comdty",
  ZW  = "W1 Comdty",
  NQ  = "NQ1 Index"
)


# ============================================================
# 8. Validate named mappings
# ============================================================

validate_named_map <- function(x, map_name) {
  
  if (!is.character(x)) {
    stop(
      paste0(
        map_name,
        " must be a character vector."
      ),
      call. = FALSE
    )
  }
  
  if (is.null(names(x))) {
    stop(
      paste0(
        map_name,
        " must be a named character vector."
      ),
      call. = FALSE
    )
  }
  
  missing_symbols <- setdiff(
    LIVE_FUTURES_SYMBOLS,
    names(x)
  )
  
  extra_symbols <- setdiff(
    names(x),
    LIVE_FUTURES_SYMBOLS
  )
  
  if (length(missing_symbols) > 0L) {
    stop(
      paste0(
        map_name,
        " is missing symbols: ",
        paste(
          missing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (length(extra_symbols) > 0L) {
    stop(
      paste0(
        map_name,
        " contains unsupported symbols: ",
        paste(
          extra_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (any(is.na(x)) || any(trimws(x) == "")) {
    stop(
      paste0(
        map_name,
        " contains missing or blank values."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


validate_named_map(
  HISTORICAL_FILE_MAP,
  "HISTORICAL_FILE_MAP"
)

validate_named_map(
  UPDATE_FILE_MAP,
  "UPDATE_FILE_MAP"
)

validate_named_map(
  LIVE_BLOOMBERG_SECURITY_MAP,
  "LIVE_BLOOMBERG_SECURITY_MAP"
)


# ============================================================
# 9. Resolve and validate source files
# ============================================================

resolve_source_paths <- function(file_map, source_type) {
  
  result <- tibble(
    Symbol = LIVE_FUTURES_SYMBOLS,
    SourceType = source_type,
    FileName = unname(
      file_map[LIVE_FUTURES_SYMBOLS]
    ),
    Path = file.path(
      RAW_DIR,
      unname(
        file_map[LIVE_FUTURES_SYMBOLS]
      )
    )
  )
  
  result <- result |>
    mutate(
      Exists = file.exists(Path)
    )
  
  if (!all(result$Exists)) {
    
    missing_files <- result |>
      filter(!Exists)
    
    stop(
      paste0(
        "The following ",
        source_type,
        " Bloomberg source files are missing:\n",
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
  
  result
}


historical_paths <- resolve_source_paths(
  HISTORICAL_FILE_MAP,
  "Historical"
)

update_paths <- resolve_source_paths(
  UPDATE_FILE_MAP,
  "Update"
)


# ============================================================
# 10. Parse one Bloomberg source file
# ============================================================
#
# This deliberately calls the parsing functions from
# scripts/futures_data.R instead of reimplementing Bloomberg
# date/time parsing.
#
# ============================================================

parse_source_file <- function(
    symbol,
    path,
    source_type,
    security_label
) {
  
  raw_data <- read_bloomberg_git_raw(
    path = path
  )
  
  parsed <- parse_bloomberg_git_data(
    raw_data = raw_data,
    symbol = symbol,
    source_file = path
  )
  
  validate_parsed_futures_data(
    data = parsed,
    symbol = symbol
  )
  
  parsed |>
    mutate(
      BloombergSecurity = security_label,
      SourceType = source_type
    )
}


# ============================================================
# 11. Load existing canonical history or bootstrap history
# ============================================================
#
# First run:
#
#   If futures_history.csv does not yet exist, bootstrap the
#   canonical history from the original frozen Bloomberg GIT
#   files.
#
# Later runs:
#
#   If futures_history.csv already exists, use that file as
#   the starting canonical history. This allows each new set
#   of *_missing_data.csv files to incrementally extend the
#   persistent history without rebuilding from scratch.
#
# ============================================================

if (EXISTING_HISTORY_EXISTS) {
  
  cat(
    "\nExisting futures_history.csv found.\n"
  )
  
  cat(
    "Using it as the starting canonical price history.\n\n"
  )
  
  historical_data <- readr::read_csv(
    OUTPUT_PATH,
    show_col_types = FALSE
  )
  
  required_existing_columns <- c(
    "Symbol",
    "BloombergSecurity",
    "SourceType",
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
  
  missing_existing_columns <- setdiff(
    required_existing_columns,
    names(historical_data)
  )
  
  if (length(missing_existing_columns) > 0L) {
    stop(
      paste0(
        "Existing futures_history.csv is missing required columns: ",
        paste(
          missing_existing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  historical_data <- historical_data |>
    mutate(
      StartTime = as.character(
        StartTime
      ),
      EndTime = as.character(
        EndTime
      ),
      StartTimeCT = as.character(
        StartTimeCT
      ),
      EndTimeCT = as.character(
        EndTimeCT
      ),
      StartTimestampET = as.POSIXct(
        StartTimestampET,
        tz = "America/New_York"
      ),
      EndTimestampET = as.POSIXct(
        EndTimestampET,
        tz = "America/New_York"
      ),
      StartTimestampCT = as.POSIXct(
        StartTimestampCT,
        tz = "America/Chicago"
      ),
      EndTimestampCT = as.POSIXct(
        EndTimestampCT,
        tz = "America/Chicago"
      ),
      RawDate = as.Date(
        RawDate
      ),
      StartDateCT = as.Date(
        StartDateCT
      ),
      EndDateCT = as.Date(
        EndDateCT
      )
    )
  
  missing_existing_symbols <- setdiff(
    LIVE_FUTURES_SYMBOLS,
    unique(
      historical_data$Symbol
    )
  )
  
  if (length(missing_existing_symbols) > 0L) {
    stop(
      paste0(
        "Existing futures_history.csv is missing symbols: ",
        paste(
          missing_existing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  existing_duplicate_keys <- historical_data |>
    count(
      Symbol,
      EndTimestampET,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    )
  
  if (nrow(existing_duplicate_keys) > 0L) {
    stop(
      paste0(
        "Existing futures_history.csv contains duplicate ",
        "Symbol/EndTimestampET observations."
      ),
      call. = FALSE
    )
  }
  
} else {
  
  cat(
    "\nNo existing futures_history.csv found.\n"
  )
  
  cat(
    "Bootstrapping from the original historical Bloomberg files.\n\n"
  )
  
  historical_list <- vector(
    mode = "list",
    length = length(LIVE_FUTURES_SYMBOLS)
  )
  
  names(historical_list) <- LIVE_FUTURES_SYMBOLS
  
  for (symbol in LIVE_FUTURES_SYMBOLS) {
    
    path <- historical_paths$Path[
      historical_paths$Symbol == symbol
    ]
    
    historical_list[[symbol]] <- parse_source_file(
      symbol = symbol,
      path = path,
      source_type = "Historical",
      security_label = unname(
        FUTURES_BLOOMBERG_SECURITY_MAP[[symbol]]
      )
    )
  }
  
  historical_data <- bind_rows(
    historical_list[LIVE_FUTURES_SYMBOLS]
  )
}


# ============================================================
# 12. Load all new Bloomberg update data
# ============================================================

update_list <- vector(
  mode = "list",
  length = length(LIVE_FUTURES_SYMBOLS)
)

names(update_list) <- LIVE_FUTURES_SYMBOLS


for (symbol in LIVE_FUTURES_SYMBOLS) {
  
  path <- update_paths$Path[
    update_paths$Symbol == symbol
  ]
  
  update_list[[symbol]] <- parse_source_file(
    symbol = symbol,
    path = path,
    source_type = "Update",
    security_label = unname(
      LIVE_BLOOMBERG_SECURITY_MAP[[symbol]]
    )
  )
}


update_data <- bind_rows(
  update_list[LIVE_FUTURES_SYMBOLS]
)


# ============================================================
# 13. Keep canonical price-history fields
# ============================================================
#
# We preserve both ET and CT timestamps plus source provenance.
#
# Close is the authoritative price used later to construct
# returns.
#
# No return column is created here.
#
# ============================================================

select_history_fields <- function(data) {
  
  data |>
    select(
      Symbol,
      BloombergSecurity,
      SourceType,
      SourceFile,
      SourceRow,
      RawDateMarker,
      RawDate,
      RawTimeInterval,
      StartTime,
      EndTime,
      StartTimestampET,
      EndTimestampET,
      StartTimestampCT,
      EndTimestampCT,
      StartDateCT,
      StartTimeCT,
      EndDateCT,
      EndTimeCT,
      NominalIntervalMinutes,
      ActualIntervalMinutes,
      Close,
      NetChg,
      Open,
      High,
      Low,
      TickCount,
      Volume
    )
}


historical_data <- select_history_fields(
  historical_data
)

update_data <- select_history_fields(
  update_data
)


# ============================================================
# 14. Validate Close values
# ============================================================

validate_close_values <- function(data, source_name) {
  
  missing_close <- data |>
    filter(is.na(Close))
  
  if (nrow(missing_close) > 0L) {
    stop(
      paste0(
        source_name,
        " contains ",
        nrow(missing_close),
        " Bloomberg interval rows with missing Close values."
      ),
      call. = FALSE
    )
  }
  
  nonpositive_close <- data |>
    filter(!is.finite(Close) | Close <= 0)
  
  if (nrow(nonpositive_close) > 0L) {
    stop(
      paste0(
        source_name,
        " contains ",
        nrow(nonpositive_close),
        " non-finite or nonpositive Close values."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


validate_close_values(
  historical_data,
  "Historical Bloomberg data"
)

validate_close_values(
  update_data,
  "Bloomberg update data"
)


# ============================================================
# 15. Check overlap between historical and update files
# ============================================================
#
# Same Symbol + EndTimestampET identifies the same Bloomberg
# interval endpoint.
#
# Ordinary overlaps must have matching Close values.
#
# One narrow exception is allowed on later live runs:
#
#   The latest observation currently stored for each symbol may
#   have been captured while Bloomberg's terminal GIT interval
#   was still forming. If a later Bloomberg pull contains that
#   same terminal endpoint with a revised Close, the newer row
#   is allowed to supersede the stored terminal row.
#
# Revisions to any older historical endpoint remain forbidden.
#
# ============================================================

OVERLAP_TOLERANCE <- 1e-10


# ------------------------------------------------------------
# 15A. Identify the currently stored terminal endpoint
#     separately for each future.
# ------------------------------------------------------------

historical_terminal_endpoints <- historical_data |>
  group_by(
    Symbol
  ) |>
  summarise(
    HistoricalTerminalEndTimestampET = max(
      EndTimestampET
    ),
    .groups = "drop"
  )


# ------------------------------------------------------------
# 15B. Compare all historical/update overlaps.
# ------------------------------------------------------------

overlap_check <- historical_data |>
  select(
    Symbol,
    EndTimestampET,
    HistoricalClose = Close
  ) |>
  inner_join(
    update_data |>
      select(
        Symbol,
        EndTimestampET,
        UpdateClose = Close
      ),
    by = c(
      "Symbol",
      "EndTimestampET"
    )
  ) |>
  left_join(
    historical_terminal_endpoints,
    by = "Symbol"
  ) |>
  mutate(
    AbsoluteDifference = abs(
      HistoricalClose - UpdateClose
    ),
    Scale = pmax(
      1,
      abs(HistoricalClose),
      abs(UpdateClose)
    ),
    RelativeDifference = AbsoluteDifference / Scale,
    Matches = RelativeDifference <= OVERLAP_TOLERANCE,
    IsHistoricalTerminal = (
      EndTimestampET == HistoricalTerminalEndTimestampET
    ),
    IsRecentHistoricalTail = (
      EndTimestampET >=
        HistoricalTerminalEndTimestampET - minutes(30)
    ),
    AllowedTerminalRevision = (
      !Matches &
        IsRecentHistoricalTail
    )
  )


# ------------------------------------------------------------
# 15C. Any conflicting NON-terminal overlap is still fatal.
# ------------------------------------------------------------

overlap_conflicts <- overlap_check |>
  filter(
    !Matches,
    !AllowedTerminalRevision
  )


if (nrow(overlap_conflicts) > 0L) {
  
  print(
    overlap_conflicts
  )
  
  stop(
    paste0(
      "Historical and update Bloomberg files contain conflicting ",
      "Close values at a non-terminal Symbol/EndTimestampET. ",
      "Only observations in the most recent 30-minute historical ",
      "tail may be revised by a later Bloomberg pull. ",
      "See the printed overlap conflicts above."
    ),
    call. = FALSE
  )
}


# ------------------------------------------------------------
# 15D. Record permitted terminal revisions for diagnostics.
# ------------------------------------------------------------

terminal_revisions <- overlap_check |>
  filter(
    AllowedTerminalRevision
  )


if (nrow(terminal_revisions) > 0L) {
  
  cat(
    "\nPermitted Bloomberg terminal-bar revisions:\n\n"
  )
  
  print(
    terminal_revisions
  )
}


# ============================================================
# 16. Combine historical and update observations
# ============================================================
#
# Existing observations normally have precedence.
#
# The only exception is a permitted revision of the previously
# stored terminal observation for a symbol. At that endpoint,
# the newer Bloomberg update receives precedence.
#
# ============================================================

terminal_revision_keys <- terminal_revisions |>
  select(
    Symbol,
    EndTimestampET
  ) |>
  distinct()


historical_for_combination <- historical_data |>
  anti_join(
    terminal_revision_keys,
    by = c(
      "Symbol",
      "EndTimestampET"
    )
  ) |>
  mutate(
    InputPriority = 1L
  )


update_for_combination <- update_data |>
  mutate(
    InputPriority = 2L
  )


combined_data <- bind_rows(
  historical_for_combination,
  update_for_combination
) |>
  arrange(
    Symbol,
    EndTimestampET,
    InputPriority,
    SourceFile,
    SourceRow
  )


# ============================================================
# 17. Detect duplicate canonical timestamps
# ============================================================
#
# After permitted terminal revisions have removed the stale
# stored terminal row, any remaining duplicate timestamp must
# have matching Close values.
#
# ============================================================

duplicate_timestamp_summary <- combined_data |>
  group_by(
    Symbol,
    EndTimestampET
  ) |>
  summarise(
    Occurrences = n(),
    DistinctCloseValues = n_distinct(
      Close
    ),
    MinimumClose = min(
      Close
    ),
    MaximumClose = max(
      Close
    ),
    .groups = "drop"
  ) |>
  filter(
    Occurrences > 1L
  )


duplicate_conflicts <- duplicate_timestamp_summary |>
  mutate(
    AbsoluteDifference = abs(
      MaximumClose - MinimumClose
    ),
    Scale = pmax(
      1,
      abs(MinimumClose),
      abs(MaximumClose)
    ),
    RelativeDifference = AbsoluteDifference / Scale
  ) |>
  filter(
    RelativeDifference > OVERLAP_TOLERANCE
  )


if (nrow(duplicate_conflicts) > 0L) {
  
  print(
    duplicate_conflicts
  )
  
  stop(
    paste0(
      "Conflicting duplicate Bloomberg observations were found ",
      "after permitted terminal revisions were handled. ",
      "The canonical history was not written."
    ),
    call. = FALSE
  )
}


# ============================================================
# 18. Deduplicate canonical observations
# ============================================================
#
# Exact historical/update overlaps retain the existing row.
#
# A permitted terminal revision has already removed the stale
# historical row, so the new Bloomberg row is retained there.
#
# ============================================================

canonical_history <- combined_data |>
  group_by(
    Symbol,
    EndTimestampET
  ) |>
  slice_head(
    n = 1L
  ) |>
  ungroup() |>
  select(
    -InputPriority
  )


# ============================================================
# 19. Canonical chronological ordering
# ============================================================

canonical_history <- canonical_history |>
  mutate(
    Symbol = factor(
      Symbol,
      levels = LIVE_FUTURES_SYMBOLS
    )
  ) |>
  arrange(
    EndTimestampET,
    Symbol
  ) |>
  mutate(
    Symbol = as.character(
      Symbol
    )
  )


# ============================================================
# 20. Final structural validation
# ============================================================

missing_symbols <- setdiff(
  LIVE_FUTURES_SYMBOLS,
  unique(
    canonical_history$Symbol
  )
)


if (length(missing_symbols) > 0L) {
  stop(
    paste0(
      "Canonical futures history is missing symbols: ",
      paste(
        missing_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


final_duplicates <- canonical_history |>
  count(
    Symbol,
    EndTimestampET,
    name = "Occurrences"
  ) |>
  filter(
    Occurrences > 1L
  )


if (nrow(final_duplicates) > 0L) {
  stop(
    "Canonical futures history still contains duplicate timestamps.",
    call. = FALSE
  )
}


if (
  any(is.na(canonical_history$EndTimestampET)) ||
  any(is.na(canonical_history$EndTimestampCT))
) {
  stop(
    "Canonical futures history contains missing timestamps.",
    call. = FALSE
  )
}


if (
  any(is.na(canonical_history$Close)) ||
  any(!is.finite(canonical_history$Close)) ||
  any(canonical_history$Close <= 0)
) {
  stop(
    "Canonical futures history contains invalid Close values.",
    call. = FALSE
  )
}


# ============================================================
# 21. Coverage summary
# ============================================================

coverage_summary <- canonical_history |>
  group_by(
    Symbol
  ) |>
  summarise(
    Observations = n(),
    FirstTimestampCT = min(
      EndTimestampCT
    ),
    LastTimestampCT = max(
      EndTimestampCT
    ),
    HistoricalRowsRetained = sum(
      SourceType == "Historical"
    ),
    UpdateRowsRetained = sum(
      SourceType == "Update"
    ),
    .groups = "drop"
  )


# ============================================================
# 22. Update-file contribution summary
# ============================================================

update_contribution <- canonical_history |>
  filter(
    SourceType == "Update"
  ) |>
  group_by(
    Symbol
  ) |>
  summarise(
    NewRows = n(),
    FirstNewTimestampCT = min(
      EndTimestampCT
    ),
    LastNewTimestampCT = max(
      EndTimestampCT
    ),
    .groups = "drop"
  )


# ============================================================
# 23. Print validation diagnostics
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FUTURES HISTORY BUILD VALIDATION\n"
)

cat(
  "============================================================\n\n"
)


cat(
  "Historical rows parsed: ",
  nrow(historical_data),
  "\n",
  sep = ""
)

cat(
  "Update rows parsed:     ",
  nrow(update_data),
  "\n",
  sep = ""
)

cat(
  "Historical/update overlapping timestamps: ",
  nrow(overlap_check),
  "\n",
  sep = ""
)

cat(
  "Conflicting non-terminal overlaps: ",
  nrow(overlap_conflicts),
  "\n",
  sep = ""
)

cat(
  "Permitted terminal revisions: ",
  nrow(terminal_revisions),
  "\n",
  sep = ""
)

cat(
  "Duplicate timestamps before deduplication: ",
  nrow(duplicate_timestamp_summary),
  "\n",
  sep = ""
)

cat(
  "Conflicting duplicate timestamps: ",
  nrow(duplicate_conflicts),
  "\n",
  sep = ""
)

cat(
  "Canonical rows after deduplication: ",
  nrow(canonical_history),
  "\n\n",
  sep = ""
)


cat(
  "Canonical coverage by future:\n\n"
)

print(
  coverage_summary,
  n = Inf
)


cat(
  "\nUpdate observations retained by future:\n\n"
)

print(
  update_contribution,
  n = Inf
)


# ============================================================
# 24. Write futures_history.csv
# ============================================================
#
# IMPORTANT:
#
# This output contains PRICE LEVELS.
#
# It does not contain:
#
#   - log returns;
#   - arithmetic returns;
#   - optimizer mu;
#   - optimizer Sigma;
#   - leverage;
#   - margin weights.
#
# Those transformations belong downstream.
#
# ============================================================

readr::write_csv(
  canonical_history,
  OUTPUT_PATH,
  na = ""
)


# ============================================================
# 25. Verify written file
# ============================================================

if (!file.exists(OUTPUT_PATH)) {
  stop(
    paste0(
      "futures_history.csv was not created:\n",
      OUTPUT_PATH
    ),
    call. = FALSE
  )
}


written_history <- readr::read_csv(
  OUTPUT_PATH,
  show_col_types = FALSE
)


if (nrow(written_history) != nrow(canonical_history)) {
  stop(
    paste0(
      "Written futures_history.csv row count does not match ",
      "the in-memory canonical history."
    ),
    call. = FALSE
  )
}


# ============================================================
# 26. Final success message
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FUTURES HISTORY BUILD PASSED\n"
)

cat(
  "============================================================\n\n"
)

cat(
  "Output:\n",
  OUTPUT_PATH,
  "\n\n",
  sep = ""
)

cat(
  "Rows written: ",
  nrow(canonical_history),
  "\n",
  sep = ""
)

cat(
  "Symbols: ",
  paste(
    LIVE_FUTURES_SYMBOLS,
    collapse = ", "
  ),
  "\n",
  sep = ""
)

cat(
  "\nThe canonical futures history contains Bloomberg price levels.\n"
)

cat(
  "No returns were calculated by this script.\n"
)

cat(
  "Post-historical generic-contract roll handling remains a\n"
)

cat(
  "separate downstream validation step before new returns are used.\n"
)

cat(
  "\n============================================================\n"
)