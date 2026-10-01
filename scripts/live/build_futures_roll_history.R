# ============================================================
# scripts/live/build_futures_roll_history.R
# ============================================================
#
# Purpose:
#
#   Build the canonical, append-only futures generic-contract
#   roll history used by the live production pipeline.
#
#   The output is:
#
#     data/live_inputs/futures_roll_history.csv
#
#   The file contains daily underlying-contract identities for:
#
#     CL, GC, SR3, ZN, KE, HO, ZW, NQ
#
#   First run:
#
#     1. Load the original frozen *_roll_mapping.csv files.
#     2. Load the latest Bloomberg *_missing_rolls.csv files.
#     3. Validate overlap.
#     4. Deduplicate.
#     5. Write futures_roll_history.csv.
#
#   Later runs:
#
#     1. Load the existing futures_roll_history.csv.
#     2. Load the latest Bloomberg *_missing_rolls.csv files.
#     3. Validate overlap.
#     4. Add genuinely new dates.
#     5. Rewrite the expanded futures_roll_history.csv.
#
#   IMPORTANT:
#
#     - This script does NOT calculate futures returns.
#     - This script does NOT modify futures_history.csv.
#     - This script does NOT fabricate missing business dates.
#     - Existing canonical observations take precedence over
#       overlapping update observations after validation.
#     - Conflicting overlapping contract identities stop the
#       script.
#     - The builder is idempotent.
#
#   Bloomberg Excel/BDH exports used for the update files may
#   store the first date and first contract as the two CSV
#   column names. This script explicitly recovers that first
#   observation.
#
#   Future_5 is named KE internally. The current Bloomberg
#   generic source uses root KW. Historical KE and live KW
#   contract labels are normalized to a common KE identity for
#   comparison while the raw source label is preserved.
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
# 2. Canonical symbols
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


# ============================================================
# 3. Directories
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
  "futures_roll_history.csv"
)

EXISTING_ROLL_HISTORY_EXISTS <- file.exists(
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
# 4. Original historical roll-mapping files
# ============================================================
#
# These are the frozen historical roll mappings already used
# by the historical futures-return pipeline.
#
# ============================================================

HISTORICAL_ROLL_FILE_MAP <- c(
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
# 5. Bloomberg roll-update files
# ============================================================
#
# These files contain Bloomberg FUT_CUR_GEN_TICKER history.
#
# Future_5 remains KE internally, but the Bloomberg source
# uses KW for KC HRW Wheat.
#
# ============================================================

UPDATE_ROLL_FILE_MAP <- c(
  CL  = "CL_missing_rolls.csv",
  GC  = "GC_missing_rolls.csv",
  SR3 = "SFR_missing_rolls.csv",
  ZN  = "TY_missing_rolls.csv",
  KE  = "KW_missing_rolls.csv",
  HO  = "HO_missing_rolls.csv",
  ZW  = "W_missing_rolls.csv",
  NQ  = "NQ_missing_rolls.csv"
)


# ============================================================
# 6. Bloomberg generic-security provenance labels
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
# 7. Validate named maps
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
  HISTORICAL_ROLL_FILE_MAP,
  "HISTORICAL_ROLL_FILE_MAP"
)

validate_named_map(
  UPDATE_ROLL_FILE_MAP,
  "UPDATE_ROLL_FILE_MAP"
)

validate_named_map(
  LIVE_BLOOMBERG_SECURITY_MAP,
  "LIVE_BLOOMBERG_SECURITY_MAP"
)


# ============================================================
# 8. Resolve and validate source files
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
      filter(
        !Exists
      )
    
    stop(
      paste0(
        "The following ",
        source_type,
        " roll files are missing:\n",
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
  HISTORICAL_ROLL_FILE_MAP,
  "Historical"
)

update_paths <- resolve_source_paths(
  UPDATE_ROLL_FILE_MAP,
  "Update"
)


# ============================================================
# 9. Contract-label cleaning
# ============================================================
#
# Remove whitespace and normalize capitalization before
# contract identities are compared.
#
# Examples:
#
#   "W U6" -> "WU6"
#   "CLV6" -> "CLV6"
#
# ============================================================

clean_contract_label <- function(x) {
  
  x <- as.character(
    x
  )
  
  x <- trimws(
    x
  )
  
  x <- toupper(
    x
  )
  
  x <- gsub(
    "[[:space:]]+",
    "",
    x
  )
  
  x
}


# ============================================================
# 10. Canonical contract identity
# ============================================================
#
# The canonical identity is used for overlap validation and
# roll detection.
#
# Most contracts retain their cleaned Bloomberg/historical
# label.
#
# KE is special:
#
#   Historical files may use KEU6.
#   Current Bloomberg data may use KWU6.
#
# Both refer to the same KC HRW Wheat contract month/year.
# Therefore KW is normalized to KE for the internal canonical
# contract identity.
#
# RawUnderlyingContract remains untouched for provenance.
#
# ============================================================

canonicalize_contract_identity <- function(symbol, raw_contract) {
  
  if (length(symbol) != length(raw_contract)) {
    stop(
      paste0(
        "symbol and raw_contract must have equal lengths in ",
        "canonicalize_contract_identity()."
      ),
      call. = FALSE
    )
  }
  
  cleaned <- clean_contract_label(
    raw_contract
  )
  
  result <- cleaned
  
  ke_rows <- symbol == "KE" & !is.na(cleaned)
  
  result[ke_rows] <- sub(
    "^KW",
    "KE",
    result[ke_rows]
  )
  
  result
}


# ============================================================
# 11. Parse dates safely
# ============================================================

parse_roll_date <- function(x) {
  
  x <- trimws(
    as.character(
      x
    )
  )
  
  parsed <- suppressWarnings(
    mdy(
      x,
      quiet = TRUE
    )
  )
  
  still_missing <- is.na(parsed)
  
  if (any(still_missing)) {
    parsed[still_missing] <- suppressWarnings(
      ymd(
        x[still_missing],
        quiet = TRUE
      )
    )
  }
  
  as.Date(
    parsed
  )
}


# ============================================================
# 12. Parse one original historical roll file
# ============================================================

parse_historical_roll_file <- function(symbol, path) {
  
  raw_data <- readr::read_csv(
    path,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character()
    )
  )
  
  required_columns <- c(
    "Date",
    "UnderlyingContract"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(raw_data)
  )
  
  if (length(missing_columns) > 0L) {
    stop(
      paste0(
        "Historical roll file for ",
        symbol,
        " is missing required columns: ",
        paste(
          missing_columns,
          collapse = ", "
        ),
        "\nFile: ",
        path
      ),
      call. = FALSE
    )
  }
  
  result <- raw_data |>
    transmute(
      Symbol = symbol,
      Date = parse_roll_date(
        Date
      ),
      RawUnderlyingContract = trimws(
        as.character(
          UnderlyingContract
        )
      ),
      SourceType = "Historical",
      SourceFile = basename(
        path
      ),
      SourceRow = row_number()
    )
  
  result <- result |>
    mutate(
      UnderlyingContract = canonicalize_contract_identity(
        Symbol,
        RawUnderlyingContract
      )
    )
  
  result
}


# ============================================================
# 13. Parse one Bloomberg update roll file
# ============================================================
#
# Bloomberg/Excel exports may have this physical CSV shape:
#
#   9/3/26,CLV6
#   9/4/26,CLV6
#   9/8/26,CLV6
#   ...
#
# When read normally, R interprets:
#
#   "9/3/26" and "CLV6"
#
# as column names, which would lose the first observation.
#
# We therefore read the file with col_names = FALSE so every
# physical CSV line is preserved as data.
#
# This handles both the current malformed-header-style export
# and future files of the same two-column physical structure.
#
# ============================================================

parse_update_roll_file <- function(symbol, path) {
  
  raw_data <- readr::read_csv(
    path,
    col_names = FALSE,
    show_col_types = FALSE,
    trim_ws = TRUE,
    col_types = cols(
      .default = col_character()
    )
  )
  
  if (ncol(raw_data) < 2L) {
    stop(
      paste0(
        "Bloomberg roll update file for ",
        symbol,
        " must contain at least two columns.\nFile: ",
        path
      ),
      call. = FALSE
    )
  }
  
  raw_data <- raw_data |>
    select(
      1,
      2
    )
  
  names(raw_data) <- c(
    "RawDate",
    "RawUnderlyingContract"
  )
  
  result <- raw_data |>
    mutate(
      RawDate = trimws(
        as.character(
          RawDate
        )
      ),
      RawUnderlyingContract = trimws(
        as.character(
          RawUnderlyingContract
        )
      )
    ) |>
    filter(
      !is.na(RawDate),
      RawDate != "",
      !is.na(RawUnderlyingContract),
      RawUnderlyingContract != ""
    )
  
  if (nrow(result) == 0L) {
    stop(
      paste0(
        "Bloomberg roll update file for ",
        symbol,
        " contains no usable observations.\nFile: ",
        path
      ),
      call. = FALSE
    )
  }
  
  result <- result |>
    mutate(
      ParsedDate = parse_roll_date(
        RawDate
      )
    )
  
  possible_header_rows <- result |>
    filter(
      is.na(ParsedDate)
    )
  
  if (nrow(possible_header_rows) > 0L) {
    
    header_like <- possible_header_rows |>
      mutate(
        DateHeader = toupper(
          trimws(
            RawDate
          )
        ),
        ContractHeader = toupper(
          trimws(
            RawUnderlyingContract
          )
        ),
        IsHeader = DateHeader %in% c(
          "DATE",
          "DATES"
        ) |
          ContractHeader %in% c(
            "UNDERLYINGCONTRACT",
            "UNDERLYING CONTRACT",
            "FUT_CUR_GEN_TICKER"
          )
      )
    
    nonheader_invalid_rows <- header_like |>
      filter(
        !IsHeader
      )
    
    if (nrow(nonheader_invalid_rows) > 0L) {
      print(
        nonheader_invalid_rows
      )
      
      stop(
        paste0(
          "Bloomberg roll update file for ",
          symbol,
          " contains unparseable non-header dates. ",
          "See printed rows above.\nFile: ",
          path
        ),
        call. = FALSE
      )
    }
  }
  
  result <- result |>
    filter(
      !is.na(ParsedDate)
    ) |>
    transmute(
      Symbol = symbol,
      Date = as.Date(
        ParsedDate
      ),
      RawUnderlyingContract = RawUnderlyingContract,
      SourceType = "Update",
      SourceFile = basename(
        path
      ),
      SourceRow = row_number(),
      BloombergSecurity = unname(
        LIVE_BLOOMBERG_SECURITY_MAP[[symbol]]
      )
    )
  
  result <- result |>
    mutate(
      UnderlyingContract = canonicalize_contract_identity(
        Symbol,
        RawUnderlyingContract
      )
    )
  
  result
}


# ============================================================
# 14. Load existing canonical roll history or bootstrap
# ============================================================

if (EXISTING_ROLL_HISTORY_EXISTS) {
  
  cat(
    "\nExisting futures_roll_history.csv found.\n"
  )
  
  cat(
    "Using it as the starting canonical roll history.\n\n"
  )
  
  existing_roll_history <- readr::read_csv(
    OUTPUT_PATH,
    show_col_types = FALSE,
    col_types = cols(
      .default = col_character()
    )
  )
  
  required_existing_columns <- c(
    "Symbol",
    "Date",
    "UnderlyingContract",
    "RawUnderlyingContract",
    "SourceType",
    "SourceFile",
    "SourceRow",
    "BloombergSecurity"
  )
  
  missing_existing_columns <- setdiff(
    required_existing_columns,
    names(existing_roll_history)
  )
  
  if (length(missing_existing_columns) > 0L) {
    stop(
      paste0(
        "Existing futures_roll_history.csv is missing required columns: ",
        paste(
          missing_existing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  existing_roll_history <- existing_roll_history |>
    transmute(
      Symbol = as.character(
        Symbol
      ),
      Date = as.Date(
        Date
      ),
      UnderlyingContract = as.character(
        UnderlyingContract
      ),
      RawUnderlyingContract = as.character(
        RawUnderlyingContract
      ),
      SourceType = as.character(
        SourceType
      ),
      SourceFile = as.character(
        SourceFile
      ),
      SourceRow = as.integer(
        SourceRow
      ),
      BloombergSecurity = as.character(
        BloombergSecurity
      )
    )
  
  existing_roll_history <- existing_roll_history |>
    mutate(
      UnderlyingContract = canonicalize_contract_identity(
        Symbol,
        UnderlyingContract
      )
    )
  
  missing_existing_symbols <- setdiff(
    LIVE_FUTURES_SYMBOLS,
    unique(
      existing_roll_history$Symbol
    )
  )
  
  if (length(missing_existing_symbols) > 0L) {
    stop(
      paste0(
        "Existing futures_roll_history.csv is missing symbols: ",
        paste(
          missing_existing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  unsupported_existing_symbols <- setdiff(
    unique(
      existing_roll_history$Symbol
    ),
    LIVE_FUTURES_SYMBOLS
  )
  
  if (length(unsupported_existing_symbols) > 0L) {
    stop(
      paste0(
        "Existing futures_roll_history.csv contains unsupported symbols: ",
        paste(
          unsupported_existing_symbols,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  existing_duplicates <- existing_roll_history |>
    count(
      Symbol,
      Date,
      name = "Occurrences"
    ) |>
    filter(
      Occurrences > 1L
    )
  
  if (nrow(existing_duplicates) > 0L) {
    stop(
      paste0(
        "Existing futures_roll_history.csv contains duplicate ",
        "Symbol/Date observations."
      ),
      call. = FALSE
    )
  }
  
} else {
  
  cat(
    "\nNo existing futures_roll_history.csv found.\n"
  )
  
  cat(
    "Bootstrapping from original historical roll mappings.\n\n"
  )
  
  historical_roll_list <- vector(
    mode = "list",
    length = length(
      LIVE_FUTURES_SYMBOLS
    )
  )
  
  names(historical_roll_list) <- LIVE_FUTURES_SYMBOLS
  
  for (symbol in LIVE_FUTURES_SYMBOLS) {
    
    path <- historical_paths$Path[
      historical_paths$Symbol == symbol
    ]
    
    historical_roll_list[[symbol]] <- parse_historical_roll_file(
      symbol = symbol,
      path = path
    )
  }
  
  existing_roll_history <- bind_rows(
    historical_roll_list[LIVE_FUTURES_SYMBOLS]
  ) |>
    mutate(
      BloombergSecurity = NA_character_
    )
}


# ============================================================
# 15. Load all Bloomberg roll updates
# ============================================================

update_roll_list <- vector(
  mode = "list",
  length = length(
    LIVE_FUTURES_SYMBOLS
  )
)

names(update_roll_list) <- LIVE_FUTURES_SYMBOLS


for (symbol in LIVE_FUTURES_SYMBOLS) {
  
  path <- update_paths$Path[
    update_paths$Symbol == symbol
  ]
  
  update_roll_list[[symbol]] <- parse_update_roll_file(
    symbol = symbol,
    path = path
  )
}


update_roll_data <- bind_rows(
  update_roll_list[LIVE_FUTURES_SYMBOLS]
)


# ============================================================
# 16. Structural validation of source observations
# ============================================================

validate_roll_observations <- function(data, source_name) {
  
  if (nrow(data) == 0L) {
    stop(
      paste0(
        source_name,
        " contains zero observations."
      ),
      call. = FALSE
    )
  }
  
  if (any(is.na(data$Symbol))) {
    stop(
      paste0(
        source_name,
        " contains missing Symbol values."
      ),
      call. = FALSE
    )
  }
  
  if (any(!data$Symbol %in% LIVE_FUTURES_SYMBOLS)) {
    stop(
      paste0(
        source_name,
        " contains unsupported Symbol values."
      ),
      call. = FALSE
    )
  }
  
  if (any(is.na(data$Date))) {
    stop(
      paste0(
        source_name,
        " contains missing or invalid Date values."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(is.na(data$UnderlyingContract)) ||
    any(trimws(data$UnderlyingContract) == "")
  ) {
    stop(
      paste0(
        source_name,
        " contains missing canonical contract identities."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(is.na(data$RawUnderlyingContract)) ||
    any(trimws(data$RawUnderlyingContract) == "")
  ) {
    stop(
      paste0(
        source_name,
        " contains missing raw contract identities."
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


validate_roll_observations(
  existing_roll_history,
  "Existing/historical roll history"
)

validate_roll_observations(
  update_roll_data,
  "Bloomberg roll update data"
)


# ============================================================
# 17. Validate duplicates within the update files
# ============================================================
#
# A single update batch must not give two different contract
# identities for the same Symbol + Date.
#
# ============================================================

update_duplicate_summary <- update_roll_data |>
  group_by(
    Symbol,
    Date
  ) |>
  summarise(
    Occurrences = n(),
    DistinctContracts = n_distinct(
      UnderlyingContract
    ),
    Contracts = paste(
      sort(
        unique(
          UnderlyingContract
        )
      ),
      collapse = ", "
    ),
    .groups = "drop"
  ) |>
  filter(
    Occurrences > 1L
  )


update_duplicate_conflicts <- update_duplicate_summary |>
  filter(
    DistinctContracts > 1L
  )


if (nrow(update_duplicate_conflicts) > 0L) {
  
  print(
    update_duplicate_conflicts
  )
  
  stop(
    paste0(
      "Bloomberg roll update files contain conflicting contract ",
      "identities for the same Symbol/Date. See printed rows above."
    ),
    call. = FALSE
  )
}


update_roll_data <- update_roll_data |>
  arrange(
    Symbol,
    Date,
    SourceFile,
    SourceRow
  ) |>
  group_by(
    Symbol,
    Date
  ) |>
  slice_head(
    n = 1L
  ) |>
  ungroup()


# ============================================================
# 18. Validate overlap with canonical history
# ============================================================
#
# Existing history and the new update may overlap by any
# number of dates.
#
# The canonical contract identity must agree.
#
# For KE:
#
#   KEU6 and KWU6 both canonicalize to KEU6.
#
# Therefore the source-root change is not incorrectly treated
# as a futures roll.
#
# ============================================================

overlap_check <- existing_roll_history |>
  select(
    Symbol,
    Date,
    ExistingContract = UnderlyingContract,
    ExistingRawContract = RawUnderlyingContract
  ) |>
  inner_join(
    update_roll_data |>
      select(
        Symbol,
        Date,
        UpdateContract = UnderlyingContract,
        UpdateRawContract = RawUnderlyingContract
      ),
    by = c(
      "Symbol",
      "Date"
    )
  ) |>
  mutate(
    Matches = ExistingContract == UpdateContract
  )


overlap_conflicts <- overlap_check |>
  filter(
    !Matches
  )


if (nrow(overlap_conflicts) > 0L) {
  
  print(
    overlap_conflicts
  )
  
  stop(
    paste0(
      "Existing and update roll histories contain conflicting ",
      "contract identities at the same Symbol/Date. ",
      "See printed overlap conflicts above."
    ),
    call. = FALSE
  )
}


# ============================================================
# 19. Combine existing and update observations
# ============================================================
#
# Existing canonical history receives priority after overlap
# validation.
#
# ============================================================

combined_roll_history <- bind_rows(
  existing_roll_history |>
    mutate(
      InputPriority = 1L
    ),
  update_roll_data |>
    mutate(
      InputPriority = 2L
    )
) |>
  arrange(
    Symbol,
    Date,
    InputPriority,
    SourceFile,
    SourceRow
  )


# ============================================================
# 20. Detect conflicting combined duplicates
# ============================================================

combined_duplicate_summary <- combined_roll_history |>
  group_by(
    Symbol,
    Date
  ) |>
  summarise(
    Occurrences = n(),
    DistinctContracts = n_distinct(
      UnderlyingContract
    ),
    Contracts = paste(
      sort(
        unique(
          UnderlyingContract
        )
      ),
      collapse = ", "
    ),
    .groups = "drop"
  ) |>
  filter(
    Occurrences > 1L
  )


combined_duplicate_conflicts <- combined_duplicate_summary |>
  filter(
    DistinctContracts > 1L
  )


if (nrow(combined_duplicate_conflicts) > 0L) {
  
  print(
    combined_duplicate_conflicts
  )
  
  stop(
    paste0(
      "Conflicting duplicate roll observations were found. ",
      "The canonical roll history was not written."
    ),
    call. = FALSE
  )
}


# ============================================================
# 21. Deduplicate canonical roll observations
# ============================================================

canonical_roll_history <- combined_roll_history |>
  group_by(
    Symbol,
    Date
  ) |>
  slice_head(
    n = 1L
  ) |>
  ungroup() |>
  select(
    -InputPriority
  )


# ============================================================
# 22. Canonical chronological ordering
# ============================================================

canonical_roll_history <- canonical_roll_history |>
  mutate(
    Symbol = factor(
      Symbol,
      levels = LIVE_FUTURES_SYMBOLS
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


# ============================================================
# 23. Final structural validation
# ============================================================

missing_symbols <- setdiff(
  LIVE_FUTURES_SYMBOLS,
  unique(
    canonical_roll_history$Symbol
  )
)


if (length(missing_symbols) > 0L) {
  stop(
    paste0(
      "Canonical futures roll history is missing symbols: ",
      paste(
        missing_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


final_duplicates <- canonical_roll_history |>
  count(
    Symbol,
    Date,
    name = "Occurrences"
  ) |>
  filter(
    Occurrences > 1L
  )


if (nrow(final_duplicates) > 0L) {
  stop(
    paste0(
      "Canonical futures roll history still contains duplicate ",
      "Symbol/Date observations."
    ),
    call. = FALSE
  )
}


if (any(is.na(canonical_roll_history$Date))) {
  stop(
    "Canonical futures roll history contains missing dates.",
    call. = FALSE
  )
}


if (
  any(is.na(canonical_roll_history$UnderlyingContract)) ||
  any(trimws(canonical_roll_history$UnderlyingContract) == "")
) {
  stop(
    "Canonical futures roll history contains invalid contract identities.",
    call. = FALSE
  )
}


# ============================================================
# 24. Detect actual contract changes
# ============================================================
#
# This is diagnostic only.
#
# A roll is identified when the canonical underlying contract
# changes from the previous available mapping date for the same
# optimizer future.
#
# Missing calendar/business dates are NOT fabricated.
#
# ============================================================

roll_events <- canonical_roll_history |>
  arrange(
    Symbol,
    Date
  ) |>
  group_by(
    Symbol
  ) |>
  mutate(
    PreviousContract = lag(
      UnderlyingContract
    ),
    ContractChanged = !is.na(
      PreviousContract
    ) &
      UnderlyingContract != PreviousContract
  ) |>
  ungroup() |>
  filter(
    ContractChanged
  ) |>
  select(
    Symbol,
    Date,
    PreviousContract,
    UnderlyingContract,
    RawUnderlyingContract,
    SourceType,
    SourceFile
  )


# ============================================================
# 25. Coverage summary
# ============================================================

coverage_summary <- canonical_roll_history |>
  group_by(
    Symbol
  ) |>
  summarise(
    Observations = n(),
    FirstDate = min(
      Date
    ),
    LastDate = max(
      Date
    ),
    CurrentContract = UnderlyingContract[
      which.max(
        Date
      )
    ],
    .groups = "drop"
  )


# ============================================================
# 26. Current update contribution
# ============================================================
#
# Count only observations from the current update batch that
# were not already present in the starting canonical history.
#
# ============================================================

new_update_rows <- update_roll_data |>
  anti_join(
    existing_roll_history |>
      select(
        Symbol,
        Date
      ),
    by = c(
      "Symbol",
      "Date"
    )
  )


update_contribution <- new_update_rows |>
  group_by(
    Symbol
  ) |>
  summarise(
    NewRows = n(),
    FirstNewDate = min(
      Date
    ),
    LastNewDate = max(
      Date
    ),
    .groups = "drop"
  )


# ============================================================
# 27. Print validation diagnostics
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FUTURES ROLL HISTORY BUILD VALIDATION\n"
)

cat(
  "============================================================\n\n"
)


cat(
  "Starting canonical/historical rows: ",
  nrow(
    existing_roll_history
  ),
  "\n",
  sep = ""
)

cat(
  "Update rows parsed:                  ",
  nrow(
    update_roll_data
  ),
  "\n",
  sep = ""
)

cat(
  "Overlapping Symbol/Date observations: ",
  nrow(
    overlap_check
  ),
  "\n",
  sep = ""
)

cat(
  "Conflicting overlaps:                ",
  nrow(
    overlap_conflicts
  ),
  "\n",
  sep = ""
)

cat(
  "New update observations:             ",
  nrow(
    new_update_rows
  ),
  "\n",
  sep = ""
)

cat(
  "Canonical rows after deduplication:  ",
  nrow(
    canonical_roll_history
  ),
  "\n\n",
  sep = ""
)


cat(
  "Canonical roll coverage by future:\n\n"
)

print(
  coverage_summary,
  n = Inf
)


cat(
  "\nNew observations contributed by this update:\n\n"
)

if (nrow(update_contribution) == 0L) {
  
  cat(
    "No genuinely new Symbol/Date observations.\n"
  )
  
} else {
  
  print(
    update_contribution,
    n = Inf
  )
}


cat(
  "\nDetected canonical contract changes:\n\n"
)

if (nrow(roll_events) == 0L) {
  
  cat(
    "No contract changes detected.\n"
  )
  
} else {
  
  print(
    roll_events,
    n = Inf
  )
}


# ============================================================
# 28. Write futures_roll_history.csv
# ============================================================

readr::write_csv(
  canonical_roll_history,
  OUTPUT_PATH,
  na = ""
)


# ============================================================
# 29. Verify written file
# ============================================================

if (!file.exists(OUTPUT_PATH)) {
  stop(
    paste0(
      "futures_roll_history.csv was not created:\n",
      OUTPUT_PATH
    ),
    call. = FALSE
  )
}


written_roll_history <- readr::read_csv(
  OUTPUT_PATH,
  show_col_types = FALSE
)


if (nrow(written_roll_history) != nrow(canonical_roll_history)) {
  stop(
    paste0(
      "Written futures_roll_history.csv row count does not match ",
      "the in-memory canonical roll history."
    ),
    call. = FALSE
  )
}


written_duplicates <- written_roll_history |>
  count(
    Symbol,
    Date,
    name = "Occurrences"
  ) |>
  filter(
    Occurrences > 1L
  )


if (nrow(written_duplicates) > 0L) {
  stop(
    paste0(
      "Written futures_roll_history.csv contains duplicate ",
      "Symbol/Date observations."
    ),
    call. = FALSE
  )
}


# ============================================================
# 30. Final success message
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "FUTURES ROLL HISTORY BUILD PASSED\n"
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
  nrow(
    canonical_roll_history
  ),
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
  "\nThe canonical roll history contains daily underlying-contract identities.\n"
)

cat(
  "No futures returns were calculated by this script.\n"
)

cat(
  "No missing mapping dates were fabricated.\n"
)

cat(
  "KE/KW source-root differences are normalized for contract comparison.\n"
)

cat(
  "\n============================================================\n"
)