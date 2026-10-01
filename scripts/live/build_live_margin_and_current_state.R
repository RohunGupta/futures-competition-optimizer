# ============================================================
# build_live_margin_and_current_state.R
#
# Stage 7:
#   Build live margin.csv and current_state.csv
#
# Inputs:
#   data/live_inputs/current_account.csv
#   data/live_inputs/current_leaderboard.csv
#   data/live_inputs/futures_history.csv
#
# Uses:
#   scripts/live/build_live_competition_clock.R
#
# Outputs:
#   outputs/optimizer_inputs/margin.csv
#   outputs/optimizer_inputs/current_state.csv
#
# Frozen production conventions:
#
#   Future order:
#     Future_1 = CL
#     Future_2 = GC
#     Future_3 = SR3
#     Future_4 = ZN
#     Future_5 = KE
#     Future_6 = HO
#     Future_7 = ZW
#     Future_8 = NQ
#
#   Implementation contracts:
#     CL  -> MCL
#     GC  -> MGC
#     SR3 -> SR3
#     ZN  -> ZN
#     KE  -> KE
#     HO  -> HO
#     ZW  -> ZW
#     NQ  -> MNQ
#
#   Implementation price-notional multipliers:
#     MCL = 100
#     MGC = 10
#     SR3 = 2500
#     ZN  = 1000
#     KE  = 50
#     HO  = 420
#     ZW  = 50
#     MNQ = 2
#
#   Margin scaling:
#     CL  = 0.10
#     GC  = 0.10
#     SR3 = 1.00
#     ZN  = 1.00
#     KE  = 1.00
#     HO  = 1.00
#     ZW  = 1.00
#     NQ  = 0.10
#
#   Margin model:
#     Persistence for all eight production futures.
#
#   Current carried position weights:
#     signed margin commitment / current equity
#
#     For the current_account.csv convention used here:
#
#       weight_k =
#         ContractsHeld_k *
#         MarginPerContract_k /
#         current leaderboard Balance
#
#     The account balance is NOT manually supplied in
#     current_account.csv. Stage 7 identifies our unique row in
#     current_leaderboard.csv using DisplayName and uses that row's
#     Balance as the single authoritative current-equity value.
#
#     CL, GC, and NQ ContractsHeld may be expressed in
#     standard-contract-equivalent units while MarginPerContract
#     is the corresponding standard-contract margin.
#
#     This is economically identical to converting both quantity
#     and margin to the micro implementation contract:
#
#       q_micro = q_standard / 0.10
#       margin_micro = margin_standard * 0.10
#
#     so:
#
#       q_micro * margin_micro
#       =
#       q_standard * margin_standard
#
#   No clipping or renormalization of current weights.
#
#   Daily rank ordering:
#     descending DailyPnl,
#     then ascending overall Rank,
#     then ascending DisplayName.
#
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
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

if (length(missing_packages) > 0L) {
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


# ============================================================
# 2. Project paths
# ============================================================

PROJECT_ROOT <- here::here()

LIVE_INPUT_DIR <- file.path(
  PROJECT_ROOT,
  "data",
  "live_inputs"
)

LIVE_SCRIPT_DIR <- file.path(
  PROJECT_ROOT,
  "scripts",
  "live"
)

OPTIMIZER_INPUT_DIR <- file.path(PROJECT_ROOT, "outputs", "optimizer_inputs")

CURRENT_ACCOUNT_PATH <- file.path(
  LIVE_INPUT_DIR,
  "current_account.csv"
)

CURRENT_LEADERBOARD_PATH <- file.path(
  LIVE_INPUT_DIR,
  "current_leaderboard.csv"
)

DAILY_PRIZE_WINNERS_PATH <- file.path(
  LIVE_INPUT_DIR,
  "daily_prize_winners.csv"
)

BLOOMBERG_FUTURES_HISTORY_PATH <- file.path(
  LIVE_INPUT_DIR,
  "futures_history.csv"
)

YAHOO_FUTURES_HISTORY_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "yahoo",
  "yahoo_shadow_futures_history.csv"
)

if (
  !exists(
    "LIVE_FUTURES_DATA_SOURCE",
    envir = .GlobalEnv
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
  !LIVE_FUTURES_DATA_SOURCE %in% c(
    "BLOOMBERG",
    "YAHOO"
  )
) {
  stop(
    paste0(
      "LIVE_FUTURES_DATA_SOURCE must be exactly ",
      "'BLOOMBERG' or 'YAHOO'."
    ),
    call. = FALSE
  )
}

FUTURES_HISTORY_PATH <- if (
  LIVE_FUTURES_DATA_SOURCE == "YAHOO"
) {
  YAHOO_FUTURES_HISTORY_PATH
} else {
  BLOOMBERG_FUTURES_HISTORY_PATH
}

LIVE_CLOCK_SCRIPT_PATH <- file.path(
  LIVE_SCRIPT_DIR,
  "build_live_competition_clock.R"
)

MARGIN_OUTPUT_PATH <- file.path(
  OPTIMIZER_INPUT_DIR,
  "margin.csv"
)

CURRENT_STATE_OUTPUT_PATH <- file.path(
  OPTIMIZER_INPUT_DIR,
  "current_state.csv"
)

if (!dir.exists(OPTIMIZER_INPUT_DIR)) {
  dir.create(
    OPTIMIZER_INPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

required_input_files <- c(
  CURRENT_ACCOUNT_PATH,
  DAILY_PRIZE_WINNERS_PATH,
  FUTURES_HISTORY_PATH,
  LIVE_CLOCK_SCRIPT_PATH
)

missing_input_files <- required_input_files[
  !file.exists(required_input_files)
]

if (length(missing_input_files) > 0L) {
  stop(
    paste0(
      "Stage 7 is missing required files:\n",
      paste(
        missing_input_files,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 3. Frozen production constants
# ============================================================

EXPECTED_T <- 147L

COMPETITION_TIME_ZONE <- "America/Chicago"

EXPECTED_SYMBOLS <- c(
  "CL",
  "GC",
  "SR3",
  "ZN",
  "KE",
  "HO",
  "ZW",
  "NQ"
)

EXPECTED_FUTURES <- c(
  "Future_1",
  "Future_2",
  "Future_3",
  "Future_4",
  "Future_5",
  "Future_6",
  "Future_7",
  "Future_8"
)

SYMBOL_TO_FUTURE <- setNames(
  EXPECTED_FUTURES,
  EXPECTED_SYMBOLS
)

IMPLEMENTATION_SYMBOLS <- c(
  Future_1 = "MCL",
  Future_2 = "MGC",
  Future_3 = "SR3",
  Future_4 = "ZN",
  Future_5 = "KE",
  Future_6 = "HO",
  Future_7 = "ZW",
  Future_8 = "MNQ"
)

PRICE_NOTIONAL_MULTIPLIERS <- c(
  Future_1 = 100,
  Future_2 = 10,
  Future_3 = 2500,
  Future_4 = 1000,
  Future_5 = 50,
  Future_6 = 420,
  Future_7 = 50,
  Future_8 = 2
)

MARGIN_SCALE_FACTORS <- c(
  Future_1 = 0.10,
  Future_2 = 0.10,
  Future_3 = 1.00,
  Future_4 = 1.00,
  Future_5 = 1.00,
  Future_6 = 1.00,
  Future_7 = 1.00,
  Future_8 = 0.10
)

ACTIVE_DAILY_PRIZE_DAYS <- c(
  "Monday",
  "Tuesday",
  "Wednesday",
  "Thursday"
)

TARGET_OVERALL_RANKS <- c(
  1L,
  2L,
  3L,
  4L,
  15L,
  16L
)

PRICE_STALENESS_LIMIT_MINUTES <- 120

SCHEDULED_LONG_GAP_LIMIT_MINUTES <- 510

SUNDAY_T0_WEEKEND_GAP_LIMIT_MINUTES <- 3000

WHEAT_SYMBOLS <- c(
  "KE",
  "ZW"
)

# KE and ZW do not reopen until 19:00 CT.
#
# Their final pre-weekend 30-minute endpoint can be around
# 13:30 CT Friday. At the Sunday 19:00 decision timestamp,
# that observation is 3210 minutes old.
#
# 3240 minutes gives one 30-minute-bar allowance while keeping
# the exception tightly tied to the known wheat closure.
WHEAT_WEEKEND_GAP_LIMIT_MINUTES <- 3240

# During the regular daily wheat closure, the last 30-minute
# endpoint can be around 13:30 CT and remain the correct latest
# price through the 19:00 CT reopening decision.
WHEAT_DAILY_GAP_LIMIT_MINUTES <- 330


# ============================================================
# TEST-ONLY historical futures-price cutoff
# ============================================================
#
# FALSE:
#   Normal production behavior. No futures-history truncation.
#
# TRUE:
#   For Stage 7 testing only, pretend futures-price observations
#   after TEST_FUTURES_HISTORY_CUTOFF_CT do not yet exist.
#
# IMPORTANT:
#   This affects only the futures-history data read by this
#   Stage 7 script. It does NOT modify any Yahoo archive on disk.
#
# ============================================================

TEST_USE_FUTURES_HISTORY_CUTOFF <- FALSE

TEST_FUTURES_HISTORY_CUTOFF_CT <- as.POSIXct(
  "2026-09-20 16:00:00",
  format = "%Y-%m-%d %H:%M:%S",
  tz = COMPETITION_TIME_ZONE
)


# ============================================================
# 4. Load validated Stage 2 competition clock
# ============================================================

source(
  LIVE_CLOCK_SCRIPT_PATH,
  local = .GlobalEnv
)

# Restore Stage 7 canonical future identifiers after sourcing Stage 2.
# The Stage 2 script defines EXPECTED_FUTURES using underlying symbols
# in the global environment, so Stage 7 must restore its own canonical
# optimizer identifiers before continuing.
EXPECTED_FUTURES <- c(
  "Future_1",
  "Future_2",
  "Future_3",
  "Future_4",
  "Future_5",
  "Future_6",
  "Future_7",
  "Future_8"
)

if (!exists("live_competition_clock", envir = .GlobalEnv)) {
  stop(
    paste0(
      "build_live_competition_clock.R did not create ",
      "live_competition_clock."
    ),
    call. = FALSE
  )
}

if (
  exists(
    "LIVE_COMPETITION_CLOCK_VALIDATED",
    envir = .GlobalEnv
  )
) {
  if (
    !isTRUE(
      get(
        "LIVE_COMPETITION_CLOCK_VALIDATED",
        envir = .GlobalEnv
      )
    )
  ) {
    stop(
      "Stage 2 live competition clock is not validated.",
      call. = FALSE
    )
  }
}

current_t <- as.integer(
  live_competition_clock[["current_t"]]
)

current_day <- as.character(
  live_competition_clock[["current_day"]]
)

current_time_ct <- as.character(
  live_competition_clock[["current_time_ct"]]
)

if (
  length(current_t) != 1L ||
  !is.finite(current_t) ||
  current_t < 0L ||
  current_t >= EXPECTED_T
) {
  stop(
    "Invalid current_t from live_competition_clock.",
    call. = FALSE
  )
}

if (
  current_t >= 1L &&
  !file.exists(
    CURRENT_LEADERBOARD_PATH
  )
) {
  stop(
    paste0(
      "current_leaderboard.csv is required when current_t >= 1. ",
      "Current current_t = ",
      current_t,
      "."
    ),
    call. = FALSE
  )
}

cat(
  "\nStage 7 clock:\n"
)

cat(
  "  futures data source: ",
  LIVE_FUTURES_DATA_SOURCE,
  "\n",
  sep = ""
)

cat(
  "  futures history path: ",
  FUTURES_HISTORY_PATH,
  "\n",
  sep = ""
)

cat(
  "  current_t: ",
  current_t,
  "\n",
  sep = ""
)

cat(
  "  current day: ",
  current_day,
  "\n",
  sep = ""
)

cat(
  "  current time CT: ",
  current_time_ct,
  "\n\n",
  sep = ""
)


# ============================================================
# 5. Read current_account.csv
# ============================================================

current_account <- readr::read_csv(
  CURRENT_ACCOUNT_PATH,
  show_col_types = FALSE
)

required_account_columns <- c(
  "DisplayName",
  "Symbol",
  "ContractsHeld",
  "MarginPerContract",
  "Day",
  "Time (CT)"
)

missing_account_columns <- setdiff(
  required_account_columns,
  names(current_account)
)

if (length(missing_account_columns) > 0L) {
  stop(
    paste0(
      "current_account.csv is missing required columns: ",
      paste(
        missing_account_columns,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (nrow(current_account) != 8L) {
  stop(
    "current_account.csv must contain exactly 8 rows.",
    call. = FALSE
  )
}

current_account[["DisplayName"]] <- trimws(
  as.character(
    current_account[["DisplayName"]]
  )
)

current_account[["Symbol"]] <- trimws(
  as.character(
    current_account[["Symbol"]]
  )
)

current_account[["ContractsHeld"]] <- as.numeric(
  current_account[["ContractsHeld"]]
)

current_account[["MarginPerContract"]] <- as.numeric(
  current_account[["MarginPerContract"]]
)

if (
  any(
    is.na(
      current_account[["DisplayName"]]
    )
  ) ||
  any(
    current_account[["DisplayName"]] == ""
  )
) {
  stop(
    "current_account.csv contains missing or blank DisplayName values.",
    call. = FALSE
  )
}

unique_display_names <- unique(
  current_account[["DisplayName"]]
)

if (length(unique_display_names) != 1L) {
  stop(
    paste0(
      "All 8 current_account.csv rows must have the same DisplayName. ",
      "Found: ",
      paste(
        unique_display_names,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

OUR_DISPLAY_NAME <- unique_display_names[[1]]

if (
  anyDuplicated(
    current_account[["Symbol"]]
  ) > 0L
) {
  stop(
    "current_account.csv contains duplicate Symbol values.",
    call. = FALSE
  )
}

if (
  !setequal(
    current_account[["Symbol"]],
    EXPECTED_SYMBOLS
  )
) {
  stop(
    paste0(
      "current_account.csv Symbol values must be exactly: ",
      paste(
        EXPECTED_SYMBOLS,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      current_account[["ContractsHeld"]]
    )
  )
) {
  stop(
    "ContractsHeld must contain only finite values.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      current_account[["MarginPerContract"]]
    )
  ) ||
  any(
    current_account[["MarginPerContract"]] <= 0
  )
) {
  stop(
    "MarginPerContract must be finite and strictly positive.",
    call. = FALSE
  )
}

account_order <- match(
  EXPECTED_SYMBOLS,
  current_account[["Symbol"]]
)

if (any(is.na(account_order))) {
  stop(
    "Could not place current_account.csv into canonical future order.",
    call. = FALSE
  )
}

current_account <- current_account[
  account_order,
  ,
  drop = FALSE
]

if (
  !identical(
    current_account[["Symbol"]],
    EXPECTED_SYMBOLS
  )
) {
  stop(
    "Canonical current-account symbol ordering failed.",
    call. = FALSE
  )
}


# ============================================================
# 6. Validate current_account clock against Stage 2
# ============================================================

normalize_day_stage7 <- function(x) {
  x <- trimws(
    as.character(
      x
    )
  )
  
  x <- tools::toTitleCase(
    tolower(
      x
    )
  )
  
  x
}

normalize_time_stage7 <- function(x) {
  x <- trimws(
    as.character(
      x
    )
  )
  
  parse_one <- function(value) {
    value <- trimws(
      value
    )
    
    parsed <- suppressWarnings(
      as.POSIXct(
        value,
        format = "%H:%M",
        tz = COMPETITION_TIME_ZONE
      )
    )
    
    if (is.na(parsed)) {
      parsed <- suppressWarnings(
        as.POSIXct(
          value,
          format = "%H:%M:%S",
          tz = COMPETITION_TIME_ZONE
        )
      )
    }
    
    if (is.na(parsed)) {
      stop(
        paste0(
          "Could not parse Time (CT) value: ",
          value
        ),
        call. = FALSE
      )
    }
    
    format(
      parsed,
      "%H:%M",
      tz = COMPETITION_TIME_ZONE
    )
  }
  
  vapply(
    x,
    parse_one,
    character(1)
  )
}

account_days <- normalize_day_stage7(
  current_account[["Day"]]
)

account_times <- normalize_time_stage7(
  current_account[["Time (CT)"]]
)

if (
  length(
    unique(
      account_days
    )
  ) != 1L
) {
  stop(
    "All current_account.csv rows must have the same Day.",
    call. = FALSE
  )
}

if (
  length(
    unique(
      account_times
    )
  ) != 1L
) {
  stop(
    "All current_account.csv rows must have the same Time (CT).",
    call. = FALSE
  )
}

stage7_current_day <- unique(
  account_days
)[[1]]

stage7_current_time <- unique(
  account_times
)[[1]]

stage2_normalized_time <- normalize_time_stage7(
  current_time_ct
)[[1]]

if (
  stage7_current_day != current_day ||
  stage7_current_time != stage2_normalized_time
) {
  stop(
    paste0(
      "current_account.csv clock does not match Stage 2. ",
      "Account says ",
      stage7_current_day,
      " ",
      stage7_current_time,
      " CT; Stage 2 says ",
      current_day,
      " ",
      stage2_normalized_time,
      " CT."
    ),
    call. = FALSE
  )
}


# ============================================================
# 7. Build live persistence margin path
# ============================================================
#
# current_account.csv MarginPerContract is the current observed
# margin on the source/standard-equivalent contract basis.
#
# The optimizer margin primitive is per IMPLEMENTATION contract.
#
# Therefore:
#
#   CL -> MCL: standard CL margin * 0.10, rounded upward
#   GC -> MGC: standard GC margin * 0.10, rounded upward
#   NQ -> MNQ: standard NQ margin * 0.10, rounded upward
#
# Direct contracts retain their observed margin.
#
# All eight frozen production margin models use persistence.
#
# ============================================================

current_source_margins <- setNames(
  current_account[["MarginPerContract"]],
  EXPECTED_FUTURES
)

current_implementation_margins <- numeric(
  length(
    EXPECTED_FUTURES
  )
)

names(
  current_implementation_margins
) <- EXPECTED_FUTURES

for (future_name in EXPECTED_FUTURES) {
  raw_scaled_margin <- (
    current_source_margins[[future_name]] *
      MARGIN_SCALE_FACTORS[[future_name]]
  )
  
  if (
    MARGIN_SCALE_FACTORS[[future_name]] < 1
  ) {
    current_implementation_margins[[future_name]] <- ceiling(
      raw_scaled_margin
    )
  } else {
    current_implementation_margins[[future_name]] <- raw_scaled_margin
  }
}

if (
  any(
    !is.finite(
      current_implementation_margins
    )
  ) ||
  any(
    current_implementation_margins <= 0
  )
) {
  stop(
    "Calculated implementation margins are invalid.",
    call. = FALSE
  )
}

margin_csv <- data.frame(
  t = 0:(EXPECTED_T - 1L),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

for (future_name in EXPECTED_FUTURES) {
  margin_csv[[future_name]] <- rep(
    current_implementation_margins[[future_name]],
    EXPECTED_T
  )
}

expected_margin_columns <- c(
  "t",
  EXPECTED_FUTURES
)

if (
  !identical(
    names(
      margin_csv
    ),
    expected_margin_columns
  )
) {
  stop(
    "margin.csv column ordering is invalid.",
    call. = FALSE
  )
}

if (
  nrow(
    margin_csv
  ) != EXPECTED_T
) {
  stop(
    "margin.csv does not contain exactly 147 rows.",
    call. = FALSE
  )
}

if (
  !identical(
    as.integer(
      margin_csv[["t"]]
    ),
    0:(EXPECTED_T - 1L)
  )
) {
  stop(
    "margin.csv t index must be exactly 0:146.",
    call. = FALSE
  )
}

margin_matrix <- as.matrix(
  margin_csv[
    ,
    EXPECTED_FUTURES,
    drop = FALSE
  ]
)

storage.mode(
  margin_matrix
) <- "double"

if (
  any(
    !is.finite(
      margin_matrix
    )
  ) ||
  any(
    margin_matrix <= 0
  )
) {
  stop(
    "margin.csv contains invalid margin values.",
    call. = FALSE
  )
}


# ============================================================
# 8A. Read and validate prior daily prize winners
# ============================================================

daily_prize_winners <- readr::read_csv(
  DAILY_PRIZE_WINNERS_PATH,
  show_col_types = FALSE
)

required_daily_prize_winner_columns <- c(
  "Day",
  "DisplayName"
)

missing_daily_prize_winner_columns <- setdiff(
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

daily_prize_winners[["Day"]] <- trimws(
  as.character(
    daily_prize_winners[["Day"]]
  )
)

daily_prize_winners[["DisplayName"]] <- trimws(
  as.character(
    daily_prize_winners[["DisplayName"]]
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

valid_daily_prize_days <- c(
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


# ============================================================
# 9-12. Construct current competition state
# ============================================================
#
# At t = 0 the competition state is known deterministically:
#
#   - every account begins at $25,000
#   - all overall threshold balances are $25,000
#   - daily PnL is zero
#
# Therefore no leaderboard snapshot is required.
#
# At t >= 1 the current leaderboard becomes authoritative.
#
# ============================================================

STARTING_BALANCE <- 25000

if (
  current_t == 0L
) {
  current_leaderboard <- NULL
  
  our_balance <- STARTING_BALANCE
  
  # All accounts are tied at the starting balance. Rank is not
  # economically informative before the first realized interval.
  # Use rank 1 as the deterministic tie-position convention so
  # current_state.csv remains fully numeric.
  our_rank <- 1L
  
  our_daily_pnl <- 0
  our_daily_rank <- 1L
  
  overall_place_balances <-
    c(
      balance_1st = STARTING_BALANCE,
      balance_2nd = STARTING_BALANCE,
      balance_3rd = STARTING_BALANCE,
      balance_4th = STARTING_BALANCE,
      balance_15th = STARTING_BALANCE,
      balance_16th = STARTING_BALANCE
    )
  
  daily_1st_pnl <- 0
  daily_2nd_pnl <- 0
  
  cat(
    "current_t = 0: initialized deterministic starting leaderboard state.\n"
  )
  
} else {
  current_leaderboard <- readr::read_csv(
    CURRENT_LEADERBOARD_PATH,
    show_col_types = FALSE
  )
  
  required_leaderboard_columns <- c(
    "Rank",
    "DisplayName",
    "Balance",
    "Trades",
    "ProductsTraded",
    "DailyPnl",
    "TotalPnl"
  )
  
  missing_leaderboard_columns <- setdiff(
    required_leaderboard_columns,
    names(current_leaderboard)
  )
  
  if (
    length(
      missing_leaderboard_columns
    ) > 0L
  ) {
    stop(
      paste0(
        "current_leaderboard.csv is missing required columns: ",
        paste(
          missing_leaderboard_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (
    nrow(
      current_leaderboard
    ) == 0L
  ) {
    stop(
      "current_leaderboard.csv contains zero rows.",
      call. = FALSE
    )
  }
  
  current_leaderboard[["DisplayName"]] <- trimws(
    as.character(
      current_leaderboard[["DisplayName"]]
    )
  )
  
  current_leaderboard[["Rank"]] <- suppressWarnings(
    as.integer(
      current_leaderboard[["Rank"]]
    )
  )
  
  current_leaderboard[["Balance"]] <- suppressWarnings(
    as.numeric(
      current_leaderboard[["Balance"]]
    )
  )
  
  current_leaderboard[["DailyPnl"]] <- suppressWarnings(
    as.numeric(
      current_leaderboard[["DailyPnl"]]
    )
  )
  
  current_leaderboard[["TotalPnl"]] <- suppressWarnings(
    as.numeric(
      current_leaderboard[["TotalPnl"]]
    )
  )
  
  if (
    any(
      !is.finite(
        current_leaderboard[["Rank"]]
      )
    )
  ) {
    stop(
      "current_leaderboard.csv contains invalid Rank values.",
      call. = FALSE
    )
  }
  
  if (
    anyDuplicated(
      current_leaderboard[["Rank"]]
    ) > 0L
  ) {
    stop(
      "current_leaderboard.csv contains duplicate Rank values.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        current_leaderboard[["DailyPnl"]]
      )
    )
  ) {
    stop(
      "current_leaderboard.csv contains invalid DailyPnl values.",
      call. = FALSE
    )
  }
  
  
  # ----------------------------------------------------------
  # Identify our exact leaderboard row
  # ----------------------------------------------------------
  
  our_row_indices <- which(
    current_leaderboard[["DisplayName"]] ==
      OUR_DISPLAY_NAME
  )
  
  if (
    length(
      our_row_indices
    ) == 0L
  ) {
    stop(
      paste0(
        "DisplayName '",
        OUR_DISPLAY_NAME,
        "' was not found in current_leaderboard.csv."
      ),
      call. = FALSE
    )
  }
  
  if (
    length(
      our_row_indices
    ) > 1L
  ) {
    stop(
      paste0(
        "DisplayName '",
        OUR_DISPLAY_NAME,
        "' appears ",
        length(
          our_row_indices
        ),
        " times in current_leaderboard.csv. ",
        "Stage 7 will not guess which row is the account."
      ),
      call. = FALSE
    )
  }
  
  our_leaderboard_row <- current_leaderboard[
    our_row_indices[[1]],
    ,
    drop = FALSE
  ]
  
  our_rank <- as.integer(
    our_leaderboard_row[["Rank"]][[1]]
  )
  
  our_balance <- as.numeric(
    our_leaderboard_row[["Balance"]][[1]]
  )
  
  our_daily_pnl <- as.numeric(
    our_leaderboard_row[["DailyPnl"]][[1]]
  )
  
  if (
    !is.finite(
      our_rank
    ) ||
    our_rank < 1L
  ) {
    stop(
      "Our leaderboard Rank is invalid.",
      call. = FALSE
    )
  }
  
  if (
    !is.finite(
      our_balance
    ) ||
    our_balance <= 0
  ) {
    stop(
      "Our leaderboard Balance is invalid.",
      call. = FALSE
    )
  }
  
  if (
    !is.finite(
      our_daily_pnl
    )
  ) {
    stop(
      "Our leaderboard DailyPnl is invalid.",
      call. = FALSE
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract overall leaderboard threshold balances
  # ----------------------------------------------------------
  
  extract_overall_balance <- function(
    target_rank
  ) {
    rank_rows <- which(
      current_leaderboard[["Rank"]] ==
        target_rank
    )
    
    if (
      length(
        rank_rows
      ) != 1L
    ) {
      stop(
        paste0(
          "Expected exactly one leaderboard row for overall Rank ",
          target_rank,
          ". Found ",
          length(
            rank_rows
          ),
          "."
        ),
        call. = FALSE
      )
    }
    
    value <- as.numeric(
      current_leaderboard[["Balance"]][
        rank_rows[[1]]
      ]
    )
    
    if (
      !is.finite(
        value
      ) ||
      value <= 0
    ) {
      stop(
        paste0(
          "Overall Rank ",
          target_rank,
          " has an invalid Balance."
        ),
        call. = FALSE
      )
    }
    
    value
  }
  
  overall_place_balances <- vapply(
    TARGET_OVERALL_RANKS,
    extract_overall_balance,
    numeric(1)
  )
  
  names(
    overall_place_balances
  ) <- c(
    "balance_1st",
    "balance_2nd",
    "balance_3rd",
    "balance_4th",
    "balance_15th",
    "balance_16th"
  )
  
  if (
    any(
      diff(
        overall_place_balances
      ) > 0
    )
  ) {
    stop(
      "Extracted overall place balances are not non-increasing by rank.",
      call. = FALSE
    )
  }
  
  
  # ----------------------------------------------------------
  # Reconstruct daily leaderboard rank
  # ----------------------------------------------------------
  
  daily_prize_winner_match_counts <- vapply(
    daily_prize_winner_names,
    function(
      winner_name
    ) {
      sum(
        current_leaderboard[["DisplayName"]] ==
          winner_name,
        na.rm = TRUE
      )
    },
    integer(1)
  )
  
  if (
    any(
      daily_prize_winner_match_counts > 1L
    )
  ) {
    ambiguous_winners <- names(
      daily_prize_winner_match_counts[
        daily_prize_winner_match_counts > 1L
      ]
    )
    
    stop(
      paste0(
        "current_leaderboard.csv contains multiple rows matching prior ",
        "daily prize winner(s): ",
        paste(
          ambiguous_winners,
          collapse = ", "
        ),
        ". Daily prize eligibility cannot be identified unambiguously."
      ),
      call. = FALSE
    )
  }
  
  daily_leaderboard <- current_leaderboard[
    is.finite(
      current_leaderboard[["DailyPnl"]]
    ) &
      !is.na(
        current_leaderboard[["DisplayName"]]
      ) &
      current_leaderboard[["DisplayName"]] != "" &
      !current_leaderboard[["DisplayName"]] %in%
      daily_prize_winner_names,
    ,
    drop = FALSE
  ]
  
  daily_order <- order(
    -daily_leaderboard[["DailyPnl"]],
    daily_leaderboard[["Rank"]],
    daily_leaderboard[["DisplayName"]],
    na.last = TRUE
  )
  
  daily_leaderboard <- daily_leaderboard[
    daily_order,
    ,
    drop = FALSE
  ]
  
  daily_leaderboard[["DailyRank"]] <- seq_len(
    nrow(
      daily_leaderboard
    )
  )
  
  our_daily_rows <- which(
    daily_leaderboard[["DisplayName"]] ==
      OUR_DISPLAY_NAME
  )
  
  if (
    length(
      our_daily_rows
    ) != 1L
  ) {
    stop(
      paste0(
        "Could not uniquely identify our daily leaderboard row for '",
        OUR_DISPLAY_NAME,
        "'."
      ),
      call. = FALSE
    )
  }
  
  our_daily_rank <- as.integer(
    daily_leaderboard[["DailyRank"]][
      our_daily_rows[[1]]
    ]
  )
  
  if (
    nrow(
      daily_leaderboard
    ) < 2L
  ) {
    stop(
      "Daily leaderboard contains fewer than two usable rows.",
      call. = FALSE
    )
  }
  
  daily_1st_pnl <- as.numeric(
    daily_leaderboard[["DailyPnl"]][[1]]
  )
  
  daily_2nd_pnl <- as.numeric(
    daily_leaderboard[["DailyPnl"]][[2]]
  )
  
  if (
    !is.finite(
      daily_1st_pnl
    ) ||
    !is.finite(
      daily_2nd_pnl
    )
  ) {
    stop(
      "Daily top-two PnL values are invalid.",
      call. = FALSE
    )
  }
  
  if (
    daily_1st_pnl <
      daily_2nd_pnl
  ) {
    stop(
      "Daily leaderboard ordering validation failed.",
      call. = FALSE
    )
  }
}


# ============================================================
# Calculate actual carried margin weights
# ============================================================
#
# These are SIGNED current margin weights.
#
# Do not clip.
# Do not normalize.
# Do not force gross exposure <= 1.
#
# Grandfathered exposure above 100% is intentionally preserved.
#
# current_account.csv supplies signed positions and source margins.
# At t = 0, starting equity is known to be $25,000.
# At t >= 1, current_leaderboard.csv supplies authoritative equity.
#
# ============================================================

contracts_held <- setNames(
  current_account[["ContractsHeld"]],
  EXPECTED_FUTURES
)

source_margin_for_weight <- setNames(
  current_account[["MarginPerContract"]],
  EXPECTED_FUTURES
)

current_margin_weights <- (
  contracts_held *
    source_margin_for_weight /
    our_balance
)

if (
  any(
    !is.finite(
      current_margin_weights
    )
  )
) {
  stop(
    "Calculated current margin weights contain non-finite values.",
    call. = FALSE
  )
}

current_gross_margin_weight <- sum(
  abs(
    current_margin_weights
  )
)

current_net_margin_weight <- sum(
  current_margin_weights
)


# ============================================================
# 13. Read canonical futures price history
# ============================================================

futures_history <- readr::read_csv(
  FUTURES_HISTORY_PATH,
  show_col_types = FALSE
)

required_history_columns <- c(
  "Symbol",
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

futures_history[["Symbol"]] <- trimws(
  as.character(
    futures_history[["Symbol"]]
  )
)

futures_history[["EndTimestampCT"]] <- as.POSIXct(
  futures_history[["EndTimestampCT"]],
  tz = COMPETITION_TIME_ZONE
)

futures_history[["Close"]] <- as.numeric(
  futures_history[["Close"]]
)

if (
  any(
    is.na(
      futures_history[["EndTimestampCT"]]
    )
  )
) {
  stop(
    "futures_history.csv contains unparseable EndTimestampCT values.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      futures_history[["Close"]]
    )
  ) ||
  any(
    futures_history[["Close"]] <= 0
  )
) {
  stop(
    "futures_history.csv contains invalid Close values.",
    call. = FALSE
  )
}

# ------------------------------------------------------------
# TEST ONLY: optionally hide futures observations after a
# hard-coded historical cutoff.
# ------------------------------------------------------------

if (isTRUE(TEST_USE_FUTURES_HISTORY_CUTOFF)) {
  
  rows_before_test_cutoff <- nrow(
    futures_history
  )
  
  futures_history <- futures_history[
    futures_history[["EndTimestampCT"]] <=
      TEST_FUTURES_HISTORY_CUTOFF_CT,
    ,
    drop = FALSE
  ]
  
  rows_after_test_cutoff <- nrow(
    futures_history
  )
  
  if (rows_after_test_cutoff == 0L) {
    stop(
      paste0(
        "TEST futures-history cutoff removed every futures-price observation. ",
        "Cutoff CT = ",
        format(
          TEST_FUTURES_HISTORY_CUTOFF_CT,
          "%Y-%m-%d %H:%M:%S",
          tz = COMPETITION_TIME_ZONE
        ),
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      futures_history[["EndTimestampCT"]] >
        TEST_FUTURES_HISTORY_CUTOFF_CT
    )
  ) {
    stop(
      "TEST futures-history cutoff validation failed.",
      call. = FALSE
    )
  }
  
  cat(
    "\n*** TEST-ONLY FUTURES-HISTORY CUTOFF ACTIVE ***\n",
    "  cutoff CT: ",
    format(
      TEST_FUTURES_HISTORY_CUTOFF_CT,
      "%Y-%m-%d %H:%M:%S",
      tz = COMPETITION_TIME_ZONE
    ),
    "\n",
    "  rows before cutoff: ",
    rows_before_test_cutoff,
    "\n",
    "  rows after cutoff: ",
    rows_after_test_cutoff,
    "\n",
    "  rows hidden: ",
    rows_before_test_cutoff - rows_after_test_cutoff,
    "\n",
    "*** TEST MODE ONLY ***\n\n",
    sep = ""
  )
}

missing_price_symbols <- setdiff(
  EXPECTED_SYMBOLS,
  unique(
    futures_history[["Symbol"]]
  )
)

if (length(missing_price_symbols) > 0L) {
  stop(
    paste0(
      "futures_history.csv is missing production symbols: ",
      paste(
        missing_price_symbols,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 14. Resolve actual current decision timestamp
# ============================================================
#
# Stage 2 stores the competition day and clock time.
#
# futures_history.csv contains actual calendar timestamps.
#
# We therefore locate the most recent historical timestamp whose
# Chicago weekday and clock match the Stage 2 decision timestamp.
#
# This avoids hard-coding the competition calendar date.
#
# ============================================================

if (
  !exists(
    "LIVE_COMPETITION_SUNDAY",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  LIVE_COMPETITION_SUNDAY <- as.Date("2026-09-13")
}

competition_day_offset <- c(
  Sunday = 0L,
  Monday = 1L,
  Tuesday = 2L,
  Wednesday = 3L,
  Thursday = 4L,
  Friday = 5L
)

if (
  !current_day %in% names(competition_day_offset)
) {
  stop(
    paste0(
      "Unsupported competition day from Stage 2: ",
      current_day,
      "."
    ),
    call. = FALSE
  )
}

if (isTRUE(TEST_USE_FUTURES_HISTORY_CUTOFF)) {
  
  # Historical test mode intentionally resolves the simulated
  # competition date from the test cutoff.
  
  current_decision_date <- as.Date(
    TEST_FUTURES_HISTORY_CUTOFF_CT,
    tz = COMPETITION_TIME_ZONE
  )
  
} else {
  
  # Normal production behavior: resolve the calendar date from
  # the canonical live competition week supplied by the
  # orchestrator, rather than inferring it from price history.
  
  current_decision_date <-
    LIVE_COMPETITION_SUNDAY +
    unname(
      competition_day_offset[
        current_day
      ]
    )
}

current_decision_timestamp_ct <- as.POSIXct(
  paste(
    current_decision_date,
    stage2_normalized_time
  ),
  format = "%Y-%m-%d %H:%M",
  tz = COMPETITION_TIME_ZONE
)

if (is.na(current_decision_timestamp_ct)) {
  stop(
    paste0(
      "Could not construct current decision timestamp for ",
      current_day,
      " ",
      stage2_normalized_time,
      " CT."
    ),
    call. = FALSE
  )
}

cat(
  "Resolved current decision timestamp CT: ",
  format(
    current_decision_timestamp_ct,
    "%Y-%m-%d %H:%M:%S",
    tz = COMPETITION_TIME_ZONE
  ),
  "\n\n",
  sep = ""
)


# ============================================================
# 15. Extract latest valid price at or before current decision
# ============================================================

latest_price_rows <- vector(
  "list",
  length(
    EXPECTED_SYMBOLS
  )
)

names(
  latest_price_rows
) <- EXPECTED_SYMBOLS

for (symbol in EXPECTED_SYMBOLS) {
  symbol_rows <- futures_history[
    futures_history[["Symbol"]] == symbol &
      futures_history[["EndTimestampCT"]] <= current_decision_timestamp_ct,
    ,
    drop = FALSE
  ]
  
  if (nrow(symbol_rows) == 0L) {
    stop(
      paste0(
        "No futures price exists at or before the current decision ",
        "timestamp for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  latest_index <- which.max(
    symbol_rows[["EndTimestampCT"]]
  )
  
  latest_row <- symbol_rows[
    latest_index,
    ,
    drop = FALSE
  ]
  
  staleness_minutes <- as.numeric(
    difftime(
      current_decision_timestamp_ct,
      latest_row[["EndTimestampCT"]][[1]],
      units = "mins"
    )
  )
  
  if (
    !is.finite(
      staleness_minutes
    ) ||
    staleness_minutes < 0
  ) {
    stop(
      paste0(
        "Invalid futures-price staleness for ",
        symbol,
        "."
      ),
      call. = FALSE
    )
  }
  
  allowed_staleness_minutes <- PRICE_STALENESS_LIMIT_MINUTES

  # Scheduled intraday/overnight long-gap exception.
  if (
    current_time_ct == "00:30"
  ) {
    allowed_staleness_minutes <- SCHEDULED_LONG_GAP_LIMIT_MINUTES
  }

  # Sunday t = 0 occurs immediately after the weekend closure
  # for the non-wheat contracts. No competition-period return
  # has occurred yet, so the latest valid 30-minute endpoint may
  # still be Friday.
  if (
    current_t == 0L &&
    current_day == "Sunday" &&
    current_time_ct == "17:00" &&
    !symbol %in% WHEAT_SYMBOLS
  ) {
    allowed_staleness_minutes <- SUNDAY_T0_WEEKEND_GAP_LIMIT_MINUTES
  }

  # KE and ZW have different trading hours. They remain closed
  # on Sunday until 19:00 CT, so during the competition's
  # Sunday 17:00-19:00 decision timestamps the latest valid
  # observation can legitimately still be Friday's closing-area
  # price.
  if (
    symbol %in% WHEAT_SYMBOLS &&
    current_day == "Sunday" &&
    current_time_ct %in% c(
      "17:00",
      "17:30",
      "18:00",
      "18:30",
      "19:00"
    )
  ) {
    allowed_staleness_minutes <- WHEAT_WEEKEND_GAP_LIMIT_MINUTES
  }

  # KE and ZW also have a longer scheduled daily closure before
  # their 19:00 CT evening reopening. During that closure, an
  # older price is expected rather than evidence of a broken
  # data feed.
  if (
    symbol %in% WHEAT_SYMBOLS &&
    current_day %in% c(
      "Monday",
      "Tuesday",
      "Wednesday",
      "Thursday"
    ) &&
    current_time_ct %in% c(
      "13:30",
      "14:00",
      "14:30",
      "15:00",
      "15:30",
      "16:00",
      "16:30",
      "17:00",
      "17:30",
      "18:00",
      "18:30",
      "19:00"
    )
  ) {
    allowed_staleness_minutes <- WHEAT_DAILY_GAP_LIMIT_MINUTES
  }
  
  if (
    staleness_minutes >
    allowed_staleness_minutes
  ) {
    stop(
      paste0(
        "Latest price for ",
        symbol,
        " is ",
        round(
          staleness_minutes,
          2
        ),
        " minutes stale, exceeding the allowed ",
        allowed_staleness_minutes,
        "-minute limit for the current competition timestamp."
      ),
      call. = FALSE
    )
  }
  
  latest_price_rows[[symbol]] <- list(
    Symbol = symbol,
    TimestampCT = latest_row[["EndTimestampCT"]][[1]],
    Close = as.numeric(
      latest_row[["Close"]][[1]]
    ),
    StalenessMinutes = staleness_minutes
  )
}


# ============================================================
# 16. Calculate current per-contract dollar notionals
# ============================================================
#
# These are NOT portfolio exposures.
#
# Each value is the dollar notional of ONE canonical
# implementation contract at the start of the current horizon.
#
# ============================================================

current_contract_prices <- numeric(
  length(
    EXPECTED_FUTURES
  )
)

names(
  current_contract_prices
) <- EXPECTED_FUTURES

current_contract_notionals <- numeric(
  length(
    EXPECTED_FUTURES
  )
)

names(
  current_contract_notionals
) <- EXPECTED_FUTURES

price_staleness_minutes <- numeric(
  length(
    EXPECTED_FUTURES
  )
)

names(
  price_staleness_minutes
) <- EXPECTED_FUTURES

for (i in seq_along(EXPECTED_SYMBOLS)) {
  symbol <- EXPECTED_SYMBOLS[[i]]
  future_name <- EXPECTED_FUTURES[[i]]
  
  current_contract_prices[[future_name]] <- latest_price_rows[[symbol]][["Close"]]
  
  price_staleness_minutes[[future_name]] <- latest_price_rows[[symbol]][["StalenessMinutes"]]
  
  current_contract_notionals[[future_name]] <- (
    current_contract_prices[[future_name]] *
      PRICE_NOTIONAL_MULTIPLIERS[[future_name]]
  )
}

if (
  any(
    !is.finite(
      current_contract_notionals
    )
  ) ||
  any(
    current_contract_notionals <= 0
  )
) {
  stop(
    "Calculated current contract notionals are invalid.",
    call. = FALSE
  )
}


# ============================================================
# 17. Build current_state.csv
# ============================================================

current_state <- data.frame(
  our_balance = our_balance,
  our_rank = our_rank,
  balance_1st = overall_place_balances[["balance_1st"]],
  balance_2nd = overall_place_balances[["balance_2nd"]],
  balance_3rd = overall_place_balances[["balance_3rd"]],
  balance_4th = overall_place_balances[["balance_4th"]],
  balance_15th = overall_place_balances[["balance_15th"]],
  balance_16th = overall_place_balances[["balance_16th"]],
  our_daily_pnl = our_daily_pnl,
  our_daily_rank = our_daily_rank,
  daily_1st_pnl = daily_1st_pnl,
  daily_2nd_pnl = daily_2nd_pnl,
  weight_Future_1 = current_margin_weights[["Future_1"]],
  weight_Future_2 = current_margin_weights[["Future_2"]],
  weight_Future_3 = current_margin_weights[["Future_3"]],
  weight_Future_4 = current_margin_weights[["Future_4"]],
  weight_Future_5 = current_margin_weights[["Future_5"]],
  weight_Future_6 = current_margin_weights[["Future_6"]],
  weight_Future_7 = current_margin_weights[["Future_7"]],
  weight_Future_8 = current_margin_weights[["Future_8"]],
  notional_Future_1 = current_contract_notionals[["Future_1"]],
  notional_Future_2 = current_contract_notionals[["Future_2"]],
  notional_Future_3 = current_contract_notionals[["Future_3"]],
  notional_Future_4 = current_contract_notionals[["Future_4"]],
  notional_Future_5 = current_contract_notionals[["Future_5"]],
  notional_Future_6 = current_contract_notionals[["Future_6"]],
  notional_Future_7 = current_contract_notionals[["Future_7"]],
  notional_Future_8 = current_contract_notionals[["Future_8"]],
  stringsAsFactors = FALSE,
  check.names = FALSE
)

EXPECTED_CURRENT_STATE_COLUMNS <- c(
  "our_balance",
  "our_rank",
  "balance_1st",
  "balance_2nd",
  "balance_3rd",
  "balance_4th",
  "balance_15th",
  "balance_16th",
  "our_daily_pnl",
  "our_daily_rank",
  "daily_1st_pnl",
  "daily_2nd_pnl",
  paste0(
    "weight_Future_",
    seq_len(8L)
  ),
  paste0(
    "notional_Future_",
    seq_len(8L)
  )
)

if (
  !identical(
    names(
      current_state
    ),
    EXPECTED_CURRENT_STATE_COLUMNS
  )
) {
  stop(
    "current_state.csv does not have the exact required 28-column schema.",
    call. = FALSE
  )
}

if (
  nrow(
    current_state
  ) != 1L
) {
  stop(
    "current_state.csv must contain exactly one row.",
    call. = FALSE
  )
}

if (
  ncol(
    current_state
  ) != 28L
) {
  stop(
    "current_state.csv must contain exactly 28 columns.",
    call. = FALSE
  )
}

current_state_numeric <- as.numeric(
  current_state[
    1,
    ,
    drop = TRUE
  ]
)

if (
  any(
    !is.finite(
      current_state_numeric
    )
  )
) {
  stop(
    "current_state.csv contains non-finite values.",
    call. = FALSE
  )
}


# ============================================================
# 18. Internal weight equivalence validation
# ============================================================
#
# For micro implementations:
#
#   standard-equivalent quantity * standard margin
#
# must equal:
#
#   micro quantity * micro margin
#
# where:
#
#   micro quantity =
#     standard-equivalent quantity / margin scale factor
#
# ============================================================

micro_futures <- c(
  "Future_1",
  "Future_2",
  "Future_8"
)

for (future_name in micro_futures) {
  scale_factor <- MARGIN_SCALE_FACTORS[[future_name]]
  
  standard_equivalent_quantity <- contracts_held[[future_name]]
  
  standard_margin <- source_margin_for_weight[[future_name]]
  
  micro_quantity <- (
    standard_equivalent_quantity /
      scale_factor
  )
  
  exact_scaled_micro_margin <- (
    standard_margin *
      scale_factor
  )
  
  standard_margin_commitment <- (
    standard_equivalent_quantity *
      standard_margin
  )
  
  equivalent_micro_margin_commitment <- (
    micro_quantity *
      exact_scaled_micro_margin
  )
  
  equivalence_error <- abs(
    standard_margin_commitment -
      equivalent_micro_margin_commitment
  )
  
  if (
    equivalence_error >
    1e-10 *
    max(
      1,
      abs(
        standard_margin_commitment
      )
    )
  ) {
    stop(
      paste0(
        "Margin-weight equivalence validation failed for ",
        future_name,
        "."
      ),
      call. = FALSE
    )
  }
}


# ============================================================
# 19. Explicit CL hand check
# ============================================================

cl_expected_weight <- (
  current_account[["ContractsHeld"]][
    current_account[["Symbol"]] == "CL"
  ][[1]] *
    current_account[["MarginPerContract"]][
      current_account[["Symbol"]] == "CL"
    ][[1]] /
    our_balance
)

cl_actual_weight <- current_margin_weights[["Future_1"]]

if (
  abs(
    cl_expected_weight -
    cl_actual_weight
  ) > 1e-12
) {
  stop(
    "CL current margin-weight hand check failed.",
    call. = FALSE
  )
}


# ============================================================
# 20. Write margin.csv and current_state.csv
# ============================================================

readr::write_csv(
  margin_csv,
  MARGIN_OUTPUT_PATH,
  na = ""
)

readr::write_csv(
  current_state,
  CURRENT_STATE_OUTPUT_PATH,
  na = ""
)

if (
  !file.exists(
    MARGIN_OUTPUT_PATH
  )
) {
  stop(
    "margin.csv was not written.",
    call. = FALSE
  )
}

if (
  !file.exists(
    CURRENT_STATE_OUTPUT_PATH
  )
) {
  stop(
    "current_state.csv was not written.",
    call. = FALSE
  )
}


# ============================================================
# 21. Reload written files
# ============================================================

margin_reload <- readr::read_csv(
  MARGIN_OUTPUT_PATH,
  show_col_types = FALSE
)

current_state_reload <- readr::read_csv(
  CURRENT_STATE_OUTPUT_PATH,
  show_col_types = FALSE
)


# ============================================================
# 22. Strict margin.csv round-trip validation
# ============================================================

if (
  !identical(
    names(
      margin_reload
    ),
    expected_margin_columns
  )
) {
  stop(
    "Reloaded margin.csv column schema changed.",
    call. = FALSE
  )
}

if (
  nrow(
    margin_reload
  ) != EXPECTED_T
) {
  stop(
    "Reloaded margin.csv does not contain 147 rows.",
    call. = FALSE
  )
}

if (
  !identical(
    as.integer(
      margin_reload[["t"]]
    ),
    0:(EXPECTED_T - 1L)
  )
) {
  stop(
    "Reloaded margin.csv t index is invalid.",
    call. = FALSE
  )
}

margin_reload_matrix <- as.matrix(
  margin_reload[
    ,
    EXPECTED_FUTURES,
    drop = FALSE
  ]
)

storage.mode(
  margin_reload_matrix
) <- "double"

if (
  any(
    !is.finite(
      margin_reload_matrix
    )
  ) ||
  any(
    margin_reload_matrix <= 0
  )
) {
  stop(
    "Reloaded margin.csv contains invalid values.",
    call. = FALSE
  )
}

margin_roundtrip_error <- max(
  abs(
    margin_reload_matrix -
      margin_matrix
  )
)

if (
  margin_roundtrip_error >
  1e-12
) {
  stop(
    paste0(
      "margin.csv round-trip error exceeds tolerance: ",
      margin_roundtrip_error
    ),
    call. = FALSE
  )
}


# ============================================================
# 23. Strict current_state.csv round-trip validation
# ============================================================

if (
  !identical(
    names(
      current_state_reload
    ),
    EXPECTED_CURRENT_STATE_COLUMNS
  )
) {
  stop(
    "Reloaded current_state.csv column schema changed.",
    call. = FALSE
  )
}

if (
  nrow(
    current_state_reload
  ) != 1L
) {
  stop(
    "Reloaded current_state.csv does not contain exactly one row.",
    call. = FALSE
  )
}

if (
  ncol(
    current_state_reload
  ) != 28L
) {
  stop(
    "Reloaded current_state.csv does not contain exactly 28 columns.",
    call. = FALSE
  )
}

reloaded_current_state_numeric <- as.numeric(
  current_state_reload[
    1,
    ,
    drop = TRUE
  ]
)

if (
  any(
    !is.finite(
      reloaded_current_state_numeric
    )
  )
) {
  stop(
    "Reloaded current_state.csv contains non-finite values.",
    call. = FALSE
  )
}

current_state_roundtrip_error <- max(
  abs(
    reloaded_current_state_numeric -
      current_state_numeric
  )
)

if (
  current_state_roundtrip_error >
  1e-10
) {
  stop(
    paste0(
      "current_state.csv round-trip error exceeds tolerance: ",
      current_state_roundtrip_error
    ),
    call. = FALSE
  )
}


# ============================================================
# 24. Validate Python-facing position-weight columns
# ============================================================

weight_columns <- paste0(
  "weight_Future_",
  seq_len(8L)
)

reloaded_weights <- as.numeric(
  current_state_reload[
    1,
    weight_columns,
    drop = TRUE
  ]
)

if (
  length(
    reloaded_weights
  ) != 8L
) {
  stop(
    "Python-facing current weight vector does not have length 8.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      reloaded_weights
    )
  )
) {
  stop(
    "Python-facing current weight vector contains non-finite values.",
    call. = FALSE
  )
}

weight_serialization_error <- max(
  abs(
    reloaded_weights -
      as.numeric(
        current_margin_weights
      )
  )
)

if (
  weight_serialization_error >
  1e-10
) {
  stop(
    "Current position weights changed during serialization.",
    call. = FALSE
  )
}


# ============================================================
# 25. Validate Python-facing current contract notionals
# ============================================================

notional_columns <- paste0(
  "notional_Future_",
  seq_len(8L)
)

reloaded_notionals <- as.numeric(
  current_state_reload[
    1,
    notional_columns,
    drop = TRUE
  ]
)

if (
  length(
    reloaded_notionals
  ) != 8L
) {
  stop(
    "Python-facing current contract notional vector does not have length 8.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      reloaded_notionals
    )
  ) ||
  any(
    reloaded_notionals <= 0
  )
) {
  stop(
    "Python-facing current contract notionals are invalid.",
    call. = FALSE
  )
}

notional_serialization_error <- max(
  abs(
    reloaded_notionals -
      as.numeric(
        current_contract_notionals
      )
  )
)

if (
  notional_serialization_error >
  1e-8
) {
  stop(
    "Current contract notionals changed during serialization.",
    call. = FALSE
  )
}


# ============================================================
# 26. Build Stage 7 audit tables
# ============================================================

stage7_contract_audit <- tibble::tibble(
  Future = EXPECTED_FUTURES,
  Symbol = EXPECTED_SYMBOLS,
  ImplementationSymbol = unname(
    IMPLEMENTATION_SYMBOLS[
      EXPECTED_FUTURES
    ]
  ),
  ContractsHeldStandardEquivalent = as.numeric(
    contracts_held[
      EXPECTED_FUTURES
    ]
  ),
  SourceMarginPerContract = as.numeric(
    current_source_margins[
      EXPECTED_FUTURES
    ]
  ),
  MarginScaleFactor = as.numeric(
    MARGIN_SCALE_FACTORS[
      EXPECTED_FUTURES
    ]
  ),
  ImplementationMarginPerContract = as.numeric(
    current_implementation_margins[
      EXPECTED_FUTURES
    ]
  ),
  CurrentMarginWeight = as.numeric(
    current_margin_weights[
      EXPECTED_FUTURES
    ]
  ),
  CurrentPrice = as.numeric(
    current_contract_prices[
      EXPECTED_FUTURES
    ]
  ),
  PriceNotionalMultiplier = as.numeric(
    PRICE_NOTIONAL_MULTIPLIERS[
      EXPECTED_FUTURES
    ]
  ),
  CurrentContractNotional = as.numeric(
    current_contract_notionals[
      EXPECTED_FUTURES
    ]
  ),
  PriceStalenessMinutes = as.numeric(
    price_staleness_minutes[
      EXPECTED_FUTURES
    ]
  )
)

stage7_state_audit <- tibble::tibble(
  DisplayName = OUR_DISPLAY_NAME,
  CurrentT = current_t,
  Day = current_day,
  TimeCT = stage2_normalized_time,
  DecisionTimestampCT = format(
    current_decision_timestamp_ct,
    "%Y-%m-%d %H:%M:%S",
    tz = COMPETITION_TIME_ZONE
  ),
  OurBalance = our_balance,
  OurRank = our_rank,
  OurDailyPnl = our_daily_pnl,
  OurDailyRank = our_daily_rank,
  Daily1stPnl = daily_1st_pnl,
  Daily2ndPnl = daily_2nd_pnl,
  CurrentGrossMarginWeight = current_gross_margin_weight,
  CurrentNetMarginWeight = current_net_margin_weight
)


# ============================================================
# 27. Build canonical Stage 7 system object
# ============================================================

live_margin_and_current_state_system <- list(
  Version = "1.0.0",
  Stage = "7",
  CurrentT = current_t,
  CurrentDay = current_day,
  CurrentTimeCT = stage2_normalized_time,
  CurrentDecisionTimestampCT = current_decision_timestamp_ct,
  DisplayName = OUR_DISPLAY_NAME,
  FutureOrder = EXPECTED_FUTURES,
  SymbolOrder = EXPECTED_SYMBOLS,
  ImplementationSymbols = IMPLEMENTATION_SYMBOLS,
  PriceNotionalMultipliers = PRICE_NOTIONAL_MULTIPLIERS,
  MarginScaleFactors = MARGIN_SCALE_FACTORS,
  MarginForecastModel = "persistence",
  CurrentSourceMargins = current_source_margins,
  CurrentImplementationMargins = current_implementation_margins,
  CurrentMarginWeights = current_margin_weights,
  CurrentGrossMarginWeight = current_gross_margin_weight,
  CurrentNetMarginWeight = current_net_margin_weight,
  CurrentContractPrices = current_contract_prices,
  CurrentContractNotionals = current_contract_notionals,
  OverallPlaceBalances = overall_place_balances,
  OurRank = our_rank,
  OurDailyPnl = our_daily_pnl,
  OurDailyRank = our_daily_rank,
  CurrentDailyPlacePnl = c(
    DailyRank1 = daily_1st_pnl,
    DailyRank2 = daily_2nd_pnl
  ),
  MarginCSV = margin_csv,
  CurrentStateCSV = current_state,
  ContractAudit = stage7_contract_audit,
  StateAudit = stage7_state_audit
)


# ============================================================
# 28. Final validation flag
# ============================================================

LIVE_MARGIN_AND_CURRENT_STATE_VALIDATED <- TRUE


# ============================================================
# 29. Final summary
# ============================================================

cat(
  "\n============================================================\n"
)

cat(
  "LIVE MARGIN AND CURRENT STATE STAGE COMPLETE\n"
)

cat(
  "============================================================\n\n"
)

cat(
  "DisplayName: ",
  OUR_DISPLAY_NAME,
  "\n",
  sep = ""
)

cat(
  "Current t: ",
  current_t,
  "\n",
  sep = ""
)

cat(
  "Current decision timestamp CT: ",
  format(
    current_decision_timestamp_ct,
    "%Y-%m-%d %H:%M:%S",
    tz = COMPETITION_TIME_ZONE
  ),
  "\n",
  sep = ""
)

cat(
  "Our balance: ",
  format(
    our_balance,
    scientific = FALSE
  ),
  "\n",
  sep = ""
)

cat(
  "Our overall rank: ",
  our_rank,
  "\n",
  sep = ""
)

cat(
  "Our daily PnL: ",
  format(
    our_daily_pnl,
    scientific = FALSE
  ),
  "\n",
  sep = ""
)

cat(
  "Our daily rank: ",
  our_daily_rank,
  "\n",
  sep = ""
)

cat(
  "Current gross margin weight: ",
  format(
    current_gross_margin_weight,
    digits = 12
  ),
  "\n",
  sep = ""
)

cat(
  "Current net margin weight: ",
  format(
    current_net_margin_weight,
    digits = 12
  ),
  "\n\n",
  sep = ""
)

cat(
  "Current implementation margins:\n"
)

print(
  current_implementation_margins
)

cat(
  "\nCurrent signed margin weights:\n"
)

print(
  current_margin_weights
)

cat(
  "\nCurrent contract notionals:\n"
)

print(
  current_contract_notionals
)

cat(
  "\nCurrent overall threshold balances:\n"
)

print(
  overall_place_balances
)

cat(
  "\nCurrent daily top-two PnL:\n"
)

print(
  c(
    DailyRank1 = daily_1st_pnl,
    DailyRank2 = daily_2nd_pnl
  )
)

cat(
  "\nContract audit:\n"
)

print(
  stage7_contract_audit,
  n = Inf
)

cat(
  "\nState audit:\n"
)

print(
  stage7_state_audit,
  n = Inf
)

cat(
  "\nOutput files:\n"
)

cat(
  "  ",
  MARGIN_OUTPUT_PATH,
  "\n",
  sep = ""
)

cat(
  "  ",
  CURRENT_STATE_OUTPUT_PATH,
  "\n\n",
  sep = ""
)

cat(
  "margin.csv rows: ",
  nrow(
    margin_reload
  ),
  "\n",
  sep = ""
)

cat(
  "current_state.csv rows: ",
  nrow(
    current_state_reload
  ),
  "\n",
  sep = ""
)

cat(
  "current_state.csv columns: ",
  ncol(
    current_state_reload
  ),
  "\n",
  sep = ""
)

cat(
  "Margin round-trip max error: ",
  format(
    margin_roundtrip_error,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Current-state round-trip max error: ",
  format(
    current_state_roundtrip_error,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Weight serialization max error: ",
  format(
    weight_serialization_error,
    scientific = TRUE
  ),
  "\n",
  sep = ""
)

cat(
  "Notional serialization max error: ",
  format(
    notional_serialization_error,
    scientific = TRUE
  ),
  "\n\n",
  sep = ""
)

cat(
  "LIVE_MARGIN_AND_CURRENT_STATE_VALIDATED = TRUE\n"
)

cat(
  "============================================================\n"
)
