# ============================================================
# scripts/live/run_live_pipeline.R
# ============================================================
#
# Purpose:
#
#   Run the complete validated live R production pipeline with
#   one command.
#
#   This script DOES NOT change the calculations performed by
#   the individual live production scripts. It only orchestrates
#   them in the required dependency order.
#
# Pipeline:
#
#   1. build_futures_history.R
#   2. build_futures_roll_history.R
#   3. build_live_competition_clock.R
#   4. build_live_futures_returns.R
#   5. build_live_futures_forecasts.R
#   6. build_live_competitor_forecasts.R
#   7. build_live_dependence_and_innovations.R
#   8. build_live_margin_and_current_state.R
#
# Final production outputs:
#
#   mu.csv
#   Sigma.csv
#   state_mean_competition.csv
#   state_cov_competition.csv
#   state_futures_cov_competition.csv
#   innovation_marginals.csv
#   innovation_latent_correlation.csv
#   innovation_arithmetic_mapping.csv
#   innovation_system_metadata.csv
#   margin.csv
#   current_state.csv
#
# Important:
#
#   - Scripts are sourced sequentially into .GlobalEnv.
#   - If any stage fails, the pipeline stops immediately.
#   - Later stages are NOT run after a failed stage.
#   - Each stage is timed.
#   - The final 11 optimizer files are checked for existence.
#   - The final optimizer files are checked for nonzero size.
#   - No frozen model-selection logic is changed here.
#
# ============================================================

# ============================================================
# Live futures-data source mode
# ============================================================
#
# "BLOOMBERG"
#   Preserve the original production pipeline.
#
# "YAHOO"
#   Run the Yahoo updater first, then run the downstream
#   production R pipeline from the competition clock onward.
#
# ============================================================

if (
  !exists(
    "LIVE_FUTURES_DATA_SOURCE",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  LIVE_FUTURES_DATA_SOURCE <- "YAHOO"
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

# ============================================================
# Live competition calendar
# ============================================================

LIVE_COMPETITION_SUNDAY <- as.Date("2026-09-27")
LIVE_COMPETITION_FRIDAY <- LIVE_COMPETITION_SUNDAY + 5L

# ============================================================
# 1. Project paths
# ============================================================

PROJECT_ROOT <- here::here()

LIVE_SCRIPT_DIR <- file.path(
  PROJECT_ROOT,
  "scripts",
  "live"
)

OPTIMIZER_INPUT_DIR <- file.path(PROJECT_ROOT, "outputs", "optimizer_inputs")

YAHOO_UPDATER_PATH <- file.path(
  PROJECT_ROOT,
  "scripts",
  "live",
  "update_yahoo_futures.py"
)

if (!dir.exists(LIVE_SCRIPT_DIR)) {
  stop(
    paste0(
      "Live script directory does not exist:\n",
      LIVE_SCRIPT_DIR
    ),
    call. = FALSE
  )
}

if (!dir.exists(OPTIMIZER_INPUT_DIR)) {
  dir.create(
    OPTIMIZER_INPUT_DIR,
    recursive = TRUE,
    showWarnings = FALSE
  )
}


# ============================================================
# 2. Define live production stages
# ============================================================

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "BLOOMBERG"
) {
  
  LIVE_PIPELINE_STAGES <- data.frame(
    StageNumber = 1:9,
    StageName = c(
      "Workbook -> raw CSVs",
      "Futures history",
      "Futures roll history",
      "Competition clock",
      "Futures returns",
      "Futures forecasts",
      "Competitor forecasts",
      "Dependence + innovations",
      "Margin + current state"
    ),
    ScriptName = c(
      "extract_futures_live_update.R",
      "build_futures_history.R",
      "build_futures_roll_history.R",
      "build_live_competition_clock.R",
      "build_live_futures_returns.R",
      "build_live_futures_forecasts.R",
      "build_live_competitor_forecasts.R",
      "build_live_dependence_and_innovations.R",
      "build_live_margin_and_current_state.R"
    ),
    StageType = rep(
      "R",
      9L
    ),
    stringsAsFactors = FALSE
  )
  
} else {
  
  LIVE_PIPELINE_STAGES <- data.frame(
    StageNumber = 1:7,
    StageName = c(
      "Yahoo futures update",
      "Competition clock",
      "Futures returns",
      "Futures forecasts",
      "Competitor forecasts",
      "Dependence + innovations",
      "Margin + current state"
    ),
    ScriptName = c(
      "update_yahoo_futures.py",
      "build_live_competition_clock.R",
      "build_live_futures_returns.R",
      "build_live_futures_forecasts.R",
      "build_live_competitor_forecasts.R",
      "build_live_dependence_and_innovations.R",
      "build_live_margin_and_current_state.R"
    ),
    StageType = c(
      "PYTHON",
      rep(
        "R",
        6L
      )
    ),
    stringsAsFactors = FALSE
  )
}

LIVE_PIPELINE_STAGES[["ScriptPath"]] <-
  ifelse(
    LIVE_PIPELINE_STAGES[["StageType"]] ==
      "PYTHON",
    YAHOO_UPDATER_PATH,
    file.path(
      LIVE_SCRIPT_DIR,
      LIVE_PIPELINE_STAGES[["ScriptName"]]
    )
  )

# ============================================================
# 3. Validate that every required live script exists
# ============================================================

missing_live_scripts <- LIVE_PIPELINE_STAGES[
  !file.exists(
    LIVE_PIPELINE_STAGES[["ScriptPath"]]
  ),
  ,
  drop = FALSE
]

if (nrow(missing_live_scripts) > 0L) {
  stop(
    paste0(
      "Missing required live production script(s):\n",
      paste(
        missing_live_scripts[["ScriptPath"]],
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

cat(
  "\n",
  "============================================================\n",
  "LIVE PRODUCTION PIPELINE\n",
  "============================================================\n",
  "Futures data source: ",
  LIVE_FUTURES_DATA_SOURCE,
  "\n",
  "Project root: ",
  PROJECT_ROOT,
  "\n",
  "Live script directory: ",
  LIVE_SCRIPT_DIR,
  "\n",
  "Optimizer input directory: ",
  OPTIMIZER_INPUT_DIR,
  "\n",
  "All 9 required live scripts found: TRUE\n",
  "============================================================\n",
  sep = ""
)


# ============================================================
# 4. Stage timing storage
# ============================================================

stage_runtime_seconds <- rep(
  NA_real_,
  nrow(LIVE_PIPELINE_STAGES)
)

stage_status <- rep(
  "NOT RUN",
  nrow(LIVE_PIPELINE_STAGES)
)

pipeline_start_time <- Sys.time()


# ============================================================
# 5. Helper to run one stage
# ============================================================

run_live_stage <- function(
    stage_number,
    stage_name,
    script_path,
    stage_type
) {
  cat(
    "\n",
    "============================================================\n",
    "[",
    stage_number,
    "/",
    nrow(LIVE_PIPELINE_STAGES),
    "] ",
    stage_name,
    "\n",
    "============================================================\n",
    "Script: ",
    script_path,
    "\n",
    sep = ""
  )
  
  stage_start_time <- Sys.time()
  
  stage_error <- NULL
  
  tryCatch(
    {
      if (
        stage_type ==
        "R"
      ) {
        
        source(
          script_path,
          local = .GlobalEnv,
          echo = FALSE,
          chdir = FALSE
        )
        
      } else if (
        stage_type ==
        "PYTHON"
      ) {
        
        python_executable <-
          "/Applications/Xcode.app/Contents/Developer/usr/bin/python3"
        
        if (
          !file.exists(
            python_executable
          )
        ) {
          stop(
            paste0(
              "Configured Python executable does not exist:\n",
              python_executable
            ),
            call. = FALSE
          )
        }
        
        yahoo_status <- system2(
          command = python_executable,
          args = shQuote(
            script_path
          ),
          stdout = "",
          stderr = ""
        )
        
        if (
          !identical(
            as.integer(
              yahoo_status
            ),
            0L
          )
        ) {
          stop(
            paste0(
              "Yahoo updater exited with status ",
              yahoo_status,
              "."
            ),
            call. = FALSE
          )
        }
        
      } else {
        
        stop(
          paste0(
            "Unsupported pipeline stage type: ",
            stage_type
          ),
          call. = FALSE
        )
      }
    },
    error = function(e) {
      stage_error <<- e
    }
  )
  
  stage_end_time <- Sys.time()
  
  elapsed_seconds <- as.numeric(
    difftime(
      stage_end_time,
      stage_start_time,
      units = "secs"
    )
  )
  
  if (!is.null(stage_error)) {
    cat(
      "\n",
      "============================================================\n",
      "PIPELINE STOPPED\n",
      "============================================================\n",
      "Failed stage: ",
      stage_number,
      " - ",
      stage_name,
      "\n",
      "Runtime before failure: ",
      sprintf(
        "%.2f",
        elapsed_seconds
      ),
      " seconds\n",
      "Error:\n",
      conditionMessage(
        stage_error
      ),
      "\n",
      "============================================================\n",
      sep = ""
    )
    
    stop(
      paste0(
        "Live pipeline failed during Stage ",
        stage_number,
        ": ",
        stage_name,
        "."
      ),
      call. = FALSE
    )
  }
  
  cat(
    "\n",
    "[",
    stage_number,
    "/",
    nrow(LIVE_PIPELINE_STAGES),
    "] ",
    stage_name,
    " COMPLETE - ",
    sprintf(
      "%.2f",
      elapsed_seconds
    ),
    " seconds\n",
    sep = ""
  )
  
  elapsed_seconds
}


# ============================================================
# 6. Run all eight live stages sequentially
# ============================================================

for (
  stage_index in seq_len(
    nrow(LIVE_PIPELINE_STAGES)
  )
) {
  stage_number <- LIVE_PIPELINE_STAGES[["StageNumber"]][[stage_index]]
  
  stage_name <- LIVE_PIPELINE_STAGES[["StageName"]][[stage_index]]
  
  script_path <- LIVE_PIPELINE_STAGES[["ScriptPath"]][[stage_index]]
  
  stage_type <- LIVE_PIPELINE_STAGES[["StageType"]][[stage_index]]
  
  stage_runtime_seconds[[stage_index]] <- run_live_stage(
    stage_number = stage_number,
    stage_name = stage_name,
    script_path = script_path,
    stage_type = stage_type
  )
  
  stage_status[[stage_index]] <- "PASS"
}


# ============================================================
# 7. Require final validation flags from the validated stages
# ============================================================

if (
  LIVE_FUTURES_DATA_SOURCE ==
  "BLOOMBERG"
) {
  
  required_validation_flags <- c(
    "LIVE_FUTURES_WORKBOOK_EXTRACTED",
    "LIVE_COMPETITION_CLOCK_VALIDATED",
    "LIVE_FUTURES_RETURNS_VALIDATED",
    "LIVE_FUTURES_FORECASTS_VALIDATED",
    "LIVE_COMPETITOR_FORECASTS_VALIDATED"
  )
  
} else {
  
  required_validation_flags <- c(
    "LIVE_COMPETITION_CLOCK_VALIDATED",
    "LIVE_FUTURES_RETURNS_VALIDATED",
    "LIVE_FUTURES_FORECASTS_VALIDATED",
    "LIVE_COMPETITOR_FORECASTS_VALIDATED"
  )
}

missing_validation_flags <- required_validation_flags[
  !vapply(
    required_validation_flags,
    function(flag_name) {
      exists(
        flag_name,
        envir = .GlobalEnv,
        inherits = FALSE
      )
    },
    logical(1)
  )
]

if (length(missing_validation_flags) > 0L) {
  stop(
    paste0(
      "Pipeline completed its scripts, but required validation ",
      "flag(s) are missing:\n",
      paste(
        missing_validation_flags,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

failed_validation_flags <- required_validation_flags[
  !vapply(
    required_validation_flags,
    function(flag_name) {
      isTRUE(
        get(
          flag_name,
          envir = .GlobalEnv,
          inherits = FALSE
        )
      )
    },
    logical(1)
  )
]

if (length(failed_validation_flags) > 0L) {
  stop(
    paste0(
      "Pipeline completed its scripts, but required validation ",
      "flag(s) are not TRUE:\n",
      paste(
        failed_validation_flags,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 8. Extract final live clock information
# ============================================================

if (
  !exists(
    "live_competition_clock",
    envir = .GlobalEnv,
    inherits = FALSE
  )
) {
  stop(
    "live_competition_clock is missing after pipeline execution.",
    call. = FALSE
  )
}

if (
  !is.list(
    live_competition_clock
  )
) {
  stop(
    "live_competition_clock is not a list.",
    call. = FALSE
  )
}

required_clock_fields <- c(
  "T",
  "current_t",
  "current_day",
  "current_time_ct"
)

missing_clock_fields <- setdiff(
  required_clock_fields,
  names(
    live_competition_clock
  )
)

if (length(missing_clock_fields) > 0L) {
  stop(
    paste0(
      "live_competition_clock is missing required field(s): ",
      paste(
        missing_clock_fields,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

final_T <- as.integer(
  live_competition_clock[["T"]]
)

final_current_t <- as.integer(
  live_competition_clock[["current_t"]]
)

final_current_day <- as.character(
  live_competition_clock[["current_day"]]
)

final_current_time_ct <- as.character(
  live_competition_clock[["current_time_ct"]]
)

if (
  length(final_T) != 1L ||
  !is.finite(final_T) ||
  final_T != 147L
) {
  stop(
    paste0(
      "Final live clock has invalid T: ",
      final_T
    ),
    call. = FALSE
  )
}

if (
  length(final_current_t) != 1L ||
  !is.finite(final_current_t) ||
  final_current_t < 0L ||
  final_current_t >= final_T
) {
  stop(
    paste0(
      "Final live clock has invalid current_t: ",
      final_current_t
    ),
    call. = FALSE
  )
}


# ============================================================
# 9. Define exact 11 optimizer production files
# ============================================================

EXPECTED_OPTIMIZER_FILES <- c(
  "mu.csv",
  "Sigma.csv",
  "state_mean_competition.csv",
  "state_cov_competition.csv",
  "state_futures_cov_competition.csv",
  "innovation_marginals.csv",
  "innovation_latent_correlation.csv",
  "innovation_arithmetic_mapping.csv",
  "innovation_system_metadata.csv",
  "margin.csv",
  "current_state.csv"
)

EXPECTED_OPTIMIZER_PATHS <- file.path(
  OPTIMIZER_INPUT_DIR,
  EXPECTED_OPTIMIZER_FILES
)


# ============================================================
# 10. Validate existence of all 11 production files
# ============================================================

optimizer_files_exist <- file.exists(
  EXPECTED_OPTIMIZER_PATHS
)

if (!all(optimizer_files_exist)) {
  missing_optimizer_files <- EXPECTED_OPTIMIZER_PATHS[
    !optimizer_files_exist
  ]
  
  stop(
    paste0(
      "Live pipeline did not produce all 11 optimizer files.\n",
      "Missing file(s):\n",
      paste(
        missing_optimizer_files,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 11. Validate all 11 files are nonempty
# ============================================================

optimizer_file_info <- file.info(
  EXPECTED_OPTIMIZER_PATHS
)

optimizer_file_sizes <- optimizer_file_info[["size"]]

if (
  any(
    is.na(
      optimizer_file_sizes
    )
  )
) {
  stop(
    "Could not determine size of one or more optimizer files.",
    call. = FALSE
  )
}

if (
  any(
    optimizer_file_sizes <= 0
  )
) {
  empty_optimizer_files <- EXPECTED_OPTIMIZER_PATHS[
    optimizer_file_sizes <= 0
  ]
  
  stop(
    paste0(
      "One or more optimizer production files are empty:\n",
      paste(
        empty_optimizer_files,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 12. Validate modification times are from this pipeline run
# ============================================================
#
# This protects against a stage silently leaving behind an old
# optimizer file that happens to exist from an earlier run.
#
# A small one-second tolerance is allowed for filesystem time
# precision.
#
# ============================================================

optimizer_file_mtimes <- optimizer_file_info[["mtime"]]

freshness_cutoff <- pipeline_start_time - 1

stale_optimizer_files <- EXPECTED_OPTIMIZER_PATHS[
  is.na(
    optimizer_file_mtimes
  ) |
    optimizer_file_mtimes < freshness_cutoff
]

if (length(stale_optimizer_files) > 0L) {
  stop(
    paste0(
      "One or more optimizer files were not freshly written ",
      "during this pipeline run:\n",
      paste(
        stale_optimizer_files,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}


# ============================================================
# 13. Build stage timing summary
# ============================================================

pipeline_end_time <- Sys.time()

total_pipeline_seconds <- as.numeric(
  difftime(
    pipeline_end_time,
    pipeline_start_time,
    units = "secs"
  )
)

stage_timing_summary <- data.frame(
  Stage = LIVE_PIPELINE_STAGES[["StageNumber"]],
  Name = LIVE_PIPELINE_STAGES[["StageName"]],
  Status = stage_status,
  Seconds = round(
    stage_runtime_seconds,
    2
  ),
  stringsAsFactors = FALSE
)


# ============================================================
# 14. Print stage timing summary
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "LIVE PIPELINE STAGE TIMING\n",
  "============================================================\n",
  sep = ""
)

for (
  stage_index in seq_len(
    nrow(stage_timing_summary)
  )
) {
  cat(
    sprintf(
      "[%d/%d] %-28s %8.2f sec   %s\n",
      stage_timing_summary[["Stage"]][[stage_index]],
      nrow(stage_timing_summary),
      stage_timing_summary[["Name"]][[stage_index]],
      stage_timing_summary[["Seconds"]][[stage_index]],
      stage_timing_summary[["Status"]][[stage_index]]
    )
  )
}


# ============================================================
# 15. Print final optimizer file summary
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "FINAL OPTIMIZER FILES\n",
  "============================================================\n",
  sep = ""
)

for (
  file_index in seq_along(
    EXPECTED_OPTIMIZER_FILES
  )
) {
  cat(
    sprintf(
      "[%02d/11] %-40s %10d bytes\n",
      file_index,
      EXPECTED_OPTIMIZER_FILES[[file_index]],
      as.integer(
        optimizer_file_sizes[[file_index]]
      )
    )
  )
}


# ============================================================
# 16. Final success message
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "LIVE PIPELINE COMPLETE\n",
  "============================================================\n",
  "Decision timestamp: ",
  final_current_day,
  " ",
  final_current_time_ct,
  " CT\n",
  "Current global t: ",
  final_current_t,
  "\n",
  "Completed intervals: ",
  final_current_t,
  "\n",
  "Remaining intervals including current_t: ",
  final_T - final_current_t,
  "\n",
  "All required stage validation flags TRUE: TRUE\n",
  "All 11 optimizer production inputs exist: TRUE\n",
  "All 11 optimizer production inputs nonempty: TRUE\n",
  "All 11 optimizer production inputs freshly written: TRUE\n",
  "Total pipeline runtime: ",
  sprintf(
    "%.2f",
    total_pipeline_seconds
  ),
  " seconds\n",
  "Ready to run Python optimizer: TRUE\n",
  "============================================================\n",
  sep = ""
)
