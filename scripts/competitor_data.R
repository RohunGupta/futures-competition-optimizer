# ============================================================
# competitor_data.R
# ============================================================
#
# Purpose:
#   Load, validate, transform, and prepare the final-rank
#   competitor log-return data used in the trading competition
#   forecasting pipeline.
#
# Expected input:
#   data/processed/competitors/aug_final_place_rts.csv
#
# Required rank columns:
#   Rank1, Rank2, Rank3, Rank4, Rank15, Rank16
#
# Important:
#   These series are assumed to be LOG RETURNS.
#   Because all competitors start from the same initial balance,
#   cumulative log returns must preserve leaderboard ordering:
#
#   Rank1 >= Rank2 >= Rank3 >= Rank4 >= Rank15 >= Rank16
#
#   at every observation when comparing cumulative sums.
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(here)
})


# ============================================================
# 2. Constants
# ============================================================

FINAL_RANK_COLUMNS <- c(
  "Rank1",
  "Rank2",
  "Rank3",
  "Rank4",
  "Rank15",
  "Rank16"
)

DEFAULT_FINAL_RANK_PATH <- here(
  "data",
  "processed",
  "competitors",
  "aug_final_place_rts.csv"
)


# ============================================================
# 3. Load Final-Rank Data
# ============================================================

load_final_rank_data <- function(
    path = DEFAULT_FINAL_RANK_PATH
) {
  
  if (!file.exists(path)) {
    stop(
      paste0(
        "Final-rank data file does not exist:\n",
        path
      ),
      call. = FALSE
    )
  }
  
  data <- readr::read_csv(
    path,
    show_col_types = FALSE
  )
  
  # Convert percentage-formatted rank columns
  # Example: "18.59%" -> 0.1859
  data <- data |>
    mutate(
      across(
        all_of(FINAL_RANK_COLUMNS),
        ~ readr::parse_number(.x) / 100
      )
    )
  
  if (nrow(data) == 0) {
    stop(
      "Final-rank data file contains zero rows.",
      call. = FALSE
    )
  }
  
  if (ncol(data) == 0) {
    stop(
      "Final-rank data file contains zero columns.",
      call. = FALSE
    )
  }
  
  return(data)
}


# ============================================================
# 4. Check Required Rank Columns
# ============================================================

check_final_rank_columns <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  missing_columns <- setdiff(
    rank_columns,
    names(data)
  )
  
  if (length(missing_columns) > 0) {
    stop(
      paste0(
        "Missing required final-rank columns: ",
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
# 5. Check Rank Columns Are Numeric
# ============================================================

check_final_rank_numeric <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  check_final_rank_columns(
    data = data,
    rank_columns = rank_columns
  )
  
  numeric_check <- vapply(
    data[rank_columns],
    is.numeric,
    logical(1)
  )
  
  if (!all(numeric_check)) {
    
    bad_columns <- names(
      numeric_check
    )[!numeric_check]
    
    stop(
      paste0(
        "The following rank columns are not numeric: ",
        paste(
          bad_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  return(numeric_check)
}


# ============================================================
# 6. Missing-Value Summary
# ============================================================

summarize_final_rank_missing <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  check_final_rank_columns(
    data = data,
    rank_columns = rank_columns
  )
  
  missing_counts <- vapply(
    data[rank_columns],
    function(x) {
      sum(is.na(x))
    },
    numeric(1)
  )
  
  tibble(
    Rank = names(missing_counts),
    MissingValues = as.integer(
      missing_counts
    )
  )
}


# ============================================================
# 7. Validate Missing Values
# ============================================================

check_final_rank_missing <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  missing_summary <- summarize_final_rank_missing(
    data = data,
    rank_columns = rank_columns
  )
  
  if (
    any(
      missing_summary$MissingValues > 0
    )
  ) {
    
    bad_rows <- missing_summary |>
      filter(
        MissingValues > 0
      )
    
    stop(
      paste0(
        "Missing values found in final-rank series: ",
        paste(
          paste0(
            bad_rows$Rank,
            " (",
            bad_rows$MissingValues,
            ")"
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 8. Non-Finite Value Summary
# ============================================================

summarize_final_rank_nonfinite <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  check_final_rank_numeric(
    data = data,
    rank_columns = rank_columns
  )
  
  nonfinite_counts <- vapply(
    data[rank_columns],
    function(x) {
      sum(
        !is.finite(x)
        & !is.na(x)
      )
    },
    numeric(1)
  )
  
  tibble(
    Rank = names(nonfinite_counts),
    NonFiniteValues = as.integer(
      nonfinite_counts
    )
  )
}


# ============================================================
# 9. Validate Finite Values
# ============================================================

check_final_rank_finite <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  nonfinite_summary <-
    summarize_final_rank_nonfinite(
      data = data,
      rank_columns = rank_columns
    )
  
  if (
    any(
      nonfinite_summary$NonFiniteValues > 0
    )
  ) {
    
    bad_rows <- nonfinite_summary |>
      filter(
        NonFiniteValues > 0
      )
    
    stop(
      paste0(
        "Non-finite values found in final-rank series: ",
        paste(
          paste0(
            bad_rows$Rank,
            " (",
            bad_rows$NonFiniteValues,
            ")"
          ),
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  invisible(TRUE)
}


# ============================================================
# 10. Duplicate-Row Summary
# ============================================================

count_duplicate_rows <- function(
    data
) {
  
  sum(
    duplicated(data)
  )
}


# ============================================================
# 11. Zero-Return Summary
# ============================================================

summarize_final_rank_zeros <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  check_final_rank_numeric(
    data = data,
    rank_columns = rank_columns
  )
  
  zero_counts <- vapply(
    data[rank_columns],
    function(x) {
      sum(
        x == 0,
        na.rm = TRUE
      )
    },
    numeric(1)
  )
  
  n <- nrow(data)
  
  tibble(
    Rank = names(zero_counts),
    ZeroReturns = as.integer(
      zero_counts
    ),
    ZeroShare = as.numeric(
      zero_counts / n
    )
  )
}


# ============================================================
# 12. Basic Summary Statistics
# ============================================================

summarize_final_rank_data <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  check_final_rank_numeric(
    data = data,
    rank_columns = rank_columns
  )
  
  purrr::map_dfr(
    rank_columns,
    function(rank_name) {
      
      x <- data[[rank_name]]
      
      tibble(
        Rank = rank_name,
        N = length(x),
        Mean = mean(
          x,
          na.rm = TRUE
        ),
        Median = median(
          x,
          na.rm = TRUE
        ),
        SD = sd(
          x,
          na.rm = TRUE
        ),
        Variance = var(
          x,
          na.rm = TRUE
        ),
        Min = min(
          x,
          na.rm = TRUE
        ),
        Max = max(
          x,
          na.rm = TRUE
        ),
        Q01 = quantile(
          x,
          probs = 0.01,
          na.rm = TRUE,
          names = FALSE
        ),
        Q05 = quantile(
          x,
          probs = 0.05,
          na.rm = TRUE,
          names = FALSE
        ),
        Q25 = quantile(
          x,
          probs = 0.25,
          na.rm = TRUE,
          names = FALSE
        ),
        Q75 = quantile(
          x,
          probs = 0.75,
          na.rm = TRUE,
          names = FALSE
        ),
        Q95 = quantile(
          x,
          probs = 0.95,
          na.rm = TRUE,
          names = FALSE
        ),
        Q99 = quantile(
          x,
          probs = 0.99,
          na.rm = TRUE,
          names = FALSE
        )
      )
    }
  )
}


# ============================================================
# 13. Add Observation Index
# ============================================================

add_final_rank_index <- function(
    data,
    index_name = "Observation"
) {
  
  if (
    index_name %in% names(data)
  ) {
    return(data)
  }
  
  data |>
    mutate(
      !!index_name :=
        seq_len(
          nrow(data)
        ),
      .before = 1
    )
}


# ============================================================
# 14. Create Cumulative Log Returns
# ============================================================

create_cumulative_final_rank_returns <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  check_final_rank_numeric(
    data = data,
    rank_columns = rank_columns
  )
  
  data |>
    mutate(
      across(
        all_of(rank_columns),
        cumsum,
        .names = "cum_{.col}"
      )
    )
}


# ============================================================
# 15. Check Historical Cumulative Rank Ordering
# ============================================================

check_cumulative_rank_ordering <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS,
    tolerance = 1e-12
) {
  
  required_order <- c(
    "Rank1",
    "Rank2",
    "Rank3",
    "Rank4",
    "Rank15",
    "Rank16"
  )
  
  if (
    !identical(
      rank_columns,
      required_order
    )
  ) {
    stop(
      paste0(
        "For cumulative ordering validation, ",
        "rank_columns must be exactly: ",
        paste(
          required_order,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  cumulative_data <-
    create_cumulative_final_rank_returns(
      data = data,
      rank_columns = rank_columns
    )
  
  ordering_check <- cumulative_data |>
    transmute(
      Rank1_ge_Rank2 =
        cum_Rank1 + tolerance
      >= cum_Rank2,
      
      Rank2_ge_Rank3 =
        cum_Rank2 + tolerance
      >= cum_Rank3,
      
      Rank3_ge_Rank4 =
        cum_Rank3 + tolerance
      >= cum_Rank4,
      
      Rank4_ge_Rank15 =
        cum_Rank4 + tolerance
      >= cum_Rank15,
      
      Rank15_ge_Rank16 =
        cum_Rank15 + tolerance
      >= cum_Rank16
    )
  
  violation_counts <- ordering_check |>
    summarise(
      across(
        everything(),
        ~ sum(!.x)
      )
    ) |>
    pivot_longer(
      cols = everything(),
      names_to = "Constraint",
      values_to = "Violations"
    )
  
  first_violation_rows <- purrr::map_dfr(
    names(ordering_check),
    function(constraint_name) {
      
      bad_rows <- which(
        !ordering_check[[constraint_name]]
      )
      
      tibble(
        Constraint = constraint_name,
        FirstViolationRow =
          if (
            length(bad_rows) == 0
          ) {
            NA_integer_
          } else {
            bad_rows[1]
          }
      )
    }
  )
  
  violation_summary <-
    violation_counts |>
    left_join(
      first_violation_rows,
      by = "Constraint"
    )
  
  valid <- all(
    violation_summary$Violations == 0
  )
  
  list(
    valid = valid,
    cumulative_data =
      cumulative_data,
    ordering_check =
      ordering_check,
    violation_summary =
      violation_summary
  )
}


# ============================================================
# 16. Enforce Historical Ordering Validation
# ============================================================

validate_cumulative_rank_ordering <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS,
    tolerance = 1e-12,
    stop_on_violation = TRUE
) {
  
  ordering_result <-
    check_cumulative_rank_ordering(
      data = data,
      rank_columns = rank_columns,
      tolerance = tolerance
    )
  
  if (
    !ordering_result$valid
  ) {
    
    violated <- ordering_result$violation_summary |>
      filter(
        Violations > 0
      )
    
    message_text <- paste0(
      "Historical cumulative final-rank ordering is violated. ",
      "Violations: ",
      paste(
        paste0(
          violated$Constraint,
          "=",
          violated$Violations,
          " (first row ",
          violated$FirstViolationRow,
          ")"
        ),
        collapse = "; "
      )
    )
    
    if (stop_on_violation) {
      stop(
        message_text,
        call. = FALSE
      )
    } else {
      warning(
        message_text,
        call. = FALSE
      )
    }
  }
  
  return(
    ordering_result
  )
}


# ============================================================
# 17. Create Long-Format Rank Data
# ============================================================

create_final_rank_long <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  check_final_rank_columns(
    data = data,
    rank_columns = rank_columns
  )
  
  data |>
    pivot_longer(
      cols = all_of(
        rank_columns
      ),
      names_to = "Rank",
      values_to = "LogReturn"
    ) |>
    mutate(
      Rank = factor(
        Rank,
        levels = rank_columns,
        ordered = TRUE
      )
    )
}


# ============================================================
# 18. Create Long-Format Cumulative Rank Data
# ============================================================

create_cumulative_final_rank_long <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS
) {
  
  cumulative_data <-
    create_cumulative_final_rank_returns(
      data = data,
      rank_columns = rank_columns
    )
  
  cumulative_columns <- paste0(
    "cum_",
    rank_columns
  )
  
  cumulative_data |>
    pivot_longer(
      cols = all_of(
        cumulative_columns
      ),
      names_to = "Rank",
      values_to = "CumulativeLogReturn"
    ) |>
    mutate(
      Rank = sub(
        "^cum_",
        "",
        Rank
      ),
      Rank = factor(
        Rank,
        levels = rank_columns,
        ordered = TRUE
      )
    )
}


# ============================================================
# 19. Full Validation Function
# ============================================================

validate_final_rank_data <- function(
    data,
    rank_columns = FINAL_RANK_COLUMNS,
    ordering_tolerance = 1e-12,
    stop_on_ordering_violation = TRUE
) {
  
  check_final_rank_columns(
    data = data,
    rank_columns = rank_columns
  )
  
  numeric_check <-
    check_final_rank_numeric(
      data = data,
      rank_columns = rank_columns
    )
  
  check_final_rank_missing(
    data = data,
    rank_columns = rank_columns
  )
  
  check_final_rank_finite(
    data = data,
    rank_columns = rank_columns
  )
  
  duplicate_rows <-
    count_duplicate_rows(
      data = data
    )
  
  missing_summary <-
    summarize_final_rank_missing(
      data = data,
      rank_columns = rank_columns
    )
  
  nonfinite_summary <-
    summarize_final_rank_nonfinite(
      data = data,
      rank_columns = rank_columns
    )
  
  zero_summary <-
    summarize_final_rank_zeros(
      data = data,
      rank_columns = rank_columns
    )
  
  ordering_result <-
    validate_cumulative_rank_ordering(
      data = data,
      rank_columns = rank_columns,
      tolerance = ordering_tolerance,
      stop_on_violation =
        stop_on_ordering_violation
    )
  
  list(
    n_rows = nrow(data),
    n_columns = ncol(data),
    rank_columns = rank_columns,
    numeric_check = numeric_check,
    duplicate_rows = duplicate_rows,
    missing_summary = missing_summary,
    nonfinite_summary =
      nonfinite_summary,
    zero_summary = zero_summary,
    ordering_valid =
      ordering_result$valid,
    ordering_summary =
      ordering_result$violation_summary
  )
}


# ============================================================
# 20. Prepare Final-Rank Dataset
# ============================================================

prepare_final_rank_data <- function(
    path = DEFAULT_FINAL_RANK_PATH,
    add_index = TRUE,
    ordering_tolerance = 1e-12,
    stop_on_ordering_violation = TRUE
) {
  
  data <- load_final_rank_data(
    path = path
  )
  
  validation <- validate_final_rank_data(
    data = data,
    rank_columns = FINAL_RANK_COLUMNS,
    ordering_tolerance =
      ordering_tolerance,
    stop_on_ordering_violation =
      stop_on_ordering_violation
  )
  
  if (add_index) {
    data <- add_final_rank_index(
      data = data
    )
  }
  
  cumulative_data <-
    create_cumulative_final_rank_returns(
      data = data,
      rank_columns =
        FINAL_RANK_COLUMNS
    )
  
  long_data <-
    create_final_rank_long(
      data = data,
      rank_columns =
        FINAL_RANK_COLUMNS
    )
  
  cumulative_long <-
    create_cumulative_final_rank_long(
      data = data,
      rank_columns =
        FINAL_RANK_COLUMNS
    )
  
  summary_statistics <-
    summarize_final_rank_data(
      data = data,
      rank_columns =
        FINAL_RANK_COLUMNS
    )
  
  list(
    data = data,
    cumulative_data =
      cumulative_data,
    long_data = long_data,
    cumulative_long =
      cumulative_long,
    summary_statistics =
      summary_statistics,
    validation = validation
  )
}


# ============================================================
# End of competitor_data.R
# ============================================================
