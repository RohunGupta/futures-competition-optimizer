# ============================================================
# competitor_futures_dependence.R
# ============================================================
#
# Reusable mathematical infrastructure for estimating,
# constructing, transforming, validating, and exporting
# competitor-futures cross-dependence.
#
# This script intentionally contains:
# - no hard-coded project paths;
# - no production-data loading;
# - no notebook-specific model fitting;
# - no arbitrary production tuning parameters.
#
# It is intended to support:
#
# 44_CompetitorFutures_HistoricalDependence.Rmd
# 44.5_CompetitorFutures_HoldingsDependence.Rmd
# 44.7_CompetitorFutures_Production.Rmd
#
# Core objects:
#
# Raw competitor vector:
# q_t = [r1, r2, r3, r4, r15, r16, h1, h2]
#
# State vector:
# S_t = M8 q_t
#
# Futures log-return vector:
# L_t
#
# Futures arithmetic-return vector:
# R_t = exp(L_t) - 1
#
# Raw competitor / futures log cross-covariance:
# C_raw_log,t = Cov(q_t, L_t)
#
# State / futures log cross-covariance:
# C_state_log,t = M8 C_raw_log,t
#
# Arithmetic-return conversion:
#
# Cov(X, R_i)
# =
# Cov(X, L_i) *
# exp(mu_L,i + 0.5 * Var(L_i))
#
# Joint PSD requirement:
#
# [ Sigma_state    C       ]
# [ C'             Sigma_R ]
#
# must be positive semidefinite.
#
# ============================================================


# ============================================================
# 1. Canonical Names
# ============================================================

COMPETITOR_FUTURES_RAW_NAMES <- c(
  "r1",
  "r2",
  "r3",
  "r4",
  "r15",
  "r16",
  "h1",
  "h2"
)

COMPETITOR_FUTURES_STATE_NAMES <- c(
  "Delta_Final_X1",
  "Delta_Final_D12",
  "Delta_Final_D23",
  "Delta_Final_D34",
  "Delta_Final_D4_15",
  "Delta_Final_D15_16",
  "Delta_Daily_H1",
  "Delta_Daily_D12"
)

COMPETITOR_FUTURES_NAMES <- c(
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
# 2. Canonical State Transform
# ============================================================

COMPETITOR_FUTURES_FINAL_STATE_TRANSFORM <- matrix(
  c(
    1,  0,  0,  0,  0,  0,
    1, -1,  0,  0,  0,  0,
    0,  1, -1,  0,  0,  0,
    0,  0,  1, -1,  0,  0,
    0,  0,  0,  1, -1,  0,
    0,  0,  0,  0,  1, -1
  ),
  nrow = 6L,
  byrow = TRUE
)

rownames(
  COMPETITOR_FUTURES_FINAL_STATE_TRANSFORM
) <- COMPETITOR_FUTURES_STATE_NAMES[1:6]

colnames(
  COMPETITOR_FUTURES_FINAL_STATE_TRANSFORM
) <- COMPETITOR_FUTURES_RAW_NAMES[1:6]


COMPETITOR_FUTURES_DAILY_STATE_TRANSFORM <- matrix(
  c(
    1,  0,
    1, -1
  ),
  nrow = 2L,
  byrow = TRUE
)

rownames(
  COMPETITOR_FUTURES_DAILY_STATE_TRANSFORM
) <- COMPETITOR_FUTURES_STATE_NAMES[7:8]

colnames(
  COMPETITOR_FUTURES_DAILY_STATE_TRANSFORM
) <- COMPETITOR_FUTURES_RAW_NAMES[7:8]


COMPETITOR_FUTURES_STATE_TRANSFORM <- matrix(
  0,
  nrow = 8L,
  ncol = 8L
)

COMPETITOR_FUTURES_STATE_TRANSFORM[
  1:6,
  1:6
] <- COMPETITOR_FUTURES_FINAL_STATE_TRANSFORM

COMPETITOR_FUTURES_STATE_TRANSFORM[
  7:8,
  7:8
] <- COMPETITOR_FUTURES_DAILY_STATE_TRANSFORM

rownames(
  COMPETITOR_FUTURES_STATE_TRANSFORM
) <- COMPETITOR_FUTURES_STATE_NAMES

colnames(
  COMPETITOR_FUTURES_STATE_TRANSFORM
) <- COMPETITOR_FUTURES_RAW_NAMES


# ============================================================
# 3. Basic Validation Helpers
# ============================================================

assert_finite_numeric <- function(
    x,
    object_name = "object"
) {
  if (
    !is.numeric(
      x
    )
  ) {
    stop(
      paste0(
        object_name,
        " must be numeric."
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        x
      )
    )
  ) {
    stop(
      paste0(
        object_name,
        " contains non-finite values."
      ),
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


assert_matrix_dimensions <- function(
    x,
    expected_rows = NULL,
    expected_columns = NULL,
    object_name = "matrix"
) {
  x <- as.matrix(
    x
  )
  
  if (
    !is.null(
      expected_rows
    ) &&
    nrow(
      x
    ) !=
    expected_rows
  ) {
    stop(
      paste0(
        object_name,
        " must have ",
        expected_rows,
        " rows."
      ),
      call. = FALSE
    )
  }
  
  if (
    !is.null(
      expected_columns
    ) &&
    ncol(
      x
    ) !=
    expected_columns
  ) {
    stop(
      paste0(
        object_name,
        " must have ",
        expected_columns,
        " columns."
      ),
      call. = FALSE
    )
  }
  
  invisible(
    TRUE
  )
}


validate_square_matrix <- function(
    x,
    object_name = "matrix",
    require_symmetric = FALSE,
    symmetry_tolerance = 1e-10
) {
  x <- as.matrix(
    x
  )
  
  assert_finite_numeric(
    x,
    object_name
  )
  
  if (
    nrow(
      x
    ) !=
    ncol(
      x
    )
  ) {
    stop(
      paste0(
        object_name,
        " must be square."
      ),
      call. = FALSE
    )
  }
  
  if (
    require_symmetric
  ) {
    symmetry_error <- max(
      abs(
        x -
          t(
            x
          )
      )
    )
    
    if (
      symmetry_error >
      symmetry_tolerance
    ) {
      stop(
        paste0(
          object_name,
          " is not symmetric within tolerance."
        ),
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}


symmetrize_matrix <- function(
    x
) {
  x <- as.matrix(
    x
  )
  
  (
    x +
      t(
        x
      )
  ) /
    2
}


# ============================================================
# 4. Positive Semidefinite Matrix Helpers
# ============================================================

matrix_eigenvalues_symmetric <- function(
    x
) {
  x <- symmetrize_matrix(
    x
  )
  
  eigen(
    x,
    symmetric = TRUE,
    only.values = TRUE
  )[["values"]]
}


is_positive_semidefinite <- function(
    x,
    tolerance = 1e-10
) {
  validate_square_matrix(
    x,
    object_name = "x",
    require_symmetric = TRUE,
    symmetry_tolerance = max(
      tolerance,
      1e-12
    )
  )
  
  minimum_eigenvalue <- min(
    matrix_eigenvalues_symmetric(
      x
    )
  )
  
  minimum_eigenvalue >=
    -tolerance
}


is_positive_definite <- function(
    x,
    tolerance = 1e-10
) {
  validate_square_matrix(
    x,
    object_name = "x",
    require_symmetric = TRUE,
    symmetry_tolerance = max(
      tolerance,
      1e-12
    )
  )
  
  minimum_eigenvalue <- min(
    matrix_eigenvalues_symmetric(
      x
    )
  )
  
  minimum_eigenvalue >
    tolerance
}


# ============================================================
# 5. Symmetric Matrix Square-Root Helpers
# ============================================================

symmetric_matrix_power <- function(
    x,
    power,
    eigenvalue_floor = 1e-12
) {
  x <- symmetrize_matrix(
    x
  )
  
  validate_square_matrix(
    x,
    object_name = "x",
    require_symmetric = TRUE
  )
  
  decomposition <- eigen(
    x,
    symmetric = TRUE
  )
  
  eigenvalues <- decomposition[["values"]]
  
  eigenvectors <- decomposition[["vectors"]]
  
  adjusted_eigenvalues <- pmax(
    eigenvalues,
    eigenvalue_floor
  )
  
  powered_values <- adjusted_eigenvalues^power
  
  result <- (
    eigenvectors %*%
      diag(
        powered_values,
        nrow = length(
          powered_values
        )
      ) %*%
      t(
        eigenvectors
      )
  )
  
  symmetrize_matrix(
    result
  )
}


symmetric_matrix_inverse_sqrt <- function(
    x,
    eigenvalue_floor = 1e-12
) {
  symmetric_matrix_power(
    x = x,
    power = -0.5,
    eigenvalue_floor = eigenvalue_floor
  )
}


symmetric_matrix_sqrt <- function(
    x,
    eigenvalue_floor = 0
) {
  symmetric_matrix_power(
    x = x,
    power = 0.5,
    eigenvalue_floor = eigenvalue_floor
  )
}


# ============================================================
# 6. Stable Symmetric Generalized Inverse
# ============================================================

symmetric_generalized_inverse <- function(
    x,
    eigenvalue_floor = 1e-12
) {
  x <- symmetrize_matrix(
    x
  )
  
  decomposition <- eigen(
    x,
    symmetric = TRUE
  )
  
  eigenvalues <- decomposition[["values"]]
  
  eigenvectors <- decomposition[["vectors"]]
  
  inverse_values <- ifelse(
    eigenvalues >
      eigenvalue_floor,
    1 /
      eigenvalues,
    0
  )
  
  result <- (
    eigenvectors %*%
      diag(
        inverse_values,
        nrow = length(
          inverse_values
        )
      ) %*%
      t(
        eigenvectors
      )
  )
  
  symmetrize_matrix(
    result
  )
}


# ============================================================
# 7. Timestamp Normalization
# ============================================================

normalize_timestamp_vector <- function(
    x,
    timezone = "UTC"
) {
  if (
    inherits(
      x,
      "POSIXct"
    )
  ) {
    return(
      as.POSIXct(
        x,
        tz = timezone
      )
    )
  }
  
  if (
    inherits(
      x,
      "POSIXlt"
    )
  ) {
    return(
      as.POSIXct(
        x,
        tz = timezone
      )
    )
  }
  
  if (
    inherits(
      x,
      "Date"
    )
  ) {
    return(
      as.POSIXct(
        x,
        tz = timezone
      )
    )
  }
  
  parsed <- as.POSIXct(
    x,
    tz = timezone
  )
  
  if (
    any(
      is.na(
        parsed
      )
    )
  ) {
    stop(
      "Timestamp parsing produced missing values.",
      call. = FALSE
    )
  }
  
  parsed
}


# ============================================================
# 8. Align Two Innovation Panels by Timestamp
# ============================================================

align_innovation_panels_by_timestamp <- function(
    competitor_data,
    futures_data,
    competitor_timestamp_column,
    futures_timestamp_column,
    competitor_value_columns,
    futures_value_columns,
    timezone = "UTC"
) {
  competitor_data <- as.data.frame(
    competitor_data
  )
  
  futures_data <- as.data.frame(
    futures_data
  )
  
  required_competitor_columns <- c(
    competitor_timestamp_column,
    competitor_value_columns
  )
  
  required_futures_columns <- c(
    futures_timestamp_column,
    futures_value_columns
  )
  
  missing_competitor_columns <- setdiff(
    required_competitor_columns,
    names(
      competitor_data
    )
  )
  
  missing_futures_columns <- setdiff(
    required_futures_columns,
    names(
      futures_data
    )
  )
  
  if (
    length(
      missing_competitor_columns
    ) >
    0L
  ) {
    stop(
      paste0(
        "Competitor data is missing columns: ",
        paste(
          missing_competitor_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (
    length(
      missing_futures_columns
    ) >
    0L
  ) {
    stop(
      paste0(
        "Futures data is missing columns: ",
        paste(
          missing_futures_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  competitor_timestamp <- normalize_timestamp_vector(
    competitor_data[[competitor_timestamp_column]],
    timezone = timezone
  )
  
  futures_timestamp <- normalize_timestamp_vector(
    futures_data[[futures_timestamp_column]],
    timezone = timezone
  )
  
  competitor_panel <- data.frame(
    Timestamp = competitor_timestamp,
    competitor_data[
      competitor_value_columns
    ],
    check.names = FALSE
  )
  
  futures_panel <- data.frame(
    Timestamp = futures_timestamp,
    futures_data[
      futures_value_columns
    ],
    check.names = FALSE
  )
  
  if (
    anyDuplicated(
      competitor_panel[["Timestamp"]]
    ) >
    0L
  ) {
    stop(
      "Competitor innovation timestamps are not unique.",
      call. = FALSE
    )
  }
  
  if (
    anyDuplicated(
      futures_panel[["Timestamp"]]
    ) >
    0L
  ) {
    stop(
      "Futures innovation timestamps are not unique.",
      call. = FALSE
    )
  }
  
  aligned <- merge(
    competitor_panel,
    futures_panel,
    by = "Timestamp",
    all = FALSE,
    sort = TRUE,
    suffixes = c(
      "_competitor",
      "_future"
    )
  )
  
  if (
    nrow(
      aligned
    ) <
    2L
  ) {
    stop(
      "Fewer than two timestamps remain after innovation alignment.",
      call. = FALSE
    )
  }
  
  competitor_matrix <- as.matrix(
    aligned[
      competitor_value_columns
    ]
  )
  
  futures_matrix <- as.matrix(
    aligned[
      futures_value_columns
    ]
  )
  
  storage.mode(
    competitor_matrix
  ) <- "double"
  
  storage.mode(
    futures_matrix
  ) <- "double"
  
  complete_rows <- (
    apply(
      competitor_matrix,
      1,
      function(x) {
        all(
          is.finite(
            x
          )
        )
      }
    ) &
      apply(
        futures_matrix,
        1,
        function(x) {
          all(
            is.finite(
              x
            )
          )
        }
      )
  )
  
  aligned <- aligned[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  competitor_matrix <- competitor_matrix[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  futures_matrix <- futures_matrix[
    complete_rows,
    ,
    drop = FALSE
  ]
  
  if (
    nrow(
      competitor_matrix
    ) <
    2L
  ) {
    stop(
      "Fewer than two complete aligned innovation observations remain.",
      call. = FALSE
    )
  }
  
  colnames(
    competitor_matrix
  ) <- competitor_value_columns
  
  colnames(
    futures_matrix
  ) <- futures_value_columns
  
  list(
    aligned_data = aligned,
    timestamps = aligned[["Timestamp"]],
    competitor_innovations = competitor_matrix,
    futures_innovations = futures_matrix,
    observations = nrow(
      aligned
    )
  )
}


# ============================================================
# 9. Cross-Covariance Estimation
# ============================================================

estimate_cross_covariance <- function(
    x,
    y,
    center = TRUE
) {
  x <- as.matrix(
    x
  )
  
  y <- as.matrix(
    y
  )
  
  if (
    nrow(
      x
    ) !=
    nrow(
      y
    )
  ) {
    stop(
      "x and y must contain the same number of rows.",
      call. = FALSE
    )
  }
  
  if (
    nrow(
      x
    ) <
    2L
  ) {
    stop(
      "At least two aligned observations are required.",
      call. = FALSE
    )
  }
  
  assert_finite_numeric(
    x,
    "x"
  )
  
  assert_finite_numeric(
    y,
    "y"
  )
  
  if (
    center
  ) {
    x_centered <- sweep(
      x,
      2,
      colMeans(
        x
      ),
      FUN = "-"
    )
    
    y_centered <- sweep(
      y,
      2,
      colMeans(
        y
      ),
      FUN = "-"
    )
  } else {
    x_centered <- x
    y_centered <- y
  }
  
  covariance_matrix <- (
    t(
      x_centered
    ) %*%
      y_centered
  ) /
    (
      nrow(
        x_centered
      ) -
        1
    )
  
  rownames(
    covariance_matrix
  ) <- colnames(
    x
  )
  
  colnames(
    covariance_matrix
  ) <- colnames(
    y
  )
  
  covariance_matrix
}


# ============================================================
# 10. Cross-Correlation Estimation
# ============================================================

estimate_cross_correlation <- function(
    x,
    y
) {
  x <- as.matrix(
    x
  )
  
  y <- as.matrix(
    y
  )
  
  cross_covariance <- estimate_cross_covariance(
    x = x,
    y = y,
    center = TRUE
  )
  
  x_sd <- apply(
    x,
    2,
    stats::sd
  )
  
  y_sd <- apply(
    y,
    2,
    stats::sd
  )
  
  if (
    any(
      !is.finite(
        x_sd
      )
    ) ||
    any(
      !is.finite(
        y_sd
      )
    ) ||
    any(
      x_sd <= 0
    ) ||
    any(
      y_sd <= 0
    )
  ) {
    stop(
      "Cross-correlation cannot be estimated because at least one series has zero or invalid sample variance.",
      call. = FALSE
    )
  }
  
  denominator <- outer(
    x_sd,
    y_sd
  )
  
  cross_correlation <- (
    cross_covariance /
      denominator
  )
  
  rownames(
    cross_correlation
  ) <- colnames(
    x
  )
  
  colnames(
    cross_correlation
  ) <- colnames(
    y
  )
  
  cross_correlation
}


# ============================================================
# 11. Build Cross-Covariance From Standardized Dependence
# ============================================================

cross_correlation_to_covariance <- function(
    cross_correlation,
    left_standard_deviations,
    right_standard_deviations
) {
  cross_correlation <- as.matrix(
    cross_correlation
  )
  
  left_standard_deviations <- as.numeric(
    left_standard_deviations
  )
  
  right_standard_deviations <- as.numeric(
    right_standard_deviations
  )
  
  if (
    nrow(
      cross_correlation
    ) !=
    length(
      left_standard_deviations
    )
  ) {
    stop(
      "Left standard-deviation dimension does not match cross-correlation rows.",
      call. = FALSE
    )
  }
  
  if (
    ncol(
      cross_correlation
    ) !=
    length(
      right_standard_deviations
    )
  ) {
    stop(
      "Right standard-deviation dimension does not match cross-correlation columns.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        left_standard_deviations
      )
    ) ||
    any(
      !is.finite(
        right_standard_deviations
      )
    ) ||
    any(
      left_standard_deviations < 0
    ) ||
    any(
      right_standard_deviations < 0
    )
  ) {
    stop(
      "Standard deviations must be finite and nonnegative.",
      call. = FALSE
    )
  }
  
  result <- (
    diag(
      left_standard_deviations,
      nrow = length(
        left_standard_deviations
      )
    ) %*%
      cross_correlation %*%
      diag(
        right_standard_deviations,
        nrow = length(
          right_standard_deviations
        )
      )
  )
  
  rownames(
    result
  ) <- rownames(
    cross_correlation
  )
  
  colnames(
    result
  ) <- colnames(
    cross_correlation
  )
  
  result
}


# ============================================================
# 12. Dollar Beta to Log-Balance Beta
# ============================================================

dollar_beta_to_log_balance_beta <- function(
    dollar_beta,
    balance,
    invalid_balance_value = NA_real_
) {
  dollar_beta <- as.numeric(
    dollar_beta
  )
  
  if (
    length(
      balance
    ) !=
    1L ||
    !is.finite(
      balance
    )
  ) {
    stop(
      "balance must be one finite numeric value.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        dollar_beta
      )
    )
  ) {
    stop(
      "dollar_beta must contain only finite values.",
      call. = FALSE
    )
  }
  
  if (
    balance <=
    0
  ) {
    return(
      rep(
        invalid_balance_value,
        length(
          dollar_beta
        )
      )
    )
  }
  
  dollar_beta /
    balance
}


# ============================================================
# 13. Matrix Dollar-Beta to Log-Balance-Beta Conversion
# ============================================================

dollar_beta_matrix_to_log_balance_beta <- function(
    dollar_beta_matrix,
    balances,
    invalid_balance_value = NA_real_
) {
  dollar_beta_matrix <- as.matrix(
    dollar_beta_matrix
  )
  
  balances <- as.numeric(
    balances
  )
  
  if (
    nrow(
      dollar_beta_matrix
    ) !=
    length(
      balances
    )
  ) {
    stop(
      "Number of beta rows must equal number of balances.",
      call. = FALSE
    )
  }
  
  result <- matrix(
    invalid_balance_value,
    nrow = nrow(
      dollar_beta_matrix
    ),
    ncol = ncol(
      dollar_beta_matrix
    ),
    dimnames = dimnames(
      dollar_beta_matrix
    )
  )
  
  valid_rows <- which(
    is.finite(
      balances
    ) &
      balances >
      0
  )
  
  if (
    length(
      valid_rows
    ) >
    0L
  ) {
    result[
      valid_rows,
      ,
      drop = FALSE
    ] <- sweep(
      dollar_beta_matrix[
        valid_rows,
        ,
        drop = FALSE
      ],
      1,
      balances[
        valid_rows
      ],
      FUN = "/"
    )
  }
  
  result
}


# ============================================================
# 14. Exposure Beta to Futures Log Cross-Covariance
# ============================================================

exposure_beta_to_log_cross_covariance <- function(
    beta_matrix,
    futures_log_covariance
) {
  beta_matrix <- as.matrix(
    beta_matrix
  )
  
  futures_log_covariance <- as.matrix(
    futures_log_covariance
  )
  
  validate_square_matrix(
    futures_log_covariance,
    object_name = "futures_log_covariance",
    require_symmetric = TRUE
  )
  
  if (
    ncol(
      beta_matrix
    ) !=
    nrow(
      futures_log_covariance
    )
  ) {
    stop(
      "Beta columns must equal futures covariance dimension.",
      call. = FALSE
    )
  }
  
  assert_finite_numeric(
    beta_matrix,
    "beta_matrix"
  )
  
  result <- (
    beta_matrix %*%
      futures_log_covariance
  )
  
  rownames(
    result
  ) <- rownames(
    beta_matrix
  )
  
  if (
    !is.null(
      colnames(
        futures_log_covariance
      )
    )
  ) {
    colnames(
      result
    ) <- colnames(
      futures_log_covariance
    )
  }
  
  result
}


# ============================================================
# 15. Raw Competitor Cross-Covariance to State Cross-Covariance
# ============================================================

transform_raw_cross_covariance_to_state <- function(
    raw_cross_covariance,
    state_transform = COMPETITOR_FUTURES_STATE_TRANSFORM
) {
  raw_cross_covariance <- as.matrix(
    raw_cross_covariance
  )
  
  state_transform <- as.matrix(
    state_transform
  )
  
  if (
    ncol(
      state_transform
    ) !=
    nrow(
      raw_cross_covariance
    )
  ) {
    stop(
      "State-transform columns must equal raw cross-covariance rows.",
      call. = FALSE
    )
  }
  
  result <- (
    state_transform %*%
      raw_cross_covariance
  )
  
  if (
    !is.null(
      rownames(
        state_transform
      )
    )
  ) {
    rownames(
      result
    ) <- rownames(
      state_transform
    )
  }
  
  if (
    !is.null(
      colnames(
        raw_cross_covariance
      )
    )
  ) {
    colnames(
      result
    ) <- colnames(
      raw_cross_covariance
    )
  }
  
  result
}


# ============================================================
# 16. Final-Rank Cross-Covariance to Final State
# ============================================================

transform_final_raw_cross_covariance_to_state <- function(
    final_raw_cross_covariance
) {
  transform_raw_cross_covariance_to_state(
    raw_cross_covariance = final_raw_cross_covariance,
    state_transform = COMPETITOR_FUTURES_FINAL_STATE_TRANSFORM
  )
}


# ============================================================
# 17. Daily Cross-Covariance to Daily State
# ============================================================

transform_daily_raw_cross_covariance_to_state <- function(
    daily_raw_cross_covariance
) {
  transform_raw_cross_covariance_to_state(
    raw_cross_covariance = daily_raw_cross_covariance,
    state_transform = COMPETITOR_FUTURES_DAILY_STATE_TRANSFORM
  )
}


# ============================================================
# 18. Combine Final and Daily Raw Cross-Covariance Blocks
# ============================================================

combine_raw_competitor_cross_covariance_blocks <- function(
    final_cross_covariance,
    daily_cross_covariance
) {
  final_cross_covariance <- as.matrix(
    final_cross_covariance
  )
  
  daily_cross_covariance <- as.matrix(
    daily_cross_covariance
  )
  
  if (
    nrow(
      final_cross_covariance
    ) !=
    6L
  ) {
    stop(
      "Final cross-covariance must contain 6 rows.",
      call. = FALSE
    )
  }
  
  if (
    nrow(
      daily_cross_covariance
    ) !=
    2L
  ) {
    stop(
      "Daily cross-covariance must contain 2 rows.",
      call. = FALSE
    )
  }
  
  if (
    ncol(
      final_cross_covariance
    ) !=
    ncol(
      daily_cross_covariance
    )
  ) {
    stop(
      "Final and daily cross-covariance blocks must have the same futures dimension.",
      call. = FALSE
    )
  }
  
  result <- rbind(
    final_cross_covariance,
    daily_cross_covariance
  )
  
  rownames(
    result
  ) <- COMPETITOR_FUTURES_RAW_NAMES
  
  if (
    !is.null(
      colnames(
        final_cross_covariance
      )
    )
  ) {
    colnames(
      result
    ) <- colnames(
      final_cross_covariance
    )
  }
  
  result
}


# ============================================================
# 19. Log-Futures to Arithmetic-Futures Scaling Factors
# ============================================================

log_to_arithmetic_cross_covariance_scaling <- function(
    futures_log_mean,
    futures_log_covariance
) {
  futures_log_mean <- as.numeric(
    futures_log_mean
  )
  
  futures_log_covariance <- as.matrix(
    futures_log_covariance
  )
  
  validate_square_matrix(
    futures_log_covariance,
    object_name = "futures_log_covariance",
    require_symmetric = TRUE
  )
  
  if (
    length(
      futures_log_mean
    ) !=
    nrow(
      futures_log_covariance
    )
  ) {
    stop(
      "futures_log_mean dimension does not match covariance dimension.",
      call. = FALSE
    )
  }
  
  assert_finite_numeric(
    futures_log_mean,
    "futures_log_mean"
  )
  
  scaling <- exp(
    futures_log_mean +
      0.5 *
      diag(
        futures_log_covariance
      )
  )
  
  if (
    !is.null(
      colnames(
        futures_log_covariance
      )
    )
  ) {
    names(
      scaling
    ) <- colnames(
      futures_log_covariance
    )
  }
  
  scaling
}


# ============================================================
# 20. Convert State-Log Cross-Covariance to Arithmetic Returns
# ============================================================

convert_log_cross_covariance_to_arithmetic <- function(
    state_log_cross_covariance,
    futures_log_mean,
    futures_log_covariance
) {
  state_log_cross_covariance <- as.matrix(
    state_log_cross_covariance
  )
  
  scaling <- log_to_arithmetic_cross_covariance_scaling(
    futures_log_mean = futures_log_mean,
    futures_log_covariance = futures_log_covariance
  )
  
  if (
    ncol(
      state_log_cross_covariance
    ) !=
    length(
      scaling
    )
  ) {
    stop(
      "Cross-covariance futures dimension does not match arithmetic scaling dimension.",
      call. = FALSE
    )
  }
  
  result <- sweep(
    state_log_cross_covariance,
    2,
    scaling,
    FUN = "*"
  )
  
  rownames(
    result
  ) <- rownames(
    state_log_cross_covariance
  )
  
  colnames(
    result
  ) <- colnames(
    state_log_cross_covariance
  )
  
  result
}


# ============================================================
# 21. Holdings-Predicted Competitor Innovation
# ============================================================

predict_competitor_innovation_from_exposure <- function(
    futures_log_returns,
    beta_matrix
) {
  futures_log_returns <- as.matrix(
    futures_log_returns
  )
  
  beta_matrix <- as.matrix(
    beta_matrix
  )
  
  if (
    nrow(
      futures_log_returns
    ) !=
    nrow(
      beta_matrix
    )
  ) {
    stop(
      "futures_log_returns and beta_matrix must contain the same number of observations.",
      call. = FALSE
    )
  }
  
  if (
    ncol(
      futures_log_returns
    ) !=
    ncol(
      beta_matrix
    )
  ) {
    stop(
      "futures_log_returns and beta_matrix must contain the same futures dimension.",
      call. = FALSE
    )
  }
  
  assert_finite_numeric(
    futures_log_returns,
    "futures_log_returns"
  )
  
  assert_finite_numeric(
    beta_matrix,
    "beta_matrix"
  )
  
  rowSums(
    futures_log_returns *
      beta_matrix
  )
}


# ============================================================
# 22. Residualize One Competitor Series Against Holdings
# ============================================================

residualize_competitor_series_against_holdings <- function(
    observed_competitor_innovation,
    futures_log_returns,
    beta_matrix
) {
  observed_competitor_innovation <- as.numeric(
    observed_competitor_innovation
  )
  
  if (
    length(
      observed_competitor_innovation
    ) !=
    nrow(
      as.matrix(
        futures_log_returns
      )
    )
  ) {
    stop(
      "Observed competitor innovations and futures returns must have the same observation count.",
      call. = FALSE
    )
  }
  
  predicted_innovation <- predict_competitor_innovation_from_exposure(
    futures_log_returns = futures_log_returns,
    beta_matrix = beta_matrix
  )
  
  residual_innovation <- (
    observed_competitor_innovation -
      predicted_innovation
  )
  
  list(
    observed = observed_competitor_innovation,
    predicted = predicted_innovation,
    residual = residual_innovation
  )
}


# ============================================================
# 23. Residualize Multiple Competitor Series Against Holdings
# ============================================================

residualize_competitor_matrix_against_holdings <- function(
    observed_competitor_innovations,
    futures_log_returns,
    beta_array
) {
  observed_competitor_innovations <- as.matrix(
    observed_competitor_innovations
  )
  
  futures_log_returns <- as.matrix(
    futures_log_returns
  )
  
  if (
    length(
      dim(
        beta_array
      )
    ) !=
    3L
  ) {
    stop(
      "beta_array must have dimensions observation x competitor x future.",
      call. = FALSE
    )
  }
  
  number_of_observations <- nrow(
    observed_competitor_innovations
  )
  
  number_of_competitors <- ncol(
    observed_competitor_innovations
  )
  
  number_of_futures <- ncol(
    futures_log_returns
  )
  
  if (
    dim(
      beta_array
    )[[1]] !=
    number_of_observations ||
    dim(
      beta_array
    )[[2]] !=
    number_of_competitors ||
    dim(
      beta_array
    )[[3]] !=
    number_of_futures
  ) {
    stop(
      "beta_array dimensions do not match competitor and futures matrices.",
      call. = FALSE
    )
  }
  
  if (
    nrow(
      futures_log_returns
    ) !=
    number_of_observations
  ) {
    stop(
      "Competitor and futures matrices must have the same number of observations.",
      call. = FALSE
    )
  }
  
  predicted <- matrix(
    NA_real_,
    nrow = number_of_observations,
    ncol = number_of_competitors,
    dimnames = dimnames(
      observed_competitor_innovations
    )
  )
  
  for (
    competitor_index in
    seq_len(
      number_of_competitors
    )
  ) {
    beta_matrix <- beta_array[
      ,
      competitor_index,
      ,
      drop = TRUE
    ]
    
    if (
      is.null(
        dim(
          beta_matrix
        )
      )
    ) {
      beta_matrix <- matrix(
        beta_matrix,
        nrow = number_of_observations,
        ncol = number_of_futures
      )
    }
    
    predicted[
      ,
      competitor_index
    ] <- predict_competitor_innovation_from_exposure(
      futures_log_returns = futures_log_returns,
      beta_matrix = beta_matrix
    )
  }
  
  residual <- (
    observed_competitor_innovations -
      predicted
  )
  
  list(
    observed = observed_competitor_innovations,
    predicted = predicted,
    residual = residual
  )
}


# ============================================================
# 24. Estimate Residual Competitor-Futures Dependence
# ============================================================

estimate_residual_competitor_futures_dependence <- function(
    residual_competitor_innovations,
    futures_innovations,
    standardized = TRUE
) {
  residual_competitor_innovations <- as.matrix(
    residual_competitor_innovations
  )
  
  futures_innovations <- as.matrix(
    futures_innovations
  )
  
  if (
    standardized
  ) {
    estimate_cross_correlation(
      x = residual_competitor_innovations,
      y = futures_innovations
    )
  } else {
    estimate_cross_covariance(
      x = residual_competitor_innovations,
      y = futures_innovations,
      center = TRUE
    )
  }
}


# ============================================================
# 25. Linear Horizon Interpolation
# ============================================================

interpolate_horizon_values <- function(
    target_horizon,
    known_horizons,
    known_values,
    extrapolation = c(
      "flat",
      "linear",
      "zero"
    )
) {
  extrapolation <- match.arg(
    extrapolation
  )
  
  target_horizon <- as.numeric(
    target_horizon
  )
  
  known_horizons <- as.numeric(
    known_horizons
  )
  
  known_values <- as.numeric(
    known_values
  )
  
  if (
    length(
      target_horizon
    ) !=
    1L ||
    !is.finite(
      target_horizon
    )
  ) {
    stop(
      "target_horizon must be one finite numeric value.",
      call. = FALSE
    )
  }
  
  if (
    length(
      known_horizons
    ) !=
    length(
      known_values
    )
  ) {
    stop(
      "known_horizons and known_values must have the same length.",
      call. = FALSE
    )
  }
  
  if (
    length(
      known_horizons
    ) <
    1L
  ) {
    stop(
      "At least one known horizon is required.",
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        known_horizons
      )
    ) ||
    any(
      !is.finite(
        known_values
      )
    )
  ) {
    stop(
      "Known horizons and values must be finite.",
      call. = FALSE
    )
  }
  
  ordering <- order(
    known_horizons
  )
  
  known_horizons <- known_horizons[
    ordering
  ]
  
  known_values <- known_values[
    ordering
  ]
  
  if (
    anyDuplicated(
      known_horizons
    ) >
    0L
  ) {
    stop(
      "known_horizons must be unique.",
      call. = FALSE
    )
  }
  
  minimum_horizon <- min(
    known_horizons
  )
  
  maximum_horizon <- max(
    known_horizons
  )
  
  if (
    target_horizon >=
    minimum_horizon &&
    target_horizon <=
    maximum_horizon
  ) {
    return(
      stats::approx(
        x = known_horizons,
        y = known_values,
        xout = target_horizon,
        method = "linear",
        ties = "ordered"
      )[["y"]]
    )
  }
  
  if (
    extrapolation ==
    "flat"
  ) {
    if (
      target_horizon <
      minimum_horizon
    ) {
      return(
        known_values[[1]]
      )
    }
    
    return(
      known_values[[
        length(
          known_values
        )
      ]]
    )
  }
  
  if (
    extrapolation ==
    "zero"
  ) {
    return(
      0
    )
  }
  
  if (
    length(
      known_horizons
    ) <
    2L
  ) {
    stop(
      "Linear extrapolation requires at least two known horizons.",
      call. = FALSE
    )
  }
  
  if (
    target_horizon <
    minimum_horizon
  ) {
    x1 <- known_horizons[[1]]
    x2 <- known_horizons[[2]]
    
    y1 <- known_values[[1]]
    y2 <- known_values[[2]]
  } else {
    n <- length(
      known_horizons
    )
    
    x1 <- known_horizons[[n - 1L]]
    x2 <- known_horizons[[n]]
    
    y1 <- known_values[[n - 1L]]
    y2 <- known_values[[n]]
  }
  
  slope <- (
    y2 -
      y1
  ) /
    (
      x2 -
        x1
    )
  
  y1 +
    slope *
    (
      target_horizon -
        x1
    )
}


# ============================================================
# 26. Matrix Horizon Interpolation
# ============================================================

interpolate_horizon_matrix <- function(
    target_horizon,
    known_horizons,
    matrix_array,
    extrapolation = c(
      "flat",
      "linear",
      "zero"
    )
) {
  extrapolation <- match.arg(
    extrapolation
  )
  
  if (
    length(
      dim(
        matrix_array
      )
    ) !=
    3L
  ) {
    stop(
      "matrix_array must have dimensions horizon x row x column.",
      call. = FALSE
    )
  }
  
  if (
    dim(
      matrix_array
    )[[1]] !=
    length(
      known_horizons
    )
  ) {
    stop(
      "First matrix_array dimension must equal number of known horizons.",
      call. = FALSE
    )
  }
  
  result <- matrix(
    NA_real_,
    nrow = dim(
      matrix_array
    )[[2]],
    ncol = dim(
      matrix_array
    )[[3]]
  )
  
  for (
    row_index in
    seq_len(
      nrow(
        result
      )
    )
  ) {
    for (
      column_index in
      seq_len(
        ncol(
          result
        )
      )
    ) {
      result[
        row_index,
        column_index
      ] <- interpolate_horizon_values(
        target_horizon = target_horizon,
        known_horizons = known_horizons,
        known_values = matrix_array[
          ,
          row_index,
          column_index
        ],
        extrapolation = extrapolation
      )
    }
  }
  
  if (
    !is.null(
      dimnames(
        matrix_array
      )[[2]]
    )
  ) {
    rownames(
      result
    ) <- dimnames(
      matrix_array
    )[[2]]
  }
  
  if (
    !is.null(
      dimnames(
        matrix_array
      )[[3]]
    )
  ) {
    colnames(
      result
    ) <- dimnames(
      matrix_array
    )[[3]]
  }
  
  result
}


# ============================================================
# 27. Exponential Persistence Weight
# ============================================================

exponential_persistence_weight <- function(
    elapsed_minutes,
    half_life_minutes
) {
  elapsed_minutes <- as.numeric(
    elapsed_minutes
  )
  
  half_life_minutes <- as.numeric(
    half_life_minutes
  )
  
  if (
    any(
      !is.finite(
        elapsed_minutes
      )
    ) ||
    any(
      elapsed_minutes < 0
    )
  ) {
    stop(
      "elapsed_minutes must be finite and nonnegative.",
      call. = FALSE
    )
  }
  
  if (
    length(
      half_life_minutes
    ) !=
    1L ||
    !is.finite(
      half_life_minutes
    ) ||
    half_life_minutes <=
    0
  ) {
    stop(
      "half_life_minutes must be one positive finite value.",
      call. = FALSE
    )
  }
  
  exp(
    -log(
      2
    ) *
      elapsed_minutes /
      half_life_minutes
  )
}


# ============================================================
# 28. Apply Persistence Weight to Cross-Covariance
# ============================================================

apply_cross_covariance_persistence <- function(
    cross_covariance,
    weight
) {
  cross_covariance <- as.matrix(
    cross_covariance
  )
  
  if (
    length(
      weight
    ) !=
    1L ||
    !is.finite(
      weight
    ) ||
    weight <
    0 ||
    weight >
    1
  ) {
    stop(
      "weight must be one finite value between 0 and 1.",
      call. = FALSE
    )
  }
  
  cross_covariance *
    weight
}


# ============================================================
# 29. Combine Holdings and Residual Components
# ============================================================

combine_cross_covariance_components <- function(
    holdings_cross_covariance,
    residual_cross_covariance,
    holdings_weight = 1,
    residual_weight = 1
) {
  holdings_cross_covariance <- as.matrix(
    holdings_cross_covariance
  )
  
  residual_cross_covariance <- as.matrix(
    residual_cross_covariance
  )
  
  if (
    !identical(
      dim(
        holdings_cross_covariance
      ),
      dim(
        residual_cross_covariance
      )
    )
  ) {
    stop(
      "Holdings and residual cross-covariance matrices must have identical dimensions.",
      call. = FALSE
    )
  }
  
  if (
    length(
      holdings_weight
    ) !=
    1L ||
    !is.finite(
      holdings_weight
    )
  ) {
    stop(
      "holdings_weight must be one finite value.",
      call. = FALSE
    )
  }
  
  if (
    length(
      residual_weight
    ) !=
    1L ||
    !is.finite(
      residual_weight
    )
  ) {
    stop(
      "residual_weight must be one finite value.",
      call. = FALSE
    )
  }
  
  (
    holdings_weight *
      holdings_cross_covariance
  ) +
    (
      residual_weight *
        residual_cross_covariance
    )
}


# ============================================================
# 30. Build Joint Covariance Matrix
# ============================================================

build_joint_covariance_matrix <- function(
    state_covariance,
    futures_covariance,
    cross_covariance
) {
  state_covariance <- as.matrix(
    state_covariance
  )
  
  futures_covariance <- as.matrix(
    futures_covariance
  )
  
  cross_covariance <- as.matrix(
    cross_covariance
  )
  
  validate_square_matrix(
    state_covariance,
    object_name = "state_covariance",
    require_symmetric = TRUE
  )
  
  validate_square_matrix(
    futures_covariance,
    object_name = "futures_covariance",
    require_symmetric = TRUE
  )
  
  if (
    nrow(
      cross_covariance
    ) !=
    nrow(
      state_covariance
    )
  ) {
    stop(
      "Cross-covariance rows must equal state dimension.",
      call. = FALSE
    )
  }
  
  if (
    ncol(
      cross_covariance
    ) !=
    nrow(
      futures_covariance
    )
  ) {
    stop(
      "Cross-covariance columns must equal futures dimension.",
      call. = FALSE
    )
  }
  
  rbind(
    cbind(
      state_covariance,
      cross_covariance
    ),
    cbind(
      t(
        cross_covariance
      ),
      futures_covariance
    )
  )
}


# ============================================================
# 31. Schur Complement
# ============================================================

calculate_state_schur_complement <- function(
    state_covariance,
    futures_covariance,
    cross_covariance,
    eigenvalue_floor = 1e-12
) {
  state_covariance <- as.matrix(
    state_covariance
  )
  
  futures_covariance <- as.matrix(
    futures_covariance
  )
  
  cross_covariance <- as.matrix(
    cross_covariance
  )
  
  futures_inverse <- symmetric_generalized_inverse(
    futures_covariance,
    eigenvalue_floor = eigenvalue_floor
  )
  
  result <- (
    state_covariance -
      cross_covariance %*%
      futures_inverse %*%
      t(
        cross_covariance
      )
  )
  
  symmetrize_matrix(
    result
  )
}


# ============================================================
# 32. Maximum Canonical Correlation
# ============================================================

calculate_max_canonical_correlation <- function(
    state_covariance,
    futures_covariance,
    cross_covariance,
    eigenvalue_floor = 1e-12
) {
  state_covariance <- as.matrix(
    state_covariance
  )
  
  futures_covariance <- as.matrix(
    futures_covariance
  )
  
  cross_covariance <- as.matrix(
    cross_covariance
  )
  
  state_inverse_sqrt <- symmetric_matrix_inverse_sqrt(
    state_covariance,
    eigenvalue_floor = eigenvalue_floor
  )
  
  futures_inverse_sqrt <- symmetric_matrix_inverse_sqrt(
    futures_covariance,
    eigenvalue_floor = eigenvalue_floor
  )
  
  normalized_cross_covariance <- (
    state_inverse_sqrt %*%
      cross_covariance %*%
      futures_inverse_sqrt
  )
  
  singular_values <- svd(
    normalized_cross_covariance,
    nu = 0L,
    nv = 0L
  )[["d"]]
  
  if (
    length(
      singular_values
    ) ==
    0L
  ) {
    return(
      0
    )
  }
  
  max(
    singular_values
  )
}


# ============================================================
# 33. Joint Covariance PSD Diagnostics
# ============================================================

diagnose_joint_covariance <- function(
    state_covariance,
    futures_covariance,
    cross_covariance,
    eigenvalue_tolerance = 1e-10,
    eigenvalue_floor = 1e-12
) {
  joint_covariance <- build_joint_covariance_matrix(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    cross_covariance = cross_covariance
  )
  
  joint_covariance <- symmetrize_matrix(
    joint_covariance
  )
  
  joint_eigenvalues <- matrix_eigenvalues_symmetric(
    joint_covariance
  )
  
  schur_complement <- calculate_state_schur_complement(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    cross_covariance = cross_covariance,
    eigenvalue_floor = eigenvalue_floor
  )
  
  schur_eigenvalues <- matrix_eigenvalues_symmetric(
    schur_complement
  )
  
  max_canonical_correlation <- calculate_max_canonical_correlation(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    cross_covariance = cross_covariance,
    eigenvalue_floor = eigenvalue_floor
  )
  
  list(
    joint_covariance = joint_covariance,
    schur_complement = schur_complement,
    minimum_joint_eigenvalue = min(
      joint_eigenvalues
    ),
    minimum_schur_eigenvalue = min(
      schur_eigenvalues
    ),
    max_canonical_correlation = max_canonical_correlation,
    joint_psd = (
      min(
        joint_eigenvalues
      ) >=
        -eigenvalue_tolerance
    ),
    schur_psd = (
      min(
        schur_eigenvalues
      ) >=
        -eigenvalue_tolerance
    )
  )
}


# ============================================================
# 34. Cross-Covariance Shrink Factor
# ============================================================

calculate_cross_covariance_shrink_factor <- function(
    state_covariance,
    futures_covariance,
    cross_covariance,
    target_max_canonical_correlation = 0.999,
    eigenvalue_floor = 1e-12
) {
  if (
    length(
      target_max_canonical_correlation
    ) !=
    1L ||
    !is.finite(
      target_max_canonical_correlation
    ) ||
    target_max_canonical_correlation <=
    0 ||
    target_max_canonical_correlation >=
    1
  ) {
    stop(
      "target_max_canonical_correlation must be strictly between 0 and 1.",
      call. = FALSE
    )
  }
  
  current_max_canonical_correlation <- calculate_max_canonical_correlation(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    cross_covariance = cross_covariance,
    eigenvalue_floor = eigenvalue_floor
  )
  
  if (
    current_max_canonical_correlation <=
    target_max_canonical_correlation
  ) {
    return(
      1
    )
  }
  
  target_max_canonical_correlation /
    current_max_canonical_correlation
}


# ============================================================
# 35. PSD-Safe Cross-Covariance Shrinkage
# ============================================================

shrink_cross_covariance_to_psd <- function(
    state_covariance,
    futures_covariance,
    cross_covariance,
    target_max_canonical_correlation = 0.999,
    eigenvalue_tolerance = 1e-10,
    eigenvalue_floor = 1e-12
) {
  cross_covariance <- as.matrix(
    cross_covariance
  )
  
  shrink_factor <- calculate_cross_covariance_shrink_factor(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    cross_covariance = cross_covariance,
    target_max_canonical_correlation =
      target_max_canonical_correlation,
    eigenvalue_floor = eigenvalue_floor
  )
  
  shrunk_cross_covariance <- (
    shrink_factor *
      cross_covariance
  )
  
  diagnostics <- diagnose_joint_covariance(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    cross_covariance = shrunk_cross_covariance,
    eigenvalue_tolerance = eigenvalue_tolerance,
    eigenvalue_floor = eigenvalue_floor
  )
  
  if (
    !diagnostics[["joint_psd"]]
  ) {
    stop(
      "Cross-covariance shrinkage did not produce a PSD joint covariance matrix.",
      call. = FALSE
    )
  }
  
  list(
    original_cross_covariance = cross_covariance,
    cross_covariance = shrunk_cross_covariance,
    shrink_factor = shrink_factor,
    was_shrunk = (
      shrink_factor <
        1
    ),
    diagnostics = diagnostics
  )
}


# ============================================================
# 36. Build Production Cross-Covariance Array
# ============================================================

build_cross_covariance_array <- function(
    matrix_list,
    row_names = NULL,
    column_names = NULL
) {
  if (
    !is.list(
      matrix_list
    ) ||
    length(
      matrix_list
    ) <
    1L
  ) {
    stop(
      "matrix_list must be a nonempty list of matrices.",
      call. = FALSE
    )
  }
  
  first_matrix <- as.matrix(
    matrix_list[[1]]
  )
  
  number_of_times <- length(
    matrix_list
  )
  
  number_of_rows <- nrow(
    first_matrix
  )
  
  number_of_columns <- ncol(
    first_matrix
  )
  
  result <- array(
    NA_real_,
    dim = c(
      number_of_times,
      number_of_rows,
      number_of_columns
    )
  )
  
  for (
    time_index in
    seq_len(
      number_of_times
    )
  ) {
    current_matrix <- as.matrix(
      matrix_list[[time_index]]
    )
    
    if (
      !identical(
        dim(
          current_matrix
        ),
        c(
          number_of_rows,
          number_of_columns
        )
      )
    ) {
      stop(
        "All cross-covariance matrices must have identical dimensions.",
        call. = FALSE
      )
    }
    
    assert_finite_numeric(
      current_matrix,
      paste0(
        "matrix_list[[",
        time_index,
        "]]"
      )
    )
    
    result[
      time_index,
      ,
    ] <- current_matrix
  }
  
  if (
    is.null(
      row_names
    )
  ) {
    row_names <- rownames(
      first_matrix
    )
  }
  
  if (
    is.null(
      column_names
    )
  ) {
    column_names <- colnames(
      first_matrix
    )
  }
  
  dimnames(
    result
  ) <- list(
    NULL,
    row_names,
    column_names
  )
  
  result
}


# ============================================================
# 37. Long-Format State-Futures CSV Conversion
# ============================================================

state_futures_cross_covariance_to_long <- function(
    cross_covariance,
    t_original,
    state_names = COMPETITOR_FUTURES_STATE_NAMES,
    futures_names = COMPETITOR_FUTURES_NAMES
) {
  cross_covariance <- as.matrix(
    cross_covariance
  )
  
  if (
    nrow(
      cross_covariance
    ) !=
    length(
      state_names
    )
  ) {
    stop(
      "Cross-covariance row dimension does not match state_names.",
      call. = FALSE
    )
  }
  
  if (
    ncol(
      cross_covariance
    ) !=
    length(
      futures_names
    )
  ) {
    stop(
      "Cross-covariance column dimension does not match futures_names.",
      call. = FALSE
    )
  }
  
  if (
    length(
      t_original
    ) !=
    1L ||
    !is.finite(
      t_original
    )
  ) {
    stop(
      "t_original must be one finite numeric value.",
      call. = FALSE
    )
  }
  
  result <- expand.grid(
    state = state_names,
    future = futures_names,
    stringsAsFactors = FALSE
  )
  
  result[["state_index"]] <- match(
    result[["state"]],
    state_names
  )
  
  result[["future_index"]] <- match(
    result[["future"]],
    futures_names
  )
  
  result[["covariance"]] <- mapply(
    FUN = function(
    state_index,
    future_index
    ) {
      cross_covariance[
        state_index,
        future_index
      ]
    },
    state_index =
      result[["state_index"]],
    future_index =
      result[["future_index"]]
  )
  
  result[["t_original"]] <- as.integer(
    t_original
  )
  
  result <- result[
    c(
      "t_original",
      "state",
      "future",
      "covariance"
    )
  ]
  
  result
}


# ============================================================
# 38. Convert Full Production Array to Long Format
# ============================================================

state_futures_cross_covariance_array_to_long <- function(
    cross_covariance_array,
    t_original = NULL,
    state_names = COMPETITOR_FUTURES_STATE_NAMES,
    futures_names = COMPETITOR_FUTURES_NAMES
) {
  if (
    length(
      dim(
        cross_covariance_array
      )
    ) !=
    3L
  ) {
    stop(
      "cross_covariance_array must have dimensions time x state x future.",
      call. = FALSE
    )
  }
  
  number_of_times <- dim(
    cross_covariance_array
  )[[1]]
  
  if (
    dim(
      cross_covariance_array
    )[[2]] !=
    length(
      state_names
    )
  ) {
    stop(
      "State dimension does not match state_names.",
      call. = FALSE
    )
  }
  
  if (
    dim(
      cross_covariance_array
    )[[3]] !=
    length(
      futures_names
    )
  ) {
    stop(
      "Futures dimension does not match futures_names.",
      call. = FALSE
    )
  }
  
  if (
    is.null(
      t_original
    )
  ) {
    t_original <- 0:(
      number_of_times -
        1L
    )
  }
  
  if (
    length(
      t_original
    ) !=
    number_of_times
  ) {
    stop(
      "t_original length must equal production time dimension.",
      call. = FALSE
    )
  }
  
  rows <- vector(
    "list",
    number_of_times
  )
  
  for (
    time_index in
    seq_len(
      number_of_times
    )
  ) {
    rows[[time_index]] <- state_futures_cross_covariance_to_long(
      cross_covariance =
        cross_covariance_array[
          time_index,
          ,
          ,
          drop = TRUE
        ],
      t_original =
        t_original[[time_index]],
      state_names =
        state_names,
      futures_names =
        futures_names
    )
  }
  
  do.call(
    rbind,
    rows
  )
}


# ============================================================
# 39. Validate Long-Format Production Cross-Covariance
# ============================================================

validate_state_futures_long_format <- function(
    data,
    expected_times = NULL,
    state_names = COMPETITOR_FUTURES_STATE_NAMES,
    futures_names = COMPETITOR_FUTURES_NAMES
) {
  data <- as.data.frame(
    data
  )
  
  required_columns <- c(
    "t_original",
    "state",
    "future",
    "covariance"
  )
  
  missing_columns <- setdiff(
    required_columns,
    names(
      data
    )
  )
  
  if (
    length(
      missing_columns
    ) >
    0L
  ) {
    stop(
      paste0(
        "Long-format cross-covariance data is missing columns: ",
        paste(
          missing_columns,
          collapse = ", "
        )
      ),
      call. = FALSE
    )
  }
  
  if (
    any(
      !is.finite(
        data[["covariance"]]
      )
    )
  ) {
    stop(
      "Long-format covariance contains non-finite values.",
      call. = FALSE
    )
  }
  
  if (
    !setequal(
      unique(
        data[["state"]]
      ),
      state_names
    )
  ) {
    stop(
      "Long-format state names do not match expected states.",
      call. = FALSE
    )
  }
  
  if (
    !setequal(
      unique(
        data[["future"]]
      ),
      futures_names
    )
  ) {
    stop(
      "Long-format future names do not match expected futures.",
      call. = FALSE
    )
  }
  
  available_times <- sort(
    unique(
      as.integer(
        data[["t_original"]]
      )
    )
  )
  
  if (
    !is.null(
      expected_times
    ) &&
    !identical(
      available_times,
      sort(
        as.integer(
          expected_times
        )
      )
    )
  ) {
    stop(
      "Long-format time indices do not match expected_times.",
      call. = FALSE
    )
  }
  
  expected_pairs_per_time <- (
    length(
      state_names
    ) *
      length(
        futures_names
      )
  )
  
  for (
    current_time in
    available_times
  ) {
    current_data <- data[
      data[["t_original"]] ==
        current_time,
      ,
      drop = FALSE
    ]
    
    if (
      nrow(
        current_data
      ) !=
      expected_pairs_per_time
    ) {
      stop(
        paste0(
          "Incorrect row count for t_original=",
          current_time,
          "."
        ),
        call. = FALSE
      )
    }
    
    pair_key <- paste(
      current_data[["state"]],
      current_data[["future"]],
      sep = "||"
    )
    
    if (
      anyDuplicated(
        pair_key
      ) >
      0L
    ) {
      stop(
        paste0(
          "Duplicate state-future pair for t_original=",
          current_time,
          "."
        ),
        call. = FALSE
      )
    }
  }
  
  invisible(
    TRUE
  )
}


# ============================================================
# 40. Reconstruct Matrix From Long Format
# ============================================================

state_futures_long_to_matrix <- function(
    data,
    t_original,
    state_names = COMPETITOR_FUTURES_STATE_NAMES,
    futures_names = COMPETITOR_FUTURES_NAMES
) {
  data <- as.data.frame(
    data
  )
  
  current_data <- data[
    data[["t_original"]] ==
      t_original,
    ,
    drop = FALSE
  ]
  
  result <- matrix(
    NA_real_,
    nrow = length(
      state_names
    ),
    ncol = length(
      futures_names
    ),
    dimnames = list(
      state_names,
      futures_names
    )
  )
  
  for (
    current_state in
    state_names
  ) {
    for (
      current_future in
      futures_names
    ) {
      match_rows <- current_data[
        current_data[["state"]] ==
          current_state &
          current_data[["future"]] ==
          current_future,
        ,
        drop = FALSE
      ]
      
      if (
        nrow(
          match_rows
        ) !=
        1L
      ) {
        stop(
          paste0(
            "Expected exactly one row for t=",
            t_original,
            ", state=",
            current_state,
            ", future=",
            current_future,
            "."
          ),
          call. = FALSE
        )
      }
      
      result[
        current_state,
        current_future
      ] <- match_rows[["covariance"]][[1]]
    }
  }
  
  result
}


# ============================================================
# 41. Validate Long-Format Round Trip
# ============================================================

validate_state_futures_long_round_trip <- function(
    cross_covariance_array,
    long_data,
    t_original = NULL,
    state_names = COMPETITOR_FUTURES_STATE_NAMES,
    futures_names = COMPETITOR_FUTURES_NAMES,
    tolerance = 1e-12
) {
  number_of_times <- dim(
    cross_covariance_array
  )[[1]]
  
  if (
    is.null(
      t_original
    )
  ) {
    t_original <- 0:(
      number_of_times -
        1L
    )
  }
  
  maximum_error <- 0
  
  for (
    time_index in
    seq_len(
      number_of_times
    )
  ) {
    reconstructed <- state_futures_long_to_matrix(
      data = long_data,
      t_original =
        t_original[[time_index]],
      state_names =
        state_names,
      futures_names =
        futures_names
    )
    
    original <- cross_covariance_array[
      time_index,
      ,
      ,
      drop = TRUE
    ]
    
    current_error <- max(
      abs(
        reconstructed -
          original
      )
    )
    
    maximum_error <- max(
      maximum_error,
      current_error
    )
  }
  
  if (
    maximum_error >
    tolerance
  ) {
    stop(
      paste0(
        "Long-format round-trip error exceeds tolerance: ",
        format(
          maximum_error,
          scientific = TRUE
        )
      ),
      call. = FALSE
    )
  }
  
  maximum_error
}


# ============================================================
# 42. Structural Validation
# ============================================================

validate_competitor_futures_dependence_structure <- function() {
  expected_final_transform <- matrix(
    c(
      1,  0,  0,  0,  0,  0,
      1, -1,  0,  0,  0,  0,
      0,  1, -1,  0,  0,  0,
      0,  0,  1, -1,  0,  0,
      0,  0,  0,  1, -1,  0,
      0,  0,  0,  0,  1, -1
    ),
    nrow = 6L,
    byrow = TRUE
  )
  
  expected_daily_transform <- matrix(
    c(
      1,  0,
      1, -1
    ),
    nrow = 2L,
    byrow = TRUE
  )
  
  stopifnot(
    isTRUE(
      all.equal(
        unname(
          COMPETITOR_FUTURES_FINAL_STATE_TRANSFORM
        ),
        expected_final_transform,
        tolerance = 0
      )
    )
  )
  
  stopifnot(
    isTRUE(
      all.equal(
        unname(
          COMPETITOR_FUTURES_DAILY_STATE_TRANSFORM
        ),
        expected_daily_transform,
        tolerance = 0
      )
    )
  )
  
  stopifnot(
    all(
      dim(
        COMPETITOR_FUTURES_STATE_TRANSFORM
      ) ==
        c(
          8L,
          8L
        )
    )
  )
  
  stopifnot(
    identical(
      rownames(
        COMPETITOR_FUTURES_STATE_TRANSFORM
      ),
      COMPETITOR_FUTURES_STATE_NAMES
    )
  )
  
  stopifnot(
    identical(
      colnames(
        COMPETITOR_FUTURES_STATE_TRANSFORM
      ),
      COMPETITOR_FUTURES_RAW_NAMES
    )
  )
  
  stopifnot(
    length(
      COMPETITOR_FUTURES_NAMES
    ) ==
      8L
  )
  
  invisible(
    TRUE
  )
}


# ============================================================
# 43. Mathematical Unit Tests
# ============================================================

run_competitor_futures_dependence_unit_tests <- function() {
  set.seed(
    20260909
  )
  
  number_of_states <- 8L
  
  number_of_futures <- 8L
  
  
  # ----------------------------------------------------------
  # Test 1: State transformation
  # ----------------------------------------------------------
  
  test_raw_cross_covariance <- matrix(
    seq_len(
      number_of_states *
        number_of_futures
    ),
    nrow = number_of_states,
    ncol = number_of_futures
  )
  
  transformed_test <- transform_raw_cross_covariance_to_state(
    test_raw_cross_covariance
  )
  
  expected_transformed_test <- (
    COMPETITOR_FUTURES_STATE_TRANSFORM %*%
      test_raw_cross_covariance
  )
  
  stopifnot(
    isTRUE(
      all.equal(
        transformed_test,
        expected_transformed_test,
        tolerance = 0
      )
    )
  )
  
  
  # ----------------------------------------------------------
  # Test 2: Exposure beta uses full covariance
  # ----------------------------------------------------------
  
  test_beta <- matrix(
    c(
      1,
      rep(
        0,
        7
      )
    ),
    nrow = 1L
  )
  
  test_futures_covariance <- diag(
    8L
  )
  
  test_futures_covariance[
    1,
    2
  ] <- 0.25
  
  test_futures_covariance[
    2,
    1
  ] <- 0.25
  
  test_exposure_covariance <- exposure_beta_to_log_cross_covariance(
    beta_matrix = test_beta,
    futures_log_covariance = test_futures_covariance
  )
  
  stopifnot(
    abs(
      test_exposure_covariance[
        1,
        2
      ] -
        0.25
    ) <
      1e-12
  )
  
  
  # ----------------------------------------------------------
  # Test 3: Dollar-beta conversion
  # ----------------------------------------------------------
  
  test_dollar_beta <- c(
    100,
    200
  )
  
  test_log_beta <- dollar_beta_to_log_balance_beta(
    dollar_beta = test_dollar_beta,
    balance = 1000
  )
  
  stopifnot(
    isTRUE(
      all.equal(
        test_log_beta,
        c(
          0.1,
          0.2
        ),
        tolerance = 1e-12
      )
    )
  )
  
  
  # ----------------------------------------------------------
  # Test 4: Arithmetic cross-covariance conversion
  # ----------------------------------------------------------
  
  test_log_mean <- rep(
    0,
    8L
  )
  
  test_log_covariance <- diag(
    rep(
      0.01,
      8L
    )
  )
  
  test_state_log_cross <- matrix(
    1,
    nrow = 8L,
    ncol = 8L
  )
  
  test_state_arithmetic_cross <- convert_log_cross_covariance_to_arithmetic(
    state_log_cross_covariance =
      test_state_log_cross,
    futures_log_mean =
      test_log_mean,
    futures_log_covariance =
      test_log_covariance
  )
  
  expected_scaling <- exp(
    0.005
  )
  
  stopifnot(
    max(
      abs(
        test_state_arithmetic_cross -
          expected_scaling
      )
    ) <
      1e-12
  )
  
  
  # ----------------------------------------------------------
  # Test 5: Horizon interpolation
  # ----------------------------------------------------------
  
  test_interpolation <- interpolate_horizon_values(
    target_horizon = 90,
    known_horizons = c(
      60,
      120
    ),
    known_values = c(
      1,
      3
    ),
    extrapolation = "flat"
  )
  
  stopifnot(
    abs(
      test_interpolation -
        2
    ) <
      1e-12
  )
  
  
  # ----------------------------------------------------------
  # Test 6: Persistence
  # ----------------------------------------------------------
  
  test_persistence <- exponential_persistence_weight(
    elapsed_minutes = 120,
    half_life_minutes = 120
  )
  
  stopifnot(
    abs(
      test_persistence -
        0.5
    ) <
      1e-12
  )
  
  
  # ----------------------------------------------------------
  # Test 7: Joint covariance PSD when C = 0
  # ----------------------------------------------------------
  
  test_state_covariance <- diag(
    rep(
      1,
      8L
    )
  )
  
  test_futures_arithmetic_covariance <- diag(
    rep(
      1,
      8L
    )
  )
  
  zero_cross_covariance <- matrix(
    0,
    nrow = 8L,
    ncol = 8L
  )
  
  zero_diagnostics <- diagnose_joint_covariance(
    state_covariance =
      test_state_covariance,
    futures_covariance =
      test_futures_arithmetic_covariance,
    cross_covariance =
      zero_cross_covariance
  )
  
  stopifnot(
    isTRUE(
      zero_diagnostics[["joint_psd"]]
    )
  )
  
  stopifnot(
    abs(
      zero_diagnostics[["max_canonical_correlation"]]
    ) <
      1e-12
  )
  
  
  # ----------------------------------------------------------
  # Test 8: Shrink oversized cross-covariance
  # ----------------------------------------------------------
  
  oversized_cross_covariance <- diag(
    rep(
      2,
      8L
    )
  )
  
  shrink_result <- shrink_cross_covariance_to_psd(
    state_covariance =
      test_state_covariance,
    futures_covariance =
      test_futures_arithmetic_covariance,
    cross_covariance =
      oversized_cross_covariance,
    target_max_canonical_correlation =
      0.99
  )
  
  stopifnot(
    shrink_result[["shrink_factor"]] <
      1
  )
  
  stopifnot(
    shrink_result[["diagnostics"]][["joint_psd"]]
  )
  
  stopifnot(
    shrink_result[["diagnostics"]][["max_canonical_correlation"]] <=
      0.9900001
  )
  
  
  # ----------------------------------------------------------
  # Test 9: Long-format conversion
  # ----------------------------------------------------------
  
  test_cross_array <- array(
    0,
    dim = c(
      2L,
      8L,
      8L
    ),
    dimnames = list(
      NULL,
      COMPETITOR_FUTURES_STATE_NAMES,
      COMPETITOR_FUTURES_NAMES
    )
  )
  
  test_cross_array[
    1,
    ,
  ] <- matrix(
    seq_len(
      64L
    ),
    nrow = 8L,
    ncol = 8L
  )
  
  test_cross_array[
    2,
    ,
  ] <- matrix(
    seq_len(
      64L
    ) *
      2,
    nrow = 8L,
    ncol = 8L
  )
  
  test_long <- state_futures_cross_covariance_array_to_long(
    cross_covariance_array =
      test_cross_array,
    t_original =
      c(
        0L,
        1L
      )
  )
  
  stopifnot(
    nrow(
      test_long
    ) ==
      128L
  )
  
  validate_state_futures_long_format(
    data = test_long,
    expected_times = c(
      0L,
      1L
    )
  )
  
  round_trip_error <- validate_state_futures_long_round_trip(
    cross_covariance_array =
      test_cross_array,
    long_data =
      test_long,
    t_original =
      c(
        0L,
        1L
      )
  )
  
  stopifnot(
    round_trip_error <
      1e-12
  )
  
  
  # ----------------------------------------------------------
  # Test 10: Residualization
  # ----------------------------------------------------------
  
  test_returns <- matrix(
    c(
      0.01, 0.02,
      0.02, 0.01,
      -0.01, 0.03
    ),
    nrow = 3L,
    byrow = TRUE
  )
  
  test_beta_matrix <- matrix(
    c(
      2, 0,
      2, 0,
      2, 0
    ),
    nrow = 3L,
    byrow = TRUE
  )
  
  predicted_test <- predict_competitor_innovation_from_exposure(
    futures_log_returns =
      test_returns,
    beta_matrix =
      test_beta_matrix
  )
  
  stopifnot(
    isTRUE(
      all.equal(
        predicted_test,
        c(
          0.02,
          0.04,
          -0.02
        ),
        tolerance = 1e-12
      )
    )
  )
  
  invisible(
    TRUE
  )
}


# ============================================================
# 44. Script-Level Validation
# ============================================================

validate_competitor_futures_dependence_structure()

run_competitor_futures_dependence_unit_tests()

COMPETITOR_FUTURES_DEPENDENCE_SCRIPT_VALIDATION <- TRUE


# ============================================================
# 45. Completion Message
# ============================================================

message(
  paste0(
    "competitor_futures_dependence.R loaded successfully. ",
    "State dimension=8, futures dimension=8, ",
    "optimizer long-form schema=",
    "t_original/state/future/covariance, ",
    "structural and mathematical unit tests=TRUE."
  )
)