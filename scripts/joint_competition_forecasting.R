# ============================================================
# joint_competition_forecasting.R
#
# Reusable forecasting logic for the full 8D competition state.
#
# Raw competitor processes:
#
# q_t = [
#   r1,
#   r2,
#   r3,
#   r4,
#   r15,
#   r16,
#   h1,
#   h2
# ]
#
# Final-rank raw processes:
#   r1, r2, r3, r4, r15, r16
#
# Daily raw processes:
#   h1, h2
#
# Full competition state increments:
#
# Delta_S_t = [
#   Delta_Final_X1,
#   Delta_Final_D12,
#   Delta_Final_D23,
#   Delta_Final_D34,
#   Delta_Final_D4_15,
#   Delta_Final_D15_16,
#   Delta_Daily_H1,
#   Delta_Daily_D12
# ]
#
# The script:
#
# 1. aligns final and daily OOS standardized innovations;
# 2. estimates the full 8x8 residual correlation matrix;
# 3. constructs raw 8D means and covariance matrices;
# 4. constructs the block transformation M8;
# 5. transforms raw forecasts into 8D state forecasts;
# 6. validates all mean, covariance, correlation, and transform objects;
# 7. provides reusable state-futures covariance helpers for the
#    later futures-dependence stage.
#
# This script does NOT:
#
# - reselect final-rank models;
# - reselect daily-rank models;
# - write production CSV files;
# - assume final-daily residual independence;
# - estimate state-futures covariance unless aligned futures
#   innovations are explicitly supplied.
#
# Production CSV writing belongs to the later production notebook.
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

suppressPackageStartupMessages(
  library(
    tidyverse
  )
)

suppressPackageStartupMessages(
  library(
    here
  )
)


# ============================================================
# 2. Source completed marginal forecasting systems
# ============================================================

source(
  here(
    "scripts",
    "competitor_data.R"
  )
)

source(
  here(
    "scripts",
    "competitor_models.R"
  )
)

source(
  here(
    "scripts",
    "competitor_forecasting.R"
  )
)

source(
  here(
    "scripts",
    "daily_competitor_data.R"
  )
)

source(
  here(
    "scripts",
    "daily_competitor_models.R"
  )
)

source(
  here(
    "scripts",
    "daily_competitor_forecasting.R"
  )
)


# ============================================================
# 3. Production dimensions and names
# ============================================================

JOINT_FINAL_RAW_NAMES <-
  c(
    "r1",
    "r2",
    "r3",
    "r4",
    "r15",
    "r16"
  )

JOINT_DAILY_RAW_NAMES <-
  c(
    "h1",
    "h2"
  )

JOINT_RAW_NAMES <-
  c(
    JOINT_FINAL_RAW_NAMES,
    JOINT_DAILY_RAW_NAMES
  )

JOINT_FINAL_STATE_NAMES <-
  c(
    "Delta_Final_X1",
    "Delta_Final_D12",
    "Delta_Final_D23",
    "Delta_Final_D34",
    "Delta_Final_D4_15",
    "Delta_Final_D15_16"
  )

JOINT_DAILY_STATE_NAMES <-
  c(
    "Delta_Daily_H1",
    "Delta_Daily_D12"
  )

JOINT_STATE_NAMES <-
  c(
    JOINT_FINAL_STATE_NAMES,
    JOINT_DAILY_STATE_NAMES
  )

JOINT_RAW_DIMENSION <-
  length(
    JOINT_RAW_NAMES
  )

JOINT_STATE_DIMENSION <-
  length(
    JOINT_STATE_NAMES
  )

stopifnot(
  JOINT_RAW_DIMENSION ==
    8L
)

stopifnot(
  JOINT_STATE_DIMENSION ==
    8L
)


# ============================================================
# 4. Structural transformations
# ============================================================

JOINT_FINAL_STATE_TRANSFORM <-
  matrix(
    c(
      1, 1, 0, 0, 0, 0,
      0, -1, 1, 0, 0, 0,
      0, 0, -1, 1, 0, 0,
      0, 0, 0, -1, 1, 0,
      0, 0, 0, 0, -1, 1,
      0, 0, 0, 0, 0, -1
    ),
    nrow =
      6,
    byrow =
      FALSE
  )

rownames(
  JOINT_FINAL_STATE_TRANSFORM
) <-
  JOINT_FINAL_STATE_NAMES

colnames(
  JOINT_FINAL_STATE_TRANSFORM
) <-
  JOINT_FINAL_RAW_NAMES


JOINT_DAILY_STATE_TRANSFORM <-
  matrix(
    c(
      1,
      1,
      0,
      -1
    ),
    nrow =
      2,
    byrow =
      FALSE
  )

rownames(
  JOINT_DAILY_STATE_TRANSFORM
) <-
  JOINT_DAILY_STATE_NAMES

colnames(
  JOINT_DAILY_STATE_TRANSFORM
) <-
  JOINT_DAILY_RAW_NAMES


build_joint_state_transform <-
  function() {
    
    transform_matrix <-
      matrix(
        0,
        nrow =
          8,
        ncol =
          8
      )
    
    transform_matrix[
      1:6,
      1:6
    ] <-
      JOINT_FINAL_STATE_TRANSFORM
    
    transform_matrix[
      7:8,
      7:8
    ] <-
      JOINT_DAILY_STATE_TRANSFORM
    
    rownames(
      transform_matrix
    ) <-
      JOINT_STATE_NAMES
    
    colnames(
      transform_matrix
    ) <-
      JOINT_RAW_NAMES
    
    transform_matrix
  }


JOINT_STATE_TRANSFORM <-
  build_joint_state_transform()


# ============================================================
# 5. Basic scalar validation helpers
# ============================================================

is_finite_scalar <-
  function(
    x
  ) {
    
    length(
      x
    ) ==
      1L &&
      is.finite(
        x
      )
  }


is_positive_finite_scalar <-
  function(
    x
  ) {
    
    is_finite_scalar(
      x
    ) &&
      x >
      0
  }


is_nonnegative_finite_scalar <-
  function(
    x
  ) {
    
    is_finite_scalar(
      x
    ) &&
      x >=
      0
  }


# ============================================================
# 6. Matrix validation helpers
# ============================================================

validate_named_square_matrix <-
  function(
    x,
    expected_names,
    tolerance =
      1e-10
  ) {
    
    if (
      !is.matrix(
        x
      )
    ) {
      
      stop(
        "Object must be a matrix."
      )
    }
    
    expected_dimension <-
      length(
        expected_names
      )
    
    if (
      nrow(
        x
      ) !=
      expected_dimension ||
      ncol(
        x
      ) !=
      expected_dimension
    ) {
      
      stop(
        "Matrix has incorrect dimensions."
      )
    }
    
    if (
      !all(
        is.finite(
          x
        )
      )
    ) {
      
      stop(
        "Matrix contains non-finite values."
      )
    }
    
    if (
      max(
        abs(
          x -
          t(
            x
          )
        )
      ) >
      tolerance
    ) {
      
      stop(
        "Matrix is not symmetric."
      )
    }
    
    invisible(
      TRUE
    )
  }


validate_covariance_matrix <-
  function(
    covariance_matrix,
    expected_names,
    tolerance =
      1e-8,
    require_positive_definite =
      FALSE
  ) {
    
    validate_named_square_matrix(
      x =
        covariance_matrix,
      expected_names =
        expected_names,
      tolerance =
        tolerance
    )
    
    if (
      any(
        diag(
          covariance_matrix
        ) <
        -tolerance
      )
    ) {
      
      stop(
        "Covariance matrix contains a negative diagonal variance."
      )
    }
    
    covariance_eigenvalues <-
      eigen(
        covariance_matrix,
        symmetric =
          TRUE,
        only.values =
          TRUE
      )[["values"]]
    
    if (
      any(
        covariance_eigenvalues <
        -tolerance
      )
    ) {
      
      stop(
        "Covariance matrix is not positive semidefinite."
      )
    }
    
    if (
      require_positive_definite &&
      any(
        covariance_eigenvalues <=
        tolerance
      )
    ) {
      
      stop(
        "Covariance matrix is not positive definite."
      )
    }
    
    invisible(
      TRUE
    )
  }


validate_correlation_matrix <-
  function(
    correlation_matrix,
    expected_names,
    tolerance =
      1e-8,
    require_positive_definite =
      TRUE
  ) {
    
    validate_named_square_matrix(
      x =
        correlation_matrix,
      expected_names =
        expected_names,
      tolerance =
        tolerance
    )
    
    if (
      max(
        abs(
          diag(
            correlation_matrix
          ) -
          1
        )
      ) >
      tolerance
    ) {
      
      stop(
        "Correlation matrix diagonal is not equal to one."
      )
    }
    
    if (
      any(
        correlation_matrix >
        1 +
        tolerance
      ) ||
      any(
        correlation_matrix <
        -1 -
        tolerance
      )
    ) {
      
      stop(
        "Correlation matrix contains entries outside [-1, 1]."
      )
    }
    
    correlation_eigenvalues <-
      eigen(
        correlation_matrix,
        symmetric =
          TRUE,
        only.values =
          TRUE
      )[["values"]]
    
    if (
      any(
        correlation_eigenvalues <
        -tolerance
      )
    ) {
      
      stop(
        "Correlation matrix is not positive semidefinite."
      )
    }
    
    if (
      require_positive_definite &&
      any(
        correlation_eigenvalues <=
        tolerance
      )
    ) {
      
      stop(
        "Correlation matrix is not positive definite."
      )
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 7. Normalize innovation panel column names
# ============================================================

rename_standardized_innovation_columns <-
  function(
    data,
    timestamp_column,
    innovation_columns,
    output_names
  ) {
    
    if (
      !is.data.frame(
        data
      )
    ) {
      
      stop(
        "Innovation panel must be a data frame."
      )
    }
    
    if (
      length(
        innovation_columns
      ) !=
      length(
        output_names
      )
    ) {
      
      stop(
        "Innovation-column and output-name lengths do not match."
      )
    }
    
    required_columns <-
      c(
        timestamp_column,
        innovation_columns
      )
    
    missing_columns <-
      setdiff(
        required_columns,
        names(
          data
        )
      )
    
    if (
      length(
        missing_columns
      ) >
      0
    ) {
      
      stop(
        paste0(
          "Missing innovation-panel columns: ",
          paste(
            missing_columns,
            collapse =
              ", "
          )
        )
      )
    }
    
    output <-
      data[
        ,
        required_columns,
        drop =
          FALSE
      ]
    
    names(
      output
    ) <-
      c(
        "Timestamp",
        output_names
      )
    
    output[["Timestamp"]] <-
      as.character(
        output[["Timestamp"]]
      )
    
    for (
      variable_name in output_names
    ) {
      
      output[[variable_name]] <-
        as.numeric(
          output[[variable_name]]
        )
    }
    
    output
  }


# ============================================================
# 8. Validate standardized innovation panel
# ============================================================

validate_standardized_innovation_panel <-
  function(
    data,
    innovation_names
  ) {
    
    required_columns <-
      c(
        "Timestamp",
        innovation_names
      )
    
    missing_columns <-
      setdiff(
        required_columns,
        names(
          data
        )
      )
    
    if (
      length(
        missing_columns
      ) >
      0
    ) {
      
      stop(
        paste0(
          "Missing standardized innovation columns: ",
          paste(
            missing_columns,
            collapse =
              ", "
          )
        )
      )
    }
    
    if (
      any(
        is.na(
          data[["Timestamp"]]
        )
      )
    ) {
      
      stop(
        "Innovation panel contains missing timestamps."
      )
    }
    
    if (
      anyDuplicated(
        data[["Timestamp"]]
      ) >
      0
    ) {
      
      stop(
        "Innovation panel contains duplicated timestamps."
      )
    }
    
    for (
      variable_name in innovation_names
    ) {
      
      if (
        !all(
          is.finite(
            data[[variable_name]]
          )
        )
      ) {
        
        stop(
          paste0(
            "Non-finite standardized innovations found in ",
            variable_name,
            "."
          )
        )
      }
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 9. Align final and daily OOS standardized innovations
# ============================================================

align_joint_standardized_innovations <-
  function(
    final_innovations,
    daily_innovations,
    final_timestamp_column =
      "Timestamp",
    daily_timestamp_column =
      "Timestamp",
    final_innovation_columns =
      JOINT_FINAL_RAW_NAMES,
    daily_innovation_columns =
      JOINT_DAILY_RAW_NAMES
  ) {
    
    final_panel <-
      rename_standardized_innovation_columns(
        data =
          final_innovations,
        timestamp_column =
          final_timestamp_column,
        innovation_columns =
          final_innovation_columns,
        output_names =
          JOINT_FINAL_RAW_NAMES
      )
    
    daily_panel <-
      rename_standardized_innovation_columns(
        data =
          daily_innovations,
        timestamp_column =
          daily_timestamp_column,
        innovation_columns =
          daily_innovation_columns,
        output_names =
          JOINT_DAILY_RAW_NAMES
      )
    
    validate_standardized_innovation_panel(
      data =
        final_panel,
      innovation_names =
        JOINT_FINAL_RAW_NAMES
    )
    
    validate_standardized_innovation_panel(
      data =
        daily_panel,
      innovation_names =
        JOINT_DAILY_RAW_NAMES
    )
    
    aligned_panel <-
      inner_join(
        final_panel,
        daily_panel,
        by =
          "Timestamp"
      ) |>
      arrange(
        Timestamp
      )
    
    if (
      nrow(
        aligned_panel
      ) <
      9L
    ) {
      
      stop(
        paste0(
          "Too few aligned observations to estimate an 8D ",
          "correlation matrix."
        )
      )
    }
    
    validate_standardized_innovation_panel(
      data =
        aligned_panel,
      innovation_names =
        JOINT_RAW_NAMES
    )
    
    aligned_panel
  }


# ============================================================
# 10. Estimate full 8x8 residual correlation matrix
# ============================================================

estimate_joint_residual_correlation <-
  function(
    aligned_innovations,
    method =
      "pearson",
    require_positive_definite =
      TRUE
  ) {
    
    validate_standardized_innovation_panel(
      data =
        aligned_innovations,
      innovation_names =
        JOINT_RAW_NAMES
    )
    
    innovation_matrix <-
      as.matrix(
        aligned_innovations[
          ,
          JOINT_RAW_NAMES,
          drop =
            FALSE
        ]
      )
    
    residual_correlation <-
      cor(
        innovation_matrix,
        method =
          method,
        use =
          "complete.obs"
      )
    
    rownames(
      residual_correlation
    ) <-
      JOINT_RAW_NAMES
    
    colnames(
      residual_correlation
    ) <-
      JOINT_RAW_NAMES
    
    validate_correlation_matrix(
      correlation_matrix =
        residual_correlation,
      expected_names =
        JOINT_RAW_NAMES,
      require_positive_definite =
        require_positive_definite
    )
    
    residual_correlation
  }


# ============================================================
# 11. Extract correlation blocks
# ============================================================

extract_joint_correlation_blocks <-
  function(
    residual_correlation
  ) {
    
    validate_correlation_matrix(
      correlation_matrix =
        residual_correlation,
      expected_names =
        JOINT_RAW_NAMES
    )
    
    list(
      final_final =
        residual_correlation[
          JOINT_FINAL_RAW_NAMES,
          JOINT_FINAL_RAW_NAMES,
          drop =
            FALSE
        ],
      daily_daily =
        residual_correlation[
          JOINT_DAILY_RAW_NAMES,
          JOINT_DAILY_RAW_NAMES,
          drop =
            FALSE
        ],
      final_daily =
        residual_correlation[
          JOINT_FINAL_RAW_NAMES,
          JOINT_DAILY_RAW_NAMES,
          drop =
            FALSE
        ],
      daily_final =
        residual_correlation[
          JOINT_DAILY_RAW_NAMES,
          JOINT_FINAL_RAW_NAMES,
          drop =
            FALSE
        ]
    )
  }


# ============================================================
# 12. Build raw 8D mean vector
# ============================================================

build_joint_raw_mean <-
  function(
    final_mean,
    daily_mean
  ) {
    
    final_mean <-
      as.numeric(
        final_mean
      )
    
    daily_mean <-
      as.numeric(
        daily_mean
      )
    
    if (
      length(
        final_mean
      ) !=
      6L
    ) {
      
      stop(
        "Final mean vector must contain exactly 6 values."
      )
    }
    
    if (
      length(
        daily_mean
      ) !=
      2L
    ) {
      
      stop(
        "Daily mean vector must contain exactly 2 values."
      )
    }
    
    joint_mean <-
      c(
        final_mean,
        daily_mean
      )
    
    names(
      joint_mean
    ) <-
      JOINT_RAW_NAMES
    
    if (
      !all(
        is.finite(
          joint_mean
        )
      )
    ) {
      
      stop(
        "Joint raw mean vector contains non-finite values."
      )
    }
    
    joint_mean
  }


# ============================================================
# 13. Build raw 8D standard deviation vector
# ============================================================

build_joint_raw_sd <-
  function(
    final_variance,
    daily_variance
  ) {
    
    final_variance <-
      as.numeric(
        final_variance
      )
    
    daily_variance <-
      as.numeric(
        daily_variance
      )
    
    if (
      length(
        final_variance
      ) !=
      6L
    ) {
      
      stop(
        "Final variance vector must contain exactly 6 values."
      )
    }
    
    if (
      length(
        daily_variance
      ) !=
      2L
    ) {
      
      stop(
        "Daily variance vector must contain exactly 2 values."
      )
    }
    
    joint_variance <-
      c(
        final_variance,
        daily_variance
      )
    
    names(
      joint_variance
    ) <-
      JOINT_RAW_NAMES
    
    if (
      !all(
        is.finite(
          joint_variance
        )
      )
    ) {
      
      stop(
        "Joint raw variance vector contains non-finite values."
      )
    }
    
    if (
      any(
        joint_variance <=
        0
      )
    ) {
      
      stop(
        "All joint raw variances must be strictly positive."
      )
    }
    
    joint_sd <-
      sqrt(
        joint_variance
      )
    
    names(
      joint_sd
    ) <-
      JOINT_RAW_NAMES
    
    joint_sd
  }


# ============================================================
# 14. Build raw 8x8 covariance matrix
# ============================================================

construct_joint_raw_covariance <-
  function(
    joint_sd,
    residual_correlation
  ) {
    
    joint_sd <-
      as.numeric(
        joint_sd
      )
    
    if (
      length(
        joint_sd
      ) !=
      8L
    ) {
      
      stop(
        "Joint standard-deviation vector must contain 8 values."
      )
    }
    
    names(
      joint_sd
    ) <-
      JOINT_RAW_NAMES
    
    if (
      !all(
        is.finite(
          joint_sd
        )
      )
    ) {
      
      stop(
        "Joint standard-deviation vector contains non-finite values."
      )
    }
    
    if (
      any(
        joint_sd <=
        0
      )
    ) {
      
      stop(
        "Joint standard deviations must be strictly positive."
      )
    }
    
    validate_correlation_matrix(
      correlation_matrix =
        residual_correlation,
      expected_names =
        JOINT_RAW_NAMES
    )
    
    scale_matrix <-
      diag(
        joint_sd,
        nrow =
          8,
        ncol =
          8
      )
    
    rownames(
      scale_matrix
    ) <-
      JOINT_RAW_NAMES
    
    colnames(
      scale_matrix
    ) <-
      JOINT_RAW_NAMES
    
    raw_covariance <-
      scale_matrix %*%
      residual_correlation %*%
      scale_matrix
    
    rownames(
      raw_covariance
    ) <-
      JOINT_RAW_NAMES
    
    colnames(
      raw_covariance
    ) <-
      JOINT_RAW_NAMES
    
    validate_covariance_matrix(
      covariance_matrix =
        raw_covariance,
      expected_names =
        JOINT_RAW_NAMES,
      require_positive_definite =
        TRUE
    )
    
    raw_covariance
  }


# ============================================================
# 15. Alternative covariance constructor from variances
# ============================================================

construct_joint_raw_covariance_from_variances <-
  function(
    final_variance,
    daily_variance,
    residual_correlation
  ) {
    
    joint_sd <-
      build_joint_raw_sd(
        final_variance =
          final_variance,
        daily_variance =
          daily_variance
      )
    
    construct_joint_raw_covariance(
      joint_sd =
        joint_sd,
      residual_correlation =
        residual_correlation
    )
  }


# ============================================================
# 16. Transform raw mean into state mean
# ============================================================

transform_joint_mean_to_state <-
  function(
    raw_mean
  ) {
    
    raw_mean <-
      as.numeric(
        raw_mean
      )
    
    if (
      length(
        raw_mean
      ) !=
      8L
    ) {
      
      stop(
        "Raw joint mean vector must contain 8 values."
      )
    }
    
    if (
      !all(
        is.finite(
          raw_mean
        )
      )
    ) {
      
      stop(
        "Raw joint mean vector contains non-finite values."
      )
    }
    
    state_mean <-
      as.numeric(
        JOINT_STATE_TRANSFORM %*%
          raw_mean
      )
    
    names(
      state_mean
    ) <-
      JOINT_STATE_NAMES
    
    state_mean
  }


# ============================================================
# 17. Transform raw covariance into state covariance
# ============================================================

transform_joint_covariance_to_state <-
  function(
    raw_covariance
  ) {
    
    validate_covariance_matrix(
      covariance_matrix =
        raw_covariance,
      expected_names =
        JOINT_RAW_NAMES,
      require_positive_definite =
        TRUE
    )
    
    state_covariance <-
      JOINT_STATE_TRANSFORM %*%
      raw_covariance %*%
      t(
        JOINT_STATE_TRANSFORM
      )
    
    rownames(
      state_covariance
    ) <-
      JOINT_STATE_NAMES
    
    colnames(
      state_covariance
    ) <-
      JOINT_STATE_NAMES
    
    validate_covariance_matrix(
      covariance_matrix =
        state_covariance,
      expected_names =
        JOINT_STATE_NAMES,
      require_positive_definite =
        TRUE
    )
    
    state_covariance
  }


# ============================================================
# 18. Build one complete joint competitor interval forecast
# ============================================================

construct_joint_competition_interval <-
  function(
    final_mean,
    final_variance,
    daily_mean,
    daily_variance,
    residual_correlation
  ) {
    
    raw_mean <-
      build_joint_raw_mean(
        final_mean =
          final_mean,
        daily_mean =
          daily_mean
      )
    
    raw_sd <-
      build_joint_raw_sd(
        final_variance =
          final_variance,
        daily_variance =
          daily_variance
      )
    
    raw_covariance <-
      construct_joint_raw_covariance(
        joint_sd =
          raw_sd,
        residual_correlation =
          residual_correlation
      )
    
    state_mean <-
      transform_joint_mean_to_state(
        raw_mean =
          raw_mean
      )
    
    state_covariance <-
      transform_joint_covariance_to_state(
        raw_covariance =
          raw_covariance
      )
    
    output <-
      list(
        raw_mean =
          raw_mean,
        raw_sd =
          raw_sd,
        raw_covariance =
          raw_covariance,
        state_mean =
          state_mean,
        state_covariance =
          state_covariance,
        residual_correlation =
          residual_correlation,
        state_transform =
          JOINT_STATE_TRANSFORM
      )
    
    validate_joint_competition_interval(
      output
    )
    
    output
  }


# ============================================================
# 19. Validate one complete joint interval
# ============================================================

validate_joint_competition_interval <-
  function(
    forecast_object
  ) {
    
    required_components <-
      c(
        "raw_mean",
        "raw_sd",
        "raw_covariance",
        "state_mean",
        "state_covariance",
        "residual_correlation",
        "state_transform"
      )
    
    missing_components <-
      setdiff(
        required_components,
        names(
          forecast_object
        )
      )
    
    if (
      length(
        missing_components
      ) >
      0
    ) {
      
      stop(
        paste0(
          "Joint forecast object is missing components: ",
          paste(
            missing_components,
            collapse =
              ", "
          )
        )
      )
    }
    
    if (
      length(
        forecast_object[["raw_mean"]]
      ) !=
      8L ||
      !all(
        is.finite(
          forecast_object[["raw_mean"]]
        )
      )
    ) {
      
      stop(
        "Invalid raw joint mean."
      )
    }
    
    if (
      length(
        forecast_object[["raw_sd"]]
      ) !=
      8L ||
      !all(
        is.finite(
          forecast_object[["raw_sd"]]
        )
      ) ||
      any(
        forecast_object[["raw_sd"]] <=
        0
      )
    ) {
      
      stop(
        "Invalid raw joint standard deviations."
      )
    }
    
    if (
      length(
        forecast_object[["state_mean"]]
      ) !=
      8L ||
      !all(
        is.finite(
          forecast_object[["state_mean"]]
        )
      )
    ) {
      
      stop(
        "Invalid joint state mean."
      )
    }
    
    validate_correlation_matrix(
      correlation_matrix =
        forecast_object[["residual_correlation"]],
      expected_names =
        JOINT_RAW_NAMES
    )
    
    validate_covariance_matrix(
      covariance_matrix =
        forecast_object[["raw_covariance"]],
      expected_names =
        JOINT_RAW_NAMES,
      require_positive_definite =
        TRUE
    )
    
    validate_covariance_matrix(
      covariance_matrix =
        forecast_object[["state_covariance"]],
      expected_names =
        JOINT_STATE_NAMES,
      require_positive_definite =
        TRUE
    )
    
    if (
      !isTRUE(
        all.equal(
          forecast_object[["state_transform"]],
          JOINT_STATE_TRANSFORM,
          tolerance =
          1e-12
        )
      )
    ) {
      
      stop(
        "Joint state transformation does not match M8."
      )
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 20. Validate structural M8 transformation
# ============================================================

validate_joint_state_transform <-
  function() {
    
    if (
      !is.matrix(
        JOINT_STATE_TRANSFORM
      )
    ) {
      
      stop(
        "Joint state transform is not a matrix."
      )
    }
    
    if (
      !identical(
        dim(
          JOINT_STATE_TRANSFORM
        ),
        c(
          8L,
          8L
        )
      )
    ) {
      
      stop(
        "Joint state transform must be 8x8."
      )
    }
    
    if (
      !isTRUE(
        all.equal(
          JOINT_STATE_TRANSFORM[
            1:6,
            1:6,
            drop =
            FALSE
          ],
          JOINT_FINAL_STATE_TRANSFORM,
          tolerance =
          1e-12
        )
      )
    ) {
      
      stop(
        "Final-rank block of M8 is incorrect."
      )
    }
    
    if (
      !isTRUE(
        all.equal(
          JOINT_STATE_TRANSFORM[
            7:8,
            7:8,
            drop =
            FALSE
          ],
          JOINT_DAILY_STATE_TRANSFORM,
          tolerance =
          1e-12
        )
      )
    ) {
      
      stop(
        "Daily-rank block of M8 is incorrect."
      )
    }
    
    if (
      any(
        JOINT_STATE_TRANSFORM[
          1:6,
          7:8,
          drop =
          FALSE
        ] !=
        0
      )
    ) {
      
      stop(
        "Upper-right block of M8 must be zero."
      )
    }
    
    if (
      any(
        JOINT_STATE_TRANSFORM[
          7:8,
          1:6,
          drop =
          FALSE
        ] !=
        0
      )
    ) {
      
      stop(
        "Lower-left block of M8 must be zero."
      )
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 21. Convert a sequence of state means to a table
# ============================================================

joint_state_mean_list_to_table <-
  function(
    state_mean_list,
    timestamps =
      NULL
  ) {
    
    if (
      length(
        state_mean_list
      ) ==
      0L
    ) {
      
      stop(
        "State-mean list is empty."
      )
    }
    
    mean_matrix <-
      do.call(
        rbind,
        lapply(
          state_mean_list,
          function(
    x
          ) {
            
            x <-
              as.numeric(
                x
              )
            
            if (
              length(
                x
              ) !=
              8L
            ) {
              
              stop(
                "Each state mean must contain 8 values."
              )
            }
            
            x
          }
        )
      )
    
    colnames(
      mean_matrix
    ) <-
      JOINT_STATE_NAMES
    
    output <-
      as_tibble(
        mean_matrix
      )
    
    if (
      !is.null(
        timestamps
      )
    ) {
      
      if (
        length(
          timestamps
        ) !=
        nrow(
          output
        )
      ) {
        
        stop(
          "Timestamp count does not match state-mean count."
        )
      }
      
      output <-
        bind_cols(
          tibble(
            Timestamp =
              as.character(
                timestamps
              )
          ),
          output
        )
    }
    
    output
  }


# ============================================================
# 22. Convert a sequence of state covariances to an array
# ============================================================

joint_state_covariance_list_to_array <-
  function(
    state_covariance_list
  ) {
    
    number_of_forecasts <-
      length(
        state_covariance_list
      )
    
    if (
      number_of_forecasts ==
      0L
    ) {
      
      stop(
        "State-covariance list is empty."
      )
    }
    
    output_array <-
      array(
        NA_real_,
        dim =
          c(
            8,
            8,
            number_of_forecasts
          ),
        dimnames =
          list(
            JOINT_STATE_NAMES,
            JOINT_STATE_NAMES,
            NULL
          )
      )
    
    for (
      i in seq_len(
        number_of_forecasts
      )
    ) {
      
      covariance_matrix <-
        state_covariance_list[[i]]
      
      validate_covariance_matrix(
        covariance_matrix =
          covariance_matrix,
        expected_names =
          JOINT_STATE_NAMES
      )
      
      output_array[
        ,
        ,
        i
      ] <-
        covariance_matrix
    }
    
    output_array
  }


# ============================================================
# 23. Convert covariance matrices to long tables
# ============================================================

covariance_matrix_to_long_table <-
  function(
    covariance_matrix,
    timestamp =
      NULL
  ) {
    
    if (
      !is.matrix(
        covariance_matrix
      ) ||
      nrow(
        covariance_matrix
      ) !=
      ncol(
        covariance_matrix
      )
    ) {
      
      stop(
        "Covariance input must be a square matrix."
      )
    }
    
    variable_names <-
      rownames(
        covariance_matrix
      )
    
    if (
      is.null(
        variable_names
      )
    ) {
      
      variable_names <-
        paste0(
          "V",
          seq_len(
            nrow(
              covariance_matrix
            )
          )
        )
    }
    
    output <-
      expand_grid(
        Row =
          variable_names,
        Column =
          variable_names
      )
    
    output[["Covariance"]] <-
      as.numeric(
        covariance_matrix[
          cbind(
            match(
              output[["Row"]],
              variable_names
            ),
            match(
              output[["Column"]],
              variable_names
            )
          )
        ]
      )
    
    if (
      !is.null(
        timestamp
      )
    ) {
      
      output <-
        output |>
        mutate(
          Timestamp =
            as.character(
              timestamp
            ),
          .before =
            1
        )
    }
    
    output
  }


# ============================================================
# 24. Estimate generic covariance between two innovation panels
# ============================================================

estimate_cross_covariance <-
  function(
    x,
    y
  ) {
    
    x <-
      as.matrix(
        x
      )
    
    y <-
      as.matrix(
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
        "Innovation panels must have the same number of rows."
      )
    }
    
    if (
      nrow(
        x
      ) <
      2L
    ) {
      
      stop(
        "At least two aligned observations are required."
      )
    }
    
    if (
      !all(
        is.finite(
          x
        )
      ) ||
      !all(
        is.finite(
          y
        )
      )
    ) {
      
      stop(
        "Innovation panels contain non-finite values."
      )
    }
    
    x_centered <-
      sweep(
        x,
        2,
        colMeans(
          x
        ),
        FUN =
          "-"
      )
    
    y_centered <-
      sweep(
        y,
        2,
        colMeans(
          y
        ),
        FUN =
          "-"
      )
    
    cross_covariance <-
      crossprod(
        x_centered,
        y_centered
      ) /
      (
        nrow(
          x
        ) -
          1
      )
    
    cross_covariance
  }


# ============================================================
# 25. Transform raw competitor-futures covariance to state space
# ============================================================

transform_raw_futures_covariance_to_state <-
  function(
    raw_futures_covariance
  ) {
    
    raw_futures_covariance <-
      as.matrix(
        raw_futures_covariance
      )
    
    if (
      nrow(
        raw_futures_covariance
      ) !=
      8L
    ) {
      
      stop(
        paste0(
          "Raw competitor-futures covariance must have ",
          "8 competitor rows."
        )
      )
    }
    
    if (
      !all(
        is.finite(
          raw_futures_covariance
        )
      )
    ) {
      
      stop(
        "Raw competitor-futures covariance contains non-finite values."
      )
    }
    
    state_futures_covariance <-
      JOINT_STATE_TRANSFORM %*%
      raw_futures_covariance
    
    rownames(
      state_futures_covariance
    ) <-
      JOINT_STATE_NAMES
    
    if (
      !is.null(
        colnames(
          raw_futures_covariance
        )
      )
    ) {
      
      colnames(
        state_futures_covariance
      ) <-
        colnames(
          raw_futures_covariance
        )
    }
    
    state_futures_covariance
  }


# ============================================================
# 26. Estimate state-futures covariance directly
# ============================================================

estimate_state_futures_covariance <-
  function(
    state_innovations,
    futures_innovations,
    state_names =
      JOINT_STATE_NAMES,
    futures_names =
      NULL
  ) {
    
    state_innovations <-
      as.matrix(
        state_innovations
      )
    
    futures_innovations <-
      as.matrix(
        futures_innovations
      )
    
    if (
      ncol(
        state_innovations
      ) !=
      8L
    ) {
      
      stop(
        "State innovation matrix must contain 8 columns."
      )
    }
    
    if (
      nrow(
        state_innovations
      ) !=
      nrow(
        futures_innovations
      )
    ) {
      
      stop(
        "State and futures innovation rows are not aligned."
      )
    }
    
    covariance_matrix <-
      estimate_cross_covariance(
        x =
          state_innovations,
        y =
          futures_innovations
      )
    
    rownames(
      covariance_matrix
    ) <-
      state_names
    
    if (
      is.null(
        futures_names
      )
    ) {
      
      if (
        !is.null(
          colnames(
            futures_innovations
          )
        )
      ) {
        
        futures_names <-
          colnames(
            futures_innovations
          )
        
      } else {
        
        futures_names <-
          paste0(
            "Future",
            seq_len(
              ncol(
                futures_innovations
              )
            )
          )
      }
    }
    
    colnames(
      covariance_matrix
    ) <-
      futures_names
    
    covariance_matrix
  }


# ============================================================
# 27. Validate state-futures covariance matrix
# ============================================================

validate_state_futures_covariance <-
  function(
    state_futures_covariance,
    number_of_futures =
      NULL
  ) {
    
    state_futures_covariance <-
      as.matrix(
        state_futures_covariance
      )
    
    if (
      nrow(
        state_futures_covariance
      ) !=
      8L
    ) {
      
      stop(
        "State-futures covariance must contain 8 state rows."
      )
    }
    
    if (
      !is.null(
        number_of_futures
      ) &&
      ncol(
        state_futures_covariance
      ) !=
      number_of_futures
    ) {
      
      stop(
        "State-futures covariance has incorrect futures dimension."
      )
    }
    
    if (
      !all(
        is.finite(
          state_futures_covariance
        )
      )
    ) {
      
      stop(
        "State-futures covariance contains non-finite values."
      )
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 28. Stable linear solver for covariance systems
# ============================================================

solve_symmetric_system <-
  function(
    covariance_matrix,
    right_hand_side,
    tolerance =
      1e-10
  ) {
    
    covariance_matrix <-
      as.matrix(
        covariance_matrix
      )
    
    if (
      nrow(
        covariance_matrix
      ) !=
      ncol(
        covariance_matrix
      )
    ) {
      
      stop(
        "System covariance matrix must be square."
      )
    }
    
    if (
      !all(
        is.finite(
          covariance_matrix
        )
      )
    ) {
      
      stop(
        "System covariance matrix contains non-finite values."
      )
    }
    
    eigenvalues <-
      eigen(
        covariance_matrix,
        symmetric =
          TRUE,
        only.values =
          TRUE
      )[["values"]]
    
    if (
      min(
        eigenvalues
      ) <=
      tolerance
    ) {
      
      stop(
        "System covariance matrix is not positive definite."
      )
    }
    
    chol_factor <-
      chol(
        covariance_matrix
      )
    
    backsolve(
      chol_factor,
      forwardsolve(
        t(
          chol_factor
        ),
        right_hand_side
      )
    )
  }


# ============================================================
# 29. Conditional state moments given futures returns
# ============================================================

conditional_state_moments_given_futures <-
  function(
    state_mean,
    state_covariance,
    futures_mean,
    futures_covariance,
    state_futures_covariance,
    realized_futures
  ) {
    
    state_mean <-
      as.numeric(
        state_mean
      )
    
    futures_mean <-
      as.numeric(
        futures_mean
      )
    
    realized_futures <-
      as.numeric(
        realized_futures
      )
    
    state_covariance <-
      as.matrix(
        state_covariance
      )
    
    futures_covariance <-
      as.matrix(
        futures_covariance
      )
    
    state_futures_covariance <-
      as.matrix(
        state_futures_covariance
      )
    
    if (
      length(
        state_mean
      ) !=
      8L
    ) {
      
      stop(
        "State mean must contain 8 values."
      )
    }
    
    number_of_futures <-
      length(
        futures_mean
      )
    
    if (
      length(
        realized_futures
      ) !=
      number_of_futures
    ) {
      
      stop(
        "Realized futures vector has incorrect length."
      )
    }
    
    if (
      nrow(
        futures_covariance
      ) !=
      number_of_futures ||
      ncol(
        futures_covariance
      ) !=
      number_of_futures
    ) {
      
      stop(
        "Futures covariance has incorrect dimensions."
      )
    }
    
    validate_covariance_matrix(
      covariance_matrix =
        state_covariance,
      expected_names =
        JOINT_STATE_NAMES,
      require_positive_definite =
        TRUE
    )
    
    validate_state_futures_covariance(
      state_futures_covariance =
        state_futures_covariance,
      number_of_futures =
        number_of_futures
    )
    
    futures_deviation <-
      realized_futures -
      futures_mean
    
    solved_deviation <-
      solve_symmetric_system(
        covariance_matrix =
          futures_covariance,
        right_hand_side =
          futures_deviation
      )
    
    conditional_adjustment <-
      as.numeric(
        state_futures_covariance %*%
          solved_deviation
      )
    
    conditional_mean <-
      state_mean +
      conditional_adjustment
    
    solve_cross_covariance <-
      solve_symmetric_system(
        covariance_matrix =
          futures_covariance,
        right_hand_side =
          t(
            state_futures_covariance
          )
      )
    
    conditional_covariance <-
      state_covariance -
      state_futures_covariance %*%
      solve_cross_covariance
    
    conditional_covariance <-
      (
        conditional_covariance +
          t(
            conditional_covariance
          )
      ) /
      2
    
    names(
      conditional_mean
    ) <-
      JOINT_STATE_NAMES
    
    rownames(
      conditional_covariance
    ) <-
      JOINT_STATE_NAMES
    
    colnames(
      conditional_covariance
    ) <-
      JOINT_STATE_NAMES
    
    validate_covariance_matrix(
      covariance_matrix =
        conditional_covariance,
      expected_names =
        JOINT_STATE_NAMES,
      require_positive_definite =
        FALSE
    )
    
    list(
      mean =
        conditional_mean,
      covariance =
        conditional_covariance,
      adjustment =
        conditional_adjustment
    )
  }


# ============================================================
# 30. Construct residual conditional state covariance only
# ============================================================

construct_conditional_state_covariance <-
  function(
    state_covariance,
    futures_covariance,
    state_futures_covariance
  ) {
    
    state_covariance <-
      as.matrix(
        state_covariance
      )
    
    futures_covariance <-
      as.matrix(
        futures_covariance
      )
    
    state_futures_covariance <-
      as.matrix(
        state_futures_covariance
      )
    
    number_of_futures <-
      ncol(
        state_futures_covariance
      )
    
    validate_covariance_matrix(
      covariance_matrix =
        state_covariance,
      expected_names =
        JOINT_STATE_NAMES,
      require_positive_definite =
        TRUE
    )
    
    validate_state_futures_covariance(
      state_futures_covariance =
        state_futures_covariance,
      number_of_futures =
        number_of_futures
    )
    
    if (
      nrow(
        futures_covariance
      ) !=
      number_of_futures ||
      ncol(
        futures_covariance
      ) !=
      number_of_futures
    ) {
      
      stop(
        "Futures covariance dimension does not match cross covariance."
      )
    }
    
    solve_cross_covariance <-
      solve_symmetric_system(
        covariance_matrix =
          futures_covariance,
        right_hand_side =
          t(
            state_futures_covariance
          )
      )
    
    conditional_covariance <-
      state_covariance -
      state_futures_covariance %*%
      solve_cross_covariance
    
    conditional_covariance <-
      (
        conditional_covariance +
          t(
            conditional_covariance
          )
      ) /
      2
    
    rownames(
      conditional_covariance
    ) <-
      JOINT_STATE_NAMES
    
    colnames(
      conditional_covariance
    ) <-
      JOINT_STATE_NAMES
    
    validate_covariance_matrix(
      covariance_matrix =
        conditional_covariance,
      expected_names =
        JOINT_STATE_NAMES,
      require_positive_definite =
        FALSE
    )
    
    conditional_covariance
  }


# ============================================================
# 31. Validate full joint competitor-futures covariance
# ============================================================

validate_full_competitor_futures_covariance <-
  function(
    state_covariance,
    futures_covariance,
    state_futures_covariance,
    tolerance =
      1e-8
  ) {
    
    state_covariance <-
      as.matrix(
        state_covariance
      )
    
    futures_covariance <-
      as.matrix(
        futures_covariance
      )
    
    state_futures_covariance <-
      as.matrix(
        state_futures_covariance
      )
    
    number_of_futures <-
      nrow(
        futures_covariance
      )
    
    if (
      ncol(
        futures_covariance
      ) !=
      number_of_futures
    ) {
      
      stop(
        "Futures covariance matrix must be square."
      )
    }
    
    validate_covariance_matrix(
      covariance_matrix =
        state_covariance,
      expected_names =
        JOINT_STATE_NAMES,
      require_positive_definite =
        TRUE
    )
    
    validate_state_futures_covariance(
      state_futures_covariance =
        state_futures_covariance,
      number_of_futures =
        number_of_futures
    )
    
    full_covariance <-
      rbind(
        cbind(
          futures_covariance,
          t(
            state_futures_covariance
          )
        ),
        cbind(
          state_futures_covariance,
          state_covariance
        )
      )
    
    full_covariance <-
      (
        full_covariance +
          t(
            full_covariance
          )
      ) /
      2
    
    full_eigenvalues <-
      eigen(
        full_covariance,
        symmetric =
          TRUE,
        only.values =
          TRUE
      )[["values"]]
    
    if (
      any(
        full_eigenvalues <
        -tolerance
      )
    ) {
      
      stop(
        paste0(
          "Combined futures-competitor covariance is not ",
          "positive semidefinite."
        )
      )
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 32. Build joint residual correlation system object
# ============================================================

build_joint_residual_system <-
  function(
    final_innovations,
    daily_innovations,
    final_timestamp_column =
      "Timestamp",
    daily_timestamp_column =
      "Timestamp",
    final_innovation_columns =
      JOINT_FINAL_RAW_NAMES,
    daily_innovation_columns =
      JOINT_DAILY_RAW_NAMES
  ) {
    
    aligned_innovations <-
      align_joint_standardized_innovations(
        final_innovations =
          final_innovations,
        daily_innovations =
          daily_innovations,
        final_timestamp_column =
          final_timestamp_column,
        daily_timestamp_column =
          daily_timestamp_column,
        final_innovation_columns =
          final_innovation_columns,
        daily_innovation_columns =
          daily_innovation_columns
      )
    
    residual_correlation <-
      estimate_joint_residual_correlation(
        aligned_innovations =
          aligned_innovations
      )
    
    correlation_blocks <-
      extract_joint_correlation_blocks(
        residual_correlation =
          residual_correlation
      )
    
    output <-
      list(
        aligned_innovations =
          aligned_innovations,
        residual_correlation =
          residual_correlation,
        correlation_blocks =
          correlation_blocks,
        number_aligned =
          nrow(
            aligned_innovations
          )
      )
    
    validate_joint_residual_system(
      output
    )
    
    output
  }


# ============================================================
# 33. Validate joint residual system
# ============================================================

validate_joint_residual_system <-
  function(
    residual_system
  ) {
    
    required_components <-
      c(
        "aligned_innovations",
        "residual_correlation",
        "correlation_blocks",
        "number_aligned"
      )
    
    missing_components <-
      setdiff(
        required_components,
        names(
          residual_system
        )
      )
    
    if (
      length(
        missing_components
      ) >
      0
    ) {
      
      stop(
        paste0(
          "Joint residual system is missing components: ",
          paste(
            missing_components,
            collapse =
              ", "
          )
        )
      )
    }
    
    validate_standardized_innovation_panel(
      data =
        residual_system[["aligned_innovations"]],
      innovation_names =
        JOINT_RAW_NAMES
    )
    
    validate_correlation_matrix(
      correlation_matrix =
        residual_system[["residual_correlation"]],
      expected_names =
        JOINT_RAW_NAMES
    )
    
    if (
      residual_system[["number_aligned"]] !=
      nrow(
        residual_system[["aligned_innovations"]]
      )
    ) {
      
      stop(
        "Stored aligned-observation count is incorrect."
      )
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 34. Correlation summary table
# ============================================================

summarize_joint_residual_correlation <-
  function(
    residual_correlation
  ) {
    
    validate_correlation_matrix(
      correlation_matrix =
        residual_correlation,
      expected_names =
        JOINT_RAW_NAMES
    )
    
    correlation_blocks <-
      extract_joint_correlation_blocks(
        residual_correlation
      )
    
    final_daily_values <-
      as.numeric(
        correlation_blocks[["final_daily"]]
      )
    
    full_off_diagonal <-
      residual_correlation[
        lower.tri(
          residual_correlation
        )
      ]
    
    tibble(
      Metric =
        c(
          "Mean off-diagonal correlation",
          "Mean absolute off-diagonal correlation",
          "Maximum absolute off-diagonal correlation",
          "Mean final-daily correlation",
          "Mean absolute final-daily correlation",
          "Maximum absolute final-daily correlation",
          "Minimum eigenvalue"
        ),
      Value =
        c(
          mean(
            full_off_diagonal
          ),
          mean(
            abs(
              full_off_diagonal
            )
          ),
          max(
            abs(
              full_off_diagonal
            )
          ),
          mean(
            final_daily_values
          ),
          mean(
            abs(
              final_daily_values
            )
          ),
          max(
            abs(
              final_daily_values
            )
          ),
          min(
            eigen(
              residual_correlation,
              symmetric =
                TRUE,
              only.values =
                TRUE
            )[["values"]]
          )
        )
    )
  }


# ============================================================
# 35. Production state names helper
# ============================================================

get_joint_state_names <-
  function() {
    
    JOINT_STATE_NAMES
  }


get_joint_raw_names <-
  function() {
    
    JOINT_RAW_NAMES
  }


# ============================================================
# 36. Full script-level structural validation
# ============================================================

validate_joint_competition_forecasting_script <-
  function() {
    
    validate_joint_state_transform()
    
    if (
      !identical(
        rownames(
          JOINT_STATE_TRANSFORM
        ),
        JOINT_STATE_NAMES
      )
    ) {
      
      stop(
        "Joint state-transform row names are incorrect."
      )
    }
    
    if (
      !identical(
        colnames(
          JOINT_STATE_TRANSFORM
        ),
        JOINT_RAW_NAMES
      )
    ) {
      
      stop(
        "Joint state-transform column names are incorrect."
      )
    }
    
    test_correlation <-
      diag(
        8
      )
    
    rownames(
      test_correlation
    ) <-
      JOINT_RAW_NAMES
    
    colnames(
      test_correlation
    ) <-
      JOINT_RAW_NAMES
    
    validate_correlation_matrix(
      correlation_matrix =
        test_correlation,
      expected_names =
        JOINT_RAW_NAMES
    )
    
    test_final_mean <-
      rep(
        0,
        6
      )
    
    test_daily_mean <-
      rep(
        0,
        2
      )
    
    test_final_variance <-
      rep(
        1,
        6
      )
    
    test_daily_variance <-
      rep(
        1,
        2
      )
    
    test_forecast <-
      construct_joint_competition_interval(
        final_mean =
          test_final_mean,
        final_variance =
          test_final_variance,
        daily_mean =
          test_daily_mean,
        daily_variance =
          test_daily_variance,
        residual_correlation =
          test_correlation
      )
    
    validate_joint_competition_interval(
      test_forecast
    )
    
    if (
      !isTRUE(
        all.equal(
          test_forecast[["state_mean"]],
          rep(
            0,
            8
          ),
          check.attributes =
          FALSE,
          tolerance =
          1e-12
        )
      )
    ) {
      
      stop(
        "Joint state-mean transformation test failed."
      )
    }
    
    invisible(
      TRUE
    )
  }


# ============================================================
# 37. Run script-level validation
# ============================================================

validate_joint_competition_forecasting_script()


# ============================================================
# 38. Completion message
# ============================================================

cat(
  paste0(
    "joint_competition_forecasting.R loaded successfully.\n",
    "Raw competitor dimension: 8\n",
    "State dimension: 8\n",
    "Final block dimension: 6\n",
    "Daily block dimension: 2\n",
    "M8 structural validation: TRUE\n",
    "Joint covariance construction validation: TRUE\n",
    "State-futures covariance helpers available: TRUE\n"
  )
)