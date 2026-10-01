# ============================================================
# build_live_dependence_and_innovations.R
# ============================================================
#
# Stage 6 live production builder.
#
# PURPOSE
# -------
# Build the five live Stage 6 optimizer inputs:
#
# 1. state_futures_cov_competition.csv
# 2. innovation_marginals.csv
# 3. innovation_latent_correlation.csv
# 4. innovation_arithmetic_mapping.csv
# 5. innovation_system_metadata.csv
#
# LIVE POLICY
# -----------
# - Stage 4 futures moments are already frozen/validated.
# - Stage 5 competitor-state moments are already frozen/validated.
# - 44.7 holdings/residual dependence architecture is frozen.
# - 50.0 marginal families and parameters are frozen.
# - 50.1 formal copula parameters are frozen.
# - 50.15 16D coupling architecture is frozen.
# - 50.25 arithmetic-return mapping is frozen.
#
# This script does NOT reopen model selection or refit frozen
# distribution/copula parameters.
#
# The historical 44.7 final state/futures covariance system is
# converted to a standardized cross-correlation target and mapped
# onto the live 147-period schedule. The target is then rescaled
# using the LIVE Stage 4 and Stage 5 marginal standard deviations.
#
# Only cross-covariance / cross-block dependence may be shrunk
# when required for PSD/PD compatibility.
#
# Past rows t < current_t remain present in all time-indexed files
# but are economically inert.
#
# ============================================================


# ============================================================
# 1. Packages
# ============================================================

required_packages <- c(
  "readr",
  "dplyr",
  "tibble",
  "tidyr",
  "purrr"
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
      "Missing required packages: ",
      paste(
        missing_packages,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(purrr)
})


# ============================================================
# 2. Project root
# ============================================================

PROJECT_ROOT <- here::here()

cat(
  "Project root: ",
  PROJECT_ROOT,
  "\n",
  sep = ""
)


# ============================================================
# 3. Canonical paths
# ============================================================

LIVE_SCRIPT_DIRECTORY <- file.path(
  PROJECT_ROOT,
  "scripts",
  "live"
)

OPTIMIZER_INPUT_DIRECTORY <- file.path(PROJECT_ROOT, "outputs", "optimizer_inputs")

FROZEN_44_7_SYSTEM_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "processed",
  "competitors",
  "futures_dependence",
  "competitor_futures_production_system.rds"
)

MARGINAL_SYSTEM_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "processed",
  "innovations",
  "production",
  "innovation_distribution_system.rds"
)

DEPENDENCE_SYSTEM_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "processed",
  "innovations",
  "production",
  "innovation_dependence_system.rds"
)

JOINT_COUPLING_SYSTEM_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "processed",
  "innovations",
  "production",
  "joint_coupling",
  "joint_innovation_coupling_system.rds"
)

ARITHMETIC_MAPPING_SYSTEM_PATH <- file.path(
  PROJECT_ROOT,
  "data",
  "processed",
  "innovations",
  "production",
  "arithmetic_return_mapping",
  "arithmetic_return_mapping_system.rds"
)

FROZEN_PYTHON_PRODUCTION_DIRECTORY <- file.path(
  PROJECT_ROOT,
  "data",
  "processed",
  "innovations",
  "python_production"
)

FROZEN_MARGINAL_CSV_PATH <- file.path(
  FROZEN_PYTHON_PRODUCTION_DIRECTORY,
  "innovation_marginals.csv"
)

FROZEN_MAPPING_CSV_PATH <- file.path(
  FROZEN_PYTHON_PRODUCTION_DIRECTORY,
  "innovation_arithmetic_mapping.csv"
)

FROZEN_METADATA_CSV_PATH <- file.path(
  FROZEN_PYTHON_PRODUCTION_DIRECTORY,
  "innovation_system_metadata.csv"
)

LIVE_STATE_MEAN_PATH <- file.path(
  OPTIMIZER_INPUT_DIRECTORY,
  "state_mean_competition.csv"
)

LIVE_STATE_COV_PATH <- file.path(
  OPTIMIZER_INPUT_DIRECTORY,
  "state_cov_competition.csv"
)

STATE_FUTURES_OUTPUT_PATH <- file.path(
  OPTIMIZER_INPUT_DIRECTORY,
  "state_futures_cov_competition.csv"
)

MARGINAL_OUTPUT_PATH <- file.path(
  OPTIMIZER_INPUT_DIRECTORY,
  "innovation_marginals.csv"
)

LATENT_CORRELATION_OUTPUT_PATH <- file.path(
  OPTIMIZER_INPUT_DIRECTORY,
  "innovation_latent_correlation.csv"
)

ARITHMETIC_MAPPING_OUTPUT_PATH <- file.path(
  OPTIMIZER_INPUT_DIRECTORY,
  "innovation_arithmetic_mapping.csv"
)

METADATA_OUTPUT_PATH <- file.path(
  OPTIMIZER_INPUT_DIRECTORY,
  "innovation_system_metadata.csv"
)

STAGE6_AUDIT_DIRECTORY <- file.path(
  PROJECT_ROOT,
  "data",
  "processed",
  "live",
  "stage6"
)

dir.create(
  OPTIMIZER_INPUT_DIRECTORY,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  STAGE6_AUDIT_DIRECTORY,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 4. Validate required inputs
# ============================================================

required_input_paths <- c(
  FROZEN_44_7_SYSTEM_PATH,
  MARGINAL_SYSTEM_PATH,
  DEPENDENCE_SYSTEM_PATH,
  JOINT_COUPLING_SYSTEM_PATH,
  ARITHMETIC_MAPPING_SYSTEM_PATH,
  FROZEN_MARGINAL_CSV_PATH,
  FROZEN_MAPPING_CSV_PATH,
  FROZEN_METADATA_CSV_PATH,
  LIVE_STATE_MEAN_PATH,
  LIVE_STATE_COV_PATH
)

missing_input_paths <- required_input_paths[
  !file.exists(
    required_input_paths
  )
]

if (length(missing_input_paths) > 0L) {
  stop(
    paste0(
      "Missing required Stage 6 input files:\n",
      paste(
        missing_input_paths,
        collapse = "\n"
      )
    ),
    call. = FALSE
  )
}

cat(
  "All required Stage 6 input files exist: TRUE\n"
)


# ============================================================
# 5. Require validated Stage 2, Stage 4, and Stage 5 objects
# ============================================================

required_live_objects <- c(
  "live_competition_clock",
  "live_futures_forecast_system",
  "LIVE_COMPETITION_CLOCK_VALIDATED",
  "LIVE_FUTURES_FORECASTS_VALIDATED",
  "LIVE_COMPETITOR_FORECASTS_VALIDATED"
)

missing_live_objects <- required_live_objects[
  !vapply(
    required_live_objects,
    exists,
    logical(1),
    envir = .GlobalEnv,
    inherits = TRUE
  )
]

if (length(missing_live_objects) > 0L) {
  stop(
    paste0(
      "Stage 6 requires validated Stage 2, Stage 4, and Stage 5 ",
      "objects in the current R session.\nMissing objects:\n",
      paste(
        missing_live_objects,
        collapse = "\n"
      ),
      "\n\nRun the validated live clock, futures forecast, and ",
      "competitor forecast scripts first."
    ),
    call. = FALSE
  )
}

stopifnot(
  isTRUE(
    LIVE_COMPETITION_CLOCK_VALIDATED
  )
)

stopifnot(
  isTRUE(
    LIVE_FUTURES_FORECASTS_VALIDATED
  )
)

stopifnot(
  isTRUE(
    LIVE_COMPETITOR_FORECASTS_VALIDATED
  )
)

T_LIVE <- as.integer(
  live_competition_clock[["T"]]
)

CURRENT_T <- as.integer(
  live_competition_clock[["current_t"]]
)

if (T_LIVE != 147L) {
  stop(
    paste0(
      "Stage 6 expected T = 147 but received T = ",
      T_LIVE,
      "."
    ),
    call. = FALSE
  )
}

if (
  CURRENT_T < 0L ||
  CURRENT_T >= T_LIVE
) {
  stop(
    "current_t is outside 0:(T-1).",
    call. = FALSE
  )
}

cat(
  "Live clock validated. T=",
  T_LIVE,
  ", current_t=",
  CURRENT_T,
  ", remaining=",
  T_LIVE - CURRENT_T,
  "\n",
  sep = ""
)


# ============================================================
# 6. Canonical orders
# ============================================================

FUTURES_ORDER <- c(
  "CL",
  "GC",
  "SR3",
  "ZN",
  "KE",
  "HO",
  "ZW",
  "NQ"
)

OPTIMIZER_FUTURE_ORDER <- paste0(
  "Future_",
  seq_len(
    length(
      FUTURES_ORDER
    )
  )
)

FINAL_STATE_ORDER <- c(
  "Delta_Final_X1",
  "Delta_Final_D12",
  "Delta_Final_D23",
  "Delta_Final_D34",
  "Delta_Final_D4_15",
  "Delta_Final_D15_16"
)

DAILY_STATE_ORDER <- c(
  "Delta_Daily_H1",
  "Delta_Daily_D12"
)

STATE_ORDER <- c(
  FINAL_STATE_ORDER,
  DAILY_STATE_ORDER
)

CANONICAL_COMPONENT_ORDER <- c(
  FUTURES_ORDER,
  FINAL_STATE_ORDER,
  DAILY_STATE_ORDER
)

stopifnot(
  length(
    FUTURES_ORDER
  ) == 8L
)

stopifnot(
  length(
    STATE_ORDER
  ) == 8L
)

stopifnot(
  length(
    CANONICAL_COMPONENT_ORDER
  ) == 16L
)


# ============================================================
# 7. Numerical constants
# ============================================================

SYMMETRY_TOLERANCE <- 1e-8

PSD_TOLERANCE <- 1e-8

PD_TARGET_EIGENVALUE <- 1e-8

ZERO_VARIANCE_TOLERANCE <- 1e-14

BINARY_SEARCH_ITERATIONS <- 80L


# ============================================================
# 8. Matrix helpers
# ============================================================

symmetrize_matrix <- function(x) {
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


minimum_eigenvalue <- function(x) {
  x <- symmetrize_matrix(
    x
  )
  
  min(
    eigen(
      x,
      symmetric = TRUE,
      only.values = TRUE
    )[["values"]]
  )
}


covariance_to_correlation_safe <- function(
    covariance_matrix,
    zero_variance_tolerance = ZERO_VARIANCE_TOLERANCE
) {
  covariance_matrix <- symmetrize_matrix(
    covariance_matrix
  )
  
  variances <- diag(
    covariance_matrix
  )
  
  if (
    any(
      !is.finite(
        variances
      )
    ) ||
    any(
      variances < -PSD_TOLERANCE
    )
  ) {
    stop(
      "Invalid covariance diagonal encountered.",
      call. = FALSE
    )
  }
  
  variances <- pmax(
    variances,
    0
  )
  
  standard_deviations <- sqrt(
    variances
  )
  
  dimension_count <- nrow(
    covariance_matrix
  )
  
  correlation_matrix <- diag(
    1,
    dimension_count
  )
  
  for (i in seq_len(dimension_count)) {
    for (j in seq_len(dimension_count)) {
      if (i == j) {
        correlation_matrix[i, j] <- 1
      } else if (
        standard_deviations[i] >
        zero_variance_tolerance &&
        standard_deviations[j] >
        zero_variance_tolerance
      ) {
        correlation_matrix[i, j] <- (
          covariance_matrix[i, j] /
            (
              standard_deviations[i] *
                standard_deviations[j]
            )
        )
      } else {
        correlation_matrix[i, j] <- 0
      }
    }
  }
  
  correlation_matrix <- symmetrize_matrix(
    correlation_matrix
  )
  
  diag(
    correlation_matrix
  ) <- 1
  
  correlation_matrix <- pmax(
    pmin(
      correlation_matrix,
      1
    ),
    -1
  )
  
  diag(
    correlation_matrix
  ) <- 1
  
  correlation_matrix
}


cross_covariance_to_correlation <- function(
    cross_covariance,
    row_covariance,
    column_covariance,
    zero_variance_tolerance = ZERO_VARIANCE_TOLERANCE
) {
  cross_covariance <- as.matrix(
    cross_covariance
  )
  
  row_sd <- sqrt(
    pmax(
      diag(
        row_covariance
      ),
      0
    )
  )
  
  column_sd <- sqrt(
    pmax(
      diag(
        column_covariance
      ),
      0
    )
  )
  
  result <- matrix(
    0,
    nrow = nrow(
      cross_covariance
    ),
    ncol = ncol(
      cross_covariance
    )
  )
  
  for (i in seq_len(nrow(result))) {
    for (j in seq_len(ncol(result))) {
      if (
        row_sd[i] >
        zero_variance_tolerance &&
        column_sd[j] >
        zero_variance_tolerance
      ) {
        result[i, j] <- (
          cross_covariance[i, j] /
            (
              row_sd[i] *
                column_sd[j]
            )
        )
      } else {
        result[i, j] <- 0
      }
    }
  }
  
  pmax(
    pmin(
      result,
      1
    ),
    -1
  )
}


correlation_to_cross_covariance <- function(
    cross_correlation,
    row_covariance,
    column_covariance
) {
  row_sd <- sqrt(
    pmax(
      diag(
        row_covariance
      ),
      0
    )
  )
  
  column_sd <- sqrt(
    pmax(
      diag(
        column_covariance
      ),
      0
    )
  )
  
  diag(
    row_sd,
    nrow = length(
      row_sd
    )
  ) %*%
    cross_correlation %*%
    diag(
      column_sd,
      nrow = length(
        column_sd
      )
    )
}


assemble_joint_covariance <- function(
    state_covariance,
    futures_covariance,
    state_futures_covariance
) {
  rbind(
    cbind(
      state_covariance,
      state_futures_covariance
    ),
    cbind(
      t(
        state_futures_covariance
      ),
      futures_covariance
    )
  )
}


shrink_cross_covariance_for_psd <- function(
    state_covariance,
    futures_covariance,
    proposed_cross_covariance,
    tolerance = PSD_TOLERANCE
) {
  zero_cross <- matrix(
    0,
    nrow = nrow(
      proposed_cross_covariance
    ),
    ncol = ncol(
      proposed_cross_covariance
    )
  )
  
  zero_joint <- assemble_joint_covariance(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    state_futures_covariance = zero_cross
  )
  
  zero_minimum_eigenvalue <- minimum_eigenvalue(
    zero_joint
  )
  
  if (
    zero_minimum_eigenvalue <
    -tolerance
  ) {
    stop(
      paste0(
        "Marginal covariance matrices are not jointly PSD even ",
        "with zero cross-covariance. Minimum eigenvalue = ",
        format(
          zero_minimum_eigenvalue,
          digits = 17
        )
      ),
      call. = FALSE
    )
  }
  
  full_joint <- assemble_joint_covariance(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    state_futures_covariance = proposed_cross_covariance
  )
  
  full_minimum_eigenvalue <- minimum_eigenvalue(
    full_joint
  )
  
  if (
    full_minimum_eigenvalue >=
    -tolerance
  ) {
    return(
      list(
        cross_covariance = proposed_cross_covariance,
        lambda = 1,
        minimum_eigenvalue = full_minimum_eigenvalue
      )
    )
  }
  
  lower <- 0
  upper <- 1
  
  for (iteration in seq_len(BINARY_SEARCH_ITERATIONS)) {
    midpoint <- (
      lower +
        upper
    ) /
      2
    
    candidate_cross <- (
      midpoint *
        proposed_cross_covariance
    )
    
    candidate_joint <- assemble_joint_covariance(
      state_covariance = state_covariance,
      futures_covariance = futures_covariance,
      state_futures_covariance = candidate_cross
    )
    
    candidate_minimum_eigenvalue <- minimum_eigenvalue(
      candidate_joint
    )
    
    if (
      candidate_minimum_eigenvalue >=
      -tolerance
    ) {
      lower <- midpoint
    } else {
      upper <- midpoint
    }
  }
  
  final_cross <- (
    lower *
      proposed_cross_covariance
  )
  
  final_joint <- assemble_joint_covariance(
    state_covariance = state_covariance,
    futures_covariance = futures_covariance,
    state_futures_covariance = final_cross
  )
  
  list(
    cross_covariance = final_cross,
    lambda = lower,
    minimum_eigenvalue = minimum_eigenvalue(
      final_joint
    )
  )
}


assemble_latent_correlation <- function(
    futures_formal_correlation,
    final_formal_correlation,
    daily_correlation,
    futures_final_cross,
    futures_daily_cross,
    final_daily_cross
) {
  result <- diag(
    1,
    16L
  )
  
  result[1:8, 1:8] <- futures_formal_correlation
  
  result[9:14, 9:14] <- final_formal_correlation
  
  result[15:16, 15:16] <- daily_correlation
  
  result[1:8, 9:14] <- futures_final_cross
  
  result[9:14, 1:8] <- t(
    futures_final_cross
  )
  
  result[1:8, 15:16] <- futures_daily_cross
  
  result[15:16, 1:8] <- t(
    futures_daily_cross
  )
  
  result[9:14, 15:16] <- final_daily_cross
  
  result[15:16, 9:14] <- t(
    final_daily_cross
  )
  
  result <- symmetrize_matrix(
    result
  )
  
  diag(
    result
  ) <- 1
  
  result
}


shrink_latent_cross_blocks_for_pd <- function(
    futures_formal_correlation,
    final_formal_correlation,
    daily_correlation,
    futures_final_cross,
    futures_daily_cross,
    final_daily_cross,
    target_minimum_eigenvalue = PD_TARGET_EIGENVALUE
) {
  build_candidate <- function(lambda) {
    assemble_latent_correlation(
      futures_formal_correlation =
        futures_formal_correlation,
      final_formal_correlation =
        final_formal_correlation,
      daily_correlation =
        daily_correlation,
      futures_final_cross =
        lambda *
        futures_final_cross,
      futures_daily_cross =
        lambda *
        futures_daily_cross,
      final_daily_cross =
        lambda *
        final_daily_cross
    )
  }
  
  full_candidate <- build_candidate(
    1
  )
  
  full_minimum_eigenvalue <- minimum_eigenvalue(
    full_candidate
  )
  
  if (
    full_minimum_eigenvalue >=
    target_minimum_eigenvalue
  ) {
    return(
      list(
        correlation = full_candidate,
        lambda = 1,
        minimum_eigenvalue =
          full_minimum_eigenvalue
      )
    )
  }
  
  zero_candidate <- build_candidate(
    0
  )
  
  zero_minimum_eigenvalue <- minimum_eigenvalue(
    zero_candidate
  )
  
  if (
    zero_minimum_eigenvalue <
    target_minimum_eigenvalue
  ) {
    stop(
      paste0(
        "Frozen within-block latent dependence is not sufficiently ",
        "positive definite even with zero cross-block dependence. ",
        "Minimum eigenvalue = ",
        format(
          zero_minimum_eigenvalue,
          digits = 17
        )
      ),
      call. = FALSE
    )
  }
  
  lower <- 0
  upper <- 1
  
  for (iteration in seq_len(BINARY_SEARCH_ITERATIONS)) {
    midpoint <- (
      lower +
        upper
    ) /
      2
    
    candidate <- build_candidate(
      midpoint
    )
    
    candidate_minimum_eigenvalue <- minimum_eigenvalue(
      candidate
    )
    
    if (
      candidate_minimum_eigenvalue >=
      target_minimum_eigenvalue
    ) {
      lower <- midpoint
    } else {
      upper <- midpoint
    }
  }
  
  final_candidate <- build_candidate(
    lower
  )
  
  list(
    correlation = final_candidate,
    lambda = lower,
    minimum_eigenvalue = minimum_eigenvalue(
      final_candidate
    )
  )
}


# ============================================================
# 9. Load frozen production systems
# ============================================================

frozen_44_7_system <- readRDS(
  FROZEN_44_7_SYSTEM_PATH
)

innovation_distribution_system <- readRDS(
  MARGINAL_SYSTEM_PATH
)

innovation_dependence_system <- readRDS(
  DEPENDENCE_SYSTEM_PATH
)

joint_coupling_system <- readRDS(
  JOINT_COUPLING_SYSTEM_PATH
)

arithmetic_return_mapping_system <- readRDS(
  ARITHMETIC_MAPPING_SYSTEM_PATH
)

cat(
  "Frozen 44.7 and 50.x systems loaded: TRUE\n"
)


# ============================================================
# 10. Validate frozen 44.7 system
# ============================================================

required_44_7_members <- c(
  "ProductionSchedule",
  "ProductionHorizon",
  "RawProcessNames",
  "StateNames",
  "FuturesSymbols",
  "OptimizerFutureNames",
  "ResidualStandardizedCrossCovariance",
  "RawExposureBetaArray",
  "StateExposureBetaArray",
  "HoldingsStateLogCrossCovarianceArray",
  "ResidualStateLogCrossCovarianceArray",
  "TotalStateLogCrossCovarianceArray",
  "FinalStateFuturesCovarianceArray",
  "ProductionPSDDiagnostics",
  "StateFuturesCovarianceCSV",
  "ProductionArchitecture"
)

missing_44_7_members <- setdiff(
  required_44_7_members,
  names(
    frozen_44_7_system
  )
)

if (length(missing_44_7_members) > 0L) {
  stop(
    paste0(
      "Frozen 44.7 system is missing members: ",
      paste(
        missing_44_7_members,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

stopifnot(
  identical(
    as.character(
      frozen_44_7_system[["StateNames"]]
    ),
    STATE_ORDER
  )
)

stopifnot(
  identical(
    as.character(
      frozen_44_7_system[["FuturesSymbols"]]
    ),
    FUTURES_ORDER
  )
)

stopifnot(
  identical(
    as.character(
      frozen_44_7_system[["OptimizerFutureNames"]]
    ),
    OPTIMIZER_FUTURE_ORDER
  )
)

frozen_44_7_cross_array <- frozen_44_7_system[["FinalStateFuturesCovarianceArray"]]

if (
  length(
    dim(
      frozen_44_7_cross_array
    )
  ) != 3L ||
  dim(
    frozen_44_7_cross_array
  )[2] != 8L ||
  dim(
    frozen_44_7_cross_array
  )[3] != 8L
) {
  stop(
    "Frozen 44.7 cross-covariance array has unexpected dimensions.",
    call. = FALSE
  )
}

T_FROZEN_44_7 <- dim(
  frozen_44_7_cross_array
)[1]

cat(
  "Frozen 44.7 horizon: ",
  T_FROZEN_44_7,
  "\n",
  sep = ""
)


# ============================================================
# 11. Validate frozen 50.x systems
# ============================================================

stopifnot(
  identical(
    as.character(
      innovation_distribution_system[["Version"]]
    ),
    "50.0"
  )
)

stopifnot(
  identical(
    as.character(
      innovation_dependence_system[["Version"]]
    ),
    "50.1"
  )
)

stopifnot(
  identical(
    as.character(
      joint_coupling_system[["Version"]]
    ),
    "50.15"
  )
)

stopifnot(
  identical(
    as.character(
      arithmetic_return_mapping_system[["Version"]]
    ),
    "50.25"
  )
)

stopifnot(
  identical(
    as.character(
      joint_coupling_system[["CanonicalComponentOrder"]]
    ),
    CANONICAL_COMPONENT_ORDER
  )
)

stopifnot(
  identical(
    as.character(
      joint_coupling_system[["FuturesOrder"]]
    ),
    FUTURES_ORDER
  )
)

stopifnot(
  identical(
    as.character(
      joint_coupling_system[["FinalStateOrder"]]
    ),
    FINAL_STATE_ORDER
  )
)

stopifnot(
  identical(
    as.character(
      joint_coupling_system[["DailyStateOrder"]]
    ),
    DAILY_STATE_ORDER
  )
)

stopifnot(
  identical(
    as.character(
      joint_coupling_system[["StateOrder"]]
    ),
    STATE_ORDER
  )
)

stopifnot(
  isTRUE(
    joint_coupling_system[["FuturesCopulaFinal"]]
  )
)

stopifnot(
  isTRUE(
    joint_coupling_system[["FinalStateCopulaFinal"]]
  )
)

stopifnot(
  isTRUE(
    joint_coupling_system[["DailyStateGaussianDependencePreserved"]]
  )
)

stopifnot(
  isTRUE(
    joint_coupling_system[["CrossSystemConservativeDependencePreserved"]]
  )
)

stopifnot(
  !isTRUE(
    joint_coupling_system[["SparseBlocksIndependenceAssumed"]]
  )
)

stopifnot(
  isTRUE(
    joint_coupling_system[["Joint16DLatentCouplingFinal"]]
  )
)

stopifnot(
  isTRUE(
    joint_coupling_system[["Joint16DSimulatorContractFinal"]]
  )
)

stopifnot(
  isTRUE(
    arithmetic_return_mapping_system[["ArithmeticReturnMappingFinal"]]
  )
)

stopifnot(
  isTRUE(
    arithmetic_return_mapping_system[["ArithmeticMarginalMeanTargetsPreserved"]]
  )
)

stopifnot(
  isTRUE(
    arithmetic_return_mapping_system[["ArithmeticMarginalVarianceTargetsPreserved"]]
  )
)

stopifnot(
  isTRUE(
    arithmetic_return_mapping_system[["CopulaPreservedByMonotonicMapping"]]
  )
)

stopifnot(
  isTRUE(
    arithmetic_return_mapping_system[["ReturnFloorStrictlyAboveMinusOne"]]
  )
)

FUTURES_FORMAL_CORRELATION <- as.matrix(
  joint_coupling_system[["FuturesFormalCorrelation"]]
)

FINAL_STATE_FORMAL_CORRELATION <- as.matrix(
  joint_coupling_system[["FinalStateFormalCorrelation"]]
)

FINAL_STATE_T_DF <- as.numeric(
  joint_coupling_system[["FinalStateStudentTDF"]]
)

if (
  !identical(
    dim(
      FUTURES_FORMAL_CORRELATION
    ),
    c(
      8L,
      8L
    )
  )
) {
  stop(
    "Frozen futures formal correlation is not 8x8.",
    call. = FALSE
  )
}

if (
  !identical(
    dim(
      FINAL_STATE_FORMAL_CORRELATION
    ),
    c(
      6L,
      6L
    )
  )
) {
  stop(
    "Frozen final-state formal correlation is not 6x6.",
    call. = FALSE
  )
}

if (
  !is.finite(
    FINAL_STATE_T_DF
  ) ||
  FINAL_STATE_T_DF <= 0
) {
  stop(
    "Frozen FinalState Student-t degrees of freedom is invalid.",
    call. = FALSE
  )
}

cat(
  "Frozen 50.x architecture validation: TRUE\n"
)


# ============================================================
# 12. Load live Stage 4 and Stage 5 CSVs
# ============================================================

live_mu <-
  live_futures_forecast_system[["MuCSV"]]

live_sigma <-
  live_futures_forecast_system[["SigmaCSV"]]

live_state_mean <- readr::read_csv(
  LIVE_STATE_MEAN_PATH,
  show_col_types = FALSE
)

live_state_cov <- readr::read_csv(
  LIVE_STATE_COV_PATH,
  show_col_types = FALSE
)

required_mu_columns <- c(
  "t_original",
  OPTIMIZER_FUTURE_ORDER
)

if (
  !all(
    required_mu_columns %in%
    names(
      live_mu
    )
  )
) {
  stop(
    "mu.csv does not have the expected Stage 4 schema.",
    call. = FALSE
  )
}

required_sigma_columns <- c(
  "t_original",
  "future_1",
  "future_2",
  "covariance"
)

if (
  !all(
    required_sigma_columns %in%
    names(
      live_sigma
    )
  )
) {
  stop(
    "Sigma.csv does not have the expected Stage 4 schema.",
    call. = FALSE
  )
}

required_state_cov_columns <- c(
  "t_original",
  "state_1",
  "state_2",
  "covariance"
)

if (
  !all(
    required_state_cov_columns %in%
    names(
      live_state_cov
    )
  )
) {
  stop(
    "state_cov_competition.csv does not have the expected Stage 5 schema.",
    call. = FALSE
  )
}

if (
  nrow(
    live_mu
  ) != T_LIVE
) {
  stop(
    "mu.csv does not contain exactly 147 rows.",
    call. = FALSE
  )
}

if (
  nrow(
    live_sigma
  ) !=
  T_LIVE *
  8L *
  8L
) {
  stop(
    "Sigma.csv does not contain exactly 147*8*8 rows.",
    call. = FALSE
  )
}

if (
  nrow(
    live_state_cov
  ) !=
  T_LIVE *
  8L *
  8L
) {
  stop(
    "state_cov_competition.csv does not contain exactly 147*8*8 rows.",
    call. = FALSE
  )
}

cat(
  "Live Stage 4/5 CSV dimensions validated: TRUE\n"
)


# ============================================================
# 13. Long covariance -> arrays
# ============================================================

build_covariance_array <- function(
    covariance_data,
    time_column,
    row_column,
    column_column,
    value_column,
    row_names,
    column_names,
    T_expected
) {
  result <- array(
    0,
    dim = c(
      T_expected,
      length(row_names),
      length(column_names)
    ),
    dimnames = list(
      as.character(0:(T_expected - 1L)),
      row_names,
      column_names
    )
  )
  
  for (t_original in 0:(T_expected - 1L)) {
    one_time <- covariance_data[
      covariance_data[[time_column]] == t_original,
      ,
      drop = FALSE
    ]
    
    if (
      nrow(one_time) !=
      length(row_names) *
      length(column_names)
    ) {
      stop(
        paste0(
          "Unexpected covariance row count at t=",
          t_original,
          "."
        ),
        call. = FALSE
      )
    }
    
    for (row_name in row_names) {
      for (column_name in column_names) {
        one_value <- one_time[
          one_time[[row_column]] == row_name &
            one_time[[column_column]] == column_name,
          value_column,
          drop = TRUE
        ]
        
        if (length(one_value) != 1L) {
          stop(
            paste0(
              "Could not uniquely recover covariance entry at t=",
              t_original,
              ", row=",
              row_name,
              ", column=",
              column_name,
              "."
            ),
            call. = FALSE
          )
        }
        
        result[
          t_original + 1L,
          row_name,
          column_name
        ] <- as.numeric(one_value)
      }
    }
  }
  
  result
}


LIVE_FUTURES_COVARIANCE_ARRAY <- build_covariance_array(
  covariance_data = live_sigma,
  time_column = "t_original",
  row_column = "future_1",
  column_column = "future_2",
  value_column = "covariance",
  row_names = OPTIMIZER_FUTURE_ORDER,
  column_names = OPTIMIZER_FUTURE_ORDER,
  T_expected = T_LIVE
)

LIVE_STATE_COVARIANCE_ARRAY <- build_covariance_array(
  covariance_data = live_state_cov,
  time_column = "t_original",
  row_column = "state_1",
  column_column = "state_2",
  value_column = "covariance",
  row_names = STATE_ORDER,
  column_names = STATE_ORDER,
  T_expected = T_LIVE
)

future_live_slice <- LIVE_FUTURES_COVARIANCE_ARRAY[
  (CURRENT_T + 1L):T_LIVE,
  ,
  ,
  drop = FALSE
]

if (any(!is.finite(future_live_slice))) {
  stop(
    "Live futures covariance contains non-finite values at or after current_t.",
    call. = FALSE
  )
}

for (time_index in (CURRENT_T + 1L):T_LIVE) {
  futures_covariance <- LIVE_FUTURES_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  state_covariance <- LIVE_STATE_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  if (
    max(
      abs(
        futures_covariance -
        t(
          futures_covariance
        )
      )
    ) >
    SYMMETRY_TOLERANCE
  ) {
    stop(
      paste0(
        "Live futures covariance is not symmetric at t=",
        time_index - 1L,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    max(
      abs(
        state_covariance -
        t(
          state_covariance
        )
      )
    ) >
    SYMMETRY_TOLERANCE
  ) {
    stop(
      paste0(
        "Live state covariance is not symmetric at t=",
        time_index - 1L,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    minimum_eigenvalue(
      futures_covariance
    ) <
    -PSD_TOLERANCE
  ) {
    stop(
      paste0(
        "Live futures covariance is not PSD at t=",
        time_index - 1L,
        "."
      ),
      call. = FALSE
    )
  }
  
  if (
    minimum_eigenvalue(
      state_covariance
    ) <
    -PSD_TOLERANCE
  ) {
    stop(
      paste0(
        "Live state covariance is not PSD at t=",
        time_index - 1L,
        "."
      ),
      call. = FALSE
    )
  }
}

cat(
  "Live marginal covariance arrays validated: TRUE\n"
)


# ============================================================
# 14. Recover frozen 44.7 marginal covariance arrays
# ============================================================
#
# The frozen 44.7 final cross-covariance itself is not enough to
# normalize into a correlation target unless the corresponding
# frozen state/futures marginal covariance schedules are available.
#
# 50.15 already stores exactly the normalized legacy state/futures
# correlation array produced from the finalized production
# covariance systems. That is therefore the canonical frozen
# standardized target used here.
#
# ============================================================

required_joint_members <- c(
  "LegacyStateFuturesCorrelationArray",
  "LegacyStateCorrelationArray",
  "FuturesFormalCorrelation",
  "FinalStateFormalCorrelation",
  "FinalStateStudentTDF",
  "ProductionTimes",
  "ProductionTimeCount"
)

missing_joint_members <- setdiff(
  required_joint_members,
  names(
    joint_coupling_system
  )
)

if (length(missing_joint_members) > 0L) {
  stop(
    paste0(
      "Frozen 50.15 coupling system is missing members: ",
      paste(
        missing_joint_members,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

FROZEN_STATE_FUTURES_CORRELATION_ARRAY <- joint_coupling_system[["LegacyStateFuturesCorrelationArray"]]

FROZEN_STATE_CORRELATION_ARRAY <- joint_coupling_system[["LegacyStateCorrelationArray"]]

T_FROZEN_COUPLING <- as.integer(
  joint_coupling_system[["ProductionTimeCount"]]
)

if (
  !identical(
    dim(
      FROZEN_STATE_FUTURES_CORRELATION_ARRAY
    ),
    c(
      T_FROZEN_COUPLING,
      8L,
      8L
    )
  )
) {
  stop(
    "Frozen state/futures correlation array has unexpected dimensions.",
    call. = FALSE
  )
}

if (
  !identical(
    dim(
      FROZEN_STATE_CORRELATION_ARRAY
    ),
    c(
      T_FROZEN_COUPLING,
      8L,
      8L
    )
  )
) {
  stop(
    "Frozen state correlation array has unexpected dimensions.",
    call. = FALSE
  )
}

cat(
  "Frozen normalized legacy dependence targets recovered: TRUE\n"
)


# ============================================================
# 15. Map frozen production horizon onto live 147-period horizon
# ============================================================
#
# Both systems begin at competition t=0.
#
# The old frozen production system had 151 periods and the live
# corrected competition clock has 147. The common production
# architecture is indexed by elapsed competition progress.
#
# We therefore map each live t to the nearest frozen production
# progress index. This preserves:
#
# - t=0 -> frozen t=0
# - final live t -> final frozen t
# - monotonic horizon progression
#
# No frozen dependence parameter is re-estimated.
#
# ============================================================

if (T_LIVE == 1L) {
  frozen_index_for_live <- 1L
} else {
  frozen_index_for_live <- as.integer(
    round(
      seq(
        from = 0,
        to = T_FROZEN_COUPLING - 1L,
        length.out = T_LIVE
      )
    )
  ) +
    1L
}

if (
  length(
    frozen_index_for_live
  ) != T_LIVE
) {
  stop(
    "Frozen/live horizon mapping has wrong length.",
    call. = FALSE
  )
}

if (
  any(
    frozen_index_for_live < 1L
  ) ||
  any(
    frozen_index_for_live >
    T_FROZEN_COUPLING
  )
) {
  stop(
    "Frozen/live horizon mapping is outside valid bounds.",
    call. = FALSE
  )
}

if (
  is.unsorted(
    frozen_index_for_live
  )
) {
  stop(
    "Frozen/live horizon mapping is not monotonic.",
    call. = FALSE
  )
}

horizon_mapping_audit <- tibble(
  t_original = 0:(T_LIVE - 1L),
  frozen_time_index =
    frozen_index_for_live - 1L
)

readr::write_csv(
  horizon_mapping_audit,
  file.path(
    STAGE6_AUDIT_DIRECTORY,
    "stage6_frozen_horizon_mapping.csv"
  )
)

cat(
  "Frozen/live dependence horizon mapping constructed: TRUE\n"
)


# ============================================================
# 16. Build live state/futures covariance
# ============================================================

LIVE_STATE_FUTURES_COVARIANCE_ARRAY <- array(
  0,
  dim = c(
    T_LIVE,
    8L,
    8L
  ),
  dimnames = list(
    as.character(
      0:(T_LIVE - 1L)
    ),
    STATE_ORDER,
    OPTIMIZER_FUTURE_ORDER
  )
)

state_futures_psd_diagnostics <- vector(
  "list",
  T_LIVE
)

for (time_index in (CURRENT_T + 1L):T_LIVE) {
  t_original <- time_index - 1L
  
  state_covariance <- LIVE_STATE_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  futures_covariance <- LIVE_FUTURES_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  frozen_index <- frozen_index_for_live[
    time_index
  ]
  
  frozen_cross_correlation <- FROZEN_STATE_FUTURES_CORRELATION_ARRAY[
    frozen_index,
    ,
    ,
    drop = TRUE
  ]
  
  frozen_cross_correlation[
    !is.finite(
      frozen_cross_correlation
    )
  ] <- 0
  
  frozen_cross_correlation <- pmax(
    pmin(
      frozen_cross_correlation,
      1
    ),
    -1
  )
  
  proposed_cross_covariance <- correlation_to_cross_covariance(
    cross_correlation =
      frozen_cross_correlation,
    row_covariance =
      state_covariance,
    column_covariance =
      futures_covariance
  )
  
  # Exact state-covariance null directions imply zero economic
  # covariance for those state coordinates.
  state_variances <- diag(
    state_covariance
  )
  
  inactive_state_rows <- which(
    state_variances <=
      ZERO_VARIANCE_TOLERANCE
  )
  
  if (
    length(
      inactive_state_rows
    ) > 0L
  ) {
    proposed_cross_covariance[
      inactive_state_rows,
      
    ] <- 0
  }
  
  psd_result <- shrink_cross_covariance_for_psd(
    state_covariance =
      state_covariance,
    futures_covariance =
      futures_covariance,
    proposed_cross_covariance =
      proposed_cross_covariance
  )
  
  LIVE_STATE_FUTURES_COVARIANCE_ARRAY[
    time_index,
    ,
    
  ] <- psd_result[["cross_covariance"]]
  
  state_futures_psd_diagnostics[[time_index]] <- tibble(
    t_original = t_original,
    frozen_time_index =
      frozen_index - 1L,
    cross_covariance_lambda =
      psd_result[["lambda"]],
    minimum_joint_covariance_eigenvalue =
      psd_result[["minimum_eigenvalue"]]
  )
}

state_futures_psd_diagnostics <- bind_rows(
  state_futures_psd_diagnostics
)

cat(
  "Live state/futures covariance array constructed: TRUE\n"
)


# ============================================================
# 17. Validate live state/futures covariance
# ============================================================

for (time_index in (CURRENT_T + 1L):T_LIVE) {
  state_covariance <- LIVE_STATE_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  futures_covariance <- LIVE_FUTURES_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  cross_covariance <- LIVE_STATE_FUTURES_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  if (
    any(
      !is.finite(
        cross_covariance
      )
    )
  ) {
    stop(
      paste0(
        "Non-finite state/futures covariance at t=",
        time_index - 1L,
        "."
      ),
      call. = FALSE
    )
  }
  
  joint_covariance <- assemble_joint_covariance(
    state_covariance =
      state_covariance,
    futures_covariance =
      futures_covariance,
    state_futures_covariance =
      cross_covariance
  )
  
  joint_minimum_eigenvalue <- minimum_eigenvalue(
    joint_covariance
  )
  
  if (
    joint_minimum_eigenvalue <
    -PSD_TOLERANCE
  ) {
    stop(
      paste0(
        "Final live 16D covariance is not PSD at t=",
        time_index - 1L,
        ". Minimum eigenvalue=",
        format(
          joint_minimum_eigenvalue,
          digits = 17
        )
      ),
      call. = FALSE
    )
  }
  
  daily_variances <- diag(
    state_covariance
  )[7:8]
  
  if (
    all(
      daily_variances <=
      ZERO_VARIANCE_TOLERANCE
    )
  ) {
    daily_cross_error <- max(
      abs(
        cross_covariance[
          7:8,
          ,
          drop = FALSE
        ]
      )
    )
    
    if (
      daily_cross_error >
      1e-12
    ) {
      stop(
        paste0(
          "Inactive daily state has nonzero state/futures covariance at t=",
          time_index - 1L,
          "."
        ),
        call. = FALSE
      )
    }
  }
}

cat(
  "Live state/futures covariance PSD validation: TRUE\n"
)


# ============================================================
# 18. Serialize state_futures_cov_competition.csv
# ============================================================

state_futures_long_rows <- vector(
  "list",
  T_LIVE *
    8L *
    8L
)

row_counter <- 1L

for (time_index in seq_len(T_LIVE)) {
  t_original <- time_index - 1L
  
  for (state_name in STATE_ORDER) {
    for (future_name in OPTIMIZER_FUTURE_ORDER) {
      state_futures_long_rows[[row_counter]] <- tibble(
        t_original = t_original,
        state = state_name,
        future = future_name,
        covariance =
          LIVE_STATE_FUTURES_COVARIANCE_ARRAY[
            time_index,
            state_name,
            future_name
          ]
      )
      
      row_counter <- row_counter + 1L
    }
  }
}

state_futures_covariance_csv <- bind_rows(
  state_futures_long_rows
)

stopifnot(
  nrow(
    state_futures_covariance_csv
  ) ==
    T_LIVE *
    8L *
    8L
)

stopifnot(
  identical(
    sort(
      unique(
        as.integer(
          state_futures_covariance_csv[["t_original"]]
        )
      )
    ),
    0:(T_LIVE - 1L)
  )
)

stopifnot(
  setequal(
    unique(
      state_futures_covariance_csv[["state"]]
    ),
    STATE_ORDER
  )
)

stopifnot(
  setequal(
    unique(
      state_futures_covariance_csv[["future"]]
    ),
    OPTIMIZER_FUTURE_ORDER
  )
)


# ============================================================
# 19. Build live 16D latent correlation array
# ============================================================

LIVE_LATENT_CORRELATION_ARRAY <- array(
  0,
  dim = c(
    T_LIVE,
    16L,
    16L
  ),
  dimnames = list(
    as.character(
      0:(T_LIVE - 1L)
    ),
    CANONICAL_COMPONENT_ORDER,
    CANONICAL_COMPONENT_ORDER
  )
)

latent_coupling_diagnostics <- vector(
  "list",
  T_LIVE
)

for (time_index in seq_len(T_LIVE)) {
  t_original <- time_index - 1L
  
  state_covariance <- LIVE_STATE_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  futures_covariance <- LIVE_FUTURES_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  state_futures_covariance <- LIVE_STATE_FUTURES_COVARIANCE_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  live_state_correlation <- covariance_to_correlation_safe(
    state_covariance
  )
  
  live_state_futures_correlation <- cross_covariance_to_correlation(
    cross_covariance =
      state_futures_covariance,
    row_covariance =
      state_covariance,
    column_covariance =
      futures_covariance
  )
  
  # state_futures is State x Futures.
  # 50.15 latent assembly is Futures x State.
  futures_state_correlation <- t(
    live_state_futures_correlation
  )
  
  daily_variances <- diag(
    state_covariance
  )[7:8]
  
  daily_active <- any(
    daily_variances >
      ZERO_VARIANCE_TOLERANCE
  )
  
  if (daily_active) {
    daily_correlation <- live_state_correlation[
      7:8,
      7:8,
      drop = FALSE
    ]
    
    daily_correlation <- symmetrize_matrix(
      daily_correlation
    )
    
    diag(
      daily_correlation
    ) <- 1
  } else {
    daily_correlation <- diag(
      1,
      2L
    )
  }
  
  futures_final_cross <- futures_state_correlation[
    ,
    1:6,
    drop = FALSE
  ]
  
  futures_daily_cross <- futures_state_correlation[
    ,
    7:8,
    drop = FALSE
  ]
  
  final_daily_cross <- live_state_correlation[
    1:6,
    7:8,
    drop = FALSE
  ]
  
  if (!daily_active) {
    futures_daily_cross[,] <- 0
    final_daily_cross[,] <- 0
  }
  
  latent_result <- shrink_latent_cross_blocks_for_pd(
    futures_formal_correlation =
      FUTURES_FORMAL_CORRELATION,
    final_formal_correlation =
      FINAL_STATE_FORMAL_CORRELATION,
    daily_correlation =
      daily_correlation,
    futures_final_cross =
      futures_final_cross,
    futures_daily_cross =
      futures_daily_cross,
    final_daily_cross =
      final_daily_cross
  )
  
  LIVE_LATENT_CORRELATION_ARRAY[
    time_index,
    ,
    
  ] <- latent_result[["correlation"]]
  
  latent_coupling_diagnostics[[time_index]] <- tibble(
    t_original = t_original,
    daily_active = daily_active,
    cross_block_lambda =
      latent_result[["lambda"]],
    minimum_latent_eigenvalue =
      latent_result[["minimum_eigenvalue"]]
  )
}

latent_coupling_diagnostics <- bind_rows(
  latent_coupling_diagnostics
)

cat(
  "Live 16D latent correlation array constructed: TRUE\n"
)


# ============================================================
# 20. Validate exact 50.15 within-block preservation
# ============================================================

maximum_futures_block_error <- 0

maximum_final_block_error <- 0

minimum_latent_eigenvalue_overall <- Inf

for (time_index in seq_len(T_LIVE)) {
  latent_matrix <- LIVE_LATENT_CORRELATION_ARRAY[
    time_index,
    ,
    ,
    drop = TRUE
  ]
  
  symmetry_error <- max(
    abs(
      latent_matrix -
        t(
          latent_matrix
        )
    )
  )
  
  if (
    symmetry_error >
    SYMMETRY_TOLERANCE
  ) {
    stop(
      paste0(
        "Latent correlation is not symmetric at t=",
        time_index - 1L,
        "."
      ),
      call. = FALSE
    )
  }
  
  diagonal_error <- max(
    abs(
      diag(
        latent_matrix
      ) -
        1
    )
  )
  
  if (
    diagonal_error >
    1e-10
  ) {
    stop(
      paste0(
        "Latent correlation diagonal is invalid at t=",
        time_index - 1L,
        "."
      ),
      call. = FALSE
    )
  }
  
  futures_block_error <- max(
    abs(
      latent_matrix[
        1:8,
        1:8,
        drop = FALSE
      ] -
        FUTURES_FORMAL_CORRELATION
    )
  )
  
  final_block_error <- max(
    abs(
      latent_matrix[
        9:14,
        9:14,
        drop = FALSE
      ] -
        FINAL_STATE_FORMAL_CORRELATION
    )
  )
  
  maximum_futures_block_error <- max(
    maximum_futures_block_error,
    futures_block_error
  )
  
  maximum_final_block_error <- max(
    maximum_final_block_error,
    final_block_error
  )
  
  one_minimum_eigenvalue <- minimum_eigenvalue(
    latent_matrix
  )
  
  minimum_latent_eigenvalue_overall <- min(
    minimum_latent_eigenvalue_overall,
    one_minimum_eigenvalue
  )
  
  if (
    one_minimum_eigenvalue <
    PD_TARGET_EIGENVALUE *
    0.99
  ) {
    stop(
      paste0(
        "Live latent correlation is not sufficiently PD at t=",
        time_index - 1L,
        ". Minimum eigenvalue=",
        format(
          one_minimum_eigenvalue,
          digits = 17
        )
      ),
      call. = FALSE
    )
  }
}

if (
  maximum_futures_block_error >
  1e-12
) {
  stop(
    "Frozen futures formal copula block was modified.",
    call. = FALSE
  )
}

if (
  maximum_final_block_error >
  1e-12
) {
  stop(
    "Frozen FinalState formal copula block was modified.",
    call. = FALSE
  )
}

cat(
  "Frozen futures block maximum error: ",
  format(
    maximum_futures_block_error,
    digits = 17
  ),
  "\n",
  sep = ""
)

cat(
  "Frozen final-state block maximum error: ",
  format(
    maximum_final_block_error,
    digits = 17
  ),
  "\n",
  sep = ""
)

cat(
  "Minimum live latent-correlation eigenvalue: ",
  format(
    minimum_latent_eigenvalue_overall,
    digits = 17
  ),
  "\n",
  sep = ""
)

cat(
  "Live 50.15 within-block and PD validation: TRUE\n"
)


# ============================================================
# 21. Serialize innovation_latent_correlation.csv
# ============================================================

latent_long_rows <- vector(
  "list",
  T_LIVE *
    16L *
    16L
)

row_counter <- 1L

for (time_index in seq_len(T_LIVE)) {
  t_original <- time_index - 1L
  
  for (row_component_index in seq_along(CANONICAL_COMPONENT_ORDER)) {
    row_component <-
      CANONICAL_COMPONENT_ORDER[
        row_component_index
      ]
    
    for (column_component_index in seq_along(CANONICAL_COMPONENT_ORDER)) {
      column_component <-
        CANONICAL_COMPONENT_ORDER[
          column_component_index
        ]
      
      latent_long_rows[[row_counter]] <- tibble(
        t_original = t_original,
        component_1_index =
          as.integer(
            row_component_index
          ),
        component_2_index =
          as.integer(
            column_component_index
          ),
        component_1 = row_component,
        component_2 = column_component,
        correlation =
          LIVE_LATENT_CORRELATION_ARRAY[
            time_index,
            row_component,
            column_component
          ]
      )
      
      row_counter <- row_counter + 1L
    }
  }
}

innovation_latent_correlation <- bind_rows(
  latent_long_rows
)

stopifnot(
  nrow(
    innovation_latent_correlation
  ) ==
    T_LIVE *
    16L *
    16L
)

stopifnot(
  nrow(
    innovation_latent_correlation
  ) ==
    37632L
)

stopifnot(
  identical(
    sort(
      unique(
        as.integer(
          innovation_latent_correlation[["t_original"]]
        )
      )
    ),
    0:146
  )
)

cat(
  "innovation_latent_correlation rows: ",
  nrow(
    innovation_latent_correlation
  ),
  "\n",
  sep = ""
)


# ============================================================
# 22. Load frozen Python-facing marginal and mapping tables
# ============================================================
#
# 50.4 is serialization only. These two tables contain frozen
# 50.0 and 50.25 parameters and therefore should be copied
# exactly rather than recomputed.
#
# ============================================================

innovation_marginals <- readr::read_csv(
  FROZEN_MARGINAL_CSV_PATH,
  show_col_types = FALSE
)

innovation_arithmetic_mapping <- readr::read_csv(
  FROZEN_MAPPING_CSV_PATH,
  show_col_types = FALSE
)

required_marginal_columns <- c(
  "component",
  "component_index",
  "system",
  "distribution",
  "df",
  "shape",
  "skew",
  "parameter_string"
)

if (
  !all(
    required_marginal_columns %in%
    names(
      innovation_marginals
    )
  )
) {
  stop(
    "Frozen innovation_marginals.csv has unexpected schema.",
    call. = FALSE
  )
}

if (
  nrow(
    innovation_marginals
  ) != 16L
) {
  stop(
    "Frozen innovation_marginals.csv must contain 16 rows.",
    call. = FALSE
  )
}

if (
  !identical(
    as.character(
      innovation_marginals[["component"]]
    ),
    CANONICAL_COMPONENT_ORDER
  )
) {
  stop(
    "Frozen marginal component order is incorrect.",
    call. = FALSE
  )
}

required_mapping_columns <- c(
  "future",
  "future_index",
  "mapping_family",
  "floor_parameter",
  "raw_mean",
  "raw_sd",
  "return_floor",
  "return_floor_epsilon"
)

if (
  !all(
    required_mapping_columns %in%
    names(
      innovation_arithmetic_mapping
    )
  )
) {
  stop(
    "Frozen innovation_arithmetic_mapping.csv has unexpected schema.",
    call. = FALSE
  )
}

if (
  nrow(
    innovation_arithmetic_mapping
  ) != 8L
) {
  stop(
    "Frozen innovation_arithmetic_mapping.csv must contain 8 rows.",
    call. = FALSE
  )
}

if (
  !identical(
    as.character(
      innovation_arithmetic_mapping[["future"]]
    ),
    FUTURES_ORDER
  )
) {
  stop(
    "Frozen arithmetic mapping futures order is incorrect.",
    call. = FALSE
  )
}

if (
  any(
    !is.finite(
      innovation_arithmetic_mapping[["floor_parameter"]]
    )
  ) ||
  any(
    !is.finite(
      innovation_arithmetic_mapping[["raw_mean"]]
    )
  ) ||
  any(
    !is.finite(
      innovation_arithmetic_mapping[["raw_sd"]]
    )
  ) ||
  any(
    innovation_arithmetic_mapping[["raw_sd"]] <= 0
  ) ||
  any(
    innovation_arithmetic_mapping[["return_floor"]] != -1
  ) ||
  any(
    innovation_arithmetic_mapping[["return_floor_epsilon"]] <= 0
  )
) {
  stop(
    "Frozen arithmetic mapping parameter validation failed.",
    call. = FALSE
  )
}

cat(
  "Frozen marginal and arithmetic mapping tables validated: TRUE\n"
)


# ============================================================
# 23. Build live metadata
# ============================================================

frozen_metadata <- readr::read_csv(
  FROZEN_METADATA_CSV_PATH,
  col_types = cols(
    key = col_character(),
    value = col_character()
  ),
  show_col_types = FALSE
)

if (
  !all(
    c(
      "key",
      "value"
    ) %in%
    names(
      frozen_metadata
    )
  )
) {
  stop(
    "Frozen innovation metadata has unexpected schema.",
    call. = FALSE
  )
}

if (
  anyDuplicated(
    frozen_metadata[["key"]]
  ) >
  0L
) {
  stop(
    "Frozen innovation metadata contains duplicate keys.",
    call. = FALSE
  )
}

metadata_values <- setNames(
  as.character(
    frozen_metadata[["value"]]
  ),
  as.character(
    frozen_metadata[["key"]]
  )
)

metadata_values[["production_time_count"]] <- as.character(
  T_LIVE
)

metadata_values[["future_count"]] <- "8"

metadata_values[["final_state_count"]] <- "6"

metadata_values[["daily_state_count"]] <- "2"

metadata_values[["state_count"]] <- "8"

metadata_values[["component_count"]] <- "16"

metadata_values[["final_state_student_t_df"]] <- format(
  FINAL_STATE_T_DF,
  digits = 17,
  scientific = FALSE,
  trim = TRUE
)

metadata_values[["futures_order"]] <- paste(
  FUTURES_ORDER,
  collapse = "|"
)

metadata_values[["final_state_order"]] <- paste(
  FINAL_STATE_ORDER,
  collapse = "|"
)

metadata_values[["daily_state_order"]] <- paste(
  DAILY_STATE_ORDER,
  collapse = "|"
)

metadata_values[["state_order"]] <- paste(
  STATE_ORDER,
  collapse = "|"
)

metadata_values[["canonical_component_order"]] <- paste(
  CANONICAL_COMPONENT_ORDER,
  collapse = "|"
)

metadata_values[["marginal_production_parameters_final"]] <- "TRUE"

metadata_values[["dependence_production_parameters_final"]] <- "TRUE"

metadata_values[["joint_16d_coupling_final"]] <- "TRUE"

metadata_values[["arithmetic_return_mapping_final"]] <- "TRUE"

metadata_values[["old_off_diagonal_arithmetic_covariance_policy"]] <- "Benchmark_Only"

metadata_values[["python_monte_carlo_modified"]] <- "FALSE"

# Preserve original metadata ordering, while updating only values
# that legitimately depend on the live production horizon.
innovation_system_metadata <- frozen_metadata

for (row_index in seq_len(nrow(innovation_system_metadata))) {
  one_key <- innovation_system_metadata[["key"]][row_index]
  
  if (
    one_key %in%
    names(
      metadata_values
    )
  ) {
    innovation_system_metadata[["value"]][row_index] <- metadata_values[[one_key]]
  }
}

required_metadata_keys <- c(
  "interface_version",
  "marginal_system_version",
  "dependence_system_version",
  "joint_coupling_version",
  "arithmetic_mapping_version",
  "production_time_count",
  "future_count",
  "final_state_count",
  "daily_state_count",
  "state_count",
  "component_count",
  "final_state_student_t_df",
  "futures_dependence",
  "final_state_dependence",
  "daily_state_dependence",
  "cross_system_dependence",
  "arithmetic_mapping_family",
  "futures_order",
  "final_state_order",
  "daily_state_order",
  "state_order",
  "canonical_component_order",
  "marginal_production_parameters_final",
  "dependence_production_parameters_final",
  "joint_16d_coupling_final",
  "arithmetic_return_mapping_final",
  "old_off_diagonal_arithmetic_covariance_policy"
)

missing_metadata_keys <- setdiff(
  required_metadata_keys,
  innovation_system_metadata[["key"]]
)

if (
  length(
    missing_metadata_keys
  ) > 0L
) {
  stop(
    paste0(
      "Live metadata is missing required keys: ",
      paste(
        missing_metadata_keys,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

if (
  innovation_system_metadata[["value"]][
    innovation_system_metadata[["key"]] ==
    "production_time_count"
  ] !=
  "147"
) {
  stop(
    "Live innovation metadata production_time_count is not 147.",
    call. = FALSE
  )
}

cat(
  "Live innovation metadata constructed: TRUE\n"
)


# ============================================================
# 24. Past-row policy validation
# ============================================================
#
# Stage 6 preserves all t=0:146 rows. The optimizer uses
# current_t:146. We do not overwrite historical rows with realized
# innovations because several Stage 6 objects have no single
# realized analogue.
#
# ============================================================

stopifnot(
  CURRENT_T >= 0L
)

stopifnot(
  CURRENT_T < T_LIVE
)

stopifnot(
  nrow(
    state_futures_covariance_csv
  ) ==
    9408L
)

stopifnot(
  nrow(
    innovation_latent_correlation
  ) ==
    37632L
)

cat(
  "Full 147-period Stage 6 interface preserved: TRUE\n"
)


# ============================================================
# 25. Write audit diagnostics first
# ============================================================

readr::write_csv(
  state_futures_psd_diagnostics,
  file.path(
    STAGE6_AUDIT_DIRECTORY,
    "state_futures_psd_diagnostics.csv"
  )
)

readr::write_csv(
  latent_coupling_diagnostics,
  file.path(
    STAGE6_AUDIT_DIRECTORY,
    "latent_coupling_diagnostics.csv"
  )
)

cat(
  "Stage 6 audit diagnostics written: TRUE\n"
)


# ============================================================
# 26. Write Stage 6 production CSVs
# ============================================================

readr::write_csv(
  state_futures_covariance_csv,
  STATE_FUTURES_OUTPUT_PATH,
  na = ""
)

readr::write_csv(
  innovation_marginals,
  MARGINAL_OUTPUT_PATH,
  na = ""
)

readr::write_csv(
  innovation_latent_correlation,
  LATENT_CORRELATION_OUTPUT_PATH,
  na = ""
)

readr::write_csv(
  innovation_arithmetic_mapping,
  ARITHMETIC_MAPPING_OUTPUT_PATH,
  na = ""
)

readr::write_csv(
  innovation_system_metadata,
  METADATA_OUTPUT_PATH,
  na = ""
)

stage6_output_paths <- c(
  STATE_FUTURES_OUTPUT_PATH,
  MARGINAL_OUTPUT_PATH,
  LATENT_CORRELATION_OUTPUT_PATH,
  ARITHMETIC_MAPPING_OUTPUT_PATH,
  METADATA_OUTPUT_PATH
)

if (
  !all(
    file.exists(
      stage6_output_paths
    )
  )
) {
  stop(
    "One or more Stage 6 output files were not written.",
    call. = FALSE
  )
}

cat(
  "Five Stage 6 production CSVs written: TRUE\n"
)


# ============================================================
# 27. Reload all five outputs
# ============================================================

reloaded_state_futures <- readr::read_csv(
  STATE_FUTURES_OUTPUT_PATH,
  show_col_types = FALSE
)

reloaded_marginals <- readr::read_csv(
  MARGINAL_OUTPUT_PATH,
  show_col_types = FALSE
)

reloaded_latent <- readr::read_csv(
  LATENT_CORRELATION_OUTPUT_PATH,
  show_col_types = FALSE
)

reloaded_mapping <- readr::read_csv(
  ARITHMETIC_MAPPING_OUTPUT_PATH,
  show_col_types = FALSE
)

reloaded_metadata <- readr::read_csv(
  METADATA_OUTPUT_PATH,
  col_types = cols(
    key = col_character(),
    value = col_character()
  ),
  show_col_types = FALSE
)


# ============================================================
# 28. Serialization validation
# ============================================================

stopifnot(
  nrow(
    reloaded_state_futures
  ) ==
    9408L
)

stopifnot(
  nrow(
    reloaded_marginals
  ) ==
    16L
)

stopifnot(
  nrow(
    reloaded_latent
  ) ==
    37632L
)

stopifnot(
  nrow(
    reloaded_mapping
  ) ==
    8L
)

stopifnot(
  nrow(
    reloaded_metadata
  ) ==
    nrow(
      innovation_system_metadata
    )
)

state_futures_serialization_error <- max(
  abs(
    as.numeric(
      reloaded_state_futures[["covariance"]]
    ) -
      as.numeric(
        state_futures_covariance_csv[["covariance"]]
      )
  )
)

latent_serialization_error <- max(
  abs(
    as.numeric(
      reloaded_latent[["correlation"]]
    ) -
      as.numeric(
        innovation_latent_correlation[["correlation"]]
      )
  )
)

if (
  state_futures_serialization_error >
  1e-12
) {
  stop(
    "state_futures_cov_competition.csv serialization error is too large.",
    call. = FALSE
  )
}

if (
  latent_serialization_error >
  1e-12
) {
  stop(
    "innovation_latent_correlation.csv serialization error is too large.",
    call. = FALSE
  )
}

if (
  !identical(
    as.character(
      reloaded_marginals[["component"]]
    ),
    CANONICAL_COMPONENT_ORDER
  )
) {
  stop(
    "Reloaded marginal component order changed.",
    call. = FALSE
  )
}

if (
  !identical(
    as.character(
      reloaded_mapping[["future"]]
    ),
    FUTURES_ORDER
  )
) {
  stop(
    "Reloaded arithmetic mapping futures order changed.",
    call. = FALSE
  )
}

cat(
  "Stage 6 serialization validation: TRUE\n"
)


# ============================================================
# 29. Final diagnostic summary
# ============================================================

state_futures_lambda_min <- min(
  state_futures_psd_diagnostics[["cross_covariance_lambda"]]
)

state_futures_lambda_median <- stats::median(
  state_futures_psd_diagnostics[["cross_covariance_lambda"]]
)

state_futures_shrunk_count <- sum(
  state_futures_psd_diagnostics[["cross_covariance_lambda"]] <
    1 -
    1e-12
)

latent_lambda_min <- min(
  latent_coupling_diagnostics[["cross_block_lambda"]]
)

latent_lambda_median <- stats::median(
  latent_coupling_diagnostics[["cross_block_lambda"]]
)

latent_shrunk_count <- sum(
  latent_coupling_diagnostics[["cross_block_lambda"]] <
    1 -
    1e-12
)

cat(
  "\n",
  "============================================================\n",
  "STAGE 6 LIVE DEPENDENCE / INNOVATION SUMMARY\n",
  "============================================================\n",
  "T: ",
  T_LIVE,
  "\n",
  "current_t: ",
  CURRENT_T,
  "\n",
  "remaining intervals: ",
  T_LIVE - CURRENT_T,
  "\n",
  "\n",
  "state_futures_cov_competition rows: ",
  nrow(
    reloaded_state_futures
  ),
  "\n",
  "innovation_marginals rows: ",
  nrow(
    reloaded_marginals
  ),
  "\n",
  "innovation_latent_correlation rows: ",
  nrow(
    reloaded_latent
  ),
  "\n",
  "innovation_arithmetic_mapping rows: ",
  nrow(
    reloaded_mapping
  ),
  "\n",
  "innovation_system_metadata rows: ",
  nrow(
    reloaded_metadata
  ),
  "\n",
  "\n",
  "State/futures covariance shrink count: ",
  state_futures_shrunk_count,
  " of ",
  T_LIVE,
  "\n",
  "State/futures minimum lambda: ",
  format(
    state_futures_lambda_min,
    digits = 12
  ),
  "\n",
  "State/futures median lambda: ",
  format(
    state_futures_lambda_median,
    digits = 12
  ),
  "\n",
  "\n",
  "Latent cross-block shrink count: ",
  latent_shrunk_count,
  " of ",
  T_LIVE,
  "\n",
  "Latent minimum lambda: ",
  format(
    latent_lambda_min,
    digits = 12
  ),
  "\n",
  "Latent median lambda: ",
  format(
    latent_lambda_median,
    digits = 12
  ),
  "\n",
  "Minimum latent eigenvalue: ",
  format(
    minimum_latent_eigenvalue_overall,
    digits = 12
  ),
  "\n",
  "Frozen futures block max error: ",
  format(
    maximum_futures_block_error,
    digits = 12
  ),
  "\n",
  "Frozen final-state block max error: ",
  format(
    maximum_final_block_error,
    digits = 12
  ),
  "\n",
  "FinalState Student-t df: ",
  format(
    FINAL_STATE_T_DF,
    digits = 12
  ),
  "\n",
  "============================================================\n",
  sep = ""
)


# ============================================================
# 30. Final validation flag
# ============================================================

LIVE_DEPENDENCE_AND_INNOVATIONS_VALIDATED <- TRUE

cat(
  "\nLIVE_DEPENDENCE_AND_INNOVATIONS_VALIDATED = TRUE\n"
)

cat(
  "LIVE DEPENDENCE AND INNOVATION STAGE COMPLETE\n"
)